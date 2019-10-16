# Beating C with 80 lines of Haskell: wc

You can find the blog version of this article [here](https://chrispenner.ca/posts/wc)

And if you're at all interested in the book I'm writing on lenses and optics, you can find that [here](https://www.patreon.com/ChrisPenner). Cheers!

* * *

Despite the click-bait title I hope you'll find this post generally illuminating, or at the very least a bit of fun! This article makes no claims that Haskell is "better" than C, nor does it make claims about the respective value of either language, or either implementation. It's simply an exploration into high-performance Haskell, with a few fun tricks and hacks along the way.

You can find source code for this post [here](https://github.com/ChrisPenner/wc).

For reference, I'm using the Mac's version of `wc`; you can find [reference source code here](https://opensource.apple.com/source/text_cmds/text_cmds-68/wc/wc.c.auto.html).  Yes, there are faster `wc` implementations out there.

The challenge is to build a *faster* clone of the hand-optimized C implementation of the `wc` utility in our favourite high-level garbage-collected runtime-based language: Haskell! Sounds simple enough right?

Here's the criteria we'll be considering as we go along:

* Correctness: Should return identical character, word, and line counts as `wc` on the test files.
* Speed (wall-clock-time): How do we compare to the execution time of `wc`?
* Max Resident Memory: What's the peak of our memory usage? Is our memory usage constant, linear, or otherwise?

Those are the main things we need to worry about.

Let's dive in.

## The dumbest thing that could possibly work

As always, we should start by just trying the dumbest possible thing and see how it goes. We can build up from there. What's the dumbest way to count characters, lines, and words in Haskell? Well, we could read the file, then run the functions `length`,`length . words`, and `length . lines` to get our counts!

```haskell
stupid :: FilePath -> IO (Int, Int, Int)
stupid fp = do
    contents <- readFile fp
    return (length s, length (words s), length (lines s))
```

Amazingly enough, this actually DOES work, and gets us the same answers as `wc`, IF you're willing to wait for it... I got sick of waiting for it to finish on my large test file (it was taking more than a few minutes), but on a smaller test file (90 MB) got the following results:

90 MB test file:

|          | wc        | stupid-wc |
| -------- | --------- |---------- |
| time     | 0.37s     | 17.07s    |
| max mem. | 1.86 MB   | 2403 MB   |

Yikes... Needless to say there's some room for improvement...

## Being slightly less dumb

Let's think about why this is doing so poorly; the first thing that comes to mind is that we're iterating through the contents of the file 3 separate times! This also means GHC can't garbage collect our list as we iterate through it since we're still using it in other places. The fact that we're keeping every character of the file in a linked list helps explain the 2.4 GB of memory on a file that's only 90 MB! Ouch!

Okay, so that's REALLY not great. Let's see if we can get this down to a SINGLE pass over the structure. We're accumulating 3 simple things, so maybe we can process all three parts at once? When iterating through a structure to get one final result I reach for folds!

It's pretty easy to use a fold to count characters or lines; the character count always adds one to the total, the line count adds one when the current character is a newline; but what about the word count? We can't add one on every space character because consecutive spaces doesn't count as a new word! We'll need to keep track of whether the previous character was a space, and only add one to the counter whenever we start a completely **new** word. That's not too tough to do; we'll use `foldl'` from `Data.List` for our implementation.

```haskell
import Data.List
import Data.Char

simpleFold :: FilePath -> IO (Int, Int, Int)
simpleFold fp = do
    countFile <$> readFile fp

countFile :: String -> (Int, Int, Int)
countFile s =
    let (cs, ws, ls, _) = foldl' go (0, 0, 0, False) s
     in (cs, ws, ls)
  where
    go :: (Int, Int, Int, Bool) -> Char -> (Int, Int, Int, Bool)
    go (cs, ws, ls, wasSpace) c =
        let addLine | c == '\n' = 1
                    | otherwise = 0
            addWord | wasSpace = 0
                    | isSpace c = 1
                    | otherwise = 0
         in (cs + 1, ws + addWord, ls + addLine, isSpace c)
```

Running this version we run into an even worse problem! The program takes more than a few minutes and quickly spikes up to more than 3 GB of memory! What's gone wrong? Well, we used the strict version of `foldl` (indicated by the trailing tick `'`); BUT it's only strict up to "Weak Head Normal Form" (WHNF), which means it'll be strict in the **structure** of the tuple accumulator, but not the actual values! That's annoying, because it means we're building up a HUGE thunk of additions that we never fully evaluate until we've finished iterating through the whole file! Sometimes laziness sneaks in and bites us like this. This is the sort of memory leak that can easily take down web-servers if you aren't careful!

90 MB test file:

|          | wc        | simple-fold-wc                |
| -------- | --------- |------------------------------ |
| time     | 0.37s     | longer than I want to wait    |
| max mem. | 1.86 MB   | > 3 GB                        |


We can fix it by telling GHC to strictly evaluate the contents of the tuple on ever iteration. An easy way to do that is with the `BangPatterns` extension; it lets us use `!` in our argument list to force evaluation on each run of the function. Here's the new version of `go`:

```haskell
{-# LANGUAGE BangPatterns #-}

...
    go :: (Int, Int, Int, Bool) -> Char -> (Int, Int, Int, Bool)
    go (!cs, !ws, !ls, !wasSpace) c =
        let addLine | c == '\n' = 1
                    | otherwise = 0
            addWord | wasSpace = 0
                    | isSpace c = 1
                    | otherwise = 0
         in (cs + 1, ws + addWord, ls + addLine, isSpace c)
```

That simple change speeds things up like **CRAZY**; here's our new performance breakdown:

90 MB test file:

|          | wc        | strict-fold-wc |
| -------- | --------- |--------------- |
| time     | 0.37s     | 8.12s          |
| max mem. | 1.86 MB   | 3.7 MB         |

Okay; so we're doing WAY better on memory now, a few MBs of memory on a 90 MB file means we must finally be streaming the file contents properly! Even though laziness has already bitten us on this problem, now that we've localized the laziness into the right places it provides us with streaming for free! The streaming happens naturally because `readFile` actually does **lazy IO**; which can be a real nuisance sometimes for things like web servers since you're never quite sure when the IO is happening, but in our case it gives us much better memory residency.

## Better with ByteStrings

We can probably stop worrying about memory for now, so we're back to crunching for performance! One thing I can think to try there to switch to using a ByteString rather than a String. Using a String means we're implicitly decoding the file as we read it, which takes time, AND we have the overhead of using a linked list for the whole thing, we can't easily take advantage of batching or buffering our data as we read it.

This change is actually laughably easy, the `bytestring` package provides the module: `Data.ByteString.Lazy.Char8`, which provides operations for working with Lazy ByteStrings as though they were strings of characters, but with all the performance benefits of ByteStrings. Note that it DOESN'T actually verify that each byte is a valid Character, or do any decoding, so it's on us to make sure we're passing it valid data. By default `wc` assumes its input is ASCII, so I think we're safe to do the same. If our input is ASCII then the functions in this module will behave sensibly.

Literally the only changes I need to make are to switch the `Data.List` import to `Data.ByteString.Lazy.Char8` and then switch the `readFile` and `foldl'` functions to their ByteString versions:

```haskell
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as BS

simpleFold :: FilePath -> IO (Int, Int, Int)
simpleFold fp = do
    simpleFoldCountFile <$> BS.readFile fp

simpleFoldCountFile :: BS.ByteString -> (Int, Int, Int)
simpleFoldCountFile s =
    let (cs, ws, ls, _) = BS.foldl' go (0, 0, 0, False) s
     in (cs, ws, ls)
  where
    go :: (Int, Int, Int, Bool) -> Char -> (Int, Int, Int, Bool)
    go (!cs, !ws, !ls, !wasSpace) c =
        let addLine | c == '\n' = 1
                    | otherwise = 0
            addWord | wasSpace = 0
                    | isSpace c = 1
                    | otherwise = 0
         in (cs + 1, ws + addWord, ls + addLine, isSpace c)
```

This little change chops our time down by nearly half!

90 MB test file:

|          | wc        | strict-fold-wc | bs-fold-wc        |
| -------- | --------- | -------------- | ----------------- |
| time     | 0.37s     | 8.12s          | 3.41s             |
| max mem. | 1.86 MB   | 3.7 MB         | 5.48 MB           |
 

So we're clearly still making some progress. Our memory usage has increased slightly, but it still seems to be a constant overhead. We're still orders of magnitude away from `wc` unfortunately; let's see if there's anything else we could do.

## Moving to Monoids

At this point I feel like experimenting a little. Modern PC's tend to have multiple cores, and it seems as though newer machines scale up the number of cores moreso than their processor speed, so it would be beneficial to take advantage of that.

Splitting up a computation like this isn't exactly trivial. In order to use multiple cores we'll need to split up the job into pieces. In theory this is easy, just split the file into chunks and give one chunk to each core! As you think a bit deeper about it the problems start to appear; combining character counts is pretty easy, we can just sum the totals from each chunk. The same with line-counts, but word counts pose a problem! What happens if we split in the middle of a word, or in the middle of several consecutive spaces? In order to combine the word counts we'd need to keep track of the starting and end state of each chunk and be intelligent when we combine them together. That sounds like a lot of book-keeping that I don't really want to do.

Monoids to the rescue! The associative laws of a Monoid mean that so long as we can develop a lawful monoid it WILL work properly in spite of this type of parallelism. That really just passes the buck down the line though, is it possible to write a Monoid that can handle the complexities of word-counting like this?

It sure is! It may not be immediately apparent how a monoid like this works, but there's a class of counting problems that all fall into the same category like this, and luckily for me I've worked on these before. Basically we need to count the number of times a given invariant has **changed** from the start to the end of a sequence. I've generalized this class of monoid before, naming them [`flux` monoids](http://hackage.haskell.org/package/flux-monoid). What we need to do is count the number of times we change from characters which ARE spaces to those which AREN'T spaces. We could probably express this using the `Flux` monoid itself, but since we need to be so careful about strictness and performance I'm going to define a bespoke version of the Flux monoid for our purposes. Check this out:


```haskell
data CharType = IsSpace | NotSpace
    deriving Show

data Flux =
    Flux !CharType
         {-# UNPACK #-} !Int
         !CharType
    | Unknown
    deriving Show
```

We need these types only for the word-counting part of our solution.

The `CharType` says whether a given character is considered a space or not; then the `Flux` type represents a chunk of text, storing fields for whether the left-most character is a space, how many words are in the full block of text, and whether the right-most character is a space. We don't actually keep the text in the structure since we don't need it for this problem. I've `UNPACK`ed the `Int` and made all the fields strict to ensure we won't run into the same problems we did with lazy tuples earlier. Using a strict data type means I don't need to use BangPatterns in my computations.

Next we need a semigroup and Monoid instance for this type!

```haskell
instance Semigroup Flux where
  Unknown <> x = x
  x <> Unknown = x
  Flux l n NotSpace <> Flux NotSpace n' r = Flux l (n + n' - 1) r
  Flux l n _ <> Flux _ n' r = Flux l (n + n') r

instance Monoid Flux where
  mempty = Unknown
```

The `Unknown` constructor is there to represent a Monoidal identity, we could actually leave it out and use `Maybe` to promote our Semigroup into a Monoid, but `Maybe` introduces unwanted laziness into our semigroup append! I just define it as part of the type for simplicity.

The `(<>)` operation we define checks whether the join-point of our two text blocks happens in the middle of a word, if it does then we must have counted the start and end of the same word separately, so we subtract one when we add the word totals to make it all balance out.

Lastly we need a way to build a `Flux` object from individual characters.

```haskell
flux :: Char -> Flux
flux c | isSpace c = Flux IsSpace 0 IsSpace
       | otherwise = Flux NotSpace 1 NotSpace
```

This is simple enough, we count non-space characters as 'words' which start and end with non-space charactes and for spaces have an empty word count surrounded on both sides with space chars.

It may not be immediately clear, but that's all we need to count words monoidally!

```haskell
>>> foldMap flux "testing one two three"
Flux NotSpace 4 NotSpace

>>> foldMap flux "testing on" <> foldMap flux "e two three"
Flux NotSpace 4 NotSpace

>>> foldMap flux "testing one " <> foldMap flux " two three"
Flux NotSpace 4 NotSpace
```

Looks like it's working fine!

We've got the word count part covered, now we need the Monoidal version of the char count and line count. This is a snap to build:

```haskell
data Counts =
    Counts { charCount :: {-# UNPACK #-} !Int

           , wordCount ::  !Flux
           , lineCount :: {-# UNPACK #-} !Int
           }
    deriving (Show)

instance Semigroup Counts where
  (Counts a b c) <> (Counts a' b' c') = Counts (a + a') (b <> b') (c + c')

instance Monoid Counts where
  mempty = Counts 0 mempty 0
```

No problem! Similarly we'll need a way to turn a single char into a `Counts` object:

```haskell
countChar :: Char -> Counts
countChar c =
    Counts { charCount = 1
           , wordCount = flux c
           , lineCount = if (c == '\n') then 1 else 0
           }
```

Let's try that out too:

```haskell
>>> foldMap countChar "one two\nthree"
Counts {charCount = 13, wordCount = Flux NotSpace 3 NotSpace, lineCount = 1}
```

Looks good to me! Experiment to your heart's content to convince yourself it's a lawful Monoid.

With a lawful Monoid we no longer need to worry about how we split our file up!

Before going any further, let's try using our monoid with our existing code and make sure it gets the same answers.


```haskell
module MonoidBSFold where

import Data.Char
import qualified Data.ByteString.Lazy.Char8 as BS

monoidBSFold :: FilePath -> IO Counts
monoidBSFold paths = monoidFoldFile <$> BS.readFile fp

monoidFoldFile :: BS.ByteString -> Counts
monoidFoldFile = BS.foldl' (\a b -> a <> countChar b) mempty
```

We've moved some complexity into our `Counts` type, which allows us to really simplify our implementation here. This is nice in general because it's much easier to test a single data-type rather than testing EVERYWHERE that we do this fold.

As a side benefit, this change has *somehow* sped things up even more!

We're in the ballpark now!

90 MB test file:

|          | wc        | strict-bs-fold-wc | monoid-bs-fold-wc |
| -------- | --------- | ----------------- | ----------------- |
| time     | 0.37s     | 3.41s             | 1.94s             |
| max mem. | 1.86 MB   | 5.48 MB           | 3.83 MB           |

We've knocked off a good chunk of time AND memory with this change... I'll admit I have no idea WHY, but I won't look a gift-horse in the mouth. It's possible that by using a fully strict data structure we've strictified some laziness that snuck in somewhere; but I'm really not sure. If you can see what happened please let me know!

## Inlining away!

Next in our quest, I think I'll inline some definitions! Why? Because that's just what you do when you want performance! We can use the `INLINE` pragma to tell GHC that our function is performance critical and it'll inline it for us; possibly triggering further optimizations down the line.
 
 ```haskell
monoidBSFold :: FilePath -> IO Counts
monoidBSFold paths = monoidBSFoldFile <$> BS.readFile fp
{-# INLINE monoidBSFold #-}

monoidBSFoldFile :: BS.ByteString -> Counts
monoidBSFoldFile = BS.foldl' (\a b -> a <> countChar b) mempty
{-# INLINE monoidBSFoldFile #-}
 ```

 I also went ahead and added INLINE's to our `countChar` and `flux` functions. Let's see if it made any difference:

90 MB test file:

|          | original | inlined |
| -------- | -------- | ------- |
| time     | 1.94s    | 0.47s   |
| max mem. | 3.83 MB  | 4.35 MB |

Interestingly it seems to have slashed our time down by 75%! I'm really not sure if this is a fluke, or if we stumbled upon something lucky here; but I'll take it! It's bumped up our memory usage by a smidge; but not enough for me to worry.

Here's how we compare to the C version now:

90 MB test file:

|          | wc        | inlined-monoid-bs-wc   |
| -------- | --------- | ---------------------- |
| time     | 0.37s     | 0.47s                  |
| max mem. | 1.86 MB   | 4.35 MB                |

At this point we're pretty close to parity with `wc`; but we're looking at sub-second times, so I'm going to bump up the size of our test file and run a few times to see if we can learn anything new.

I bumped up to a 543 MB plaintext file and ran it a few times in a row to get the caches warmed up. This is clearly important because my times dropped a full 33% after a few runs.  I understand my testing method isn't exactly "scientific", but it gives us a good estimate of how we're doing. Anyways, on the much larger file here's how we perform:

543 MB test file:

|          | wc        | inlined-monoid-bs-wc   |
| -------- | --------- | ---------------------- |
| time     | 2.06s     | 2.73s                  |
| max mem. | 1.85 MB   | 3.97 MB                |


From here we can see that we're actually getting pretty close! Considering we've cloned `wc` in a high-level garbage collected language in around 80 lines of code I'd say we're doing alright!

## Using our Cores

One may not expect parallelizing to multiple cores to do a whole lot since presumably this whole operation is IO bounded, but I'm going to do it anyways because I'm stubborn and bored.

We've already expressed our problem as a Monoid, which means it should be pretty trivial to split up the computation! The trick here is actually in reading in our data. If we try to read in all the data and THEN split it into chunks we'll have to load the whole file into memory at once, which is going to be REALLY bad for our memory residency, and will probably hurt our performance too! We could try **streaming** it in and splitting it that way, but then we have to **process** the first chunk before we get to the second split; and hopefully you can see the problem there. Instead I'm going to spin up a separate thread for each core we have then open a separate file handle in each of those threads. Then I'll seek each Handle to disjoint offsets and perform our operation on each non-overlapping piece of the file that way before combining the counts together.

Here's the whole thing, did I mention how much I **love** writing concurrent code in Haskell?

```haskell
import Types
import Control.Monad
import Data.Traversable
import Data.Bits
import GHC.Conc (numCapabilities)
import Control.Concurrent.Async
import Data.Foldable
import System.IO
import System.Posix.Files
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.ByteString.Internal (c2w)
import GHC.IO.Handle

multiCoreCount :: FilePath -> IO Counts
multiCoreCount fp = do
    putStrLn ("Using available cores: " <> show numCapabilities)
    size <- fromIntegral . fileSize <$> getFileStatus fp
    let chunkSize = fromIntegral (size `div` numCapabilities)
    fold <$!> (forConcurrently [0..numCapabilities-1] $ \n -> do
        -- Take all remaining bytes on the last capability due to integer division anomolies
        let limiter = if n == numCapabilities - 1
                         then id
                         else BL.take (fromIntegral chunkSize)
        let offset = fromIntegral (n * chunkSize)
        fileHandle <- openBinaryFile fp ReadMode
        hSeek fileHandle AbsoluteSeek offset
        countBytes . limiter <$!> BL.hGetContents fileHandle)
{-# INLINE handleSplitUTF #-}

countBytes :: BL.ByteString -> Counts
countBytes = BL.foldl' (\a b -> a <> countChar b) mempty
{-# INLINE countBytes #-}
```

There's a lot going on here, so I'll break it down as best I can.

We can import the number of "capabilities" available to our program (i.e. the number of cores we have access to) from `GHC.Conc`. From there, we run a fileStat on the file we want to count to get the number of bytes in the file. From there, we use integer division to determine how many bytes should be handled by each individual core. The integer division rounds the result down, so we'll have to be careful to pick up the bytes that were possibly left out later on. We then use `forConcurrently` from `Control.Concurrent.Async` to run a separate thread for each of our capabilities.

Inside each thread we check whether we're inside the thread which handls the LAST chunk of the file, if we are we should read until the EOF to pick up the leftover bytes from the earlier rounding error, otherwise we want to limit ourselves to processing only `chunkSize` bytes. Then we can calculate our offset into the file by multiplying the chunk size by our thread number. We open a binary file handle and use `hSeek` to move our handle to the starting offset for our thread. From this point we can simply read our allocated number of bytes and fold them down using the same logic as before. After we've handled each of the threads, we'll use a simple `fold` to combine the counts of each chunk into a total count.

We use `<$!>` in a few spots to add additional strictness since we want to ensure that the folding operations happen within each thread, instead of after the threads have been joined. I might go a little overboard on strictness annotations, but it's easier to add too many than it is to track down the places we've accidentally missed them.

Let's take this puppy out for a spin!

After warming up the caches I ran each of them a few times on my 4 core 2013 Macbook Pro with an SSD, and averaged the results together:

543 MB test file:

|          | wc        | multicore-wc           |
| -------- | --------- | ---------------------- |
| time     | 2.07s     | 1.23s                  |
| max mem. | 1.87 MB   | 7.06 MB                |

It seems to make a pretty big difference! We're actually going FASTER than some C code that's been hand optimized for a few decades. These results are best taken with a hefty grain of salt; it's really hard to tell what sort of caching is going on here. There are probably mutliple layers of disk caching happening. Maybe the multithreading only helps when reading files from a cache? 

I did a bit of skimming and it seems that SOME storage devices might experience a speed-up from doing file reads in parallel, some may actually slow down. Your mileage may vary. If anyone's an expert on SSDs I'd love to hear from you on this one. Regardless I'm still pretty happy with the results.

In case you're wondering, the actual `User` time for our program comes in at `4.22s` (which is split across the 4 cores), meaning our parallel program is less efficient than the simple version in terms of actual processor cycles, but the ability to use multiple cores gets the "real" wall-clock time down.

## Handling Unicode

There's something we've been avoiding so far, we've assumed every file is simple ASCII! That's really not the way the world works. A lot of documents are encoded in UTF-8 these days; which turns out to be identical to an ASCII file IFF the file only contains valid ASCII characters, however if those crazy pre-teens put some Emoji in there then it's going to screw everything up.

The problem is two-fold; firstly we currently count BYTES not CHARACTERS, because in ASCII-land they're semantically the same. With our current code, if we come across a UTF-8 encoded frowny face we're going to count it as at least 2 characters when it should only count as one. Okay, so maybe we should actually be decoding these things, but that's much easier said than done because we're splitting the file up into chunks at arbitrary byte-counts; meaning we might end up splitting that frowny face into two different chunks, leading to an invalid decoding! What a nightmare.

This is another reason why doing a multi-threaded `wc` is probably a bad idea, but I'm not so easily deterred. In order to proceed I'm going to make a few assumptions:

* Our input will be encoded using either ASCII or UTF-8 family of encodings. There are of course other popular encodings out there; but in my limited experience most modern text files prefer one of these encodings. In fact there are [entire sites](http://utf8everywhere.org/) dedicated to making `UTF-8` the one format to rule them all.
* We count only ASCII spaces and newlines as spaces and newlines; sorry `MONGOLIAN VOWEL SEPARATOR`, but you're cut from the team.

By making these two assumptions we can exploit a few details of the UTF-8 encoding scheme to solve our problem. Firstly, we know from the UTF-8 spec that it's completely back-compatible with ASCII. What this means is that every ASCII byte is encoded in UTF-8 as exactly that same byte. Secondly, we know that NO other bytes in the file will conflict in encoding with a valid ASCII byte; you can see why in a chart on the [UTF-8 wikipedia page](https://en.wikipedia.org/wiki/UTF-8). Continuation bytes start with a leading '1', and no ASCII bytes start with a '1'. 

These two facts mean we can safely leave our current 'space' detection logic the same! It's impossible for us to 'split' a space or newline because they're all encoded in a single byte, and we know we won't accidentally count some byte that's part of a different codepoint because there's no overlap in encoding for the ASCII bytes. We do however need to change our character-counting logic.

One last fact about UTF-8 is that every UTF-8 encoded codepoint contains exactly one byte from the set: `0xxxxxxx, 110xxxxx, 1110xxxx, 11110xxx`. Continuation bytes ALL start with `10`, so if we count all bytes OTHER than those starting with `10` then we'll count each code-point exactly once, even if we split a codepoint across different chunks!

All of these facts combined means we can write a per-byte monoid for counting UTF-8 codepoints OR ASCII characters all in one!

Note that technically Unicode **codepoints** are not the same as "characters", there are many codepoints like diacritics which will "fuse" themselves to be displayed as a single character, but so far as I know `wc` doesn't handle these separately either.

Actually, our current `Counts` monoid is fine, we'll just need to adapt our `countChar` function:

```haskell
import Data.Bits
import Data.ByteString.Internal (c2w)
countByte :: Char -> Counts
countByte c =
  Counts {
            -- Only count bytes at the START of a codepoint, not continuation bytes
            charCount = if (bitAt 7 && not (bitAt 6)) then 0 else 1
            , wordCount = flux c
            , lineCount = if (c == '\n') then 1 else 0
            }
    where
      bitAt = testBit (c2w c)
{-# INLINE countByte #-}
```

And that's it! Now we can handle UTF-8 or ASCII; we don't even need to know which encoding we're handling, we'll always give the right answer.

`wc`, at least the version on my Macbook, has a `-m` flag for handling multi-byte characters when counting. A few quick experiments shows that telling `wc` to handle multi-byte chars slows down the process pretty significantly (it now decodes every byte); let's see how our version does in comparison. (I've confirmed they get the same results when running on a large UTF-8 encoded document with many non-ASCII characters)

543 MB test file:

|          | wc -mwl   | multicore-utf8-wc      |
| -------- | --------- | ---------------------- |
| time     | 5.56s     | 3.07s                  |
| max mem. | 1.86 MB   | 7.52 MB                |


Just as we suspect, we come out pretty far ahead! Our new version is a bit slower than when we just counted every byte (we're now doing a few extra bit-checks), so it's probably a good idea to add a `utf` flag to our program so we can always run as fast as possible for a given input.

## Conclusions

So; how does our high-level garbage-collected runtime-based language Haskell stack up? Pretty dang well I'd say! We ended up really quite close with our single-core lazy-bytestring `wc`. Switching to a multi-core approach ultimately allowed us to pull ahead! Whether our `wc` clone is faster in practice without a warmed up disk-cache is something that should be considered, but in terms of raw performance we managed to build something faster!

Haskell as a language isn't perfect, but if I can get ball-park comparable performance to a C program while writing high-level fully type-checked code then I'll call that a win any day.
