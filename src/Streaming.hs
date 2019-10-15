{-# LANGUAGE CPP #-}

module Streaming where

import Control.Arrow
import Control.Monad
import Data.Bits
import Data.Char
import Data.Functor.Identity
import Data.Traversable
import Data.Word
import System.IO
import Types
import qualified Streamly as S
import qualified Streamly.Data.String as S
import qualified Streamly.Internal.FileSystem.Handle as FH
import qualified Streamly.Internal.Memory.Array as A
import qualified Streamly.Prelude as S


-- #define UTF8
#define PARALLEL

{-# INLINE streamingBytestream #-}
streamingBytestream :: [FilePath] -> IO [(FilePath, Counts)]
streamingBytestream paths = for paths $ \fp -> do
    src <- openFile fp ReadMode
    count <-
          S.foldl' (flip mappend) mempty
#ifdef PARALLEL
        $ S.asyncly
        $ S.maxThreads 8
#endif
        $ S.mapM countBytes
        $ FH.toStreamArraysOf 1024000 src
    return (fp, count)
    where
    countBytes =
          S.foldl' (flip (mappend . countByte)) mempty
#if !defined(UTF8) || defined(PARALLEL)
        . S.decodeChar8
#else
        . S.decodeUtf8Lenient
#endif
        . A.toStream

{-# INLINE countByte #-}
countByte :: Char -> Counts
#if defined(UTF8) && defined(PARALLEL)
countByte c =
    let bitAt = testBit (ord c)
    in Counts
        {
          -- Only count bytes at the START of a codepoint, not continuations
          charCount = if (bitAt 7 && not (bitAt 6)) then 0 else 1
        , wordCount = flux c
        , lineCount = if (c == '\n') then 1 else 0
        }
#else
countByte = countChar
#endif
