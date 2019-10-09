{-# LANGUAGE BangPatterns #-}
module SimpleBSFold where

import Types
import Data.Traversable
import Data.Monoid
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as BS

simpleBSFold :: [FilePath] -> IO [(FilePath, Counts)]
simpleBSFold paths = for paths $ \fp -> do
    count <- fromTuple . simpleFoldCountFile <$> BS.readFile fp
    return (fp, count)
{-# INLINE simpleBSFold #-}

simpleFoldCountFile :: BS.ByteString -> (Int, Int, Int)
simpleFoldCountFile s =
    let (cs, ws, ls, _) = BS.foldl' go (0, 0, 0, False) s
     in (cs, ws, ls)
{-# INLINE simpleFoldCountFile #-}

go :: (Int, Int, Int, Bool) -> Char -> (Int, Int, Int, Bool)
go (!cs, !ws, !ls, !wasSpace) c =
    let addLine | c == '\n' = 1
                | otherwise = 0
        addWord | wasSpace = 0
                | isSpace c = 1
                | otherwise = 0
        in (cs + 1, ws + addWord, ls + addLine, isSpace c)
{-# INLINE go #-}
