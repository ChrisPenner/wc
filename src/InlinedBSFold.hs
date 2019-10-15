{-# LANGUAGE BangPatterns #-}
module InlinedBSFold where

import Types
import Data.Traversable
import Data.Monoid
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as BS

inlinedBSFold :: [FilePath] -> IO [(FilePath, Counts)]
inlinedBSFold paths = for paths $ \fp -> do
    count <- fromTuple . countFile <$> BS.readFile fp
    return (fp, count)
{-# INLINE inlinedBSFold #-}

countFile :: BS.ByteString -> (Int, Int, Int)
countFile s =
    let (cs, ws, ls, _) = BS.foldl' go (0, 0, 0, False) s
     in (cs, ws, ls)
{-# INLINE countFile #-}

go :: (Int, Int, Int, Bool) -> Char -> (Int, Int, Int, Bool)
go (!cs, !ws, !ls, !wasSpace) c =
    let addLine | c == '\n' = 1
                | otherwise = 0
        addWord | wasSpace = 0
                | isSpace c = 1
                | otherwise = 0
        in (cs + 1, ws + addWord, ls + addLine, isSpace c)
{-# INLINE go #-}
