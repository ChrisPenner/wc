{-# LANGUAGE BangPatterns #-}
module SimpleFold where

import Types
import Data.Traversable
import Data.Monoid
import Data.List
import Data.Char

simpleFold :: [FilePath] -> IO [(FilePath, Counts)]
simpleFold paths = for paths $ \fp -> do
    count <- fromTuple . simpleFoldCountFile <$> readFile fp
    return (fp, count)

simpleFoldCountFile :: String -> (Int, Int, Int)
simpleFoldCountFile s =
    let (cs, ws, ls, _) = foldl' go (0, 0, 0, False) s
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

