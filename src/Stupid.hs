module Stupid where

import Types

import Data.Traversable
import Data.Monoid

stupid :: [FilePath] -> IO [(FilePath, Counts)]
stupid paths = for paths $ \fp -> do
    count <- stupidCountFile <$> readFile fp
    return (fp, count)

stupidCountFile :: String -> Counts
stupidCountFile s = fromTuple (length s, length (words s), length (lines s))
