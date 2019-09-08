module Simple where

import Types

import Control.Monad
import Control.Arrow
import Data.Traversable

simple :: [FilePath] -> IO [(FilePath, Counts)]
simple paths = for paths $ \fp -> do
    count <- simpleCountFile <$> readFile fp
    return (fp, count)

simpleCountFile :: String -> Counts
simpleCountFile = foldMap countChar
