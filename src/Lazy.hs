module Lazy where

import qualified Data.ByteString.Lazy.Char8 as BL

import Types
import Control.Monad
import Control.Arrow
import Data.Traversable

lazyBytestream :: [FilePath] -> IO [(FilePath, Counts)]
lazyBytestream paths = for paths $ \fp -> do
    count <- lazyBytestreamCountFile <$> BL.readFile fp
    return (fp, count)

lazyBytestreamCountFile :: BL.ByteString -> Counts
lazyBytestreamCountFile = BL.foldl' (flip (mappend . countChar)) mempty
