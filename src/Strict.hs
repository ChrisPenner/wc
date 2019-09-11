module Strict where

import qualified Data.ByteString.Char8 as BS

import Types
import Control.Monad
import Control.Arrow
import Data.Traversable

strictBytestream :: [FilePath] -> IO [(FilePath, Counts)]
strictBytestream paths = for paths $ \fp -> do
    count <- strictBytestreamCountFile <$> BS.readFile fp
    return (fp, count)

strictBytestreamCountFile :: BS.ByteString -> Counts
strictBytestreamCountFile = BS.foldl' (flip (mappend . countChar)) mempty
