module Lazy where

import qualified Data.ByteString.Lazy.Char8 as BL

import Types
import Data.Traversable

lazyBytestream :: [FilePath] -> IO [(FilePath, Counts)]
lazyBytestream paths = for paths $ \fp -> do
    count <- lazyBytestreamCountFile <$> BL.readFile fp
    return (fp, count)
{-# INLINE lazyBytestream #-}

-- Only works on ASCII BTW
lazyBytestreamCountFile :: BL.ByteString -> Counts
lazyBytestreamCountFile = BL.foldl' (flip (mappend . countChar)) mempty
{-# INLINE lazyBytestreamCountFile #-}
