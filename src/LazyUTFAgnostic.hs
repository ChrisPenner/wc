{-# LANGUAGE MultiWayIf #-}
module LazyUTFAgnostic where

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.ByteString.Internal (c2w)

import Types
import Control.Monad
import Control.Arrow
import Data.Traversable
import Data.Bits

lazyUTF8 :: [FilePath] -> IO [(FilePath, Counts)]
lazyUTF8 paths = for paths $ \fp -> do
    count <- lazyBytestreamCountFile <$> BL.readFile fp
    return (fp, count)
{-# INLINE lazyUTF8 #-}

lazyBytestreamCountFile :: BL.ByteString -> Counts
lazyBytestreamCountFile = BL.foldl' (\acc c -> acc <> countByteUTF8 c) mempty
{-# INLINE lazyBytestreamCountFile #-}
