module InlinedMonoidBSFold where

import Types
import Data.Traversable
import Data.Monoid
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as BS

inlinedMonoidBSFold :: [FilePath] -> IO [(FilePath, Counts)]
inlinedMonoidBSFold paths = for paths $ \fp -> do
    count <- countFile <$> BS.readFile fp
    return (fp, count)
{-# INLINE inlinedMonoidBSFold #-}

countFile :: BS.ByteString -> Counts
countFile = BS.foldl' (\a b -> a <> countChar b) mempty
{-# INLINE countFile #-}
