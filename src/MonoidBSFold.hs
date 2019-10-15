module MonoidBSFold where

import Types
import Data.Traversable
import Data.Monoid
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as BS

monoidBSFold :: [FilePath] -> IO [(FilePath, Counts)]
monoidBSFold paths = for paths $ \fp -> do
    count <- monoidFoldFile <$> BS.readFile fp
    return (fp, count)

monoidFoldFile :: BS.ByteString -> Counts
monoidFoldFile = BS.foldl' (\a b -> a <> countChar b) mempty
