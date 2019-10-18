module StreamingUTF where

import Types
import Data.Traversable
import GHC.Conc (numCapabilities)
import System.IO (openFile, IOMode(..))
import qualified Streamly as S
import qualified Streamly.Data.String as S
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Memory.Array as A
import qualified Streamly.Internal.FileSystem.Handle as FH

streamingUTF :: [FilePath] -> IO [(FilePath, Counts)]
streamingUTF paths = for paths $ \fp -> do
    src <- openFile fp ReadMode
    count <-
          S.foldl' mappend mempty
        $ S.aheadly
        $ S.maxThreads numCapabilities
        $ S.mapM countBytes
        $ FH.toStreamArraysOf 1024000 src
    return (fp, count)
    where
    countBytes =
          S.foldl' (\acc c -> acc <> countByteUTF8 c) mempty
        . S.decodeChar8
        . A.toStream

{-# INLINE streamingUTF #-}
