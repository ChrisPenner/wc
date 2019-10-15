module HandleSplitUTF where

import Types
import Control.Monad
import Data.Traversable
import GHC.Conc (numCapabilities)
import Control.Concurrent.Async
import Data.Foldable
import System.IO
import System.Posix.Files
import qualified Data.ByteString.Lazy.Char8 as BL
import GHC.IO.Handle


handleSplitUTF :: [FilePath] -> IO [(FilePath, Counts)]
handleSplitUTF paths = for paths $ \fp -> do
    putStrLn ("Using available cores: " <> show numCapabilities)
    size <- fromIntegral . fileSize <$> getFileStatus fp
    let chunkSize = fromIntegral (size `div` numCapabilities)
    result <- fold <$!> (forConcurrently [0..numCapabilities-1] $ \n -> do
        -- Take all remaining bytes on the last capability due to integer division anomolies
        let limiter = if n == numCapabilities - 1
                         then id
                         else BL.take (fromIntegral chunkSize)
        let offset = fromIntegral (n * chunkSize)
        fileHandle <- openBinaryFile fp ReadMode
        hSeek fileHandle AbsoluteSeek offset
        countBytes . limiter <$!> BL.hGetContents fileHandle)
    return (fp, result)
{-# INLINE handleSplitUTF #-}

countBytes :: BL.ByteString -> Counts
countBytes = BL.foldl' (\acc next -> acc <> countByteUTF8 next) mempty
{-# INLINE countBytes #-}
