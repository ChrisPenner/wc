{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PackageImports #-}
module FileSplit where

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.ByteString.Internal (c2w)

import Types
import Control.Monad
import Control.Arrow
import Data.Traversable
import Data.Bits
import System.Posix.Files
import System.Posix.IO
import "unix-bytestring" System.Posix.IO.ByteString.Lazy
import GHC.Conc (numCapabilities)
import Control.Concurrent.Async
import Data.Functor
import Data.Foldable

filesplit :: [FilePath] -> IO [(FilePath, Counts)]
filesplit paths = for paths $ \fp -> do
    fd <- openFd fp ReadOnly Nothing defaultFileFlags
    size <- fromIntegral . fileSize <$> getFileStatus fp
    putStrLn ("Using available cores: " <> show numCapabilities)
    let chunkSize = size `div` numCapabilities
    sparks <- for [0..numCapabilities] $ \n -> do
        -- Adjust for inaccuracies in integer division; don't want to leave any unread bytes
        let readAmount = fromIntegral $ if n == numCapabilities
                                            then size - ((n - 1) * chunkSize)
                                            else chunkSize
        let offset = fromIntegral (n * chunkSize)
        async $ countBytes <$!> fdPread fd readAmount offset
    chunks <- traverse wait sparks
    closeFd fd $> (fp, fold chunks)

countBytes :: BL.ByteString -> Counts
countBytes = BL.foldl' (flip (mappend . countByte)) mempty

countByte :: Char -> Counts
countByte c =
    let bitAt = testBit (c2w c)
     in Counts {
                -- Only count bytes at the START of a codepoint, not continuations
                -- charCount = if (bitAt 7 && not (bitAt 6)) then 0 else 1
                charCount = 1
               , wordCount = flux c
               , lineCount = if (c == '\n') then 1 else 0
               }

