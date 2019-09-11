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

filesplit :: [FilePath] -> IO [(FilePath, Counts)]
filesplit paths = for paths $ \fp -> do
    fd <- openFd fp ReadOnly Nothing defaultFileFlags
    size <- fileSize <$> getFileStatus fp
    -- putStrLn ("num cores: " <> show numCapabilities)
    -- print $ (fromIntegral size `div` fromIntegral numCapabilities :: Integer)
    let half = fromIntegral size `div` 2
    firstChunkA <- async $ fileSplitCount <$!> fdPread fd half 0
    secondChunkA <- async $ fileSplitCount <$!> fdPread fd (fromIntegral size - half) (fromIntegral half)
    (firstCount, secondCount) <- waitBoth firstChunkA secondChunkA
    closeFd fd
    return (fp, firstCount <> secondCount)

fileSplitCount :: BL.ByteString -> Counts
fileSplitCount = BL.foldl' (flip (mappend . countByte)) mempty

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

