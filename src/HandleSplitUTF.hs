{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module HandleSplitUTF where

import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Internal (c2w)
import GHC.IO.Handle

import Types
import Control.Monad
import Control.Arrow
import Data.Traversable
import Data.Bits
import GHC.Conc (numCapabilities)
import GHC.Exts
import Control.Concurrent.Async
import Data.Functor
import Data.Foldable
import System.IO
import System.Posix.Files
import System.Posix.IO

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

countBytes :: BL.ByteString -> Counts
countBytes = BLC.foldl' (\acc next -> acc <> countByte next) mempty

countByte :: Char -> Counts
countByte c =
     Counts {
                -- Only count bytes at the START of a codepoint, not continuations
                charCount = if (bitAt 7 && not (bitAt 6)) then 0 else 1
                -- charCount = 1
               , wordCount = flux c
               , lineCount = if (c == '\n') then 1 else 0
               }
    where
      bitAt = testBit (c2w c)
