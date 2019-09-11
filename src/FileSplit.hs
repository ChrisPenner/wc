{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ViewPatterns #-}

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
import GHC.Exts
import Control.Concurrent.Async
import Data.Functor
import Data.Foldable

filesplit :: [FilePath] -> IO [(FilePath, Counts)]
filesplit paths = for paths $ \fp -> do
    fd <- openFd fp ReadOnly Nothing defaultFileFlags
    size <- fromIntegral . fileSize <$> getFileStatus fp
    putStrLn ("Using available cores: " <> show numCapabilities)
    let chunkSize = size `div` numCapabilities
    result <- fold <$!> (forConcurrently [0..numCapabilities-1] $ \n -> do
        -- Adjust for inaccuracies in integer division; don't want to leave any unread bytes
        let readAmount = fromIntegral $ if n == (numCapabilities - 1)
                                            then size - (n * chunkSize)
                                            else chunkSize
        let offset = fromIntegral (n * chunkSize)
        countBytes <$!> fdPread fd readAmount offset)
    closeFd fd $> (fp, result)

countBytes :: BL.ByteString -> Counts
countBytes = BL.foldl' (flip ((<>) . countByte)) mempty

countByte :: Char -> Counts
countByte c =
     Counts {
                -- Only count bytes at the START of a codepoint, not continuations
                -- charCount =
                --     case unsafeCoerce# c of
                --         W# w -> case w `and#` 0b11000000## of
                --                 0b10000000## -> 0
                --                 _            -> 1
                charCount = if (bitAt 7 && not (bitAt 6)) then 0 else 1
                -- charCount = 1
               , wordCount = flux c
               , lineCount = if (c == '\n') then 1 else 0
               }
    where
      bitAt = testBit (c2w c)
