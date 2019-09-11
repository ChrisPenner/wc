{-# LANGUAGE MultiWayIf #-}
module Streaming where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Internal (c2w)

import Types
import Control.Monad
import Control.Arrow
import Data.Traversable
import Data.Bits
import Data.Functor.Identity
import qualified Streamly as S
import qualified Streamly.Prelude as S

streamingBytestream :: [FilePath] -> IO [(FilePath, Counts)]
streamingBytestream paths = for paths $ \fp -> do
    count <- BL.readFile fp >>= streamingCountFile
    return (fp, count)

streamingCountFile :: BL.ByteString -> IO Counts
streamingCountFile bl = S.foldl' (flip (mappend . countBytes)) mempty S.|$. S.maxThreads 4 (S.foldMapWith S.parallel return (BL.toChunks bl))

countBytes :: BS.ByteString -> Counts
countBytes = BS.foldl' (flip (mappend . countByte)) mempty

countByte :: Char -> Counts
countByte c =
    let bitAt = testBit (c2w c)
     in Counts {
                -- Only count bytes at the START of a codepoint, not continuations
                charCount = if (bitAt 7 && not (bitAt 6)) then 0 else 1
               , wordCount = flux c
               , lineCount = if (c == '\n') then 1 else 0
               }

