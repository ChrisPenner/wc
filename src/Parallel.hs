{-# LANGUAGE MultiWayIf #-}
module Parallel where

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
import Control.Parallel.Strategies
import Control.Parallel
import qualified Data.Foldable as F

parallelBytestream :: [FilePath] -> IO [(FilePath, Counts)]
parallelBytestream paths = for paths $ \fp -> do
    count <- parallelCountFile <$> BL.readFile fp
    return (fp, count)

parallelCountFile :: BL.ByteString -> Counts
parallelCountFile bl = F.foldl' (<>) mempty . withStrategy (evalBuffer 100 rseq) $ fmap countBytes chunks
    where
      chunks = BL.toChunks bl
      -- sol1 = undefined
      --parMap rseq countBytes chunks
      -- sol2 = parBuffer

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

