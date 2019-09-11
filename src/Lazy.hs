{-# LANGUAGE MultiWayIf #-}
module Lazy where

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.ByteString.Internal (c2w)

import Types
import Control.Monad
import Control.Arrow
import Data.Traversable
import Data.Bits

lazyBytestream :: [FilePath] -> IO [(FilePath, Counts)]
lazyBytestream paths = for paths $ \fp -> do
    count <- lazyBytestreamCountFile <$> BL.readFile fp
    return (fp, count)

-- Only works on ASCII BTW
lazyBytestreamCountFile :: BL.ByteString -> Counts
lazyBytestreamCountFile = BL.foldl' (flip (mappend . countChar)) mempty

-- countByte :: Char -> Counts
-- countByte c =
--     let bitAt = testBit (c2w c)
--     -- Only increment char count on single Byte chars or the FIRST byte of a multi-byte char
--      in Counts { charCount = if (bitAt 0 && not (bitAt 1)) then 0 else 1
--                , wordCount = flux c
--                , lineCount = if (c == '\n') then 1 else 0
--                }

