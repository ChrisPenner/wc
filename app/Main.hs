{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import System.IO
import System.Environment
import System.Exit
import Data.Foldable
import Data.Monoid
import Text.Printf
import Types

import Simple
import Lazy
import Strict
import Parallel
import Streaming
import FileSplit
import FileSplitUTF

printResult (name, Counts{charCount, wordCount, lineCount}) = printf "%s %d %d %d\n" name  (getSum lineCount) (getFlux wordCount) (getSum charCount)

main :: IO ()
main = do
    results <- getArgs >>= \case
        ("simple": filenames) -> simple filenames
        ("lazy": filenames) -> lazyBytestream filenames
        ("strict": filenames) -> strictBytestream filenames
        ("parallel": filenames) -> parallelBytestream filenames
        ("streaming": filenames) -> streamingBytestream filenames
        ("split": filenames) -> filesplit filenames
        ("split-utf": filenames) -> filesplitUTF filenames
        _ -> hPutStrLn stderr "usage: <simple|lazy> [files...]" >> exitFailure
    traverse_ printResult results
