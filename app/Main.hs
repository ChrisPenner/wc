{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import System.IO
import System.Environment
import System.Exit
import Data.Foldable
import Text.Printf
import Types

import Stupid
import Simple
import SimpleFold
import SimpleBSFold
import MonoidBSFold
import Lazy
import Strict
import Parallel
import Streaming
import FileSplit
import FileSplitUTF
import HandleSplitUTF

printResult (name, Counts{charCount, wordCount, lineCount}) = printf "%d %d %d %s\n" lineCount (getFlux wordCount) charCount name

main :: IO ()
main = do
    results <- getArgs >>= \case
        ("handle-utf": filenames) -> handleSplitUTF filenames
        ("lazy": filenames) -> lazyBytestream filenames
        ("simple-bs-fold": filenames) -> simpleBSFold filenames
        ("monoid-bs-fold": filenames) -> monoidBSFold filenames
        ("stupid": filenames) -> stupid filenames
        ("simple": filenames) -> simple filenames
        ("simple-fold": filenames) -> simpleFold filenames
        ("strict": filenames) -> strictBytestream filenames
        ("parallel": filenames) -> parallelBytestream filenames
        ("streaming": filenames) -> streamingBytestream filenames
        ("split": filenames) -> filesplit filenames
        ("split-utf": filenames) -> filesplitUTF filenames
        _ -> hPutStrLn stderr "usage: <simple|lazy> [files...]" >> exitFailure
    traverse_ printResult results
