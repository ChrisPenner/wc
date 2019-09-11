{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import System.IO
import System.Environment
import System.Exit
import Data.Foldable
import Data.Monoid

import Simple
import Lazy
import Strict
import Types
import Text.Printf

printResult (name, Counts{charCount, wordCount, lineCount}) = printf "%s %d %d %d\n" name  (getSum lineCount) (getFlux wordCount) (getSum charCount)

main :: IO ()
main = do
    results <- getArgs >>= \case
        ("simple": filenames) -> simple filenames
        ("lazy": filenames) -> lazyBytestream filenames
        ("strict": filenames) -> strictBytestream filenames
        _ -> hPutStrLn stderr "usage: <simple|lazy> [files...]" >> exitFailure
    traverse_ printResult results
