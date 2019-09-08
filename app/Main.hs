{-# LANGUAGE NamedFieldPuns #-}
module Main where

import System.Environment
import Data.Foldable
import Data.Monoid

import Simple
import Types
import Text.Printf

printResult (name, Counts{charCount, wordCount, lineCount}) = printf "%s %d %d %d" name  (getSum lineCount) (getFlux wordCount) (getSum charCount)

main :: IO ()
main = do
    filenames <- getArgs
    results <- simple filenames
    traverse_ printResult results
