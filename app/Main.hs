module Main where

import System.Environment
import Data.Foldable

import Simple

main :: IO ()
main = do
    filenames <- getArgs
    results <- simple filenames
    traverse_ print results
