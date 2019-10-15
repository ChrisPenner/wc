{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Hedgehog
import qualified Hedgehog.Gen           as Gen
import qualified Hedgehog.Range         as Range
import           Control.Monad
import           System.Process
import           Control.Monad.IO.Class
import           Types
import Streaming

import Simple

main :: IO ()
main = void propertyTests

propertyTests :: IO Bool
propertyTests = checkSequential $$(discover)

prop_wc_compare :: Property
prop_wc_compare =
  property $ do
    str <- forAll longString
    wcResult <- liftIO $ wc str
    toTuple (simpleCountFile str) === wcResult

longString :: Gen String
longString = do
    Gen.string (Range.linear 0 1000) Gen.ascii

wc :: String -> IO (Int, Int, Int)
wc s = do
    output <- readProcess "wc" [] s
    let [chars, words, lines] = T.pack output ^.. regex [rx|\d+|] . match . unpacked . _Show
    return (chars, words, lines)



