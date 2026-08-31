module Main where

import qualified NumericTests
import Test.Tasty (defaultMain)

main :: IO ()
main =
  defaultMain NumericTests.tests
