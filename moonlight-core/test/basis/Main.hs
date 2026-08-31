module Main where

import qualified BasisTests
import Test.Tasty (defaultMain)

main :: IO ()
main =
  defaultMain BasisTests.tests
