module Main (main) where

import qualified IndexedTests
import Test.Tasty (defaultMain)

main :: IO ()
main =
  defaultMain IndexedTests.tests
