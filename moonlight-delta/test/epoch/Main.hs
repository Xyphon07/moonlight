module Main (main) where

import qualified EpochTests
import Test.Tasty (defaultMain)

main :: IO ()
main =
  defaultMain EpochTests.tests
