module Main (main) where

import qualified RepairTests
import Test.Tasty (defaultMain)

main :: IO ()
main =
  defaultMain RepairTests.tests
