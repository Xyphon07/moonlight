module Main (main) where

import qualified PatchTests
import Test.Tasty (defaultMain)

main :: IO ()
main =
  defaultMain PatchTests.tests
