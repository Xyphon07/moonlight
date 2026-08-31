module Main (main) where

import qualified CoreTests
import Test.Tasty (defaultMain)

main :: IO ()
main =
  defaultMain CoreTests.tests
