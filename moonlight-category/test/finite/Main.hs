module Main (main) where

import qualified FiniteTests
import Test.Tasty (defaultMain)

main :: IO ()
main =
  defaultMain FiniteTests.tests
