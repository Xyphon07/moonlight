module Main (main) where

import FiniteLatticeTests qualified
import Test.Tasty (defaultMain)

main :: IO ()
main =
  defaultMain FiniteLatticeTests.tests
