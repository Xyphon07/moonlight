module Main where

import qualified SolverTests
import Test.Tasty (defaultMain)

main :: IO ()
main =
  defaultMain SolverTests.tests
