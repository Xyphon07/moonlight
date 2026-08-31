module Main where

import qualified TermTests
import Test.Tasty (defaultMain)

main :: IO ()
main =
  defaultMain TermTests.tests
