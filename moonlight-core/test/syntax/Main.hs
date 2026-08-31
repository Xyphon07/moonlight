module Main where

import qualified SyntaxTests
import Test.Tasty (defaultMain)

main :: IO ()
main =
  defaultMain SyntaxTests.tests
