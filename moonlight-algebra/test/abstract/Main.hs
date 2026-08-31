module Main (main) where

import AbstractTests qualified
import Test.Tasty (defaultMain)

main :: IO ()
main =
  defaultMain AbstractTests.tests
