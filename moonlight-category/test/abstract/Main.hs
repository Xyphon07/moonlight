module Main (main) where

import qualified AbstractTests
import Test.Tasty (defaultMain)

main :: IO ()
main =
  defaultMain AbstractTests.tests
