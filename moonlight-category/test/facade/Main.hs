module Main (main) where

import qualified FacadeTests
import Test.Tasty (defaultMain)

main :: IO ()
main =
  defaultMain FacadeTests.tests
