module Main (main) where

import qualified SimplicialTests
import Test.Tasty (defaultMain)

main :: IO ()
main =
  defaultMain SimplicialTests.tests
