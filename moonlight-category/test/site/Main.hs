module Main (main) where

import qualified SiteTests
import Test.Tasty (defaultMain)

main :: IO ()
main =
  defaultMain SiteTests.tests
