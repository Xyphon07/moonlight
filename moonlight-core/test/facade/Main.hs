module Main where

import qualified PublicSurfaceSpec
import Test.Tasty (defaultMain)

main :: IO ()
main =
  defaultMain PublicSurfaceSpec.tests
