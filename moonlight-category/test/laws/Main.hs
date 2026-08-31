module Main (main) where

import qualified Moonlight.Category.Effect.Laws as Laws
import Test.Tasty (defaultMain)

main :: IO ()
main =
  defaultMain Laws.tests
