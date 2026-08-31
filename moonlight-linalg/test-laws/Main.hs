module Main (main) where

import Moonlight.LinAlg.Effect.Laws qualified
import Test.Tasty (defaultMain)

main :: IO ()
main =
  defaultMain Moonlight.LinAlg.Effect.Laws.tests
