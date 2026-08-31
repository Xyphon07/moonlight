module Main (main) where

import qualified CrossCarrierLaws
import Test.Tasty (defaultMain)

main :: IO ()
main =
  defaultMain CrossCarrierLaws.tests
