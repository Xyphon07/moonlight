module Main
  ( main,
  )
where

import DisciplineSpec qualified as DisciplineSpec
import RegistrySpec qualified as RegistrySpec
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain (testGroup "pale-test-surface" [DisciplineSpec.tests, RegistrySpec.tests])
