module Main
  ( main,
  )
where

import CohomologySpec qualified as CohomologySpec
import OutcomeSpec qualified as OutcomeSpec
import RefinementSpec qualified as RefinementSpec
import WriterSpec qualified as WriterSpec
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain (testGroup "pale-diagnostic" [WriterSpec.tests, OutcomeSpec.tests, RefinementSpec.tests, CohomologySpec.tests])
