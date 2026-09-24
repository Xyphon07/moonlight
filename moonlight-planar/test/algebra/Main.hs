module Main (main) where

import qualified Moonlight.Planar.AlgebraSpec as AlgebraSpec
import qualified Moonlight.Planar.RegionAlgebraSpec as RegionAlgebraSpec
import qualified Moonlight.Planar.ScheduleAgreementSpec as ScheduleAgreementSpec
import qualified Moonlight.Planar.ValuationSpec as ValuationSpec

main :: IO ()
main = do
  AlgebraSpec.tests
  RegionAlgebraSpec.tests
  ValuationSpec.tests
  ScheduleAgreementSpec.tests
