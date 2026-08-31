module Main (main) where

import qualified Moonlight.Triangulation.AlgebraSpec as AlgebraSpec
import qualified Moonlight.Triangulation.RegionAlgebraSpec as RegionAlgebraSpec
import qualified Moonlight.Triangulation.ScheduleAgreementSpec as ScheduleAgreementSpec
import qualified Moonlight.Triangulation.ValuationSpec as ValuationSpec

main :: IO ()
main = do
  AlgebraSpec.tests
  RegionAlgebraSpec.tests
  ValuationSpec.tests
  ScheduleAgreementSpec.tests
