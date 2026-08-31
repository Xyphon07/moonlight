-- | Compile every test slice against the union of their dependencies. Empty
-- imports make module and instance collisions observable without executing the
-- behavioral suites twice.
module Main (main) where

import Moonlight.Triangulation.AlgebraSpec ()
import Moonlight.Triangulation.ExactClipRetentionSpec ()
import Moonlight.Triangulation.LayerOperationsSpec ()
import Moonlight.Triangulation.MinkowskiSpec ()
import Moonlight.Triangulation.NativeSpec ()
import Moonlight.Triangulation.ParallelSpec ()
import Moonlight.Triangulation.PowerDiagramSpec ()
import Moonlight.Triangulation.RegionAlgebraSpec ()
import Moonlight.Triangulation.ScheduleAgreementSpec ()
import Moonlight.Triangulation.SerializationSpec ()
import Moonlight.Triangulation.ValuationSpec ()

main :: IO ()
main = pure ()
