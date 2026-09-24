module Main (main) where

import qualified Moonlight.Planar.ExactClipRetentionSpec as ExactClipRetentionSpec
import qualified Moonlight.Planar.ExactIncidencePublicationSpec as ExactIncidencePublicationSpec
import qualified Moonlight.Planar.ConvexSpec as ConvexSpec
import qualified Moonlight.Planar.InCircleFixedWidthSpec as InCircleFixedWidthSpec
import qualified Moonlight.Planar.LayerOperationsSpec as LayerOperationsSpec
import qualified Moonlight.Planar.MinkowskiSpec as MinkowskiSpec
import qualified Moonlight.Planar.RegularTopologySpec as RegularTopologySpec
import qualified Moonlight.Planar.RegularEditSpec as RegularEditSpec
import qualified Moonlight.Planar.RegularAlphaSpec as RegularAlphaSpec
import qualified Moonlight.Planar.PowerDiagramSpec as PowerDiagramSpec
import qualified Moonlight.Planar.PowerMassSpec as PowerMassSpec

main :: IO ()
main =
  sequence_
    [ ExactClipRetentionSpec.tests
    , ExactIncidencePublicationSpec.tests
    , ConvexSpec.tests
    , InCircleFixedWidthSpec.tests
    , MinkowskiSpec.tests
    , RegularTopologySpec.tests
    , RegularEditSpec.tests
    , RegularAlphaSpec.tests
    , PowerDiagramSpec.tests
    , putStrLn "power diagram: ok"
    , PowerMassSpec.tests
    , LayerOperationsSpec.tests
    ]
