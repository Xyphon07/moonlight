module Main (main) where

import qualified Moonlight.Triangulation.ExactClipRetentionSpec as ExactClipRetentionSpec
import qualified Moonlight.Triangulation.LayerOperationsSpec as LayerOperationsSpec
import qualified Moonlight.Triangulation.MinkowskiSpec as MinkowskiSpec
import qualified Moonlight.Triangulation.PowerDiagramSpec as PowerDiagramSpec

main :: IO ()
main =
  sequence_
    [ ExactClipRetentionSpec.tests
    , MinkowskiSpec.tests
    , PowerDiagramSpec.tests
    , LayerOperationsSpec.tests
    ]
