module Main (main) where

import qualified Moonlight.Triangulation.ExactEmbeddingSpec as ExactEmbeddingSpec
import qualified Moonlight.Triangulation.NativeSpec as NativeSpec
import qualified Moonlight.Triangulation.OverlaySpec as OverlaySpec
import qualified Moonlight.Triangulation.RegionSpec as RegionSpec

main :: IO ()
main =
  NativeSpec.tests
    >> ExactEmbeddingSpec.tests
    >> OverlaySpec.tests
    >> RegionSpec.tests
