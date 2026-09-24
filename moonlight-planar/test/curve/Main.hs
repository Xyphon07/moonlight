module Main (main) where

import qualified Moonlight.Planar.AffineSpec as Affine
import qualified Moonlight.Planar.CurveSpec as Curve
import qualified Moonlight.Planar.Curve.AuthoringSpec as Authoring
import qualified Moonlight.Planar.CurveLoweringSpec as Lowering
import qualified Moonlight.Planar.RegionAdmissionSpec as Region

main :: IO ()
main = () <$ sequenceA [Curve.tests, Authoring.tests, Affine.tests, Lowering.tests, Region.tests]
