module Main (main) where

import qualified Moonlight.Planar.AffineSpec as Affine
import qualified Moonlight.Planar.CurveSpec as Curve
import qualified Moonlight.Planar.CurveMeasureSpec as Measure
import qualified Moonlight.Planar.CurveSourceSpec as Source
import qualified Moonlight.Planar.CurveCertificateSpec as Certificate
import qualified Moonlight.Planar.CurveProximitySpec as Proximity
import qualified Moonlight.Planar.CurveFrameSpec as Frame
import qualified Moonlight.Planar.Curve.AuthoringSpec as Authoring
import qualified Moonlight.Planar.CurveLoweringSpec as Lowering
import qualified Moonlight.Planar.RegionAdmissionSpec as Region

main :: IO ()
main = () <$ sequenceA [Curve.tests, Measure.tests, Source.tests, Frame.tests, Authoring.tests, Affine.tests, Lowering.tests, Certificate.tests, Proximity.tests, Region.tests]
