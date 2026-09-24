module Moonlight.Planar.LayoutSpec (tests) where

import Control.Monad (unless)
import Data.Foldable (toList, traverse_)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isNothing)
import qualified Data.Sequence as Seq
import Moonlight.Planar.Affine
import Moonlight.Planar.Curve
import Moonlight.Planar.Curve.Authoring (polygonTrail)
import Moonlight.Planar.Exact
import Moonlight.Planar.Illustration
import Moonlight.Planar.Illustration.Layout
import Moonlight.Planar.Illustration.Motif

data Port = Root

tests :: IO ()
tests = do
  let empty = geometryEnvelope (mempty :: Picture ())
      e = geometryEnvelope art
  check "empty support/bounds" (geometrySupport (ExactVector 1 0) empty == Nothing
    && isNothing (geometryBounds empty))
  check "zero direction of nonempty envelope" (geometrySupport (ExactVector 0 0) e == Just 0)
  traverse_ (envelopeLaws e) directions
  let frame = affine2 (ExactVector 2 1) (ExactVector (-1) 3) (ExactVector 7 11)
      collapse = affine2 (ExactVector 0 0) (ExactVector 0 0) (ExactVector 4 9)
      shear = affine2 (ExactVector 1 0) (ExactVector 3 1) (ExactVector 0 0)
      unshear = affine2 (ExactVector 1 0) (ExactVector (-3) 1) (ExactVector 0 0)
  traverse_ (affineLaw e frame) directions
  traverse_ (affineLaw e collapse) directions
  traverse_ (\u -> check "nested inverse shear must not rebox"
    (geometrySupport u (geometryEnvelope (place unshear (place shear art))) == geometrySupport u e)) directions
  check "singular map retains a point"
    (boundsObservation (transformGeometryEnvelope collapse e) == Just (4,9,4,9))
  let clipped = clip NonZero (polygonTrail (exactPoint 0 0 :| [exactPoint 1 0,exactPoint 0 1]) :| []) art
  traverse_ (\u -> check "clips opacity and annotations do not shrink geometry"
    (geometrySupport u (geometryEnvelope (annotate () (opacity unitZero clipped))) == geometrySupport u e)) directions
  check "stroke width and units excluded"
    (boundsObservation (geometryEnvelope (stroke (pen positiveOne LocalUnits) samplePath))
      == boundsObservation (geometryEnvelope (stroke (pen positiveTwo OutputUnits) samplePath)))
  check "empty path differs from located empty trail"
    (boundsObservation (geometryEnvelope (stroke (pen positiveOne LocalUnits) mempty)) == Nothing
      && boundsObservation (geometryEnvelope (stroke (pen positiveOne LocalUnits)
        (path (Seq.singleton (OpenSubpath (locate (exactPoint 9 4) mempty))))))
          == Just (9,4,9,4))
  traverse_ containment shapes
  layoutLaws
  putStrLn "layout: exact support, conservative geometry, coherent linear arrangement ok"

directions :: [ExactVector]
directions = [ExactVector 1 0, ExactVector (-1) 0, ExactVector 0 1,
  ExactVector 0 (-1), ExactVector 2 3, ExactVector (-3) 2, ExactVector 0 0]

boundsObservation :: GeometryEnvelope -> Maybe (ExactRational, ExactRational, ExactRational, ExactRational)
boundsObservation = fmap (\bounds -> (boundsMinimumX bounds, boundsMinimumY bounds,
  boundsMaximumX bounds, boundsMaximumY bounds)) . geometryBounds

envelopeLaws :: GeometryEnvelope -> ExactVector -> IO ()
envelopeLaws e u = do
  let f = geometryEnvelope (motifPicture (box 13))
      g = geometryEnvelope (motifPicture (box 2))
      support = geometrySupport u
  check "envelope identity" (support (mempty <> e) == support e && support (e <> mempty) == support e)
  check "envelope observational associativity" (support ((e <> f) <> g) == support (e <> (f <> g)))
  check "envelope observational commutativity/idempotence" (support (e <> f) == support (f <> e)
    && support (e <> e) == support e)
  check "union support maximum" (support (e <> f) == max (support e) (support f))

affineLaw :: GeometryEnvelope -> Affine2 -> ExactVector -> IO ()
affineLaw e frame u@(ExactVector ux uy) = do
  let (ExactVector a b, ExactVector c d, ExactVector tx ty) = affineColumns frame
      expected = (+ (ux * tx + uy * ty)) <$> geometrySupport (ExactVector (a*ux+b*uy) (c*ux+d*uy)) e
  check "exact affine dual support law" (geometrySupport u (transformGeometryEnvelope frame e) == expected)
  check "picture and envelope transform agree"
    (geometrySupport u (geometryEnvelope (place frame art)) == expected)

shapes :: [CurveStep]
shapes = [curveStep line (ExactVector 5 0)
  ,curveStep (quadratic (ExactVector 3 8)) (ExactVector 5 0)
  ,curveStep (cubic (ExactVector (-4) 9) (ExactVector 8 (-3))) (ExactVector 5 0)
  ,curveStep (rationalQuadratic (ExactVector 4 8) positiveOne positiveTwo) (ExactVector 5 0)]

containment :: CurveStep -> IO ()
containment step = do
  let picture :: Picture ()
      picture = stroke (pen positiveOne LocalUnits)
        (path (Seq.singleton (OpenSubpath (locate (exactPoint 0 0) (openTrail (Seq.singleton step))))))
      e = geometryEnvelope picture
      samples = fmap (\t -> evaluateStep t step) [unitZero,unitHalf,unitOne]
  traverse_ (\u -> traverse_ (\p -> check "control hull and evaluation witnesses"
    (Just (dot u p) <= geometrySupport u e)) (toList (stepControlPoints step) <> samples)) directions
 where
  dot (ExactVector x y) (ExactVector a b) = x*a+y*b

layoutLaws :: IO ()
layoutLaws = do
  let original = box 10
      aligned = alignMotif Horizontal unitHalf 25 original
      empty = motif mempty (const (translationAffineIso2 (ExactVector 99 17))) :: Motif Port ()
      baseline = arrangeMotifs Horizontal 5 (Seq.fromList [box 10,empty,box 20,box 3])
      wider = arrangeMotifs Horizontal 5 (Seq.fromList [box 10,empty,box 40,box 3])
      ports :: Foldable f => f (Motif Port ()) -> [ExactPoint]
      ports = fmap (\value -> transformPoint (affineIsoMap (motifPort value Root)) (exactPoint 0 0)) . toList
  check "midpoint alignment and attached port" (ports [aligned] == [exactPoint 20 0])
  check "arrangement skips empties" (ports baseline == [exactPoint 0 0,exactPoint 99 17,exactPoint 15 0,exactPoint 40 0])
  check "width edit changes only subsequent position" (ports wider == [exactPoint 0 0,exactPoint 99 17,exactPoint 15 0,exactPoint 60 0])
  check "negative gap intentional overlap" (ports (arrangeMotifs Horizontal (-3) (Seq.fromList [box 10,box 20]))
    == [exactPoint 0 0,exactPoint 7 0])
  check "vertical maximum alignment" (ports [alignMotif Vertical unitOne 30 original] == [exactPoint 0 22])
  traverse_ (\value -> case geometryBounds (geometryEnvelope (motifPicture value)) of
    Nothing -> pure ()
    Just bounds -> check "ports follow laid-out geometry"
      (transformPoint (affineIsoMap (motifPort value Root)) (exactPoint 0 0)
        == exactPoint (boundsMinimumX bounds) (boundsMinimumY bounds))) baseline
  check "empty layout identity" (ports [alignMotif Horizontal unitOne 0 empty] == ports [empty])

box :: ExactRational -> Motif Port ()
box width = motif (fill NonZero (Solid (RGB 1 2 3))
  (polygonTrail (exactPoint 0 0 :| [exactPoint width 0,exactPoint width 8,exactPoint 0 8]) :| []))
  (const identityAffineIso2)

pen :: PositiveExact -> StrokeUnits -> StrokeStyle
pen width units = StrokeStyle (Solid (RGB 1 2 3)) width units SquareCap (MiterJoin miterLimitFour)

samplePath :: Path
samplePath = path (Seq.singleton (ClosedSubpath (ellipse (ExactVector 8 2) (ExactVector 3 6))))

art :: Picture ()
art = stroke (pen positiveOne LocalUnits) samplePath

check :: String -> Bool -> IO ()
check label condition = unless condition (fail label)
