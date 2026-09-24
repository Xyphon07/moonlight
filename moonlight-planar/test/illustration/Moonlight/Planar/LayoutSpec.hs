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
  traverse_ (\picture -> check "layout bounds reducer agrees with envelope bounds"
    (layoutBounds picture == boundsObservation (geometryEnvelope picture)))
    ([mempty, art, place frame art, place collapse art, place unshear (place shear art)
      , annotate () (opacity unitZero clipped), motifPicture (box 13), strokeOf mempty
      , strokeOf (path (Seq.singleton (OpenSubpath (locate (exactPoint 9 4) mempty)))) ]
      <> fmap (strokeOf . path . Seq.singleton . OpenSubpath . locate (exactPoint 0 0) . openTrail . Seq.singleton) shapes)
  traverse_ (controlLaws frame shear unshear collapse) controlFixtures
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

-- Bounds as layout sees them. Aligning an edge to a target moves the identity
-- port by the target minus that edge, so the edge read back is target-independent;
-- empty geometry does not move for any target.
layoutBounds :: Picture () -> Maybe (ExactRational, ExactRational, ExactRational, ExactRational)
layoutBounds picture
  | edge Horizontal unitZero 0 /= edge Horizontal unitZero 1 = Nothing
  | otherwise = Just (edge Horizontal unitZero 0, edge Vertical unitZero 0, edge Horizontal unitOne 0, edge Vertical unitOne 0)
 where
  edge :: LayoutAxis -> UnitInterval -> ExactRational -> ExactRational
  edge axis fraction target =
    let (x, y) = exactPointCoordinates (transformPoint
          (affineIsoMap (motifPort (alignMotif axis fraction target (probe picture)) Root)) (exactPoint 0 0))
     in target - case axis of
          Horizontal -> x
          Vertical -> y

probe :: Picture () -> Motif Port ()
probe picture = motif picture (const identityAffineIso2)

-- Relative controls of a trail with every shape and two constant steps. The
-- expected support is the anchor and each step's controls, enumerated by hand
-- in absolute coordinates; shared starts appear once, which cannot move support.
chain :: OpenTrail
chain = openTrail (Seq.fromList
  [ curveStep line (ExactVector 3 0)
  , curveStep (quadratic (ExactVector 1 4)) (ExactVector 2 0)
  , curveStep (cubic (ExactVector (-1) (-5)) (ExactVector 4 2)) (ExactVector 0 3)
  , curveStep line (ExactVector 0 0)
  , curveStep (quadratic (ExactVector 0 0)) (ExactVector 0 0)
  , curveStep (rationalQuadratic (ExactVector 2 (-2)) positiveTwo positiveOne) (ExactVector (-6) 1) ])

chainPoints :: NonEmpty ExactPoint
chainPoints = exactPoint 1 2 :| [exactPoint 4 2, exactPoint 5 6, exactPoint 6 2, exactPoint 5 (-3)
  , exactPoint 10 4, exactPoint 6 5, exactPoint 8 3, exactPoint 0 6]

-- The derived closing step runs from (0,6) back to the anchor through (-3,5).
closedChain :: Located ClosedTrail
closedChain = locate (exactPoint 1 2) (closeWith (quadratic (ExactVector (-3) (-1))) chain)

data ControlFixture = ControlFixture String (Picture ()) (NonEmpty ExactPoint)

controlFixtures :: [ControlFixture]
controlFixtures =
  [ ControlFixture "multi-step open trail" (strokeOf (subpaths [OpenSubpath (locate (exactPoint 1 2) chain)])) chainPoints
  , ControlFixture "closed trail with derived closing step" (fill NonZero (Solid (RGB 1 2 3)) (closedChain :| []))
      (chainPoints <> (exactPoint (-3) 5 :| []))
  , ControlFixture "located empty open trail" (strokeOf (subpaths [OpenSubpath (locate (exactPoint 9 4) mempty)]))
      (exactPoint 9 4 :| [])
  , ControlFixture "located empty closed trail" (strokeOf (subpaths [ClosedSubpath (locate (exactPoint (-2) 7) (closeWith line mempty))]))
      (exactPoint (-2) 7 :| [])
  , ControlFixture "sibling located empty and multi-step trails"
      (strokeOf (subpaths [OpenSubpath (locate (exactPoint 20 (-8)) mempty), OpenSubpath (locate (exactPoint 1 2) chain)]))
      (exactPoint 20 (-8) :| toList chainPoints) ]

-- Every fixture under identity, nested frames (each control point mapped once by
-- the composite), a nested inverse shear that must not rebox, and singular maps.
controlLaws :: Affine2 -> Affine2 -> Affine2 -> Affine2 -> ControlFixture -> IO ()
controlLaws frame shear unshear collapse (ControlFixture label picture points) = do
  let rankOne = affine2 (ExactVector 1 2) (ExactVector 2 4) (ExactVector 0 1)
  traverse_ (\(scope, scoped, expected) -> do
      let e = geometryEnvelope scoped
          bounds = Just (pointsBounds expected)
      traverse_ (\u -> check (label <> ", " <> scope <> ": support")
        (geometrySupport u e == Just (maximum (project u <$> expected)))) directions
      check (label <> ", " <> scope <> ": envelope bounds") (boundsObservation e == bounds)
      check (label <> ", " <> scope <> ": layout bounds") (layoutBounds scoped == bounds))
    [ ("identity", picture, points)
    , ("nested frames", place frame (place shear picture), transformPoint frame . transformPoint shear <$> points)
    , ("nested inverse shear", place unshear (place shear picture), points)
    , ("rank-one map", place rankOne picture, transformPoint rankOne <$> points)
    , ("collapse", place frame (place collapse picture), transformPoint frame (exactPoint 4 9) :| []) ]
 where
  project :: ExactVector -> ExactPoint -> ExactRational
  project (ExactVector x y) point = let (px, py) = exactPointCoordinates point in x * px + y * py

pointsBounds :: NonEmpty ExactPoint -> (ExactRational, ExactRational, ExactRational, ExactRational)
pointsBounds points =
  let coordinates = exactPointCoordinates <$> points
   in (minimum (fst <$> coordinates), minimum (snd <$> coordinates), maximum (fst <$> coordinates), maximum (snd <$> coordinates))

chainLayoutLaws :: IO ()
chainLayoutLaws = do
  let open = probe (strokeOf (subpaths [OpenSubpath (locate (exactPoint 1 2) chain)]))
      closed = probe (fill NonZero (Solid (RGB 1 2 3)) (closedChain :| []))
      port :: Motif Port () -> ExactPoint
      port value = transformPoint (affineIsoMap (motifPort value Root)) (exactPoint 0 0)
  check "multi-step arrangement from control bounds"
    (fmap port (toList (arrangeMotifs Horizontal 2 (Seq.fromList [open, closed, open])))
      == [exactPoint 0 0, exactPoint 15 0, exactPoint 27 0])
  check "multi-step vertical midpoint alignment"
    (port (alignMotif Vertical unitHalf 0 open) == exactPoint 0 (negate (3 * exactHalf)))

subpaths :: [Subpath] -> Path
subpaths = path . Seq.fromList

strokeOf :: Path -> Picture ()
strokeOf = stroke (pen positiveOne LocalUnits)

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
  chainLayoutLaws

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
