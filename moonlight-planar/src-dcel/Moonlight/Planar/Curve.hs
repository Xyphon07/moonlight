{-# LANGUAGE DerivingStrategies #-}

-- | Exact, translation-independent curve authoring. A step owns its endpoint
-- displacement; the next step inherits that endpoint. A location is the sole
-- absolute anchor. Closed trails derive their final displacement, and retain
-- a distinct closing shape rather than silently inserting a straight edge.
module Moonlight.Planar.Curve
  ( CurveShape
  , CurveShapeView (..)
  , shapeView
  , line
  , quadratic
  , cubic
  , rationalQuadratic
  , CurveStep
  , curveStep
  , curveStepShape
  , curveStepEnd
  , stepControlPoints
  , evaluateStep
  , splitStepHalf
  , reverseStep
  , startJet
  , endJet
  , hermiteStep
  , JoinContinuity (..)
  , joinContinuity
  , transformStep
  , OpenTrail
  , openTrail
  , trailSteps
  , trailDisplacement
  , reverseTrail
  , transformTrail
  , ClosedTrail
  , closeWith
  , closedTrailPrefix
  , closedTrailClosingShape
  , closedTrailSteps
  , reverseClosedTrail
  , transformClosedTrail
  , Located
  , locate
  , location
  , locatedValue
  , transformLocatedTrail
  , transformLocatedClosedTrail
  , reverseLocatedTrail
  , Subpath (..)
  , Path
  , path
  , pathSubpaths
  , transformPath
  , circle
  , ellipse
  ) where

import Control.DeepSeq (NFData (..))
import qualified Data.Foldable as Foldable
import Data.List.NonEmpty (NonEmpty (..))
import Data.Sequence (Seq, ViewL (..))
import qualified Data.Sequence as Seq
import Moonlight.Planar.Affine (Affine2, affine2, transformPoint, transformVector)
import Moonlight.Planar.Exact
  ( ExactPoint, ExactRational, ExactVector (..), PositiveExact, UnitInterval
  , addExactVectors, blendPositive, divideByPositive, exactCross
  , exactHalf, exactPoint, exactThird, positiveExactValue, positiveOne
  , positiveTwo, ratioPositive, translateExactPoint, unitHalf, unitIntervalValue
  )

data CurveShape
  = Linear
  | Quadratic !ExactVector
  | Cubic !ExactVector !ExactVector
  | RationalQuadratic !ExactVector !PositiveExact !PositiveExact
  deriving stock (Eq, Ord, Show)

-- | Exhaustive observation, not an alternate stored shape. Controls are
-- relative to the start; rational weights are normalized to @(1,a,b)@.
data CurveShapeView
  = LinearView
  | QuadraticView !ExactVector
  | CubicView !ExactVector !ExactVector
  | RationalQuadraticView !ExactVector !PositiveExact !PositiveExact
  deriving stock (Eq, Ord, Show)

instance NFData CurveShape where
  rnf Linear = ()
  rnf (Quadratic a) = rnf a
  rnf (Cubic a b) = rnf a `seq` rnf b
  rnf (RationalQuadratic a u v) = rnf a `seq` rnf u `seq` rnf v

shapeView :: CurveShape -> CurveShapeView
shapeView Linear = LinearView
shapeView (Quadratic a) = QuadraticView a
shapeView (Cubic a b) = CubicView a b
shapeView (RationalQuadratic a u v) = RationalQuadraticView a u v

line :: CurveShape
line = Linear

quadratic :: ExactVector -> CurveShape
quadratic = Quadratic

cubic :: ExactVector -> ExactVector -> CurveShape
cubic = Cubic

-- | Polynomial quadratics have one representation. Other rational weights
-- retain their parameterization, rather than normalizing both endpoint weights.
rationalQuadratic :: ExactVector -> PositiveExact -> PositiveExact -> CurveShape
rationalQuadratic control a b
  | a == positiveOne && b == positiveOne = Quadratic control
  | otherwise = RationalQuadratic control a b

data CurveStep = CurveStep !CurveShape !ExactVector
  deriving stock (Eq, Ord, Show)

instance NFData CurveStep where
  rnf (CurveStep shape endpoint) = rnf shape `seq` rnf endpoint

curveStep :: CurveShape -> ExactVector -> CurveStep
curveStep = CurveStep

curveStepShape :: CurveStep -> CurveShape
curveStepShape (CurveStep shape _) = shape

curveStepEnd :: CurveStep -> ExactVector
curveStepEnd (CurveStep _ endpoint) = endpoint

-- | Euclidean control hull, including start and end. Positive rational
-- Bernstein weights keep the curve inside this hull.
stepControlPoints :: CurveStep -> NonEmpty ExactVector
stepControlPoints (CurveStep shape endpoint) = zeroVector :| case shape of
  Linear -> [endpoint]
  Quadratic a -> [a, endpoint]
  Cubic a b -> [a, b, endpoint]
  RationalQuadratic a _ _ -> [a, endpoint]

evaluateStep :: UnitInterval -> CurveStep -> ExactVector
evaluateStep parameter (CurveStep shape endpoint) = case shape of
  Linear -> scaleVector t endpoint
  Quadratic a -> blend t (blend t zeroVector a) (blend t a endpoint)
  Cubic a b ->
    blend t
      (blend t (blend t zeroVector a) (blend t a b))
      (blend t (blend t a b) (blend t b endpoint))
  RationalQuadratic a u v ->
    let start = Homogeneous zeroVector positiveOne
        middle = weighted a u
        finish = weighted endpoint v
     in project (blendHomogeneous parameter
          (blendHomogeneous parameter start middle)
          (blendHomogeneous parameter middle finish))
 where
  t = unitIntervalValue parameter

-- | Exact half subdivision. The right child's controls are rebased at the
-- shared split point; parameterization is inherited on each half interval.
splitStepHalf :: CurveStep -> (CurveStep, CurveStep)
splitStepHalf (CurveStep shape endpoint) = case shape of
  Linear ->
    let midpoint = scaleVector exactHalf endpoint
     in (CurveStep Linear midpoint, CurveStep Linear midpoint)
  Quadratic a ->
    let p = midpointVector zeroVector a
        q = midpointVector a endpoint
        m = midpointVector p q
     in (CurveStep (Quadratic p) m,
         CurveStep (Quadratic (subtractVectors q m)) (subtractVectors endpoint m))
  Cubic a b ->
    let p = midpointVector zeroVector a
        q = midpointVector a b
        r = midpointVector b endpoint
        u = midpointVector p q
        v = midpointVector q r
        m = midpointVector u v
     in (CurveStep (Cubic p u) m,
         CurveStep (Cubic (subtractVectors v m) (subtractVectors r m))
           (subtractVectors endpoint m))
  RationalQuadratic a u v ->
    let start = Homogeneous zeroVector positiveOne
        middle = weighted a u
        finish = weighted endpoint v
        p = blendHomogeneous unitHalf start middle
        q = blendHomogeneous unitHalf middle finish
        m = blendHomogeneous unitHalf p q
        midpoint = project m
        leftShape = rationalQuadratic (project p) (weight p) (weight m)
        rightShape = rationalQuadratic (subtractVectors (project q) midpoint)
          (ratioPositive (weight q) (weight m))
          (ratioPositive v (weight m))
     in (CurveStep leftShape midpoint,
         CurveStep rightShape (subtractVectors endpoint midpoint))

reverseStep :: CurveStep -> CurveStep
reverseStep (CurveStep shape endpoint) = CurveStep reversed (negateVector endpoint)
 where
  reversed = case shape of
    Linear -> Linear
    Quadratic a -> Quadratic (subtractVectors a endpoint)
    Cubic a b -> Cubic (subtractVectors b endpoint) (subtractVectors a endpoint)
    RationalQuadratic a u v -> rationalQuadratic (subtractVectors a endpoint)
      (ratioPositive u v) (ratioPositive positiveOne v)

-- | Derivatives with respect to each segment's own unit parameter. Equality
-- of these jets is C1 for equal-duration segment parameterizations, not arc length.
startJet :: CurveStep -> ExactVector
startJet (CurveStep shape endpoint) = case shape of
  Linear -> endpoint
  Quadratic a -> scaleVector 2 a
  Cubic a _ -> scaleVector 3 a
  RationalQuadratic a u _ -> scaleVector (2 * positiveExactValue u) a

endJet :: CurveStep -> ExactVector
endJet (CurveStep shape endpoint) = case shape of
  Linear -> endpoint
  Quadratic a -> scaleVector 2 (subtractVectors endpoint a)
  Cubic _ b -> scaleVector 3 (subtractVectors endpoint b)
  RationalQuadratic a u v ->
    scaleVector (2 * divideByPositive (positiveExactValue u) v)
      (subtractVectors endpoint a)

hermiteStep :: ExactVector -> ExactVector -> ExactVector -> CurveStep
hermiteStep endpoint initial final =
  CurveStep (Cubic (scaleVector exactThird initial)
    (subtractVectors endpoint (scaleVector exactThird final))) endpoint

data JoinContinuity
  = StationaryJoin
  | CornerJoin
  | GeometricJoin
  | ParametricJoin
  deriving stock (Eq, Ord, Show)

joinContinuity :: CurveStep -> CurveStep -> JoinContinuity
joinContinuity before after
  | u == zeroVector || v == zeroVector = StationaryJoin
  | u == v = ParametricJoin
  | exactCross u v == 0 && dotVector u v > 0 = GeometricJoin
  | otherwise = CornerJoin
 where
  u = endJet before
  v = startJet after

-- | The affine map's linear part acts on relative controls and displacement.
-- Translation belongs exclusively to the corresponding located action.
transformStep :: Affine2 -> CurveStep -> CurveStep
transformStep placement (CurveStep shape endpoint) =
  CurveStep (transformShape placement shape) (transformVector placement endpoint)

transformShape :: Affine2 -> CurveShape -> CurveShape
transformShape placement shape = case shape of
  Linear -> Linear
  Quadratic a -> Quadratic (transformVector placement a)
  Cubic a b -> Cubic (transformVector placement a) (transformVector placement b)
  RationalQuadratic a u v -> RationalQuadratic (transformVector placement a) u v

newtype OpenTrail = OpenTrail (Seq CurveStep)
  deriving stock (Eq, Ord, Show)

instance NFData OpenTrail where
  rnf (OpenTrail steps) = rnf steps

instance Semigroup OpenTrail where
  OpenTrail a <> OpenTrail b = OpenTrail (a <> b)

instance Monoid OpenTrail where
  mempty = OpenTrail Seq.empty

openTrail :: Seq CurveStep -> OpenTrail
openTrail = OpenTrail

trailSteps :: OpenTrail -> Seq CurveStep
trailSteps (OpenTrail steps) = steps

trailDisplacement :: OpenTrail -> ExactVector
trailDisplacement = Foldable.foldl' (\offset step -> addExactVectors offset (curveStepEnd step)) zeroVector . trailSteps

reverseTrail :: OpenTrail -> OpenTrail
reverseTrail = OpenTrail . fmap reverseStep . Seq.reverse . trailSteps

transformTrail :: Affine2 -> OpenTrail -> OpenTrail
transformTrail placement = OpenTrail . fmap (transformStep placement) . trailSteps

data ClosedTrail = ClosedTrail !OpenTrail !CurveShape
  deriving stock (Eq, Ord, Show)

instance NFData ClosedTrail where
  rnf (ClosedTrail prefix closing) = rnf prefix `seq` rnf closing

closeWith :: CurveShape -> OpenTrail -> ClosedTrail
closeWith closing prefix = ClosedTrail prefix closing

closedTrailPrefix :: ClosedTrail -> OpenTrail
closedTrailPrefix (ClosedTrail prefix _) = prefix

closedTrailClosingShape :: ClosedTrail -> CurveShape
closedTrailClosingShape (ClosedTrail _ closing) = closing

closedTrailSteps :: ClosedTrail -> Seq CurveStep
closedTrailSteps (ClosedTrail prefix closing) =
  trailSteps prefix Seq.|> CurveStep closing (negateVector (trailDisplacement prefix))

reverseClosedTrail :: ClosedTrail -> ClosedTrail
reverseClosedTrail (ClosedTrail prefix closing) =
  let reversedClosing = reverseStep (CurveStep closing (negateVector (trailDisplacement prefix)))
   in case Seq.viewl (trailSteps prefix) of
        EmptyL -> ClosedTrail mempty (curveStepShape reversedClosing)
        first :< rest -> ClosedTrail
          (OpenTrail (Seq.singleton reversedClosing <> fmap reverseStep (Seq.reverse rest)))
          (curveStepShape (reverseStep first))

transformClosedTrail :: Affine2 -> ClosedTrail -> ClosedTrail
transformClosedTrail placement (ClosedTrail prefix closing) =
  ClosedTrail (transformTrail placement prefix) (transformShape placement closing)

data Located a = Located !ExactPoint !a
  deriving stock (Eq, Ord, Show)

instance NFData a => NFData (Located a) where
  rnf (Located anchor value) = rnf anchor `seq` rnf value

locate :: ExactPoint -> a -> Located a
locate = Located

location :: Located a -> ExactPoint
location (Located anchor _) = anchor

locatedValue :: Located a -> a
locatedValue (Located _ value) = value

-- | One affine value owns both the point action and the relative vector action.
transformLocatedTrail :: Affine2 -> Located OpenTrail -> Located OpenTrail
transformLocatedTrail placement (Located anchor trail) =
  Located (transformPoint placement anchor) (transformTrail placement trail)

transformLocatedClosedTrail :: Affine2 -> Located ClosedTrail -> Located ClosedTrail
transformLocatedClosedTrail placement (Located anchor trail) =
  Located (transformPoint placement anchor) (transformClosedTrail placement trail)

reverseLocatedTrail :: Located OpenTrail -> Located OpenTrail
reverseLocatedTrail (Located anchor trail) =
  Located (translateExactPoint anchor (trailDisplacement trail)) (reverseTrail trail)

data Subpath
  = OpenSubpath !(Located OpenTrail)
  | ClosedSubpath !(Located ClosedTrail)
  deriving stock (Eq, Ord, Show)

instance NFData Subpath where
  rnf (OpenSubpath value) = rnf value
  rnf (ClosedSubpath value) = rnf value

newtype Path = Path (Seq Subpath)
  deriving stock (Eq, Ord, Show)

instance NFData Path where
  rnf (Path subpaths) = rnf subpaths

instance Semigroup Path where
  Path a <> Path b = Path (a <> b)

instance Monoid Path where
  mempty = Path Seq.empty

path :: Seq Subpath -> Path
path = Path

pathSubpaths :: Path -> Seq Subpath
pathSubpaths (Path subpaths) = subpaths

transformPath :: Affine2 -> Path -> Path
transformPath placement = Path . fmap transform . pathSubpaths
 where
  transform (OpenSubpath value) = OpenSubpath (transformLocatedTrail placement value)
  transform (ClosedSubpath value) = ClosedSubpath (transformLocatedClosedTrail placement value)

-- | An exact circle centered at the origin, starting on the positive x axis.
-- The four rational quarters are G1, not uniformly parameterized by angle.
circle :: PositiveExact -> Located ClosedTrail
circle radius = ellipse (ExactVector r 0) (ExactVector 0 r)
 where
  r = positiveExactValue radius

-- | The rational affine image of the unit circle, centered at the origin.
-- The axes may be skew or singular: authored curves admit collapsed geometry.
ellipse :: ExactVector -> ExactVector -> Located ClosedTrail
ellipse axisX axisY =
  transformLocatedClosedTrail (affine2 axisX axisY zeroVector)
    (Located (exactPoint 1 0) unitCircle)
 where
  conic control = rationalQuadratic control positiveOne positiveTwo
  unitCircle = closeWith (conic (ExactVector 1 0)) (openTrail (Seq.fromList
    [ CurveStep (conic (ExactVector 0 1)) (ExactVector (-1) 1)
    , CurveStep (conic (ExactVector (-1) 0)) (ExactVector (-1) (-1))
    , CurveStep (conic (ExactVector 0 (-1))) (ExactVector 1 (-1))
    ]))

data Homogeneous = Homogeneous !ExactVector !PositiveExact

weighted :: ExactVector -> PositiveExact -> Homogeneous
weighted value w = Homogeneous (scaleVector (positiveExactValue w) value) w

weight :: Homogeneous -> PositiveExact
weight (Homogeneous _ w) = w

project :: Homogeneous -> ExactVector
project (Homogeneous (ExactVector x y) w) =
  ExactVector (divideByPositive x w) (divideByPositive y w)

blendHomogeneous :: UnitInterval -> Homogeneous -> Homogeneous -> Homogeneous
blendHomogeneous t (Homogeneous a u) (Homogeneous b v) =
  Homogeneous (blend (unitIntervalValue t) a b) (blendPositive t u v)

zeroVector :: ExactVector
zeroVector = ExactVector 0 0

scaleVector :: ExactRational -> ExactVector -> ExactVector
scaleVector scale (ExactVector x y) = ExactVector (scale * x) (scale * y)

negateVector :: ExactVector -> ExactVector
negateVector = scaleVector (-1)

subtractVectors :: ExactVector -> ExactVector -> ExactVector
subtractVectors a b = addExactVectors a (negateVector b)

blend :: ExactRational -> ExactVector -> ExactVector -> ExactVector
blend t a b = addExactVectors (scaleVector (1 - t) a) (scaleVector t b)

midpointVector :: ExactVector -> ExactVector -> ExactVector
midpointVector = blend exactHalf

dotVector :: ExactVector -> ExactVector -> ExactRational
dotVector (ExactVector ax ay) (ExactVector bx by) = ax * bx + ay * by
