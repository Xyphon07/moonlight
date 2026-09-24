-- | Source-span provenance and selection for canonical curves: the one owner
-- of which step of a source a span refines and where a site on it lies.
--
-- It owns three invariants. A 'SourceStep' is built only by 'sourceSteps',
-- from the source's own 'trailSteps', or 'closedTrailSteps' for a closed
-- trail, so its index and count are valid for its source by construction and
-- its located start is the anchor translated by the steps before it. A closed
-- trail's seam is a join: the closing step's end and the first step's start
-- are one point. A 'TrailSite' is built only by 'trailSite', which decides its
-- 'JoinSide' from the step's index and count and the source's closure.
--
-- A site's point is its step's exact point at its parameter: the step's
-- located start translated by 'evaluateStep'. 'trailSite' takes the point as
-- given, so that obligation rests with its callers inside the package:
-- 'selectSite' evaluates the step; measurement and proximity carry the point
-- of their exact subdivision, which agrees with evaluation; and proximity's
-- nearest point on a straight piece is the step's own affine point at the
-- parameter.
module Moonlight.Planar.Internal.CurveSource
  ( SourceStep
  , sourceSteps
  , sourceStepIndex
  , sourceStepCount
  , sourceStepStart
  , sourceStepCurve
  , JoinSide (..)
  , TrailSite
  , trailSite
  , siteSource
  , siteStep
  , siteStepIndex
  , siteParameter
  , sitePoint
  , siteJoinSide
  , siteJet
  , selectSite
  , joinNeighbourJet
  , SourceSpan
  , wholeSpan
  , halveSpan
  , sourceSpanStep
  , sourceSpanFrom
  , sourceSpanTo
  , sourceSpanStart
  , sourceSpanPiece
  , sourceSpanControls
  , admitSpan
  , spanBits
  ) where

import Control.DeepSeq (NFData (..))
import Data.List.NonEmpty (NonEmpty)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Moonlight.Planar.Curve
  ( CurveShapeView (..), CurveStep, StepJet, Subpath (..), closedTrailSteps, curveStepEnd
  , curveStepShape, evaluateStep, jetStep, location, locatedValue, shapeView, splitStep
  , stepControlPoints, trailSteps )
import Moonlight.Planar.Exact
  ( ExactPoint, ExactVector (..), UnitInterval, exactPointBitWidth, exactRationalBitWidth
  , positiveExactValue, translateExactPoint, unitHalf, unitIntervalValue, unitOne, unitZero )
import Moonlight.Planar.Internal.CurveBudget (BudgetObligation (..), SubdivisionBudget, budgetBits)
import Moonlight.Planar.Internal.ExactRational (unitMidpoint)

-- | One step of a source as the source itself has it: its index among the
-- source's actual steps, their count, its located start, and the step. Only
-- 'sourceSteps' builds one, so an index is valid for its source by
-- construction.
data SourceStep = SourceStep !Int !Int !ExactPoint !CurveStep
  deriving stock (Eq, Show)

instance NFData SourceStep where
  rnf (SourceStep index count start step) =
    rnf index `seq` rnf count `seq` rnf start `seq` rnf step

sourceStepIndex :: SourceStep -> Int
sourceStepIndex (SourceStep index _ _ _) = index

sourceStepCount :: SourceStep -> Int
sourceStepCount (SourceStep _ count _ _) = count

-- | The located start of the step.
sourceStepStart :: SourceStep -> ExactPoint
sourceStepStart (SourceStep _ _ start _) = start

sourceStepCurve :: SourceStep -> CurveStep
sourceStepCurve (SourceStep _ _ _ step) = step

-- | Which side of a join a site lies on. The end of one step and the start of
-- the next are one point; a site there is on exactly one of the two steps, at
-- parameter one before the join or at parameter zero after it. A site inside
-- a step, or at an open trail's own start or end, is away from any join; a
-- closed trail's seam is a join.
data JoinSide
  = AwayFromJoin
  | BeforeJoin
  | AfterJoin
  deriving stock (Eq, Ord, Show)

instance NFData JoinSide where
  rnf side = side `seq` ()

-- | A located position on one selected step of its source. Only 'trailSite'
-- builds one; the point is the step's exact point at the parameter.
data TrailSite = TrailSite !Subpath !SourceStep !UnitInterval !ExactPoint !JoinSide
  deriving stock (Eq, Show)

instance NFData TrailSite where
  rnf (TrailSite source step parameter point side) =
    rnf source `seq` rnf step `seq` rnf parameter `seq` rnf point `seq` rnf side

siteSource :: TrailSite -> Subpath
siteSource (TrailSite source _ _ _ _) = source

siteStep :: TrailSite -> SourceStep
siteStep (TrailSite _ step _ _ _) = step

siteStepIndex :: TrailSite -> Int
siteStepIndex = sourceStepIndex . siteStep

siteParameter :: TrailSite -> UnitInterval
siteParameter (TrailSite _ _ parameter _ _) = parameter

sitePoint :: TrailSite -> ExactPoint
sitePoint (TrailSite _ _ _ point _) = point

siteJoinSide :: TrailSite -> JoinSide
siteJoinSide (TrailSite _ _ _ _ side) = side

-- | The site's value and derivatives on its own step, relative to the step's
-- located start.
siteJet :: TrailSite -> StepJet
siteJet site = jetStep (siteParameter site) (sourceStepCurve (siteStep site))

-- | The source's steps with their indices and located starts: the one owner
-- of step selection.
sourceSteps :: Subpath -> Seq SourceStep
sourceSteps source =
  Seq.mapWithIndex (\index (start, step) -> SourceStep index count start step) (Seq.zip starts steps)
 where
  (anchor, steps) = case source of
    OpenSubpath value -> (location value, trailSteps (locatedValue value))
    ClosedSubpath value -> (location value, closedTrailSteps (locatedValue value))
  count = Seq.length steps
  starts = Seq.scanl (\start step -> translateExactPoint start (curveStepEnd step)) anchor steps

trailSite :: Subpath -> SourceStep -> UnitInterval -> ExactPoint -> TrailSite
trailSite source step@(SourceStep index count _ _) parameter point =
  TrailSite source step parameter point side
 where
  closed = case source of
    ClosedSubpath _ -> True
    OpenSubpath _ -> False
  side
    | parameter == unitOne && (closed || index + 1 < count) = BeforeJoin
    | parameter == unitZero && (closed || index > 0) = AfterJoin
    | otherwise = AwayFromJoin

-- | The site at a parameter of the source's step at an index, if the source
-- has that step.
selectSite :: Subpath -> Int -> UnitInterval -> Maybe TrailSite
selectSite source index parameter = place <$> Seq.lookup index (sourceSteps source)
 where
  place step = trailSite source step parameter
    (translateExactPoint (sourceStepStart step) (evaluateStep parameter (sourceStepCurve step)))

-- | The jet of the other step at a site's join: the next step's start before
-- the join, the previous step's end after it. Only a closed trail's seam
-- joins its last and first steps, and 'trailSite' marks an open trail's own
-- start and end away from any join, so the neighbour index is reduced modulo
-- the count, which the site's step makes positive.
joinNeighbourJet :: TrailSite -> Maybe StepJet
joinNeighbourJet (TrailSite source (SourceStep index count _ _) _ _ side) = case side of
  AwayFromJoin -> Nothing
  BeforeJoin -> neighbour (index + 1) unitZero
  AfterJoin -> neighbour (index - 1) unitOne
 where
  neighbour at parameter =
    jetStep parameter . sourceStepCurve <$> Seq.lookup (at `mod` count) (sourceSteps source)

-- | One span of a source step as subdivision reaches it: the step, a bracket
-- of its parameters, the span's located start, and the piece. Only
-- 'wholeSpan' and 'halveSpan' build one, so the piece is the step restricted
-- to the bracket and the start is the step's point at the bracket's lower
-- parameter.
data SourceSpan = SourceSpan !SourceStep !UnitInterval !UnitInterval !ExactPoint !CurveStep
  deriving stock (Eq, Show)

instance NFData SourceSpan where
  rnf (SourceSpan step from to start piece) =
    rnf step `seq` rnf from `seq` rnf to `seq` rnf start `seq` rnf piece

wholeSpan :: SourceStep -> SourceSpan
wholeSpan step = SourceSpan step unitZero unitOne (sourceStepStart step) (sourceStepCurve step)

-- | The span's two halves: exact de Casteljau subdivision at the piece's own
-- midpoint, which is the bracket's midpoint in the step's parameter.
halveSpan :: SourceSpan -> (SourceSpan, SourceSpan)
halveSpan (SourceSpan step from to start piece) =
  ( SourceSpan step from middle start left
  , SourceSpan step middle to (translateExactPoint start (curveStepEnd left)) right )
 where
  (left, right) = splitStep unitHalf piece
  middle = unitMidpoint from to

sourceSpanStep :: SourceSpan -> SourceStep
sourceSpanStep (SourceSpan step _ _ _ _) = step

sourceSpanFrom :: SourceSpan -> UnitInterval
sourceSpanFrom (SourceSpan _ from _ _ _) = from

sourceSpanTo :: SourceSpan -> UnitInterval
sourceSpanTo (SourceSpan _ _ to _ _) = to

sourceSpanStart :: SourceSpan -> ExactPoint
sourceSpanStart (SourceSpan _ _ _ start _) = start

sourceSpanPiece :: SourceSpan -> CurveStep
sourceSpanPiece (SourceSpan _ _ _ _ piece) = piece

-- | The piece's controls, located at the span's start: the hull a caller
-- retains, queries and compares.
sourceSpanControls :: SourceSpan -> NonEmpty ExactPoint
sourceSpanControls (SourceSpan _ _ _ start piece) = translateExactPoint start <$> stepControlPoints piece

-- | Admit a span under the budget's bits, the one gate every consumer of a
-- span passes: its located start, parameters, relative controls and weights,
-- and its located controls, whose width the relative ones do not bound.
admitSpan :: SubdivisionBudget -> SourceSpan -> Either BudgetObligation SourceSpan
admitSpan budget sourceSpan@(SourceSpan _ from to start piece)
  | width > budgetBits budget = Left (BitsExhausted width)
  | otherwise = Right sourceSpan
 where
  width = foldr (max . exactPointBitWidth) (spanBits start from to piece) (sourceSpanControls sourceSpan)

-- | The widest of a span's located start, parameters, controls and rational
-- weights.
spanBits :: ExactPoint -> UnitInterval -> UnitInterval -> CurveStep -> Int
spanBits from t0 t1 step =
  foldr (max . exactRationalBitWidth) (exactPointBitWidth from)
    (unitIntervalValue t0 : unitIntervalValue t1 : weights <> controls)
 where
  controls = concatMap (\(ExactVector x y) -> [x, y]) (stepControlPoints step)
  weights = case shapeView (curveStepShape step) of
    RationalQuadraticView _ a b -> [positiveExactValue a, positiveExactValue b]
    _ -> []
