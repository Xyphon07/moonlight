-- | Certified Euclidean arc length of canonical curves, in the coordinates of
-- the submitted curve. A measured trail is an immutable observation of its
-- source, never an editable shadow curve: it retains the source subpath, its
-- accepted spans, and their cumulative rational length enclosures. The curve's
-- arc length is enclosed, not represented; no radical expression of the arc
-- itself is fabricated.
--
-- Enclosure kernel, one for all four shapes. For a span with Euclidean control
-- polygon @P0..Pn@, the chord @|Pn - P0|@ is a lower bound and the polygon
-- length an upper bound of its arc length. For polynomial shapes this is the
-- classical Bernstein argument. For a positive-weight rational quadratic with
-- controls @P0,P1,P2@ and weights @(1,a,b)@, one homogeneous de Casteljau step
-- at any @t@ in @[0,1]@ yields @p@ on segment @P0P1@, @q@ on @P1P2@ and @m@ on
-- @pq@: each is a convex combination with positive homogeneous weights, so its
-- projection lies on the segment between the projected endpoints. By the
-- triangle inequality the refined chain @P0,p,m,q,P2@ is no longer than
-- @P0,P1,P2@, and its chords are no shorter than @P0P2@. The children, after
-- division by a common positive weight, are again positive-weight rational
-- quadratics, so the argument repeats without any square root. Under repeated
-- subdivision the inscribed chord polygons converge to the arc, which bounds
-- it from below, and the control polygons converge from above; lower
-- semicontinuity of length makes the limit of the nonincreasing polygon
-- lengths an upper bound, and endpoint distance is the lower bound at every
-- stage. Validity is independent of convergence rate: acceptance is decided by
-- the actual outward gap, and exhaustion refuses.
--
-- Error allocation is global and split in two halves. Let @U0@ be the outward
-- upper bound of the whole source's control-polygon length and @N@ the number
-- of source steps. A span @[a,b]@ of any step is accepted when its outward gap
-- @upper - lower@ is at most
-- @(tolerance / 2) * (lower / U0 + (b - a) / N)@. Summed over the accepted
-- spans, the relative half is at most @(tolerance / 2) * L / U0 <=
-- tolerance / 2@, where @L <= U0@ is the arc length, since the chords' lower
-- bounds sum to at most @L@; the parameter half is exactly
-- @(tolerance / 2) * N / N = tolerance / 2@, since the accepted spans of each
-- step partition its parameter interval. The whole gap is therefore at most
-- the tolerance. The outward dyadic rounding of every enclosure lies inside
-- the compared gap, so it is charged to the same allowance. The relative half
-- alone never accepts a span straddling a turning point, whose chord stays a
-- fixed fraction of its polygon however small it becomes; the parameter half
-- shrinks only linearly with the span while such a gap shrinks faster. If
-- @U0@ is zero the relative half is zero and every span is stationary with
-- zero gap; a source with no steps has no spans and never divides by @N@.
-- The partition, and so the cost in spans, depends on the parameterization;
-- the Euclidean length enclosure and its global tolerance do not.
--
-- Arithmetic is budgeted before it is spent. Every source step is admitted
-- against the bit budget, with its located start, controls and rational
-- weights, before any length is observed; every generated child is admitted
-- with its parameters, and every observed enclosure endpoint, prefix and
-- total included, is admitted before it is compared. A query's request, and
-- the residual and bracket it returns, are admitted the same way. The two
-- budgets bound different things: the radical precision bounds scratch work
-- inside one root, while the bit budget bounds the source and every retained
-- exact value. A high precision is never refused for its value; an
-- irrational length at high precision is refused because its retained
-- enclosure endpoints exceed the budget, while an exact root stays small at
-- any precision.
module Moonlight.Planar.Curve.Measure
  ( Distance
  , DistanceError (..)
  , distance
  , distanceValue
  , MeasurePolicy
  , measurePolicy
  , measureTolerance
  , measurePrecision
  , measureBudget
  , SubdivisionBudget
  , SubdivisionBudgetError (..)
  , subdivisionBudget
  , budgetDepth
  , budgetLeaves
  , budgetBits
  , BudgetObligation (..)
  , RadicalPrecision
  , RadicalPrecisionError (..)
  , radicalPrecision
  , radicalPrecisionBits
  , LengthEnclosure
  , lengthEnclosureLower
  , lengthEnclosureUpper
  , lengthEnclosureWidth
  , MeasureObligation (..)
  , MeasureError (..)
  , MeasuredTrail
  , measureSubpath
  , measuredSource
  , measuredSpans
  , lengthBounds
  , MeasuredSpan
  , measuredSpanStep
  , measuredSpanFrom
  , measuredSpanTo
  , measuredSpanStart
  , measuredSpanPiece
  , measuredSpanLength
  , measuredSpanPrefix
  , JoinSide (..)
  , TrailSite
  , siteSource
  , siteStepIndex
  , siteParameter
  , sitePoint
  , siteJoinSide
  , siteJet
  , ArcSample
  , sampleSite
  , sampleParameterFrom
  , sampleParameterTo
  , sampleResidual
  , pointAtLength
  , pointAtFraction
  ) where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData (..))
import Control.Monad (foldM, when)
import Data.Foldable (fold, toList, traverse_)
import Data.Sequence (Seq, ViewL (..), ViewR (..))
import qualified Data.Sequence as Seq
import Moonlight.Planar.Curve (CurveStep, Subpath, curveStepEnd, splitStep, stepControlPoints)
import Moonlight.Planar.Exact
  ( ExactPoint, ExactRational, ExactVector (..), PositiveExact, UnitInterval
  , divideByPositive, exactHalf, exactPointBitWidth, exactRationalBitWidth, positiveExact
  , positiveExactValue, translateExactPoint, unitHalf, unitIntervalValue, unitOne, unitZero )
import Moonlight.Planar.Internal.CurveBudget
  ( BudgetObligation (..), SubdivisionBudget, SubdivisionBudgetError (..), budgetBits, budgetDepth
  , budgetLeaves, subdivisionBudget )
import Moonlight.Planar.Internal.CurveSource
  ( JoinSide (..), SourceStep, TrailSite, siteJet, siteJoinSide, siteParameter, sitePoint
  , siteSource, siteStepIndex, sourceStepCurve, sourceStepIndex, sourceStepStart
  , sourceSteps, spanBits, trailSite )
import Moonlight.Planar.Internal.ExactRational (unitMidpoint)
import Moonlight.Planar.Internal.Length
  ( LengthEnclosure, RadicalPrecision, RadicalPrecisionError (..), enclosureBetween
  , euclideanLengthEnclosure, lengthEnclosureLower, lengthEnclosureUpper
  , lengthEnclosureWidth, radicalPrecision, radicalPrecisionBits )

-- | A nonnegative distance, zero included: along a trail, or between curves.
newtype Distance = Distance ExactRational
  deriving stock (Eq, Ord, Show)

instance NFData Distance where
  rnf (Distance value) = rnf value

newtype DistanceError = NegativeDistance ExactRational
  deriving stock (Eq, Show)

distance :: ExactRational -> Either DistanceError Distance
distance value
  | value >= 0 = Right (Distance value)
  | otherwise = Left (NegativeDistance value)

distanceValue :: Distance -> ExactRational
distanceValue (Distance value) = value

-- | Tolerance, radical precision, and the subdivision budget: depth, leaves,
-- and the largest admitted bit width of any span coordinate, weight,
-- parameter or observed enclosure endpoint. Each part arrives admitted.
data MeasurePolicy = MeasurePolicy !PositiveExact !RadicalPrecision !SubdivisionBudget
  deriving stock (Eq, Show)

instance NFData MeasurePolicy where
  rnf (MeasurePolicy tolerance precision budget) =
    rnf tolerance `seq` rnf precision `seq` rnf budget

measurePolicy :: PositiveExact -> RadicalPrecision -> SubdivisionBudget -> MeasurePolicy
measurePolicy = MeasurePolicy

measureTolerance :: MeasurePolicy -> PositiveExact
measureTolerance (MeasurePolicy tolerance _ _) = tolerance

measurePrecision :: MeasurePolicy -> RadicalPrecision
measurePrecision (MeasurePolicy _ precision _) = precision

measureBudget :: MeasurePolicy -> SubdivisionBudget
measureBudget (MeasurePolicy _ _ budget) = budget

-- | The obligation a span could not discharge: the subdivision budget, or one
-- of measurement's own. A precision refusal carries the
-- span's rounding width and its share of the global allowance; subdividing
-- cannot help once rounding alone exceeds that share. An ambiguous distance
-- carries the best residual the enclosures certify, which exceeds the
-- inverse's allowance.
data MeasureObligation
  = MeasureBudgetExhausted !BudgetObligation
  | PrecisionExhausted !ExactRational !ExactRational
  | AmbiguousDistance !ExactRational
  deriving stock (Eq, Show)

-- | A refused span names its source step index (in 'closedTrailSteps' order for
-- a closed trail) and parameter bracket within that step. A distance above
-- the certified upper length is beyond the trail; one above the lower length
-- but not the upper is undecided at this enclosure. Neither is clamped. A
-- fraction whose share of the length uncertainty consumes the tolerance
-- carries that share and the tolerance. A query request refused before any
-- span is consulted carries its obligation alone. A trail with no steps has no
-- source span to sample.
data MeasureError
  = SpanRefused !Int !UnitInterval !UnitInterval !MeasureObligation
  | RequestRefused !MeasureObligation
  | DistanceBeyondTrail !ExactRational !LengthEnclosure
  | DistanceUnresolved !ExactRational !LengthEnclosure
  | FractionBudgetExhausted !ExactRational !ExactRational
  | EmptyTrailSample
  deriving stock (Eq, Show)

-- | One accepted span: the source step it refines, its parameter bracket, its
-- located start, the child step obtained by subdivision, its enclosure, and
-- the cumulative enclosure of every span before it.
data MeasuredSpan = MeasuredSpan !Leaf !LengthEnclosure
  deriving stock (Eq, Show)

data Leaf = Leaf !SourceStep !UnitInterval !UnitInterval !ExactPoint !CurveStep !LengthEnclosure
  deriving stock (Eq, Show)

instance NFData MeasuredSpan where
  rnf (MeasuredSpan (Leaf step from to start piece enclosure) prefix) =
    rnf step `seq` rnf from `seq` rnf to `seq` rnf start `seq` rnf piece
      `seq` rnf enclosure `seq` rnf prefix

measuredSpanStep :: MeasuredSpan -> Int
measuredSpanStep (MeasuredSpan (Leaf step _ _ _ _ _) _) = sourceStepIndex step

measuredSpanFrom :: MeasuredSpan -> UnitInterval
measuredSpanFrom (MeasuredSpan (Leaf _ from _ _ _ _) _) = from

measuredSpanTo :: MeasuredSpan -> UnitInterval
measuredSpanTo (MeasuredSpan (Leaf _ _ to _ _ _) _) = to

measuredSpanStart :: MeasuredSpan -> ExactPoint
measuredSpanStart (MeasuredSpan (Leaf _ _ _ start _ _) _) = start

measuredSpanPiece :: MeasuredSpan -> CurveStep
measuredSpanPiece (MeasuredSpan (Leaf _ _ _ _ piece _) _) = piece

measuredSpanLength :: MeasuredSpan -> LengthEnclosure
measuredSpanLength (MeasuredSpan (Leaf _ _ _ _ _ enclosure) _) = enclosure

measuredSpanPrefix :: MeasuredSpan -> LengthEnclosure
measuredSpanPrefix (MeasuredSpan _ prefix) = prefix

-- | An open trail is measured from its anchor. A closed trail is measured as
-- exactly one lap, with its seam at the anchor: its steps are
-- 'closedTrailSteps', the explicit closing step last.
data MeasuredTrail = MeasuredTrail !Subpath !MeasurePolicy !(Seq MeasuredSpan) !LengthEnclosure
  deriving stock (Eq, Show)

instance NFData MeasuredTrail where
  rnf (MeasuredTrail source policy spans total) =
    rnf source `seq` rnf policy `seq` rnf spans `seq` rnf total

measuredSource :: MeasuredTrail -> Subpath
measuredSource (MeasuredTrail source _ _ _) = source

measuredSpans :: MeasuredTrail -> Seq MeasuredSpan
measuredSpans (MeasuredTrail _ _ spans _) = spans

-- | The trail's length lies in this enclosure, whose width is at most the
-- policy tolerance.
lengthBounds :: MeasuredTrail -> LengthEnclosure
lengthBounds (MeasuredTrail _ _ _ total) = total

-- | An answer to an inverse-length query: a site on its source, and a bracket
-- of that step's parameters containing the site's. Every parameter in the
-- bracket has arc distance within the residual of the request. The bracket is
-- certified, not maximal; it does not claim a unique inverse, which a
-- stationary span does not have.
data ArcSample = ArcSample !TrailSite !UnitInterval !UnitInterval !ExactRational
  deriving stock (Eq, Show)

instance NFData ArcSample where
  rnf (ArcSample site from to residual) =
    rnf site `seq` rnf from `seq` rnf to `seq` rnf residual

sampleSite :: ArcSample -> TrailSite
sampleSite (ArcSample site _ _ _) = site

sampleParameterFrom :: ArcSample -> UnitInterval
sampleParameterFrom (ArcSample _ from _ _) = from

sampleParameterTo :: ArcSample -> UnitInterval
sampleParameterTo (ArcSample _ _ to _) = to

sampleResidual :: ArcSample -> ExactRational
sampleResidual (ArcSample _ _ _ residual) = residual

-- | Precision, the tolerance shares per unit of chord and per unit of one
-- step's parameter, and the bit budget.
data Measuring = Measuring !RadicalPrecision !ExactRational !ExactRational !Int

measureSubpath :: MeasurePolicy -> Subpath -> Either MeasureError MeasuredTrail
measureSubpath policy@(MeasurePolicy tolerance precision budget) source = do
  -- Coordinates, weights and parameters first; no length is observed from a
  -- step the budget has not admitted. Then each source polygon and their sums.
  traverse_ (\step -> admitBits bits (whole step) (stepBits step)) selected
  traverse_ (\(step, polygon, prefix) -> admitEnclosure bits (whole step) polygon *> admitEnclosure bits (whole step) prefix)
    (Seq.zip3 selected polygons (Seq.drop 1 (Seq.scanl (<>) mempty polygons)))
  (_, measured) <- foldM appendStep (leaves, Seq.empty) selected
  let prefixes = Seq.scanl (<>) mempty (fmap leafLength measured)
  traverse_ (\(Leaf step t0 t1 _ _ _, after) -> admitEnclosure bits (SpanRefused (sourceStepIndex step) t0 t1) after)
    (Seq.zip measured (Seq.drop 1 prefixes))
  pure (MeasuredTrail source policy
    (Seq.zipWith (flip MeasuredSpan) prefixes measured) (fold (fmap leafLength measured)))
 where
  depth = budgetDepth budget
  leaves = budgetLeaves budget
  bits = budgetBits budget
  selected = sourceSteps source
  whole step = SpanRefused (sourceStepIndex step) unitZero unitOne
  stepBits step = spanBits (sourceStepStart step) unitZero unitOne (sourceStepCurve step)
  polygons = fmap (controlPolygon precision . sourceStepCurve) selected
  polygonUpper = lengthEnclosureUpper (fold polygons)
  -- A source whose control polygon has zero length is stationary; its every
  -- span then has zero gap. A source with no steps has no spans, so its
  -- parameter share is never read.
  halfTolerance = positiveExactValue tolerance * exactHalf
  lengthShare = either (const 0) (divideByPositive halfTolerance) (positiveExact polygonUpper)
  parameterShare = either (const 0) (divideByPositive halfTolerance)
    (positiveExact (fromIntegral (Seq.length selected)))
  measuring = Measuring precision lengthShare parameterShare bits
  appendStep (remaining, prefix) step = do
    (remainingAfter, spans) <-
      descend measuring step depth remaining (sourceStepStart step) unitZero unitOne (sourceStepCurve step)
    pure (remainingAfter, prefix <> spans)

-- The binary subdivision tree is consumed directly, as in lowering; the depth
-- and leaf budgets bound traversal before any exponential tree exists.
descend
  :: Measuring -> SourceStep -> Int -> Int -> ExactPoint -> UnitInterval -> UnitInterval -> CurveStep
  -> Either MeasureError (Int, Seq Leaf)
descend measuring@(Measuring precision lengthShare parameterShare budget) source depth remaining from t0 t1 step
  | remaining <= 0 = refuse (MeasureBudgetExhausted LeavesExhausted)
  | width > budget = refuse (MeasureBudgetExhausted (BitsExhausted width))
  | observed > budget = refuse (MeasureBudgetExhausted (BitsExhausted observed))
  | lengthEnclosureWidth enclosure <= allowance =
      Right (remaining - 1, Seq.singleton (Leaf source t0 t1 from step enclosure))
  | optimisticGap <= optimisticAllowance && rounding > allowance =
      refuse (PrecisionExhausted rounding allowance)
  | depth == 0 = refuse (MeasureBudgetExhausted DepthExhausted)
  | otherwise = do
      let (left, right) = splitStep unitHalf step
          middle = unitMidpoint t0 t1
          splitPoint = translateExactPoint from (curveStepEnd left)
      (afterLeft, leftLeaves) <- descend measuring source (depth - 1) remaining from t0 middle left
      (afterRight, rightLeaves) <- descend measuring source (depth - 1) afterLeft splitPoint middle t1 right
      pure (afterRight, leftLeaves <> rightLeaves)
 where
  refuse :: MeasureObligation -> Either MeasureError (Int, Seq Leaf)
  refuse = Left . SpanRefused (sourceStepIndex source) t0 t1
  width = spanBits from t0 t1 step
  chord = chordLength precision step
  polygon = controlPolygon precision step
  observed = max (enclosureBits chord) (enclosureBits polygon)
  enclosure = enclosureBetween chord polygon
  spanShare = parameterShare * (unitIntervalValue t1 - unitIntervalValue t0)
  allowance = lengthShare * lengthEnclosureLower chord + spanShare
  -- The same comparison with rounding in the span's favour: if even that
  -- fails, the gap is geometric and subdivision reduces it. If it passes but
  -- rounding alone exceeds the allowance, subdivision cannot help: it halves
  -- the parameter term while each child's rounding stays near @2^-precision@
  -- per radical term.
  optimisticGap = lengthEnclosureLower polygon - lengthEnclosureUpper chord
  optimisticAllowance = lengthShare * lengthEnclosureUpper chord + spanShare
  rounding = lengthEnclosureWidth chord + lengthEnclosureWidth polygon

-- | A point whose arc distance from the start is within the residual, at most
-- the policy tolerance, of the request. Only a request at most the certified
-- lower length is answered. The accepted span whose cumulative enclosures
-- bracket the request is bisected with the same kernel; a side is taken only
-- when enclosures separate from the request.
pointAtLength :: Distance -> MeasuredTrail -> Either MeasureError ArcSample
pointAtLength (Distance target) trail@(MeasuredTrail _ (MeasurePolicy tolerance _ budget) _ total)
  | targetBits > bits = Left (RequestRefused (MeasureBudgetExhausted (BitsExhausted targetBits)))
  | target > lengthEnclosureUpper total = Left (DistanceBeyondTrail target total)
  | target > lengthEnclosureLower total = Left (DistanceUnresolved target total)
  | otherwise = inverseLength (positiveExactValue tolerance) target trail >>= admitSample bits
 where
  targetBits = exactRationalBitWidth target
  bits = budgetBits budget

-- | The request's fraction of the lower length bound, which differs from the
-- same fraction of the true length by at most the fraction of the total
-- width. That share is reserved from the tolerance before the inverse is
-- refined, so the widened residual stays within tolerance.
pointAtFraction :: UnitInterval -> MeasuredTrail -> Either MeasureError ArcSample
pointAtFraction fraction trail@(MeasuredTrail _ (MeasurePolicy tolerance _ budget) _ total)
  | shareBits > bits = Left (RequestRefused (MeasureBudgetExhausted (BitsExhausted shareBits)))
  | derivedBits > bits = Left (RequestRefused (MeasureBudgetExhausted (BitsExhausted derivedBits)))
  | reserved >= limit = Left (FractionBudgetExhausted reserved limit)
  | otherwise = inverseLength (limit - reserved) target trail >>= admitSample bits . widen
 where
  bits = budgetBits budget
  share = unitIntervalValue fraction
  target = share * lengthEnclosureLower total
  -- The fraction first; the derived target and reserve only once it is admitted.
  shareBits = exactRationalBitWidth share
  derivedBits = max (exactRationalBitWidth target) (exactRationalBitWidth reserved)
  limit = positiveExactValue tolerance
  reserved = share * lengthEnclosureWidth total
  widen (ArcSample site from to residual) = ArcSample site from to (residual + reserved)

-- | A returned sample's residual, bracket and point are retained exact values,
-- admitted like any other before the sample is returned.
admitSample :: Int -> ArcSample -> Either MeasureError ArcSample
admitSample budget sample@(ArcSample site from to residual)
  | width > budget = Left (SpanRefused (siteStepIndex site) from to (MeasureBudgetExhausted (BitsExhausted width)))
  | otherwise = Right sample
 where
  width = foldr (max . exactRationalBitWidth) (exactPointBitWidth (sitePoint site))
    [residual, unitIntervalValue from, unitIntervalValue to]

-- | The query context shared by one inverse: the source, precision, bit
-- budget, residual allowance and target.
data Inverse = Inverse !Subpath !RadicalPrecision !Int !ExactRational !ExactRational

-- | Invariant: the target is at most the trail's certified lower length.
inverseLength :: ExactRational -> ExactRational -> MeasuredTrail -> Either MeasureError ArcSample
inverseLength limit target (MeasuredTrail source (MeasurePolicy _ precision budget) spans _) =
  case firstMonotone (\span' -> lengthEnclosureUpper (spanAfter span') > target) spans of
    -- No span ends certainly beyond the request, so it equals the trail's
    -- exact length and the trail's end answers it.
    Nothing -> case Seq.viewr spans of
      EmptyR -> Left EmptyTrailSample
      _ :> final -> boundary final
    Just span'@(MeasuredSpan (Leaf step t0 t1 start piece _) before)
      | lengthEnclosureLower (spanAfter span') >= target ->
          bisect inverse step depth start t0 t1 piece before (spanAfter span')
      -- The span's end is neither certainly before nor after the request.
      | otherwise -> boundary span'
 where
  inverse = Inverse source precision (budgetBits budget) limit target
  depth = budgetDepth budget
  boundary span'@(MeasuredSpan (Leaf step _ t1 start piece _) _) =
    let after = spanAfter span'
        residual = max (target - lengthEnclosureLower after) (lengthEnclosureUpper after - target)
     in if residual <= limit
          then Right (ArcSample (trailSite source step t1 (translateExactPoint start (curveStepEnd piece))) t1 t1 residual)
          else Left (SpanRefused (sourceStepIndex step) t1 t1 (AmbiguousDistance residual))

-- | Invariant: the arc distance at @tLo@ is at most the target and at @tHi@
-- at least it, witnessed by the separated enclosures @before@ and @after@.
-- As in 'descend', a piece is admitted before any enclosure is taken from it,
-- and the left child before its chord and polygon are observed.
bisect
  :: Inverse -> SourceStep -> Int -> ExactPoint -> UnitInterval -> UnitInterval -> CurveStep
  -> LengthEnclosure -> LengthEnclosure -> Either MeasureError ArcSample
bisect inverse@(Inverse source precision budget limit target) step depth from tLo tHi piece before after
  | width > budget = refuse (MeasureBudgetExhausted (BitsExhausted width))
  | pieceBits > budget = refuse (MeasureBudgetExhausted (BitsExhausted pieceBits))
  | residual <= limit = Right (ArcSample (trailSite source step tLo from) tLo tHi residual)
  | depth == 0 = refuse (MeasureBudgetExhausted DepthExhausted)
  | leftWidth > budget = Left (SpanRefused (sourceStepIndex step) tLo tMiddle (MeasureBudgetExhausted (BitsExhausted leftWidth)))
  | observed > budget = refuse (MeasureBudgetExhausted (BitsExhausted observed))
  | lengthEnclosureUpper middle <= target =
      bisect inverse step (depth - 1) splitPoint tMiddle tHi right middle after
  | lengthEnclosureLower middle >= target =
      bisect inverse step (depth - 1) from tLo tMiddle left before middle
  | middleResidual <= limit =
      Right (ArcSample (trailSite source step tMiddle splitPoint) tMiddle tMiddle middleResidual)
  | otherwise = refuse (AmbiguousDistance middleResidual)
 where
  refuse :: MeasureObligation -> Either MeasureError ArcSample
  refuse = Left . SpanRefused (sourceStepIndex step) tLo tHi
  width = spanBits from tLo tHi piece
  -- Every parameter of the bracket lies between the bracket's arc distances,
  -- which differ by at most the piece's control polygon.
  piecePolygon = controlPolygon precision piece
  pieceBits = enclosureBits piecePolygon
  residual = min (lengthEnclosureUpper piecePolygon)
    (max (target - lengthEnclosureLower before) (lengthEnclosureUpper after - target))
  (left, right) = splitStep unitHalf piece
  tMiddle = unitMidpoint tLo tHi
  splitPoint = translateExactPoint from (curveStepEnd left)
  leftWidth = spanBits from tLo tMiddle left
  leftChord = chordLength precision left
  leftPolygon = controlPolygon precision left
  middle = before <> enclosureBetween leftChord leftPolygon
  observed = max (enclosureBits leftChord) (max (enclosureBits leftPolygon) (enclosureBits middle))
  middleResidual = min (lengthEnclosureUpper piecePolygon)
    (max (target - lengthEnclosureLower middle) (lengthEnclosureUpper middle - target))

spanAfter :: MeasuredSpan -> LengthEnclosure
spanAfter (MeasuredSpan leaf before) = before <> leafLength leaf

leafLength :: Leaf -> LengthEnclosure
leafLength (Leaf _ _ _ _ _ enclosure) = enclosure

-- | The first element satisfying a predicate that is monotone along the
-- sequence, by bisection.
firstMonotone :: (a -> Bool) -> Seq a -> Maybe a
firstMonotone holds items =
  let (before, rest) = Seq.splitAt (Seq.length items `div` 2) items
   in case Seq.viewl rest of
        EmptyL -> Nothing
        middle :< after
          | holds middle -> firstMonotone holds before <|> Just middle
          | otherwise -> firstMonotone holds after

chordLength :: RadicalPrecision -> CurveStep -> LengthEnclosure
chordLength precision step = euclideanLengthEnclosure precision [coordinates (curveStepEnd step)]

controlPolygon :: RadicalPrecision -> CurveStep -> LengthEnclosure
controlPolygon precision step =
  euclideanLengthEnclosure precision (zipWith edge controls (drop 1 controls))
 where
  controls = toList (stepControlPoints step)
  edge (ExactVector ax ay) (ExactVector bx by) = (bx - ax, by - ay)

coordinates :: ExactVector -> (ExactRational, ExactRational)
coordinates (ExactVector x y) = (x, y)

admitBits :: Int -> (MeasureObligation -> MeasureError) -> Int -> Either MeasureError ()
admitBits budget refuse width = when (width > budget) (Left (refuse (MeasureBudgetExhausted (BitsExhausted width))))

admitEnclosure :: Int -> (MeasureObligation -> MeasureError) -> LengthEnclosure -> Either MeasureError ()
admitEnclosure budget refuse = admitBits budget refuse . enclosureBits

enclosureBits :: LengthEnclosure -> Int
enclosureBits enclosure =
  max (exactRationalBitWidth (lengthEnclosureLower enclosure)) (exactRationalBitWidth (lengthEnclosureUpper enclosure))
