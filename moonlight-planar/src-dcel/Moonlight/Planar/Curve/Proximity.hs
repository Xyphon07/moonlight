-- | Bounded distance and clearance between two curves, in the coordinates of
-- the submitted curves. Distance and contact are separate obligations: a
-- distance enclosure never implies contact, and contact is certified only by
-- an exact common point or a certified crossing. Proximity certifies "within
-- a distance", never "exactly on", except at evaluated rational sites.
--
-- The search is best-first over pairs of source spans, one from each curve,
-- in exact rational arithmetic. A pair's lower bound is the squared
-- distance between its pieces' control hulls, since a positive-weight piece
-- lies in the convex hull of its controls. The upper bound is the
-- least squared distance between evaluated sites: every piece's endpoints,
-- whose points are those of exact subdivision, and on a straight piece the
-- nearest point to the other piece's endpoints, at its rational parameter. A
-- pair whose lower bound exceeds the upper bound is dropped, since the
-- minimum does not lie there; a pair of two stationary pieces is exact and
-- kept only as sites. The pair with the least lower bound is refined by
-- halving its larger piece. The one square root is taken when an enclosure
-- is reported.
--
-- The pairs left live are candidates: several stay several, and none is
-- claimed to hold a unique nearest point. The subdivision budget bounds
-- depth below any source step and the pairs evaluated in all, and its bits
-- bound every exact value retained or compared, each admitted before it is:
-- the threshold and its square; every span, located controls included, so
-- every endpoint site; every projected site, site displacement and its
-- square, hull gap and its square; every crossing certificate, admitted
-- where it is made; and both ends of every root enclosure, as Measure admits
-- its enclosures. A projected site, being optional, is dropped when too wide;
-- anything else too wide refuses. The radical precision bounds only the
-- reported enclosure.
module Moonlight.Planar.Curve.Proximity
  ( CurveSpan
  , curveSpanSource
  , curveSpanStep
  , curveSpanFrom
  , curveSpanTo
  , ProximityCandidate
  , candidateFirst
  , candidateSecond
  , DistanceObservation
  , distanceBounds
  , distanceWitness
  , distanceCandidates
  , ProximityObligation (..)
  , ProximityError (..)
  , curveDistance
  , ContactWitness (..)
  , ClearanceVerdict (..)
  , clearance
  ) where

import Control.DeepSeq (NFData (..))
import Control.Monad (foldM)
import Data.Either (isRight)
import Data.Foldable (toList, traverse_)
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Moonlight.Planar.Curve
  ( CurveShapeView (..), Subpath (..), curveStepEnd, curveStepShape, shapeView )
import Moonlight.Planar.Curve.Measure
  ( Distance, MeasurePolicy, distanceValue, measureBudget, measurePrecision, measureTolerance )
import Moonlight.Planar.Exact
  ( ExactBounds, ExactPoint, ExactRational, ExactVector (..), UnitInterval, boundsMaximumX
  , boundsMaximumY, boundsMinimumX, boundsMinimumY, exactPointBitWidth, exactPointCoordinates
  , exactPointsBounds, exactRationalBitWidth, positiveExact, positiveExactValue, translateExactPoint
  , unitIntervalValue )
import Moonlight.Planar.Internal.CurveBudget
  ( BudgetObligation (..), SubdivisionBudget, budgetBits, budgetDepth, budgetLeaves )
import Moonlight.Planar.Internal.CurveCertificate
  ( CrossingCertificate, CrossingVerdict (..), crossingVerdict, hullGap, stationaryPiece )
import Moonlight.Planar.Internal.CurveSource
  ( SourceSpan, TrailSite, admitSpan, halveSpan, sitePoint, sourceSpanControls, sourceSpanFrom, sourceSpanPiece
  , sourceSpanStart, sourceSpanStep, sourceSpanTo, sourceStepIndex, sourceSteps, trailSite, wholeSpan )
import Moonlight.Planar.Internal.ExactRational (divideByPositive, unitBetween, unitClamp)
import Moonlight.Planar.Internal.Length
  ( LengthEnclosure, RadicalPrecision, enclosureBetween, euclideanLengthEnclosure
  , lengthEnclosureLower, lengthEnclosureUpper, lengthEnclosureWidth )

-- | A bracket of one source step of a curve: the source, the step's index
-- among its steps ('closedTrailSteps' order for a closed trail), and the
-- bracket in that step's parameter.
data CurveSpan = CurveSpan !Subpath !SourceSpan
  deriving stock (Eq, Show)

instance NFData CurveSpan where
  rnf (CurveSpan source span') = rnf source `seq` rnf span'

curveSpanSource :: CurveSpan -> Subpath
curveSpanSource (CurveSpan source _) = source

curveSpanStep :: CurveSpan -> Int
curveSpanStep (CurveSpan _ span') = sourceStepIndex (sourceSpanStep span')

curveSpanFrom :: CurveSpan -> UnitInterval
curveSpanFrom (CurveSpan _ span') = sourceSpanFrom span'

curveSpanTo :: CurveSpan -> UnitInterval
curveSpanTo (CurveSpan _ span') = sourceSpanTo span'

-- | A live pair of spans, one on each curve, whose lower bound does not
-- exceed the distance found: the minimum may lie there.
data ProximityCandidate = ProximityCandidate !CurveSpan !CurveSpan
  deriving stock (Eq, Show)

instance NFData ProximityCandidate where
  rnf (ProximityCandidate first second) = rnf first `seq` rnf second

candidateFirst :: ProximityCandidate -> CurveSpan
candidateFirst (ProximityCandidate first _) = first

candidateSecond :: ProximityCandidate -> CurveSpan
candidateSecond (ProximityCandidate _ second) = second

-- | The distance between the curves lies in the enclosure, whose width is at
-- most the policy tolerance; the two sites, one on each curve, are as far
-- apart as its upper end allows.
data DistanceObservation = DistanceObservation !LengthEnclosure !TrailSite !TrailSite !(Seq ProximityCandidate)
  deriving stock (Eq, Show)

instance NFData DistanceObservation where
  rnf (DistanceObservation enclosure first second candidates) =
    rnf enclosure `seq` rnf first `seq` rnf second `seq` rnf candidates

distanceBounds :: DistanceObservation -> LengthEnclosure
distanceBounds (DistanceObservation enclosure _ _ _) = enclosure

distanceWitness :: DistanceObservation -> (TrailSite, TrailSite)
distanceWitness (DistanceObservation _ first second _) = (first, second)

distanceCandidates :: DistanceObservation -> Seq ProximityCandidate
distanceCandidates (DistanceObservation _ _ _ candidates) = candidates

-- | Why a distance was not enclosed within the tolerance: the subdivision
-- budget, or, carrying the enclosure's rounding and the tolerance, a radical
-- precision too coarse for the tolerance however far the search refines.
data ProximityObligation
  = ProximityBudgetExhausted !BudgetObligation
  | ProximityPrecisionExhausted !ExactRational !ExactRational
  deriving stock (Eq, Show)

-- | A curve with no steps has no span to measure from. A clearance threshold
-- or its square wider than the bit budget is refused on entry. Step pairs,
-- first curve's steps by second's, more than the leaf budget are refused
-- from the two counts alone, before any source is admitted or pair built. A
-- source step whose start, controls or weights, relative or located, exceed
-- the bit budget is refused before any pair is evaluated. A value wider than the bit budget
-- met before any enclosure is admitted, or in the enclosure a result would
-- report, is refused with no enclosure. Otherwise a refused distance carries
-- the last enclosure admitted and the live candidates there.
data ProximityError
  = EmptyProximitySource
  | ProximityRequestRefused !ProximityObligation
  | ProximitySourceRefused !CurveSpan !BudgetObligation
  | ProximityStepPairsRefused !Int !Int
  | ProximityUnenclosed !ProximityObligation
  | ProximityRefused !ProximityObligation !LengthEnclosure !(Seq ProximityCandidate)
  deriving stock (Eq, Show)

-- | Why clearance fails: two evaluated sites no farther apart than the
-- threshold, one on each curve; one exact point on both curves; or a
-- certified single transversal crossing of two spans, whose point, algebraic
-- in general, is not given.
data ContactWitness
  = CloserThan !TrailSite !TrailSite
  | SharedPoint !TrailSite !TrailSite
  | TransversalCrossing !CurveSpan !CurveSpan !CrossingCertificate
  deriving stock (Eq, Show)

instance NFData ContactWitness where
  rnf (CloserThan first second) = rnf first `seq` rnf second
  rnf (SharedPoint first second) = rnf first `seq` rnf second
  rnf (TransversalCrossing first second certificate) = rnf first `seq` rnf second `seq` rnf certificate

-- | Clearance at a threshold @d@ is strict: it holds when the distance
-- exceeds @d@, so at zero it holds exactly when the curves are disjoint. It
-- is violated, with a witness, when the distance is at most @d@. Otherwise
-- the budget ran out first, with the enclosure and candidates reached; a
-- tangency at a parameter subdivision never reaches, or a distance of
-- exactly @d@ approached from above, never resolves. Enclosures are reported
-- at the policy's precision and are not bound by its tolerance.
data ClearanceVerdict
  = ClearanceHolds !LengthEnclosure
  | ClearanceViolated !ContactWitness
  | ClearanceUnresolved !LengthEnclosure !(Seq ProximityCandidate) !BudgetObligation
  deriving stock (Eq, Show)

instance NFData ClearanceVerdict where
  rnf (ClearanceHolds enclosure) = rnf enclosure
  rnf (ClearanceViolated witness) = rnf witness
  rnf (ClearanceUnresolved enclosure candidates obligation) =
    rnf enclosure `seq` rnf candidates `seq` rnf obligation

-- | The distance between two curves within the policy tolerance. Exhaustion
-- refuses; a success is never wider than the tolerance.
curveDistance :: MeasurePolicy -> Subpath -> Subpath -> Either ProximityError DistanceObservation
curveDistance policy first second = startSearch False budget first second >>= search Nothing
 where
  budget = measureBudget policy
  precision = measurePrecision policy
  tolerance = positiveExactValue (measureTolerance policy)
  -- The last admitted enclosure and its candidates, when there is one.
  search
    :: Maybe (LengthEnclosure, Seq ProximityCandidate) -> Search -> Either ProximityError DistanceObservation
  search admitted reached@(Search frontier (Nearest _ _ siteA siteB) _ _ _ _) =
    case enclosed budget precision reached of
      Left obligation -> Left (refusal (ProximityBudgetExhausted obligation) admitted)
      Right (lower, upper)
        | lengthEnclosureWidth reached' <= tolerance -> Right (DistanceObservation reached' siteA siteB candidates)
        | otherwise -> case Map.minViewWithKey frontier of
            -- Refinement narrows a geometric gap; once rounding alone exceeds
            -- the tolerance, or the bounds are exact, it cannot help.
            Just ((_, (_, pair)), rest)
              | optimisticGap > tolerance || rounding <= tolerance ->
                  either
                    (\(obligation, _) -> Left (refusal (ProximityBudgetExhausted obligation) here))
                    (search here)
                    (refine budget reached rest pair)
            _ -> Left (refusal (ProximityPrecisionExhausted rounding tolerance) here)
       where
        reached' = enclosureBetween lower upper
        candidates = liveCandidates reached
        here = Just (reached', candidates)
        optimisticGap = lengthEnclosureLower upper - lengthEnclosureUpper lower
        rounding = lengthEnclosureWidth lower + lengthEnclosureWidth upper
  refusal :: ProximityObligation -> Maybe (LengthEnclosure, Seq ProximityCandidate) -> ProximityError
  refusal obligation = maybe (ProximityUnenclosed obligation) (uncurry (ProximityRefused obligation))

-- | Strict clearance at the threshold between two curves.
clearance :: MeasurePolicy -> Distance -> Subpath -> Subpath -> Either ProximityError ClearanceVerdict
clearance policy threshold first second = do
  -- The threshold before its square, so an over-wide threshold is never squared.
  either (Left . ProximityRequestRefused . ProximityBudgetExhausted) Right
    (admitWidth budget (exactRationalBitWidth level) *> admitWidth budget (exactRationalBitWidth limit))
  startSearch True budget first second >>= decide
 where
  budget = measureBudget policy
  precision = measurePrecision policy
  level = distanceValue threshold
  limit = level * level
  decide :: Search -> Either ProximityError ClearanceVerdict
  decide reached@(Search frontier (Nearest squared _ siteA siteB) crossing _ _ _)
    | squared == 0 = Right (ClearanceViolated (SharedPoint siteA siteB))
    | squared <= limit = Right (ClearanceViolated (CloserThan siteA siteB))
    | Just (spanA, spanB, certificate) <- crossing = Right (ClearanceViolated (TransversalCrossing spanA spanB certificate))
    | otherwise = case Map.minViewWithKey frontier of
        Just (((gap, _), (_, pair)), rest) | gap <= limit ->
          either
            (\(obligation, _) -> (\enclosure -> ClearanceUnresolved enclosure (liveCandidates reached) obligation) <$> report reached)
            decide (refine budget reached rest pair)
        -- No live pair lies within the threshold.
        _ -> ClearanceHolds <$> report reached
  report :: Search -> Either ProximityError LengthEnclosure
  report reached =
    either (Left . ProximityUnenclosed . ProximityBudgetExhausted) (Right . uncurry enclosureBetween)
      (enclosed budget precision reached)

-- A piece of one curve: its depth below its source step, its source, its
-- span, and its control box, whose larger side picks the piece to halve.
data Piece = Piece !Int !Subpath !SourceSpan !ExactBounds

data Pair = Pair !Piece !Piece

-- A pair of evaluated sites, one on each curve, with their squared distance
-- and displacement.
data Nearest = Nearest !ExactRational !ExactVector !TrailSite !TrailSite

-- The live pairs keyed by squared gap and admission ordinal, each with its
-- gap displacement; the nearest sites; the first certified crossing, when
-- crossings are watched; the pairs evaluated; and whether crossings are
-- watched.
data Search = Search
  !(Map (ExactRational, Int) (ExactVector, Pair)) !Nearest
  !(Maybe (CurveSpan, CurveSpan, CrossingCertificate)) !Int !Int !Bool

-- Every step pair, evaluated in order. The pair count is decided from the
-- two step counts, their exact product against the leaf budget, before any
-- source is admitted or any pair is built; an empty curve's product is zero.
startSearch :: Bool -> SubdivisionBudget -> Subpath -> Subpath -> Either ProximityError Search
startSearch watching budget first second
  | toInteger countA * toInteger countB > toInteger (budgetLeaves budget) =
      Left (ProximityStepPairsRefused countA countB)
  | otherwise = do
      piecesA <- traverse (admitSource budget) (sourcePieces first)
      piecesB <- traverse (admitSource budget) (sourcePieces second)
      case (piecesA, piecesB) of
        (a : _, b : _) -> either (Left . ProximityUnenclosed . ProximityBudgetExhausted) Right $ do
          nearest :| _ <- pairSites budget (Pair a b)
          foldM (enter budget) (Search Map.empty nearest Nothing 0 0 watching)
            [Pair pieceA pieceB | pieceA <- piecesA, pieceB <- piecesB]
        _ -> Left EmptyProximitySource
 where
  countA = Seq.length (sourceSteps first)
  countB = Seq.length (sourceSteps second)

-- Halve the pair's larger refinable piece and admit both children. A refusal
-- keeps the search as it was, the pair still live.
refine
  :: SubdivisionBudget -> Search -> Map (ExactRational, Int) (ExactVector, Pair) -> Pair
  -> Either (BudgetObligation, Search) Search
refine budget reached rest pair =
  either (\obligation -> Left (obligation, reached)) Right $ do
    children <- split budget pair
    foldM (admit budget) (withFrontier rest reached) children

admit :: SubdivisionBudget -> Search -> Pair -> Either BudgetObligation Search
admit budget reached@(Search _ _ _ spent _ _) pair
  | spent >= budgetLeaves budget = Left LeavesExhausted
  | otherwise = enter budget reached pair

-- Evaluate a pair: its sites may improve the nearest, which prunes every
-- pair whose gap now exceeds it; the pair stays live if its gap does not and
-- one of its pieces can move; a watched crossing is recorded once. Every
-- value retained or compared is admitted first.
enter :: SubdivisionBudget -> Search -> Pair -> Either BudgetObligation Search
enter budget (Search frontier nearest crossing spent ordinal watching) pair = do
  sites <- pairSites budget pair
  (gapVector, gap) <- pairGap budget pair
  let nearest' = foldl' closer nearest sites
      bound = nearestSquared nearest'
      kept
        | gap <= bound && refinable pair = Map.insert (gap, ordinal) (gapVector, pair) frontier
        | otherwise = frontier
  crossing' <- case crossing of
    Nothing | watching && gap == 0 -> pairCrossing budget pair
    _ -> Right crossing
  pure (Search (Map.takeWhileAntitone ((<= bound) . fst) kept) nearest' crossing' (spent + 1) (ordinal + 1) watching)

withFrontier :: Map (ExactRational, Int) (ExactVector, Pair) -> Search -> Search
withFrontier frontier (Search _ nearest crossing spent ordinal watching) =
  Search frontier nearest crossing spent ordinal watching

split :: SubdivisionBudget -> Pair -> Either BudgetObligation [Pair]
split budget (Pair a b)
  | refinable' a && (not (refinable' b) || extent a >= extent b) =
      (\(left, right) -> [Pair left b, Pair right b]) <$> halve budget a
  | refinable' b = (\(left, right) -> [Pair a left, Pair a right]) <$> halve budget b
  | otherwise = Left DepthExhausted
 where
  refinable' piece'@(Piece depth _ _ _) = moving piece' && depth < budgetDepth budget

halve :: SubdivisionBudget -> Piece -> Either BudgetObligation (Piece, Piece)
halve budget (Piece depth source span' _) = do
  let (left, right) = halveSpan span'
  traverse_ (admitSpan budget) [left, right]
  pure (piece (depth + 1) source left, piece (depth + 1) source right)

admitSource :: SubdivisionBudget -> Piece -> Either ProximityError Piece
admitSource budget source@(Piece _ subpath span' _) =
  either (Left . ProximitySourceRefused (CurveSpan subpath span')) (const (Right source)) (admitSpan budget span')

sourcePieces :: Subpath -> [Piece]
sourcePieces source = [piece 0 source (wholeSpan step) | step <- toList (sourceSteps source)]

piece :: Int -> Subpath -> SourceSpan -> Piece
piece depth source span' = Piece depth source span' (exactPointsBounds (sourceSpanControls span'))

moving :: Piece -> Bool
moving (Piece _ _ span' _) = not (stationaryPiece (sourceSpanPiece span'))

refinable :: Pair -> Bool
refinable (Pair a b) = moving a || moving b

extent :: Piece -> ExactRational
extent (Piece _ _ _ box) = max (boundsMaximumX box - boundsMinimumX box) (boundsMaximumY box - boundsMinimumY box)

-- The shortest displacement between two pieces' control hulls, zero where
-- they meet, and its square, admitted. Each piece lies in its hull, so the
-- square bounds the pair's squared distance from below; unlike the control
-- boxes, the hulls of two concentric arcs close on the arcs quadratically.
pairGap :: SubdivisionBudget -> Pair -> Either BudgetObligation (ExactVector, ExactRational)
pairGap budget (Pair (Piece _ _ spanA _) (Piece _ _ spanB _)) =
  (gap, squared) <$ admitWidth budget (max (vectorBits gap) (exactRationalBitWidth squared))
 where
  gap = hullGap (sourceSpanControls spanA) (sourceSpanControls spanB)
  squared = dot gap gap

-- A certified crossing, its certificate admitted by 'crossingVerdict'.
pairCrossing :: SubdivisionBudget -> Pair -> Either BudgetObligation (Maybe (CurveSpan, CurveSpan, CrossingCertificate))
pairCrossing budget (Pair (Piece _ sourceA spanA _) (Piece _ sourceB spanB _)) =
  fmap single (crossingVerdict budget (sourceSpanStart spanA) (sourceSpanPiece spanA) (sourceSpanStart spanB) (sourceSpanPiece spanB))
 where
  single verdict = case verdict of
    Just (SingleCrossing certificate) -> Just (CurveSpan sourceA spanA, CurveSpan sourceB spanB, certificate)
    _ -> Nothing

-- Every endpoint pairing, and each endpoint's nearest point on the other
-- piece when that piece is straight. An endpoint site is a located control
-- of an admitted span; the pairings are admitted or refuse, and a projected
-- pairing too wide is dropped.
pairSites :: SubdivisionBudget -> Pair -> Either BudgetObligation (NonEmpty Nearest)
pairSites budget (Pair a b) = do
  corners <- traverse (admitNearest budget) (sitePair <$> ends a <*> ends b)
  let projected =
        mapMaybe (\siteB -> (`sitePair` siteB) <$> projection budget a (sitePoint siteB)) (toList (ends b))
          <> mapMaybe (\siteA -> sitePair siteA <$> projection budget b (sitePoint siteA)) (toList (ends a))
  pure (NonEmpty.appendList corners (filter (isRight . admitNearest budget) projected))

admitNearest :: SubdivisionBudget -> Nearest -> Either BudgetObligation Nearest
admitNearest budget near@(Nearest squared gap _ _) =
  near <$ admitWidth budget (max (vectorBits gap) (exactRationalBitWidth squared))

-- A piece's two endpoint sites, at the points of its exact subdivision.
ends :: Piece -> NonEmpty TrailSite
ends (Piece _ source span' _) =
  trailSite source step (sourceSpanFrom span') start
    :| [trailSite source step (sourceSpanTo span') (translateExactPoint start (curveStepEnd (sourceSpanPiece span')))]
 where
  step = sourceSpanStep span'
  start = sourceSpanStart span'

-- The nearest point of a straight piece to a point, as a site. A straight
-- piece is its step restricted to its bracket, affinely in the parameter, so
-- the piece's fraction @t@ is the step's parameter a fraction @t@ through the
-- bracket, and the point there is the step's. A site wider than the bit
-- budget is not retained.
projection :: SubdivisionBudget -> Piece -> ExactPoint -> Maybe TrailSite
projection budget (Piece _ source span' _) point = case shapeView (curveStepShape (sourceSpanPiece span')) of
  LinearView -> do
    squaredLength <- either (const Nothing) Just (positiveExact (dot direction direction))
    let fraction = unitClamp (divideByPositive (dot (displacement start point) direction) squaredLength)
        nearest = translateExactPoint start (scale (unitIntervalValue fraction) direction)
        parameter = unitBetween (sourceSpanFrom span') (sourceSpanTo span') fraction
    if max (exactPointBitWidth nearest) (exactRationalBitWidth (unitIntervalValue parameter)) <= budgetBits budget
      then Just (trailSite source (sourceSpanStep span') parameter nearest)
      else Nothing
  _ -> Nothing
 where
  start = sourceSpanStart span'
  direction = curveStepEnd (sourceSpanPiece span')

sitePair :: TrailSite -> TrailSite -> Nearest
sitePair siteA siteB = Nearest (dot gap gap) gap siteA siteB
 where
  gap = displacement (sitePoint siteA) (sitePoint siteB)

closer :: Nearest -> Nearest -> Nearest
closer current candidate
  | nearestSquared candidate < nearestSquared current = candidate
  | otherwise = current

nearestSquared :: Nearest -> ExactRational
nearestSquared (Nearest squared _ _ _) = squared

-- Root enclosures of the least live gap and of the nearest distance, both
-- ends of each admitted as Measure admits its enclosures.
enclosed :: SubdivisionBudget -> RadicalPrecision -> Search -> Either BudgetObligation (LengthEnclosure, LengthEnclosure)
enclosed budget precision (Search frontier (Nearest _ gap _ _) _ _ _ _) =
  (lower, upper) <$ traverse_ (admitWidth budget . enclosureBits) [lower, upper]
 where
  upper = root gap
  lower = maybe upper (root . fst . snd) (Map.lookupMin frontier)
  root (ExactVector x y) = euclideanLengthEnclosure precision [(x, y)]
  enclosureBits enclosure =
    max (exactRationalBitWidth (lengthEnclosureLower enclosure)) (exactRationalBitWidth (lengthEnclosureUpper enclosure))

admitWidth :: SubdivisionBudget -> Int -> Either BudgetObligation ()
admitWidth budget width
  | width > budgetBits budget = Left (BitsExhausted width)
  | otherwise = Right ()

vectorBits :: ExactVector -> Int
vectorBits (ExactVector x y) = max (exactRationalBitWidth x) (exactRationalBitWidth y)

liveCandidates :: Search -> Seq ProximityCandidate
liveCandidates (Search frontier _ _ _ _ _) =
  Seq.fromList
    [ ProximityCandidate (CurveSpan sourceA spanA) (CurveSpan sourceB spanB)
    | (_, Pair (Piece _ sourceA spanA _) (Piece _ sourceB spanB _)) <- Map.elems frontier ]

displacement :: ExactPoint -> ExactPoint -> ExactVector
displacement from to =
  let (ax, ay) = exactPointCoordinates from
      (bx, by) = exactPointCoordinates to
   in ExactVector (bx - ax) (by - ay)

dot :: ExactVector -> ExactVector -> ExactRational
dot (ExactVector ax ay) (ExactVector bx by) = ax * bx + ay * by

scale :: ExactRational -> ExactVector -> ExactVector
scale factor (ExactVector x y) = ExactVector (factor * x) (factor * y)
