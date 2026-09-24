{-# LANGUAGE DerivingStrategies #-}

-- | Simple outer/hole assembly of closed curves, certified. A region is
-- lowered only after its curves are proved simple, pairwise disjoint and
-- wound as their roles say, and its polygon is admitted only when the
-- lowered loops nest as the curves do. The certificate reads exact
-- predicates on control points alone, never the lowering's metric.
--
-- The domain is four predicates within a finite subdivision budget. D1:
-- every piece strictly advances along some direction. D2: at every joint,
-- with stationary steps contracted, the incoming piece lies ahead of the
-- joint and the outgoing piece behind it along one direction. D3: every
-- other pair of pieces, in one contour or two, has disjoint control hulls or
-- a certified absence of crossing. Under D1 to D3 the straight-line homotopy
-- of every piece to its chord is an isotopy of the whole configuration that
-- fixes the piece endpoints: each contour is simple, the contours are
-- pairwise disjoint, a contour's winding is its chord polygon's, and a point
-- outside every piece's control hull lies inside a contour exactly when it
-- lies inside that contour's chord polygon. D4: the admitted polygon's loops
-- are pairwise boundary-disjoint and each lies inside another exactly when
-- its curve does.
--
-- Outside the domain it refuses: with the certified contact or crossing
-- when one is found, and otherwise with the unresolved obligation and the
-- budget it spent. Contact is certified only at a source step's own
-- endpoint, where the point is exact and on both curves; any other tangency
-- is never certified either way.
module Moonlight.Planar.Curve.Region
  ( CurveComponent (..)
  , ContourRole (..)
  , ContourRef (..)
  , ContourSpan (..)
  , TopologyObstruction (..)
  , CurveRegionError (..)
  , SubdivisionBudget
  , SubdivisionBudgetError (..)
  , subdivisionBudget
  , BudgetObligation (..)
  , CrossingCertificate
  , CurveTopologyEvidence
  , certifiedPieceCounts
  , certifiedPointLocation
  , certifySimpleRegion
  , lowerSimpleRegion
  ) where

import Control.DeepSeq (NFData (..))
import Control.Monad (join, unless, zipWithM)
import Data.Bifunctor (first)
import Data.Foldable (toList, traverse_)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (isJust, isNothing, listToMaybe)
import Moonlight.Planar.Curve
  ( ClosedTrail, CurveShapeView (..), CurveStep, Located, Subpath (..), curveStepEnd, curveStepShape
  , shapeView )
import Moonlight.Planar.Curve.Lowering
  ( LoweringPolicy, LoweringError, LoweredPath, lowerClosedTrail, loweredPoints )
import Moonlight.Planar.Exact
  ( ExactBounds, ExactPoint, UnitInterval, exactOrient2d, exactPointsBounds, pointInBounds
  , translateExactPoint )
import Moonlight.Planar.Internal.CurveBudget
  ( BudgetObligation (..), SubdivisionBudget, SubdivisionBudgetError (..), budgetDepth
  , budgetLeaves, subdivisionBudget )
import Moonlight.Planar.Internal.CurveCertificate
  ( CrossingCertificate, CrossingVerdict (..), crossingVerdict, hullSeparation, jointWitness
  , monotoneWitness, separatingAxis, stationaryPiece )
import Moonlight.Planar.Internal.CurveSource
  ( SourceSpan, admitSpan, halveSpan, sourceSpanControls, sourceSpanFrom, sourceSpanPiece, sourceSpanStart
  , sourceSpanStep, sourceSpanTo, sourceStepIndex, sourceSteps, wholeSpan )
import Moonlight.Planar.Internal.Region.Bounds (overlappingPairs)
import Moonlight.Planar.Internal.Region.Loop
  ( PreparedLoop, crossLoopRelations, cycleWinding, firstPreparedPoint, pointLocationInCycle, prepareLoop
  , preparedBounds, preparedPointLocation )
import Moonlight.Planar.Region
  ( ExactLoop, PlanarRegion, RegionPointLocation (..), RegionValidationError, exactLoop
  , polygonComponent, planarRegion )

-- | The submitted winding is significant: outer CCW, holes CW. An explicitly
-- located closed trail does not by itself claim simplicity or containment.
data CurveComponent = CurveComponent
  { curveOuter :: !(Located ClosedTrail)
  , curveHoles :: ![Located ClosedTrail]
  }

data ContourRole = OuterContour | HoleContour !Int
  deriving stock (Eq, Ord, Show)

-- | A contour by its component's submitted ordinal and its role there.
data ContourRef = ContourRef !Int !ContourRole
  deriving stock (Eq, Ord, Show)

-- | The part of a contour a refusal is about: a source step's index among
-- the closed trail's steps, closing step last, and a bracket in that step's
-- parameter.
data ContourSpan = ContourSpan !ContourRef !Int !UnitInterval !UnitInterval
  deriving stock (Eq, Show)

-- | Why a region's curves are outside the certified domain.
data TopologyObstruction
  = DegenerateContour !ContourRef
    -- ^ Every step of the contour is stationary.
  | SourceSpanRefused !ContourSpan !BudgetObligation
    -- ^ The source steps themselves exceed the budget.
  | UnresolvedMonotonicity !ContourSpan !BudgetObligation
  | UnseparatedJoin !ContourSpan !ContourSpan !BudgetObligation
    -- ^ The incoming and outgoing spans at a joint.
  | CertifiedContact !ContourSpan !ContourSpan !ExactPoint
    -- ^ Two non-adjacent source steps meet at an exact point on both: an
    -- endpoint of each, or an endpoint of one on the other, a line.
  | UnresolvedContact !ContourSpan !ContourSpan !BudgetObligation
  | CertifiedSelfCrossing !ContourSpan !ContourSpan !CrossingCertificate
  | CertifiedContourCrossing !ContourSpan !ContourSpan !CrossingCertificate
  | ContourWindingRefused !ContourRef !Ordering
    -- ^ A certified simple contour whose winding is not its role's. Under D1
    -- to D3 the isotopy to the chords keeps the contour simple, so its chord
    -- polygon is simple and carries the curve's winding.
  | PolygonTopologyDiffers !ContourRef !ContourRef
    -- ^ The two lowered loops touch, or the first lies inside the second,
    -- where the curves do not, or the reverse.
  deriving stock (Eq, Show)

data CurveRegionError
  = CurveTopologyRefused !TopologyObstruction
  | CurveLoweringRefused !LoweringError
  | CurvePolygonRefused !RegionValidationError
  deriving stock (Eq, Show)

-- | What certification proved about a region's curves, observed only through
-- the questions it answers exactly.
newtype CurveTopologyEvidence = CurveTopologyEvidence [CertifiedContour]

instance NFData CurveTopologyEvidence where
  rnf (CurveTopologyEvidence contours) = rnf contours

-- | A certified contour's chord cycle, anchor first, and its pieces' control
-- hulls.
data CertifiedContour = CertifiedContour !ContourRef !(NonEmpty ExactPoint) ![PieceHull]

instance NFData CertifiedContour where
  rnf (CertifiedContour ref chord hulls) = ref `seq` rnf chord `seq` rnf hulls

data PieceHull = PieceHull !ExactBounds !(NonEmpty ExactPoint)

instance NFData PieceHull where
  rnf (PieceHull bounds hull) = bounds `seq` rnf hull

-- | Each contour's certified piece count, in submitted order.
certifiedPieceCounts :: CurveTopologyEvidence -> [(ContourRef, Int)]
certifiedPieceCounts (CurveTopologyEvidence contours) =
  [(ref, length hulls) | CertifiedContour ref _ hulls <- contours]

-- | Where a point lies in the curves' region, when it lies strictly outside
-- every piece's control hull; the region is each component's outer contour
-- less its holes. Nearer the curves than that, it is not decided.
certifiedPointLocation :: CurveTopologyEvidence -> ExactPoint -> Maybe RegionPointLocation
certifiedPointLocation (CurveTopologyEvidence contours) point
  | any touchesHull [hull | CertifiedContour _ _ hulls <- contours, hull <- hulls] = Nothing
  | any outerWithoutHole inside = Just RegionInterior
  | otherwise = Just RegionExterior
 where
  touchesHull (PieceHull bounds hull) =
    pointInBounds point bounds && isNothing (separatingAxis (point :| []) hull)
  inside = [ref | CertifiedContour ref chord _ <- contours, pointLocationInCycle chord point == RegionInterior]
  outerWithoutHole (ContourRef component role) =
    role == OuterContour && not (any (\(ContourRef other otherRole) -> other == component && otherRole /= OuterContour) inside)

-- | Certify the region's curves: D1 to D3 and each contour's winding. The
-- budget bounds the whole region: depth below any source step, pieces over
-- all contours, and the bits of every piece, located controls included, and
-- of every crossing certificate, each admitted where it is made.
certifySimpleRegion
  :: SubdivisionBudget -> [CurveComponent] -> Either TopologyObstruction CurveTopologyEvidence
certifySimpleRegion budget components = do
  initial <- traverse (uncurry (initialContour budget)) (contourTrails components)
  case drop (budgetLeaves budget) [contourSpan ref sourceSpan | Contour ref pieces <- initial, Piece _ sourceSpan <- toList pieces] of
    extra : _ -> Left (SourceSpanRefused extra LeavesExhausted)
    [] -> Right ()
  traverse_ endpointContact (overlappingPairs placedBounds (concat (zipWith placeContour [0 ..] initial)))
  certified <- refineUntilCertified budget initial
  CurveTopologyEvidence <$> traverse certifiedContour certified

-- | Certify, lower each contour under the policy, admit the polygon region,
-- and check D4 on its loops. Receipts remain in submitted component, outer,
-- hole order, independently of polygon canonicalization. Coordinates remain
-- source-local; the policy's affine map only measures error, and its leaf
-- budget applies per contour.
lowerSimpleRegion
  :: LoweringPolicy
  -> SubdivisionBudget
  -> [CurveComponent]
  -> Either CurveRegionError (PlanarRegion, [LoweredPath], CurveTopologyEvidence)
lowerSimpleRegion policy budget components = do
  evidence@(CurveTopologyEvidence contours) <- first CurveTopologyRefused (certifySimpleRegion budget components)
  admitted <- traverse lowerComponent components
  region <- first CurvePolygonRefused (planarRegion [component | (component, _, _) <- admitted])
  loopsAgree contours (concat [loops | (_, _, loops) <- admitted])
  pure (region, concat [paths | (_, paths, _) <- admitted], evidence)
 where
  lowerComponent (CurveComponent outer holes) = do
    outerPath <- first CurveLoweringRefused (lowerClosedTrail policy outer)
    holePaths <- traverse (first CurveLoweringRefused . lowerClosedTrail policy) holes
    outerLoop <- first CurvePolygonRefused (exactLoop (loweredPoints outerPath))
    holeLoops <- traverse (first CurvePolygonRefused . exactLoop . loweredPoints) holePaths
    component <- first CurvePolygonRefused (polygonComponent outerLoop holeLoops)
    pure (component, outerPath : holePaths, outerLoop : holeLoops)

-- D4. Within a component the admitted polygon already has disjoint
-- boundaries, holes inside the outer loop and no hole inside another, so
-- boundary contact is asked only across components; nesting is compared for
-- every ordered pair. With boundaries disjoint, a loop's first point decides
-- whether it lies inside another.
loopsAgree :: [CertifiedContour] -> [ExactLoop] -> Either CurveRegionError ()
loopsAgree contours loops = do
  prepared <- first CurvePolygonRefused (traverse prepareLoop loops)
  let paired = zip contours prepared
  traverse_ disjointBoundaries
    (filter acrossComponents (overlappingPairs (preparedBounds . snd) paired))
  traverse_ sameNesting [(a, b) | a <- paired, b <- paired, certifiedRef (fst a) /= certifiedRef (fst b)]
 where
  acrossComponents :: ((CertifiedContour, PreparedLoop), (CertifiedContour, PreparedLoop)) -> Bool
  acrossComponents ((a, _), (b, _)) = certifiedComponent a /= certifiedComponent b
  disjointBoundaries ((a, loopA), (b, loopB)) = do
    relations <- first CurvePolygonRefused (crossLoopRelations loopA loopB)
    unless (null relations) (differs a b)
  sameNesting ((a, loopA), (b, loopB)) =
    unless
      (curveInside a b == (preparedPointLocation loopB (firstPreparedPoint loopA) == RegionInterior))
      (differs a b)
  differs :: CertifiedContour -> CertifiedContour -> Either CurveRegionError ()
  differs a b = Left (CurveTopologyRefused (PolygonTopologyDiffers (certifiedRef a) (certifiedRef b)))

-- | Whether the first certified contour lies inside the second. Its anchor is
-- a piece endpoint of a contour disjoint from the second's pieces throughout
-- the isotopy, so the second's chord polygon decides, never on its boundary.
curveInside :: CertifiedContour -> CertifiedContour -> Bool
curveInside (CertifiedContour _ (anchor :| _) _) (CertifiedContour _ chord _) =
  pointLocationInCycle chord anchor == RegionInterior

certifiedRef :: CertifiedContour -> ContourRef
certifiedRef (CertifiedContour ref _ _) = ref

certifiedComponent :: CertifiedContour -> Int
certifiedComponent (CertifiedContour (ContourRef component _) _ _) = component

-- A piece of a contour: its depth below its source step and its span.
data Piece = Piece !Int !SourceSpan

-- A contour's pieces in trail order, stationary steps contracted.
data Contour = Contour !ContourRef !(NonEmpty Piece)

-- A piece placed for one round: contour ordinal, position, the contour's
-- piece count, the contour, and the piece.
data Placed = Placed !Int !Int !Int !ContourRef !Piece

-- The refusal a demanded piece reports if the budget stops its halving.
type Refusal = BudgetObligation -> TopologyObstruction

contourTrails :: [CurveComponent] -> [(ContourRef, Located ClosedTrail)]
contourTrails components = concat (zipWith componentContours [0 ..] components)
 where
  componentContours component (CurveComponent outer holes) =
    (ContourRef component OuterContour, outer)
      : zipWith (\hole trail -> (ContourRef component (HoleContour hole), trail)) [0 ..] holes

initialContour
  :: SubdivisionBudget -> ContourRef -> Located ClosedTrail -> Either TopologyObstruction Contour
initialContour budget ref trail = do
  spans <- traverse admit (filter moving (map wholeSpan (toList (sourceSteps (ClosedSubpath trail)))))
  case spans of
    [] -> Left (DegenerateContour ref)
    span0 : rest -> Right (Contour ref (Piece 0 <$> span0 :| rest))
 where
  moving = not . stationaryPiece . sourceSpanPiece
  admit sourceSpan = sourceSpan <$ admitBits budget (SourceSpanRefused (contourSpan ref sourceSpan)) sourceSpan

refineUntilCertified :: SubdivisionBudget -> [Contour] -> Either TopologyObstruction [Contour]
refineUntilCertified budget contours = do
  demands <- roundDemands budget contours
  case Map.minView demands of
    Nothing -> Right contours
    Just (firstRefusal, _) -> do
      refined <- zipWithM (refineContour budget demands) [0 ..] contours
      unless (sum [length pieces | Contour _ pieces <- refined] <= budgetLeaves budget)
        (Left (firstRefusal LeavesExhausted))
      refineUntilCertified budget refined

-- Every piece this round cannot certify, keyed by contour ordinal and
-- position, with the first refusal it met: monotonicity, then joints, then
-- contacts. A certified crossing refuses at once.
roundDemands :: SubdivisionBudget -> [Contour] -> Either TopologyObstruction (Map (Int, Int) Refusal)
roundDemands budget contours = do
  contacts <- concat <$> traverse (contactDemands budget) (overlappingPairs placedBounds (concat placed))
  pure (Map.fromListWith (\_ earlier -> earlier) (monotonicity <> joints <> contacts))
 where
  placed = zipWith placeContour [0 ..] contours
  monotonicity =
    [ (placedKey piece, UnresolvedMonotonicity (placedSpan piece))
    | piece <- concat placed, isNothing (monotoneWitness (placedStep piece)) ]
  joints =
    [ demand
    | pieces <- placed
    , (incoming, outgoing) <- zip pieces (drop 1 pieces <> take 1 pieces)
    , isNothing (jointWitness (placedStep incoming) (placedStep outgoing))
    , let refusal = UnseparatedJoin (placedSpan incoming) (placedSpan outgoing)
    , demand <- [(placedKey incoming, refusal), (placedKey outgoing, refusal)] ]

placeContour :: Int -> Contour -> [Placed]
placeContour ordinal (Contour ref pieces) =
  zipWith (\position piece -> Placed ordinal position count ref piece) [0 ..] (toList pieces)
 where
  count = length pieces

contactDemands :: SubdivisionBudget -> (Placed, Placed) -> Either TopologyObstruction [((Int, Int), Refusal)]
contactDemands budget (a, b)
  | adjacent a b = Right []
  | isJust (hullSeparation startA stepA startB stepB) = Right []
  | otherwise = case crossingVerdict budget startA stepA startB stepB of
      Left obligation -> Left (refusal obligation)
      Right (Just (SingleCrossing certificate)) -> Left (crossing (placedSpan a) (placedSpan b) certificate)
      Right (Just (NoCrossing _)) -> Right []
      Right Nothing -> Right [(placedKey a, refusal), (placedKey b, refusal)]
 where
  startA = placedStart a
  stepA = placedStep a
  startB = placedStart b
  stepB = placedStep b
  refusal = UnresolvedContact (placedSpan a) (placedSpan b)
  crossing
    | placedContour a == placedContour b = CertifiedSelfCrossing
    | otherwise = CertifiedContourCrossing

-- Contact at the source steps' own endpoints, before any refinement: two
-- non-adjacent steps sharing an endpoint, or an endpoint of one on the other
-- when that is a line segment. Endpoints made by halving lie on the other
-- curve only by coincidence, so they are left to the refinement.
endpointContact :: (Placed, Placed) -> Either TopologyObstruction ()
endpointContact (a, b)
  | adjacent a b = Right ()
  | otherwise = maybe (Right ()) (Left . CertifiedContact (placedSpan a) (placedSpan b)) contact
 where
  contact = listToMaybe (filter (`onStep` b) (bothEnds a) <> filter (`onStep` a) (bothEnds b))
  bothEnds :: Placed -> [ExactPoint]
  bothEnds placed = let (start, end) = placedEnds placed in [start, end]

placedEnds :: Placed -> (ExactPoint, ExactPoint)
placedEnds placed = (start, translateExactPoint start (curveStepEnd (placedStep placed)))
 where
  start = placedStart placed

onStep :: ExactPoint -> Placed -> Bool
onStep point placed =
  point == start || point == end
    || (straight && exactOrient2d start end point == EQ && pointInBounds point (exactPointsBounds (start :| [end])))
 where
  (start, end) = placedEnds placed
  straight = case shapeView (curveStepShape (placedStep placed)) of
    LinearView -> True
    _ -> False

adjacent :: Placed -> Placed -> Bool
adjacent a@(Placed contourA positionA _ _ _) b@(Placed contourB positionB _ _ _) =
  contourA == contourB && (positionB == next a || positionA == next b)
 where
  next (Placed _ position count _ _) = (position + 1) `mod` count

refineContour
  :: SubdivisionBudget -> Map (Int, Int) Refusal -> Int -> Contour -> Either TopologyObstruction Contour
refineContour budget demands ordinal (Contour ref pieces) =
  Contour ref . join <$> traverse refine (NonEmpty.zip (0 :| [1 ..]) pieces)
 where
  refine (position, piece@(Piece depth sourceSpan)) = case Map.lookup (ordinal, position) demands of
    Nothing -> Right (piece :| [])
    Just refusal
      | depth >= budgetDepth budget -> Left (refusal DepthExhausted)
      | otherwise -> do
          let (left, right) = halveSpan sourceSpan
          traverse_ (admitBits budget refusal) [left, right]
          Right (Piece (depth + 1) left :| [Piece (depth + 1) right])

admitBits :: SubdivisionBudget -> Refusal -> SourceSpan -> Either TopologyObstruction ()
admitBits budget refusal sourceSpan = either (Left . refusal) (const (Right ())) (admitSpan budget sourceSpan)

certifiedContour :: Contour -> Either TopologyObstruction CertifiedContour
certifiedContour (Contour ref pieces)
  | winding == expected = Right (CertifiedContour ref chord (map pieceHull (toList pieces)))
  | otherwise = Left (ContourWindingRefused ref winding)
 where
  chord = (\(Piece _ sourceSpan) -> sourceSpanStart sourceSpan) <$> pieces
  winding = cycleWinding chord
  expected = case ref of
    ContourRef _ OuterContour -> GT
    ContourRef _ (HoleContour _) -> LT
  pieceHull (Piece _ sourceSpan) =
    let hull = sourceSpanControls sourceSpan in PieceHull (exactPointsBounds hull) hull

contourSpan :: ContourRef -> SourceSpan -> ContourSpan
contourSpan ref sourceSpan =
  ContourSpan ref (sourceStepIndex (sourceSpanStep sourceSpan)) (sourceSpanFrom sourceSpan) (sourceSpanTo sourceSpan)

placedKey :: Placed -> (Int, Int)
placedKey (Placed contour position _ _ _) = (contour, position)

placedContour :: Placed -> Int
placedContour (Placed contour _ _ _ _) = contour

placedSpan :: Placed -> ContourSpan
placedSpan (Placed _ _ _ ref (Piece _ sourceSpan)) = contourSpan ref sourceSpan

placedStart :: Placed -> ExactPoint
placedStart (Placed _ _ _ _ (Piece _ sourceSpan)) = sourceSpanStart sourceSpan

placedStep :: Placed -> CurveStep
placedStep (Placed _ _ _ _ (Piece _ sourceSpan)) = sourceSpanPiece sourceSpan

placedBounds :: Placed -> ExactBounds
placedBounds (Placed _ _ _ _ (Piece _ sourceSpan)) = exactPointsBounds (sourceSpanControls sourceSpan)
