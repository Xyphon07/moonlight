{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Exact rational planar geometry over admitted binary64 points.
module Moonlight.Triangulation.Exact
  ( ExactRational
  , ExactArithmeticError (..)
  , exactRational
  , exactRationalNumerator
  , exactRationalDenominator
  , exactRationalBitWidth
  , exactDivide
  , ExactPoint
  , exactPoint
  , exactPointCoordinates
  , exactPointCross
  , exactPointBitWidth
  , ExactAffineLine
  , ExactHalfPlaneError (..)
  , exactAffineLine
  , exactAffineLineCoefficients
  , oppositeExactAffineLine
  , exactAffineLineIntersection
  , ExactClosedHalfPlane
  , exactClosedHalfPlane
  , exactClosedHalfPlaneFromDirectedEdge
  , exactClosedHalfPlaneLine
  , classifyExactPoint
  , ExactRetainedPolygon
  , exactRetainedPolygon
  , exactRetainedPolygonPoints
  , ExactClipDisposition (..)
  , ExactClipError (..)
  , ExactClipReceipt (..)
  , exactClipRetainedPolygon
  , ExactVector (..)
  , exactVectorFromPoints
  , addExactVectors
  , exactCross
  , compareExactVectorAngle
  , translateExactPoint
  , ExactRay
  , exactRay
  , exactRayOrigin
  , exactRayDirection
  , ExactSegment
  , ExactGeometryError (..)
  , exactSegment
  , exactSegmentEndpoints
  , exactPointFromPoint
  , exactPointFromQueryPoint
  , exactPointToEmbeddingCandidate
  , exactOrient2d
  , exactOnClosedSegment
  , SegmentRelation (..)
  , allSegmentRelations
  , exactSegmentRelation
  , ExactIntersectionError (..)
  , exactLineIntersection
  , exactSupportingLineIntersection
  ) where

import Control.DeepSeq (NFData)
import Control.Monad (foldM)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (mapMaybe)
import qualified Data.Sequence as Sequence
import Data.Sequence (Seq, ViewL (..), ViewR (..), (|>))
import GHC.Generics (Generic)
import Moonlight.Triangulation.Internal.Dyadic (integerRatioToDouble)
import Moonlight.Triangulation.Internal.BoundaryCycle
  ( admitsSimpleCycleEdgeRelation
  , cyclePairsNonEmpty
  , firstNonCounterClockwiseTurn
  , unorderedPairs
  )
import Moonlight.Triangulation.Internal.ExactRational
  ( ExactArithmeticError (..)
  , ExactRational
  , exactDivide
  , exactRational
  , exactRationalBitWidth
  , exactRationalDenominator
  , exactRationalDenominatorBitWidth
  , exactRationalFromFiniteDouble
  , exactRationalIsZero
  , exactRationalNumerator
  , exactSignum
  )
import Moonlight.Triangulation.Internal.SegmentRelation
  ( SegmentRelation (..)
  , allSegmentRelations
  , segmentRelationWith
  )
import Moonlight.Triangulation.Math (mkQueryPoint)
import Moonlight.Triangulation.Types
  ( Point (..)
  , PointValidationError
  , QueryPoint
  , queryPointValue
  )

-- | A strict exact Cartesian point.
data ExactPoint = ExactPoint !ExactRational !ExactRational
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | An exact affine line @a*x + b*y + c = 0@ with a nonzero normal.
data ExactAffineLine =
  ExactAffineLine !ExactRational !ExactRational !ExactRational
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | The closed left half-plane of an oriented affine line. A point belongs
-- when the line evaluation is nonnegative.
newtype ExactClosedHalfPlane = ExactClosedHalfPlane ExactAffineLine
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | One polygon vertex together with the original affine line supporting its
-- incoming edge. Intersections therefore never reconstruct a line from
-- already-derived endpoints.
data ExactRetainedVertex =
  ExactRetainedVertex !ExactPoint !ExactAffineLine
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | A strict counter-clockwise convex polygon whose incoming edges retain
-- their original supporting equations.
newtype ExactRetainedPolygon =
  ExactRetainedPolygon (NonEmpty ExactRetainedVertex)
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Typed refusals from affine-line and retained-polygon admission.
data ExactHalfPlaneError
  = ExactAffineLineZeroNormal
      !ExactRational
      !ExactRational
      !ExactRational
  | ExactRetainedPolygonTooFewVertices !Int
  | ExactRetainedPolygonNonConvexTurn !Int !Ordering
  | ExactRetainedPolygonSelfRelation !Int !Int !SegmentRelation
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Dimensional disposition after exact closed-half-plane clipping.
data ExactClipDisposition
  = ExactClipFullDimensional !ExactRetainedPolygon
  | ExactClipLowerDimensional !(NonEmpty ExactPoint)
  | ExactClipEmpty
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Typed obstructions from an affine intersection that should be unique or
-- from an impossible orientation reversal during convex descent.
data ExactClipError
  = ExactClipIntersection !ExactIntersectionError
  | ExactClipOrientationReversed !ExactRational
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Structural work and exact-rational width observations for one angular
-- half-plane descent. Submitted half-planes exclude the domain; active
-- boundaries include domain edges admitted to the deque after
-- equal-direction coalescence; zero records a single-plane domain rejection
-- before deque construction.
-- Compatibility checks count deque endpoint predicates; intersections count
-- successful affine-line intersections actually evaluated. Widths describe
-- observed reduced values, not bounds.
data ExactClipReceipt = ExactClipReceipt
  { exactClipSubmittedHalfPlanes :: !Int
  , exactClipActiveBoundaries :: !Int
  , exactClipBoundaryCompatibilityChecks :: !Int
  , exactClipExactIntersections :: !Int
  , exactClipInputCoordinateBits :: !Int
  , exactClipMaximumAffineCoefficientBits :: !Int
  , exactClipPeakIntermediateCoordinateBits :: !Int
  , exactClipFinalCoordinateBits :: !Int
  , exactClipFinalDenominatorBits :: !Int
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

instance Semigroup ExactClipReceipt where
  left <> right =
    ExactClipReceipt
      { exactClipSubmittedHalfPlanes =
          exactClipSubmittedHalfPlanes left + exactClipSubmittedHalfPlanes right
      , exactClipActiveBoundaries =
          exactClipActiveBoundaries left + exactClipActiveBoundaries right
      , exactClipBoundaryCompatibilityChecks =
          exactClipBoundaryCompatibilityChecks left
            + exactClipBoundaryCompatibilityChecks right
      , exactClipExactIntersections =
          exactClipExactIntersections left + exactClipExactIntersections right
      , exactClipInputCoordinateBits =
          max (exactClipInputCoordinateBits left) (exactClipInputCoordinateBits right)
      , exactClipMaximumAffineCoefficientBits =
          max
            (exactClipMaximumAffineCoefficientBits left)
            (exactClipMaximumAffineCoefficientBits right)
      , exactClipPeakIntermediateCoordinateBits =
          max
            (exactClipPeakIntermediateCoordinateBits left)
            (exactClipPeakIntermediateCoordinateBits right)
      , exactClipFinalCoordinateBits =
          max (exactClipFinalCoordinateBits left) (exactClipFinalCoordinateBits right)
      , exactClipFinalDenominatorBits =
          max (exactClipFinalDenominatorBits left) (exactClipFinalDenominatorBits right)
      }

instance Monoid ExactClipReceipt where
  mempty = ExactClipReceipt 0 0 0 0 0 0 0 0 0

-- | A strict exact segment whose endpoints are distinct.
data ExactSegment = ExactSegment !ExactPoint !ExactPoint
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | An exact half-line with an admitted nonzero direction.
data ExactRay = ExactRay !ExactPoint !ExactVector
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Witness-bearing refusals from exact segment construction.
data ExactGeometryError
  = ExactSegmentEndpointsCoincide !ExactPoint
  | ExactRayZeroDirection !ExactPoint
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Admit a half-line, rejecting the only direction that cannot carry one.
exactRay
  :: ExactPoint
  -> ExactVector
  -> Either ExactGeometryError ExactRay
exactRay originPoint direction@(ExactVector directionX directionY)
  | exactRationalIsZero directionX && exactRationalIsZero directionY =
      Left (ExactRayZeroDirection originPoint)
  | otherwise = Right (ExactRay originPoint direction)

-- | Finite endpoint of an exact ray.
exactRayOrigin :: ExactRay -> ExactPoint
exactRayOrigin (ExactRay originPoint _) = originPoint
{-# INLINE exactRayOrigin #-}

-- | Nonzero direction of an exact ray.
exactRayDirection :: ExactRay -> ExactVector
exactRayDirection (ExactRay _ direction) = direction
{-# INLINE exactRayDirection #-}

-- | Witness-bearing refusals from exact line intersection.
data ExactIntersectionError
  = ExactIntersectionAbsent !SegmentRelation
  | ExactIntersectionNonUnique !SegmentRelation
  | ExactIntersectionParallelOrDegenerate !ExactRational
  | ExactIntersectionArithmetic !ExactArithmeticError
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Construct an exact point from two exact coordinates.
exactPoint :: ExactRational -> ExactRational -> ExactPoint
exactPoint = ExactPoint
{-# INLINE exactPoint #-}

-- | Read both exact point coordinates.
exactPointCoordinates :: ExactPoint -> (ExactRational, ExactRational)
exactPointCoordinates (ExactPoint x y) = (x, y)
{-# INLINE exactPointCoordinates #-}

-- | Determinant of two points regarded as vectors from the Cartesian origin.
exactPointCross :: ExactPoint -> ExactPoint -> ExactRational
exactPointCross (ExactPoint ax ay) (ExactPoint bx by) = ax * by - ay * bx
{-# INLINE exactPointCross #-}

-- | Admit an affine line, refusing precisely the zero normal. Coefficients
-- are retained verbatim because their source identity, rather than a chosen
-- scalar normalization, is the point of this carrier.
exactAffineLine
  :: ExactRational
  -> ExactRational
  -> ExactRational
  -> Either ExactHalfPlaneError ExactAffineLine
exactAffineLine coefficientX coefficientY constant
  | exactRationalIsZero coefficientX
      && exactRationalIsZero coefficientY =
      Left
        ( ExactAffineLineZeroNormal
            coefficientX
            coefficientY
            constant
        )
  | otherwise =
      Right (ExactAffineLine coefficientX coefficientY constant)

-- | Read the retained coefficients @(a,b,c)@ of @a*x+b*y+c=0@.
exactAffineLineCoefficients
  :: ExactAffineLine
  -> (ExactRational, ExactRational, ExactRational)
exactAffineLineCoefficients (ExactAffineLine coefficientX coefficientY constant) =
  (coefficientX, coefficientY, constant)
{-# INLINE exactAffineLineCoefficients #-}

-- | Reverse the oriented normal without changing the geometric line.
-- Admission already proves the normal nonzero, so negation is total.
oppositeExactAffineLine :: ExactAffineLine -> ExactAffineLine
oppositeExactAffineLine (ExactAffineLine coefficientX coefficientY constant) =
  ExactAffineLine (negate coefficientX) (negate coefficientY) (negate constant)
{-# INLINE oppositeExactAffineLine #-}

-- | Intersect two admitted affine lines directly from their retained source
-- coefficients. A parallel pair retains the exact determinant witness.
exactAffineLineIntersection
  :: ExactAffineLine
  -> ExactAffineLine
  -> Either ExactIntersectionError ExactPoint
exactAffineLineIntersection
  (ExactAffineLine firstX firstY firstConstant)
  (ExactAffineLine secondX secondY secondConstant) =
    -- Descend in homogeneous integer coordinates and normalize each published
    -- coordinate once. Generic Ratio arithmetic would normalize every product
    -- in Cramer's rule even though those intermediate rationals are invisible.
    let (firstXNumerator, firstXDenominator) = exactRationalParts firstX
        (firstYNumerator, firstYDenominator) = exactRationalParts firstY
        (firstConstantNumerator, firstConstantDenominator) =
          exactRationalParts firstConstant
        (secondXNumerator, secondXDenominator) = exactRationalParts secondX
        (secondYNumerator, secondYDenominator) = exactRationalParts secondY
        (secondConstantNumerator, secondConstantDenominator) =
          exactRationalParts secondConstant
        determinantNumerator =
          firstXNumerator
            * secondYNumerator
            * secondXDenominator
            * firstYDenominator
            - secondXNumerator
              * firstYNumerator
              * firstXDenominator
              * secondYDenominator
        xNumerator =
          ( firstYNumerator
              * secondConstantNumerator
              * secondYDenominator
              * firstConstantDenominator
              - secondYNumerator
                * firstConstantNumerator
                * firstYDenominator
                * secondConstantDenominator
          )
            * firstXDenominator
            * secondXDenominator
        yNumerator =
          ( firstConstantNumerator
              * secondXNumerator
              * secondConstantDenominator
              * firstXDenominator
              - secondConstantNumerator
                * firstXNumerator
                * firstConstantDenominator
                * secondXDenominator
          )
            * firstYDenominator
            * secondYDenominator
        coordinateDenominator =
          determinantNumerator
            * firstConstantDenominator
            * secondConstantDenominator
     in if determinantNumerator == 0
          then Left (ExactIntersectionParallelOrDegenerate 0)
          else do
            x <- admitCoordinate xNumerator coordinateDenominator
            y <- admitCoordinate yNumerator coordinateDenominator
            pure (ExactPoint x y)
 where
  admitCoordinate numerator denominator =
    case exactRational numerator denominator of
      Left arithmeticError ->
        Left (ExactIntersectionArithmetic arithmeticError)
      Right coordinate -> Right coordinate

exactRationalParts :: ExactRational -> (Integer, Integer)
exactRationalParts value =
  (exactRationalNumerator value, exactRationalDenominator value)
{-# INLINE exactRationalParts #-}

-- | Orient an admitted affine line so its nonnegative side is retained.
exactClosedHalfPlane :: ExactAffineLine -> ExactClosedHalfPlane
exactClosedHalfPlane = ExactClosedHalfPlane
{-# INLINE exactClosedHalfPlane #-}

-- | Construct the closed half-plane to the left of a directed edge.
exactClosedHalfPlaneFromDirectedEdge
  :: ExactPoint
  -> ExactPoint
  -> Either ExactHalfPlaneError ExactClosedHalfPlane
exactClosedHalfPlaneFromDirectedEdge
  (ExactPoint fromX fromY)
  (ExactPoint toX toY) =
  exactClosedHalfPlane
    <$> exactAffineLine
      (fromY - toY)
      (toX - fromX)
      (toY * fromX - toX * fromY)

-- | Read the original affine boundary retained by a closed half-plane.
exactClosedHalfPlaneLine :: ExactClosedHalfPlane -> ExactAffineLine
exactClosedHalfPlaneLine (ExactClosedHalfPlane line) = line
{-# INLINE exactClosedHalfPlaneLine #-}

-- | Classify a point against a closed half-plane. 'GT' is strict interior,
-- 'EQ' lies on the boundary, and 'LT' is exterior.
classifyExactPoint :: ExactClosedHalfPlane -> ExactPoint -> Ordering
classifyExactPoint
  (ExactClosedHalfPlane (ExactAffineLine coefficientX coefficientY constant))
  (ExactPoint x y) =
  -- The common denominator is strictly positive, so its unnormalized integer
  -- numerator is already the authoritative sign witness.
  let (coefficientXNumerator, coefficientXDenominator) =
        exactRationalParts coefficientX
      (coefficientYNumerator, coefficientYDenominator) =
        exactRationalParts coefficientY
      (constantNumerator, constantDenominator) = exactRationalParts constant
      (xNumerator, xDenominator) = exactRationalParts x
      (yNumerator, yDenominator) = exactRationalParts y
      evaluationNumerator =
        coefficientXNumerator
          * xNumerator
          * coefficientYDenominator
          * yDenominator
          * constantDenominator
          + coefficientYNumerator
            * yNumerator
            * coefficientXDenominator
            * xDenominator
            * constantDenominator
          + constantNumerator
            * coefficientXDenominator
            * xDenominator
            * coefficientYDenominator
            * yDenominator
   in compare evaluationNumerator 0
{-# INLINE classifyExactPoint #-}

-- | Admit strict counter-clockwise convex points and retain the original
-- supporting line of every incoming edge.
exactRetainedPolygon
  :: NonEmpty ExactPoint
  -> Either ExactHalfPlaneError ExactRetainedPolygon
exactRetainedPolygon points
  | NonEmpty.length points < 3 =
      Left (ExactRetainedPolygonTooFewVertices (NonEmpty.length points))
  | otherwise =
      case firstNonCounterClockwiseTurn exactOrient2d points of
        Just (index, turn) ->
          Left (ExactRetainedPolygonNonConvexTurn index turn)
        Nothing -> do
          validateSimpleRetainedCycle points
          ExactRetainedPolygon
            <$> traverse retainIncomingLine (cyclePairsNonEmpty points)
 where
  retainIncomingLine (from, to) = do
    halfPlane <- exactClosedHalfPlaneFromDirectedEdge from to
    pure (ExactRetainedVertex to (exactClosedHalfPlaneLine halfPlane))

validateSimpleRetainedCycle
  :: NonEmpty ExactPoint
  -> Either ExactHalfPlaneError ()
validateSimpleRetainedCycle points =
  case
    [ (leftIndex, rightIndex, relation)
    | ( (leftIndex, (leftFrom, leftTo))
        , (rightIndex, (rightFrom, rightTo))
        ) <- unorderedPairs indexedEdges
    , let relation =
            exactSegmentRelation
              leftFrom
              leftTo
              rightFrom
              rightTo
    , not
        ( admitsSimpleCycleEdgeRelation
            segmentCount
            leftIndex
            rightIndex
            relation
        )
    ] of
    (leftIndex, rightIndex, relation) : _ ->
      Left
        ( ExactRetainedPolygonSelfRelation
            leftIndex
            rightIndex
            relation
        )
    [] -> Right ()
 where
  indexedEdges = zip [0 :: Int ..] (NonEmpty.toList (cyclePairsNonEmpty points))
  segmentCount = NonEmpty.length points

-- | Project the retained-edge carrier to its authoritative point cycle.
exactRetainedPolygonPoints :: ExactRetainedPolygon -> NonEmpty ExactPoint
exactRetainedPolygonPoints (ExactRetainedPolygon vertices) =
  fmap retainedVertexPoint vertices

-- | Intersect a retained convex polygon with exact closed half-planes.
-- Boundaries descend in exact angular order through one immutable deque;
-- every published edge retains one submitted source line, and derived
-- endpoints never become line coefficients.
exactClipRetainedPolygon
  :: ExactRetainedPolygon
  -> [ExactClosedHalfPlane]
  -> Either ExactClipError (ExactClipDisposition, ExactClipReceipt)
exactClipRetainedPolygon polygon halfPlanes = do
  let inputPoints = exactRetainedPolygonPoints polygon
      inputBits = maximumPointBitWidth inputPoints
      domainHalfPlanes = retainedPolygonHalfPlanes polygon
      allHalfPlanes = domainHalfPlanes <> halfPlanes
      initialReceipt =
        ExactClipReceipt
          { exactClipSubmittedHalfPlanes = length halfPlanes
          , exactClipActiveBoundaries =
              if null halfPlanes then NonEmpty.length inputPoints else 0
          , exactClipBoundaryCompatibilityChecks = 0
          , exactClipExactIntersections = 0
          , exactClipInputCoordinateBits = inputBits
          , exactClipMaximumAffineCoefficientBits =
              List.foldl'
                (\bits -> max bits . affineLineBitWidth . exactClosedHalfPlaneLine)
                0
                allHalfPlanes
          , exactClipPeakIntermediateCoordinateBits = inputBits
          , exactClipFinalCoordinateBits = inputBits
          , exactClipFinalDenominatorBits =
              maximumPointDenominatorBitWidth inputPoints
          }
  if null halfPlanes
    then pure (ExactClipFullDimensional polygon, initialReceipt)
    else case List.find (`excludesExactDomain` inputPoints) halfPlanes of
      Just _ ->
        pure
          ( ExactClipEmpty
          , initialReceipt
              { exactClipFinalCoordinateBits = 0
              , exactClipFinalDenominatorBits = 0
              }
          )
      Nothing -> do
        let orderedBoundaries =
              coalesceAngularBoundaries (fmap angularBoundary allHalfPlanes)
            descentReceipt =
              initialReceipt
                { exactClipActiveBoundaries = length orderedBoundaries
                }
        (openDeque, descendedReceipt) <-
          foldM insertAngularBoundary (Sequence.empty, descentReceipt) orderedBoundaries
        (closedDeque, accumulatedReceipt) <- closeAngularBoundaryDeque openDeque descendedReceipt
        (finalState, finalizedReceipt) <-
          angularBoundaryDequeState orderedBoundaries closedDeque accumulatedReceipt
        let disposition = exactClipStateDisposition finalState
            finalPoints = exactClipDispositionPoints disposition
            finalReceipt =
              finalizedReceipt
                { exactClipFinalCoordinateBits = maybe 0 maximumPointBitWidth finalPoints
                , exactClipFinalDenominatorBits =
                    maybe 0 maximumPointDenominatorBitWidth finalPoints
                }
        pure (disposition, finalReceipt)

-- | A linear functional attains its maximum over a convex polygon at a
-- vertex.  One half-plane whose closed side contains no domain vertex is
-- therefore an exact emptiness certificate; recognizing it before angular
-- descent avoids sorting a section that has no global point to glue.
excludesExactDomain
  :: ExactClosedHalfPlane
  -> NonEmpty ExactPoint
  -> Bool
excludesExactDomain halfPlane =
  all ((== LT) . classifyExactPoint halfPlane)

-- | One original source boundary together with its exact counter-clockwise
-- direction.  Equal-direction gluing retains one of these values verbatim.
data ExactAngularBoundary = ExactAngularBoundary
  { angularBoundaryLine :: !ExactAffineLine
  , angularBoundaryDirection :: !ExactVector
  }

angularBoundary :: ExactClosedHalfPlane -> ExactAngularBoundary
angularBoundary halfPlane =
  let line@(ExactAffineLine coefficientX coefficientY _) =
        exactClosedHalfPlaneLine halfPlane
   in ExactAngularBoundary
        { angularBoundaryLine = line
        , angularBoundaryDirection = ExactVector coefficientY (negate coefficientX)
        }

angularBoundaryHalfPlane :: ExactAngularBoundary -> ExactClosedHalfPlane
angularBoundaryHalfPlane = exactClosedHalfPlane . angularBoundaryLine
{-# INLINE angularBoundaryHalfPlane #-}

retainedPolygonHalfPlanes :: ExactRetainedPolygon -> [ExactClosedHalfPlane]
retainedPolygonHalfPlanes (ExactRetainedPolygon vertices) =
  fmap
    (exactClosedHalfPlane . retainedVertexIncomingLine)
    (NonEmpty.toList vertices)

-- | Descent over one angular stalk.  A stricter parallel boundary replaces a
-- weaker one; coincident boundaries retain the least original source line so
-- input permutations cannot alter the glued carrier.
coalesceAngularBoundaries :: [ExactAngularBoundary] -> [ExactAngularBoundary]
coalesceAngularBoundaries =
  fmap strongestAngularBoundary
    . mapMaybe NonEmpty.nonEmpty
    . List.groupBy sameBoundaryDirection
    . List.sortBy compareAngularBoundaries
 where
  sameBoundaryDirection left right =
    compareExactVectorAngle
      (angularBoundaryDirection left)
      (angularBoundaryDirection right)
      == EQ

compareAngularBoundaries :: ExactAngularBoundary -> ExactAngularBoundary -> Ordering
compareAngularBoundaries left right =
  case
    compareExactVectorAngle
      (angularBoundaryDirection left)
      (angularBoundaryDirection right) of
    EQ -> compare (angularBoundaryLine left) (angularBoundaryLine right)
    ordering -> ordering

strongestAngularBoundary :: NonEmpty ExactAngularBoundary -> ExactAngularBoundary
strongestAngularBoundary (initial :| remaining) =
  List.foldl' chooseStrongerAngularBoundary initial remaining

chooseStrongerAngularBoundary
  :: ExactAngularBoundary
  -> ExactAngularBoundary
  -> ExactAngularBoundary
chooseStrongerAngularBoundary selected candidate =
  case compareParallelBoundaryStrength selected candidate of
    LT -> candidate
    GT -> selected
    EQ ->
      if angularBoundaryLine candidate < angularBoundaryLine selected
        then candidate
        else selected

-- | Compare same-direction boundaries.  'LT' means the right boundary is
-- stricter, 'GT' means the left is stricter, and 'EQ' means the closed
-- half-planes coincide.  Cross multiplication avoids inventing a normalized
-- replacement line.
compareParallelBoundaryStrength
  :: ExactAngularBoundary
  -> ExactAngularBoundary
  -> Ordering
compareParallelBoundaryStrength left right =
  let ExactAffineLine leftX leftY leftConstant = angularBoundaryLine left
      ExactAffineLine rightX rightY rightConstant = angularBoundaryLine right
      compareScaled leftScale rightScale scaleSign =
        let raw = compare (rightConstant * leftScale) (rightScale * leftConstant)
         in if exactSignum scaleSign == LT then reverseOrdering raw else raw
   in if exactRationalIsZero leftX
        then compareScaled leftY rightY leftY
        else compareScaled leftX rightX leftX

reverseOrdering :: Ordering -> Ordering
reverseOrdering LT = GT
reverseOrdering EQ = EQ
reverseOrdering GT = LT

-- | One active source boundary and its cached intersection with the preceding
-- deque boundary.  Every adjacency point is derived once and reused by both
-- compatibility trims and final publication.
data ExactAngularBoundaryNode = ExactAngularBoundaryNode
  { angularNodeBoundary :: !ExactAngularBoundary
  , angularNodePreviousIntersection :: !(Maybe ExactPoint)
  }

type ExactAngularBoundaryDeque = Seq ExactAngularBoundaryNode

insertAngularBoundary
  :: (ExactAngularBoundaryDeque, ExactClipReceipt)
  -> ExactAngularBoundary
  -> Either ExactClipError (ExactAngularBoundaryDeque, ExactClipReceipt)
insertAngularBoundary (boundaries, receipt) incoming = do
  (backTrimmed, backReceipt) <- trimAngularBack incoming boundaries receipt
  (frontTrimmed, frontReceipt) <- trimAngularFront incoming backTrimmed backReceipt
  appendAngularBoundary incoming frontTrimmed frontReceipt

appendAngularBoundary
  :: ExactAngularBoundary
  -> ExactAngularBoundaryDeque
  -> ExactClipReceipt
  -> Either ExactClipError (ExactAngularBoundaryDeque, ExactClipReceipt)
appendAngularBoundary incoming boundaries receipt =
  case Sequence.viewr boundaries of
    EmptyR ->
      Right
        ( Sequence.singleton (ExactAngularBoundaryNode incoming Nothing)
        , receipt
        )
    _ :> finalNode -> do
      (intersection, observedReceipt) <-
        observeAdjacentIntersection
          (angularNodeBoundary finalNode)
          incoming
          receipt
      pure
        ( boundaries
            |> ExactAngularBoundaryNode incoming intersection
        , observedReceipt
        )

trimAngularBack
  :: ExactAngularBoundary
  -> ExactAngularBoundaryDeque
  -> ExactClipReceipt
  -> Either ExactClipError (ExactAngularBoundaryDeque, ExactClipReceipt)
trimAngularBack incoming boundaries receipt =
  case Sequence.viewr boundaries of
    EmptyR -> Right (boundaries, receipt)
    remaining :> finalNode ->
      case Sequence.viewr remaining of
        EmptyR -> Right (boundaries, receipt)
        _ :> _ ->
          case angularNodePreviousIntersection finalNode of
            Nothing -> Right (boundaries, receipt)
            Just point ->
              let checkedReceipt = observeBoundaryCompatibility receipt
               in if classifyExactPoint (angularBoundaryHalfPlane incoming) point == LT
                    then trimAngularBack incoming remaining checkedReceipt
                    else Right (boundaries, checkedReceipt)

trimAngularFront
  :: ExactAngularBoundary
  -> ExactAngularBoundaryDeque
  -> ExactClipReceipt
  -> Either ExactClipError (ExactAngularBoundaryDeque, ExactClipReceipt)
trimAngularFront incoming boundaries receipt =
  case Sequence.viewl boundaries of
    EmptyL -> Right (boundaries, receipt)
    _ :< remaining ->
      case Sequence.viewl remaining of
        EmptyL -> Right (boundaries, receipt)
        secondNode :< suffix ->
          case angularNodePreviousIntersection secondNode of
            Nothing -> Right (boundaries, receipt)
            Just point ->
              let checkedReceipt = observeBoundaryCompatibility receipt
               in if classifyExactPoint (angularBoundaryHalfPlane incoming) point == LT
                    then
                      trimAngularFront
                        incoming
                        ( secondNode
                            { angularNodePreviousIntersection = Nothing
                            }
                            Sequence.<| suffix
                        )
                        checkedReceipt
                    else Right (boundaries, checkedReceipt)

closeAngularBoundaryDeque
  :: ExactAngularBoundaryDeque
  -> ExactClipReceipt
  -> Either ExactClipError (ExactAngularBoundaryDeque, ExactClipReceipt)
closeAngularBoundaryDeque boundaries receipt = do
  (backClosed, backReceipt) <- closeAngularBack boundaries receipt
  (frontClosed, frontReceipt) <- closeAngularFront backClosed backReceipt
  if Sequence.length frontClosed == Sequence.length boundaries
    then Right (frontClosed, frontReceipt)
    else closeAngularBoundaryDeque frontClosed frontReceipt

closeAngularBack
  :: ExactAngularBoundaryDeque
  -> ExactClipReceipt
  -> Either ExactClipError (ExactAngularBoundaryDeque, ExactClipReceipt)
closeAngularBack boundaries receipt =
  case (Sequence.viewl boundaries, Sequence.viewr boundaries) of
    (firstNode :< _, remaining :> finalNode) ->
      case Sequence.viewr remaining of
        _ :> _ ->
          case angularNodePreviousIntersection finalNode of
            Nothing -> Right (boundaries, receipt)
            Just point ->
              let checkedReceipt = observeBoundaryCompatibility receipt
               in if
                    classifyExactPoint
                      (angularBoundaryHalfPlane (angularNodeBoundary firstNode))
                      point
                      == LT
                    then closeAngularBack remaining checkedReceipt
                    else Right (boundaries, checkedReceipt)
        EmptyR -> Right (boundaries, receipt)
    _ -> Right (boundaries, receipt)

closeAngularFront
  :: ExactAngularBoundaryDeque
  -> ExactClipReceipt
  -> Either ExactClipError (ExactAngularBoundaryDeque, ExactClipReceipt)
closeAngularFront boundaries receipt =
  case (Sequence.viewl boundaries, Sequence.viewr boundaries) of
    (_ :< remaining, _ :> finalNode) ->
      case Sequence.viewl remaining of
        secondNode :< suffix ->
          case angularNodePreviousIntersection secondNode of
            Nothing -> Right (boundaries, receipt)
            Just point ->
              let checkedReceipt = observeBoundaryCompatibility receipt
               in if
                    classifyExactPoint
                      (angularBoundaryHalfPlane (angularNodeBoundary finalNode))
                      point
                      == LT
                    then
                      closeAngularFront
                        ( secondNode
                            { angularNodePreviousIntersection = Nothing
                            }
                            Sequence.<| suffix
                        )
                        checkedReceipt
                    else Right (boundaries, checkedReceipt)
        EmptyL -> Right (boundaries, receipt)
    _ -> Right (boundaries, receipt)

observeAdjacentIntersection
  :: ExactAngularBoundary
  -> ExactAngularBoundary
  -> ExactClipReceipt
  -> Either ExactClipError (Maybe ExactPoint, ExactClipReceipt)
observeAdjacentIntersection left right receipt =
  let leftLine = angularBoundaryLine left
      rightLine = angularBoundaryLine right
   in case exactAffineLineIntersection leftLine rightLine of
        Left (ExactIntersectionParallelOrDegenerate _) ->
          Right (Nothing, receipt)
        Left obstruction -> Left (ExactClipIntersection obstruction)
        Right point ->
          pure
            ( Just point
            , receipt
                { exactClipExactIntersections =
                    exactClipExactIntersections receipt + 1
                , exactClipPeakIntermediateCoordinateBits =
                    max
                      (exactClipPeakIntermediateCoordinateBits receipt)
                      (exactPointBitWidth point)
                }
            )

observeBoundaryCompatibility :: ExactClipReceipt -> ExactClipReceipt
observeBoundaryCompatibility receipt =
  receipt
    { exactClipBoundaryCompatibilityChecks =
        exactClipBoundaryCompatibilityChecks receipt + 1
    }

angularBoundaryDequeState
  :: [ExactAngularBoundary]
  -> ExactAngularBoundaryDeque
  -> ExactClipReceipt
  -> Either ExactClipError (ExactClipState, ExactClipReceipt)
angularBoundaryDequeState allBoundaries boundaries receipt = do
  let nodeList = foldr (:) [] boundaries
      linearVertices =
        mapMaybe
          retainedLinearVertex
          (zip nodeList (drop 1 nodeList))
  (closingVertices, finalizedReceipt) <-
    case (Sequence.viewl boundaries, Sequence.viewr boundaries) of
      (firstNode :< _, _ :> finalNode) -> do
        (intersection, observedReceipt) <-
          observeAdjacentIntersection
            (angularNodeBoundary finalNode)
            (angularNodeBoundary firstNode)
            receipt
        pure
          ( maybe
              []
              (\point ->
                 [ ExactRetainedVertex
                     point
                     (angularBoundaryLine (angularNodeBoundary finalNode))
                 ])
              intersection
          , observedReceipt
          )
      _ -> Right ([], receipt)
  state <-
    retainedVerticesToClipState
      (normalizeRetainedVertices (linearVertices <> closingVertices))
  pure (validateLowerDimensionalState allBoundaries state, finalizedReceipt)
 where
  retainedLinearVertex (node, successor) =
    fmap
      (\point ->
         ExactRetainedVertex
           point
           (angularBoundaryLine (angularNodeBoundary node)))
      (angularNodePreviousIntersection successor)

validateLowerDimensionalState
  :: [ExactAngularBoundary]
  -> ExactClipState
  -> ExactClipState
validateLowerDimensionalState boundaries state =
  case state of
    ExactClipStateSegment from to ->
      if all (containsBoth from to) boundaries
        then state
        else ExactClipStateEmpty
    ExactClipStatePoint point ->
      if all
          (\boundary ->
             classifyExactPoint (angularBoundaryHalfPlane boundary) point /= LT)
          boundaries
        then state
        else ExactClipStateEmpty
    other -> other
 where
  containsBoth from to boundary =
    let halfPlane = angularBoundaryHalfPlane boundary
     in classifyExactPoint halfPlane from /= LT
          && classifyExactPoint halfPlane to /= LT

data ExactClipState
  = ExactClipStatePolygon !ExactRetainedPolygon
  | ExactClipStateSegment !ExactPoint !ExactPoint
  | ExactClipStatePoint !ExactPoint
  | ExactClipStateEmpty
exactSegmentClipState
  :: ExactPoint
  -> ExactPoint
  -> ExactClipState
exactSegmentClipState from to
  | from == to = ExactClipStatePoint from
  | otherwise = ExactClipStateSegment from to

retainedVerticesToClipState
  :: [ExactRetainedVertex]
  -> Either ExactClipError ExactClipState
retainedVerticesToClipState [] = Right ExactClipStateEmpty
retainedVerticesToClipState [vertex] =
  Right (ExactClipStatePoint (retainedVertexPoint vertex))
retainedVerticesToClipState [fromVertex, toVertex] =
  Right
    ( exactSegmentClipState
        (retainedVertexPoint fromVertex)
        (retainedVertexPoint toVertex)
    )
retainedVerticesToClipState (firstVertex : secondVertex : remaining) =
  let retained = firstVertex :| (secondVertex : remaining)
      twiceArea = retainedVerticesTwiceArea retained
   in case exactSignum twiceArea of
        GT -> Right (ExactClipStatePolygon (ExactRetainedPolygon retained))
        LT -> Left (ExactClipOrientationReversed twiceArea)
        EQ -> Right (lowerDimensionalClipState retained)

lowerDimensionalClipState
  :: NonEmpty ExactRetainedVertex
  -> ExactClipState
lowerDimensionalClipState vertices =
  let points = fmap retainedVertexPoint vertices
      initialPoint :| remainingPoints = points
      leastPoint = List.foldl' min initialPoint remainingPoints
      greatestPoint = List.foldl' max initialPoint remainingPoints
   in if leastPoint == greatestPoint
        then ExactClipStatePoint leastPoint
        else ExactClipStateSegment leastPoint greatestPoint

retainedVerticesTwiceArea
  :: NonEmpty ExactRetainedVertex
  -> ExactRational
retainedVerticesTwiceArea =
  List.foldl'
    (\area (from, to) ->
       area
         + exactPointCross
           (retainedVertexPoint from)
           (retainedVertexPoint to))
    0
    . cyclePairsNonEmpty

normalizeRetainedVertices
  :: [ExactRetainedVertex]
  -> [ExactRetainedVertex]
normalizeRetainedVertices = stripClosingDuplicate . deduplicateAdjacent
 where
  deduplicateAdjacent [] = []
  deduplicateAdjacent (firstVertex : remaining) =
    firstVertex : deduplicateAfter firstVertex remaining

  deduplicateAfter _ [] = []
  deduplicateAfter previous (candidate : remaining)
    | retainedVertexPoint previous == retainedVertexPoint candidate =
        deduplicateAfter previous remaining
    | otherwise = candidate : deduplicateAfter candidate remaining

  stripClosingDuplicate vertices@(firstVertex : remainingVertices) =
    case reverse remainingVertices of
      finalVertex : reversedInterior
        | retainedVertexPoint finalVertex == retainedVertexPoint firstVertex ->
            finalVertex : reverse reversedInterior
      _ -> vertices
  stripClosingDuplicate [] = []

exactClipStateDisposition :: ExactClipState -> ExactClipDisposition
exactClipStateDisposition state =
  case state of
    ExactClipStatePolygon polygon -> ExactClipFullDimensional polygon
    ExactClipStateSegment from to ->
      ExactClipLowerDimensional (from :| [to])
    ExactClipStatePoint point -> ExactClipLowerDimensional (point :| [])
    ExactClipStateEmpty -> ExactClipEmpty

exactClipDispositionPoints
  :: ExactClipDisposition
  -> Maybe (NonEmpty ExactPoint)
exactClipDispositionPoints disposition =
  case disposition of
    ExactClipFullDimensional polygon ->
      Just (exactRetainedPolygonPoints polygon)
    ExactClipLowerDimensional points -> Just points
    ExactClipEmpty -> Nothing

retainedVertexPoint :: ExactRetainedVertex -> ExactPoint
retainedVertexPoint (ExactRetainedVertex point _) = point
{-# INLINE retainedVertexPoint #-}

retainedVertexIncomingLine :: ExactRetainedVertex -> ExactAffineLine
retainedVertexIncomingLine (ExactRetainedVertex _ line) = line
{-# INLINE retainedVertexIncomingLine #-}

affineLineBitWidth :: ExactAffineLine -> Int
affineLineBitWidth (ExactAffineLine coefficientX coefficientY constant) =
  max
    (exactRationalBitWidth coefficientX)
    (max (exactRationalBitWidth coefficientY) (exactRationalBitWidth constant))

maximumPointBitWidth :: NonEmpty ExactPoint -> Int
maximumPointBitWidth =
  List.foldl'
    (\maximumBits (ExactPoint x y) ->
       max maximumBits (max (exactRationalBitWidth x) (exactRationalBitWidth y)))
    0

-- | Maximum reduced numerator-or-denominator width of either coordinate.
exactPointBitWidth :: ExactPoint -> Int
exactPointBitWidth (ExactPoint x y) =
  max (exactRationalBitWidth x) (exactRationalBitWidth y)
{-# INLINE exactPointBitWidth #-}

maximumPointDenominatorBitWidth :: NonEmpty ExactPoint -> Int
maximumPointDenominatorBitWidth =
  List.foldl'
    (\maximumBits (ExactPoint x y) ->
       max
         maximumBits
         ( max
             (exactRationalDenominatorBitWidth x)
             (exactRationalDenominatorBitWidth y)
         ))
    0

-- | Construct an exact segment, refusing coincident endpoints with their
-- shared point as the witness.
exactSegment
  :: ExactPoint
  -> ExactPoint
  -> Either ExactGeometryError ExactSegment
exactSegment from to
  | from == to = Left (ExactSegmentEndpointsCoincide from)
  | otherwise = Right (ExactSegment from to)

-- | Read both distinct exact segment endpoints.
exactSegmentEndpoints :: ExactSegment -> (ExactPoint, ExactPoint)
exactSegmentEndpoints (ExactSegment from to) = (from, to)
{-# INLINE exactSegmentEndpoints #-}

-- | Validate and exactly embed a raw binary64 point.
exactPointFromPoint :: Point -> Either PointValidationError ExactPoint
exactPointFromPoint = fmap exactPointFromQueryPoint . mkQueryPoint

-- | Exactly embed an already-admitted query point without repeating
-- coordinate validation.
exactPointFromQueryPoint :: QueryPoint -> ExactPoint
exactPointFromQueryPoint queryPoint =
  case queryPointValue queryPoint of
    Point x y ->
      ExactPoint
        (exactRationalFromFiniteDouble x)
        (exactRationalFromFiniteDouble y)
{-# INLINE exactPointFromQueryPoint #-}

-- | Deterministically project an exact point to a validated binary64
-- embedding candidate. This is not a correctly-rounded nearest-double claim;
-- callers must certify the candidate projection before relying on it.
exactPointToEmbeddingCandidate
  :: ExactPoint
  -> Either PointValidationError QueryPoint
exactPointToEmbeddingCandidate (ExactPoint x y) =
  mkQueryPoint
    ( Point
        (integerRatioToDouble (exactRationalNumerator x) (exactRationalDenominator x))
        (integerRatioToDouble (exactRationalNumerator y) (exactRationalDenominator y))
    )

-- | Exact orientation of an ordered triple. 'GT' is a positive determinant
-- and counter-clockwise turn, 'EQ' is collinear, and 'LT' is clockwise.
exactOrient2d :: ExactPoint -> ExactPoint -> ExactPoint -> Ordering
exactOrient2d
  (ExactPoint ax ay)
  (ExactPoint bx by)
  (ExactPoint cx cy) =
    exactSignum
      ((bx - ax) * (cy - ay) - (by - ay) * (cx - ax))
{-# INLINE exactOrient2d #-}

-- | Whether an exact point lies on an exact closed segment.
exactOnClosedSegment :: ExactPoint -> ExactPoint -> ExactPoint -> Bool
exactOnClosedSegment
  from@(ExactPoint ax ay)
  to@(ExactPoint bx by)
  query@(ExactPoint qx qy) =
    exactOrient2d from to query == EQ
      && qx >= min ax bx
      && qx <= max ax bx
      && qy >= min ay by
      && qy <= max ay by
{-# INLINE exactOnClosedSegment #-}

-- | Exact rational specialization of the one closed-segment relation policy.
exactSegmentRelation
  :: ExactPoint
  -> ExactPoint
  -> ExactPoint
  -> ExactPoint
  -> SegmentRelation
exactSegmentRelation =
  segmentRelationWith (==) compare exactOrient2d exactOnClosedSegment
{-# INLINE exactSegmentRelation #-}

-- | Return the unique exact intersection of two exact segments. Disjoint and
-- non-unique relations are refused with their relation witness; a zero line
-- cross product and arithmetic failure retain their exact witnesses.
exactLineIntersection
  :: ExactSegment
  -> ExactSegment
  -> Either ExactIntersectionError ExactPoint
exactLineIntersection
  (ExactSegment a b)
  (ExactSegment c d) =
    case exactSegmentRelation a b c d of
      SegmentsDisjoint -> Left (ExactIntersectionAbsent SegmentsDisjoint)
      SegmentsDuplicate -> Left (ExactIntersectionNonUnique SegmentsDuplicate)
      SegmentsCollinearlyOverlap ->
        Left (ExactIntersectionNonUnique SegmentsCollinearlyOverlap)
      SegmentsShareEndpoint -> uniqueIntersection
      SegmentsProperlyCross -> uniqueIntersection
      SegmentEndpointTouchesInterior -> uniqueIntersection
 where
  uniqueIntersection =
    exactSupportingLineIntersection (ExactSegment a b) (ExactSegment c d)

-- | Intersect the infinite supporting lines of two admitted exact segments.
-- Unlike 'exactLineIntersection', the intersection need not lie inside either
-- closed segment. Parallel supporting lines retain the exact zero denominator
-- witness.
exactSupportingLineIntersection
  :: ExactSegment
  -> ExactSegment
  -> Either ExactIntersectionError ExactPoint
exactSupportingLineIntersection
  (ExactSegment a b)
  (ExactSegment c d) =
  let directionAB = exactVectorFromPoints a b
      directionCD = exactVectorFromPoints c d
      fromAToC = exactVectorFromPoints a c
      denominator = exactCross directionAB directionCD
      numerator = exactCross fromAToC directionCD
   in case exactDivide numerator denominator of
        Left ExactZeroDivisor ->
          Left (ExactIntersectionParallelOrDegenerate denominator)
        Left arithmeticError -> Left (ExactIntersectionArithmetic arithmeticError)
        Right parameter ->
          Right (translateExactPoint a (scaleExactVector parameter directionAB))

-- | A strict exact displacement vector. Points and vectors remain distinct;
-- all exact planar algorithms share this single vector carrier.
data ExactVector = ExactVector !ExactRational !ExactRational
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

exactVectorFromPoints :: ExactPoint -> ExactPoint -> ExactVector
exactVectorFromPoints (ExactPoint ax ay) (ExactPoint bx by) =
  ExactVector (bx - ax) (by - ay)

addExactVectors :: ExactVector -> ExactVector -> ExactVector
addExactVectors (ExactVector ax ay) (ExactVector bx by) =
  ExactVector (ax + bx) (ay + by)

exactCross :: ExactVector -> ExactVector -> ExactRational
exactCross (ExactVector ax ay) (ExactVector bx by) =
  ax * by - ay * bx

-- | Counter-clockwise angular order from the positive x-axis. Collinear
-- vectors on the same ray compare equal so convolution can merge them;
-- callers that need a total point order may add their own radial tie-break.
compareExactVectorAngle :: ExactVector -> ExactVector -> Ordering
compareExactVectorAngle left right =
  case compare (vectorHalf left) (vectorHalf right) of
    EQ ->
      case exactCrossSign left right of
        GT -> LT
        LT -> GT
        EQ -> EQ
    ordering -> ordering
 where
  vectorHalf (ExactVector x y)
    | exactSignum y == GT = False
    | exactSignum y == EQ && exactSignum x /= LT = False
    | otherwise = True
{-# INLINE compareExactVectorAngle #-}

exactCrossSign :: ExactVector -> ExactVector -> Ordering
exactCrossSign (ExactVector leftX leftY) (ExactVector rightX rightY) =
  -- Reduced rational denominators are positive; compare the two products in a
  -- common integer scale without constructing and normalizing their ratios.
  let (leftXNumerator, leftXDenominator) = exactRationalParts leftX
      (leftYNumerator, leftYDenominator) = exactRationalParts leftY
      (rightXNumerator, rightXDenominator) = exactRationalParts rightX
      (rightYNumerator, rightYDenominator) = exactRationalParts rightY
   in compare
        ( leftXNumerator
            * rightYNumerator
            * leftYDenominator
            * rightXDenominator
        )
        ( leftYNumerator
            * rightXNumerator
            * leftXDenominator
            * rightYDenominator
        )
{-# INLINE exactCrossSign #-}

scaleExactVector :: ExactRational -> ExactVector -> ExactVector
scaleExactVector scale (ExactVector x y) =
  ExactVector (scale * x) (scale * y)

translateExactPoint :: ExactPoint -> ExactVector -> ExactPoint
translateExactPoint (ExactPoint x y) (ExactVector dx dy) =
  ExactPoint (x + dx) (y + dy)
