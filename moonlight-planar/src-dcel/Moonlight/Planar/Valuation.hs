{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}

-- | Intrinsic valuations of exact closed cell selections and admitted planar
-- regions. Euler characteristic and area remain exact; Euclidean length is an
-- exact radical expression accompanied by outward-rounded binary64 bounds.
module Moonlight.Planar.Valuation
  ( EulerCharacteristic
  , eulerCharacteristicValue
  , ExactArea
  , exactAreaValue
  , ExactPlanarMoments
  , exactPlanarMeasure
  , exactPlanarFirstX
  , exactPlanarFirstY
  , exactPlanarSecondXX
  , exactPlanarSecondXY
  , exactPlanarSecondYY
  , scaleExactPlanarMoments
  , orientedBoundaryMoments
  , orientedBoundaryArea
  , polygonComponentMoments
  , polygonComponentArea
  , ExactLengthTerm
  , lengthCoefficient
  , lengthRadicand
  , ExactLengthExpression
  , exactLengthTerms
  , CertifiedInterval (..)
  , ExactLengthMeasurement
  , exactLengthExpression
  , exactLengthBounds
  , PlanarValuations
  , valuationEuler
  , valuationArea
  , valuationIntrinsic1
  , ValuationError (..)
  , cellValuations
  , regionValuations
  , planarValuationsPerimeter
  , cellSetPerimeter
  , regionPerimeter
  ) where

import Control.DeepSeq (NFData)
import Data.Bifunctor (first)
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import qualified Data.Ratio as Ratio
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import GHC.Generics (Generic)
import Moonlight.Planar.Exact
  ( ExactBounds
  , ExactGeometryError
  , ExactPoint
  , ExactSegment
  , exactOnClosedSegment
  , exactPointCross
  , exactPointCoordinates
  , exactSegment
  , exactSegmentEndpoints
  )
import Moonlight.Planar.Internal.HandleDefs
  ( FaceId (..)
  , UndirectedEdgeId (..)
  , VertexId (..)
  , directedPair
  , faceIdIndex
  , vertexIdIndex
  )
import Moonlight.Planar.Internal.CellSet
  ( ExactCellSet (..)
  , exactCellSetIsFaceClosure
  )
import Moonlight.Planar.Internal.Incidence
  ( PlanarIncidence
  , faceBoundaryComponents
  , faceEulerContribution
  , incidenceIncidentFace
  , incidenceOrigin
  , incidenceUndirectedEndpoints
  )
import Moonlight.Planar.Internal.BoundaryCycle
  ( consecutivePairs
  , cyclePairs
  , orderedPair
  )
import Moonlight.Planar.Internal.ExactRational
  ( ExactRational
  , PositiveExact
  , exactRationalFromDyadic
  , exactRationalFromNormalizedRatio
  , positiveOne
  , positiveTwo
  , ratioPositive
  )
import Moonlight.Planar.Internal.Length
  ( CertifiedInterval (..)
  , ExactLengthExpression
  , ExactLengthMeasurement
  , ExactLengthTerm
  , LengthError (..)
  , exactLengthBounds
  , exactLengthExpression
  , exactLengthTerms
  , lengthCoefficient
  , lengthRadicand
  , measureLengthExpression
  , normalizeLengthContributions
  , publicationPrecision
  , scaleLengthExpression
  )
import Moonlight.Planar.Internal.ExactSegmentEvents
  ( ExactSegmentEvent (..)
  , ExactSegmentEventObstruction
  , ExactSegmentEventPlan
  , ExactSweepSegmentId (..)
  , exactSegmentEventPlan
  , exactSegmentEvents
  , exactSegmentSplitPoints
  )
import Moonlight.Planar.Internal.Region.Types
  ( ExactLoop (..)
  , PlanarRegion (..)
  , PolygonComponent (..)
  , polygonOuterLoop
  , polygonHoleLoops
  )
import Moonlight.Planar.Internal.Region.Bounds
  ( componentBounds
  , overlappingPairs
  )

newtype EulerCharacteristic = EulerCharacteristic Int
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

eulerCharacteristicValue :: EulerCharacteristic -> Int
eulerCharacteristicValue (EulerCharacteristic value) = value

newtype ExactArea = ExactArea ExactRational
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

instance Semigroup ExactArea where
  ExactArea left <> ExactArea right = ExactArea (left + right)

instance Monoid ExactArea where
  mempty = ExactArea 0

exactAreaValue :: ExactArea -> ExactRational
exactAreaValue (ExactArea value) = value

-- | Exact moments of a bounded planar measure through total degree two.
data ExactPlanarMoments = ExactPlanarMoments
  { exactPlanarMeasure :: !ExactRational
    -- ^ Total signed measure.
  , exactPlanarFirstX :: !ExactRational
    -- ^ Raw first moment integral of @x@.
  , exactPlanarFirstY :: !ExactRational
    -- ^ Raw first moment integral of @y@.
  , exactPlanarSecondXX :: !ExactRational
    -- ^ Raw second moment integral of @x^2@.
  , exactPlanarSecondXY :: !ExactRational
    -- ^ Raw mixed moment integral of @x*y@.
  , exactPlanarSecondYY :: !ExactRational
    -- ^ Raw second moment integral of @y^2@.
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

instance Semigroup ExactPlanarMoments where
  left <> right =
    ExactPlanarMoments
      { exactPlanarMeasure = exactPlanarMeasure left + exactPlanarMeasure right
      , exactPlanarFirstX = exactPlanarFirstX left + exactPlanarFirstX right
      , exactPlanarFirstY = exactPlanarFirstY left + exactPlanarFirstY right
      , exactPlanarSecondXX = exactPlanarSecondXX left + exactPlanarSecondXX right
      , exactPlanarSecondXY = exactPlanarSecondXY left + exactPlanarSecondXY right
      , exactPlanarSecondYY = exactPlanarSecondYY left + exactPlanarSecondYY right
      }

instance Monoid ExactPlanarMoments where
  mempty = ExactPlanarMoments 0 0 0 0 0 0

-- | Scale every moment by one exact coefficient.
scaleExactPlanarMoments
  :: ExactRational
  -> ExactPlanarMoments
  -> ExactPlanarMoments
scaleExactPlanarMoments scalar moments =
  ExactPlanarMoments
    { exactPlanarMeasure = scalar * exactPlanarMeasure moments
    , exactPlanarFirstX = scalar * exactPlanarFirstX moments
    , exactPlanarFirstY = scalar * exactPlanarFirstY moments
    , exactPlanarSecondXX = scalar * exactPlanarSecondXX moments
    , exactPlanarSecondXY = scalar * exactPlanarSecondXY moments
    , exactPlanarSecondYY = scalar * exactPlanarSecondYY moments
    }

-- | Exact area and raw moments of one admitted component. The six unscaled
-- boundary sums are accumulated together and normalized once.
polygonComponentMoments :: PolygonComponent -> ExactPlanarMoments
polygonComponentMoments component =
  normalizeMomentSums
    ( foldMap
        loopMomentSums
        (polygonOuterLoop component : polygonHoleLoops component)
    )

-- | Integrate an oriented exact boundary directly, with the represented cell
-- on the left of each edge. Outer cycles contribute positively and holes
-- negatively; no polygon publication or repeated geometric admission occurs.
-- The same raw boundary algebra serves admitted polygon components.
orientedBoundaryMoments
  :: Foldable boundary
  => boundary (ExactPoint, ExactPoint)
  -> ExactPlanarMoments
orientedBoundaryMoments = normalizeMomentSums . Foldable.foldl' accumulateMomentEdge mempty

normalizeMomentSums :: RawPlanarMomentSums -> ExactPlanarMoments
normalizeMomentSums raw =
  ExactPlanarMoments
    { exactPlanarMeasure = oneHalf * rawDoubleArea raw
    , exactPlanarFirstX = oneSixth * rawFirstX raw
    , exactPlanarFirstY = oneSixth * rawFirstY raw
    , exactPlanarSecondXX = oneTwelfth * rawSecondXX raw
    , exactPlanarSecondXY = oneTwentyFourth * rawSecondXY raw
    , exactPlanarSecondYY = oneTwelfth * rawSecondYY raw
    }

-- | Exact unsigned area of one already-admitted polygon component.  Winding
-- and hole containment were discharged by 'polygonComponent', so this
-- observation performs no second geometric validation.
polygonComponentArea :: PolygonComponent -> ExactArea
polygonComponentArea = ExactArea . (oneHalf *) . componentDoubleArea

-- | Exact signed area of an oriented boundary. An admitted bounded cell's
-- left-oriented outer and hole cycles give its nonnegative area. This fold
-- computes only area; it does not charge an area-only observation for moments.
orientedBoundaryArea
  :: Foldable boundary
  => boundary (ExactPoint, ExactPoint)
  -> ExactArea
orientedBoundaryArea = ExactArea . (oneHalf *) . orientedBoundaryDoubleArea

data RawPlanarMomentSums = RawPlanarMomentSums
  { rawDoubleArea :: !ExactRational
  , rawFirstX :: !ExactRational
  , rawFirstY :: !ExactRational
  , rawSecondXX :: !ExactRational
  , rawSecondXY :: !ExactRational
  , rawSecondYY :: !ExactRational
  }

instance Semigroup RawPlanarMomentSums where
  left <> right =
    RawPlanarMomentSums
      { rawDoubleArea = rawDoubleArea left + rawDoubleArea right
      , rawFirstX = rawFirstX left + rawFirstX right
      , rawFirstY = rawFirstY left + rawFirstY right
      , rawSecondXX = rawSecondXX left + rawSecondXX right
      , rawSecondXY = rawSecondXY left + rawSecondXY right
      , rawSecondYY = rawSecondYY left + rawSecondYY right
      }

instance Monoid RawPlanarMomentSums where
  mempty = RawPlanarMomentSums 0 0 0 0 0 0

loopMomentSums :: ExactLoop -> RawPlanarMomentSums
loopMomentSums (ExactLoop points) =
  List.foldl' accumulateMomentEdge mempty (cyclePairs points)

accumulateMomentEdge
  :: RawPlanarMomentSums
  -> (ExactPoint, ExactPoint)
  -> RawPlanarMomentSums
accumulateMomentEdge accumulated (from, to) =
  let (fromX, fromY) = exactPointCoordinates from
      (toX, toY) = exactPointCoordinates to
      cross = exactPointCross from to
   in RawPlanarMomentSums
        { rawDoubleArea = rawDoubleArea accumulated + cross
        , rawFirstX = rawFirstX accumulated + (fromX + toX) * cross
        , rawFirstY = rawFirstY accumulated + (fromY + toY) * cross
        , rawSecondXX =
            rawSecondXX accumulated
              + (fromX * fromX + fromX * toX + toX * toX) * cross
        , rawSecondXY =
            rawSecondXY accumulated
              + (2 * fromX * fromY + fromX * toY + toX * fromY + 2 * toX * toY) * cross
        , rawSecondYY =
            rawSecondYY accumulated
              + (fromY * fromY + fromY * toY + toY * toY) * cross
        }

data PlanarValuations = PlanarValuations
  { valuationEuler :: !EulerCharacteristic
  , valuationArea :: !ExactArea
  , valuationIntrinsic1 :: !ExactLengthMeasurement
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data ValuationError
  = ValuationCoordinateMissing !VertexId
  | ValuationInvalidRegionSegment !ExactGeometryError
  | ValuationSegmentEventsInvalid !ExactSegmentEventObstruction
  | ValuationBoundaryMultiplicity !ExactPoint !ExactPoint !Int
  | ValuationNegativeSquaredLength !ExactRational
  | ValuationCellSetNotPureRegion
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

cellValuations :: ExactCellSet -> Either ValuationError PlanarValuations
cellValuations (ExactCellSet incidence points selectedEdges selectedFaces) = do
  let selectedFaceIds = fmap (FaceId . fromIntegral) (IntSet.toAscList selectedFaces)
  faceDoubleAreas <-
    traverse
      (cellFaceDoubleArea incidence points)
      selectedFaceIds
  edgeContributions <-
    Foldable.foldlM
      ( \contributions ->
          cellEdgeLengthContribution incidence points selectedFaces contributions
            . UndirectedEdgeId
            . fromIntegral
      )
      []
      (IntSet.toAscList selectedEdges)
  assembleValuations
    (IntMap.size points - IntSet.size selectedEdges + sum (fmap (faceEulerContribution incidence) selectedFaceIds))
    (List.foldl' (+) 0 faceDoubleAreas)
    (normalizeLengthContributions id edgeContributions)

regionValuations :: PlanarRegion -> Either ValuationError PlanarValuations
regionValuations (PlanarRegion components) = do
  componentBoundaries <- traverse componentBoundaryData components
  let segments = V.concat (map componentBoundarySegments componentBoundaries)
      componentEuler =
        List.foldl'
          (\total boundary -> total + componentBoundaryEuler boundary)
          0
          componentBoundaries
  contactPlan <-
    if null (overlappingPairs componentBoundaryBounds componentBoundaries)
      then Right Nothing
      else Just <$> first ValuationSegmentEventsInvalid (exactSegmentEventPlan segments)
  boundaryAtoms <- normalizedRegionBoundaryAtoms segments contactPlan
  let euler =
        componentEuler
          - maybe 0 (boundaryContactEuler componentBoundaries) contactPlan
      doubleArea =
        List.foldl'
          (\area component -> area + componentDoubleArea component)
          0
          components
  assembleValuations
    euler
    doubleArea
    ( normalizeLengthContributions
        (\(from, to) -> (positiveHalf, segmentSquaredLength from to))
        boundaryAtoms
    )

assembleValuations
  :: Int
  -> ExactRational
  -> ExactLengthExpression
  -> Either ValuationError PlanarValuations
assembleValuations euler doubleArea lengthExpression =
  PlanarValuations (EulerCharacteristic euler) (ExactArea (oneHalf * doubleArea))
    <$> measureLength lengthExpression
cellSetPerimeter
  :: ExactCellSet
  -> Either ValuationError ExactLengthMeasurement
cellSetPerimeter cellSet
  | exactCellSetIsFaceClosure cellSet =
      cellValuations cellSet >>= planarValuationsPerimeter
  | otherwise = Left ValuationCellSetNotPureRegion

regionPerimeter
  :: PlanarRegion
  -> Either ValuationError ExactLengthMeasurement
regionPerimeter region = regionValuations region >>= planarValuationsPerimeter

-- | Derive conventional boundary length from an already-computed intrinsic
-- valuation without traversing the source geometry again.
planarValuationsPerimeter
  :: PlanarValuations
  -> Either ValuationError ExactLengthMeasurement
planarValuationsPerimeter valuations =
  measureLength
    (scaleLengthExpression positiveTwo (exactLengthExpression (valuationIntrinsic1 valuations)))

-- | Valuation lengths are published at the shared owner's binary64
-- publication precision.
measureLength
  :: ExactLengthExpression
  -> Either ValuationError ExactLengthMeasurement
measureLength = first valuationLengthError . measureLengthExpression publicationPrecision

valuationLengthError :: LengthError -> ValuationError
valuationLengthError (LengthNegativeSquare square) = ValuationNegativeSquaredLength square

cellFaceDoubleArea
  :: PlanarIncidence
  -> IntMap.IntMap ExactPoint
  -> FaceId
  -> Either ValuationError ExactRational
cellFaceDoubleArea incidence points face =
  sum <$> traverse boundaryDoubleArea (faceBoundaryComponents incidence face)
 where
  boundaryDoubleArea edges = do
    coordinates <- traverse (cellPoint points . incidenceOrigin incidence) edges
    pure (maybe 0 (orientedBoundaryDoubleArea . cyclePairs) (NonEmpty.nonEmpty coordinates))

-- | Prepend the edge's boundary length contribution: an edge between two
-- selected faces is interior and contributes nothing, an edge with one
-- selected side contributes half, and a wire edge contributes whole.
cellEdgeLengthContribution
  :: PlanarIncidence
  -> IntMap.IntMap ExactPoint
  -> IntSet.IntSet
  -> [(PositiveExact, ExactRational)]
  -> UndirectedEdgeId
  -> Either ValuationError [(PositiveExact, ExactRational)]
cellEdgeLengthContribution incidence points selectedFaces rest edge =
  case (selected (incidenceIncidentFace incidence forward), selected (incidenceIncidentFace incidence backward)) of
    (True, True) -> Right rest
    (False, False) -> prepend positiveOne
    _ -> prepend positiveHalf
 where
  (fromVertex, toVertex) = incidenceUndirectedEndpoints incidence edge
  (forward, backward) = directedPair edge
  selected face = IntSet.member (faceIdIndex face) selectedFaces
  prepend coefficient = do
    from <- cellPoint points fromVertex
    to <- cellPoint points toVertex
    pure ((coefficient, segmentSquaredLength from to) : rest)

cellPoint
  :: IntMap.IntMap ExactPoint
  -> VertexId
  -> Either ValuationError ExactPoint
cellPoint points vertex =
  maybe
    (Left (ValuationCoordinateMissing vertex))
    Right
    (IntMap.lookup (vertexIdIndex vertex) points)

componentDoubleArea :: PolygonComponent -> ExactRational
componentDoubleArea component =
  List.foldl'
    (\area loop -> area + loopDoubleArea loop)
    0
    (polygonOuterLoop component : polygonHoleLoops component)

loopDoubleArea :: ExactLoop -> ExactRational
loopDoubleArea (ExactLoop points) =
  orientedBoundaryDoubleArea (cyclePairs points)

orientedBoundaryDoubleArea
  :: Foldable boundary
  => boundary (ExactPoint, ExactPoint)
  -> ExactRational
orientedBoundaryDoubleArea =
  Foldable.foldl' (\area (from, to) -> area + exactPointCross from to) 0

segmentSquaredLength :: ExactPoint -> ExactPoint -> ExactRational
segmentSquaredLength from to =
  let (fromX, fromY) = exactPointCoordinates from
      (toX, toY) = exactPointCoordinates to
      deltaX = toX - fromX
      deltaY = toY - fromY
   in deltaX * deltaX + deltaY * deltaY

data ComponentBoundaryData = ComponentBoundaryData
  { componentBoundaryEuler :: !Int
  , componentBoundaryBounds :: !ExactBounds
  , componentBoundarySegments :: !(V.Vector ExactSegment)
  }

componentBoundaryData
  :: PolygonComponent
  -> Either ValuationError ComponentBoundaryData
componentBoundaryData component = do
  segments <-
    V.fromList
      <$> traverse
        admittedSegment
        ( concatMap
            (cyclePairs . loopPoints)
            (polygonOuterLoop component : polygonHoleLoops component)
        )
  pure
    ComponentBoundaryData
      { componentBoundaryEuler = 1 - length (polygonHoleLoops component)
      , componentBoundaryBounds = componentBounds component
      , componentBoundarySegments = segments
      }

-- | The Euler characteristic of the contact each component's boundary makes
-- with the boundaries of the components before it, summed over components.
--
-- Every contact is read from one plan over the whole boundary family. An
-- event between two components is charged to the later one, so each
-- component's contact graph is exactly what a sweep over that component and
-- its predecessors would have reported; the vertex and edge sets stay
-- per-component because the sum, not a global graph, is the quantity.
boundaryContactEuler
  :: [ComponentBoundaryData]
  -> ExactSegmentEventPlan
  -> Int
boundaryContactEuler boundaries plan =
  IntMap.foldl'
    (\total contacts -> total + contactGraphEuler plan contacts)
    0
    contactsByComponent
 where
  componentOf =
    U.concat
      ( zipWith
          (\component boundary ->
             U.replicate (V.length (componentBoundarySegments boundary)) component)
          [0 :: Int ..]
          boundaries
      )
  contactsByComponent =
    IntMap.fromListWith
      (<>)
      [ (max leftComponent rightComponent, [contactFromEvent event])
      | event <- exactSegmentEvents plan
      , let (ExactSweepSegmentId left, ExactSweepSegmentId right) = eventIds event
            leftComponent = componentOf `U.unsafeIndex` left
            rightComponent = componentOf `U.unsafeIndex` right
      , leftComponent /= rightComponent
      ]

-- | Vertices minus edges of the graph the contacts form once every interval
-- is subdivided at the split points of the segment carrying it.
--
-- A split point of any segment that lies on an interval is a contact of that
-- segment with the interval's carrier and therefore already a split point of
-- the carrier, so the carrier's own split points subdivide the interval
-- exactly as the split points of the whole family would.
contactGraphEuler :: ExactSegmentEventPlan -> [BoundaryContact] -> Int
contactGraphEuler plan contacts = Set.size vertices - Set.size contactEdges
 where
  contactPoints =
    Set.fromList
      [ point
      | ContactPoint point <- contacts
      ]
  contactEdges =
    Set.fromList
      [ orderedPair from to
      | contact <- contacts
      , (from, to) <- consecutivePairs (intervalPoints contact)
      , from /= to
      ]
  intervalPoints contact =
    case contact of
      ContactPoint _ -> []
      ContactSegment carrier -> exactSegmentSplitPoints plan carrier
      ContactInterval carrier lower upper ->
        filter (exactOnClosedSegment lower upper) (exactSegmentSplitPoints plan carrier)
  vertices =
    Set.union
      contactPoints
      ( Set.fromList
          [ point
          | (from, to) <- Set.toAscList contactEdges
          , point <- [from, to]
          ]
      )

data BoundaryContact
  = ContactPoint !ExactPoint
  | ContactSegment !ExactSweepSegmentId
  | ContactInterval !ExactSweepSegmentId !ExactPoint !ExactPoint

contactFromEvent :: ExactSegmentEvent -> BoundaryContact
contactFromEvent (ExactProperCrossing _ _ point) = ContactPoint point
contactFromEvent (ExactEndpointTouch _ _ point) = ContactPoint point
contactFromEvent (ExactSharedEndpoint _ _ point) = ContactPoint point
contactFromEvent (ExactDuplicateSegments leftId _) = ContactSegment leftId
contactFromEvent (ExactCollinearOverlap leftId _ lower upper) =
  uncurry (ContactInterval leftId) (orderedPair lower upper)

eventIds
  :: ExactSegmentEvent
  -> (ExactSweepSegmentId, ExactSweepSegmentId)
eventIds (ExactProperCrossing left right _) = (left, right)
eventIds (ExactEndpointTouch left right _) = (left, right)
eventIds (ExactSharedEndpoint left right _) = (left, right)
eventIds (ExactDuplicateSegments left right) = (left, right)
eventIds (ExactCollinearOverlap left right _ _) = (left, right)

normalizedRegionBoundaryAtoms
  :: V.Vector ExactSegment
  -> Maybe ExactSegmentEventPlan
  -> Either ValuationError (Set.Set (ExactPoint, ExactPoint))
normalizedRegionBoundaryAtoms segments Nothing =
  Right (Set.fromList (map canonicalSegmentEndpoints (V.toList segments)))
normalizedRegionBoundaryAtoms segments (Just plan) =
  traverseMultiplicity
    (Map.toAscList (Map.fromListWith (+) orientedAtoms))
 where
  orientedAtoms =
    concatMap
      segmentAtoms
      [ exactSegmentSplitPoints plan (ExactSweepSegmentId index)
      | index <- [0 .. V.length segments - 1]
      ]
  segmentAtoms :: [ExactPoint] -> [((ExactPoint, ExactPoint), Int)]
  segmentAtoms points =
    [ ( orderedPair firstPoint secondPoint
      , if firstPoint <= secondPoint then 1 else -1
      )
    | (firstPoint, secondPoint) <- consecutivePairs points
    , firstPoint /= secondPoint
    ]
  traverseMultiplicity
    :: [((ExactPoint, ExactPoint), Int)]
    -> Either ValuationError (Set.Set (ExactPoint, ExactPoint))
  traverseMultiplicity entries = do
    retained <-
      traverse
        (\(edge@(from, to), multiplicity) ->
           case abs multiplicity of
             0 -> Right Nothing
             1 -> Right (Just edge)
             _ -> Left (ValuationBoundaryMultiplicity from to multiplicity))
        entries
    pure (Set.fromList (catMaybes retained))

admittedSegment
  :: (ExactPoint, ExactPoint)
  -> Either ValuationError ExactSegment
admittedSegment (from, to) = first ValuationInvalidRegionSegment (exactSegment from to)

canonicalSegmentEndpoints :: ExactSegment -> (ExactPoint, ExactPoint)
canonicalSegmentEndpoints = uncurry orderedPair . exactSegmentEndpoints

loopPoints :: ExactLoop -> NonEmpty ExactPoint
loopPoints (ExactLoop points) = points

oneHalf :: ExactRational
oneHalf = exactRationalFromDyadic 1 (-1)

positiveHalf :: PositiveExact
positiveHalf = ratioPositive positiveOne positiveTwo

oneSixth :: ExactRational
oneSixth = exactRationalFromNormalizedRatio (1 Ratio.% 6)

oneTwelfth :: ExactRational
oneTwelfth = exactRationalFromNormalizedRatio (1 Ratio.% 12)

oneTwentyFourth :: ExactRational
oneTwentyFourth = exactRationalFromNormalizedRatio (1 Ratio.% 24)
