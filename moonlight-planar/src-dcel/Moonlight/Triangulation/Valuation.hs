{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}

-- | Intrinsic valuations of exact closed cell selections and admitted planar
-- regions. Euler characteristic and area remain exact; Euclidean length is an
-- exact radical expression accompanied by outward-rounded binary64 bounds.
module Moonlight.Triangulation.Valuation
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
  , polygonComponentMoments
  , polygonComponentArea
  , ExactLengthTerm
  , lengthCoefficient
  , squaredLength
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
import Data.Bits (shiftL)
import Data.Foldable (foldlM)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import qualified Data.Ratio as Ratio
import qualified Data.Set as Set
import qualified Data.Vector as V
import GHC.Float (castDoubleToWord64, castWord64ToDouble)
import GHC.Generics (Generic)
import Moonlight.Triangulation.Dcel
  ( faceVertices
  , incidentFace
  , undirectedEndpoints
  )
import Moonlight.Triangulation.Exact
  ( ExactGeometryError
  , ExactPoint
  , ExactSegment
  , exactOnClosedSegment
  , exactPointCross
  , exactPointCoordinates
  , exactSegment
  , exactSegmentEndpoints
  )
import Moonlight.Triangulation.Internal.HandleDefs
  ( FaceId (..)
  , UndirectedEdgeId (..)
  , VertexId (..)
  , directedPair
  , faceIdIndex
  , vertexIdIndex
  )
import Moonlight.Triangulation.Internal.CellSet
  ( ExactCellSet (..)
  , exactCellSetIsFaceClosure
  )
import Moonlight.Triangulation.Internal.BoundaryCycle
  ( consecutivePairs
  , cyclePairs
  , orderedPair
  )
import Moonlight.Triangulation.Internal.Dyadic (integerBitLength)
import Moonlight.Triangulation.Internal.ExactRational
  ( ExactRational
  , exactRationalDenominator
  , exactRationalFromDyadic
  , exactRationalFromFiniteDouble
  , exactRationalFromNormalizedRatio
  , exactRationalIsZero
  , exactRationalNumerator
  , exactSignum
  )
import Moonlight.Triangulation.Internal.ExactSegmentEvents
  ( ExactSegmentEvent (..)
  , ExactSegmentEventObstruction
  , ExactSweepSegmentId (..)
  , exactSegmentEventPlan
  , exactSegmentEvents
  , exactSegmentSplitPoints
  )
import Moonlight.Triangulation.Internal.Region.Types
  ( ExactLoop (..)
  , PlanarRegion (..)
  , PolygonComponent (..)
  )
import Moonlight.Triangulation.Internal.Region.Bounds
  ( ExactBounds
  , componentBounds
  , overlappingPredecessors
  )
import Moonlight.Triangulation.Internal.Representation (Triangulation)

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
  let raw =
        foldMap
          loopMomentSums
          (polygonOuterLoop component : polygonHoleLoops component)
   in ExactPlanarMoments
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

data ExactLengthTerm = ExactLengthTerm
  { lengthCoefficient :: !ExactRational
  , squaredLength :: !ExactRational
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | A normalized sum of rational coefficients times square roots of rational
-- squared lengths. It intentionally has no 'Eq' instance: syntactic radical
-- normalization is not algebraic-number equality.
newtype ExactLengthExpression = ExactLengthExpression [ExactLengthTerm]
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

exactLengthTerms :: ExactLengthExpression -> [ExactLengthTerm]
exactLengthTerms (ExactLengthExpression terms) = terms

data CertifiedInterval = CertifiedInterval
  { intervalLower :: !Double
  , intervalUpper :: !Double
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data ExactLengthMeasurement = ExactLengthMeasurement
  { exactLengthExpression :: !ExactLengthExpression
  , exactLengthBounds :: !CertifiedInterval
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data PlanarValuations = PlanarValuations
  { valuationEuler :: !EulerCharacteristic
  , valuationArea :: !ExactArea
  , valuationIntrinsic1 :: !ExactLengthMeasurement
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data ValuationError
  = ValuationCoordinateMissing !VertexId
  | ValuationFaceArity !FaceId !Int
  | ValuationInvalidRegionSegment !ExactGeometryError
  | ValuationSegmentEventsInvalid !ExactSegmentEventObstruction
  | ValuationSegmentMissing !ExactSweepSegmentId
  | ValuationBoundaryMultiplicity !ExactPoint !ExactPoint !Int
  | ValuationNegativeSquaredLength !ExactRational
  | ValuationCellSetNotPureRegion
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

cellValuations :: ExactCellSet -> Either ValuationError PlanarValuations
cellValuations (ExactCellSet triangulation points selectedEdges selectedFaces) = do
  faceDoubleAreas <-
    traverse
      (cellFaceDoubleArea triangulation points . FaceId . fromIntegral)
      (IntSet.toAscList selectedFaces)
  edgeContributions <-
    traverse
      ( cellEdgeLengthContribution triangulation points selectedFaces
          . UndirectedEdgeId
          . fromIntegral
      )
      (IntSet.toAscList selectedEdges)
  assembleValuations
    (IntMap.size points - IntSet.size selectedEdges + IntSet.size selectedFaces)
    (List.foldl' (+) 0 faceDoubleAreas)
    (normalizeLengthContributions id edgeContributions)

regionValuations :: PlanarRegion -> Either ValuationError PlanarValuations
regionValuations (PlanarRegion components) = do
  componentBoundaries <- traverse componentBoundaryData components
  let boundaryCover =
        overlappingPredecessors componentBoundaryBounds componentBoundaries
      hasPotentialBoundaryContacts = not (all (null . snd) boundaryCover)
  euler <- regionEuler boundaryCover
  boundaryAtoms <-
    normalizedRegionBoundaryAtoms
      hasPotentialBoundaryContacts
      componentBoundaries
  let doubleArea =
        List.foldl'
          (\area component -> area + componentDoubleArea component)
          0
          components
  assembleValuations
    euler
    doubleArea
    ( normalizeLengthContributions
        (\(from, to) -> (oneHalf, segmentSquaredLength from to))
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
    (scaleLengthExpression 2 (exactLengthExpression (valuationIntrinsic1 valuations)))

cellFaceDoubleArea
  :: Triangulation mode vertex directed undirected face
  -> IntMap.IntMap ExactPoint
  -> FaceId
  -> Either ValuationError ExactRational
cellFaceDoubleArea triangulation points face =
  case faceVertices triangulation face of
    [firstVertex, secondVertex, thirdVertex] -> do
      firstPoint <- cellPoint points firstVertex
      secondPoint <- cellPoint points secondVertex
      thirdPoint <- cellPoint points thirdVertex
      pure (triangleDoubleArea firstPoint secondPoint thirdPoint)
    vertices -> Left (ValuationFaceArity face (length vertices))

cellEdgeLengthContribution
  :: Triangulation mode vertex directed undirected face
  -> IntMap.IntMap ExactPoint
  -> IntSet.IntSet
  -> UndirectedEdgeId
  -> Either ValuationError (ExactRational, ExactRational)
cellEdgeLengthContribution triangulation points selectedFaces edge = do
  let (fromVertex, toVertex) = undirectedEndpoints triangulation edge
      (forward, backward) = directedPair edge
      selected face = IntSet.member (faceIdIndex face) selectedFaces
      coefficient = case (selected (incidentFace triangulation forward), selected (incidentFace triangulation backward)) of
        (False, False) -> 1
        (True, True) -> 0
        _ -> oneHalf
  from <- cellPoint points fromVertex
  to <- cellPoint points toVertex
  pure (coefficient, segmentSquaredLength from to)

cellPoint
  :: IntMap.IntMap ExactPoint
  -> VertexId
  -> Either ValuationError ExactPoint
cellPoint points vertex =
  maybe
    (Left (ValuationCoordinateMissing vertex))
    Right
    (IntMap.lookup (vertexIdIndex vertex) points)

triangleDoubleArea :: ExactPoint -> ExactPoint -> ExactPoint -> ExactRational
triangleDoubleArea firstPoint secondPoint thirdPoint =
  exactPointCross firstPoint secondPoint
    + exactPointCross secondPoint thirdPoint
    + exactPointCross thirdPoint firstPoint

componentDoubleArea :: PolygonComponent -> ExactRational
componentDoubleArea component =
  List.foldl'
    (\area loop -> area + loopDoubleArea loop)
    0
    (polygonOuterLoop component : polygonHoleLoops component)

loopDoubleArea :: ExactLoop -> ExactRational
loopDoubleArea (ExactLoop points) =
  List.foldl'
    (\area (from, to) -> area + exactPointCross from to)
    0
    (cyclePairs points)

segmentSquaredLength :: ExactPoint -> ExactPoint -> ExactRational
segmentSquaredLength from to =
  let (fromX, fromY) = exactPointCoordinates from
      (toX, toY) = exactPointCoordinates to
      deltaX = toX - fromX
      deltaY = toY - fromY
   in deltaX * deltaX + deltaY * deltaY

normalizeLengthContributions
  :: Foldable collection
  => (value -> (ExactRational, ExactRational))
  -> collection value
  -> ExactLengthExpression
normalizeLengthContributions contribution contributions =
  ExactLengthExpression
    [ ExactLengthTerm coefficient square
    | (square, coefficient) <- Map.toAscList coefficientsBySquare
    , not (exactRationalIsZero coefficient)
    ]
 where
  coefficientsBySquare =
    List.foldl' accumulateContribution Map.empty contributions
  accumulateContribution coefficients value =
    case contribution value of
      (coefficient, square)
        | exactRationalIsZero coefficient -> coefficients
        | otherwise -> Map.insertWith (+) square coefficient coefficients

scaleLengthExpression
  :: Integer
  -> ExactLengthExpression
  -> ExactLengthExpression
scaleLengthExpression scalar (ExactLengthExpression terms) =
  let exactScalar = fromInteger scalar
   in ExactLengthExpression
        [ term
            { lengthCoefficient =
                exactScalar * lengthCoefficient term
            }
        | term <- terms
        ]

measureLength
  :: ExactLengthExpression
  -> Either ValuationError ExactLengthMeasurement
measureLength expression@(ExactLengthExpression terms) = do
  (lower, upper) <-
    foldlM
      addTermBounds
      (0, 0)
      terms
  pure
    ExactLengthMeasurement
      { exactLengthExpression = expression
      , exactLengthBounds =
          CertifiedInterval
            { intervalLower = directedLowerDouble lower
            , intervalUpper = directedUpperDouble upper
            }
      }
 where
  addTermBounds (lowerTotal, upperTotal) term = do
    (lowerRoot, upperRoot) <- exactSquareRootBounds (squaredLength term)
    let coefficient = lengthCoefficient term
    pure
      ( lowerTotal + coefficient * lowerRoot
      , upperTotal + coefficient * upperRoot
      )

exactSquareRootBounds
  :: ExactRational
  -> Either ValuationError (ExactRational, ExactRational)
exactSquareRootBounds value =
  case exactSignum value of
    LT -> Left (ValuationNegativeSquaredLength value)
    _ ->
      let numerator = exactRationalNumerator value
          denominator = exactRationalDenominator value
          scale = 1 `shiftL` radicalPrecisionBits
          scaledNumerator = numerator * scale * scale
          root = integerSquareRoot (scaledNumerator `div` denominator)
          exact = root * root * denominator == scaledNumerator
          dyadicPower = negate radicalPrecisionBits
       in Right
            ( exactRationalFromDyadic root dyadicPower
            , exactRationalFromDyadic (if exact then root else root + 1) dyadicPower
            )

radicalPrecisionBits :: Int
radicalPrecisionBits = 128

integerSquareRoot :: Integer -> Integer
integerSquareRoot value
  | value < 2 = value
  | otherwise = descend initial
 where
  initial = 1 `shiftL` ((integerBitLength value + 1) `div` 2)
  descend estimate =
    let refined = (estimate + value `div` estimate) `div` 2
     in if refined >= estimate then estimate else descend refined

directedLowerDouble :: ExactRational -> Double
directedLowerDouble value =
  let candidate = rationalToDouble value
   in if isInfinite candidate
        then maximumFiniteDouble
        else
          if exactRationalFromFiniteDouble candidate <= value
            then candidate
            else previousPositiveDouble candidate

directedUpperDouble :: ExactRational -> Double
directedUpperDouble value =
  let candidate = rationalToDouble value
   in if isInfinite candidate
        || exactRationalFromFiniteDouble candidate >= value
        then candidate
        else nextPositiveDouble candidate

rationalToDouble :: ExactRational -> Double
rationalToDouble value =
  fromRational
    ( exactRationalNumerator value
        Ratio.% exactRationalDenominator value
    )

previousPositiveDouble :: Double -> Double
previousPositiveDouble value
  | value <= 0 = 0
  | otherwise = castWord64ToDouble (castDoubleToWord64 value - 1)

nextPositiveDouble :: Double -> Double
nextPositiveDouble value
  | value == 0 = castWord64ToDouble 1
  | otherwise = castWord64ToDouble (castDoubleToWord64 value + 1)

maximumFiniteDouble :: Double
maximumFiniteDouble = castWord64ToDouble 0x7fefffffffffffff

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

regionEuler
  :: [(ComponentBoundaryData, [ComponentBoundaryData])]
  -> Either ValuationError Int
regionEuler = foldlM attachComponent 0
 where
  attachComponent accumulatedEuler (current, priorCandidates) = do
    let priorSegments = V.concat (map componentBoundarySegments priorCandidates)
    intersectionEuler <-
      boundaryIntersectionEuler
        (componentBoundarySegments current)
        priorSegments
    pure
      ( accumulatedEuler
          + componentBoundaryEuler current
          - intersectionEuler
      )

boundaryIntersectionEuler
  :: V.Vector ExactSegment
  -> V.Vector ExactSegment
  -> Either ValuationError Int
boundaryIntersectionEuler current prior
  | V.null prior = Right 0
  | otherwise = do
      let currentCount = V.length current
          segments = current <> prior
      plan <- first ValuationSegmentEventsInvalid (exactSegmentEventPlan segments)
      contacts <-
        traverse
          (contactFromEvent segments)
          [ event
          | event <- exactSegmentEvents plan
          , crossPartition currentCount event
          ]
      let contactPoints =
            Set.fromList
              [ point
              | ContactPoint point <- contacts
              ]
          intervals =
            [ interval
            | ContactInterval interval <- contacts
            ]
          allSplitPoints =
            Set.fromList
              ( concatMap
                  (exactSegmentSplitPoints plan)
                  [ ExactSweepSegmentId index
                  | index <- [0 .. V.length segments - 1]
                  ]
              )
          contactEdges =
            Set.fromList
              [ orderedPair from to
              | interval <- intervals
              , let (lower, upper) = interval
                    points =
                      Set.toAscList
                        ( Set.filter
                            (exactOnClosedSegment lower upper)
                            allSplitPoints
                        )
              , (from, to) <- consecutivePairs points
              , from /= to
              ]
          vertices =
            Set.unions
              [ contactPoints
              , Set.fromList
                  [ point
                  | (from, to) <- Set.toAscList contactEdges
                  , point <- [from, to]
                  ]
              ]
      pure (Set.size vertices - Set.size contactEdges)

data BoundaryContact
  = ContactPoint !ExactPoint
  | ContactInterval !(ExactPoint, ExactPoint)

contactFromEvent
  :: V.Vector ExactSegment
  -> ExactSegmentEvent
  -> Either ValuationError BoundaryContact
contactFromEvent _ (ExactProperCrossing _ _ point) = Right (ContactPoint point)
contactFromEvent _ (ExactEndpointTouch _ _ point) = Right (ContactPoint point)
contactFromEvent _ (ExactSharedEndpoint _ _ point) = Right (ContactPoint point)
contactFromEvent segments (ExactDuplicateSegments leftId _) =
  ContactInterval . canonicalSegmentEndpoints
    <$> requireSegment segments leftId
contactFromEvent _ (ExactCollinearOverlap _ _ lower upper) =
  Right (ContactInterval (orderedPair lower upper))

crossPartition :: Int -> ExactSegmentEvent -> Bool
crossPartition boundary event =
  let (ExactSweepSegmentId left, ExactSweepSegmentId right) = eventIds event
   in (left < boundary) /= (right < boundary)

eventIds
  :: ExactSegmentEvent
  -> (ExactSweepSegmentId, ExactSweepSegmentId)
eventIds (ExactProperCrossing left right _) = (left, right)
eventIds (ExactEndpointTouch left right _) = (left, right)
eventIds (ExactSharedEndpoint left right _) = (left, right)
eventIds (ExactDuplicateSegments left right) = (left, right)
eventIds (ExactCollinearOverlap left right _ _) = (left, right)

normalizedRegionBoundaryAtoms
  :: Bool
  -> [ComponentBoundaryData]
  -> Either ValuationError (Set.Set (ExactPoint, ExactPoint))
normalizedRegionBoundaryAtoms hasPotentialBoundaryContacts boundaries
  | V.null segments = Right Set.empty
  | not hasPotentialBoundaryContacts =
      Right
        ( Set.fromList
            (map canonicalSegmentEndpoints (V.toList segments))
        )
  | otherwise = do
      plan <- first ValuationSegmentEventsInvalid (exactSegmentEventPlan segments)
      let orientedAtoms =
            concatMap
              segmentAtoms
              [ exactSegmentSplitPoints plan (ExactSweepSegmentId index)
              | index <- [0 .. V.length segments - 1]
              ]
      traverseMultiplicity
        (Map.toAscList (Map.fromListWith (+) orientedAtoms))
 where
  segments = V.concat (map componentBoundarySegments boundaries)
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

requireSegment
  :: V.Vector ExactSegment
  -> ExactSweepSegmentId
  -> Either ValuationError ExactSegment
requireSegment segments segmentId@(ExactSweepSegmentId index) =
  maybe
    (Left (ValuationSegmentMissing segmentId))
    Right
    (segments V.!? index)

canonicalSegmentEndpoints :: ExactSegment -> (ExactPoint, ExactPoint)
canonicalSegmentEndpoints = uncurry orderedPair . exactSegmentEndpoints

loopPoints :: ExactLoop -> NonEmpty ExactPoint
loopPoints (ExactLoop points) = points

oneHalf :: ExactRational
oneHalf = exactRationalFromDyadic 1 (-1)

oneSixth :: ExactRational
oneSixth = exactRationalFromNormalizedRatio (1 Ratio.% 6)

oneTwelfth :: ExactRational
oneTwelfth = exactRationalFromNormalizedRatio (1 Ratio.% 12)

oneTwentyFourth :: ExactRational
oneTwentyFourth = exactRationalFromNormalizedRatio (1 Ratio.% 24)
