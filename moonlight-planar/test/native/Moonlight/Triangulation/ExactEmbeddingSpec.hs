{-# LANGUAGE NumericUnderscores #-}

-- | Focused exact-geometry and local-embedding milestone acceptance.
module Moonlight.Triangulation.ExactEmbeddingSpec (tests) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (unless)
import Data.Bits (xor)
import Data.Foldable (traverse_)
import Data.List (sort)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import GHC.Stats (allocated_bytes, getRTSStats, getRTSStatsEnabled)
import Moonlight.Triangulation.Exact
  ( ExactGeometryError (..)
  , ExactIntersectionError (..)
  , ExactPoint
  , exactLineIntersection
  , exactPoint
  , exactPointCoordinates
  , exactPointFromPoint
  , exactPointToEmbeddingCandidate
  , exactSegment
  , exactSegmentRelation
  )
import Moonlight.Triangulation.Internal.ExactRational
  ( ExactArithmeticError (..)
  , ExactRational
  , exactDivide
  , exactRational
  , exactRationalDenominator
  , exactRationalFromDouble
  , exactRationalNumerator
  , exactSignum
  )
import Moonlight.Triangulation.Embedding
  ( DraftIncidence (..)
  , DraftId (..)
  , DraftNeighborhood (..)
  , DraftSegmentId
  , DraftVertexId
  , EmbeddingObligation (..)
  , ExactArrangementDraft (..)
  , certificateIncidenceRelationPreservationCount
  , certificateNeighborhoodRotationPreservationCount
  , certificateResidual
  , certificateRoundedVertexDistinctnessCount
  , certificateRoundedVertices
  , certificateSplitOrderPreservationCount
  , OverlayEmbeddingObstruction (..)
  , certifyLocalEmbedding
  , residualUndischargedObligations
  )
import Moonlight.Triangulation.Internal.Predicates (segmentRelation)
import Moonlight.Triangulation.Math
  ( SegmentRelation (..)
  , allSegmentRelations
  )
import Moonlight.Triangulation.Types
  ( CoordinateError (..)
  , Point (..)
  , PointValidationError (..)
  , queryPointValue
  )
import Support (assertEqual, integerPoint, requireRight)
import System.Mem (performGC)

-- | Run the exact geometry, embedding, and frozen binary64 acceptance actions.
tests :: IO ()
tests =
  sequence_
    [ testExactNonDyadicCrossing
    , testAllSegmentRelationsAgree
    , testSubUlpExactVertexCluster
    , testContactDuplicateAndOverlapAgreement
    , testVerticalNestedOverlapRefusal
    , testCoordinateRefusalsAndRoundtrip
    , testExactArithmeticReceipt
    , testEmbeddingAdmissionReceipt
    , testOrdinaryDraftCertificate
    , testCollinearNeighborhoodRotation
    , testFrozenBinary64RelationOracle
    ]

testExactNonDyadicCrossing :: IO ()
testExactNonDyadicCrossing = do
  firstSegment <-
    requireRight
      "first non-dyadic crossing segment"
      (exactSegment (integerPoint 0 0) (integerPoint 1 1))
  secondSegment <-
    requireRight
      "second non-dyadic crossing segment"
      (exactSegment (integerPoint 0 1) (integerPoint 2 0))
  crossing <-
    requireRight
      "non-dyadic exact line intersection"
      (exactLineIntersection firstSegment secondSegment)
  let (crossingX, crossingY) = exactPointCoordinates crossing
  assertRatio "non-dyadic crossing x" 2 3 crossingX
  assertRatio "non-dyadic crossing y" 2 3 crossingY
  putStrLn
    ( "exact non-dyadic crossing coordinates: "
        <> show (exactRationalNumerator crossingX, exactRationalDenominator crossingX)
        <> ", "
        <> show (exactRationalNumerator crossingY, exactRationalDenominator crossingY)
    )

testAllSegmentRelationsAgree :: IO ()
testAllSegmentRelationsAgree = do
  observed <- traverse observeExactAndRounded handFixtures
  traverse_ assertExpectedAndAgreement observed
  assertEqual
    "all exact segment relations covered"
    allSegmentRelations
    (sort (map observedExactRelation observed))

testSubUlpExactVertexCluster :: IO ()
testSubUlpExactVertexCluster = do
  let base = 2 ^ (52 :: Int)
  firstCoordinate <- requireRight "first sub-ulp coordinate" (exactRational (base * 4 + 1) 4)
  secondCoordinate <- requireRight "second sub-ulp coordinate" (exactRational (base * 4 + 2) 4)
  thirdCoordinate <- requireRight "third sub-ulp coordinate" (exactRational (base * 4 + 3) 4)
  firstCrossing <- exactCrossingAt firstCoordinate
  secondCrossing <- exactCrossingAt secondCoordinate
  thirdCrossing <- exactCrossingAt thirdCoordinate
  let draft =
        emptyDraft
          { draftVertices =
              Map.fromList
                [ (DraftId 0, firstCrossing)
                , (DraftId 1, secondCrossing)
                , (DraftId 2, thirdCrossing)
                ]
          }
  assertLeftContains
    "sub-ulp exact vertex cluster"
    isRoundedCollision
    (certifyLocalEmbedding draft)

testContactDuplicateAndOverlapAgreement :: IO ()
testContactDuplicateAndOverlapAgreement =
  traverse_ assertContactAgreement contactFixtures

testVerticalNestedOverlapRefusal :: IO ()
testVerticalNestedOverlapRefusal = do
  let base = 2 ^ (52 :: Int)
      zero = 0
      lower = fromInteger base
      upper = fromInteger (base + 1)
  innerLower <- requireRight "vertical inner lower" (exactRational (base * 3 + 1) 3)
  innerUpper <- requireRight "vertical inner upper" (exactRational (base * 3 + 2) 3)
  let outerFrom = exactPoint zero lower
      outerTo = exactPoint zero upper
      innerFrom = exactPoint zero innerLower
      innerTo = exactPoint zero innerUpper
      outerSegmentId, innerSegmentId :: DraftSegmentId
      outerSegmentId = DraftId 0
      innerSegmentId = DraftId 1
      draft =
        emptyDraft
          { draftVertices =
              Map.fromList
                [ (DraftId 0, outerFrom)
                , (DraftId 1, outerTo)
                , (DraftId 2, innerFrom)
                , (DraftId 3, innerTo)
                ]
          , draftSegments =
              Map.fromList
                [ (outerSegmentId, (DraftId 0, DraftId 1))
                , (innerSegmentId, (DraftId 2, DraftId 3))
                ]
          , draftIncidences =
              [ DraftIncidence
                  outerSegmentId
                  innerSegmentId
                  SegmentsCollinearlyOverlap
              ]
          }
  assertEqual
    "vertical nested overlap remains exact"
    SegmentsCollinearlyOverlap
    (exactSegmentRelation outerFrom outerTo innerFrom innerTo)
  assertLeftContains
    "vertical nested overlap rounded incidence"
    isIncidenceChange
    (certifyLocalEmbedding draft)

testCoordinateRefusalsAndRoundtrip :: IO ()
testCoordinateRefusalsAndRoundtrip = do
  assertEqual
    "zero exact denominator refusal"
    (Left ExactZeroDenominator)
    (exactRational 1 0)
  assertEqual
    "zero exact divisor refusal"
    (Left ExactZeroDivisor)
    (exactDivide 1 0)
  normalized <- requireRight "normalized exact rational" (exactRational (-2) (-4))
  assertRatio "normalized exact rational" 1 2 normalized
  canonicalZero <- requireRight "canonical exact zero" (exactRational 0 (-7))
  assertRatio "canonical exact zero" 0 1 canonicalZero
  assertEqual
    "NaN exact rational refusal"
    (Left ExactNaNInput)
    (exactRationalFromDouble (0 / 0))
  assertEqual
    "infinite exact rational refusal"
    (Left ExactInfiniteInput)
    (exactRationalFromDouble (1 / 0))
  assertEqual
    "NaN exact point refusal"
    (Left (InvalidPointX CoordinateNaN))
    (exactPointFromPoint (Point (0 / 0) 0))
  assertEqual
    "infinite exact point refusal"
    (Left (InvalidPointX CoordinateInfinite))
    (exactPointFromPoint (Point (1 / 0) 0))
  let unprojectablePoint =
        exactPoint
          (fromInteger (10 ^ (400 :: Int)))
          0
      unprojectableDraft =
        emptyDraft
          { draftVertices =
              Map.singleton (DraftId 0) unprojectablePoint
          }
  assertLeftContains
    "unprojectable exact draft vertex"
    isProjectionRefusal
    (certifyLocalEmbedding unprojectableDraft)
  let finitePoint = Point 1.25 (-2.5)
  exactFinite <- requireRight "finite point exact conversion" (exactPointFromPoint finitePoint)
  projectedFinite <-
    requireRight
      "finite point candidate projection"
      (exactPointToEmbeddingCandidate exactFinite)
  assertEqual "finite exact point roundtrip" finitePoint (queryPointValue projectedFinite)
  assertEqual
    "coincident exact segment endpoints"
    (Left (ExactSegmentEndpointsCoincide exactFinite))
    (exactSegment exactFinite exactFinite)
  duplicateSegment <-
    requireRight
      "duplicate line intersection segment"
      (exactSegment (integerPoint 0 0) (integerPoint 2 0))
  assertEqual
    "duplicate line intersection refusal"
    (Left (ExactIntersectionNonUnique SegmentsDuplicate))
    (exactLineIntersection duplicateSegment duplicateSegment)

testOrdinaryDraftCertificate :: IO ()
testOrdinaryDraftCertificate = do
  half <- requireRight "ordinary draft half parameter" (exactRational 1 2)
  let west, center, east, south, north :: DraftVertexId
      west = DraftId 0
      center = DraftId 1
      east = DraftId 2
      south = DraftId 3
      north = DraftId 4
      westCenter, centerEast, southCenter, centerNorth :: DraftSegmentId
      westCenter = DraftId 0
      centerEast = DraftId 1
      southCenter = DraftId 2
      centerNorth = DraftId 3
      draft =
        ExactArrangementDraft
          { draftVertices =
              Map.fromList
                [ (west, integerPoint (-1) 0)
                , (center, integerPoint 0 0)
                , (east, integerPoint 1 0)
                , (south, integerPoint 0 (-1))
                , (north, integerPoint 0 1)
                ]
          , draftSegments =
              Map.fromList
                [ (westCenter, (west, center))
                , (centerEast, (center, east))
                , (southCenter, (south, center))
                , (centerNorth, (center, north))
                ]
          , draftSourceMemberships =
              Map.fromList
                [ ( DraftId 0
                  , [ (0, west)
                    , (half, center)
                    , (1, east)
                    ]
                  )
                , ( DraftId 1
                  , [ (0, south)
                    , (half, center)
                    , (1, north)
                    ]
                  )
                ]
          , draftIncidences =
              [ DraftIncidence westCenter southCenter SegmentsShareEndpoint
              ]
          , draftNeighborhoods =
              [ DraftNeighborhood center (east :| [north, west, south])
              ]
          }
  certificate <- requireRight "ordinary local embedding" (certifyLocalEmbedding draft)
  westEmbedding <-
    maybe
      (fail "ordinary local embedding omitted its admitted west candidate")
      pure
      (Map.lookup west (certificateRoundedVertices certificate))
  assertEqual
    "ordinary certificate retains admitted projection"
    (Point (-1) 0)
    (queryPointValue westEmbedding)
  assertEqual
    "ordinary distinctness obligations"
    10
    (certificateRoundedVertexDistinctnessCount certificate)
  assertEqual
    "ordinary split-order obligations"
    4
    (certificateSplitOrderPreservationCount certificate)
  assertEqual
    "ordinary incidence obligations"
    1
    (certificateIncidenceRelationPreservationCount certificate)
  assertEqual
    "ordinary neighborhood obligations"
    4
    (certificateNeighborhoodRotationPreservationCount certificate)
  assertEqual
    "ordinary residual"
    (GlobalNoNewCrossing :| [])
    (residualUndischargedObligations (certificateResidual certificate))
  putStrLn
    ( "local embedding certificate counts: "
        <> show
          ( certificateRoundedVertexDistinctnessCount certificate
          , certificateSplitOrderPreservationCount certificate
          , certificateIncidenceRelationPreservationCount certificate
          , certificateNeighborhoodRotationPreservationCount certificate
          )
    )
  putStrLn
    ( "local embedding declared residual: "
        <> show (residualUndischargedObligations (certificateResidual certificate))
    )

-- Independent binary64 projection bends the exact straight-through pair at
-- the center, but the cyclic neighbor order is unchanged. The local topology
-- obligation is rotation preservation, not literal preservation of a zero
-- determinant.
testCollinearNeighborhoodRotation :: IO ()
testCollinearNeighborhoodRotation = do
  oneThird <- requireRight "collinear rotation coordinate" (exactRational 1 3)
  let center, right, branch, left :: DraftVertexId
      center = DraftId 0
      right = DraftId 1
      branch = DraftId 2
      left = DraftId 3
      centerRight, centerBranch, centerLeft :: DraftSegmentId
      centerRight = DraftId 0
      centerBranch = DraftId 1
      centerLeft = DraftId 2
      draft =
        emptyDraft
          { draftVertices =
              Map.fromList
                [ (center, exactPoint 1 oneThird)
                , (right, integerPoint 3 1)
                , (branch, integerPoint 1 2)
                , (left, integerPoint 0 0)
                ]
          , draftSegments =
              Map.fromList
                [ (centerRight, (center, right))
                , (centerBranch, (center, branch))
                , (centerLeft, (center, left))
                ]
          , draftIncidences =
              [ DraftIncidence centerRight centerBranch SegmentsShareEndpoint
              , DraftIncidence centerBranch centerLeft SegmentsShareEndpoint
              , DraftIncidence centerLeft centerRight SegmentsShareEndpoint
              ]
          , draftNeighborhoods =
              [DraftNeighborhood center (right :| [branch, left])]
          }
  certificate <-
    requireRight
      "collinear neighborhood retains its cyclic rotation"
      (certifyLocalEmbedding draft)
  assertEqual
    "collinear neighborhood rotation obligations"
    3
    (certificateNeighborhoodRotationPreservationCount certificate)

testExactArithmeticReceipt :: IO ()
testExactArithmeticReceipt = do
  enabled <- getRTSStatsEnabled
  unless enabled $
    fail "exact arithmetic allocation receipt requires +RTS -T"
  operands <-
    traverse
      ( \index ->
          requireRight
            "exact arithmetic receipt operand"
            (exactRational (toInteger (index `mod` 89 + 1)) 97)
      )
      [0 .. 9_999 :: Int]
  _ <- evaluate (force operands)
  performGC
  before <- allocated_bytes <$> getRTSStats
  checksum <-
    evaluate
      ( force
          ( List.foldl'
              ( \accumulated value ->
                  accumulated
                    + fromEnum
                      ( exactSignum
                          ((value + 1) * value)
                      )
              )
              0
              operands
          )
      )
  performGC
  after <- allocated_bytes <$> getRTSStats
  let operationCount = 3 * length operands
      allocated = after - before
  unless (checksum > 0) $
    fail "exact arithmetic receipt did not force its operation chain"
  putStrLn
    ( "exact arithmetic receipt: operations="
        <> show operationCount
        <> " allocated-bytes="
        <> show allocated
        <> " checksum="
        <> show checksum
    )

testEmbeddingAdmissionReceipt :: IO ()
testEmbeddingAdmissionReceipt = do
  let base = 2 ^ (52 :: Int)
  firstCollision <- requireRight "admission receipt collision a" (exactRational (base * 4 + 1) 4)
  secondCollision <- requireRight "admission receipt collision b" (exactRational (base * 4 + 2) 4)
  let admittedDraft =
        emptyDraft
          { draftVertices =
              Map.fromList
                [ (DraftId 0, integerPoint 0 0)
                , (DraftId 1, integerPoint 1 0)
                ]
          }
      collisionDraft =
        emptyDraft
          { draftVertices =
              Map.fromList
                [ (DraftId 0, exactPoint firstCollision 0)
                , (DraftId 1, exactPoint secondCollision 0)
                ]
          }
      projectionDraft =
        emptyDraft
          { draftVertices =
              Map.singleton
                (DraftId 0)
                (exactPoint (fromInteger (10 ^ (400 :: Int))) 0)
          }
      outcomes =
        map certifyLocalEmbedding [admittedDraft, collisionDraft, projectionDraft]
      admitted = length [() | Right _ <- outcomes]
      refused = length [() | Left _ <- outcomes]
  assertEqual "embedding admission receipt admissions" 1 admitted
  assertEqual "embedding admission receipt refusals" 2 refused
  putStrLn
    ( "embedding admission receipt: admitted="
        <> show admitted
        <> " refused="
        <> show refused
    )

testFrozenBinary64RelationOracle :: IO ()
testFrozenBinary64RelationOracle = do
  let observed =
        map
          ( \(name, expected, (a, b, c, d)) ->
              (name, expected, segmentRelation a b c d)
          )
          handFixtures
      relations =
        map (\(_, _, relation) -> relation) observed <> corpusRelations
  traverse_
    (\(name, expected, actual) -> assertEqual name expected actual)
    observed
  assertEqual "frozen relation count" 16_390 (length relations)
  assertEqual
    "frozen relation digest"
    1_170_735_657_727_369_596
    (digest relations)

data ObservedRelation = ObservedRelation
  { observedName :: !String
  , observedExpectedRelation :: !SegmentRelation
  , observedRoundedRelation :: !SegmentRelation
  , observedExactRelation :: !SegmentRelation
  }

observeExactAndRounded
  :: (String, SegmentRelation, (Point, Point, Point, Point))
  -> IO ObservedRelation
observeExactAndRounded (name, expected, points@(a, b, c, d)) = do
  (exactA, exactB, exactC, exactD) <- exactPointTuple points
  pure
    ObservedRelation
      { observedName = name
      , observedExpectedRelation = expected
      , observedRoundedRelation = segmentRelation a b c d
      , observedExactRelation = exactSegmentRelation exactA exactB exactC exactD
      }

assertExpectedAndAgreement :: ObservedRelation -> IO ()
assertExpectedAndAgreement observed = do
  assertEqual
    (observedName observed <> " binary64 expectation")
    (observedExpectedRelation observed)
    (observedRoundedRelation observed)
  assertEqual
    (observedName observed <> " exact agreement")
    (observedRoundedRelation observed)
    (observedExactRelation observed)

contactFixtures
  :: [(String, SegmentRelation, (Point, Point, Point, Point))]
contactFixtures =
  [ ( "endpoint-on-edge"
    , SegmentEndpointTouchesInterior
    , (Point 0 0, Point 2 0, Point 1 0, Point 1 1)
    )
  , ( "duplicate"
    , SegmentsDuplicate
    , (Point 0 0, Point 2 0, Point 2 0, Point 0 0)
    )
  , ( "partial-collinear-overlap"
    , SegmentsCollinearlyOverlap
    , (Point 0 0, Point 3 0, Point 1 0, Point 2 0)
    )
  ]

assertContactAgreement
  :: (String, SegmentRelation, (Point, Point, Point, Point))
  -> IO ()
assertContactAgreement fixture =
  observeExactAndRounded fixture >>= assertExpectedAndAgreement

exactPointTuple
  :: (Point, Point, Point, Point)
  -> IO (ExactPoint, ExactPoint, ExactPoint, ExactPoint)
exactPointTuple (a, b, c, d) =
  (,,,)
    <$> requireRight "exact fixture point a" (exactPointFromPoint a)
    <*> requireRight "exact fixture point b" (exactPointFromPoint b)
    <*> requireRight "exact fixture point c" (exactPointFromPoint c)
    <*> requireRight "exact fixture point d" (exactPointFromPoint d)

exactCrossingAt :: ExactRational -> IO ExactPoint
exactCrossingAt coordinate = do
  horizontal <-
    requireRight
      "sub-ulp horizontal crossing segment"
      ( exactSegment
          (exactPoint (coordinate - 1) coordinate)
          (exactPoint (coordinate + 1) coordinate)
      )
  vertical <-
    requireRight
      "sub-ulp vertical crossing segment"
      ( exactSegment
          (exactPoint coordinate (coordinate - 1))
          (exactPoint coordinate (coordinate + 1))
      )
  requireRight "sub-ulp exact crossing" (exactLineIntersection horizontal vertical)

assertRatio :: String -> Integer -> Integer -> ExactRational -> IO ()
assertRatio label numerator denominator value = do
  assertEqual (label <> " numerator") numerator (exactRationalNumerator value)
  assertEqual (label <> " denominator") denominator (exactRationalDenominator value)

emptyDraft :: ExactArrangementDraft
emptyDraft =
  ExactArrangementDraft
    { draftVertices = Map.empty
    , draftSegments = Map.empty
    , draftSourceMemberships = Map.empty
    , draftIncidences = []
    , draftNeighborhoods = []
    }

assertLeftContains
  :: String
  -> (OverlayEmbeddingObstruction -> Bool)
  -> Either (NonEmpty OverlayEmbeddingObstruction) value
  -> IO ()
assertLeftContains label predicate result =
  case result of
    Left obstructions ->
      unless (any predicate obstructions) $
        fail (label <> ": missing witness in " <> show obstructions)
    Right _ -> fail (label <> ": unexpectedly certified")

isRoundedCollision :: OverlayEmbeddingObstruction -> Bool
isRoundedCollision RoundedVerticesCollide {} = True
isRoundedCollision _ = False

isProjectionRefusal :: OverlayEmbeddingObstruction -> Bool
isProjectionRefusal VertexProjectionRefused {} = True
isProjectionRefusal _ = False

isIncidenceChange :: OverlayEmbeddingObstruction -> Bool
isIncidenceChange IncidenceRelationChanged {} = True
isIncidenceChange _ = False

handFixtures :: [(String, SegmentRelation, (Point, Point, Point, Point))]
handFixtures =
  [ ("disjoint", SegmentsDisjoint, (Point 0 0, Point 1 0, Point 0 2, Point 1 2))
  , ("duplicate", SegmentsDuplicate, (Point 0 0, Point 2 0, Point 2 0, Point 0 0))
  , ("shared-endpoint", SegmentsShareEndpoint, (Point 0 0, Point 2 0, Point 2 0, Point 3 1))
  , ("proper-crossing", SegmentsProperlyCross, (Point 0 0, Point 2 2, Point 0 2, Point 2 0))
  , ("endpoint-interior", SegmentEndpointTouchesInterior, (Point 0 0, Point 2 0, Point 1 0, Point 1 1))
  , ("collinear-overlap", SegmentsCollinearlyOverlap, (Point 0 0, Point 3 0, Point 1 0, Point 2 0))
  ]

corpusPoint :: Int -> Int -> Point
corpusPoint index salt =
  Point
    (fromIntegral (((index * 17 + salt * 11) `mod` 47) - 23))
    (fromIntegral (((index * 29 + salt * 7) `mod` 43) - 21))

corpusRelations :: [SegmentRelation]
corpusRelations =
  [ segmentRelation
      (corpusPoint index 1)
      (corpusPoint index 2)
      (corpusPoint index 3)
      (corpusPoint index 4)
  | index <- [0 .. 16_383]
  ]

digest :: [SegmentRelation] -> Word64
digest = List.foldl' step 14_695_981_039_346_656_037
 where
  step :: Word64 -> SegmentRelation -> Word64
  step hash relation =
    (hash `xor` fromIntegral (fromEnum relation + 1)) * 1_099_511_628_211
