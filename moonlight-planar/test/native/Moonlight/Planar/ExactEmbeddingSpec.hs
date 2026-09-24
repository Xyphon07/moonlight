{-# LANGUAGE NumericUnderscores #-}

-- | Focused exact-geometry and local-embedding milestone acceptance.
module Moonlight.Planar.ExactEmbeddingSpec (tests) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (unless)
import Data.Bits (shiftR, xor)
import Data.Foldable (traverse_)
import Data.List (sort)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import GHC.Stats (allocated_bytes, getRTSStats, getRTSStatsEnabled)
import Moonlight.Planar.Exact
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
import Moonlight.Planar.Internal.ExactRational
  ( ExactArithmeticError (..)
  , ExactRational
  , exactDivide
  , exactRational
  , exactRationalDenominator
  , exactRationalFromDouble
  , exactRationalNumerator
  , exactSignum
  )
import Moonlight.Planar.Embedding
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
  , certificateDistinctRoundedVertexCount
  , certificateRoundedVertices
  , certificateSplitOrderPreservationCount
  , OverlayEmbeddingObstruction (..)
  , certifyLocalEmbedding
  , residualUndischargedObligations
  )
import Moonlight.Planar.Internal.Predicates (segmentRelation)
import Moonlight.Planar.Math
  ( SegmentRelation (..)
  , allSegmentRelations
  )
import Moonlight.Planar.Scalar (CoordinateError (..))
import Moonlight.Planar.Point (Point (..), PointValidationError (..), queryPointValue)
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
    , testContactFamilyOracle
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
    "ordinary distinct rounded vertices"
    5
    (certificateDistinctRoundedVertexCount certificate)
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
          ( certificateDistinctRoundedVertexCount certificate
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

-- | The frozen corpus above holds only disjoint pairs and proper crossings;
-- these families are the contact relations it never reaches. Every pair is
-- built on one integer line through a base point with an integer direction,
-- its endpoints at integer stations along that line and, where a point must
-- leave the line, displaced by a nonzero multiple of the perpendicular. All
-- coordinates stay below 2^10, so every orientation determinant is exact in
-- binary64 and the derived class is the class, with nothing left to
-- rounding. The relation is symmetric under reversing either segment and
-- under swapping the pair, so each draw also flips those three coins.
testContactFamilyOracle :: IO ()
testContactFamilyOracle = do
  relations <-
    concat <$> traverse (uncurry assertContactFamily) (zip [0 ..] contactFamilies)
  assertEqual
    "contact family relation count"
    (contactFamilySize * length contactFamilies)
    (length relations)
  -- FNV of the derived class sequence, computed independently of this
  -- binary from the family definitions alone (contact-families-replica.py in
  -- the campaign receipts): eight runs of 512.
  assertEqual
    "contact family relation digest"
    18_184_234_789_390_492_453
    (digest relations)

data ContactFamily = ContactFamily
  { contactFamilyName :: !String
  , contactFamilyExpected :: !SegmentRelation
  , contactFamilyPair :: ContactDraw -> (Point, Point, Point, Point)
  }

contactFamilySize :: Int
contactFamilySize = 512

-- | Stations are ordered t0 < t1 < t2 < t3 along the line, so "between" and
-- "beyond" below are exact statements about the parameter, and lexicographic
-- point order along one line is monotone in the parameter, which is what the
-- classifier's overlap window reads.
contactFamilies :: [ContactFamily]
contactFamilies =
  [ -- C is A itself; D leaves the line, so the segments meet in exactly the
    -- one shared endpoint and nothing is collinear.
    ContactFamily "shared-endpoint-non-collinear" SegmentsShareEndpoint $ \draw ->
      let (t0, t1, t2, _) = drawStations draw
       in (station draw t0, station draw t1, station draw t0, offLine draw t2)
  , -- Collinear, C is B, D beyond: the overlap window is the single point B,
    -- an endpoint of both, so a straight continuation is a shared endpoint.
    ContactFamily "straight-continuation" SegmentsShareEndpoint $ \draw ->
      let (t0, t1, t2, _) = drawStations draw
       in (station draw t0, station draw t1, station draw t1, station draw t2)
  , -- Collinear, C is A, and D lies strictly between A and B (or strictly
    -- beyond B, on the fourth coin): the overlap window has positive length,
    -- so the shared endpoint does not demote it below an overlap.
    ContactFamily "collinear-overlap-shared-endpoint" SegmentsCollinearlyOverlap $ \draw ->
      let (t0, t1, t2, _) = drawStations draw
       in if drawCoin draw 3
            then (station draw t0, station draw t1, station draw t0, station draw t2)
            else (station draw t0, station draw t2, station draw t0, station draw t1)
  , -- Collinear and staggered: A < C < B < D, overlap window [C, B] of
    -- positive length, no endpoint shared.
    ContactFamily "collinear-overlap-staggered" SegmentsCollinearlyOverlap $ \draw ->
      let (t0, t1, t2, t3) = drawStations draw
       in (station draw t0, station draw t2, station draw t1, station draw t3)
  , -- Collinear, CD strictly inside AB: overlap window [C, D], no endpoint
    -- shared.
    ContactFamily "collinear-containment" SegmentsCollinearlyOverlap $ \draw ->
      let (t0, t1, t2, t3) = drawStations draw
       in (station draw t0, station draw t3, station draw t1, station draw t2)
  , -- C sits strictly between A and B on the line and D leaves the line: C is
    -- on AB's interior, no endpoint is shared, and with D off the line the
    -- orientations of C and D about AB are EQ and nonzero, never opposite.
    ContactFamily "endpoint-touches-interior" SegmentEndpointTouchesInterior $ \draw ->
      let (t0, t1, t2, _) = drawStations draw
       in (station draw t0, station draw t2, station draw t1, offLine draw t1)
  , -- The same segment twice; the orientation coins cover both directions.
    ContactFamily "duplicate" SegmentsDuplicate $ \draw ->
      let (t0, t1, _, _) = drawStations draw
       in (station draw t0, station draw t1, station draw t0, station draw t1)
  , -- Collinear with a gap: B < C strictly, so the overlap window is empty.
    ContactFamily "collinear-disjoint" SegmentsDisjoint $ \draw ->
      let (t0, t1, t2, t3) = drawStations draw
       in (station draw t0, station draw t1, station draw t2, station draw t3)
  ]

assertContactFamily :: Int -> ContactFamily -> IO [SegmentRelation]
assertContactFamily familyIndex family = do
  let pairs =
        [ orientPair draw (contactFamilyPair family draw)
        | index <- [0 .. contactFamilySize - 1]
        , let draw = contactDraw familyIndex index
        ]
      rounded = [segmentRelation a b c d | (a, b, c, d) <- pairs]
  exact <- traverse exactPointTuple pairs
  let exactRelations = [exactSegmentRelation a b c d | (a, b, c, d) <- exact]
      expected = contactFamilyExpected family
      disagreements = filter ((/= expected) . snd) . zip [0 :: Int ..]
  assertEqual (name <> " pair count") contactFamilySize (length pairs)
  assertEqual (name <> " binary64 disagreements") [] (disagreements rounded)
  assertEqual (name <> " exact disagreements") [] (disagreements exactRelations)
  pure rounded
 where
  name = contactFamilyName family

data ContactDraw = ContactDraw
  { drawBase :: !(Int, Int)
  , drawDirection :: !(Int, Int)
  , drawOffset :: !Int
  , drawStations :: !(Int, Int, Int, Int)
  , drawCoins :: !Word64
  }

-- | Base in [-64, 64]^2, direction in [-8, 8]^2 without the origin, offset in
-- [-3, 3] without zero, four strictly increasing stations from [-4, 4] with
-- gaps in [1, 4], and four coins; all read from one SplitMix64 mix of the
-- family and index.
contactDraw :: Int -> Int -> ContactDraw
contactDraw family index =
  ContactDraw
    { drawBase = (field 0 129 - 64, field 7 129 - 64)
    , drawDirection = direction
    , drawOffset = if offsetField < 3 then offsetField - 3 else offsetField - 2
    , drawStations = (t0, t1, t2, t3)
    , drawCoins = seed `shiftR` 44
    }
 where
  seed = splitMix64 (fromIntegral (family * 1_048_576 + index))
  field :: Int -> Word64 -> Int
  field offset modulus = fromIntegral ((seed `shiftR` offset) `mod` modulus)
  direction =
    case (field 14 17 - 8, field 18 17 - 8) of
      (0, 0) -> (1, 0)
      nonzero -> nonzero
  offsetField = field 22 6
  t0 = field 25 9 - 4
  t1 = t0 + field 29 4 + 1
  t2 = t1 + field 31 4 + 1
  t3 = t2 + field 33 4 + 1

drawCoin :: ContactDraw -> Int -> Bool
drawCoin draw bit = (drawCoins draw `shiftR` bit) `mod` 2 == 1

station :: ContactDraw -> Int -> Point
station draw parameter =
  let (baseX, baseY) = drawBase draw
      (directionX, directionY) = drawDirection draw
   in Point
        (fromIntegral (baseX + parameter * directionX))
        (fromIntegral (baseY + parameter * directionY))

-- | The station displaced off the line by the offset times the perpendicular
-- of the direction; nonzero offset and nonzero direction keep it off.
offLine :: ContactDraw -> Int -> Point
offLine draw parameter =
  let (baseX, baseY) = drawBase draw
      (directionX, directionY) = drawDirection draw
      offset = drawOffset draw
   in Point
        (fromIntegral (baseX + parameter * directionX - offset * directionY))
        (fromIntegral (baseY + parameter * directionY + offset * directionX))

orientPair :: ContactDraw -> (Point, Point, Point, Point) -> (Point, Point, Point, Point)
orientPair draw (a, b, c, d) =
  let (a', b') = if drawCoin draw 0 then (b, a) else (a, b)
      (c', d') = if drawCoin draw 1 then (d, c) else (c, d)
   in if drawCoin draw 2 then (c', d', a', b') else (a', b', c', d')

splitMix64 :: Word64 -> Word64
splitMix64 input =
  let z0 = input + 0x9E37_79B9_7F4A_7C15
      z1 = (z0 `xor` (z0 `shiftR` 30)) * 0xBF58_476D_1CE4_E5B9
      z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94D0_49BB_1331_11EB
   in z2 `xor` (z2 `shiftR` 31)
