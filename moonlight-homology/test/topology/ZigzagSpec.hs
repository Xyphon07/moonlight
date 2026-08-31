module ZigzagSpec (tests) where

import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Moonlight.Homology.Boundary
  ( BoundaryIncidence,
    BoundaryIncidenceShapeError,
    FiniteChainComplex,
    degreeCardinality,
    emptyBoundaryIncidence,
    emptyBoundaryIncidenceOf,
    mkBoundaryEntry,
    mkBoundaryIncidence,
  )
import Moonlight.Homology.Boundary.Finite (mkFiniteChainComplex)
import Moonlight.Homology.Chain (HomologicalDegree (..))
import Moonlight.Homology.Persistence
  ( FiniteChainMap,
    ZigzagArrow (..),
    ZigzagDirection (..),
    ZigzagFailure (..),
    ZigzagInterval (..),
    mkFiniteChainMapChecked,
    mkFiniteChainZigzag,
    rationalZigzagIntervals,
    zigzagBettiAt,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "exact zigzag persistence"
    [ testCase "decomposes a singleton vector space" singletonIntervals,
      testCase "preserves one class through a forward identity" forwardIdentityInterval,
      testCase "separates classes across a forward zero map" forwardZeroIntervals,
      testCase "separates classes across a backward zero map" backwardZeroIntervals,
      testCase "decomposes a non-monotone cospan" nonMonotoneCospanIntervals,
      testCase "pulls a flag backward through exact cancellation" backwardPullbackCancellation,
      testCase "recovers interval sums under every arrow orientation" everyDirectionIntervalSum,
      testCase "induces the identity on degree-one homology" degreeOneIdentityInterval,
      testCase "treats degrees above a complex maximum as zero" mixedMaximumDegrees,
      testCase "reconstructs every vertex Betti number" reconstructVertexBetti,
      testCase "rejects a component with the wrong source dimension" rejectMapShape,
      testCase "rejects a degree map that does not commute with boundaries" rejectChainMapLaw,
      testCase "rejects diagram endpoint mismatches" rejectEndpointMismatch
    ]

singletonIntervals :: Assertion
singletonIntervals = do
  diagram <- requireRight "singleton diagram" (mkFiniteChainZigzag (zeroComplex 2) [])
  intervals <- requireRight "singleton intervals" (rationalZigzagIntervals diagram)
  intervals @?= [ZigzagInterval (HomologicalDegree 0) 0 0 2]

forwardIdentityInterval :: Assertion
forwardIdentityInterval = do
  identityMap <- requireRight "identity map" (coordinateMap (zeroComplex 1) (zeroComplex 1) [(0, 0)])
  diagram <- requireRight "forward diagram" (mkFiniteChainZigzag (zeroComplex 1) [ForwardArrow identityMap])
  intervals <- requireRight "forward intervals" (rationalZigzagIntervals diagram)
  intervals @?= [ZigzagInterval (HomologicalDegree 0) 0 1 1]

forwardZeroIntervals :: Assertion
forwardZeroIntervals = do
  zeroMap <- requireRight "forward zero map" (coordinateMap (zeroComplex 1) (zeroComplex 1) [])
  diagram <- requireRight "forward zero diagram" (mkFiniteChainZigzag (zeroComplex 1) [ForwardArrow zeroMap])
  intervals <- requireRight "forward zero intervals" (rationalZigzagIntervals diagram)
  intervals
    @?= [ ZigzagInterval (HomologicalDegree 0) 0 0 1,
          ZigzagInterval (HomologicalDegree 0) 1 1 1
        ]

backwardZeroIntervals :: Assertion
backwardZeroIntervals = do
  zeroMap <- requireRight "backward zero map" (coordinateMap (zeroComplex 1) (zeroComplex 1) [])
  diagram <- requireRight "backward zero diagram" (mkFiniteChainZigzag (zeroComplex 1) [BackwardArrow zeroMap])
  intervals <- requireRight "backward zero intervals" (rationalZigzagIntervals diagram)
  intervals
    @?= [ ZigzagInterval (HomologicalDegree 0) 0 0 1,
          ZigzagInterval (HomologicalDegree 0) 1 1 1
        ]

nonMonotoneCospanIntervals :: Assertion
nonMonotoneCospanIntervals = do
  leftInclusion <- requireRight "left inclusion" (coordinateMap (zeroComplex 1) (zeroComplex 2) [(0, 0)])
  rightInclusion <- requireRight "right inclusion" (coordinateMap (zeroComplex 1) (zeroComplex 2) [(0, 1)])
  diagram <-
    requireRight
      "cospan diagram"
      ( mkFiniteChainZigzag
          (zeroComplex 1)
          [ForwardArrow leftInclusion, BackwardArrow rightInclusion]
      )
  intervals <- requireRight "cospan intervals" (rationalZigzagIntervals diagram)
  intervals
    @?= [ ZigzagInterval (HomologicalDegree 0) 0 1 1,
          ZigzagInterval (HomologicalDegree 0) 1 2 1
        ]

backwardPullbackCancellation :: Assertion
backwardPullbackCancellation = do
  firstMap <-
    requireRight
      "filtered inclusion"
      (coordinateMap (zeroComplex 1) (zeroComplex 2) [(0, 0)])
  cancellationMap <-
    requireRight
      "cancelling backward map"
      (coordinateMap (zeroComplex 2) (zeroComplex 2) [(0, 0), (0, 1), (1, 1)])
  diagram <-
    requireRight
      "backward cancellation diagram"
      ( mkFiniteChainZigzag
          (zeroComplex 1)
          [ForwardArrow firstMap, BackwardArrow cancellationMap]
      )
  intervals <- requireRight "backward cancellation intervals" (rationalZigzagIntervals diagram)
  intervals
    @?= [ ZigzagInterval (HomologicalDegree 0) 0 2 1,
          ZigzagInterval (HomologicalDegree 0) 1 2 1
        ]

everyDirectionIntervalSum :: Assertion
everyDirectionIntervalSum = do
  let intervalSeeds :: [(Int, Int, Int)]
      intervalSeeds =
        zipWith
          (\seedIndex (firstIndex, lastIndex) -> (seedIndex, firstIndex, lastIndex))
          [0 :: Int ..]
          [(0, 3), (0, 1), (1, 2), (2, 3), (1, 1), (1, 1)]
      stageSeeds stageIndex =
        filter
          (\(_, firstIndex, lastIndex) -> firstIndex <= stageIndex && stageIndex <= lastIndex)
          intervalSeeds
      stageComplex stageIndex = zeroComplex (length (stageSeeds stageIndex))
      coordinates sourceIndexValue targetIndexValue =
        [ (sourceCoordinate, targetCoordinate)
        | (sourceCoordinate, seed) <- zip [0 :: Int ..] (stageSeeds sourceIndexValue)
        , Just targetCoordinate <- [List.elemIndex seed (stageSeeds targetIndexValue)]
        ]
      arrowAt arrowIndex direction =
        let leftIndex = arrowIndex
            rightIndex = arrowIndex + 1
         in case direction of
              ZigzagForward ->
                ForwardArrow
                  <$> coordinateMap
                    (stageComplex leftIndex)
                    (stageComplex rightIndex)
                    (coordinates leftIndex rightIndex)
              ZigzagBackward ->
                BackwardArrow
                  <$> coordinateMap
                    (stageComplex rightIndex)
                    (stageComplex leftIndex)
                    (coordinates rightIndex leftIndex)
      expected =
        [ ZigzagInterval (HomologicalDegree 0) 0 1 1
        , ZigzagInterval (HomologicalDegree 0) 0 3 1
        , ZigzagInterval (HomologicalDegree 0) 1 1 2
        , ZigzagInterval (HomologicalDegree 0) 1 2 1
        , ZigzagInterval (HomologicalDegree 0) 2 3 1
        ]
      assertOrientation directions = do
        arrows <- requireRight "oriented interval-sum maps" (traverse (uncurry arrowAt) (zip [0 ..] directions))
        diagram <- requireRight "oriented interval-sum diagram" (mkFiniteChainZigzag (stageComplex 0) arrows)
        intervals <- requireRight "oriented interval-sum barcode" (rationalZigzagIntervals diagram)
        intervals @?= expected
  traverse_ assertOrientation (sequence (replicate 3 [ZigzagForward, ZigzagBackward]))

degreeOneIdentityInterval :: Assertion
degreeOneIdentityInterval = do
  complexValue <- requireRight "circle complex" circleComplex
  degreeZeroIdentity <- requireRight "circle vertex identity" (identityIncidence 3)
  degreeOneIdentity <- requireRight "circle edge identity" (identityIncidence 3)
  identityMap <-
    requireRight
      "circle chain identity"
      ( mkFiniteChainMapChecked complexValue complexValue $ \(HomologicalDegree degreeIndex) ->
          case degreeIndex of
            0 -> degreeZeroIdentity
            1 -> degreeOneIdentity
            _ -> emptyBoundaryIncidence
      )
  diagram <-
    requireRight
      "circle identity diagram"
      (mkFiniteChainZigzag complexValue [ForwardArrow identityMap])
  intervals <- requireRight "circle identity intervals" (rationalZigzagIntervals diagram)
  intervals
    @?= [ ZigzagInterval (HomologicalDegree 0) 0 1 1
        , ZigzagInterval (HomologicalDegree 1) 0 1 1
        ]

mixedMaximumDegrees :: Assertion
mixedMaximumDegrees = do
  targetComplex <- requireRight "mixed-maximum circle" circleComplex
  degreeZeroInclusion <-
    requireRight
      "mixed-maximum degree-zero inclusion"
      ( first (ZigzagMapComponentInvalid (HomologicalDegree 0))
          (mkBoundaryIncidence 1 3 [mkBoundaryEntry 0 0 (1 :: Int)])
      )
  let degreeOneInclusion :: BoundaryIncidence Int
      degreeOneInclusion = emptyBoundaryIncidenceOf 0 3
  inclusion <-
    requireRight
      "mixed-maximum chain inclusion"
      ( mkFiniteChainMapChecked poisonedPointComplex targetComplex $ \(HomologicalDegree degreeIndex) ->
          case degreeIndex of
            0 -> degreeZeroInclusion
            1 -> degreeOneInclusion
            _ -> emptyBoundaryIncidence
      )
  diagram <-
    requireRight
      "mixed-maximum diagram"
      (mkFiniteChainZigzag poisonedPointComplex [ForwardArrow inclusion])
  intervals <- requireRight "mixed-maximum intervals" (rationalZigzagIntervals diagram)
  intervals
    @?= [ ZigzagInterval (HomologicalDegree 0) 0 1 1
        , ZigzagInterval (HomologicalDegree 1) 1 1 1
        ]

reconstructVertexBetti :: Assertion
reconstructVertexBetti = do
  leftInclusion <- requireRight "shared left inclusion" (coordinateMap (zeroComplex 1) (zeroComplex 2) [(0, 0)])
  rightInclusion <- requireRight "shared right inclusion" (coordinateMap (zeroComplex 1) (zeroComplex 2) [(0, 0)])
  diagram <-
    requireRight
      "shared cospan diagram"
      ( mkFiniteChainZigzag
          (zeroComplex 1)
          [ForwardArrow leftInclusion, BackwardArrow rightInclusion]
      )
  intervals <- requireRight "shared cospan intervals" (rationalZigzagIntervals diagram)
  intervals
    @?= [ ZigzagInterval (HomologicalDegree 0) 0 2 1,
          ZigzagInterval (HomologicalDegree 0) 1 1 1
        ]
  fmap (`zigzagBettiAt` intervals) [0, 1, 2]
    @?= fmap (Map.singleton (HomologicalDegree 0)) [1, 2, 1]

rejectMapShape :: Assertion
rejectMapShape =
  case
      mkFiniteChainMapChecked
        (zeroComplex 1)
        (zeroComplex 1)
        (const (emptyBoundaryIncidenceOf 2 1))
    of
      Left (ZigzagMapSourceCardinalityMismatch (HomologicalDegree 0) 1 2) -> pure ()
      Left failure -> assertFailure ("wrong shape refusal: " <> show failure)
      Right _ -> assertFailure "malformed chain map was admitted"

rejectChainMapLaw :: Assertion
rejectChainMapLaw = do
  complexValue <- requireRight "interval complex" intervalComplex
  degreeZeroIdentity <- requireRight "degree-zero identity" (identityIncidence 2)
  case
      mkFiniteChainMapChecked
        complexValue
        complexValue
        ( \(HomologicalDegree degreeIndex) ->
            case degreeIndex of
              0 -> degreeZeroIdentity
              1 -> emptyBoundaryIncidenceOf 1 1
              _ -> emptyBoundaryIncidence
        )
    of
      Left (ZigzagChainMapLawViolation (HomologicalDegree 1)) -> pure ()
      Left failure -> assertFailure ("wrong chain-law refusal: " <> show failure)
      Right _ -> assertFailure "noncommuting chain map was admitted"

rejectEndpointMismatch :: Assertion
rejectEndpointMismatch = do
  identityMap <- requireRight "endpoint identity map" (coordinateMap (zeroComplex 1) (zeroComplex 1) [(0, 0)])
  case mkFiniteChainZigzag (zeroComplex 2) [ForwardArrow identityMap] of
    Left (ZigzagEndpointMismatch 0 ZigzagForward) -> pure ()
    Left failure -> assertFailure ("wrong endpoint refusal: " <> show failure)
    Right _ -> assertFailure "mismatched diagram endpoint was admitted"

zeroComplex :: Int -> FiniteChainComplex Int
zeroComplex dimension =
  mkFiniteChainComplex
    (HomologicalDegree 0)
    (const (emptyBoundaryIncidenceOf (fromIntegral dimension) 0))

poisonedPointComplex :: FiniteChainComplex Int
poisonedPointComplex =
  mkFiniteChainComplex
    (HomologicalDegree 0)
    ( \(HomologicalDegree degreeIndex) ->
        if degreeIndex == 0
          then emptyBoundaryIncidenceOf 1 0
          else emptyBoundaryIncidenceOf 7 6
    )

intervalComplex :: Either String (FiniteChainComplex Int)
intervalComplex = do
  degreeOneBoundary <-
    first show
      ( mkBoundaryIncidence
          1
          2
          [mkBoundaryEntry 0 0 (-1 :: Int), mkBoundaryEntry 0 1 1]
      )
  pure
    ( mkFiniteChainComplex
        (HomologicalDegree 1)
        ( \(HomologicalDegree degreeIndex) ->
            case degreeIndex of
              0 -> emptyBoundaryIncidenceOf 2 0
              1 -> degreeOneBoundary
              _ -> emptyBoundaryIncidence
        )
    )

circleComplex :: Either String (FiniteChainComplex Int)
circleComplex = do
  degreeOneBoundary <-
    first show
      ( mkBoundaryIncidence
          3
          3
          [ mkBoundaryEntry 0 0 (-1 :: Int)
          , mkBoundaryEntry 0 1 1
          , mkBoundaryEntry 1 1 (-1)
          , mkBoundaryEntry 1 2 1
          , mkBoundaryEntry 2 2 (-1)
          , mkBoundaryEntry 2 0 1
          ]
      )
  pure
    ( mkFiniteChainComplex
        (HomologicalDegree 1)
        ( \(HomologicalDegree degreeIndex) ->
            case degreeIndex of
              0 -> emptyBoundaryIncidenceOf 3 0
              1 -> degreeOneBoundary
              _ -> emptyBoundaryIncidence
        )
    )

coordinateMap ::
  FiniteChainComplex Int ->
  FiniteChainComplex Int ->
  [(Int, Int)] ->
  Either ZigzagFailure (FiniteChainMap Int)
coordinateMap sourceComplex targetComplex coordinates = do
  degreeZeroMap <-
    first (ZigzagMapComponentInvalid (HomologicalDegree 0))
      ( mkBoundaryIncidence
          (fromIntegral (complexZeroDimension sourceComplex))
          (fromIntegral (complexZeroDimension targetComplex))
          ( fmap
              ( \(sourceIndexValue, targetIndexValue) ->
                  mkBoundaryEntry
                    (fromIntegral sourceIndexValue)
                    (fromIntegral targetIndexValue)
                    (1 :: Int)
              )
              coordinates
          )
      )
  mkFiniteChainMapChecked sourceComplex targetComplex $ \(HomologicalDegree degreeIndex) ->
    case degreeIndex of
      0 -> degreeZeroMap
      _ -> emptyBoundaryIncidence

identityIncidence :: Int -> Either BoundaryIncidenceShapeError (BoundaryIncidence Int)
identityIncidence dimension =
  mkBoundaryIncidence
    (fromIntegral dimension)
    (fromIntegral dimension)
    ( fmap
        (\indexValue -> mkBoundaryEntry (fromIntegral indexValue) (fromIntegral indexValue) (1 :: Int))
        [0 .. dimension - 1]
    )

complexZeroDimension :: FiniteChainComplex r -> Int
complexZeroDimension complexValue = degreeCardinality complexValue (HomologicalDegree 0)

requireRight :: Show failure => String -> Either failure value -> IO value
requireRight context =
  either (assertFailure . ((context <> ": ") <>) . show) pure
