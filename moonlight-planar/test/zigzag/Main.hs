module Main (main) where

import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Moonlight.Homology.Chain (HomologicalDegree (..))
import Moonlight.Triangulation.Example.ActivationZigzag
  ( squareLoopAcrossDepths
  )
import Moonlight.Triangulation.Types
  ( Point (..)
  , RadiusSquared
  , mkRadiusSquared
  )
import Moonlight.Triangulation.Zigzag
  ( ActivationComplex
  , ActivationInterval
  , ActivationPoint (..)
  , ActivationSlice (..)
  , ActivationStage (..)
  , ActivationZigzagError (..)
  , ZigzagInterval (..)
  , activationAlphaPersistence
  , activationAlphaZigzag
  , activationComplexCells
  , activationZigzagComplexes
  , activationZigzagCorrespondences
  , activationZigzagIntervals
  , activationZigzagStages
  , adjacentCommonCells
  , adjacentUnionComplex
  )
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit
  ( Assertion
  , assertFailure
  , testCase
  , (@?=)
  )

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "activation alpha zigzag"
    [ testCase "tracks a loop born and filled across depth" loopAcrossDepths
    , testCase "compiled documentation example returns the same loop" compiledExample
    , testCase "accepts genuinely non-nested adjacent complexes" nonNestedCorrespondence
    , testCase "is invariant under point input permutation" permutationInvariant
    , testCase "retains exact common and union cell witnesses" correspondenceCells
    , testCase "rejects duplicate depth keys" rejectDuplicateDepths
    , testCase "rejects duplicate labels within a depth" rejectDuplicateLabels
    , testCase "rejects distinct labels collapsed onto one coordinate" rejectCoordinateCollapse
    ]

loopAcrossDepths :: Assertion
loopAcrossDepths = do
  slices <- squareSlices squarePoints
  intervals <- requireRight "square loop barcode" (activationAlphaPersistence slices)
  degreeOneIntervals intervals
    @?= [ ZigzagInterval
            { zigzagIntervalDegree = HomologicalDegree 1
            , zigzagIntervalFirst = AdjacentDepthUnion 0 1
            , zigzagIntervalLast = ObservedDepth 1
            , zigzagIntervalMultiplicity = 1
            }
        ]

compiledExample :: Assertion
compiledExample = do
  intervals <- requireRight "compiled activation example" squareLoopAcrossDepths
  fmap zigzagIntervalMultiplicity intervals @?= [1]

nonNestedCorrespondence :: Assertion
nonNestedCorrespondence = do
  threshold <- requireRadius 10
  let slices :: NonEmpty (ActivationSlice Int String)
      slices =
        ActivationSlice
          0
          threshold
          ( Vector.fromList
              [ ActivationPoint "a" (Point 0 0)
              , ActivationPoint "b" (Point 2 0)
              , ActivationPoint "c" (Point 0 2)
              ]
          )
          :| [ ActivationSlice
                 1
                 threshold
                 ( Vector.fromList
                     [ ActivationPoint "b" (Point 2 0)
                     , ActivationPoint "c" (Point 0 2)
                     , ActivationPoint "d" (Point 2 2)
                     ]
                 )
             ]
  zigzag <-
    requireRight
      "non-nested triangle zigzag"
      (activationAlphaZigzag slices)
  intervals <- requireRight "non-nested triangle barcode" (activationZigzagIntervals zigzag)
  filter ((== HomologicalDegree 0) . zigzagIntervalDegree) intervals
    @?= [ZigzagInterval (HomologicalDegree 0) (ObservedDepth 0) (ObservedDepth 1) 1]

permutationInvariant :: Assertion
permutationInvariant = do
  forwardSlices <- squareSlices squarePoints
  reverseSlices <- squareSlices (Vector.reverse squarePoints)
  forwardIntervals <- requireRight "forward square barcode" (activationAlphaPersistence forwardSlices)
  reverseIntervals <- requireRight "reversed square barcode" (activationAlphaPersistence reverseSlices)
  reverseIntervals @?= forwardIntervals

correspondenceCells :: Assertion
correspondenceCells = do
  slices <- squareSlices squarePoints
  zigzag <- requireRight "square correspondence" (activationAlphaZigzag slices)
  activationZigzagStages zigzag
    @?= Vector.fromList
      [ ObservedDepth 0
      , AdjacentDepthUnion 0 1
      , ObservedDepth 1
      , AdjacentDepthUnion 1 2
      , ObservedDepth 2
      ]
  let complexes = NonEmpty.toList (activationZigzagComplexes zigzag)
      witnesses = Vector.toList (activationZigzagCorrespondences zigzag)
  length witnesses @?= 2
  traverse_
    ( \(leftComplex, rightComplex, witness) -> do
        adjacentCommonCells witness
          @?= Set.intersection
            (activationComplexCells leftComplex)
            (activationComplexCells rightComplex)
        activationComplexCells (adjacentUnionComplex witness)
          @?= Set.union
            (activationComplexCells leftComplex)
            (activationComplexCells rightComplex)
    )
    (adjacentTriples complexes witnesses)

rejectDuplicateDepths :: Assertion
rejectDuplicateDepths = do
  threshold <- requireRadius 0
  let slices :: NonEmpty (ActivationSlice Int String)
      slices =
        ActivationSlice 4 threshold (Vector.singleton (ActivationPoint "a" (Point 0 0)))
          :| [ActivationSlice 4 threshold (Vector.singleton (ActivationPoint "a" (Point 1 0)))]
  activationAlphaPersistence slices @?= Left (ActivationDuplicateDepth 4)

rejectDuplicateLabels :: Assertion
rejectDuplicateLabels = do
  threshold <- requireRadius 0
  let result :: Either (ActivationZigzagError Int String) [ActivationInterval Int]
      result =
        activationAlphaPersistence
          ( ActivationSlice
              7
              threshold
              ( Vector.fromList
                  [ ActivationPoint "same" (Point 0 0)
                  , ActivationPoint "same" (Point 1 0)
                  ]
              )
              :| []
          )
  result @?= Left (ActivationDuplicateLabel 7 "same")

rejectCoordinateCollapse :: Assertion
rejectCoordinateCollapse = do
  threshold <- requireRadius 0
  let result :: Either (ActivationZigzagError Int String) [ActivationInterval Int]
      result =
        activationAlphaPersistence
          ( ActivationSlice
              9
              threshold
              ( Vector.fromList
                  [ ActivationPoint "first" (Point 0 0)
                  , ActivationPoint "second" (Point 0 0)
                  ]
              )
              :| []
          )
  result @?= Left (ActivationCoordinateCollapse 9 2 1)

degreeOneIntervals :: [ActivationInterval depth] -> [ActivationInterval depth]
degreeOneIntervals =
  filter ((== HomologicalDegree 1) . zigzagIntervalDegree)

squareSlices ::
  Vector.Vector (ActivationPoint String) ->
  IO (NonEmpty (ActivationSlice Int String))
squareSlices points = do
  let depths :: NonEmpty Int
      depths = 0 :| [1, 2]
  thresholds <- traverse (requireRadius . fromIntegral) depths
  pure
    ( NonEmpty.zipWith
        (\depthValue threshold -> ActivationSlice depthValue threshold points)
        depths
        thresholds
    )

squarePoints :: Vector.Vector (ActivationPoint String)
squarePoints =
  Vector.fromList
    [ ActivationPoint "south-west" (Point (-1) (-1))
    , ActivationPoint "south-east" (Point 1 (-1))
    , ActivationPoint "north-east" (Point 1 1)
    , ActivationPoint "north-west" (Point (-1) 1)
    ]

adjacentTriples ::
  [ActivationComplex depth label] ->
  [witness] ->
  [(ActivationComplex depth label, ActivationComplex depth label, witness)]
adjacentTriples complexes witnesses =
  zipWith3 (,,) complexes (drop 1 complexes) witnesses

requireRadius :: Double -> IO RadiusSquared
requireRadius value =
  requireRight "radius squared" (first show (mkRadiusSquared value))

requireRight :: Show failure => String -> Either failure value -> IO value
requireRight context =
  either (assertFailure . ((context <> ": ") <>) . show) pure
