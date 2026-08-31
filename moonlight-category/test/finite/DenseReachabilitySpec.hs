module DenseReachabilitySpec
  ( tests,
  )
where

import Data.Bits (bit, testBit, (.&.), (.|.))
import qualified Data.IntSet as IntSet
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Moonlight.Category.Pure.Finite.DenseReachability
  ( DenseClosure (..),
    denseReachabilityRows,
    denseReachabilityWithCycles,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)

tests :: TestTree
tests =
  testGroup
    "DenseReachability"
    (denseClosureCase <$> adversarialCases <> pseudoRandomCases)

data GraphCase = GraphCase String (Vector Integer)

adversarialCases :: [GraphCase]
adversarialCases =
  [ GraphCase "empty graph has no rows, cycles, or components" (edgeRows 0 []),
    GraphCase "one vertex without an edge stays acyclic" (edgeRows 1 []),
    GraphCase "singleton self-loop is reported as a cyclic component" (edgeRows 1 [(0, 0)]),
    GraphCase "two vertices reaching each other form one cyclic component" (edgeRows 2 [(0, 1), (1, 0)]),
    GraphCase "sixty-five vertex chain crosses bit-word boundaries without false cycles" (edgeRows 65 [(source, source + 1) | source <- [0 .. 63]]),
    GraphCase "forty vertex complete digraph closes to one cyclic component" (edgeRows 40 [(source, target) | source <- [0 .. 39], target <- [0 .. 39], source /= target]),
    GraphCase "stray bits above the vertex count are masked before closure" (Vector.fromList [bit 1 .|. bit 99, bit 2, bit 80]),
    GraphCase "two bridged cycles remain separate cyclic components" (edgeRows 6 [(0, 1), (1, 0), (1, 2), (2, 3), (3, 4), (4, 2), (4, 5)]),
    GraphCase "a singleton self-loop between larger components is preserved" (edgeRows 5 [(0, 1), (1, 0), (1, 2), (2, 2), (2, 3), (3, 4), (4, 3)])
  ]

pseudoRandomCases :: [GraphCase]
pseudoRandomCases =
  [ GraphCase ("deterministic pseudo-random graph seed " <> show seed <> " size " <> show vertexCount) (pseudoRandomRows vertexCount seed)
    | (vertexCount, seed) <- [(0, 17), (1, 19), (2, 23), (5, 29), (9, 31), (16, 37), (33, 41), (67, 43)]
  ]

denseClosureCase :: GraphCase -> TestTree
denseClosureCase (GraphCase caseName inputRows) =
  testCase caseName (assertDenseClosureMatchesReference caseName inputRows)

assertDenseClosureMatchesReference :: String -> Vector Integer -> Assertion
assertDenseClosureMatchesReference caseName inputRows = do
  let actualClosure = denseReachabilityWithCycles inputRows
      actualRows = denseClosureReachabilityRows actualClosure
      actualComponents = denseClosureCycleComponents actualClosure
      expectedRows = warshallRows inputRows
      expectedComponents = referenceCycleComponents inputRows
      expectedComponentCount = referenceComponentCount inputRows
  assertEqual (caseName <> ": denseReachabilityWithCycles rows match Warshall closure") expectedRows actualRows
  assertEqual (caseName <> ": denseReachabilityRows matches the same Warshall closure") expectedRows (denseReachabilityRows inputRows)
  assertEqual (caseName <> ": cycle components match mutual-reachability extraction") expectedComponents actualComponents
  assertEqual (caseName <> ": component count includes acyclic singleton SCCs") expectedComponentCount (denseClosureComponentCount actualClosure)
  assertEqual (caseName <> ": diagonal bits are exactly the reported cyclic vertices") (diagonalVertices actualRows) (componentVertices actualComponents)

warshallRows :: Vector Integer -> Vector Integer
warshallRows inputRows =
  List.foldl' closeOverPivot maskedRows [0 .. vertexCount - 1]
  where
    vertexCount :: Int
    vertexCount = Vector.length inputRows

    maskedRows :: Vector Integer
    maskedRows = Vector.map (.&. finiteBitMask vertexCount) inputRows

    closeOverPivot :: Vector Integer -> Int -> Vector Integer
    closeOverPivot rows pivot =
      Vector.imap
        (\_source row ->
           if testBit row pivot
             then row .|. rowAt rows pivot
             else row
        )
        rows

referenceCycleComponents :: Vector Integer -> [NonEmpty Int]
referenceCycleComponents inputRows =
  filterCyclic (warshallRows inputRows) (mutualReachabilityClasses inputRows)

referenceComponentCount :: Vector Integer -> Int
referenceComponentCount =
  length . mutualReachabilityClasses

mutualReachabilityClasses :: Vector Integer -> [NonEmpty Int]
mutualReachabilityClasses inputRows =
  buildClasses 0 IntSet.empty []
  where
    closureRows :: Vector Integer
    closureRows = warshallRows inputRows

    vertexCount :: Int
    vertexCount = Vector.length inputRows

    buildClasses :: Int -> IntSet.IntSet -> [NonEmpty Int] -> [NonEmpty Int]
    buildClasses candidate seen reversedClasses
      | candidate >= vertexCount = reverse reversedClasses
      | candidate `IntSet.member` seen = buildClasses (candidate + 1) seen reversedClasses
      | otherwise =
          let members = candidate : filter (mutuallyReachable candidate) [candidate + 1 .. vertexCount - 1]
              seenWithMembers = List.foldl' (flip IntSet.insert) seen members
           in case NonEmpty.nonEmpty members of
                Nothing -> buildClasses (candidate + 1) seenWithMembers reversedClasses
                Just component -> buildClasses (candidate + 1) seenWithMembers (component : reversedClasses)

    mutuallyReachable :: Int -> Int -> Bool
    mutuallyReachable left right =
      testBit (rowAt closureRows left) right && testBit (rowAt closureRows right) left

filterCyclic :: Vector Integer -> [NonEmpty Int] -> [NonEmpty Int]
filterCyclic closureRows =
  filter componentIsCyclic
  where
    componentIsCyclic :: NonEmpty Int -> Bool
    componentIsCyclic (single :| []) = testBit (rowAt closureRows single) single
    componentIsCyclic (_first :| _rest) = True

diagonalVertices :: Vector Integer -> [Int]
diagonalVertices rows =
  [vertex | vertex <- [0 .. Vector.length rows - 1], testBit (rowAt rows vertex) vertex]

componentVertices :: [NonEmpty Int] -> [Int]
componentVertices =
  List.sort . foldMap NonEmpty.toList

edgeRows :: Int -> [(Int, Int)] -> Vector Integer
edgeRows vertexCount edges =
  Vector.generate vertexCount rowForSource
  where
    rowForSource :: Int -> Integer
    rowForSource source =
      List.foldl' (addEdgeFrom source) 0 edges

    addEdgeFrom :: Int -> Integer -> (Int, Int) -> Integer
    addEdgeFrom source row (edgeSource, target)
      | source == edgeSource && 0 <= target && target < vertexCount = row .|. bit target
      | otherwise = row

pseudoRandomRows :: Int -> Integer -> Vector Integer
pseudoRandomRows vertexCount seed =
  Vector.generate vertexCount rowForSource
  where
    rowForSource :: Int -> Integer
    rowForSource source =
      List.foldl'
        (\row target ->
           if pseudoRandomEdge seed vertexCount source target
             then row .|. bit target
             else row
        )
        0
        [0 .. vertexCount - 1]

pseudoRandomEdge :: Integer -> Int -> Int -> Int -> Bool
pseudoRandomEdge seed vertexCount source target =
  lcg mixed `mod` 11 <= 2
  where
    mixed :: Integer
    mixed = seed + 97 * fromIntegral vertexCount + 104_729 * fromIntegral (source + 1) + 13_007 * fromIntegral (target + 1)

lcg :: Integer -> Integer
lcg value =
  (1_103_515_245 * value + 12_345) `mod` 2_147_483_647

rowAt :: Vector Integer -> Int -> Integer
rowAt rows index =
  maybe 0 id (rows Vector.!? index)

finiteBitMask :: Int -> Integer
finiteBitMask vertexCount
  | vertexCount <= 0 = 0
  | otherwise = bit vertexCount - 1
