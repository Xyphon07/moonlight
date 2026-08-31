module AbstractBench
  ( abstractBenchmarks,
  )
where

import Control.DeepSeq (NFData (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Vector.Unboxed qualified as UVector
import Data.Word (Word64)
import Moonlight.Algebra.Pure.Lattice qualified as Lattice
import Moonlight.Algebra.Pure.LaneVector
  ( LaneVector,
    laneCount,
    laneVectorFromLanes,
    laneVectorLanes,
  )
import Moonlight.Algebra.Pure.Polynomial (Polynomial)
import Moonlight.Algebra.Pure.Polynomial qualified as Polynomial
import Moonlight.Algebra.Pure.PowerSet qualified as PowerSet
import Moonlight.Algebra.Pure.SparseVec (SparseVec)
import Moonlight.Algebra.Pure.SparseVec qualified as SparseVec
import Moonlight.Core
  ( AdditiveGroup (..),
    AdditiveMonoid (..),
    MultiplicativeMonoid (..),
  )
import Test.Tasty.Bench
  ( Benchmark,
    bench,
    bgroup,
    env,
    nf,
  )

type SparseBenchVector = SparseVec Int Int

type PolynomialBench = Polynomial Int

abstractBenchmarks :: Benchmark
abstractBenchmarks =
  bgroup
    "abstract"
    [ laneVectorBenchmarks,
      sparseVecWorldBenchmarks,
      polynomialWorldBenchmarks,
      powerSetWorldBenchmarks
    ]

newtype LaneVectorBenchOperands = LaneVectorBenchOperands [(LaneVector, LaneVector)]

instance NFData LaneVectorBenchOperands where
  rnf (LaneVectorBenchOperands operands) =
    foldr forceLaneVectorPair () operands

laneVectorBenchmarks :: Benchmark
laneVectorBenchmarks =
  bgroup
    "lane-vector/group-arithmetic"
    (fmap laneVectorBenchmark laneVectorEditCounts)

laneVectorBenchmark :: Int -> Benchmark
laneVectorBenchmark editCount =
  env (pure (laneVectorOperands editCount)) $ \operands ->
    bgroup
      ("edits=" <> show editCount <> " lanes-per-edit=" <> show laneCount)
      [ bench "add" (nf laneVectorAddWeight operands),
        bench "sub" (nf laneVectorSubWeight operands),
        bench "neg" (nf laneVectorNegWeight operands)
      ]

laneVectorOperands :: Int -> LaneVectorBenchOperands
laneVectorOperands editCount =
  LaneVectorBenchOperands (fmap laneVectorOperandPair (keys editCount))

laneVectorOperandPair :: Int -> (LaneVector, LaneVector)
laneVectorOperandPair editIndex =
  ( laneVectorOperand 0x243f6a8885a308d3 editIndex,
    laneVectorOperand 0x13198a2e03707344 editIndex
  )

laneVectorOperand :: Word64 -> Int -> LaneVector
laneVectorOperand seed editIndex =
  laneVectorFromLanes
    (UVector.generate laneCount (laneValue seed editIndex))

laneValue :: Word64 -> Int -> Int -> Word64
laneValue seed editIndex laneIndex =
  seed
    + 0x9e3779b97f4a7c15 * fromIntegral editIndex
    + 0xbf58476d1ce4e5b9 * fromIntegral laneIndex

laneVectorAddWeight :: LaneVectorBenchOperands -> Word64
laneVectorAddWeight (LaneVectorBenchOperands operands) =
  foldr (combineLaneVectorPair add) 0 operands

laneVectorSubWeight :: LaneVectorBenchOperands -> Word64
laneVectorSubWeight (LaneVectorBenchOperands operands) =
  foldr (combineLaneVectorPair sub) 0 operands

laneVectorNegWeight :: LaneVectorBenchOperands -> Word64
laneVectorNegWeight (LaneVectorBenchOperands operands) =
  foldr (\(left, _right) accumulated -> laneVectorWeight (neg left) + accumulated) 0 operands

combineLaneVectorPair :: (LaneVector -> LaneVector -> LaneVector) -> (LaneVector, LaneVector) -> Word64 -> Word64
combineLaneVectorPair operation (left, right) accumulated =
  laneVectorWeight (operation left right) + accumulated

laneVectorWeight :: LaneVector -> Word64
laneVectorWeight =
  UVector.foldl' (+) 0 . laneVectorLanes

forceLaneVectorPair :: (LaneVector, LaneVector) -> () -> ()
forceLaneVectorPair (left, right) forcedRest =
  rnf (laneVectorLanes left) `seq` rnf (laneVectorLanes right) `seq` forcedRest

laneVectorEditCounts :: [Int]
laneVectorEditCounts =
  [128, 512, 2048]

sparseVecWorldBenchmarks :: Benchmark
sparseVecWorldBenchmarks =
  bgroup
    "sparse-vec/us-vs-world"
    (fmap sparseVecWorldBenchmark sparseVecSizes)

sparseVecWorldBenchmark :: Int -> Benchmark
sparseVecWorldBenchmark size =
  env (pure (sparseEntryPair size)) $ \entryPair ->
    bgroup
      (caseLabel "normalize+add" size)
      [ bench "moonlight: SparseVec.fromEntries/add" (nf moonlightSparseVecAddWeight entryPair),
        bench "world: containers Map.fromListWith/unionWith" (nf containersMapAddWeight entryPair)
      ]

moonlightSparseVecAddWeight :: ([(Int, Int)], [(Int, Int)]) -> Int
moonlightSparseVecAddWeight (leftEntries, rightEntries) =
  sparseEntriesWeight
    ( SparseVec.toEntries
        ( add
            (SparseVec.fromEntries leftEntries :: SparseBenchVector)
            (SparseVec.fromEntries rightEntries :: SparseBenchVector)
        )
    )

containersMapAddWeight :: ([(Int, Int)], [(Int, Int)]) -> Int
containersMapAddWeight (leftEntries, rightEntries) =
  mapEntriesWeight
    ( normalizeMap
        (Map.unionWith (+) (containersMapFromEntries leftEntries) (containersMapFromEntries rightEntries))
    )

containersMapFromEntries :: [(Int, Int)] -> Map Int Int
containersMapFromEntries =
  normalizeMap . Map.fromListWith (+)

normalizeMap :: Map Int Int -> Map Int Int
normalizeMap =
  Map.mapMaybe nonZero

nonZero :: Int -> Maybe Int
nonZero value
  | value == 0 = Nothing
  | otherwise = Just value

polynomialWorldBenchmarks :: Benchmark
polynomialWorldBenchmarks =
  bgroup
    "polynomial/us-vs-world"
    (fmap polynomialWorldBenchmark polynomialSizes)

polynomialWorldBenchmark :: Int -> Benchmark
polynomialWorldBenchmark size =
  env (pure (polynomialCoefficientPair size)) $ \coefficientPair ->
    bgroup
      (caseLabel "multiply+evaluate" size)
      [ bench "moonlight: Polynomial.mul/evaluatePolynomial" (nf moonlightPolynomialWeight coefficientPair),
        bench "world: containers Map convolution/Horner" (nf containersPolynomialWeight coefficientPair)
      ]

moonlightPolynomialWeight :: ([Int], [Int]) -> Int
moonlightPolynomialWeight (leftCoefficients, rightCoefficients) =
  let productPolynomial =
        mul
          (Polynomial.fromCoefficients leftCoefficients :: PolynomialBench)
          (Polynomial.fromCoefficients rightCoefficients :: PolynomialBench)
   in Polynomial.evaluatePolynomial 3 productPolynomial
        + coefficientsWeight (Polynomial.toCoefficients productPolynomial)

containersPolynomialWeight :: ([Int], [Int]) -> Int
containersPolynomialWeight (leftCoefficients, rightCoefficients) =
  let productTerms =
        containersPolynomialMultiply
          (polynomialMapFromCoefficients leftCoefficients)
          (polynomialMapFromCoefficients rightCoefficients)
   in denseHornerEvaluate 3 (coefficientsFromPolynomialMap productTerms)
        + mapEntriesWeight productTerms

containersPolynomialMultiply :: Map Int Int -> Map Int Int -> Map Int Int
containersPolynomialMultiply leftTerms rightTerms =
  normalizeMap
    ( Map.fromListWith
        (+)
        [ (leftDegree + rightDegree, leftCoefficient * rightCoefficient)
        | (leftDegree, leftCoefficient) <- Map.toAscList leftTerms,
          (rightDegree, rightCoefficient) <- Map.toAscList rightTerms
        ]
    )

polynomialMapFromCoefficients :: [Int] -> Map Int Int
polynomialMapFromCoefficients =
  normalizeMap . Map.fromAscList . zip [0 ..]

coefficientsFromPolynomialMap :: Map Int Int -> [Int]
coefficientsFromPolynomialMap =
  denseCoefficientsFromTerms 0 . Map.toAscList

denseCoefficientsFromTerms :: Int -> [(Int, Int)] -> [Int]
denseCoefficientsFromTerms expectedDegree remainingTerms =
  case remainingTerms of
    [] -> []
    (degreeValue, coefficientValue) : restTerms
      | expectedDegree < degreeValue ->
          replicate (degreeValue - expectedDegree) zero
            <> denseCoefficientsFromTerms degreeValue remainingTerms
      | otherwise ->
          coefficientValue : denseCoefficientsFromTerms (expectedDegree + 1) restTerms

denseHornerEvaluate :: Int -> [Int] -> Int
denseHornerEvaluate value =
  foldr (\coefficient accumulator -> coefficient + value * accumulator) 0

powerSetWorldBenchmarks :: Benchmark
powerSetWorldBenchmarks =
  bgroup
    "power-set-lattice/us-vs-world"
    (fmap powerSetWorldBenchmark powerSetSizes)

powerSetWorldBenchmark :: Int -> Benchmark
powerSetWorldBenchmark size =
  env (pure (powerSetListPair size)) $ \setPair ->
    bgroup
      (caseLabel "join+meet" size)
      [ bench "moonlight: PowerSet join/meet" (nf moonlightPowerSetJoinMeetWeight setPair),
        bench "world: containers Set union/intersection" (nf containersSetJoinMeetWeight setPair)
      ]

moonlightPowerSetJoinMeetWeight :: ([Int], [Int]) -> Int
moonlightPowerSetJoinMeetWeight (leftValues, rightValues) =
  let leftSet = PowerSet.fromList leftValues
      rightSet = PowerSet.fromList rightValues
      joinedSet = Lattice.join leftSet rightSet
      metSet = Lattice.meet leftSet rightSet
   in listWeight (PowerSet.toPowerSetList joinedSet)
        + listWeight (PowerSet.toPowerSetList metSet)

containersSetJoinMeetWeight :: ([Int], [Int]) -> Int
containersSetJoinMeetWeight (leftValues, rightValues) =
  let leftSet = Set.fromList leftValues
      rightSet = Set.fromList rightValues
   in setWeight (Set.union leftSet rightSet)
        + setWeight (Set.intersection leftSet rightSet)

sparseEntryPair :: Int -> ([(Int, Int)], [(Int, Int)])
sparseEntryPair size =
  ( sparseEntries size,
    fmap (\(basisKey, coefficient) -> (basisKey + size `quot` 4, negate coefficient)) (sparseEntries size)
  )

sparseEntries :: Int -> [(Int, Int)]
sparseEntries size =
  [ (key `mod` max 1 (size `quot` 2), coefficientForKey key)
  | key <- keys (size * 2)
  ]

polynomialCoefficientPair :: Int -> ([Int], [Int])
polynomialCoefficientPair size =
  (fmap coefficientForKey (keys size), fmap (coefficientForKey . (+ size)) (keys size))

powerSetListPair :: Int -> ([Int], [Int])
powerSetListPair size =
  ( [key | key <- keys size, key `mod` 2 == 0],
    [key | key <- keys size, key `mod` 3 /= 0]
  )

coefficientForKey :: Int -> Int
coefficientForKey key =
  case key `mod` 7 of
    0 -> 0
    residue -> residue - 3

sparseEntriesWeight :: [(Int, Int)] -> Int
sparseEntriesWeight =
  foldr (\(basisKey, coefficient) accumulated -> accumulated + basisKey * 31 + coefficient) 0

mapEntriesWeight :: Map Int Int -> Int
mapEntriesWeight =
  Map.foldlWithKey' (\accumulated key value -> accumulated + key * 31 + value) 0

coefficientsWeight :: [Int] -> Int
coefficientsWeight =
  foldr (\coefficient accumulated -> accumulated * 33 + coefficient) 5381

listWeight :: [Int] -> Int
listWeight =
  foldr (\value accumulated -> accumulated * 16777619 + value) 146959810

setWeight :: Set.Set Int -> Int
setWeight =
  listWeight . Set.toAscList

keys :: Int -> [Int]
keys size =
  [0 .. size - 1]

sparseVecSizes :: [Int]
sparseVecSizes =
  [128, 512, 2048]

polynomialSizes :: [Int]
polynomialSizes =
  [16, 64, 128]

powerSetSizes :: [Int]
powerSetSizes =
  [128, 512, 2048]

caseLabel :: String -> Int -> String
caseLabel label size =
  label <> " n=" <> show size
