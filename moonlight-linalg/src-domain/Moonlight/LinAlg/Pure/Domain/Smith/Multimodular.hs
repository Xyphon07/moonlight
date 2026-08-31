{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Moonlight.LinAlg.Pure.Domain.Smith.Multimodular
  ( PrimeSweep (..),
    certifiedPrimeSweep,
    integerResidueWord,
    modInverseWord,
    modMul,
    smithDiagonalFormMultimodular,
    wordPrimeLadder,
  )
where

import Control.Monad.ST (ST, runST)
import Data.Bifunctor (first)
import Data.List (sortBy)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as MU
import Data.Word (Word64)
import GHC.Exts (quotRemWord2#, timesWord2#, word64ToWord#, wordToWord64#)
import GHC.TypeNats (KnownNat)
import GHC.Word (Word64 (W64#))
import Moonlight.Algebra.Pure.Ring (EuclideanDomain (..), GCDDomain (..), mkNonZeroDivisor)
import Moonlight.Core
  ( AdditiveGroup (..),
    AdditiveMonoid (..),
    MoonlightError (..),
    MultiplicativeMonoid (..),
    checkedNonNegativeProduct,
  )
import Moonlight.LinAlg.Internal.Backend.Smith (SmithDiagonalForm (..))
import Moonlight.LinAlg.Pure.Dense.Types
  ( Matrix,
    fromListMatrix,
  )
import qualified Moonlight.LinAlg.Pure.Dense.Types as DenseTypes
import Prelude

data PrimeSweep = PrimeSweep
  { primeSweepRank :: !Int,
    primeSweepDeterminant :: !(Maybe Integer)
  }
  deriving stock (Eq, Show)

data CrtState = CrtState
  { crtResidue :: !Integer,
    crtModulus :: !Integer,
    crtRank :: !Int
  }
  deriving stock (Eq, Show)

data PrimeElimination = PrimeElimination
  { primeEliminationRank :: !Int,
    primeEliminationDeterminant :: !Word64
  }
  deriving stock (Eq, Show)

data SmithTier
  = SmithTierWord32
  | SmithTierWord62
  | SmithTierInteger
  deriving stock (Eq, Show)

data SmithCarrier s
  = SmithWord32Carrier !Word64 !(MU.MVector s Word64)
  | SmithWord62Carrier !Word64 !(MU.MVector s Word64)
  | SmithIntegerCarrier !(MV.MVector s Integer)

data MutableSmithState s = MutableSmithState
  { mutableSmithRowCount :: !Int,
    mutableSmithColumnCount :: !Int,
    mutableSmithModulus :: !Integer,
    mutableSmithCarrier :: !(SmithCarrier s)
  }

data SmithPivot = SmithPivot
  { pivotRowIndex :: !Int,
    pivotColumnIndex :: !Int
  }
  deriving stock (Eq, Show)

data SmithPhaseFailure
  = SmithPhaseBudgetExhausted !String
  | SmithPhaseNormalizationStalled
  | SmithPhasePivotBecameZero
  | SmithPhaseInexactDivision !String
  deriving stock (Eq, Show)

data SmithPhaseResult
  = SmithPhaseDone ![Integer]
  | SmithPhaseFailed !SmithPhaseFailure

smithDiagonalFormMultimodular ::
  forall r c.
  (KnownNat r, KnownNat c) =>
  Matrix r c Integer ->
  Either MoonlightError (SmithDiagonalForm r c Integer)
smithDiagonalFormMultimodular matrixValue = do
  rows <- DenseTypes.matrixToRows matrixValue
  let (rowCount, columnCount) = DenseTypes.matrixShape matrixValue
      diagonalSize = min rowCount columnCount
  primeSweep <- certifiedPrimeSweep rowCount columnCount rows
  invariantFactors <-
    certifiedInvariantFactors
      rowCount
      columnCount
      diagonalSize
      rows
      primeSweep
  diagonalMatrix <- fromListMatrix @r @c (diagonalFlatEntries rowCount columnCount invariantFactors)
  pure (SmithDiagonalForm diagonalMatrix)

certifiedInvariantFactors ::
  Int ->
  Int ->
  Int ->
  [[Integer]] ->
  PrimeSweep ->
  Either MoonlightError [Integer]
certifiedInvariantFactors rowCount columnCount diagonalSize rows primeSweep
  | diagonalSize == 0 = Right []
  | primeSweepRank primeSweep == 0 = Right []
  | rowCount == columnCount && primeSweepRank primeSweep == rowCount =
      case primeSweepDeterminant primeSweep of
        Just determinantValue -> smithNonsingularInvariantFactors determinantValue rows
        Nothing -> Left (InvariantViolation "Smith multimodular square prime sweep did not return a determinant")
  | otherwise = smithCompressedInvariantFactors rowCount columnCount (primeSweepRank primeSweep) rows

certifiedPrimeSweep :: Int -> Int -> [[Integer]] -> Either MoonlightError PrimeSweep
certifiedPrimeSweep rowCount columnCount rows =
  let determinantBound = hadamardMinorBound rowCount columnCount rows
      target = max 2 (2 * determinantBound + 1)
   in finishPrimeSweep rowCount columnCount target rows initialCrtState wordPrimeLadder
  where
    initialCrtState =
      CrtState
        { crtResidue = 0,
          crtModulus = 1,
          crtRank = 0
        }

finishPrimeSweep :: Int -> Int -> Integer -> [[Integer]] -> CrtState -> [Word64] -> Either MoonlightError PrimeSweep
finishPrimeSweep rowCount columnCount target rows stateValue primes
  | crtModulus stateValue > target =
      Right
        PrimeSweep
          { primeSweepRank = crtRank stateValue,
            primeSweepDeterminant =
              if rowCount == columnCount
                then Just (symmetricLift (crtModulus stateValue) (crtResidue stateValue))
                else Nothing
          }
  | otherwise =
      case primes of
        [] -> Left (InvariantViolation "Smith multimodular prime ladder exhausted before Hadamard certification")
        primeValue : remainingPrimes -> do
          let primeMatrix = residueVectorForPrime primeValue rows
              primeResult = primeElimination rowCount columnCount primeValue primeMatrix
          updatedState <- extendCrt rowCount columnCount stateValue primeValue primeResult
          finishPrimeSweep rowCount columnCount target rows updatedState remainingPrimes

extendCrt :: Int -> Int -> CrtState -> Word64 -> PrimeElimination -> Either MoonlightError CrtState
extendCrt rowCount columnCount stateValue primeValue primeResult = do
  let primeInteger = toInteger primeValue
      nextRank = max (crtRank stateValue) (primeEliminationRank primeResult)
  nextResidue <-
    if rowCount == columnCount
      then combineCrt (crtResidue stateValue) (crtModulus stateValue) primeInteger (toInteger (primeEliminationDeterminant primeResult))
      else Right (crtResidue stateValue)
  Right
    CrtState
      { crtResidue = nextResidue,
        crtModulus = crtModulus stateValue * primeInteger,
        crtRank = nextRank
      }

combineCrt :: Integer -> Integer -> Integer -> Integer -> Either MoonlightError Integer
combineCrt residueValue modulusValue primeValue primeResidue = do
  inverseValue <- modularInverseInteger (modulusValue `mod` primeValue) primeValue
  let deltaValue = (primeResidue - residueValue) `mod` primeValue
      correction = (deltaValue * inverseValue) `mod` primeValue
      nextModulus = modulusValue * primeValue
  Right ((residueValue + modulusValue * correction) `mod` nextModulus)

modularInverseInteger :: Integer -> Integer -> Either MoonlightError Integer
modularInverseInteger value modulusValue =
  let (gcdValue, coefficient, _) = extendedGcdDomain value modulusValue
   in if gcdValue == one
        then Right (coefficient `mod` modulusValue)
        else Left (InvariantViolation "Smith multimodular CRT encountered a noninvertible modulus section")

symmetricLift :: Integer -> Integer -> Integer
symmetricLift modulusValue residueValue
  | 2 * residueValue > modulusValue = residueValue - modulusValue
  | otherwise = residueValue

hadamardMinorBound :: Int -> Int -> [[Integer]] -> Integer
hadamardMinorBound rowCount columnCount rows =
  let minorDimension = min rowCount columnCount
      squaredNorms = fmap rowSquaredNorm rows
   in integerCeilingSquareRoot (product (takeLargest minorDimension squaredNorms))

rowSquaredNorm :: [Integer] -> Integer
rowSquaredNorm =
  foldl' (\total entry -> total + entry * entry) 0

takeLargest :: Int -> [Integer] -> [Integer]
takeLargest count =
  take count . sortBy (flip compare)

integerCeilingSquareRoot :: Integer -> Integer
integerCeilingSquareRoot value
  | value <= 0 = 0
  | otherwise =
      let rootValue = integerSquareRoot value
       in if rootValue * rootValue == value
            then rootValue
            else rootValue + 1

integerSquareRoot :: Integer -> Integer
integerSquareRoot value =
  go 0 (value + 1)
  where
    go low high
      | high - low <= 1 = low
      | midpoint * midpoint <= value = go midpoint high
      | otherwise = go low midpoint
      where
        midpoint = (low + high) `quot` 2

wordPrimeLadder :: [Word64]
wordPrimeLadder =
  [ 2147483647,
    2147483629,
    2147483587,
    2147483579,
    2147483563,
    2147483549,
    2147483543,
    2147483497,
    2147483489,
    2147483477,
    2147483423,
    2147483399,
    2147483353,
    2147483323,
    2147483269,
    2147483249,
    2147483237,
    2147483179,
    2147483171,
    2147483137,
    2147483123,
    2147483077,
    2147483069,
    2147483059,
    2147483053,
    2147483033,
    2147483029,
    2147482951,
    2147482949,
    2147482943,
    2147482937,
    2147482921,
    2147482877,
    2147482873,
    2147482819,
    2147482817,
    2147482811,
    2147482801,
    2147482763,
    2147482739,
    2147482697,
    2147482693,
    2147482681,
    2147482663,
    2147482661,
    2147482621,
    2147482591,
    2147482589,
    2147482577,
    2147482507,
    2147482501,
    2147482481,
    2147482417,
    2147482409,
    2147482367,
    2147482361,
    2147482349,
    2147482343,
    2147482327,
    2147482297,
    2147482291,
    2147482273,
    2147482237,
    2147482231
  ]

residueVectorForPrime :: Word64 -> [[Integer]] -> U.Vector Word64
residueVectorForPrime primeValue rows =
  U.fromList (concatMap (fmap (integerResidueWord primeValue)) rows)

integerResidueWord :: Word64 -> Integer -> Word64
integerResidueWord primeValue entryValue =
  fromInteger (entryValue `mod` toInteger primeValue)

primeElimination :: Int -> Int -> Word64 -> U.Vector Word64 -> PrimeElimination
primeElimination rowCount columnCount primeValue entries =
  runST $ do
    work <- U.thaw entries
    let readEntry rowIndex columnIndex =
          MU.read work (flatIndex columnCount rowIndex columnIndex)
        writeEntry rowIndex columnIndex entryValue =
          MU.write work (flatIndex columnCount rowIndex columnIndex) entryValue
        swapRows leftRow rightRow =
          swapRowEntries readEntry writeEntry columnCount leftRow rightRow 0
        eliminateRows pivotRow pivotColumn inversePivot rowIndex
          | rowIndex >= rowCount = pure ()
          | otherwise = do
              entryValue <- readEntry rowIndex pivotColumn
              if entryValue == 0
                then eliminateRows pivotRow pivotColumn inversePivot (rowIndex + 1)
                else do
                  let factor = modMul primeValue entryValue inversePivot
                  eliminateRowEntries readEntry writeEntry primeValue pivotRow rowIndex pivotColumn factor columnCount
                  eliminateRows pivotRow pivotColumn inversePivot (rowIndex + 1)
        step rankValue columnIndex determinantProduct signNegative
          | columnIndex >= columnCount || rankValue >= rowCount =
              pure (rankValue, determinantProduct, signNegative)
          | otherwise = do
              pivotCandidate <- findWordPivot readEntry rowCount rankValue columnIndex
              case pivotCandidate of
                Nothing -> step rankValue (columnIndex + 1) determinantProduct signNegative
                Just pivotRow -> do
                  swapRows rankValue pivotRow
                  pivotValue <- readEntry rankValue columnIndex
                  let nextSignNegative = if pivotRow == rankValue then signNegative else not signNegative
                      nextDeterminant = modMul primeValue determinantProduct pivotValue
                      inversePivot = modInverseWord primeValue pivotValue
                  eliminateRows rankValue columnIndex inversePivot (rankValue + 1)
                  step (rankValue + 1) (columnIndex + 1) nextDeterminant nextSignNegative
    (rankValue, determinantProduct, signNegative) <- step 0 0 1 False
    let determinantValue =
          if rowCount == columnCount && rankValue == rowCount
            then if signNegative then modNeg primeValue determinantProduct else determinantProduct
            else 0
    pure
      PrimeElimination
        { primeEliminationRank = rankValue,
          primeEliminationDeterminant = determinantValue
        }

flatIndex :: Int -> Int -> Int -> Int
flatIndex columnCount rowIndex columnIndex =
  rowIndex * columnCount + columnIndex

findWordPivot :: (Int -> Int -> ST s Word64) -> Int -> Int -> Int -> ST s (Maybe Int)
findWordPivot readEntry rowCount startRow columnIndex =
  go startRow
  where
    go rowIndex
      | rowIndex >= rowCount = pure Nothing
      | otherwise = do
          entryValue <- readEntry rowIndex columnIndex
          if entryValue == 0
            then go (rowIndex + 1)
            else pure (Just rowIndex)

swapRowEntries :: (Int -> Int -> ST s Word64) -> (Int -> Int -> Word64 -> ST s ()) -> Int -> Int -> Int -> Int -> ST s ()
swapRowEntries readEntry writeEntry columnCount leftRow rightRow columnIndex
  | leftRow == rightRow = pure ()
  | columnIndex >= columnCount = pure ()
  | otherwise = do
      leftValue <- readEntry leftRow columnIndex
      rightValue <- readEntry rightRow columnIndex
      writeEntry leftRow columnIndex rightValue
      writeEntry rightRow columnIndex leftValue
      swapRowEntries readEntry writeEntry columnCount leftRow rightRow (columnIndex + 1)

eliminateRowEntries :: (Int -> Int -> ST s Word64) -> (Int -> Int -> Word64 -> ST s ()) -> Word64 -> Int -> Int -> Int -> Word64 -> Int -> ST s ()
eliminateRowEntries readEntry writeEntry primeValue pivotRow targetRow columnIndex factor columnCount
  | columnIndex >= columnCount = pure ()
  | otherwise = do
      targetValue <- readEntry targetRow columnIndex
      pivotValue <- readEntry pivotRow columnIndex
      let updatedValue = modSub primeValue targetValue (modMul primeValue factor pivotValue)
      writeEntry targetRow columnIndex updatedValue
      eliminateRowEntries readEntry writeEntry primeValue pivotRow targetRow (columnIndex + 1) factor columnCount

modSub :: Word64 -> Word64 -> Word64 -> Word64
modSub primeValue leftValue rightValue
  | leftValue >= rightValue = leftValue - rightValue
  | otherwise = primeValue - (rightValue - leftValue)

modMul :: Word64 -> Word64 -> Word64 -> Word64
modMul primeValue leftValue rightValue =
  (leftValue * rightValue) `rem` primeValue

modNeg :: Word64 -> Word64 -> Word64
modNeg primeValue value
  | value == 0 = 0
  | otherwise = primeValue - value

modInverseWord :: Word64 -> Word64 -> Word64
modInverseWord primeValue value =
  modPow primeValue value (primeValue - 2)

modPow :: Word64 -> Word64 -> Word64 -> Word64
modPow primeValue baseValue exponentValue =
  go baseValue exponentValue 1
  where
    go currentBase currentExponent accumulator
      | currentExponent == 0 = accumulator
      | odd currentExponent = go (modMul primeValue currentBase currentBase) (currentExponent `quot` 2) (modMul primeValue accumulator currentBase)
      | otherwise = go (modMul primeValue currentBase currentBase) (currentExponent `quot` 2) accumulator

smithNonsingularInvariantFactors :: Integer -> [[Integer]] -> Either MoonlightError [Integer]
smithNonsingularInvariantFactors determinantValue rows
  | determinantValue == zero = Left (InvariantViolation "Smith multimodular nonsingular phase received a zero determinant")
  | otherwise = do
      let rowCount = length rows
          columnCount = firstRowLength rows
          modulusValue = 2 * abs determinantValue
      validateSmithPhaseCardinalities rowCount columnCount
      case runSmithPhase rowCount columnCount modulusValue rows of
            SmithPhaseFailed failureValue -> Left (smithPhaseFailureError failureValue)
            SmithPhaseDone diagonalValues -> do
              let invariantFactors = fmap (smithFactorFromResidue modulusValue) diagonalValues
              certifyNonsingularFactors determinantValue invariantFactors
              Right invariantFactors

smithFactorFromResidue :: Integer -> Integer -> Integer
smithFactorFromResidue modulusValue residueValue =
  abs (gcd residueValue modulusValue)

certifyNonsingularFactors :: Integer -> [Integer] -> Either MoonlightError ()
certifyNonsingularFactors determinantValue invariantFactors
  | product invariantFactors /= abs determinantValue =
      Left (InvariantViolation "Smith multimodular determinant-modulus factors failed determinant product certification")
  | otherwise = certifyDivisibilityFactors invariantFactors

certifyDivisibilityFactors :: [Integer] -> Either MoonlightError ()
certifyDivisibilityFactors values =
  case values of
    [] -> Right ()
    [_] -> Right ()
    leftValue : rightValue : restValues ->
      if leftValue == zero || maybe False ((== zero) . snd) (divideIntegerMaybe rightValue leftValue)
        then certifyDivisibilityFactors (rightValue : restValues)
        else Left (InvariantViolation "Smith multimodular invariant factors violate the divisibility chain")

smithCompressedInvariantFactors :: Int -> Int -> Int -> [[Integer]] -> Either MoonlightError [Integer]
smithCompressedInvariantFactors rowCount columnCount certifiedRank rows = do
  validateSmithPhaseCardinalities rowCount columnCount
  case runSmithPhase rowCount columnCount 0 rows of
    SmithPhaseFailed failureValue -> Left (smithPhaseFailureError failureValue)
    SmithPhaseDone diagonalValues -> do
      let compressedFactors = filter (/= zero) (fmap abs diagonalValues)
      if length compressedFactors /= certifiedRank
        then Left (InvariantViolation "Smith multimodular rank certificate disagreed with Hermite compression")
        else
          case compressedFactors of
            [] -> Right []
            _ -> smithNonsingularInvariantFactors (product compressedFactors) (diagonalCoreRows compressedFactors)

validateSmithPhaseCardinalities :: Int -> Int -> Either MoonlightError ()
validateSmithPhaseCardinalities rowCount columnCount = do
  matrixEntryCount <- checkedSmithPhaseProduct "matrix entries" rowCount columnCount
  _ <- checkedSmithPhaseProduct "normalization budget" matrixEntryCount 2
  _ <- checkedSmithPhaseProduct "divisibility-chain budget" (min rowCount columnCount) (min rowCount columnCount)
  Right ()

checkedSmithPhaseProduct :: String -> Int -> Int -> Either MoonlightError Int
checkedSmithPhaseProduct context leftFactor rightFactor =
  first
    (const (InvariantViolation ("Smith multimodular " <> context <> " exceed Int cardinality")))
    (checkedNonNegativeProduct leftFactor rightFactor)

smithPhaseFailureError :: SmithPhaseFailure -> MoonlightError
smithPhaseFailureError failureValue =
  case failureValue of
    SmithPhaseBudgetExhausted context ->
      InvariantViolation ("Smith multimodular " <> context <> " exhausted iteration budget")
    SmithPhaseNormalizationStalled ->
      InvariantViolation "Smith multimodular normalization stalled before reaching diagonal form"
    SmithPhasePivotBecameZero ->
      InvariantViolation "Smith multimodular pivot became zero during reduction"
    SmithPhaseInexactDivision context ->
      InvariantViolation ("Smith multimodular exact quotient had nonzero remainder during " <> context)

runSmithPhase :: Int -> Int -> Integer -> [[Integer]] -> SmithPhaseResult
runSmithPhase rowCount columnCount modulusValue rows =
  runST $ do
    stateValue <- newMutableSmithState rowCount columnCount modulusValue rows
    smithFailure <- smithStepMutable 0 stateValue
    case smithFailure of
      Just failureValue -> pure (SmithPhaseFailed failureValue)
      Nothing -> do
        chainFailure <- enforceDivisibilityChainMutable stateValue
        case chainFailure of
          Just failureValue -> pure (SmithPhaseFailed failureValue)
          Nothing -> SmithPhaseDone <$> readDiagonalMutable (min rowCount columnCount) stateValue

newMutableSmithState :: Int -> Int -> Integer -> [[Integer]] -> ST s (MutableSmithState s)
newMutableSmithState rowCount columnCount modulusValue rows =
  case smithTierForModulus modulusValue of
    SmithTierWord32 -> do
      let modulusWord = fromInteger modulusValue
      entries <- U.thaw (U.fromList (flattenRows (integerResidueWord modulusWord) rows))
      pure
        MutableSmithState
          { mutableSmithRowCount = rowCount,
            mutableSmithColumnCount = columnCount,
            mutableSmithModulus = modulusValue,
            mutableSmithCarrier = SmithWord32Carrier modulusWord entries
          }
    SmithTierWord62 -> do
      let modulusWord = fromInteger modulusValue
      entries <- U.thaw (U.fromList (flattenRows (integerResidueWord modulusWord) rows))
      pure
        MutableSmithState
          { mutableSmithRowCount = rowCount,
            mutableSmithColumnCount = columnCount,
            mutableSmithModulus = modulusValue,
            mutableSmithCarrier = SmithWord62Carrier modulusWord entries
          }
    SmithTierInteger -> do
      entries <- V.thaw (V.fromList (flattenRows (centerResidue modulusValue) rows))
      pure
        MutableSmithState
          { mutableSmithRowCount = rowCount,
            mutableSmithColumnCount = columnCount,
            mutableSmithModulus = modulusValue,
            mutableSmithCarrier = SmithIntegerCarrier entries
          }

smithTierForModulus :: Integer -> SmithTier
smithTierForModulus modulusValue
  | modulusValue > 0 && modulusValue < word32ModulusLimit = SmithTierWord32
  | modulusValue > 0 && modulusValue < word62ModulusLimit = SmithTierWord62
  | otherwise = SmithTierInteger

word32ModulusLimit :: Integer
word32ModulusLimit =
  2 ^ (32 :: Int)

word62ModulusLimit :: Integer
word62ModulusLimit =
  2 ^ (62 :: Int)

flattenRows :: (Integer -> a) -> [[Integer]] -> [a]
flattenRows transformEntry =
  concatMap (fmap transformEntry)

firstRowLength :: [[a]] -> Int
firstRowLength rows =
  case rows of
    [] -> 0
    rowValue : _ -> length rowValue

readDiagonalMutable :: Int -> MutableSmithState s -> ST s [Integer]
readDiagonalMutable diagonalSize stateValue =
  go 0 []
  where
    go diagonalIndex diagonalValues
      | diagonalIndex >= diagonalSize = pure (reverse diagonalValues)
      | otherwise = do
          diagonalValue <- readSmithEntry stateValue diagonalIndex diagonalIndex
          go (diagonalIndex + 1) (diagonalValue : diagonalValues)

smithStepMutable :: Int -> MutableSmithState s -> ST s (Maybe SmithPhaseFailure)
smithStepMutable pivotIndex stateValue
  | pivotIndex >= min (mutableSmithRowCount stateValue) (mutableSmithColumnCount stateValue) = pure Nothing
  | otherwise = do
      pivotCandidate <- findPivotMutable pivotIndex pivotIndex stateValue
      case pivotCandidate of
        Nothing -> pure Nothing
        Just pivotValue -> do
          swapRowsMutable pivotIndex (pivotRowIndex pivotValue) stateValue
          swapColumnsMutable pivotIndex (pivotColumnIndex pivotValue) stateValue
          normalizationFailure <-
            normalizePivotMutable
              pivotIndex
              pivotIndex
              (max 1 (mutableSmithRowCount stateValue * mutableSmithColumnCount stateValue * 2))
              stateValue
          case normalizationFailure of
            Just failureValue -> pure (Just failureValue)
            Nothing -> smithStepMutable (pivotIndex + 1) stateValue

enforceDivisibilityChainMutable :: MutableSmithState s -> ST s (Maybe SmithPhaseFailure)
enforceDivisibilityChainMutable stateValue =
  go (diagonalSize * diagonalSize)
  where
    diagonalSize = min (mutableSmithRowCount stateValue) (mutableSmithColumnCount stateValue)

    go remainingBudget
      | remainingBudget <= 0 = do
          violationValue <- findDivisibilityViolationMutable diagonalSize stateValue
          case violationValue of
            Nothing -> pure Nothing
            Just _ -> pure (Just (SmithPhaseBudgetExhausted "divisibility chain"))
      | otherwise = do
          violationValue <- findDivisibilityViolationMutable diagonalSize stateValue
          case violationValue of
            Nothing -> pure Nothing
            Just violationIndex -> do
              rowCombineMutable violationIndex (violationIndex + 1) (neg one) stateValue
              leftFailure <-
                normalizePivotMutable
                  violationIndex
                  violationIndex
                  (max 1 (mutableSmithRowCount stateValue * mutableSmithColumnCount stateValue * 2))
                  stateValue
              case leftFailure of
                Just failureValue -> pure (Just failureValue)
                Nothing -> do
                  rightFailure <-
                    normalizePivotMutable
                      (violationIndex + 1)
                      (violationIndex + 1)
                      (max 1 (mutableSmithRowCount stateValue * mutableSmithColumnCount stateValue * 2))
                      stateValue
                  case rightFailure of
                    Just failureValue -> pure (Just failureValue)
                    Nothing -> go (remainingBudget - 1)

findDivisibilityViolationMutable :: Int -> MutableSmithState s -> ST s (Maybe Int)
findDivisibilityViolationMutable diagonalSize stateValue =
  go 0
  where
    go diagonalIndex
      | diagonalIndex >= diagonalSize - 1 = pure Nothing
      | otherwise = do
          leftDiagonal <- readSmithEntry stateValue diagonalIndex diagonalIndex
          rightDiagonal <- readSmithEntry stateValue (diagonalIndex + 1) (diagonalIndex + 1)
          if leftDiagonal == zero
            || rightDiagonal == zero
            || maybe False ((== zero) . snd) (divideIntegerMaybe rightDiagonal leftDiagonal)
            then go (diagonalIndex + 1)
            else pure (Just diagonalIndex)

findPivotMutable :: Int -> Int -> MutableSmithState s -> ST s (Maybe SmithPivot)
findPivotMutable startRow startColumn stateValue =
  fmap fst <$> goRows startRow Nothing
  where
    goRows rowIndex bestValue
      | rowIndex >= mutableSmithRowCount stateValue = pure bestValue
      | otherwise = do
          rowBest <- goColumns rowIndex startColumn bestValue
          goRows (rowIndex + 1) rowBest

    goColumns rowIndex columnIndex bestValue
      | columnIndex >= mutableSmithColumnCount stateValue = pure bestValue
      | otherwise = do
          entryMagnitude <- entryMagnitudeMaybeMutable stateValue rowIndex columnIndex
          nextBest <-
            case entryMagnitude of
              Nothing -> pure bestValue
              Just magnitudeValue -> pure (betterPivot bestValue (SmithPivot rowIndex columnIndex, magnitudeValue))
          goColumns rowIndex (columnIndex + 1) nextBest

betterPivot :: Maybe (SmithPivot, Integer) -> (SmithPivot, Integer) -> Maybe (SmithPivot, Integer)
betterPivot bestValue candidateValue =
  case bestValue of
    Nothing -> Just candidateValue
    Just currentValue ->
      if pivotOrderingKey candidateValue < pivotOrderingKey currentValue
        then Just candidateValue
        else bestValue

pivotOrderingKey :: (SmithPivot, Integer) -> (Integer, Int, Int)
pivotOrderingKey (pivotValue, magnitudeValue) =
  (magnitudeValue, pivotRowIndex pivotValue, pivotColumnIndex pivotValue)

normalizePivotMutable :: Int -> Int -> Int -> MutableSmithState s -> ST s (Maybe SmithPhaseFailure)
normalizePivotMutable pivotRow pivotColumn remainingBudget stateValue
  | remainingBudget <= 0 = pure (Just (SmithPhaseBudgetExhausted "normalization"))
  | otherwise = do
      (columnFailure, columnChanged) <- clearColumnMutable pivotRow pivotColumn stateValue
      case columnFailure of
        Just failureValue -> pure (Just failureValue)
        Nothing -> do
          (rowFailure, rowChanged) <- clearRowMutable pivotRow pivotColumn stateValue
          case rowFailure of
            Just failureValue -> pure (Just failureValue)
            Nothing -> do
              clearedColumn <- columnClearedMutable pivotRow pivotColumn stateValue
              clearedRow <- rowClearedMutable pivotRow pivotColumn stateValue
              if clearedColumn && clearedRow
                then pure Nothing
                else
                  if columnChanged || rowChanged
                    then normalizePivotMutable pivotRow pivotColumn (remainingBudget - 1) stateValue
                    else pure (Just SmithPhaseNormalizationStalled)

clearColumnMutable :: Int -> Int -> MutableSmithState s -> ST s (Maybe SmithPhaseFailure, Bool)
clearColumnMutable pivotRow pivotColumn stateValue = do
  candidateRow <- firstColumnEntryMutable pivotRow pivotColumn stateValue
  case candidateRow of
    Nothing -> pure (Nothing, False)
    Just rowIndex -> do
      pivotValue <- readSmithEntry stateValue pivotRow pivotColumn
      entryValue <- readSmithEntry stateValue rowIndex pivotColumn
      case divideIntegerMaybe entryValue pivotValue of
        Nothing -> pure (Just SmithPhasePivotBecameZero, False)
        Just (quotientValue, remainderValue) -> do
          reductionFailure <-
            if remainderValue == zero
              then rowCombineMutable rowIndex pivotRow quotientValue stateValue *> pure Nothing
              else gcdCombineRowsMutable pivotRow rowIndex pivotColumn stateValue
          case reductionFailure of
            Just failureValue -> pure (Just failureValue, True)
            Nothing -> do
              (nextFailure, _) <- clearColumnMutable pivotRow pivotColumn stateValue
              pure (nextFailure, True)

clearRowMutable :: Int -> Int -> MutableSmithState s -> ST s (Maybe SmithPhaseFailure, Bool)
clearRowMutable pivotRow pivotColumn stateValue = do
  candidateColumn <- firstRowEntryMutable pivotRow pivotColumn stateValue
  case candidateColumn of
    Nothing -> pure (Nothing, False)
    Just columnIndex -> do
      pivotValue <- readSmithEntry stateValue pivotRow pivotColumn
      entryValue <- readSmithEntry stateValue pivotRow columnIndex
      case divideIntegerMaybe entryValue pivotValue of
        Nothing -> pure (Just SmithPhasePivotBecameZero, False)
        Just (quotientValue, remainderValue) -> do
          reductionFailure <-
            if remainderValue == zero
              then columnCombineMutable columnIndex pivotColumn quotientValue stateValue *> pure Nothing
              else gcdCombineColumnsMutable pivotRow pivotColumn columnIndex stateValue
          case reductionFailure of
            Just failureValue -> pure (Just failureValue, True)
            Nothing -> do
              (nextFailure, _) <- clearRowMutable pivotRow pivotColumn stateValue
              pure (nextFailure, True)

gcdCombineRowsMutable :: Int -> Int -> Int -> MutableSmithState s -> ST s (Maybe SmithPhaseFailure)
gcdCombineRowsMutable pivotRow candidateRow pivotColumn stateValue = do
  pivotValue <- readSmithEntry stateValue pivotRow pivotColumn
  entryValue <- readSmithEntry stateValue candidateRow pivotColumn
  let (gcdValue, pivotCoefficient, entryCoefficient) = extendedGcdDomain pivotValue entryValue
  case (exactQuotientMaybe "row gcd pivot quotient" pivotValue gcdValue, exactQuotientMaybe "row gcd entry quotient" entryValue gcdValue) of
    (Right pivotQuotient, Right entryQuotient) -> do
      rowPairTransformMutable pivotRow candidateRow pivotCoefficient entryCoefficient (neg entryQuotient) pivotQuotient stateValue
      pure Nothing
    (Left failureValue, _) -> pure (Just failureValue)
    (_, Left failureValue) -> pure (Just failureValue)

gcdCombineColumnsMutable :: Int -> Int -> Int -> MutableSmithState s -> ST s (Maybe SmithPhaseFailure)
gcdCombineColumnsMutable pivotRow pivotColumn candidateColumn stateValue = do
  pivotValue <- readSmithEntry stateValue pivotRow pivotColumn
  entryValue <- readSmithEntry stateValue pivotRow candidateColumn
  let (gcdValue, pivotCoefficient, entryCoefficient) = extendedGcdDomain pivotValue entryValue
  case (exactQuotientMaybe "column gcd pivot quotient" pivotValue gcdValue, exactQuotientMaybe "column gcd entry quotient" entryValue gcdValue) of
    (Right pivotQuotient, Right entryQuotient) -> do
      columnPairTransformMutable pivotColumn candidateColumn pivotCoefficient entryCoefficient (neg entryQuotient) pivotQuotient stateValue
      pure Nothing
    (Left failureValue, _) -> pure (Just failureValue)
    (_, Left failureValue) -> pure (Just failureValue)

exactQuotientMaybe :: String -> Integer -> Integer -> Either SmithPhaseFailure Integer
exactQuotientMaybe context numerator denominator =
  case divideIntegerMaybe numerator denominator of
    Nothing -> Left (SmithPhaseInexactDivision context)
    Just (quotientValue, remainderValue)
      | remainderValue == zero -> Right quotientValue
      | otherwise -> Left (SmithPhaseInexactDivision context)

divideIntegerMaybe :: Integer -> Integer -> Maybe (Integer, Integer)
divideIntegerMaybe numerator denominator =
  divideWithRemainder numerator <$> mkNonZeroDivisor denominator

columnClearedMutable :: Int -> Int -> MutableSmithState s -> ST s Bool
columnClearedMutable pivotRow pivotColumn stateValue =
  go 0
  where
    go rowIndex
      | rowIndex >= mutableSmithRowCount stateValue = pure True
      | rowIndex == pivotRow = go (rowIndex + 1)
      | otherwise = do
          isZeroEntry <- entryIsZeroMutable stateValue rowIndex pivotColumn
          if isZeroEntry
            then go (rowIndex + 1)
            else pure False

rowClearedMutable :: Int -> Int -> MutableSmithState s -> ST s Bool
rowClearedMutable pivotRow pivotColumn stateValue =
  go 0
  where
    go columnIndex
      | columnIndex >= mutableSmithColumnCount stateValue = pure True
      | columnIndex == pivotColumn = go (columnIndex + 1)
      | otherwise = do
          isZeroEntry <- entryIsZeroMutable stateValue pivotRow columnIndex
          if isZeroEntry
            then go (columnIndex + 1)
            else pure False

firstColumnEntryMutable :: Int -> Int -> MutableSmithState s -> ST s (Maybe Int)
firstColumnEntryMutable pivotRow pivotColumn stateValue =
  go 0
  where
    go rowIndex
      | rowIndex >= mutableSmithRowCount stateValue = pure Nothing
      | rowIndex == pivotRow = go (rowIndex + 1)
      | otherwise = do
          isZeroEntry <- entryIsZeroMutable stateValue rowIndex pivotColumn
          if isZeroEntry
            then go (rowIndex + 1)
            else pure (Just rowIndex)

firstRowEntryMutable :: Int -> Int -> MutableSmithState s -> ST s (Maybe Int)
firstRowEntryMutable pivotRow pivotColumn stateValue =
  go 0
  where
    go columnIndex
      | columnIndex >= mutableSmithColumnCount stateValue = pure Nothing
      | columnIndex == pivotColumn = go (columnIndex + 1)
      | otherwise = do
          isZeroEntry <- entryIsZeroMutable stateValue pivotRow columnIndex
          if isZeroEntry
            then go (columnIndex + 1)
            else pure (Just columnIndex)

readSmithEntry :: MutableSmithState s -> Int -> Int -> ST s Integer
readSmithEntry stateValue rowIndex columnIndex =
  case mutableSmithCarrier stateValue of
    SmithWord32Carrier modulusWord entries -> do
      entryValue <- MU.read entries (flatIndex (mutableSmithColumnCount stateValue) rowIndex columnIndex)
      pure (symmetricLift (toInteger modulusWord) (toInteger entryValue))
    SmithWord62Carrier modulusWord entries -> do
      entryValue <- MU.read entries (flatIndex (mutableSmithColumnCount stateValue) rowIndex columnIndex)
      pure (symmetricLift (toInteger modulusWord) (toInteger entryValue))
    SmithIntegerCarrier entries ->
      MV.read entries (flatIndex (mutableSmithColumnCount stateValue) rowIndex columnIndex)

entryIsZeroMutable :: MutableSmithState s -> Int -> Int -> ST s Bool
entryIsZeroMutable stateValue rowIndex columnIndex =
  case mutableSmithCarrier stateValue of
    SmithWord32Carrier _ entries -> (== 0) <$> MU.read entries (flatIndex (mutableSmithColumnCount stateValue) rowIndex columnIndex)
    SmithWord62Carrier _ entries -> (== 0) <$> MU.read entries (flatIndex (mutableSmithColumnCount stateValue) rowIndex columnIndex)
    SmithIntegerCarrier entries -> (== zero) <$> MV.read entries (flatIndex (mutableSmithColumnCount stateValue) rowIndex columnIndex)

entryMagnitudeMaybeMutable :: MutableSmithState s -> Int -> Int -> ST s (Maybe Integer)
entryMagnitudeMaybeMutable stateValue rowIndex columnIndex =
  case mutableSmithCarrier stateValue of
    SmithWord32Carrier modulusWord entries -> do
      entryValue <- MU.read entries (flatIndex (mutableSmithColumnCount stateValue) rowIndex columnIndex)
      pure
        ( if entryValue == 0
            then Nothing
            else Just (toInteger (wordSymmetricMagnitude modulusWord entryValue))
        )
    SmithWord62Carrier modulusWord entries -> do
      entryValue <- MU.read entries (flatIndex (mutableSmithColumnCount stateValue) rowIndex columnIndex)
      pure
        ( if entryValue == 0
            then Nothing
            else Just (toInteger (wordSymmetricMagnitude modulusWord entryValue))
        )
    SmithIntegerCarrier entries -> do
      entryValue <- MV.read entries (flatIndex (mutableSmithColumnCount stateValue) rowIndex columnIndex)
      pure
        ( if entryValue == zero
            then Nothing
            else Just (abs entryValue)
        )

wordSymmetricMagnitude :: Word64 -> Word64 -> Word64
wordSymmetricMagnitude modulusWord entryValue =
  let complementValue = modulusWord - entryValue
   in if entryValue <= complementValue
        then entryValue
        else complementValue

swapRowsMutable :: Int -> Int -> MutableSmithState s -> ST s ()
swapRowsMutable leftRow rightRow stateValue =
  case mutableSmithCarrier stateValue of
    SmithWord32Carrier _ entries -> swapRowsWord (mutableSmithColumnCount stateValue) entries leftRow rightRow 0
    SmithWord62Carrier _ entries -> swapRowsWord (mutableSmithColumnCount stateValue) entries leftRow rightRow 0
    SmithIntegerCarrier entries -> swapRowsInteger (mutableSmithColumnCount stateValue) entries leftRow rightRow 0

swapColumnsMutable :: Int -> Int -> MutableSmithState s -> ST s ()
swapColumnsMutable leftColumn rightColumn stateValue =
  case mutableSmithCarrier stateValue of
    SmithWord32Carrier _ entries -> swapColumnsWord (mutableSmithColumnCount stateValue) entries leftColumn rightColumn 0 (mutableSmithRowCount stateValue)
    SmithWord62Carrier _ entries -> swapColumnsWord (mutableSmithColumnCount stateValue) entries leftColumn rightColumn 0 (mutableSmithRowCount stateValue)
    SmithIntegerCarrier entries -> swapColumnsInteger (mutableSmithColumnCount stateValue) entries leftColumn rightColumn 0 (mutableSmithRowCount stateValue)

swapRowsWord :: Int -> MU.MVector s Word64 -> Int -> Int -> Int -> ST s ()
swapRowsWord columnCount entries leftRow rightRow columnIndex
  | leftRow == rightRow = pure ()
  | columnIndex >= columnCount = pure ()
  | otherwise = do
      let leftIndex = flatIndex columnCount leftRow columnIndex
          rightIndex = flatIndex columnCount rightRow columnIndex
      leftValue <- MU.read entries leftIndex
      rightValue <- MU.read entries rightIndex
      MU.write entries leftIndex rightValue
      MU.write entries rightIndex leftValue
      swapRowsWord columnCount entries leftRow rightRow (columnIndex + 1)

swapRowsInteger :: Int -> MV.MVector s Integer -> Int -> Int -> Int -> ST s ()
swapRowsInteger columnCount entries leftRow rightRow columnIndex
  | leftRow == rightRow = pure ()
  | columnIndex >= columnCount = pure ()
  | otherwise = do
      let leftIndex = flatIndex columnCount leftRow columnIndex
          rightIndex = flatIndex columnCount rightRow columnIndex
      leftValue <- MV.read entries leftIndex
      rightValue <- MV.read entries rightIndex
      MV.write entries leftIndex rightValue
      MV.write entries rightIndex leftValue
      swapRowsInteger columnCount entries leftRow rightRow (columnIndex + 1)

swapColumnsWord :: Int -> MU.MVector s Word64 -> Int -> Int -> Int -> Int -> ST s ()
swapColumnsWord columnCount entries leftColumn rightColumn rowIndex rowCount
  | leftColumn == rightColumn = pure ()
  | rowIndex >= rowCount = pure ()
  | otherwise = do
      let leftIndex = flatIndex columnCount rowIndex leftColumn
          rightIndex = flatIndex columnCount rowIndex rightColumn
      leftValue <- MU.read entries leftIndex
      rightValue <- MU.read entries rightIndex
      MU.write entries leftIndex rightValue
      MU.write entries rightIndex leftValue
      swapColumnsWord columnCount entries leftColumn rightColumn (rowIndex + 1) rowCount

swapColumnsInteger :: Int -> MV.MVector s Integer -> Int -> Int -> Int -> Int -> ST s ()
swapColumnsInteger columnCount entries leftColumn rightColumn rowIndex rowCount
  | leftColumn == rightColumn = pure ()
  | rowIndex >= rowCount = pure ()
  | otherwise = do
      let leftIndex = flatIndex columnCount rowIndex leftColumn
          rightIndex = flatIndex columnCount rowIndex rightColumn
      leftValue <- MV.read entries leftIndex
      rightValue <- MV.read entries rightIndex
      MV.write entries leftIndex rightValue
      MV.write entries rightIndex leftValue
      swapColumnsInteger columnCount entries leftColumn rightColumn (rowIndex + 1) rowCount

rowCombineMutable :: Int -> Int -> Integer -> MutableSmithState s -> ST s ()
rowCombineMutable targetRow sourceRow coefficient stateValue =
  case mutableSmithCarrier stateValue of
    SmithWord32Carrier modulusWord entries ->
      rowCombineWord wordMul32 (mutableSmithColumnCount stateValue) modulusWord entries targetRow sourceRow (integerResidueWord modulusWord (neg coefficient)) 0
    SmithWord62Carrier modulusWord entries ->
      rowCombineWord wordMul62 (mutableSmithColumnCount stateValue) modulusWord entries targetRow sourceRow (integerResidueWord modulusWord (neg coefficient)) 0
    SmithIntegerCarrier entries ->
      rowCombineInteger (mutableSmithColumnCount stateValue) (mutableSmithModulus stateValue) entries targetRow sourceRow coefficient 0

columnCombineMutable :: Int -> Int -> Integer -> MutableSmithState s -> ST s ()
columnCombineMutable targetColumn sourceColumn coefficient stateValue =
  case mutableSmithCarrier stateValue of
    SmithWord32Carrier modulusWord entries ->
      columnCombineWord wordMul32 (mutableSmithColumnCount stateValue) modulusWord entries targetColumn sourceColumn (integerResidueWord modulusWord (neg coefficient)) 0 (mutableSmithRowCount stateValue)
    SmithWord62Carrier modulusWord entries ->
      columnCombineWord wordMul62 (mutableSmithColumnCount stateValue) modulusWord entries targetColumn sourceColumn (integerResidueWord modulusWord (neg coefficient)) 0 (mutableSmithRowCount stateValue)
    SmithIntegerCarrier entries ->
      columnCombineInteger (mutableSmithColumnCount stateValue) (mutableSmithModulus stateValue) entries targetColumn sourceColumn coefficient 0 (mutableSmithRowCount stateValue)

rowPairTransformMutable :: Int -> Int -> Integer -> Integer -> Integer -> Integer -> MutableSmithState s -> ST s ()
rowPairTransformMutable leftRow rightRow aa ab ba bb stateValue =
  case mutableSmithCarrier stateValue of
    SmithWord32Carrier modulusWord entries ->
      rowPairTransformWord wordMul32 (mutableSmithColumnCount stateValue) modulusWord entries leftRow rightRow (integerResidueWord modulusWord aa) (integerResidueWord modulusWord ab) (integerResidueWord modulusWord ba) (integerResidueWord modulusWord bb) 0
    SmithWord62Carrier modulusWord entries ->
      rowPairTransformWord wordMul62 (mutableSmithColumnCount stateValue) modulusWord entries leftRow rightRow (integerResidueWord modulusWord aa) (integerResidueWord modulusWord ab) (integerResidueWord modulusWord ba) (integerResidueWord modulusWord bb) 0
    SmithIntegerCarrier entries ->
      rowPairTransformInteger (mutableSmithColumnCount stateValue) (mutableSmithModulus stateValue) entries leftRow rightRow aa ab ba bb 0

columnPairTransformMutable :: Int -> Int -> Integer -> Integer -> Integer -> Integer -> MutableSmithState s -> ST s ()
columnPairTransformMutable leftColumn rightColumn aa ab ba bb stateValue =
  case mutableSmithCarrier stateValue of
    SmithWord32Carrier modulusWord entries ->
      columnPairTransformWord wordMul32 (mutableSmithColumnCount stateValue) modulusWord entries leftColumn rightColumn (integerResidueWord modulusWord aa) (integerResidueWord modulusWord ab) (integerResidueWord modulusWord ba) (integerResidueWord modulusWord bb) 0 (mutableSmithRowCount stateValue)
    SmithWord62Carrier modulusWord entries ->
      columnPairTransformWord wordMul62 (mutableSmithColumnCount stateValue) modulusWord entries leftColumn rightColumn (integerResidueWord modulusWord aa) (integerResidueWord modulusWord ab) (integerResidueWord modulusWord ba) (integerResidueWord modulusWord bb) 0 (mutableSmithRowCount stateValue)
    SmithIntegerCarrier entries ->
      columnPairTransformInteger (mutableSmithColumnCount stateValue) (mutableSmithModulus stateValue) entries leftColumn rightColumn aa ab ba bb 0 (mutableSmithRowCount stateValue)

rowCombineWord :: (Word64 -> Word64 -> Word64 -> Word64) -> Int -> Word64 -> MU.MVector s Word64 -> Int -> Int -> Word64 -> Int -> ST s ()
rowCombineWord multiplyMod columnCount modulusWord entries targetRow sourceRow coefficientWord columnIndex
  | columnIndex >= columnCount = pure ()
  | otherwise = do
      let targetIndex = flatIndex columnCount targetRow columnIndex
          sourceIndex = flatIndex columnCount sourceRow columnIndex
      targetValue <- MU.read entries targetIndex
      sourceValue <- MU.read entries sourceIndex
      MU.write entries targetIndex (wordAddMod modulusWord targetValue (multiplyMod modulusWord coefficientWord sourceValue))
      rowCombineWord multiplyMod columnCount modulusWord entries targetRow sourceRow coefficientWord (columnIndex + 1)

columnCombineWord :: (Word64 -> Word64 -> Word64 -> Word64) -> Int -> Word64 -> MU.MVector s Word64 -> Int -> Int -> Word64 -> Int -> Int -> ST s ()
columnCombineWord multiplyMod columnCount modulusWord entries targetColumn sourceColumn coefficientWord rowIndex rowCount
  | rowIndex >= rowCount = pure ()
  | otherwise = do
      let targetIndex = flatIndex columnCount rowIndex targetColumn
          sourceIndex = flatIndex columnCount rowIndex sourceColumn
      targetValue <- MU.read entries targetIndex
      sourceValue <- MU.read entries sourceIndex
      MU.write entries targetIndex (wordAddMod modulusWord targetValue (multiplyMod modulusWord coefficientWord sourceValue))
      columnCombineWord multiplyMod columnCount modulusWord entries targetColumn sourceColumn coefficientWord (rowIndex + 1) rowCount

rowPairTransformWord :: (Word64 -> Word64 -> Word64 -> Word64) -> Int -> Word64 -> MU.MVector s Word64 -> Int -> Int -> Word64 -> Word64 -> Word64 -> Word64 -> Int -> ST s ()
rowPairTransformWord multiplyMod columnCount modulusWord entries leftRow rightRow aa ab ba bb columnIndex
  | columnIndex >= columnCount = pure ()
  | otherwise = do
      let leftIndex = flatIndex columnCount leftRow columnIndex
          rightIndex = flatIndex columnCount rightRow columnIndex
      leftValue <- MU.read entries leftIndex
      rightValue <- MU.read entries rightIndex
      MU.write entries leftIndex (wordLinearCombination multiplyMod modulusWord aa leftValue ab rightValue)
      MU.write entries rightIndex (wordLinearCombination multiplyMod modulusWord ba leftValue bb rightValue)
      rowPairTransformWord multiplyMod columnCount modulusWord entries leftRow rightRow aa ab ba bb (columnIndex + 1)

columnPairTransformWord :: (Word64 -> Word64 -> Word64 -> Word64) -> Int -> Word64 -> MU.MVector s Word64 -> Int -> Int -> Word64 -> Word64 -> Word64 -> Word64 -> Int -> Int -> ST s ()
columnPairTransformWord multiplyMod columnCount modulusWord entries leftColumn rightColumn aa ab ba bb rowIndex rowCount
  | rowIndex >= rowCount = pure ()
  | otherwise = do
      let leftIndex = flatIndex columnCount rowIndex leftColumn
          rightIndex = flatIndex columnCount rowIndex rightColumn
      leftValue <- MU.read entries leftIndex
      rightValue <- MU.read entries rightIndex
      MU.write entries leftIndex (wordLinearCombination multiplyMod modulusWord aa leftValue ab rightValue)
      MU.write entries rightIndex (wordLinearCombination multiplyMod modulusWord ba leftValue bb rightValue)
      columnPairTransformWord multiplyMod columnCount modulusWord entries leftColumn rightColumn aa ab ba bb (rowIndex + 1) rowCount

rowCombineInteger :: Int -> Integer -> MV.MVector s Integer -> Int -> Int -> Integer -> Int -> ST s ()
rowCombineInteger columnCount modulusValue entries targetRow sourceRow coefficient columnIndex
  | columnIndex >= columnCount = pure ()
  | otherwise = do
      let targetIndex = flatIndex columnCount targetRow columnIndex
          sourceIndex = flatIndex columnCount sourceRow columnIndex
      targetValue <- MV.read entries targetIndex
      sourceValue <- MV.read entries sourceIndex
      MV.write entries targetIndex (centerResidue modulusValue (targetValue - coefficient * sourceValue))
      rowCombineInteger columnCount modulusValue entries targetRow sourceRow coefficient (columnIndex + 1)

columnCombineInteger :: Int -> Integer -> MV.MVector s Integer -> Int -> Int -> Integer -> Int -> Int -> ST s ()
columnCombineInteger columnCount modulusValue entries targetColumn sourceColumn coefficient rowIndex rowCount
  | rowIndex >= rowCount = pure ()
  | otherwise = do
      let targetIndex = flatIndex columnCount rowIndex targetColumn
          sourceIndex = flatIndex columnCount rowIndex sourceColumn
      targetValue <- MV.read entries targetIndex
      sourceValue <- MV.read entries sourceIndex
      MV.write entries targetIndex (centerResidue modulusValue (targetValue - coefficient * sourceValue))
      columnCombineInteger columnCount modulusValue entries targetColumn sourceColumn coefficient (rowIndex + 1) rowCount

rowPairTransformInteger :: Int -> Integer -> MV.MVector s Integer -> Int -> Int -> Integer -> Integer -> Integer -> Integer -> Int -> ST s ()
rowPairTransformInteger columnCount modulusValue entries leftRow rightRow aa ab ba bb columnIndex
  | columnIndex >= columnCount = pure ()
  | otherwise = do
      let leftIndex = flatIndex columnCount leftRow columnIndex
          rightIndex = flatIndex columnCount rightRow columnIndex
      leftValue <- MV.read entries leftIndex
      rightValue <- MV.read entries rightIndex
      MV.write entries leftIndex (centerResidue modulusValue (aa * leftValue + ab * rightValue))
      MV.write entries rightIndex (centerResidue modulusValue (ba * leftValue + bb * rightValue))
      rowPairTransformInteger columnCount modulusValue entries leftRow rightRow aa ab ba bb (columnIndex + 1)

columnPairTransformInteger :: Int -> Integer -> MV.MVector s Integer -> Int -> Int -> Integer -> Integer -> Integer -> Integer -> Int -> Int -> ST s ()
columnPairTransformInteger columnCount modulusValue entries leftColumn rightColumn aa ab ba bb rowIndex rowCount
  | rowIndex >= rowCount = pure ()
  | otherwise = do
      let leftIndex = flatIndex columnCount rowIndex leftColumn
          rightIndex = flatIndex columnCount rowIndex rightColumn
      leftValue <- MV.read entries leftIndex
      rightValue <- MV.read entries rightIndex
      MV.write entries leftIndex (centerResidue modulusValue (aa * leftValue + ab * rightValue))
      MV.write entries rightIndex (centerResidue modulusValue (ba * leftValue + bb * rightValue))
      columnPairTransformInteger columnCount modulusValue entries leftColumn rightColumn aa ab ba bb (rowIndex + 1) rowCount

wordLinearCombination :: (Word64 -> Word64 -> Word64 -> Word64) -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64
wordLinearCombination multiplyMod modulusWord leftCoefficient leftValue rightCoefficient rightValue =
  wordAddMod modulusWord (multiplyMod modulusWord leftCoefficient leftValue) (multiplyMod modulusWord rightCoefficient rightValue)

wordAddMod :: Word64 -> Word64 -> Word64 -> Word64
wordAddMod modulusWord leftValue rightValue =
  let sumValue = leftValue + rightValue
   in if sumValue >= modulusWord
        then sumValue - modulusWord
        else sumValue

wordMul32 :: Word64 -> Word64 -> Word64 -> Word64
wordMul32 modulusWord leftValue rightValue =
  (leftValue * rightValue) `rem` modulusWord

wordMul62 :: Word64 -> Word64 -> Word64 -> Word64
wordMul62 (W64# modulusWord#) (W64# leftValue#) (W64# rightValue#) =
  case timesWord2# (word64ToWord# leftValue#) (word64ToWord# rightValue#) of
    (# highWord#, lowWord# #) ->
      case quotRemWord2# highWord# lowWord# (word64ToWord# modulusWord#) of
        (# _, remainderWord# #) -> W64# (wordToWord64# remainderWord#)

diagonalCoreRows :: [Integer] -> [[Integer]]
diagonalCoreRows factors =
  let coreSize = length factors
   in [ [ if rowIndex == columnIndex then diagonalValueAt factors rowIndex else zero
          | columnIndex <- [0 .. coreSize - 1]
        ]
        | rowIndex <- [0 .. coreSize - 1]
      ]

diagonalFlatEntries :: Int -> Int -> [Integer] -> [Integer]
diagonalFlatEntries rowCount columnCount invariantFactors =
  [ if rowIndex == columnIndex then diagonalValueAt invariantFactors rowIndex else zero
    | rowIndex <- [0 .. rowCount - 1],
      columnIndex <- [0 .. columnCount - 1]
  ]

diagonalValueAt :: [Integer] -> Int -> Integer
diagonalValueAt values indexValue =
  maybe zero id (values !? indexValue)

centerResidue :: Integer -> Integer -> Integer
centerResidue modulusValue value
  | modulusValue <= 1 = value
  | doubled > modulusValue = residueValue - modulusValue
  | otherwise = residueValue
  where
    residueValue = value `mod` modulusValue
    doubled = 2 * residueValue

(!?) :: [a] -> Int -> Maybe a
values !? targetIndex
  | targetIndex < 0 = Nothing
  | otherwise =
      case drop targetIndex values of
        [] -> Nothing
        value : _ -> Just value
