module Moonlight.LinAlg.Pure.Domain.Smith.Witnessed
  ( smithNormalFormWitnessed,
  )
where

import Control.Monad.ST (ST, runST)
import Data.Bifunctor (first)
import Data.Bits (shiftL)
import Data.List ((!?))
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as MU
import Data.Word (Word64)
import GHC.TypeNats (KnownNat)
import Moonlight.Algebra.Pure.Ring (GCDDomain (..))
import Moonlight.Core
  ( MoonlightError (..),
    checkedNonNegativeProduct,
    checkedNonNegativeSum,
  )
import Moonlight.LinAlg.Internal.Backend.Smith (SmithNormalForm (..))
import Moonlight.LinAlg.Pure.Dense.Types
  ( Matrix,
    fromListMatrix,
  )
import qualified Moonlight.LinAlg.Pure.Dense.Types as DenseTypes
import Moonlight.LinAlg.Pure.Domain.Smith.Multimodular
  ( PrimeSweep (..),
    certifiedPrimeSweep,
    integerResidueWord,
    modInverseWord,
    modMul,
    wordPrimeLadder,
  )
import Prelude

data SmithWitnessArena s = SmithWitnessArena
  { smithWitnessRowCount :: !Int,
    smithWitnessColumnCount :: !Int,
    smithWitnessWork :: !(MV.MVector s Integer),
    smithWitnessLeftRows :: !(MV.MVector s Integer),
    smithWitnessRightRows :: !(MV.MVector s Integer),
    smithWitnessLeftInverseRows :: !(MV.MVector s Integer),
    smithWitnessRightInverseRows :: !(MV.MVector s Integer)
  }

data SmithWitnessFailure
  = SmithWitnessBudgetExhausted !String
  | SmithWitnessNormalizationStalled
  | SmithWitnessPivotBecameZero
  | SmithWitnessInexactDivision !String
  | SmithWitnessTransformRecoveryFailed !String
  | SmithWitnessVerificationFailed !String
  deriving stock (Eq, Show)

data SmithWitnessResult
  = SmithWitnessResult ![Integer] ![Integer] ![Integer] ![Integer] ![Integer]
  | SmithWitnessFailed !SmithWitnessFailure

data SmithExactQuotient
  = SmithExactQuotient !Integer
  | SmithInexactQuotient !SmithWitnessFailure

data SmithPivot = SmithPivot
  { smithPivotRowIndex :: !Int,
    smithPivotColumnIndex :: !Int
  }
  deriving stock (Eq, Show)

smithNormalFormWitnessed ::
  forall r c.
  (KnownNat r, KnownNat c) =>
  Matrix r c Integer ->
  Either MoonlightError (SmithNormalForm r c Integer)
smithNormalFormWitnessed matrixValue = do
  rows <- DenseTypes.matrixToRows matrixValue
  let (rowCount, columnCount) = DenseTypes.matrixShape matrixValue
  witnessResult <- runWitnessedSmith rowCount columnCount rows
  case witnessResult of
    SmithWitnessFailed failureValue ->
      Left (InvariantViolation ("Smith witnessed Integer normal form failed: " <> show failureValue))
    SmithWitnessResult leftEntries diagonalEntries rightEntries leftInverseEntries rightInverseEntries -> do
      leftMatrix <- fromListMatrix @r @r leftEntries
      diagonalMatrix <- fromListMatrix @r @c diagonalEntries
      rightMatrix <- fromListMatrix @c @c rightEntries
      leftInverseMatrix <- fromListMatrix @r @r leftInverseEntries
      rightInverseMatrix <- fromListMatrix @c @c rightInverseEntries
      pure
        SmithNormalForm
          { smithLeft = leftMatrix,
            smithDiagonal = diagonalMatrix,
            smithRight = rightMatrix,
            smithLeftInverse = leftInverseMatrix,
            smithRightInverse = rightInverseMatrix
          }

runWitnessedSmith :: Int -> Int -> [[Integer]] -> Either MoonlightError SmithWitnessResult
runWitnessedSmith rowCount columnCount rows = do
  validateSmithWitnessCardinalities rowCount columnCount
  if rowCount == columnCount && rowCount >= fastWitnessSizeFloor
    then do
      primeSweep <- certifiedPrimeSweep rowCount columnCount rows
      case primeSweepDeterminant primeSweep of
        Just determinantValue
          | primeSweepRank primeSweep == rowCount && determinantValue /= 0 ->
              pure (runFastNonsingularWitnessedSmith rowCount determinantValue rows)
        _ -> pure (runAlternatingWitnessedSmith rowCount columnCount (concat rows))
    else pure (runAlternatingWitnessedSmith rowCount columnCount (concat rows))

validateSmithWitnessCardinalities :: Int -> Int -> Either MoonlightError ()
validateSmithWitnessCardinalities rowCount columnCount = do
  matrixEntryCount <- checkedWitnessProduct "matrix entries" rowCount columnCount
  rowWitnessEntryCount <- checkedWitnessProduct "left witness entries" rowCount rowCount
  columnWitnessEntryCount <- checkedWitnessProduct "right witness entries" columnCount columnCount
  _ <- checkedWitnessProduct "normalization budget" matrixEntryCount 16
  _ <- checkedWitnessProduct "divisibility-chain budget" (min rowCount columnCount) (min rowCount columnCount)
  dimensionSum <- checkedWitnessSum "alternation dimension sum" rowCount columnCount
  doubledDimensionSum <- checkedWitnessProduct "alternation budget" 2 dimensionSum
  _ <- checkedWitnessSum "alternation budget" 64 doubledDimensionSum
  doubledMaximumDimension <- checkedWitnessProduct "augmented witness width" 2 (max rowCount columnCount)
  _ <- checkedWitnessSum "fast witness budget" 8 doubledMaximumDimension
  _ <- checkedWitnessProduct "augmented left witness entries" rowWitnessEntryCount 2
  _ <- checkedWitnessProduct "augmented right witness entries" columnWitnessEntryCount 2
  Right ()

checkedWitnessProduct :: String -> Int -> Int -> Either MoonlightError Int
checkedWitnessProduct context leftFactor rightFactor =
  first
    (const (InvariantViolation ("Smith witnessed " <> context <> " exceed Int cardinality")))
    (checkedNonNegativeProduct leftFactor rightFactor)

checkedWitnessSum :: String -> Int -> Int -> Either MoonlightError Int
checkedWitnessSum context leftTerm rightTerm =
  first
    (const (InvariantViolation ("Smith witnessed " <> context <> " exceed Int cardinality")))
    (checkedNonNegativeSum leftTerm rightTerm)

fastWitnessSizeFloor :: Int
fastWitnessSizeFloor = 25

runAlternatingWitnessedSmith :: Int -> Int -> [Integer] -> SmithWitnessResult
runAlternatingWitnessedSmith rowCount columnCount entries =
  runST $ do
    arenaValue <- newSmithWitnessArena rowCount columnCount entries
    stepFailure <- alternatingHermiteMutable (alternationBudget arenaValue) arenaValue
    case stepFailure of
      Just failureValue -> pure (SmithWitnessFailed failureValue)
      Nothing -> do
        chainFailure <- enforceDivisibilityChainMutable arenaValue
        case chainFailure of
          Just failureValue -> pure (SmithWitnessFailed failureValue)
          Nothing -> do
            normalizeFailure <- normalizeDiagonalUnitsMutable arenaValue
            case normalizeFailure of
              Just failureValue -> pure (SmithWitnessFailed failureValue)
              Nothing ->
                SmithWitnessResult
                  <$> readFlatVector (smithWitnessLeftRows arenaValue)
                  <*> readFlatVector (smithWitnessWork arenaValue)
                  <*> readFlatVector (smithWitnessRightRows arenaValue)
                  <*> readFlatVector (smithWitnessLeftInverseRows arenaValue)
                  <*> readFlatVector (smithWitnessRightInverseRows arenaValue)

data FastWitnessState = FastWitnessState
  { fastWitnessWork :: ![Integer],
    fastWitnessLeft :: ![Integer],
    fastWitnessLeftTimesOriginal :: ![Integer],
    fastWitnessRight :: ![Integer],
    fastWitnessLeftInverse :: ![Integer],
    fastWitnessRightInverse :: ![Integer]
  }
  deriving stock (Eq, Show)

data FastTransformOrientation
  = FastLeftTimesInverseRight
  | FastInverseLeftTimesRight
  deriving stock (Eq, Show)

runFastNonsingularWitnessedSmith :: Int -> Integer -> [[Integer]] -> SmithWitnessResult
runFastNonsingularWitnessedSmith matrixSize determinantValue rows =
  case fastNonsingularWitness matrixSize determinantValue rows of
    Left failureValue -> SmithWitnessFailed failureValue
    Right stateValue -> finalizeFastWitness matrixSize stateValue


fastNonsingularWitness :: Int -> Integer -> [[Integer]] -> Either SmithWitnessFailure FastWitnessState
fastNonsingularWitness matrixSize determinantValue rows =
  fastWitnessStep matrixSize modulusValue originalEntries (fastWitnessBudget matrixSize) initialState
  where
    modulusValue :: Integer
    modulusValue = 2 * abs determinantValue

    originalEntries :: [Integer]
    originalEntries = concat rows

    initialState :: FastWitnessState
    initialState =
      FastWitnessState
        { fastWitnessWork = originalEntries,
          fastWitnessLeft = identityList matrixSize,
          fastWitnessLeftTimesOriginal = originalEntries,
          fastWitnessRight = identityList matrixSize,
          fastWitnessLeftInverse = identityList matrixSize,
          fastWitnessRightInverse = identityList matrixSize
        }

fastWitnessBudget :: Int -> Int
fastWitnessBudget matrixSize =
  8 + 2 * matrixSize

fastWitnessStep :: Int -> Integer -> [Integer] -> Int -> FastWitnessState -> Either SmithWitnessFailure FastWitnessState
fastWitnessStep matrixSize modulusValue originalEntries remainingBudget stateValue
  | remainingBudget <= 0 = Left (SmithWitnessBudgetExhausted "mod-det witnessed alternation")
  | matrixIsDiagonal matrixSize (fastWitnessWork stateValue) = completeFastWitness matrixSize originalEntries stateValue
  | otherwise = do
      rowState <- fastWitnessRowHermite matrixSize modulusValue stateValue
      columnState <- fastWitnessColumnHermite matrixSize modulusValue rowState
      if fastWitnessWork columnState == fastWitnessWork stateValue
        then
          if matrixIsDiagonal matrixSize (fastWitnessWork columnState)
            then completeFastWitness matrixSize originalEntries columnState
            else Left SmithWitnessNormalizationStalled
        else fastWitnessStep matrixSize modulusValue originalEntries (remainingBudget - 1) columnState




fastWitnessRowHermite :: Int -> Integer -> FastWitnessState -> Either SmithWitnessFailure FastWitnessState
fastWitnessRowHermite matrixSize modulusValue stateValue = do
  rowHermiteEntries <- rowHermiteModulo matrixSize modulusValue (fastWitnessWork stateValue)
  if rowHermiteEntries == fastWitnessWork stateValue
    then Right stateValue
    else do
      rowTransform <-
        recoverTransform
            matrixSize
            FastLeftTimesInverseRight
            rowHermiteEntries
            (fastWitnessWork stateValue)
            (transformRecoveryBound matrixSize modulusValue rowHermiteEntries (fastWitnessWork stateValue))
            "row HNF transform"
      Right
        stateValue
          { fastWitnessWork = rowHermiteEntries,
            fastWitnessLeft = composeFastFactor matrixSize rowTransform (fastWitnessLeft stateValue),
            fastWitnessLeftTimesOriginal =
              if fastWitnessLeftTimesOriginal stateValue == fastWitnessWork stateValue
                then rowHermiteEntries
                else composeFastFactor matrixSize rowTransform (fastWitnessLeftTimesOriginal stateValue)
          }

fastWitnessColumnHermite :: Int -> Integer -> FastWitnessState -> Either SmithWitnessFailure FastWitnessState
fastWitnessColumnHermite matrixSize modulusValue stateValue = do
  columnHermiteEntries <- columnHermiteModulo matrixSize modulusValue (fastWitnessWork stateValue)
  if columnHermiteEntries == fastWitnessWork stateValue
    then Right stateValue
    else do
      columnTransform <-
        recoverTransform
            matrixSize
            FastInverseLeftTimesRight
            (fastWitnessWork stateValue)
            columnHermiteEntries
            (transformRecoveryBound matrixSize modulusValue columnHermiteEntries (fastWitnessWork stateValue))
            "column HNF transform"
      Right
        stateValue
          { fastWitnessWork = columnHermiteEntries,
            fastWitnessRight = composeFastFactor matrixSize (fastWitnessRight stateValue) columnTransform
          }

composeFastFactor :: Int -> [Integer] -> [Integer] -> [Integer]
composeFastFactor matrixSize leftEntries rightEntries
  | leftEntries == identityList matrixSize = rightEntries
  | rightEntries == identityList matrixSize = leftEntries
  | otherwise = matrixProduct matrixSize matrixSize matrixSize leftEntries rightEntries

completeFastWitness :: Int -> [Integer] -> FastWitnessState -> Either SmithWitnessFailure FastWitnessState
completeFastWitness matrixSize originalEntries stateValue = do
  let leftTimesOriginal = fastWitnessLeftTimesOriginal stateValue
      originalTimesRight = matrixProduct matrixSize matrixSize matrixSize originalEntries (fastWitnessRight stateValue)
      diagonalValues = [valueAt (fastWitnessWork stateValue) (flatIndex matrixSize axisIndex axisIndex) | axisIndex <- [0 .. matrixSize - 1]]
  leftInverseEntries <- divideColumnsByDiagonal matrixSize "left inverse diagonal division" diagonalValues originalTimesRight
  rightInverseEntries <- divideRowsByDiagonal matrixSize "right inverse diagonal division" diagonalValues leftTimesOriginal
  Right
    stateValue
      { fastWitnessLeftInverse = leftInverseEntries,
        fastWitnessRightInverse = rightInverseEntries
      }

divideColumnsByDiagonal :: Int -> String -> [Integer] -> [Integer] -> Either SmithWitnessFailure [Integer]
divideColumnsByDiagonal matrixSize context diagonalValues entries =
  traverse divideEntry (zip [0 ..] entries)
  where
    divideEntry :: (Int, Integer) -> Either SmithWitnessFailure Integer
    divideEntry (entryIndex, entryValue) =
      case exactQuotientMutable context entryValue (valueAt diagonalValues (entryIndex `rem` matrixSize)) of
        SmithExactQuotient quotientValue -> Right quotientValue
        SmithInexactQuotient failureValue -> Left failureValue

divideRowsByDiagonal :: Int -> String -> [Integer] -> [Integer] -> Either SmithWitnessFailure [Integer]
divideRowsByDiagonal matrixSize context diagonalValues entries =
  traverse divideEntry (zip [0 ..] entries)
  where
    divideEntry :: (Int, Integer) -> Either SmithWitnessFailure Integer
    divideEntry (entryIndex, entryValue) =
      case exactQuotientMutable context entryValue (valueAt diagonalValues (entryIndex `quot` matrixSize)) of
        SmithExactQuotient quotientValue -> Right quotientValue
        SmithInexactQuotient failureValue -> Left failureValue

finalizeFastWitness :: Int -> FastWitnessState -> SmithWitnessResult
finalizeFastWitness matrixSize stateValue =
  runST $ do
    arenaValue <-
      newSmithWitnessArenaFromWitnesses
        matrixSize
        (fastWitnessWork stateValue)
        (fastWitnessLeft stateValue)
        (fastWitnessRight stateValue)
        (fastWitnessLeftInverse stateValue)
        (fastWitnessRightInverse stateValue)
    chainFailure <- enforceDivisibilityChainMutable arenaValue
    case chainFailure of
      Just failureValue -> pure (SmithWitnessFailed failureValue)
      Nothing -> do
        normalizeFailure <- normalizeDiagonalUnitsMutable arenaValue
        case normalizeFailure of
          Just failureValue -> pure (SmithWitnessFailed failureValue)
          Nothing ->
            SmithWitnessResult
              <$> readFlatVector (smithWitnessLeftRows arenaValue)
              <*> readFlatVector (smithWitnessWork arenaValue)
              <*> readFlatVector (smithWitnessRightRows arenaValue)
              <*> readFlatVector (smithWitnessLeftInverseRows arenaValue)
              <*> readFlatVector (smithWitnessRightInverseRows arenaValue)

newSmithWitnessArenaFromWitnesses :: Int -> [Integer] -> [Integer] -> [Integer] -> [Integer] -> [Integer] -> ST s (SmithWitnessArena s)
newSmithWitnessArenaFromWitnesses matrixSize workEntries leftEntries rightEntries leftInverseEntries rightInverseEntries = do
  work <- V.thaw (V.fromList workEntries)
  leftRows <- V.thaw (V.fromList leftEntries)
  rightRows <- V.thaw (V.fromList rightEntries)
  leftInverseRows <- V.thaw (V.fromList leftInverseEntries)
  rightInverseRows <- V.thaw (V.fromList rightInverseEntries)
  pure
    SmithWitnessArena
      { smithWitnessRowCount = matrixSize,
        smithWitnessColumnCount = matrixSize,
        smithWitnessWork = work,
        smithWitnessLeftRows = leftRows,
        smithWitnessRightRows = rightRows,
        smithWitnessLeftInverseRows = leftInverseRows,
        smithWitnessRightInverseRows = rightInverseRows
      }

rowHermiteModulo :: Int -> Integer -> [Integer] -> Either SmithWitnessFailure [Integer]
rowHermiteModulo matrixSize modulusValue entries =
  runST $ do
    work <- V.thaw (V.fromList (fmap (centerResidue modulusValue) entries <> fmap (modulusValue *) (identityList matrixSize)))
    failureValue <- rowHermiteModuloAt matrixSize modulusValue work 0
    case failureValue of
      Just hermiteFailure -> pure (Left hermiteFailure)
      Nothing -> Right . take (matrixSize * matrixSize) <$> readFlatVector work

columnHermiteModulo :: Int -> Integer -> [Integer] -> Either SmithWitnessFailure [Integer]
columnHermiteModulo matrixSize modulusValue entries =
  runST $ do
    work <- V.thaw (V.fromList (augmentedColumnPool matrixSize modulusValue entries))
    failureValue <- columnHermiteModuloAt matrixSize modulusValue work 0
    case failureValue of
      Just hermiteFailure -> pure (Left hermiteFailure)
      Nothing -> Right . extractColumnPool matrixSize <$> V.freeze work

augmentedColumnPool :: Int -> Integer -> [Integer] -> [Integer]
augmentedColumnPool matrixSize modulusValue entries =
  concat
    [ fmap (centerResidue modulusValue) (take matrixSize (drop (rowIndex * matrixSize) entries))
        <> [if columnIndex == rowIndex then modulusValue else 0 | columnIndex <- [0 .. matrixSize - 1]]
      | rowIndex <- [0 .. matrixSize - 1]
    ]

extractColumnPool :: Int -> V.Vector Integer -> [Integer]
extractColumnPool matrixSize pool =
  [ vectorValueAt pool (flatIndex (2 * matrixSize) rowIndex columnIndex)
    | rowIndex <- [0 .. matrixSize - 1],
      columnIndex <- [0 .. matrixSize - 1]
  ]

rowHermiteModuloAt :: forall s. Int -> Integer -> MV.MVector s Integer -> Int -> ST s (Maybe SmithWitnessFailure)
rowHermiteModuloAt matrixSize modulusValue work pivotIndex
  | pivotIndex >= matrixSize = pure Nothing
  | otherwise = do
      pivotCandidate <- findColumnNonZeroModulo (2 * matrixSize) matrixSize work pivotIndex pivotIndex
      case pivotCandidate of
        Nothing -> pure (Just SmithWitnessPivotBecameZero)
        Just pivotRow -> do
          swapRowsVector matrixSize work pivotIndex pivotRow pivotIndex
          signFailure <- normalizeModuloPivotRow matrixSize modulusValue work pivotIndex
          case signFailure of
            Just failureValue -> pure (Just failureValue)
            Nothing -> do
              clearFailure <- clearColumnBelowModulo (2 * matrixSize) matrixSize modulusValue work pivotIndex
              case clearFailure of
                Just failureValue -> pure (Just failureValue)
                Nothing -> do
                  finalSignFailure <- normalizeModuloPivotRow matrixSize modulusValue work pivotIndex
                  case finalSignFailure of
                    Just failureValue -> pure (Just failureValue)
                    Nothing -> do
                      reduceColumnAboveModulo matrixSize modulusValue work pivotIndex
                      rowHermiteModuloAt matrixSize modulusValue work (pivotIndex + 1)

columnHermiteModuloAt :: forall s. Int -> Integer -> MV.MVector s Integer -> Int -> ST s (Maybe SmithWitnessFailure)
columnHermiteModuloAt matrixSize modulusValue work pivotIndex
  | pivotIndex >= matrixSize = pure Nothing
  | otherwise = do
      pivotCandidate <- findRowNonZeroModulo (2 * matrixSize) work pivotIndex pivotIndex
      case pivotCandidate of
        Nothing -> pure (Just SmithWitnessPivotBecameZero)
        Just pivotColumn -> do
          swapColumnsVector (2 * matrixSize) work pivotIndex pivotColumn pivotIndex matrixSize
          signFailure <- normalizeModuloPivotColumn (2 * matrixSize) matrixSize modulusValue work pivotIndex
          case signFailure of
            Just failureValue -> pure (Just failureValue)
            Nothing -> do
              clearFailure <- clearRowRightModulo (2 * matrixSize) matrixSize modulusValue work pivotIndex
              case clearFailure of
                Just failureValue -> pure (Just failureValue)
                Nothing -> do
                  finalSignFailure <- normalizeModuloPivotColumn (2 * matrixSize) matrixSize modulusValue work pivotIndex
                  case finalSignFailure of
                    Just failureValue -> pure (Just failureValue)
                    Nothing -> do
                      reduceRowLeftModulo (2 * matrixSize) matrixSize modulusValue work pivotIndex
                      columnHermiteModuloAt matrixSize modulusValue work (pivotIndex + 1)

findColumnNonZeroModulo :: forall s. Int -> Int -> MV.MVector s Integer -> Int -> Int -> ST s (Maybe Int)
findColumnNonZeroModulo rowCount columnCount work columnIndex rowIndex
  | rowIndex >= rowCount = pure Nothing
  | otherwise = do
      entryValue <- MV.unsafeRead work (flatIndex columnCount rowIndex columnIndex)
      if entryValue == 0
        then findColumnNonZeroModulo rowCount columnCount work columnIndex (rowIndex + 1)
        else pure (Just rowIndex)

findRowNonZeroModulo :: forall s. Int -> MV.MVector s Integer -> Int -> Int -> ST s (Maybe Int)
findRowNonZeroModulo poolColumnCount work rowIndex columnIndex
  | columnIndex >= poolColumnCount = pure Nothing
  | otherwise = do
      entryValue <- MV.unsafeRead work (flatIndex poolColumnCount rowIndex columnIndex)
      if entryValue == 0
        then findRowNonZeroModulo poolColumnCount work rowIndex (columnIndex + 1)
        else pure (Just columnIndex)

normalizeModuloPivotRow :: forall s. Int -> Integer -> MV.MVector s Integer -> Int -> ST s (Maybe SmithWitnessFailure)
normalizeModuloPivotRow matrixSize modulusValue work pivotIndex = do
  pivotValue <- MV.unsafeRead work (flatIndex matrixSize pivotIndex pivotIndex)
  if pivotValue < 0
    then scaleRowModuloVector matrixSize modulusValue work pivotIndex (-1) pivotIndex *> pure Nothing
    else pure Nothing

normalizeModuloPivotColumn :: forall s. Int -> Int -> Integer -> MV.MVector s Integer -> Int -> ST s (Maybe SmithWitnessFailure)
normalizeModuloPivotColumn poolColumnCount rowCount modulusValue work pivotIndex = do
  pivotValue <- MV.unsafeRead work (flatIndex poolColumnCount pivotIndex pivotIndex)
  if pivotValue < 0
    then scaleColumnModuloVector poolColumnCount modulusValue work pivotIndex (-1) pivotIndex rowCount *> pure Nothing
    else pure Nothing

clearColumnBelowModulo :: forall s. Int -> Int -> Integer -> MV.MVector s Integer -> Int -> ST s (Maybe SmithWitnessFailure)
clearColumnBelowModulo rowCount columnCount modulusValue work pivotIndex =
  scanRows (pivotIndex + 1)
  where
    scanRows :: Int -> ST s (Maybe SmithWitnessFailure)
    scanRows rowIndex
      | rowIndex >= rowCount = pure Nothing
      | otherwise = do
          entryValue <- MV.unsafeRead work (flatIndex columnCount rowIndex pivotIndex)
          if entryValue == 0
            then scanRows (rowIndex + 1)
            else do
              pivotValue <- MV.unsafeRead work (flatIndex columnCount pivotIndex pivotIndex)
              if pivotValue == 0
                then pure (Just SmithWitnessPivotBecameZero)
                else do
                  let (quotientValue, _) = balancedDivMod entryValue pivotValue
                  if quotientValue /= 0
                    then rowCombineModuloVector columnCount modulusValue work rowIndex pivotIndex quotientValue pivotIndex
                    else pure ()
                  reducedEntry <- MV.unsafeRead work (flatIndex columnCount rowIndex pivotIndex)
                  if reducedEntry == 0
                    then scanRows (rowIndex + 1)
                    else do
                      gcdFailure <- gcdCombineRowsModulo columnCount modulusValue work pivotIndex rowIndex pivotIndex
                      case gcdFailure of
                        Just failureValue -> pure (Just failureValue)
                        Nothing -> scanRows (rowIndex + 1)

clearRowRightModulo :: forall s. Int -> Int -> Integer -> MV.MVector s Integer -> Int -> ST s (Maybe SmithWitnessFailure)
clearRowRightModulo poolColumnCount rowCount modulusValue work pivotIndex =
  scanColumns (pivotIndex + 1)
  where
    scanColumns :: Int -> ST s (Maybe SmithWitnessFailure)
    scanColumns columnIndex
      | columnIndex >= poolColumnCount = pure Nothing
      | otherwise = do
          entryValue <- MV.unsafeRead work (flatIndex poolColumnCount pivotIndex columnIndex)
          if entryValue == 0
            then scanColumns (columnIndex + 1)
            else do
              pivotValue <- MV.unsafeRead work (flatIndex poolColumnCount pivotIndex pivotIndex)
              if pivotValue == 0
                then pure (Just SmithWitnessPivotBecameZero)
                else do
                  let (quotientValue, _) = balancedDivMod entryValue pivotValue
                  if quotientValue /= 0
                    then columnCombineModuloVector poolColumnCount modulusValue work columnIndex pivotIndex quotientValue pivotIndex rowCount
                    else pure ()
                  reducedEntry <- MV.unsafeRead work (flatIndex poolColumnCount pivotIndex columnIndex)
                  if reducedEntry == 0
                    then scanColumns (columnIndex + 1)
                    else do
                      gcdFailure <- gcdCombineColumnsModulo poolColumnCount rowCount modulusValue work pivotIndex pivotIndex columnIndex
                      case gcdFailure of
                        Just failureValue -> pure (Just failureValue)
                        Nothing -> scanColumns (columnIndex + 1)

gcdCombineRowsModulo :: forall s. Int -> Integer -> MV.MVector s Integer -> Int -> Int -> Int -> ST s (Maybe SmithWitnessFailure)
gcdCombineRowsModulo matrixSize modulusValue work pivotRow candidateRow pivotColumn = do
  pivotValue <- MV.unsafeRead work (flatIndex matrixSize pivotRow pivotColumn)
  entryValue <- MV.unsafeRead work (flatIndex matrixSize candidateRow pivotColumn)
  let (gcdValue, pivotCoefficient, entryCoefficient) = extendedGcdDomain pivotValue entryValue
  case (exactQuotientMutable "row mod-det gcd pivot quotient" pivotValue gcdValue, exactQuotientMutable "row mod-det gcd entry quotient" entryValue gcdValue) of
    (SmithExactQuotient pivotQuotient, SmithExactQuotient entryQuotient) -> do
      rowPairTransformModuloVector matrixSize modulusValue work pivotRow candidateRow pivotCoefficient entryCoefficient (negate entryQuotient) pivotQuotient pivotColumn
      pure Nothing
    (SmithInexactQuotient failureValue, _) -> pure (Just failureValue)
    (_, SmithInexactQuotient failureValue) -> pure (Just failureValue)

gcdCombineColumnsModulo :: forall s. Int -> Int -> Integer -> MV.MVector s Integer -> Int -> Int -> Int -> ST s (Maybe SmithWitnessFailure)
gcdCombineColumnsModulo poolColumnCount rowCount modulusValue work pivotRow pivotColumn candidateColumn = do
  pivotValue <- MV.unsafeRead work (flatIndex poolColumnCount pivotRow pivotColumn)
  entryValue <- MV.unsafeRead work (flatIndex poolColumnCount pivotRow candidateColumn)
  let (gcdValue, pivotCoefficient, entryCoefficient) = extendedGcdDomain pivotValue entryValue
  case (exactQuotientMutable "column mod-det gcd pivot quotient" pivotValue gcdValue, exactQuotientMutable "column mod-det gcd entry quotient" entryValue gcdValue) of
    (SmithExactQuotient pivotQuotient, SmithExactQuotient entryQuotient) -> do
      columnPairTransformModuloVector poolColumnCount modulusValue work pivotColumn candidateColumn pivotCoefficient entryCoefficient (negate entryQuotient) pivotQuotient pivotRow rowCount
      pure Nothing
    (SmithInexactQuotient failureValue, _) -> pure (Just failureValue)
    (_, SmithInexactQuotient failureValue) -> pure (Just failureValue)

reduceColumnAboveModulo :: forall s. Int -> Integer -> MV.MVector s Integer -> Int -> ST s ()
reduceColumnAboveModulo matrixSize modulusValue work pivotIndex =
  scanRows 0
  where
    scanRows :: Int -> ST s ()
    scanRows rowIndex
      | rowIndex >= pivotIndex = pure ()
      | otherwise = do
          pivotValue <- MV.unsafeRead work (flatIndex matrixSize pivotIndex pivotIndex)
          entryValue <- MV.unsafeRead work (flatIndex matrixSize rowIndex pivotIndex)
          if pivotValue == 0
            then scanRows (rowIndex + 1)
            else do
              let quotientValue = entryValue `div` pivotValue
              if quotientValue /= 0
                then rowCombineModuloVector matrixSize modulusValue work rowIndex pivotIndex quotientValue pivotIndex
                else pure ()
              scanRows (rowIndex + 1)

reduceRowLeftModulo :: forall s. Int -> Int -> Integer -> MV.MVector s Integer -> Int -> ST s ()
reduceRowLeftModulo poolColumnCount rowCount modulusValue work pivotIndex =
  scanColumns 0
  where
    scanColumns :: Int -> ST s ()
    scanColumns columnIndex
      | columnIndex >= pivotIndex = pure ()
      | otherwise = do
          pivotValue <- MV.unsafeRead work (flatIndex poolColumnCount pivotIndex pivotIndex)
          entryValue <- MV.unsafeRead work (flatIndex poolColumnCount pivotIndex columnIndex)
          if pivotValue == 0
            then scanColumns (columnIndex + 1)
            else do
              let quotientValue = entryValue `div` pivotValue
              if quotientValue /= 0
                then columnCombineModuloVector poolColumnCount modulusValue work columnIndex pivotIndex quotientValue pivotIndex rowCount
                else pure ()
              scanColumns (columnIndex + 1)

rowCombineModuloVector :: Int -> Integer -> MV.MVector s Integer -> Int -> Int -> Integer -> Int -> ST s ()
rowCombineModuloVector columnCount modulusValue entries targetRow sourceRow coefficient columnIndex
  | columnIndex >= columnCount = pure ()
  | otherwise = do
      let targetIndex = flatIndex columnCount targetRow columnIndex
          sourceIndex = flatIndex columnCount sourceRow columnIndex
      sourceValue <- MV.unsafeRead entries sourceIndex
      if sourceValue == 0
        then pure ()
        else do
          targetValue <- MV.unsafeRead entries targetIndex
          MV.unsafeWrite entries targetIndex (centerResidue modulusValue (targetValue - coefficient * sourceValue))
      rowCombineModuloVector columnCount modulusValue entries targetRow sourceRow coefficient (columnIndex + 1)

columnCombineModuloVector :: Int -> Integer -> MV.MVector s Integer -> Int -> Int -> Integer -> Int -> Int -> ST s ()
columnCombineModuloVector columnCount modulusValue entries targetColumn sourceColumn coefficient rowIndex rowCount
  | rowIndex >= rowCount = pure ()
  | otherwise = do
      let targetIndex = flatIndex columnCount rowIndex targetColumn
          sourceIndex = flatIndex columnCount rowIndex sourceColumn
      sourceValue <- MV.unsafeRead entries sourceIndex
      if sourceValue == 0
        then pure ()
        else do
          targetValue <- MV.unsafeRead entries targetIndex
          MV.unsafeWrite entries targetIndex (centerResidue modulusValue (targetValue - coefficient * sourceValue))
      columnCombineModuloVector columnCount modulusValue entries targetColumn sourceColumn coefficient (rowIndex + 1) rowCount

rowPairTransformModuloVector :: Int -> Integer -> MV.MVector s Integer -> Int -> Int -> Integer -> Integer -> Integer -> Integer -> Int -> ST s ()
rowPairTransformModuloVector columnCount modulusValue entries leftRow rightRow aa ab ba bb columnIndex
  | columnIndex >= columnCount = pure ()
  | otherwise = do
      let leftIndex = flatIndex columnCount leftRow columnIndex
          rightIndex = flatIndex columnCount rightRow columnIndex
      leftValue <- MV.unsafeRead entries leftIndex
      rightValue <- MV.unsafeRead entries rightIndex
      if leftValue == 0 && rightValue == 0
        then pure ()
        else do
          MV.unsafeWrite entries leftIndex (centerResidue modulusValue (aa * leftValue + ab * rightValue))
          MV.unsafeWrite entries rightIndex (centerResidue modulusValue (ba * leftValue + bb * rightValue))
      rowPairTransformModuloVector columnCount modulusValue entries leftRow rightRow aa ab ba bb (columnIndex + 1)

columnPairTransformModuloVector :: Int -> Integer -> MV.MVector s Integer -> Int -> Int -> Integer -> Integer -> Integer -> Integer -> Int -> Int -> ST s ()
columnPairTransformModuloVector columnCount modulusValue entries leftColumn rightColumn aa ab ba bb rowIndex rowCount
  | rowIndex >= rowCount = pure ()
  | otherwise = do
      let leftIndex = flatIndex columnCount rowIndex leftColumn
          rightIndex = flatIndex columnCount rowIndex rightColumn
      leftValue <- MV.unsafeRead entries leftIndex
      rightValue <- MV.unsafeRead entries rightIndex
      if leftValue == 0 && rightValue == 0
        then pure ()
        else do
          MV.unsafeWrite entries leftIndex (centerResidue modulusValue (aa * leftValue + ab * rightValue))
          MV.unsafeWrite entries rightIndex (centerResidue modulusValue (ba * leftValue + bb * rightValue))
      columnPairTransformModuloVector columnCount modulusValue entries leftColumn rightColumn aa ab ba bb (rowIndex + 1) rowCount

scaleRowModuloVector :: Int -> Integer -> MV.MVector s Integer -> Int -> Integer -> Int -> ST s ()
scaleRowModuloVector columnCount modulusValue entries rowIndex factor columnIndex
  | columnIndex >= columnCount = pure ()
  | otherwise = do
      let entryIndex = flatIndex columnCount rowIndex columnIndex
      entryValue <- MV.unsafeRead entries entryIndex
      MV.unsafeWrite entries entryIndex (centerResidue modulusValue (factor * entryValue))
      scaleRowModuloVector columnCount modulusValue entries rowIndex factor (columnIndex + 1)

scaleColumnModuloVector :: Int -> Integer -> MV.MVector s Integer -> Int -> Integer -> Int -> Int -> ST s ()
scaleColumnModuloVector columnCount modulusValue entries columnIndex factor rowIndex rowCount
  | rowIndex >= rowCount = pure ()
  | otherwise = do
      let entryIndex = flatIndex columnCount rowIndex columnIndex
      entryValue <- MV.unsafeRead entries entryIndex
      MV.unsafeWrite entries entryIndex (centerResidue modulusValue (factor * entryValue))
      scaleColumnModuloVector columnCount modulusValue entries columnIndex factor (rowIndex + 1) rowCount

recoverTransform :: Int -> FastTransformOrientation -> [Integer] -> [Integer] -> Integer -> String -> Either SmithWitnessFailure [Integer]
recoverTransform matrixSize orientation leftEntries rightEntries coefficientBound context =
  searchPrimes initialResidues 1 Nothing wordPrimeLadder
  where
    leftVector :: V.Vector Integer
    leftVector = V.fromList leftEntries

    rightVector :: V.Vector Integer
    rightVector = V.fromList rightEntries

    target :: Integer
    target = max 2 (2 * coefficientBound + 1)

    initialResidues :: V.Vector Integer
    initialResidues = V.replicate (matrixSize * matrixSize) 0

    leftIsUpperTriangular :: Bool
    leftIsUpperTriangular = matrixIsUpperTriangularVector matrixSize leftVector

    rightIsLowerTriangular :: Bool
    rightIsLowerTriangular = matrixIsLowerTriangularVector matrixSize rightVector

    solveTransformPrime :: Word64 -> Maybe (U.Vector Word64)
    solveTransformPrime primeValue =
      case orientation of
        FastInverseLeftTimesRight
          | leftIsUpperTriangular -> solveUpperTriangularModuloPrime matrixSize primeValue leftVector rightVector
          | otherwise -> do
              leftInverse <- invertMatrixModuloPrime matrixSize primeValue leftVector
              Just (matrixProductModuloPrime matrixSize primeValue leftInverse (residueVector primeValue rightVector))
        FastLeftTimesInverseRight
          | rightIsLowerTriangular ->
              transposeResidueMatrix matrixSize <$> solveUpperTriangularModuloPrime matrixSize primeValue transposedRight transposedLeft
          | otherwise ->
              transposeResidueMatrix matrixSize <$> solveRightQuotientTransposedModuloPrime matrixSize primeValue rightVector leftVector

    transposedLeft :: V.Vector Integer
    transposedLeft = transposeIntegerMatrix matrixSize leftVector

    transposedRight :: V.Vector Integer
    transposedRight = transposeIntegerMatrix matrixSize rightVector

    verifyCandidate :: Integer -> [Word64] -> [Integer] -> Either SmithWitnessFailure ()
    verifyCandidate knownModulus freshPrimes candidate =
      case orientation of
        FastLeftTimesInverseRight -> verifyProductModuloPrimesFrom knownModulus freshPrimes matrixSize context candidate rightEntries leftEntries
        FastInverseLeftTimesRight -> verifyProductModuloPrimesFrom knownModulus freshPrimes matrixSize context leftEntries candidate rightEntries

    searchPrimes :: V.Vector Integer -> Integer -> Maybe (V.Vector Integer) -> [Word64] -> Either SmithWitnessFailure [Integer]
    searchPrimes residues modulusValue previousLift primes =
      case primes of
        [] -> Left (SmithWitnessTransformRecoveryFailed (context <> ": prime ladder exhausted"))
        primeValue : remainingPrimes ->
          case solveTransformPrime primeValue of
            Nothing -> searchPrimes residues modulusValue previousLift remainingPrimes
            Just primeResidues -> do
              nextResidues <- combineCrtVector residues modulusValue primeValue primeResidues
              let nextModulus = modulusValue * toInteger primeValue
                  candidateLift = V.map (symmetricLiftInteger nextModulus) nextResidues
              if Just candidateLift == previousLift
                then case verifyCandidate nextModulus remainingPrimes (V.toList candidateLift) of
                  Right () -> Right (V.toList candidateLift)
                  Left _ -> continueSearch nextResidues nextModulus candidateLift remainingPrimes
                else continueSearch nextResidues nextModulus candidateLift remainingPrimes

    continueSearch :: V.Vector Integer -> Integer -> V.Vector Integer -> [Word64] -> Either SmithWitnessFailure [Integer]
    continueSearch nextResidues nextModulus candidateLift remainingPrimes
      | nextModulus > target =
          let liftEntries = V.toList candidateLift
           in verifyCandidate nextModulus remainingPrimes liftEntries *> Right liftEntries
      | otherwise = searchPrimes nextResidues nextModulus (Just candidateLift) remainingPrimes

matrixIsUpperTriangularVector :: Int -> V.Vector Integer -> Bool
matrixIsUpperTriangularVector matrixSize entries =
  and
    [ V.unsafeIndex entries (flatIndex matrixSize rowIndex columnIndex) == 0
      | rowIndex <- [1 .. matrixSize - 1],
        columnIndex <- [0 .. rowIndex - 1]
    ]

matrixIsLowerTriangularVector :: Int -> V.Vector Integer -> Bool
matrixIsLowerTriangularVector matrixSize entries =
  and
    [ V.unsafeIndex entries (flatIndex matrixSize rowIndex columnIndex) == 0
      | rowIndex <- [0 .. matrixSize - 2],
        columnIndex <- [rowIndex + 1 .. matrixSize - 1]
    ]

transposeIntegerMatrix :: Int -> V.Vector Integer -> V.Vector Integer
transposeIntegerMatrix matrixSize entries =
  V.generate
    (matrixSize * matrixSize)
    ( \entryIndex ->
        let (rowIndex, columnIndex) = entryIndex `quotRem` matrixSize
         in V.unsafeIndex entries (flatIndex matrixSize columnIndex rowIndex)
    )

transposeResidueMatrix :: Int -> U.Vector Word64 -> U.Vector Word64
transposeResidueMatrix matrixSize entries =
  U.generate
    (matrixSize * matrixSize)
    ( \entryIndex ->
        let (rowIndex, columnIndex) = entryIndex `quotRem` matrixSize
         in U.unsafeIndex entries (flatIndex matrixSize columnIndex rowIndex)
    )

solveUpperTriangularModuloPrime :: Int -> Word64 -> V.Vector Integer -> V.Vector Integer -> Maybe (U.Vector Word64)
solveUpperTriangularModuloPrime matrixSize primeValue leftEntries rightEntries =
  if U.any (== 0) pivotResidues
    then Nothing
    else Just solvedEntries
  where
    leftResidues :: U.Vector Word64
    leftResidues = residueVector primeValue leftEntries

    rightResidues :: U.Vector Word64
    rightResidues = residueVector primeValue rightEntries

    pivotResidues :: U.Vector Word64
    pivotResidues = U.generate matrixSize (\axisIndex -> U.unsafeIndex leftResidues (flatIndex matrixSize axisIndex axisIndex))

    solvedEntries :: U.Vector Word64
    solvedEntries = runST $ do
      work <- MU.replicate (matrixSize * matrixSize) 0
      solveRowsBottomUp matrixSize primeValue leftResidues rightResidues pivotResidues work (matrixSize - 1)
      U.freeze work

solveRowsBottomUp :: forall s. Int -> Word64 -> U.Vector Word64 -> U.Vector Word64 -> U.Vector Word64 -> MU.MVector s Word64 -> Int -> ST s ()
solveRowsBottomUp matrixSize primeValue leftResidues rightResidues pivotResidues work rowIndex
  | rowIndex < 0 = pure ()
  | otherwise = do
      let pivotInverse = modInverseWord primeValue (U.unsafeIndex pivotResidues rowIndex)
      solveRowColumns matrixSize primeValue leftResidues rightResidues work rowIndex pivotInverse 0
      solveRowsBottomUp matrixSize primeValue leftResidues rightResidues pivotResidues work (rowIndex - 1)

solveRowColumns :: forall s. Int -> Word64 -> U.Vector Word64 -> U.Vector Word64 -> MU.MVector s Word64 -> Int -> Word64 -> Int -> ST s ()
solveRowColumns matrixSize primeValue leftResidues rightResidues work rowIndex pivotInverse columnIndex
  | columnIndex >= matrixSize = pure ()
  | otherwise = do
      accumulated <- accumulateSolvedTail matrixSize primeValue leftResidues work rowIndex columnIndex (rowIndex + 1) 0
      let rhsValue = U.unsafeIndex rightResidues (flatIndex matrixSize rowIndex columnIndex)
      MU.unsafeWrite work (flatIndex matrixSize rowIndex columnIndex) (modMul primeValue pivotInverse (modSubWord primeValue rhsValue accumulated))
      solveRowColumns matrixSize primeValue leftResidues rightResidues work rowIndex pivotInverse (columnIndex + 1)

accumulateSolvedTail :: forall s. Int -> Word64 -> U.Vector Word64 -> MU.MVector s Word64 -> Int -> Int -> Int -> Word64 -> ST s Word64
accumulateSolvedTail matrixSize primeValue leftResidues work rowIndex columnIndex sharedIndex accumulator
  | sharedIndex >= matrixSize = pure accumulator
  | otherwise = do
      solvedValue <- MU.unsafeRead work (flatIndex matrixSize sharedIndex columnIndex)
      accumulateSolvedTail matrixSize primeValue leftResidues work rowIndex columnIndex (sharedIndex + 1) (modAddWord primeValue accumulator (modMul primeValue (U.unsafeIndex leftResidues (flatIndex matrixSize rowIndex sharedIndex)) solvedValue))

residueVector :: Word64 -> V.Vector Integer -> U.Vector Word64
residueVector primeValue entries =
  U.generate (V.length entries) (integerResidueWord primeValue . V.unsafeIndex entries)

verifyProductModuloPrimesFrom :: Integer -> [Word64] -> Int -> String -> [Integer] -> [Integer] -> [Integer] -> Either SmithWitnessFailure ()
verifyProductModuloPrimesFrom priorModulus freshPrimes matrixSize context leftEntries rightEntries expectedEntries =
  checkPrimes priorModulus freshPrimes
  where
    leftVector :: V.Vector Integer
    leftVector = V.fromList leftEntries

    rightVector :: V.Vector Integer
    rightVector = V.fromList rightEntries

    expectedVector :: V.Vector Integer
    expectedVector = V.fromList expectedEntries

    entryBound :: Integer
    entryBound =
      toInteger matrixSize * maxAbsEntry leftEntries * maxAbsEntry rightEntries + maxAbsEntry expectedEntries + 1

    checkPrimes :: Integer -> [Word64] -> Either SmithWitnessFailure ()
    checkPrimes modulusValue primes
      | modulusValue > entryBound = Right ()
      | otherwise =
          case primes of
            [] -> Left (SmithWitnessVerificationFailed (context <> ": verification prime ladder exhausted"))
            primeValue : remainingPrimes ->
              let productResidues = matrixProductModuloPrime matrixSize primeValue (residueVector primeValue leftVector) (residueVector primeValue rightVector)
                  expectedResidues = residueVector primeValue expectedVector
               in if productResidues == expectedResidues
                    then checkPrimes (modulusValue * toInteger primeValue) remainingPrimes
                    else Left (SmithWitnessVerificationFailed context)

invertMatrixModuloPrime :: Int -> Word64 -> V.Vector Integer -> Maybe (U.Vector Word64)
invertMatrixModuloPrime matrixSize primeValue entries =
  runST $ do
    work <- MU.replicate (matrixSize * matrixSize * 2) 0
    writeAugmentedModuloMatrix matrixSize primeValue entries work 0
    invertFailure <- invertModuloAt matrixSize primeValue work 0
    case invertFailure of
      Just () -> pure Nothing
      Nothing -> Just <$> readInverseModuloMatrix matrixSize work

solveRightQuotientTransposedModuloPrime :: Int -> Word64 -> V.Vector Integer -> V.Vector Integer -> Maybe (U.Vector Word64)
solveRightQuotientTransposedModuloPrime matrixSize primeValue denominatorEntries numeratorEntries =
  runST $ do
    work <- MU.replicate (matrixSize * matrixSize * 2) 0
    writeAugmentedTransposedPairModuloMatrix matrixSize primeValue denominatorEntries numeratorEntries work 0
    invertFailure <- invertModuloAt matrixSize primeValue work 0
    case invertFailure of
      Just () -> pure Nothing
      Nothing -> Just <$> readInverseModuloMatrix matrixSize work

writeAugmentedTransposedPairModuloMatrix :: forall s. Int -> Word64 -> V.Vector Integer -> V.Vector Integer -> MU.MVector s Word64 -> Int -> ST s ()
writeAugmentedTransposedPairModuloMatrix matrixSize primeValue denominatorEntries numeratorEntries work entryIndex
  | entryIndex >= matrixSize * matrixSize = pure ()
  | otherwise = do
      let (rowIndex, columnIndex) = entryIndex `quotRem` matrixSize
          transposedIndex = flatIndex matrixSize columnIndex rowIndex
      MU.unsafeWrite work (augmentedIndex matrixSize rowIndex columnIndex) (integerResidueWord primeValue (V.unsafeIndex denominatorEntries transposedIndex))
      MU.unsafeWrite work (augmentedIndex matrixSize rowIndex (columnIndex + matrixSize)) (integerResidueWord primeValue (V.unsafeIndex numeratorEntries transposedIndex))
      writeAugmentedTransposedPairModuloMatrix matrixSize primeValue denominatorEntries numeratorEntries work (entryIndex + 1)

writeAugmentedModuloMatrix :: forall s. Int -> Word64 -> V.Vector Integer -> MU.MVector s Word64 -> Int -> ST s ()
writeAugmentedModuloMatrix matrixSize primeValue entries work entryIndex
  | entryIndex >= matrixSize * matrixSize = pure ()
  | otherwise = do
      let (rowIndex, columnIndex) = entryIndex `quotRem` matrixSize
          sourceValue = vectorValueAt entries entryIndex
      MU.unsafeWrite work (augmentedIndex matrixSize rowIndex columnIndex) (integerResidueWord primeValue sourceValue)
      MU.unsafeWrite work (augmentedIndex matrixSize rowIndex (columnIndex + matrixSize)) (if rowIndex == columnIndex then 1 else 0)
      writeAugmentedModuloMatrix matrixSize primeValue entries work (entryIndex + 1)

invertModuloAt :: forall s. Int -> Word64 -> MU.MVector s Word64 -> Int -> ST s (Maybe ())
invertModuloAt matrixSize primeValue work pivotIndex
  | pivotIndex >= matrixSize = pure Nothing
  | otherwise = do
      pivotCandidate <- findModuloPivot matrixSize work pivotIndex pivotIndex
      case pivotCandidate of
        Nothing -> pure (Just ())
        Just pivotRow -> do
          swapAugmentedRows matrixSize work pivotIndex pivotRow 0
          pivotValue <- MU.unsafeRead work (augmentedIndex matrixSize pivotIndex pivotIndex)
          let inversePivot = modInverseWord primeValue pivotValue
          scaleAugmentedRow matrixSize primeValue work pivotIndex inversePivot 0
          eliminateModuloColumn matrixSize primeValue work pivotIndex 0
          invertModuloAt matrixSize primeValue work (pivotIndex + 1)

findModuloPivot :: forall s. Int -> MU.MVector s Word64 -> Int -> Int -> ST s (Maybe Int)
findModuloPivot matrixSize work pivotColumn rowIndex
  | rowIndex >= matrixSize = pure Nothing
  | otherwise = do
      entryValue <- MU.unsafeRead work (augmentedIndex matrixSize rowIndex pivotColumn)
      if entryValue == 0
        then findModuloPivot matrixSize work pivotColumn (rowIndex + 1)
        else pure (Just rowIndex)

swapAugmentedRows :: forall s. Int -> MU.MVector s Word64 -> Int -> Int -> Int -> ST s ()
swapAugmentedRows matrixSize work leftRow rightRow columnIndex
  | leftRow == rightRow = pure ()
  | columnIndex >= 2 * matrixSize = pure ()
  | otherwise = do
      let leftIndex = augmentedIndex matrixSize leftRow columnIndex
          rightIndex = augmentedIndex matrixSize rightRow columnIndex
      leftValue <- MU.unsafeRead work leftIndex
      rightValue <- MU.unsafeRead work rightIndex
      MU.unsafeWrite work leftIndex rightValue
      MU.unsafeWrite work rightIndex leftValue
      swapAugmentedRows matrixSize work leftRow rightRow (columnIndex + 1)

scaleAugmentedRow :: forall s. Int -> Word64 -> MU.MVector s Word64 -> Int -> Word64 -> Int -> ST s ()
scaleAugmentedRow matrixSize primeValue work rowIndex factor columnIndex
  | columnIndex >= 2 * matrixSize = pure ()
  | otherwise = do
      let entryIndex = augmentedIndex matrixSize rowIndex columnIndex
      entryValue <- MU.unsafeRead work entryIndex
      MU.unsafeWrite work entryIndex (modMul primeValue factor entryValue)
      scaleAugmentedRow matrixSize primeValue work rowIndex factor (columnIndex + 1)

eliminateModuloColumn :: forall s. Int -> Word64 -> MU.MVector s Word64 -> Int -> Int -> ST s ()
eliminateModuloColumn matrixSize primeValue work pivotIndex rowIndex
  | rowIndex >= matrixSize = pure ()
  | rowIndex == pivotIndex = eliminateModuloColumn matrixSize primeValue work pivotIndex (rowIndex + 1)
  | otherwise = do
      factor <- MU.unsafeRead work (augmentedIndex matrixSize rowIndex pivotIndex)
      if factor == 0
        then eliminateModuloColumn matrixSize primeValue work pivotIndex (rowIndex + 1)
        else eliminateModuloRow matrixSize primeValue work pivotIndex rowIndex factor 0 *> eliminateModuloColumn matrixSize primeValue work pivotIndex (rowIndex + 1)

eliminateModuloRow :: forall s. Int -> Word64 -> MU.MVector s Word64 -> Int -> Int -> Word64 -> Int -> ST s ()
eliminateModuloRow matrixSize primeValue work pivotRow targetRow factor columnIndex
  | columnIndex >= 2 * matrixSize = pure ()
  | otherwise = do
      let targetIndex = augmentedIndex matrixSize targetRow columnIndex
          pivotEntryIndex = augmentedIndex matrixSize pivotRow columnIndex
      targetValue <- MU.unsafeRead work targetIndex
      pivotValue <- MU.unsafeRead work pivotEntryIndex
      MU.unsafeWrite work targetIndex (modSubWord primeValue targetValue (modMul primeValue factor pivotValue))
      eliminateModuloRow matrixSize primeValue work pivotRow targetRow factor (columnIndex + 1)

readInverseModuloMatrix :: forall s. Int -> MU.MVector s Word64 -> ST s (U.Vector Word64)
readInverseModuloMatrix matrixSize work =
  U.generateM
    (matrixSize * matrixSize)
    ( \entryIndex -> do
        let (rowIndex, columnIndex) = entryIndex `quotRem` matrixSize
        MU.unsafeRead work (augmentedIndex matrixSize rowIndex (columnIndex + matrixSize))
    )

matrixProductModuloPrime :: Int -> Word64 -> U.Vector Word64 -> U.Vector Word64 -> U.Vector Word64
matrixProductModuloPrime matrixSize primeValue leftEntries rightEntries =
  U.generate
    (matrixSize * matrixSize)
    ( \entryIndex ->
        let (rowIndex, columnIndex) = entryIndex `quotRem` matrixSize
         in dotModuloPrime matrixSize primeValue leftEntries rightEntries rowIndex columnIndex 0 0
    )

dotModuloPrime :: Int -> Word64 -> U.Vector Word64 -> U.Vector Word64 -> Int -> Int -> Int -> Word64 -> Word64
dotModuloPrime matrixSize primeValue leftEntries rightEntries rowIndex columnIndex sharedIndex accumulator
  | sharedIndex >= matrixSize = accumulator
  | otherwise =
      let leftValue = U.unsafeIndex leftEntries (flatIndex matrixSize rowIndex sharedIndex)
          nextAccumulator =
            if leftValue == 0
              then accumulator
              else modAddWord primeValue accumulator (modMul primeValue leftValue (U.unsafeIndex rightEntries (flatIndex matrixSize sharedIndex columnIndex)))
       in dotModuloPrime matrixSize primeValue leftEntries rightEntries rowIndex columnIndex (sharedIndex + 1) nextAccumulator

combineCrtVector :: V.Vector Integer -> Integer -> Word64 -> U.Vector Word64 -> Either SmithWitnessFailure (V.Vector Integer)
combineCrtVector residues modulusValue primeValue primeResidues
  | V.length residues /= U.length primeResidues = Left (SmithWitnessTransformRecoveryFailed "CRT residue vector shape mismatch")
  | modulusSection == 0 = Left (SmithWitnessTransformRecoveryFailed "CRT modulus section vanished")
  | otherwise = Right (V.imap (\entryIndex residueValue -> combineEntry residueValue (U.unsafeIndex primeResidues entryIndex)) residues)
  where
    primeInteger :: Integer
    primeInteger = toInteger primeValue

    modulusSection :: Word64
    modulusSection = fromInteger (modulusValue `mod` primeInteger)

    inverseValue :: Integer
    inverseValue = toInteger (modInverseWord primeValue modulusSection)

    combineEntry :: Integer -> Word64 -> Integer
    combineEntry residueValue primeResidue =
      let deltaValue = (toInteger primeResidue - residueValue) `mod` primeInteger
          correction = (deltaValue * inverseValue) `mod` primeInteger
       in residueValue + modulusValue * correction

transformRecoveryBound :: Int -> Integer -> [Integer] -> [Integer] -> Integer
transformRecoveryBound matrixSize modulusValue numeratorEntries denominatorEntries =
  (toInteger matrixSize * maxAbsEntry numeratorEntries * hadamardMinorBoundDimension (matrixSize - 1) matrixSize denominatorEntries) `quot` max 1 (modulusValue `quot` 2) + 1

hadamardMinorBoundDimension :: Int -> Int -> [Integer] -> Integer
hadamardMinorBoundDimension minorDimension columnCount entries =
  powerOfTwoSquareRootBound (product (takeLargestWitness minorDimension (rowSquaredNorms columnCount entries)))

powerOfTwoSquareRootBound :: Integer -> Integer
powerOfTwoSquareRootBound value
  | value <= 1 = max 0 value
  | otherwise = narrow 0 (expand 1)
  where
    exceeds :: Int -> Bool
    exceeds exponentValue = (1 :: Integer) `shiftL` (2 * exponentValue) > value

    expand :: Int -> Int
    expand exponentValue
      | exceeds exponentValue = exponentValue
      | otherwise = expand (2 * exponentValue)

    narrow :: Int -> Int -> Integer
    narrow low high
      | high - low <= 1 = 1 `shiftL` high
      | exceeds middle = narrow low middle
      | otherwise = narrow middle high
      where
        middle :: Int
        middle = (low + high) `quot` 2

rowSquaredNorms :: Int -> [Integer] -> [Integer]
rowSquaredNorms columnCount entries
  | columnCount <= 0 = []
  | otherwise = rowNorms entries
  where
    rowNorms :: [Integer] -> [Integer]
    rowNorms [] = []
    rowNorms remaining =
      let (rowEntries, rest) = splitAt columnCount remaining
       in foldl' (\accumulator entryValue -> accumulator + entryValue * entryValue) 0 rowEntries : rowNorms rest

takeLargestWitness :: Int -> [Integer] -> [Integer]
takeLargestWitness count values =
  take count (descendingInsertionSort values)

descendingInsertionSort :: [Integer] -> [Integer]
descendingInsertionSort =
  foldr insertDescending []

insertDescending :: Integer -> [Integer] -> [Integer]
insertDescending value values =
  case values of
    [] -> [value]
    currentValue : remainingValues ->
      if value >= currentValue
        then value : values
        else currentValue : insertDescending value remainingValues

matrixProduct :: Int -> Int -> Int -> [Integer] -> [Integer] -> [Integer]
matrixProduct rowCount sharedCount columnCount leftEntries rightEntries =
  matrixProductVector rowCount sharedCount columnCount (V.fromList leftEntries) (V.fromList rightEntries)

matrixProductVector :: Int -> Int -> Int -> V.Vector Integer -> V.Vector Integer -> [Integer]
matrixProductVector rowCount sharedCount columnCount leftEntries rightEntries =
  [ dotProductEntry sharedCount columnCount leftEntries rightEntries rowIndex columnIndex 0 0
    | rowIndex <- [0 .. rowCount - 1],
      columnIndex <- [0 .. columnCount - 1]
  ]

dotProductEntry :: Int -> Int -> V.Vector Integer -> V.Vector Integer -> Int -> Int -> Int -> Integer -> Integer
dotProductEntry sharedCount columnCount leftEntries rightEntries rowIndex columnIndex sharedIndex accumulator
  | sharedIndex >= sharedCount = accumulator
  | otherwise =
      let leftValue = vectorValueAt leftEntries (rowIndex * sharedCount + sharedIndex)
          nextAccumulator =
            if leftValue == 0
              then accumulator
              else accumulator + leftValue * vectorValueAt rightEntries (sharedIndex * columnCount + columnIndex)
       in dotProductEntry sharedCount columnCount leftEntries rightEntries rowIndex columnIndex (sharedIndex + 1) nextAccumulator

matrixIsDiagonal :: Int -> [Integer] -> Bool
matrixIsDiagonal matrixSize entries =
  and
    [ rowIndex == columnIndex || valueAt entries (flatIndex matrixSize rowIndex columnIndex) == 0
      | rowIndex <- [0 .. matrixSize - 1],
        columnIndex <- [0 .. matrixSize - 1]
    ]

identityList :: Int -> [Integer]
identityList matrixSize =
  [ if rowIndex == columnIndex then 1 else 0
    | rowIndex <- [0 .. matrixSize - 1],
      columnIndex <- [0 .. matrixSize - 1]
  ]

maxAbsEntry :: [Integer] -> Integer
maxAbsEntry =
  foldl' (\current entryValue -> max current (abs entryValue)) 0

centerResidue :: Integer -> Integer -> Integer
centerResidue modulusValue value
  | modulusValue <= 1 = value
  | doubled > modulusValue = residueValue - modulusValue
  | otherwise = residueValue
  where
    residueValue :: Integer
    residueValue = value `mod` modulusValue

    doubled :: Integer
    doubled = 2 * residueValue

symmetricLiftInteger :: Integer -> Integer -> Integer
symmetricLiftInteger modulusValue residueValue
  | 2 * residueValue > modulusValue = residueValue - modulusValue
  | otherwise = residueValue

modAddWord :: Word64 -> Word64 -> Word64 -> Word64
modAddWord primeValue leftValue rightValue =
  let sumValue = leftValue + rightValue
   in if sumValue >= primeValue
        then sumValue - primeValue
        else sumValue

modSubWord :: Word64 -> Word64 -> Word64 -> Word64
modSubWord primeValue leftValue rightValue
  | leftValue >= rightValue = leftValue - rightValue
  | otherwise = primeValue - (rightValue - leftValue)

augmentedIndex :: Int -> Int -> Int -> Int
augmentedIndex matrixSize rowIndex columnIndex =
  rowIndex * (2 * matrixSize) + columnIndex

valueAt :: [Integer] -> Int -> Integer
valueAt values indexValue =
  maybe 0 id (values !? indexValue)


vectorValueAt :: V.Vector Integer -> Int -> Integer
vectorValueAt values indexValue =
  maybe 0 id (values V.!? indexValue)

newSmithWitnessArena :: Int -> Int -> [Integer] -> ST s (SmithWitnessArena s)
newSmithWitnessArena rowCount columnCount entries = do
  work <- V.thaw (V.fromList entries)
  leftRows <- V.thaw (identityVector rowCount)
  rightRows <- V.thaw (identityVector columnCount)
  leftInverseRows <- V.thaw (identityVector rowCount)
  rightInverseRows <- V.thaw (identityVector columnCount)
  pure
    SmithWitnessArena
      { smithWitnessRowCount = rowCount,
        smithWitnessColumnCount = columnCount,
        smithWitnessWork = work,
        smithWitnessLeftRows = leftRows,
        smithWitnessRightRows = rightRows,
        smithWitnessLeftInverseRows = leftInverseRows,
        smithWitnessRightInverseRows = rightInverseRows
      }

identityVector :: Int -> V.Vector Integer
identityVector sizeValue =
  V.generate
    (sizeValue * sizeValue)
    ( \entryIndex ->
        let (rowIndex, columnIndex) = entryIndex `quotRem` sizeValue
         in if rowIndex == columnIndex then 1 else 0
    )

flatIndex :: Int -> Int -> Int -> Int
flatIndex columnCount rowIndex columnIndex =
  rowIndex * columnCount + columnIndex

readWorkEntry :: SmithWitnessArena s -> Int -> Int -> ST s Integer
readWorkEntry arenaValue rowIndex columnIndex =
  MV.read (smithWitnessWork arenaValue) (flatIndex (smithWitnessColumnCount arenaValue) rowIndex columnIndex)

entryIsZeroMutable :: SmithWitnessArena s -> Int -> Int -> ST s Bool
entryIsZeroMutable arenaValue rowIndex columnIndex =
  (== 0) <$> readWorkEntry arenaValue rowIndex columnIndex

readFlatVector :: forall s. MV.MVector s Integer -> ST s [Integer]
readFlatVector entries =
  readFlatAt 0 []
  where
    entryCount :: Int
    entryCount = MV.length entries

    readFlatAt :: Int -> [Integer] -> ST s [Integer]
    readFlatAt entryIndex values
      | entryIndex >= entryCount = pure (reverse values)
      | otherwise = do
          entryValue <- MV.read entries entryIndex
          readFlatAt (entryIndex + 1) (entryValue : values)

alternationBudget :: SmithWitnessArena s -> Int
alternationBudget arenaValue =
  64 + 2 * (smithWitnessRowCount arenaValue + smithWitnessColumnCount arenaValue)

normalizationBudget :: SmithWitnessArena s -> Int
normalizationBudget arenaValue =
  max 1 (smithWitnessRowCount arenaValue * smithWitnessColumnCount arenaValue * 16)

alternatingHermiteMutable :: forall s. Int -> SmithWitnessArena s -> ST s (Maybe SmithWitnessFailure)
alternatingHermiteMutable remainingBudget arenaValue
  | remainingBudget <= 0 = pure (Just (SmithWitnessBudgetExhausted "hermite alternation"))
  | otherwise = do
      rowFailure <- rowHermitePhaseMutable arenaValue
      case rowFailure of
        Just failureValue -> pure (Just failureValue)
        Nothing -> do
          columnFailure <- columnHermitePhaseMutable arenaValue
          case columnFailure of
            Just failureValue -> pure (Just failureValue)
            Nothing -> do
              cleared <- offDiagonalClearMutable arenaValue
              if cleared
                then pure Nothing
                else alternatingHermiteMutable (remainingBudget - 1) arenaValue

rowHermitePhaseMutable :: forall s. SmithWitnessArena s -> ST s (Maybe SmithWitnessFailure)
rowHermitePhaseMutable arenaValue =
  phaseStep 0
  where
    diagonalSize :: Int
    diagonalSize = min (smithWitnessRowCount arenaValue) (smithWitnessColumnCount arenaValue)

    phaseStep :: Int -> ST s (Maybe SmithWitnessFailure)
    phaseStep pivotIndex
      | pivotIndex >= diagonalSize = backwardReduceAboveMutable pivotIndex arenaValue *> pure Nothing
      | otherwise = do
          pivotCandidate <- findPivotMutable pivotIndex pivotIndex arenaValue
          case pivotCandidate of
            Nothing -> backwardReduceAboveMutable pivotIndex arenaValue *> pure Nothing
            Just pivotValue -> do
              swapRowsWitnessed pivotIndex (smithPivotRowIndex pivotValue) arenaValue
              swapColumnsWitnessed pivotIndex (smithPivotColumnIndex pivotValue) arenaValue
              signFailure <- normalizePivotSignMutable pivotIndex pivotIndex arenaValue
              case signFailure of
                Just failureValue -> pure (Just failureValue)
                Nothing -> do
                  clearFailure <- clearColumnBelowMutable pivotIndex arenaValue
                  case clearFailure of
                    Just failureValue -> pure (Just failureValue)
                    Nothing -> phaseStep (pivotIndex + 1)

columnHermitePhaseMutable :: forall s. SmithWitnessArena s -> ST s (Maybe SmithWitnessFailure)
columnHermitePhaseMutable arenaValue =
  phaseStep 0
  where
    diagonalSize :: Int
    diagonalSize = min (smithWitnessRowCount arenaValue) (smithWitnessColumnCount arenaValue)

    phaseStep :: Int -> ST s (Maybe SmithWitnessFailure)
    phaseStep pivotIndex
      | pivotIndex >= diagonalSize = backwardReduceLeftMutable pivotIndex arenaValue *> pure Nothing
      | otherwise = do
          pivotCandidate <- findPivotMutable pivotIndex pivotIndex arenaValue
          case pivotCandidate of
            Nothing -> backwardReduceLeftMutable pivotIndex arenaValue *> pure Nothing
            Just pivotValue -> do
              swapRowsWitnessed pivotIndex (smithPivotRowIndex pivotValue) arenaValue
              swapColumnsWitnessed pivotIndex (smithPivotColumnIndex pivotValue) arenaValue
              signFailure <- normalizePivotSignMutable pivotIndex pivotIndex arenaValue
              case signFailure of
                Just failureValue -> pure (Just failureValue)
                Nothing -> do
                  clearFailure <- clearRowRightMutable pivotIndex arenaValue
                  case clearFailure of
                    Just failureValue -> pure (Just failureValue)
                    Nothing -> phaseStep (pivotIndex + 1)

clearColumnBelowMutable :: forall s. Int -> SmithWitnessArena s -> ST s (Maybe SmithWitnessFailure)
clearColumnBelowMutable pivotIndex arenaValue =
  scanRows (pivotIndex + 1)
  where
    scanRows :: Int -> ST s (Maybe SmithWitnessFailure)
    scanRows rowIndex
      | rowIndex >= smithWitnessRowCount arenaValue = pure Nothing
      | otherwise = do
          entryValue <- readWorkEntry arenaValue rowIndex pivotIndex
          if entryValue == 0
            then scanRows (rowIndex + 1)
            else do
              pivotValue <- readWorkEntry arenaValue pivotIndex pivotIndex
              if pivotValue == 0
                then pure (Just SmithWitnessPivotBecameZero)
                else do
                  let (quotientValue, remainderValue) = balancedDivMod entryValue pivotValue
                  if quotientValue /= 0
                    then rowCombineWitnessed rowIndex pivotIndex quotientValue arenaValue
                    else pure ()
                  if remainderValue == 0
                    then scanRows (rowIndex + 1)
                    else do
                      gcdFailure <- gcdCombineRowsWitnessed pivotIndex rowIndex pivotIndex arenaValue
                      case gcdFailure of
                        Just failureValue -> pure (Just failureValue)
                        Nothing -> scanRows (rowIndex + 1)

clearRowRightMutable :: forall s. Int -> SmithWitnessArena s -> ST s (Maybe SmithWitnessFailure)
clearRowRightMutable pivotIndex arenaValue =
  scanColumns (pivotIndex + 1)
  where
    scanColumns :: Int -> ST s (Maybe SmithWitnessFailure)
    scanColumns columnIndex
      | columnIndex >= smithWitnessColumnCount arenaValue = pure Nothing
      | otherwise = do
          entryValue <- readWorkEntry arenaValue pivotIndex columnIndex
          if entryValue == 0
            then scanColumns (columnIndex + 1)
            else do
              pivotValue <- readWorkEntry arenaValue pivotIndex pivotIndex
              if pivotValue == 0
                then pure (Just SmithWitnessPivotBecameZero)
                else do
                  let (quotientValue, remainderValue) = balancedDivMod entryValue pivotValue
                  if quotientValue /= 0
                    then columnCombineWitnessed columnIndex pivotIndex quotientValue arenaValue
                    else pure ()
                  if remainderValue == 0
                    then scanColumns (columnIndex + 1)
                    else do
                      gcdFailure <- gcdCombineColumnsWitnessed pivotIndex pivotIndex columnIndex arenaValue
                      case gcdFailure of
                        Just failureValue -> pure (Just failureValue)
                        Nothing -> scanColumns (columnIndex + 1)

backwardReduceAboveMutable :: forall s. Int -> SmithWitnessArena s -> ST s ()
backwardReduceAboveMutable settledCount arenaValue =
  scanRows (settledCount - 1)
  where
    scanRows :: Int -> ST s ()
    scanRows rowIndex
      | rowIndex < 0 = pure ()
      | otherwise = do
          scanColumns rowIndex (rowIndex + 1)
          scanRows (rowIndex - 1)

    scanColumns :: Int -> Int -> ST s ()
    scanColumns rowIndex columnIndex
      | columnIndex >= settledCount = pure ()
      | otherwise = do
          pivotValue <- readWorkEntry arenaValue columnIndex columnIndex
          if pivotValue == 0
            then scanColumns rowIndex (columnIndex + 1)
            else do
              entryValue <- readWorkEntry arenaValue rowIndex columnIndex
              let (quotientValue, _) = balancedDivMod entryValue pivotValue
              if quotientValue /= 0
                then rowCombineWitnessed rowIndex columnIndex quotientValue arenaValue
                else pure ()
              scanColumns rowIndex (columnIndex + 1)

backwardReduceLeftMutable :: forall s. Int -> SmithWitnessArena s -> ST s ()
backwardReduceLeftMutable settledCount arenaValue =
  scanColumns (settledCount - 1)
  where
    scanColumns :: Int -> ST s ()
    scanColumns columnIndex
      | columnIndex < 0 = pure ()
      | otherwise = do
          scanRows columnIndex (columnIndex + 1)
          scanColumns (columnIndex - 1)

    scanRows :: Int -> Int -> ST s ()
    scanRows columnIndex rowIndex
      | rowIndex >= settledCount = pure ()
      | otherwise = do
          pivotValue <- readWorkEntry arenaValue rowIndex rowIndex
          if pivotValue == 0
            then scanRows columnIndex (rowIndex + 1)
            else do
              entryValue <- readWorkEntry arenaValue rowIndex columnIndex
              let (quotientValue, _) = balancedDivMod entryValue pivotValue
              if quotientValue /= 0
                then columnCombineWitnessed columnIndex rowIndex quotientValue arenaValue
                else pure ()
              scanRows columnIndex (rowIndex + 1)

offDiagonalClearMutable :: forall s. SmithWitnessArena s -> ST s Bool
offDiagonalClearMutable arenaValue =
  scanRows 0
  where
    scanRows :: Int -> ST s Bool
    scanRows rowIndex
      | rowIndex >= smithWitnessRowCount arenaValue = pure True
      | otherwise = do
          rowClear <- scanColumns rowIndex 0
          if rowClear
            then scanRows (rowIndex + 1)
            else pure False

    scanColumns :: Int -> Int -> ST s Bool
    scanColumns rowIndex columnIndex
      | columnIndex >= smithWitnessColumnCount arenaValue = pure True
      | rowIndex == columnIndex = scanColumns rowIndex (columnIndex + 1)
      | otherwise = do
          isZeroEntry <- entryIsZeroMutable arenaValue rowIndex columnIndex
          if isZeroEntry
            then scanColumns rowIndex (columnIndex + 1)
            else pure False

findPivotMutable :: forall s. Int -> Int -> SmithWitnessArena s -> ST s (Maybe SmithPivot)
findPivotMutable startRow startColumn arenaValue =
  fmap fst <$> scanRows startRow Nothing
  where
    scanRows :: Int -> Maybe (SmithPivot, Integer) -> ST s (Maybe (SmithPivot, Integer))
    scanRows rowIndex bestValue
      | rowIndex >= smithWitnessRowCount arenaValue = pure bestValue
      | otherwise = do
          rowBest <- scanColumns rowIndex startColumn bestValue
          scanRows (rowIndex + 1) rowBest

    scanColumns :: Int -> Int -> Maybe (SmithPivot, Integer) -> ST s (Maybe (SmithPivot, Integer))
    scanColumns rowIndex columnIndex bestValue
      | columnIndex >= smithWitnessColumnCount arenaValue = pure bestValue
      | otherwise = do
          entryValue <- readWorkEntry arenaValue rowIndex columnIndex
          let nextBest =
                if entryValue == 0
                  then bestValue
                  else betterPivot bestValue (SmithPivot rowIndex columnIndex, abs entryValue)
          scanColumns rowIndex (columnIndex + 1) nextBest

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
  (magnitudeValue, smithPivotRowIndex pivotValue, smithPivotColumnIndex pivotValue)

normalizePivotMutable :: Int -> Int -> Int -> SmithWitnessArena s -> ST s (Maybe SmithWitnessFailure)
normalizePivotMutable pivotRow pivotColumn remainingBudget arenaValue
  | remainingBudget <= 0 = pure (Just (SmithWitnessBudgetExhausted "normalization"))
  | otherwise = do
      signFailure <- normalizePivotSignMutable pivotRow pivotColumn arenaValue
      case signFailure of
        Just failureValue -> pure (Just failureValue)
        Nothing -> do
          (columnFailure, columnChanged) <- reduceDiagonalColumnMutable pivotRow arenaValue
          case columnFailure of
            Just failureValue -> pure (Just failureValue)
            Nothing -> do
              (rowFailure, rowChanged) <- reduceDiagonalRowMutable pivotRow arenaValue
              case rowFailure of
                Just failureValue -> pure (Just failureValue)
                Nothing -> do
                  clearedColumn <- columnClearedMutable pivotRow pivotColumn arenaValue
                  clearedRow <- rowClearedMutable pivotRow pivotColumn arenaValue
                  if clearedColumn && clearedRow
                    then pure Nothing
                    else
                      if columnChanged || rowChanged
                        then normalizePivotMutable pivotRow pivotColumn (remainingBudget - 1) arenaValue
                        else pure (Just SmithWitnessNormalizationStalled)

normalizePivotSignMutable :: Int -> Int -> SmithWitnessArena s -> ST s (Maybe SmithWitnessFailure)
normalizePivotSignMutable pivotRow pivotColumn arenaValue = do
  pivotValue <- readWorkEntry arenaValue pivotRow pivotColumn
  if pivotValue < 0
    then scaleRowWitnessed pivotRow (-1) arenaValue *> pure Nothing
    else pure Nothing

reduceDiagonalColumnMutable :: forall s. Int -> SmithWitnessArena s -> ST s (Maybe SmithWitnessFailure, Bool)
reduceDiagonalColumnMutable diagonalIndex arenaValue =
  scanRows 0 False
  where
    scanRows :: Int -> Bool -> ST s (Maybe SmithWitnessFailure, Bool)
    scanRows rowIndex changed
      | rowIndex >= smithWitnessRowCount arenaValue = pure (Nothing, changed)
      | rowIndex == diagonalIndex = scanRows (rowIndex + 1) changed
      | otherwise = do
          entryValue <- readWorkEntry arenaValue rowIndex diagonalIndex
          if entryValue == 0
            then scanRows (rowIndex + 1) changed
            else do
              pivotValue <- readWorkEntry arenaValue diagonalIndex diagonalIndex
              if pivotValue == 0
                then pure (Just SmithWitnessPivotBecameZero, changed)
                else do
                  let (quotientValue, remainderValue) = balancedDivMod entryValue pivotValue
                  if quotientValue /= 0
                    then rowCombineWitnessed rowIndex diagonalIndex quotientValue arenaValue
                    else pure ()
                  reducedEntry <- readWorkEntry arenaValue rowIndex diagonalIndex
                  reductionFailure <-
                    if remainderValue == 0 && reducedEntry == 0
                      then pure Nothing
                      else gcdCombineRowsWitnessed diagonalIndex rowIndex diagonalIndex arenaValue
                  case reductionFailure of
                    Just failureValue -> pure (Just failureValue, True)
                    Nothing -> scanRows 0 True

reduceDiagonalRowMutable :: forall s. Int -> SmithWitnessArena s -> ST s (Maybe SmithWitnessFailure, Bool)
reduceDiagonalRowMutable diagonalIndex arenaValue =
  scanColumns 0 False
  where
    scanColumns :: Int -> Bool -> ST s (Maybe SmithWitnessFailure, Bool)
    scanColumns columnIndex changed
      | columnIndex >= smithWitnessColumnCount arenaValue = pure (Nothing, changed)
      | columnIndex == diagonalIndex = scanColumns (columnIndex + 1) changed
      | otherwise = do
          entryValue <- readWorkEntry arenaValue diagonalIndex columnIndex
          if entryValue == 0
            then scanColumns (columnIndex + 1) changed
            else do
              pivotValue <- readWorkEntry arenaValue diagonalIndex diagonalIndex
              if pivotValue == 0
                then pure (Just SmithWitnessPivotBecameZero, changed)
                else do
                  let (quotientValue, remainderValue) = balancedDivMod entryValue pivotValue
                  if quotientValue /= 0
                    then columnCombineWitnessed columnIndex diagonalIndex quotientValue arenaValue
                    else pure ()
                  reducedEntry <- readWorkEntry arenaValue diagonalIndex columnIndex
                  reductionFailure <-
                    if remainderValue == 0 && reducedEntry == 0
                      then pure Nothing
                      else gcdCombineColumnsWitnessed diagonalIndex diagonalIndex columnIndex arenaValue
                  case reductionFailure of
                    Just failureValue -> pure (Just failureValue, True)
                    Nothing -> scanColumns 0 True

balancedDivMod :: Integer -> Integer -> (Integer, Integer)
balancedDivMod numerator denominator =
  let positiveDenominator = abs denominator
      (floorQuotient, floorRemainder) = numerator `divMod` positiveDenominator
      (quotientValue, remainderValue) =
        if 2 * floorRemainder > positiveDenominator
          then (floorQuotient + 1, floorRemainder - positiveDenominator)
          else (floorQuotient, floorRemainder)
   in if denominator < 0
        then (negate quotientValue, remainderValue)
        else (quotientValue, remainderValue)

columnClearedMutable :: forall s. Int -> Int -> SmithWitnessArena s -> ST s Bool
columnClearedMutable pivotRow pivotColumn arenaValue =
  scanRows 0
  where
    scanRows :: Int -> ST s Bool
    scanRows rowIndex
      | rowIndex >= smithWitnessRowCount arenaValue = pure True
      | rowIndex == pivotRow = scanRows (rowIndex + 1)
      | otherwise = do
          isZeroEntry <- entryIsZeroMutable arenaValue rowIndex pivotColumn
          if isZeroEntry
            then scanRows (rowIndex + 1)
            else pure False

rowClearedMutable :: forall s. Int -> Int -> SmithWitnessArena s -> ST s Bool
rowClearedMutable pivotRow pivotColumn arenaValue =
  scanColumns 0
  where
    scanColumns :: Int -> ST s Bool
    scanColumns columnIndex
      | columnIndex >= smithWitnessColumnCount arenaValue = pure True
      | columnIndex == pivotColumn = scanColumns (columnIndex + 1)
      | otherwise = do
          isZeroEntry <- entryIsZeroMutable arenaValue pivotRow columnIndex
          if isZeroEntry
            then scanColumns (columnIndex + 1)
            else pure False

gcdCombineRowsWitnessed :: Int -> Int -> Int -> SmithWitnessArena s -> ST s (Maybe SmithWitnessFailure)
gcdCombineRowsWitnessed pivotRow candidateRow pivotColumn arenaValue = do
  pivotValue <- readWorkEntry arenaValue pivotRow pivotColumn
  entryValue <- readWorkEntry arenaValue candidateRow pivotColumn
  let (gcdValue, pivotCoefficient, entryCoefficient) = extendedGcdDomain pivotValue entryValue
  case (exactQuotientMutable "row gcd pivot quotient" pivotValue gcdValue, exactQuotientMutable "row gcd entry quotient" entryValue gcdValue) of
    (SmithExactQuotient pivotQuotient, SmithExactQuotient entryQuotient) -> do
      rowPairTransformWitnessed pivotRow candidateRow pivotCoefficient entryCoefficient (negate entryQuotient) pivotQuotient arenaValue
      pure Nothing
    (SmithInexactQuotient failureValue, _) -> pure (Just failureValue)
    (_, SmithInexactQuotient failureValue) -> pure (Just failureValue)

gcdCombineColumnsWitnessed :: Int -> Int -> Int -> SmithWitnessArena s -> ST s (Maybe SmithWitnessFailure)
gcdCombineColumnsWitnessed pivotRow pivotColumn candidateColumn arenaValue = do
  pivotValue <- readWorkEntry arenaValue pivotRow pivotColumn
  entryValue <- readWorkEntry arenaValue pivotRow candidateColumn
  let (gcdValue, pivotCoefficient, entryCoefficient) = extendedGcdDomain pivotValue entryValue
  case (exactQuotientMutable "column gcd pivot quotient" pivotValue gcdValue, exactQuotientMutable "column gcd entry quotient" entryValue gcdValue) of
    (SmithExactQuotient pivotQuotient, SmithExactQuotient entryQuotient) -> do
      columnPairTransformWitnessed pivotColumn candidateColumn pivotCoefficient entryCoefficient (negate entryQuotient) pivotQuotient arenaValue
      pure Nothing
    (SmithInexactQuotient failureValue, _) -> pure (Just failureValue)
    (_, SmithInexactQuotient failureValue) -> pure (Just failureValue)

exactQuotientMutable :: String -> Integer -> Integer -> SmithExactQuotient
exactQuotientMutable context numerator denominator
  | denominator == 0 = SmithInexactQuotient (SmithWitnessInexactDivision context)
  | remainderValue == 0 = SmithExactQuotient quotientValue
  | otherwise = SmithInexactQuotient (SmithWitnessInexactDivision context)
  where
    (quotientValue, remainderValue) = numerator `divMod` denominator

enforceDivisibilityChainMutable :: forall s. SmithWitnessArena s -> ST s (Maybe SmithWitnessFailure)
enforceDivisibilityChainMutable arenaValue =
  repairAt (diagonalSize * diagonalSize)
  where
    diagonalSize :: Int
    diagonalSize = min (smithWitnessRowCount arenaValue) (smithWitnessColumnCount arenaValue)

    repairAt :: Int -> ST s (Maybe SmithWitnessFailure)
    repairAt remainingBudget
      | remainingBudget <= 0 = do
          violationValue <- findDivisibilityViolationMutable diagonalSize arenaValue
          case violationValue of
            Nothing -> pure Nothing
            Just _ -> pure (Just (SmithWitnessBudgetExhausted "divisibility chain"))
      | otherwise = do
          violationValue <- findDivisibilityViolationMutable diagonalSize arenaValue
          case violationValue of
            Nothing -> pure Nothing
            Just violationIndex -> do
              rowCombineWitnessed violationIndex (violationIndex + 1) (-1) arenaValue
              leftFailure <- normalizePivotMutable violationIndex violationIndex (normalizationBudget arenaValue) arenaValue
              case leftFailure of
                Just failureValue -> pure (Just failureValue)
                Nothing -> do
                  rightFailure <- normalizePivotMutable (violationIndex + 1) (violationIndex + 1) (normalizationBudget arenaValue) arenaValue
                  case rightFailure of
                    Just failureValue -> pure (Just failureValue)
                    Nothing -> repairAt (remainingBudget - 1)

findDivisibilityViolationMutable :: forall s. Int -> SmithWitnessArena s -> ST s (Maybe Int)
findDivisibilityViolationMutable diagonalSize arenaValue =
  scanDiagonal 0
  where
    scanDiagonal :: Int -> ST s (Maybe Int)
    scanDiagonal diagonalIndex
      | diagonalIndex >= diagonalSize - 1 = pure Nothing
      | otherwise = do
          leftDiagonal <- readWorkEntry arenaValue diagonalIndex diagonalIndex
          rightDiagonal <- readWorkEntry arenaValue (diagonalIndex + 1) (diagonalIndex + 1)
          if leftDiagonal == 0
            || rightDiagonal == 0
            || rightDiagonal `mod` leftDiagonal == 0
            then scanDiagonal (diagonalIndex + 1)
            else pure (Just diagonalIndex)

normalizeDiagonalUnitsMutable :: forall s. SmithWitnessArena s -> ST s (Maybe SmithWitnessFailure)
normalizeDiagonalUnitsMutable arenaValue =
  normalizeAt 0
  where
    diagonalSize :: Int
    diagonalSize = min (smithWitnessRowCount arenaValue) (smithWitnessColumnCount arenaValue)

    normalizeAt :: Int -> ST s (Maybe SmithWitnessFailure)
    normalizeAt diagonalIndex
      | diagonalIndex >= diagonalSize = pure Nothing
      | otherwise = do
          diagonalValue <- readWorkEntry arenaValue diagonalIndex diagonalIndex
          if diagonalValue < 0
            then scaleRowWitnessed diagonalIndex (-1) arenaValue *> normalizeAt (diagonalIndex + 1)
            else normalizeAt (diagonalIndex + 1)

swapRowsWitnessed :: Int -> Int -> SmithWitnessArena s -> ST s ()
swapRowsWitnessed leftRow rightRow arenaValue = do
  swapRowsVector (smithWitnessColumnCount arenaValue) (smithWitnessWork arenaValue) leftRow rightRow 0
  swapRowsVector (smithWitnessRowCount arenaValue) (smithWitnessLeftRows arenaValue) leftRow rightRow 0
  swapColumnsVector (smithWitnessRowCount arenaValue) (smithWitnessLeftInverseRows arenaValue) leftRow rightRow 0 (smithWitnessRowCount arenaValue)

swapColumnsWitnessed :: Int -> Int -> SmithWitnessArena s -> ST s ()
swapColumnsWitnessed leftColumn rightColumn arenaValue = do
  swapColumnsVector (smithWitnessColumnCount arenaValue) (smithWitnessWork arenaValue) leftColumn rightColumn 0 (smithWitnessRowCount arenaValue)
  swapColumnsVector (smithWitnessColumnCount arenaValue) (smithWitnessRightRows arenaValue) leftColumn rightColumn 0 (smithWitnessColumnCount arenaValue)
  swapRowsVector (smithWitnessColumnCount arenaValue) (smithWitnessRightInverseRows arenaValue) leftColumn rightColumn 0

rowCombineWitnessed :: Int -> Int -> Integer -> SmithWitnessArena s -> ST s ()
rowCombineWitnessed targetRow sourceRow coefficient arenaValue = do
  rowCombineVector (smithWitnessColumnCount arenaValue) (smithWitnessWork arenaValue) targetRow sourceRow coefficient 0
  rowCombineVector (smithWitnessRowCount arenaValue) (smithWitnessLeftRows arenaValue) targetRow sourceRow coefficient 0
  columnAddScaledVector (smithWitnessRowCount arenaValue) (smithWitnessLeftInverseRows arenaValue) sourceRow targetRow coefficient 0 (smithWitnessRowCount arenaValue)

columnCombineWitnessed :: Int -> Int -> Integer -> SmithWitnessArena s -> ST s ()
columnCombineWitnessed targetColumn sourceColumn coefficient arenaValue = do
  columnCombineVector (smithWitnessColumnCount arenaValue) (smithWitnessWork arenaValue) targetColumn sourceColumn coefficient 0 (smithWitnessRowCount arenaValue)
  columnCombineVector (smithWitnessColumnCount arenaValue) (smithWitnessRightRows arenaValue) targetColumn sourceColumn coefficient 0 (smithWitnessColumnCount arenaValue)
  rowAddScaledVector (smithWitnessColumnCount arenaValue) (smithWitnessRightInverseRows arenaValue) sourceColumn targetColumn coefficient 0

rowPairTransformWitnessed :: Int -> Int -> Integer -> Integer -> Integer -> Integer -> SmithWitnessArena s -> ST s ()
rowPairTransformWitnessed leftRow rightRow aa ab ba bb arenaValue = do
  rowPairTransformVector (smithWitnessColumnCount arenaValue) (smithWitnessWork arenaValue) leftRow rightRow aa ab ba bb 0
  rowPairTransformVector (smithWitnessRowCount arenaValue) (smithWitnessLeftRows arenaValue) leftRow rightRow aa ab ba bb 0
  columnPairTransformVector (smithWitnessRowCount arenaValue) (smithWitnessLeftInverseRows arenaValue) leftRow rightRow bb (negate ba) (negate ab) aa 0 (smithWitnessRowCount arenaValue)

columnPairTransformWitnessed :: Int -> Int -> Integer -> Integer -> Integer -> Integer -> SmithWitnessArena s -> ST s ()
columnPairTransformWitnessed leftColumn rightColumn aa ab ba bb arenaValue = do
  columnPairTransformVector (smithWitnessColumnCount arenaValue) (smithWitnessWork arenaValue) leftColumn rightColumn aa ab ba bb 0 (smithWitnessRowCount arenaValue)
  columnPairTransformVector (smithWitnessColumnCount arenaValue) (smithWitnessRightRows arenaValue) leftColumn rightColumn aa ab ba bb 0 (smithWitnessColumnCount arenaValue)
  rowPairTransformVector (smithWitnessColumnCount arenaValue) (smithWitnessRightInverseRows arenaValue) leftColumn rightColumn bb (negate ba) (negate ab) aa 0

scaleRowWitnessed :: Int -> Integer -> SmithWitnessArena s -> ST s ()
scaleRowWitnessed rowIndex factor arenaValue = do
  scaleRowVector (smithWitnessColumnCount arenaValue) (smithWitnessWork arenaValue) rowIndex factor 0
  scaleRowVector (smithWitnessRowCount arenaValue) (smithWitnessLeftRows arenaValue) rowIndex factor 0
  scaleColumnVector (smithWitnessRowCount arenaValue) (smithWitnessLeftInverseRows arenaValue) rowIndex factor 0 (smithWitnessRowCount arenaValue)

swapRowsVector :: Int -> MV.MVector s Integer -> Int -> Int -> Int -> ST s ()
swapRowsVector columnCount entries leftRow rightRow columnIndex
  | leftRow == rightRow = pure ()
  | columnIndex >= columnCount = pure ()
  | otherwise = do
      let leftIndex = flatIndex columnCount leftRow columnIndex
          rightIndex = flatIndex columnCount rightRow columnIndex
      leftValue <- MV.unsafeRead entries leftIndex
      rightValue <- MV.unsafeRead entries rightIndex
      MV.unsafeWrite entries leftIndex rightValue
      MV.unsafeWrite entries rightIndex leftValue
      swapRowsVector columnCount entries leftRow rightRow (columnIndex + 1)

swapColumnsVector :: Int -> MV.MVector s Integer -> Int -> Int -> Int -> Int -> ST s ()
swapColumnsVector columnCount entries leftColumn rightColumn rowIndex rowCount
  | leftColumn == rightColumn = pure ()
  | rowIndex >= rowCount = pure ()
  | otherwise = do
      let leftIndex = flatIndex columnCount rowIndex leftColumn
          rightIndex = flatIndex columnCount rowIndex rightColumn
      leftValue <- MV.unsafeRead entries leftIndex
      rightValue <- MV.unsafeRead entries rightIndex
      MV.unsafeWrite entries leftIndex rightValue
      MV.unsafeWrite entries rightIndex leftValue
      swapColumnsVector columnCount entries leftColumn rightColumn (rowIndex + 1) rowCount

rowCombineVector :: Int -> MV.MVector s Integer -> Int -> Int -> Integer -> Int -> ST s ()
rowCombineVector columnCount entries targetRow sourceRow coefficient columnIndex
  | columnIndex >= columnCount = pure ()
  | otherwise = do
      let targetIndex = flatIndex columnCount targetRow columnIndex
          sourceIndex = flatIndex columnCount sourceRow columnIndex
      targetValue <- MV.read entries targetIndex
      sourceValue <- MV.read entries sourceIndex
      MV.write entries targetIndex (targetValue - coefficient * sourceValue)
      rowCombineVector columnCount entries targetRow sourceRow coefficient (columnIndex + 1)

columnCombineVector :: Int -> MV.MVector s Integer -> Int -> Int -> Integer -> Int -> Int -> ST s ()
columnCombineVector columnCount entries targetColumn sourceColumn coefficient rowIndex rowCount
  | rowIndex >= rowCount = pure ()
  | otherwise = do
      let targetIndex = flatIndex columnCount rowIndex targetColumn
          sourceIndex = flatIndex columnCount rowIndex sourceColumn
      targetValue <- MV.read entries targetIndex
      sourceValue <- MV.read entries sourceIndex
      MV.write entries targetIndex (targetValue - coefficient * sourceValue)
      columnCombineVector columnCount entries targetColumn sourceColumn coefficient (rowIndex + 1) rowCount

rowAddScaledVector :: Int -> MV.MVector s Integer -> Int -> Int -> Integer -> Int -> ST s ()
rowAddScaledVector columnCount entries targetRow sourceRow coefficient columnIndex
  | columnIndex >= columnCount = pure ()
  | otherwise = do
      let targetIndex = flatIndex columnCount targetRow columnIndex
          sourceIndex = flatIndex columnCount sourceRow columnIndex
      targetValue <- MV.read entries targetIndex
      sourceValue <- MV.read entries sourceIndex
      MV.write entries targetIndex (targetValue + coefficient * sourceValue)
      rowAddScaledVector columnCount entries targetRow sourceRow coefficient (columnIndex + 1)

columnAddScaledVector :: Int -> MV.MVector s Integer -> Int -> Int -> Integer -> Int -> Int -> ST s ()
columnAddScaledVector columnCount entries targetColumn sourceColumn coefficient rowIndex rowCount
  | rowIndex >= rowCount = pure ()
  | otherwise = do
      let targetIndex = flatIndex columnCount rowIndex targetColumn
          sourceIndex = flatIndex columnCount rowIndex sourceColumn
      targetValue <- MV.read entries targetIndex
      sourceValue <- MV.read entries sourceIndex
      MV.write entries targetIndex (targetValue + coefficient * sourceValue)
      columnAddScaledVector columnCount entries targetColumn sourceColumn coefficient (rowIndex + 1) rowCount

rowPairTransformVector :: Int -> MV.MVector s Integer -> Int -> Int -> Integer -> Integer -> Integer -> Integer -> Int -> ST s ()
rowPairTransformVector columnCount entries leftRow rightRow aa ab ba bb columnIndex
  | columnIndex >= columnCount = pure ()
  | otherwise = do
      let leftIndex = flatIndex columnCount leftRow columnIndex
          rightIndex = flatIndex columnCount rightRow columnIndex
      leftValue <- MV.read entries leftIndex
      rightValue <- MV.read entries rightIndex
      MV.write entries leftIndex (aa * leftValue + ab * rightValue)
      MV.write entries rightIndex (ba * leftValue + bb * rightValue)
      rowPairTransformVector columnCount entries leftRow rightRow aa ab ba bb (columnIndex + 1)

columnPairTransformVector :: Int -> MV.MVector s Integer -> Int -> Int -> Integer -> Integer -> Integer -> Integer -> Int -> Int -> ST s ()
columnPairTransformVector columnCount entries leftColumn rightColumn aa ab ba bb rowIndex rowCount
  | rowIndex >= rowCount = pure ()
  | otherwise = do
      let leftIndex = flatIndex columnCount rowIndex leftColumn
          rightIndex = flatIndex columnCount rowIndex rightColumn
      leftValue <- MV.read entries leftIndex
      rightValue <- MV.read entries rightIndex
      MV.write entries leftIndex (aa * leftValue + ab * rightValue)
      MV.write entries rightIndex (ba * leftValue + bb * rightValue)
      columnPairTransformVector columnCount entries leftColumn rightColumn aa ab ba bb (rowIndex + 1) rowCount

scaleRowVector :: Int -> MV.MVector s Integer -> Int -> Integer -> Int -> ST s ()
scaleRowVector columnCount entries rowIndex factor columnIndex
  | columnIndex >= columnCount = pure ()
  | otherwise = do
      let entryIndex = flatIndex columnCount rowIndex columnIndex
      entryValue <- MV.read entries entryIndex
      MV.write entries entryIndex (factor * entryValue)
      scaleRowVector columnCount entries rowIndex factor (columnIndex + 1)

scaleColumnVector :: Int -> MV.MVector s Integer -> Int -> Integer -> Int -> Int -> ST s ()
scaleColumnVector columnCount entries columnIndex factor rowIndex rowCount
  | rowIndex >= rowCount = pure ()
  | otherwise = do
      let entryIndex = flatIndex columnCount rowIndex columnIndex
      entryValue <- MV.read entries entryIndex
      MV.write entries entryIndex (factor * entryValue)
      scaleColumnVector columnCount entries columnIndex factor (rowIndex + 1) rowCount
