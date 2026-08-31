module Moonlight.Homology.Pure.Sequence.Spectral.Linear
  ( coordinateUnitVectors,
    FiltrationPivot (..),
    FiltrationReducedColumn (..),
    FiltrationReduction (..),
    filtrationOrderedReduction,
    reduceBasisChecked,
    intersectionBasisChecked,
    kernelBasisOfMatrix,
    imageBasisOfMatrix,
    independentModuloBasis,
    firstVectorOutsideSpan,
    spanRankOfBasis,
    boundaryMatrixAtRational,
    zeroSparseMatrix,
    reshapeSparseMatrix,
    sparseMatrixDenseRows,
    sparseMatrixFromRowsChecked,
    sparseMatrixFromColumnsChecked,
    selectSparseRows,
    selectSparseColumns,
    applySparseMatrixChecked,
    sparseLinearCombination,
  )
where

import Data.Function ((&))
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as List
import qualified Data.Vector as Vector
import Moonlight.Homology.Boundary.Finite
  ( FiniteChainComplex,
    incidenceMatrixAt,
  )
import Moonlight.Homology.Boundary.LinAlg
  ( boundaryEntries,
    sourceCardinality,
    sourceIndex,
    targetCardinality,
    targetIndex,
  )
import Moonlight.Homology.Pure.Degree (HomologicalDegree (..))
import Moonlight.Homology.Pure.Failure (HomologyFailure (..))
import Moonlight.Homology.Pure.Sequence.Spectral.Types (AmbientVector)
import Moonlight.Homology.Pure.Matrix.Shape (cellCountAtDegree)
import Moonlight.Homology.Pure.Matrix.SparseLinAlg
  ( SparseMatrix (..),
    addScaledSparseRow,
    compactSparseRow,
    scaleSparseRow,
    sparseBoundaryMatrixWith,
    sparseEchelonBasis,
    sparseEchelonContains,
    sparseImageBasisOf,
    sparseIndependentModulo,
    sparseKernelBasisOf,
    sparseMatrixVectorProduct,
    sparseRowLookup,
    sparseRowToDense,
    sparseSpanRank,
    sparseTransposeMatrix,
  )

data FiltrationPivot = FiltrationPivot
  { filtrationPivotTargetIndex :: !Int,
    filtrationPivotTargetLevel :: !Int,
    filtrationPivotDistance :: !Int
  }
  deriving stock (Eq, Show)

data FiltrationReducedColumn = FiltrationReducedColumn
  { filtrationColumnSourceIndex :: !Int,
    filtrationColumnSourceLevel :: !Int,
    filtrationColumnSourceVector :: !AmbientVector,
    filtrationColumnTargetVector :: !AmbientVector,
    filtrationColumnPivot :: !(Maybe FiltrationPivot)
  }
  deriving stock (Eq, Show)

data FiltrationReduction = FiltrationReduction
  { filtrationReductionSourceDimension :: !Int,
    filtrationReductionTargetDimension :: !Int,
    filtrationReductionColumns :: ![FiltrationReducedColumn]
  }
  deriving stock (Eq, Show)

data FiltrationReductionPivotColumn = FiltrationReductionPivotColumn
  { frpcSourceVector :: !AmbientVector,
    frpcTargetVector :: !AmbientVector
  }
  deriving stock (Eq, Show)

data FiltrationReductionColumnState = FiltrationReductionColumnState
  { frcsSourceVector :: !AmbientVector,
    frcsTargetVector :: !AmbientVector
  }
  deriving stock (Eq, Show)

data FiltrationReductionState = FiltrationReductionState
  { frsPivotColumns :: !(IntMap.IntMap FiltrationReductionPivotColumn),
    frsReducedColumns :: ![FiltrationReducedColumn]
  }
  deriving stock (Eq, Show)

coordinateUnitVectors :: Int -> [Int] -> [AmbientVector]
coordinateUnitVectors ambientDimension =
  fmap (sparseUnitVector ambientDimension)

sparseUnitVector :: Int -> Int -> AmbientVector
sparseUnitVector ambientDimension selectedIndex =
  if selectedIndex < 0 || selectedIndex >= ambientDimension
    then IntMap.empty
    else IntMap.singleton selectedIndex 1

filtrationOrderedReduction ::
  [Int] ->
  [Int] ->
  SparseMatrix ->
  Either HomologyFailure FiltrationReduction
filtrationOrderedReduction sourceLevels targetLevels matrixValue = do
  compactMatrix <- validateFiltrationReductionMatrix sourceLevels targetLevels matrixValue
  let sourceLevelVector = Vector.fromList sourceLevels
      targetLevelVector = Vector.fromList targetLevels
      columnRows =
        Vector.fromList (smRows (sparseTransposeMatrix compactMatrix))
      orderedColumns =
        filtrationOrderedIndices sourceLevelVector
      finalState =
        List.foldl'
          (reduceFiltrationColumn sourceLevelVector targetLevelVector columnRows)
          emptyFiltrationReductionState
          orderedColumns
  pure
    FiltrationReduction
      { filtrationReductionSourceDimension = Vector.length sourceLevelVector,
        filtrationReductionTargetDimension = Vector.length targetLevelVector,
        filtrationReductionColumns = reverse (frsReducedColumns finalState)
      }

validateFiltrationReductionMatrix ::
  [Int] ->
  [Int] ->
  SparseMatrix ->
  Either HomologyFailure SparseMatrix
validateFiltrationReductionMatrix sourceLevels targetLevels matrixValue =
  if smColumnCount matrixValue /= length sourceLevels || length (smRows matrixValue) /= length targetLevels
    then Left (InvalidMatrixShape "filtration reduction matrix shape does not match the filtration cardinalities")
    else do
      compactRows <- traverse (validateAmbientVector (length sourceLevels)) (smRows matrixValue)
      pure
        SparseMatrix
          { smRows = compactRows,
            smColumnCount = length sourceLevels
          }

emptyFiltrationReductionState :: FiltrationReductionState
emptyFiltrationReductionState =
  FiltrationReductionState
    { frsPivotColumns = IntMap.empty,
      frsReducedColumns = []
    }

reduceFiltrationColumn ::
  Vector.Vector Int ->
  Vector.Vector Int ->
  Vector.Vector AmbientVector ->
  FiltrationReductionState ->
  Int ->
  FiltrationReductionState
reduceFiltrationColumn sourceLevels targetLevels columnRows state sourceIndexValue =
  let initialColumn =
        FiltrationReductionColumnState
          { frcsSourceVector = sparseUnitVector (Vector.length sourceLevels) sourceIndexValue,
            frcsTargetVector =
              maybe IntMap.empty compactSparseRow (columnRows Vector.!? sourceIndexValue)
          }
      reducedColumn =
        reduceColumnAgainstFiltrationPivots targetLevels (frsPivotColumns state) initialColumn
      sourceLevelValue = levelAtIndex sourceLevels sourceIndexValue
   in case filtrationPivotIndex targetLevels (frcsTargetVector reducedColumn) of
        Nothing ->
          state
            { frsReducedColumns =
                FiltrationReducedColumn
                  { filtrationColumnSourceIndex = sourceIndexValue,
                    filtrationColumnSourceLevel = sourceLevelValue,
                    filtrationColumnSourceVector = compactSparseRow (frcsSourceVector reducedColumn),
                    filtrationColumnTargetVector = IntMap.empty,
                    filtrationColumnPivot = Nothing
                  }
                  : frsReducedColumns state
            }
        Just targetIndexValue ->
          let pivotCoefficient =
                sparseRowLookup targetIndexValue (frcsTargetVector reducedColumn)
              normalizedSource =
                scaleSparseRow (recip pivotCoefficient) (frcsSourceVector reducedColumn)
              normalizedTarget =
                scaleSparseRow (recip pivotCoefficient) (frcsTargetVector reducedColumn)
              targetLevelValue = levelAtIndex targetLevels targetIndexValue
              pivotValue =
                FiltrationPivot
                  { filtrationPivotTargetIndex = targetIndexValue,
                    filtrationPivotTargetLevel = targetLevelValue,
                    filtrationPivotDistance = targetLevelValue - sourceLevelValue
                  }
              reducedValue =
                FiltrationReducedColumn
                  { filtrationColumnSourceIndex = sourceIndexValue,
                    filtrationColumnSourceLevel = sourceLevelValue,
                    filtrationColumnSourceVector = normalizedSource,
                    filtrationColumnTargetVector = normalizedTarget,
                    filtrationColumnPivot = Just pivotValue
                  }
           in state
                { frsPivotColumns =
                    IntMap.insert
                      targetIndexValue
                      FiltrationReductionPivotColumn
                        { frpcSourceVector = normalizedSource,
                          frpcTargetVector = normalizedTarget
                        }
                      (frsPivotColumns state),
                  frsReducedColumns = reducedValue : frsReducedColumns state
                }

reduceColumnAgainstFiltrationPivots ::
  Vector.Vector Int ->
  IntMap.IntMap FiltrationReductionPivotColumn ->
  FiltrationReductionColumnState ->
  FiltrationReductionColumnState
reduceColumnAgainstFiltrationPivots targetLevels pivotColumns columnState =
  case filtrationPivotIndex targetLevels (frcsTargetVector columnState) of
    Nothing -> columnState
    Just targetIndexValue ->
      case IntMap.lookup targetIndexValue pivotColumns of
        Nothing -> columnState
        Just pivotColumn ->
          let pivotCoefficient =
                sparseRowLookup targetIndexValue (frcsTargetVector columnState)
              nextColumnState =
                FiltrationReductionColumnState
                  { frcsSourceVector =
                      addScaledSparseRow
                        (negate pivotCoefficient)
                        (frcsSourceVector columnState)
                        (frpcSourceVector pivotColumn),
                    frcsTargetVector =
                      addScaledSparseRow
                        (negate pivotCoefficient)
                        (frcsTargetVector columnState)
                        (frpcTargetVector pivotColumn)
                  }
           in reduceColumnAgainstFiltrationPivots targetLevels pivotColumns nextColumnState

filtrationOrderedIndices :: Vector.Vector Int -> [Int]
filtrationOrderedIndices levels =
  [0 .. Vector.length levels - 1]
    & List.sortOn (filtrationOrderKey levels)

filtrationPivotIndex :: Vector.Vector Int -> AmbientVector -> Maybe Int
filtrationPivotIndex levels rowValue =
  case IntMap.keys (compactSparseRow rowValue) of
    [] -> Nothing
    pivotIndex : remainingIndices ->
      Just
        ( List.foldl'
            ( \bestIndex candidateIndex ->
                if filtrationOrderKey levels candidateIndex > filtrationOrderKey levels bestIndex
                  then candidateIndex
                  else bestIndex
            )
            pivotIndex
            remainingIndices
        )

filtrationOrderKey :: Vector.Vector Int -> Int -> (Int, Int)
filtrationOrderKey levels indexValue =
  (negate (levelAtIndex levels indexValue), indexValue)

levelAtIndex :: Vector.Vector Int -> Int -> Int
levelAtIndex levels indexValue =
  maybe 0 id (levels Vector.!? indexValue)

reduceBasisChecked :: Int -> [AmbientVector] -> Either HomologyFailure [AmbientVector]
reduceBasisChecked ambientDimension basisVectors = do
  compactBasis <- traverse (validateAmbientVector ambientDimension) basisVectors
  pure (independentModuloBasis [] compactBasis)

intersectionBasisChecked ::
  Int ->
  [AmbientVector] ->
  [AmbientVector] ->
  Either HomologyFailure [AmbientVector]
intersectionBasisChecked ambientDimension leftBasis rightBasis = do
  compactLeftBasis <- traverse (validateAmbientVector ambientDimension) leftBasis
  compactRightBasis <- traverse (validateAmbientVector ambientDimension) rightBasis
  if null compactLeftBasis || null compactRightBasis
    then pure []
    else do
      relationMatrix <-
        sparseMatrixFromColumnsChecked
          ambientDimension
          (compactLeftBasis <> fmap negateSparseRow compactRightBasis)
      let relationKernel =
            kernelBasisOfMatrix
              (length compactLeftBasis + length compactRightBasis)
              relationMatrix
          intersectionVectors =
            relationKernel
              & fmap
                ( \relationVector ->
                    sparseLinearCombination
                      compactLeftBasis
                      (coordinatePrefix (length compactLeftBasis) relationVector)
                )
      reduceBasisChecked ambientDimension intersectionVectors

kernelBasisOfMatrix :: Int -> SparseMatrix -> [AmbientVector]
kernelBasisOfMatrix =
  sparseKernelBasisOf

imageBasisOfMatrix :: SparseMatrix -> [AmbientVector]
imageBasisOfMatrix =
  sparseImageBasisOf

independentModuloBasis :: [AmbientVector] -> [AmbientVector] -> [AmbientVector]
independentModuloBasis =
  sparseIndependentModulo

firstVectorOutsideSpan :: [AmbientVector] -> [AmbientVector] -> Maybe AmbientVector
firstVectorOutsideSpan spanBasis candidateVectors =
  let echelonBasis =
        sparseEchelonBasis spanBasis
   in List.find
        (not . sparseEchelonContains echelonBasis)
        candidateVectors

spanRankOfBasis :: [AmbientVector] -> Int
spanRankOfBasis =
  sparseSpanRank

boundaryMatrixAtRational ::
  FiniteChainComplex Rational ->
  HomologicalDegree ->
  Either HomologyFailure SparseMatrix
boundaryMatrixAtRational finite degreeValue@(HomologicalDegree degreeIndex) =
  let incidence = incidenceMatrixAt finite degreeValue
      sourceDimension = cellCountAtDegree finite degreeValue
      targetDimension = cellCountAtDegree finite (HomologicalDegree (degreeIndex - 1))
      invalidEntry =
        boundaryEntries incidence
          & List.find
            ( \entry ->
                sourceIndex entry < 0
                  || sourceIndex entry >= sourceCardinality incidence
                  || targetIndex entry < 0
                  || targetIndex entry >= targetCardinality incidence
                  || sourceIndex entry >= sourceDimension
                  || targetIndex entry >= targetDimension
            )
   in case invalidEntry of
        Just _ ->
          Left (InvalidBoundaryIncidence "boundary incidence entry index is outside the declared cardinalities")
        Nothing ->
          reshapeSparseMatrix targetDimension sourceDimension (sparseBoundaryMatrixWith id incidence)

zeroSparseMatrix :: Int -> Int -> Either HomologyFailure SparseMatrix
zeroSparseMatrix rowCount columnCount
  | rowCount < 0 || columnCount < 0 =
      Left (InvalidMatrixShape "validated matrix received a negative shape")
  | otherwise =
      Right
        SparseMatrix
          { smRows = replicate rowCount IntMap.empty,
            smColumnCount = columnCount
          }

reshapeSparseMatrix :: Int -> Int -> SparseMatrix -> Either HomologyFailure SparseMatrix
reshapeSparseMatrix rowCount columnCount matrixValue = do
  _ <- zeroSparseMatrix rowCount columnCount
  let resizedRows =
        take rowCount (smRows matrixValue)
          <> replicate (max 0 (rowCount - length (smRows matrixValue))) IntMap.empty
  compactRows <- traverse (validateAmbientVector columnCount) resizedRows
  pure
    SparseMatrix
      { smRows = compactRows,
        smColumnCount = columnCount
      }

sparseMatrixDenseRows :: SparseMatrix -> [[Rational]]
sparseMatrixDenseRows matrixValue =
  fmap (sparseRowToDense (smColumnCount matrixValue)) (smRows matrixValue)

sparseMatrixFromRowsChecked :: Int -> [AmbientVector] -> Either HomologyFailure SparseMatrix
sparseMatrixFromRowsChecked columnCount rows = do
  compactRows <- traverse (validateAmbientVector columnCount) rows
  pure
    SparseMatrix
      { smRows = compactRows,
        smColumnCount = columnCount
      }

sparseMatrixFromColumnsChecked :: Int -> [AmbientVector] -> Either HomologyFailure SparseMatrix
sparseMatrixFromColumnsChecked rowCount columnVectors = do
  compactColumns <- traverse (validateAmbientVector rowCount) columnVectors
  pure (sparseTransposeMatrix (SparseMatrix {smRows = compactColumns, smColumnCount = rowCount}))

selectSparseRows ::
  [Int] ->
  SparseMatrix ->
  Either HomologyFailure SparseMatrix
selectSparseRows rowIndices matrixValue = do
  selectedRows <-
    traverse
      ( \rowIndexValue ->
          maybe
            (Left (InvalidMatrixShape "row selection index is outside the matrix bounds"))
            Right
            (elementAt rowIndexValue (smRows matrixValue))
      )
      rowIndices
  sparseMatrixFromRowsChecked (smColumnCount matrixValue) selectedRows

selectSparseColumns ::
  [Int] ->
  SparseMatrix ->
  Either HomologyFailure SparseMatrix
selectSparseColumns columnIndices matrixValue = do
  _ <- traverse (validateColumnIndex matrixValue) columnIndices
  sparseMatrixFromRowsChecked
    (length columnIndices)
    (fmap (selectSparseRowColumns columnIndices) (smRows matrixValue))

applySparseMatrixChecked ::
  SparseMatrix ->
  AmbientVector ->
  Either HomologyFailure AmbientVector
applySparseMatrixChecked matrixValue vectorValue = do
  compactVector <- validateMatrixVector matrixValue vectorValue
  pure (sparseMatrixVectorProduct matrixValue compactVector)

sparseLinearCombination :: [AmbientVector] -> AmbientVector -> AmbientVector
sparseLinearCombination basisVectors coefficients =
  IntMap.foldlWithKey'
    ( \combinedVector basisIndex coefficientValue ->
        case elementAt basisIndex basisVectors of
          Nothing -> combinedVector
          Just basisVector ->
            addScaledSparseRow coefficientValue combinedVector basisVector
    )
    IntMap.empty
    coefficients

validateColumnIndex :: SparseMatrix -> Int -> Either HomologyFailure Int
validateColumnIndex matrixValue columnIndexValue =
  if columnIndexValue < 0 || columnIndexValue >= smColumnCount matrixValue
    then Left (InvalidMatrixShape "column selection index is outside the matrix bounds")
    else Right columnIndexValue

selectSparseRowColumns :: [Int] -> AmbientVector -> AmbientVector
selectSparseRowColumns columnIndices rowValue =
  columnIndices
    & zip [0 :: Int ..]
    & List.foldl'
      ( \selectedRow (selectedColumnIndex, sourceColumnIndex) ->
          let coefficientValue = sparseRowLookup sourceColumnIndex rowValue
           in if coefficientValue == 0
                then selectedRow
                else IntMap.insert selectedColumnIndex coefficientValue selectedRow
      )
      IntMap.empty

validateMatrixVector :: SparseMatrix -> AmbientVector -> Either HomologyFailure AmbientVector
validateMatrixVector matrixValue vectorValue =
  case validateAmbientVector (smColumnCount matrixValue) vectorValue of
    Right compactVector -> Right compactVector
    Left _ ->
      Left
        ( InvalidMatrixShape
            ( "vector length "
                <> show (observedSparseDimension vectorValue)
                <> " does not match the expected matrix width "
                <> show (smColumnCount matrixValue)
            )
        )

validateAmbientVector :: Int -> AmbientVector -> Either HomologyFailure AmbientVector
validateAmbientVector expectedDimension vectorValue =
  let compactVector = compactSparseRow vectorValue
      observedDimension = observedSparseDimension compactVector
      supported =
        IntMap.keys compactVector
          & all (\columnIndex -> columnIndex >= 0 && columnIndex < expectedDimension)
   in if expectedDimension < 0
        then Left (InvalidMatrixShape "validated matrix received a negative shape")
        else
          if supported
            then Right compactVector
            else
              Left
                ( InvalidMatrixShape
                    ( "vector length "
                        <> show observedDimension
                        <> " does not match the expected ambient dimension "
                        <> show expectedDimension
                    )
                )

coordinatePrefix :: Int -> AmbientVector -> AmbientVector
coordinatePrefix prefixLength =
  IntMap.filterWithKey (\columnIndex _ -> columnIndex < prefixLength)

negateSparseRow :: AmbientVector -> AmbientVector
negateSparseRow =
  scaleSparseRow (-1)

elementAt :: Int -> [a] -> Maybe a
elementAt indexValue _
  | indexValue < 0 = Nothing
elementAt indexValue values =
  case drop indexValue values of
    value : _ -> Just value
    [] -> Nothing

observedSparseDimension :: AmbientVector -> Int
observedSparseDimension vectorValue =
  maybe 0 ((+ 1) . fst) (IntMap.lookupMax (compactSparseRow vectorValue))
