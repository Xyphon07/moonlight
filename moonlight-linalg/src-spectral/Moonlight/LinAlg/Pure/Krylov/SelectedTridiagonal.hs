{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}

-- | Selected symmetric-tridiagonal spectral fast path.
module Moonlight.LinAlg.Pure.Krylov.SelectedTridiagonal
  ( SelectedTridiagonalAttempt (..),
    TridiagonalRejection (..),
    selectedSymmetricTridiagonalEigenvalues,
    selectedSymmetricTridiagonalEigenvaluesDirect,
    selectedSymmetricTridiagonalEigenpairColumnsDirect,
    selectedSymmetricTridiagonalEigenpairsDirect,
    selectedSymmetricTridiagonalEigenpairsFromCSR,
    symmetricTridiagonalFromCSR,
    inverseIterationResidualToleranceBound,
  )
where

import Data.Kind (Type)
import Data.Foldable (foldlM)
import Data.List (mapAccumL, sortBy)
import Data.Ord (comparing)
import qualified Data.Vector.Unboxed as U
import Moonlight.Core (MoonlightError (..), fieldValueValid)
import Moonlight.LinAlg.Internal.Eigen.Kernels (epsDouble, safeMinimumDouble)
import Moonlight.LinAlg.Internal.VectorOps (normU)
import Moonlight.LinAlg.Pure.Krylov.Selection
  ( SpectrumEnd (..),
    sortForSpectrumBy,
  )
import Moonlight.LinAlg.Pure.Sparse.Structured
  ( TridiagonalRejection (..),
    symmetricTridiagonalFromCSR,
  )
import Moonlight.LinAlg.Pure.Sparse.Types (SparseCSR)
import Moonlight.LinAlg.Pure.Structured.Tridiagonal
  ( SymmetricTridiagonal,
    mkSymmetricTridiagonalVectors,
    symmetricTridiagonalDiagonalVector,
    symmetricTridiagonalDimension,
    symmetricTridiagonalOffDiagonalVector,
    isPathLaplacianTridiagonal,
  )
import Moonlight.LinAlg.Pure.Spectral.Result
  ( Eigenpairs,
    eigenpairsFromColumns,
  )
import Prelude

type TridiagonalBlock :: Type
data TridiagonalBlock = TridiagonalBlock
  { tridiagonalBlockStart :: !Int,
    tridiagonalBlockDiagonal :: !(U.Vector Double),
    tridiagonalBlockOffDiagonal :: !(U.Vector Double)
  }

type RankInterval :: Type
data RankInterval = RankInterval
  { rankIntervalLowerBound :: !Double,
    rankIntervalUpperBound :: !Double,
    rankIntervalLowerCount :: !Int,
    rankIntervalUpperCount :: !Int,
    rankIntervalRanks :: ![Int],
    rankIntervalIteration :: !Int
  }

type SelectedTridiagonalAttempt :: Type
data SelectedTridiagonalAttempt
  = SelectedTridiagonalSolved !Eigenpairs
  | SelectedTridiagonalNotApplicable !TridiagonalRejection
  deriving stock (Eq, Show)

type SelectedEigenvalue :: Type
data SelectedEigenvalue = SelectedEigenvalue
  { selectedEigenvalueOrdinal :: !Int,
    selectedEigenvalueValue :: !Double
  }
  deriving stock (Eq, Show)

type ClusterBasis :: Type
data ClusterBasis = ClusterBasis
  { clusterBasisVectors :: ![U.Vector Double],
    clusterBasisColumns :: ![(Double, U.Vector Double, Double)]
  }
  deriving stock (Eq, Show)

type InverseIterationState :: Type
data InverseIterationState
  = InverseIterationSearching !(U.Vector Double)
  | InverseIterationConverged !(U.Vector Double) !Double
  deriving stock (Eq, Show)

type SelectedTridiagonalPairObstruction :: Type
data SelectedTridiagonalPairObstruction
  = SelectedTridiagonalInverseIterationNonConverged !Int !Double !Double
  | SelectedTridiagonalSolveNonFinite !Int !Double
  | SelectedTridiagonalVectorDegenerate !Int !Double
  | SelectedTridiagonalClusterBasisUnstable !Int !Double
  deriving stock (Eq, Show)

selectedSymmetricTridiagonalEigenpairsFromCSR ::
  SpectrumEnd ->
  Int ->
  SparseCSR Double ->
  Either MoonlightError SelectedTridiagonalAttempt
selectedSymmetricTridiagonalEigenpairsFromCSR spectrumEnd requestedCount csrValue
  | requestedCount <= 0 = Left (InvariantViolation "selected tridiagonal eigensolve requires a positive requested count")
  | otherwise = do
      selectedOperator <- symmetricTridiagonalFromCSR csrValue
      case selectedOperator of
        Left rejection -> Right (SelectedTridiagonalNotApplicable rejection)
        Right tridiagonalValue ->
          SelectedTridiagonalSolved
            <$> selectedSymmetricTridiagonalEigenpairs
              spectrumEnd
              requestedCount
              tridiagonalValue

selectedSymmetricTridiagonalEigenvalues ::
  SpectrumEnd ->
  Int ->
  SymmetricTridiagonal ->
  Either MoonlightError (U.Vector Double)
selectedSymmetricTridiagonalEigenvalues spectrumEnd requestedCount tridiagonalValue
  | requestedCount <= 0 = Left (InvariantViolation "selected tridiagonal eigenvalue solve requires a positive requested count")
  | requestedCount > symmetricTridiagonalDimension tridiagonalValue =
      Left (InvariantViolation "selected tridiagonal eigenvalue count exceeds operator dimension")
  | otherwise = selectedSymmetricTridiagonalEigenvaluesChecked spectrumEnd requestedCount tridiagonalValue

selectedSymmetricTridiagonalEigenvaluesDirect ::
  SpectrumEnd ->
  Int ->
  SymmetricTridiagonal ->
  Either MoonlightError (U.Vector Double)
selectedSymmetricTridiagonalEigenvaluesDirect spectrumEnd requestedCount tridiagonalValue
  | requestedCount <= 0 = Left (InvariantViolation "selected tridiagonal eigenvalue solve requires a positive requested count")
  | requestedCount > symmetricTridiagonalDimension tridiagonalValue =
      Left (InvariantViolation "selected tridiagonal eigenvalue count exceeds operator dimension")
  | otherwise = selectedSymmetricTridiagonalEigenvaluesChecked spectrumEnd requestedCount tridiagonalValue

selectedSymmetricTridiagonalEigenpairsDirect ::
  SpectrumEnd ->
  Int ->
  SymmetricTridiagonal ->
  Either MoonlightError Eigenpairs
selectedSymmetricTridiagonalEigenpairsDirect spectrumEnd requestedCount tridiagonalValue
  | requestedCount <= 0 = Left (InvariantViolation "selected tridiagonal eigenpair solve requires a positive requested count")
  | otherwise = selectedSymmetricTridiagonalEigenpairs spectrumEnd requestedCount tridiagonalValue

selectedSymmetricTridiagonalEigenpairColumnsDirect ::
  SpectrumEnd ->
  Int ->
  SymmetricTridiagonal ->
  Either MoonlightError [(Double, U.Vector Double, Double)]
selectedSymmetricTridiagonalEigenpairColumnsDirect spectrumEnd requestedCount tridiagonalValue
  | requestedCount <= 0 = Left (InvariantViolation "selected tridiagonal eigenpair solve requires a positive requested count")
  | requestedCount > symmetricTridiagonalDimension tridiagonalValue =
      Left (InvariantViolation "selected tridiagonal eigenpair count exceeds operator dimension")
  | otherwise = selectedSymmetricTridiagonalEigenpairColumns spectrumEnd requestedCount tridiagonalValue

selectedSymmetricTridiagonalEigenpairs ::
  SpectrumEnd ->
  Int ->
  SymmetricTridiagonal ->
  Either MoonlightError Eigenpairs
selectedSymmetricTridiagonalEigenpairs spectrumEnd requestedCount tridiagonalValue =
  let !matrixSize = U.length (symmetricTridiagonalDiagonalVector tridiagonalValue)
   in if requestedCount > matrixSize
        then Left (InvariantViolation "selected tridiagonal eigenpair count exceeds operator dimension")
        else
          eigenpairsFromColumns matrixSize
            =<< selectedSymmetricTridiagonalEigenpairColumns spectrumEnd requestedCount tridiagonalValue

selectedSymmetricTridiagonalEigenpairColumns ::
  SpectrumEnd ->
  Int ->
  SymmetricTridiagonal ->
  Either MoonlightError [(Double, U.Vector Double, Double)]
selectedSymmetricTridiagonalEigenpairColumns spectrumEnd requestedCount tridiagonalValue =
  case pathLaplacianEigenpairs spectrumEnd requestedCount tridiagonalValue of
    Just pathPairs -> Right pathPairs
    Nothing ->
      case diagonalOperatorEigenpairs spectrumEnd tridiagonalValue of
        Just diagonalPairs -> Right (take requestedCount diagonalPairs)
        Nothing ->
          if U.any (== 0.0) (symmetricTridiagonalOffDiagonalVector tridiagonalValue)
            then selectedReducibleTridiagonalEigenpairColumnsViaInverseIteration spectrumEnd requestedCount tridiagonalValue
            else selectedTridiagonalEigenpairColumnsViaInverseIteration spectrumEnd requestedCount tridiagonalValue

selectedSymmetricTridiagonalEigenvaluesChecked ::
  SpectrumEnd ->
  Int ->
  SymmetricTridiagonal ->
  Either MoonlightError (U.Vector Double)
selectedSymmetricTridiagonalEigenvaluesChecked spectrumEnd requestedCount tridiagonalValue =
  case pathLaplacianEigenvalues spectrumEnd requestedCount tridiagonalValue of
    Just pathValues -> Right pathValues
    Nothing ->
      case diagonalOperatorEigenvalues spectrumEnd tridiagonalValue of
        Just diagonalValues -> Right (U.take requestedCount diagonalValues)
        Nothing ->
          if U.any (== 0.0) (symmetricTridiagonalOffDiagonalVector tridiagonalValue)
            then selectedReducibleTridiagonalEigenvaluesViaSturm spectrumEnd requestedCount tridiagonalValue
            else
              Right
                (selectedIrreducibleTridiagonalEigenvalues spectrumEnd requestedCount tridiagonalValue)

pathLaplacianEigenvalues :: SpectrumEnd -> Int -> SymmetricTridiagonal -> Maybe (U.Vector Double)
pathLaplacianEigenvalues spectrumEnd boundedCount tridiagonalValue =
  let !matrixSize = U.length (symmetricTridiagonalDiagonalVector tridiagonalValue)
   in if isPathLaplacianTridiagonal tridiagonalValue
        then
          Just
            ( U.generate
                boundedCount
                ( \entryIndex ->
                    pathLaplacianEigenvalueAt matrixSize $
                      case spectrumEnd of
                        SmallestEigenvalues -> entryIndex
                        LargestEigenvalues -> matrixSize - entryIndex - 1
                )
            )
        else Nothing

pathLaplacianEigenpairs :: SpectrumEnd -> Int -> SymmetricTridiagonal -> Maybe [(Double, U.Vector Double, Double)]
pathLaplacianEigenpairs spectrumEnd boundedCount tridiagonalValue =
  let !matrixSize = U.length (symmetricTridiagonalDiagonalVector tridiagonalValue)
   in if isPathLaplacianTridiagonal tridiagonalValue
        then
          Just
            ( pathLaplacianEigenpairAt matrixSize
                <$> case spectrumEnd of
                  SmallestEigenvalues -> [0 .. boundedCount - 1]
                  LargestEigenvalues -> [matrixSize - 1, matrixSize - 2 .. matrixSize - boundedCount]
            )
        else Nothing

pathLaplacianEigenpairAt :: Int -> Int -> (Double, U.Vector Double, Double)
pathLaplacianEigenpairAt !matrixSize !modeIndex =
  let !theta = pi * fromIntegral modeIndex / fromIntegral (max 1 matrixSize)
      !eigenvalue = pathLaplacianEigenvalueAt matrixSize modeIndex
      !eigenvector =
        if modeIndex == 0
          then U.replicate matrixSize (1.0 / sqrt (fromIntegral (max 1 matrixSize)))
          else
            let !normalizer = sqrt (2.0 / fromIntegral matrixSize)
             in U.generate
                  matrixSize
                  (\rowIndex -> normalizer * cos (theta * (fromIntegral rowIndex + 0.5)))
      !residualNorm = pathLaplacianResidualNorm matrixSize eigenvalue eigenvector
   in (eigenvalue, eigenvector, residualNorm)

pathLaplacianEigenvalueAt :: Int -> Int -> Double
pathLaplacianEigenvalueAt !matrixSize !modeIndex =
  2.0 - 2.0 * cos (pi * fromIntegral modeIndex / fromIntegral (max 1 matrixSize))
{-# INLINE pathLaplacianEigenvalueAt #-}

pathLaplacianResidualNorm :: Int -> Double -> U.Vector Double -> Double
pathLaplacianResidualNorm !matrixSize !eigenvalue eigenvector =
  sqrt
    ( U.ifoldl'
        ( \ !squaredNorm !rowIndex _ ->
            let !residualEntry = pathLaplacianResidualEntry matrixSize eigenvalue eigenvector rowIndex
             in squaredNorm + residualEntry * residualEntry
        )
        0.0
        eigenvector
    )

pathLaplacianResidualEntry :: Int -> Double -> U.Vector Double -> Int -> Double
pathLaplacianResidualEntry !matrixSize !eigenvalue eigenvector !rowIndex =
  let !centerValue = eigenvector `U.unsafeIndex` rowIndex
      !degree
        | matrixSize == 1 = 0.0
        | rowIndex == 0 || rowIndex + 1 == matrixSize = 1.0
        | otherwise = 2.0
      !leftValue =
        if rowIndex <= 0
          then 0.0
          else eigenvector `U.unsafeIndex` (rowIndex - 1)
      !rightValue =
        if rowIndex + 1 >= matrixSize
          then 0.0
          else eigenvector `U.unsafeIndex` (rowIndex + 1)
      !imageValue = degree * centerValue - leftValue - rightValue
   in imageValue - eigenvalue * centerValue
{-# INLINE pathLaplacianResidualEntry #-}

diagonalOperatorEigenvalues :: SpectrumEnd -> SymmetricTridiagonal -> Maybe (U.Vector Double)
diagonalOperatorEigenvalues spectrumEnd tridiagonalValue =
  if U.all (== 0.0) (symmetricTridiagonalOffDiagonalVector tridiagonalValue)
    then
      Just
        ( U.fromList
            ( snd
                <$> sortForSpectrum
                  spectrumEnd
                  (U.toList (U.indexed (symmetricTridiagonalDiagonalVector tridiagonalValue)))
            )
        )
    else Nothing

diagonalOperatorEigenpairs :: SpectrumEnd -> SymmetricTridiagonal -> Maybe [(Double, U.Vector Double, Double)]
diagonalOperatorEigenpairs spectrumEnd tridiagonalValue =
  if U.all (== 0.0) (symmetricTridiagonalOffDiagonalVector tridiagonalValue)
    then
      Just
        ( fmap
            ( \(entryIndex, eigenvalue) ->
                ( eigenvalue,
                  unitVector (U.length (symmetricTridiagonalDiagonalVector tridiagonalValue)) entryIndex,
                  0.0
                )
            )
            ( sortForSpectrum
                spectrumEnd
                (U.toList (U.indexed (symmetricTridiagonalDiagonalVector tridiagonalValue)))
            )
        )
    else Nothing

selectedIrreducibleTridiagonalEigenvalues ::
  SpectrumEnd ->
  Int ->
  SymmetricTridiagonal ->
  U.Vector Double
selectedIrreducibleTridiagonalEigenvalues spectrumEnd boundedCount tridiagonalValue =
  let !matrixSize = U.length (symmetricTridiagonalDiagonalVector tridiagonalValue)
      selectedRanks =
        case spectrumEnd of
          SmallestEigenvalues -> [1 .. boundedCount]
          LargestEigenvalues -> [matrixSize - boundedCount + 1 .. matrixSize]
      selectedValues = U.fromList (batchedBisectEigenvaluesAtRanks tridiagonalValue selectedRanks)
   in case spectrumEnd of
        SmallestEigenvalues -> selectedValues
        LargestEigenvalues -> U.reverse selectedValues

batchedBisectEigenvaluesAtRanks :: SymmetricTridiagonal -> [Int] -> [Double]
batchedBisectEigenvaluesAtRanks tridiagonalValue selectedRanks =
  let (!initialLower, !initialUpper) = gershgorinBounds tridiagonalValue
      !matrixSize = U.length (symmetricTridiagonalDiagonalVector tridiagonalValue)
      !matrixScale = tridiagonalInfinityNormBound tridiagonalValue
      initialInterval =
        RankInterval
          { rankIntervalLowerBound = initialLower,
            rankIntervalUpperBound = initialUpper,
            rankIntervalLowerCount = 0,
            rankIntervalUpperCount = matrixSize,
            rankIntervalRanks = selectedRanks,
            rankIntervalIteration = 0
          }
   in snd
        <$> sortBy
          (comparing fst)
          (refineRankInterval matrixScale tridiagonalValue initialInterval)

refineRankInterval :: Double -> SymmetricTridiagonal -> RankInterval -> [(Int, Double)]
refineRankInterval !matrixScale tridiagonalValue interval
  | null (rankIntervalRanks interval) = []
  | rankIntervalIteration interval >= tridiagonalBisectionIterationLimit =
      finalizeRankInterval interval
  | rankIntervalUpperBound interval - rankIntervalLowerBound interval
      <= eigenTolerance matrixScale (rankIntervalLowerBound interval) (rankIntervalUpperBound interval) =
      finalizeRankInterval interval
  | rankIntervalUpperBound interval == rankIntervalLowerBound interval =
      finalizeRankInterval interval
  | otherwise =
      concatMap
        (refineRankInterval matrixScale tridiagonalValue)
        (splitRankInterval matrixScale tridiagonalValue interval)

splitRankInterval :: Double -> SymmetricTridiagonal -> RankInterval -> [RankInterval]
splitRankInterval !matrixScale tridiagonalValue interval =
  maybeInterval
    lowerRanks
    (rankIntervalLowerBound interval)
    middleValue
    (rankIntervalLowerCount interval)
    middleCount
    <> maybeInterval
      upperRanks
      middleValue
      (rankIntervalUpperBound interval)
      middleCount
      (rankIntervalUpperCount interval)
  where
    !middleValue = midpoint (rankIntervalLowerBound interval) (rankIntervalUpperBound interval)
    !middleCount =
      clamp
        (rankIntervalLowerCount interval)
        (rankIntervalUpperCount interval)
        (sturmCountLessEqual matrixScale tridiagonalValue middleValue)
    lowerRanks = filter (<= middleCount) (rankIntervalRanks interval)
    upperRanks = filter (> middleCount) (rankIntervalRanks interval)
    maybeInterval ranks lowerBound upperBound lowerCount upperCount =
      if null ranks
        then []
        else
          [ RankInterval
              { rankIntervalLowerBound = lowerBound,
                rankIntervalUpperBound = upperBound,
                rankIntervalLowerCount = lowerCount,
                rankIntervalUpperCount = upperCount,
                rankIntervalRanks = ranks,
                rankIntervalIteration = rankIntervalIteration interval + 1
              }
          ]

finalizeRankInterval :: RankInterval -> [(Int, Double)]
finalizeRankInterval interval =
  (\rankValue -> (rankValue, midpoint (rankIntervalLowerBound interval) (rankIntervalUpperBound interval)))
    <$> rankIntervalRanks interval

tridiagonalBisectionIterationLimit :: Int
tridiagonalBisectionIterationLimit = 80
{-# INLINE tridiagonalBisectionIterationLimit #-}

selectedTridiagonalEigenpairColumnsViaInverseIteration ::
  SpectrumEnd ->
  Int ->
  SymmetricTridiagonal ->
  Either MoonlightError [(Double, U.Vector Double, Double)]
selectedTridiagonalEigenpairColumnsViaInverseIteration spectrumEnd requestedCount tridiagonalValue =
  selectedTridiagonalPairResultToEither
    ( selectedEigenpairColumnsFromValues
        tridiagonalValue
        ( U.toList
            (selectedIrreducibleTridiagonalEigenvalues spectrumEnd requestedCount tridiagonalValue)
        )
    )

selectedReducibleTridiagonalEigenvaluesViaSturm ::
  SpectrumEnd ->
  Int ->
  SymmetricTridiagonal ->
  Either MoonlightError (U.Vector Double)
selectedReducibleTridiagonalEigenvaluesViaSturm spectrumEnd requestedCount tridiagonalValue =
  fmap
    (U.fromList . take requestedCount . sortForSpectrumBy spectrumEnd id . concat)
    (traverse (blockEigenvaluesViaSturm spectrumEnd requestedCount) (tridiagonalBlocks tridiagonalValue))

selectedReducibleTridiagonalEigenpairColumnsViaInverseIteration ::
  SpectrumEnd ->
  Int ->
  SymmetricTridiagonal ->
  Either MoonlightError [(Double, U.Vector Double, Double)]
selectedReducibleTridiagonalEigenpairColumnsViaInverseIteration spectrumEnd requestedCount tridiagonalValue =
  fmap
    ( take requestedCount
        . sortForSpectrumBy spectrumEnd (\(eigenvalue, _, _) -> eigenvalue)
        . concat
    )
    (traverse (blockEigenpairColumnsViaInverseIteration spectrumEnd requestedCount tridiagonalValue) (tridiagonalBlocks tridiagonalValue))

blockEigenvaluesViaSturm :: SpectrumEnd -> Int -> TridiagonalBlock -> Either MoonlightError [Double]
blockEigenvaluesViaSturm spectrumEnd requestedCount blockValue = do
  blockTridiagonal <-
    mkSymmetricTridiagonalVectors
      (tridiagonalBlockDiagonal blockValue)
      (tridiagonalBlockOffDiagonal blockValue)
  let blockRequestedCount = min requestedCount (symmetricTridiagonalDimension blockTridiagonal)
  Right (U.toList (selectedIrreducibleTridiagonalEigenvalues spectrumEnd blockRequestedCount blockTridiagonal))

blockEigenpairColumnsViaInverseIteration ::
  SpectrumEnd ->
  Int ->
  SymmetricTridiagonal ->
  TridiagonalBlock ->
  Either MoonlightError [(Double, U.Vector Double, Double)]
blockEigenpairColumnsViaInverseIteration spectrumEnd requestedCount tridiagonalValue blockValue = do
  blockTridiagonal <-
    mkSymmetricTridiagonalVectors
      (tridiagonalBlockDiagonal blockValue)
      (tridiagonalBlockOffDiagonal blockValue)
  let blockRequestedCount = min requestedCount (symmetricTridiagonalDimension blockTridiagonal)
  selectedTridiagonalPairResultToEither
    ( fmap
        (fmap (embedBlockEigenpairColumn tridiagonalValue blockValue))
        ( selectedEigenpairColumnsFromValues
            blockTridiagonal
            ( U.toList
                (selectedIrreducibleTridiagonalEigenvalues spectrumEnd blockRequestedCount blockTridiagonal)
            )
        )
    )

embedBlockEigenpairColumn ::
  SymmetricTridiagonal ->
  TridiagonalBlock ->
  (Double, U.Vector Double, Double) ->
  (Double, U.Vector Double, Double)
embedBlockEigenpairColumn tridiagonalValue blockValue (eigenvalue, blockVector, _) =
  let eigenvector =
        embedBlockVector
          (symmetricTridiagonalDimension tridiagonalValue)
          (tridiagonalBlockStart blockValue)
          blockVector
   in (eigenvalue, eigenvector, tridiagonalResidualNorm tridiagonalValue eigenvalue eigenvector)

embedBlockVector :: Int -> Int -> U.Vector Double -> U.Vector Double
embedBlockVector dimension startOffset blockVector =
  U.generate
    dimension
    ( \entryIndex ->
        if entryIndex >= startOffset && entryIndex < startOffset + U.length blockVector
          then vectorEntryOrZero blockVector (entryIndex - startOffset)
          else 0.0
    )

tridiagonalBlocks :: SymmetricTridiagonal -> [TridiagonalBlock]
tridiagonalBlocks tridiagonalValue =
  makeBlock <$> blockRanges (symmetricTridiagonalOffDiagonalVector tridiagonalValue) (symmetricTridiagonalDimension tridiagonalValue)
  where
    diagonalEntries = symmetricTridiagonalDiagonalVector tridiagonalValue
    offDiagonalEntries = symmetricTridiagonalOffDiagonalVector tridiagonalValue
    makeBlock (startIndex, stopIndex) =
      let blockSize = stopIndex - startIndex
       in TridiagonalBlock
            { tridiagonalBlockStart = startIndex,
              tridiagonalBlockDiagonal = U.slice startIndex blockSize diagonalEntries,
              tridiagonalBlockOffDiagonal = U.slice startIndex (max 0 (blockSize - 1)) offDiagonalEntries
            }

blockRanges :: U.Vector Double -> Int -> [(Int, Int)]
blockRanges offDiagonalEntries dimension =
  filter
    (\(startIndex, stopIndex) -> startIndex < stopIndex)
    (zip splitStarts splitStops)
  where
    zeroIndices = zeroCouplingIndices offDiagonalEntries
    splitStarts = 0 : fmap (+ 1) zeroIndices
    splitStops = fmap (+ 1) zeroIndices <> [dimension]

zeroCouplingIndices :: U.Vector Double -> [Int]
zeroCouplingIndices offDiagonalEntries =
  fst <$> filter ((== 0.0) . snd) (U.toList (U.indexed offDiagonalEntries))

vectorEntryOrZero :: U.Vector Double -> Int -> Double
vectorEntryOrZero values indexValue =
  maybe 0.0 id (values U.!? indexValue)
{-# INLINE vectorEntryOrZero #-}

selectedEigenpairColumnsFromValues ::
  SymmetricTridiagonal ->
  [Double] ->
  Either SelectedTridiagonalPairObstruction [(Double, U.Vector Double, Double)]
selectedEigenpairColumnsFromValues tridiagonalValue eigenvalues =
  fmap
    concat
    ( traverse
        (solveSelectedEigenvalueCluster tridiagonalValue)
        (clusterSelectedEigenvalues (tridiagonalInfinityNormBound tridiagonalValue) (zipWith SelectedEigenvalue [0 ..] eigenvalues))
    )

solveSelectedEigenvalueCluster ::
  SymmetricTridiagonal ->
  [SelectedEigenvalue] ->
  Either SelectedTridiagonalPairObstruction [(Double, U.Vector Double, Double)]
solveSelectedEigenvalueCluster tridiagonalValue eigenvalueCluster =
  clusterBasisColumns
    <$> foldlM
      (appendSelectedEigenpairColumn tridiagonalValue)
      ClusterBasis {clusterBasisVectors = [], clusterBasisColumns = []}
      eigenvalueCluster

appendSelectedEigenpairColumn ::
  SymmetricTridiagonal ->
  ClusterBasis ->
  SelectedEigenvalue ->
  Either SelectedTridiagonalPairObstruction ClusterBasis
appendSelectedEigenpairColumn tridiagonalValue basis selectedValue = do
  column@(_, eigenvector, _) <-
    solveSelectedEigenpairColumn
      tridiagonalValue
      (clusterBasisVectors basis)
      selectedValue
  Right
    basis
      { clusterBasisVectors = clusterBasisVectors basis <> [eigenvector],
        clusterBasisColumns = clusterBasisColumns basis <> [column]
      }

solveSelectedEigenpairColumn ::
  SymmetricTridiagonal ->
  [U.Vector Double] ->
  SelectedEigenvalue ->
  Either SelectedTridiagonalPairObstruction (Double, U.Vector Double, Double)
solveSelectedEigenpairColumn tridiagonalValue clusterVectors selectedValue =
  let !matrixScale = tridiagonalInfinityNormBound tridiagonalValue
      !eigenvalue = selectedEigenvalueValue selectedValue
      !ordinal = selectedEigenvalueOrdinal selectedValue
      attempts =
        inverseIterationAttempt
          tridiagonalValue
          matrixScale
          clusterVectors
          selectedValue
          <$> inverseIterationShiftSchedule matrixScale eigenvalue ordinal
   in firstSuccessfulAttempt
        (SelectedTridiagonalInverseIterationNonConverged ordinal eigenvalue (inverseIterationResidualTolerance matrixScale eigenvalue tridiagonalValue))
        attempts

inverseIterationAttempt ::
  SymmetricTridiagonal ->
  Double ->
  [U.Vector Double] ->
  SelectedEigenvalue ->
  Double ->
  Either SelectedTridiagonalPairObstruction (Double, U.Vector Double, Double)
inverseIterationAttempt tridiagonalValue !matrixScale clusterVectors selectedValue !shiftValue = do
  let !eigenvalue = selectedEigenvalueValue selectedValue
      !ordinal = selectedEigenvalueOrdinal selectedValue
      !initialVector =
        inverseIterationSeed
          (symmetricTridiagonalDimension tridiagonalValue)
          ordinal
      !residualLimit = inverseIterationResidualTolerance matrixScale eigenvalue tridiagonalValue
  finalState <-
    foldlM
      (inverseIterationStep tridiagonalValue matrixScale clusterVectors selectedValue shiftValue residualLimit)
      (InverseIterationSearching initialVector)
      [1 .. inverseIterationStepLimit]
  case finalState of
    InverseIterationConverged eigenvector residualNorm ->
      Right (tridiagonalRayleighQuotient tridiagonalValue eigenvector, eigenvector, residualNorm)
    InverseIterationSearching eigenvector ->
      let !certifiedEigenvalue = tridiagonalRayleighQuotient tridiagonalValue eigenvector
          !residualNorm = tridiagonalResidualNorm tridiagonalValue certifiedEigenvalue eigenvector
       in Left (SelectedTridiagonalInverseIterationNonConverged ordinal eigenvalue residualNorm)

inverseIterationStep ::
  SymmetricTridiagonal ->
  Double ->
  [U.Vector Double] ->
  SelectedEigenvalue ->
  Double ->
  Double ->
  InverseIterationState ->
  Int ->
  Either SelectedTridiagonalPairObstruction InverseIterationState
inverseIterationStep _ _ _ _ _ _ converged@(InverseIterationConverged _ _) _ =
  Right converged
inverseIterationStep tridiagonalValue !matrixScale clusterVectors selectedValue !shiftValue !residualLimit (InverseIterationSearching eigenvector) _ = do
  solvedVector <-
    solveShiftedTridiagonal
      matrixScale
      tridiagonalValue
      selectedValue
      shiftValue
      eigenvector
  normalizedVector <-
    normalizeClusterVector
      matrixScale
      selectedValue
      clusterVectors
      solvedVector
  let !residualNorm =
        tridiagonalResidualNorm
          tridiagonalValue
          (tridiagonalRayleighQuotient tridiagonalValue normalizedVector)
          normalizedVector
  Right
    ( if residualNorm <= residualLimit
        then InverseIterationConverged normalizedVector residualNorm
        else InverseIterationSearching normalizedVector
    )

solveShiftedTridiagonal ::
  Double ->
  SymmetricTridiagonal ->
  SelectedEigenvalue ->
  Double ->
  U.Vector Double ->
  Either SelectedTridiagonalPairObstruction (U.Vector Double)
solveShiftedTridiagonal !matrixScale tridiagonalValue selectedValue !shiftValue rhsVector =
  let diagonalEntries = symmetricTridiagonalDiagonalVector tridiagonalValue
      offDiagonalEntries = symmetricTridiagonalOffDiagonalVector tridiagonalValue
      !matrixSize = U.length diagonalEntries
      forwardStep (!previousUpper, !previousRhs) !entryIndex =
        let !diagonalPivot = (diagonalEntries `U.unsafeIndex` entryIndex) - shiftValue
            !lowerEntry =
              if entryIndex <= 0
                then 0.0
                else offDiagonalEntries `U.unsafeIndex` (entryIndex - 1)
            !rawPivot = diagonalPivot - lowerEntry * previousUpper
            !pivotValue = safeTridiagonalSolvePivot matrixScale rawPivot
            !upperEntry =
              if entryIndex + 1 >= matrixSize
                then 0.0
                else offDiagonalEntries `U.unsafeIndex` entryIndex
            !forwardUpper = upperEntry / pivotValue
            !forwardRhs = ((rhsVector `U.unsafeIndex` entryIndex) - lowerEntry * previousRhs) / pivotValue
         in ((forwardUpper, forwardRhs), (forwardUpper, forwardRhs))
      (_, forwardValues) =
        mapAccumL
          forwardStep
          (0.0, 0.0)
          [0 .. matrixSize - 1]
      (solutionValues, _) =
        foldr
          ( \(!forwardUpper, !forwardRhs) (!accumulatedValues, !nextValue) ->
              let !solutionValue = forwardRhs - forwardUpper * nextValue
               in (solutionValue : accumulatedValues, solutionValue)
          )
          ([], 0.0)
          forwardValues
      solutionVector = U.fromList solutionValues
   in if U.all fieldValueValid solutionVector
        then Right solutionVector
        else Left (SelectedTridiagonalSolveNonFinite (selectedEigenvalueOrdinal selectedValue) (selectedEigenvalueValue selectedValue))

normalizeClusterVector ::
  Double ->
  SelectedEigenvalue ->
  [U.Vector Double] ->
  U.Vector Double ->
  Either SelectedTridiagonalPairObstruction (U.Vector Double)
normalizeClusterVector !matrixScale selectedValue clusterVectors candidateVector = do
  firstPass <-
    normalizeSelectedVector
      matrixScale
      selectedValue
      (orthogonalizeAgainst clusterVectors candidateVector)
  secondPass <-
    normalizeSelectedVector
      matrixScale
      selectedValue
      (orthogonalizeAgainst clusterVectors firstPass)
  let !largestOverlap =
        maximum
          (0.0 : (abs . vectorDot secondPass <$> clusterVectors))
   in if largestOverlap <= clusterOrthogonalityTolerance matrixScale (U.length secondPass)
        then Right secondPass
        else Left (SelectedTridiagonalClusterBasisUnstable (selectedEigenvalueOrdinal selectedValue) (selectedEigenvalueValue selectedValue))

normalizeSelectedVector ::
  Double ->
  SelectedEigenvalue ->
  U.Vector Double ->
  Either SelectedTridiagonalPairObstruction (U.Vector Double)
normalizeSelectedVector !matrixScale selectedValue vectorValue =
  let !vectorNorm = normU vectorValue
   in if fieldValueValid vectorNorm && vectorNorm > vectorNormTolerance matrixScale (U.length vectorValue)
        then Right (U.map (/ vectorNorm) vectorValue)
        else Left (SelectedTridiagonalVectorDegenerate (selectedEigenvalueOrdinal selectedValue) (selectedEigenvalueValue selectedValue))

orthogonalizeAgainst :: [U.Vector Double] -> U.Vector Double -> U.Vector Double
orthogonalizeAgainst basisVectors vectorValue =
  foldl'
    ( \candidateVector basisVector ->
        let !projectionScale = vectorDot candidateVector basisVector
         in U.zipWith
              (\candidateEntry basisEntry -> candidateEntry - projectionScale * basisEntry)
              candidateVector
              basisVector
    )
    vectorValue
    basisVectors

vectorDot :: U.Vector Double -> U.Vector Double -> Double
vectorDot leftVector rightVector =
  U.sum (U.zipWith (*) leftVector rightVector)
{-# INLINE vectorDot #-}

inverseIterationSeed :: Int -> Int -> U.Vector Double
inverseIterationSeed !matrixSize !ordinal =
  U.generate
    matrixSize
    ( \entryIndex ->
        let !phase =
              fromIntegral ((entryIndex + 1) * (ordinal + 1))
                * pi
                / fromIntegral (matrixSize + ordinal + 2)
         in sin phase + 0.5 * cos (phase * 0.5)
    )

clusterSelectedEigenvalues :: Double -> [SelectedEigenvalue] -> [[SelectedEigenvalue]]
clusterSelectedEigenvalues !matrixScale =
  reverse
    . fmap reverse
    . foldl' appendEigenvalueCluster []
  where
    appendEigenvalueCluster [] eigenvalue = [[eigenvalue]]
    appendEigenvalueCluster (cluster@(previousEigenvalue : _) : restClusters) eigenvalue
      | eigenvalueGapInCluster matrixScale previousEigenvalue eigenvalue =
          (eigenvalue : cluster) : restClusters
      | otherwise = [eigenvalue] : cluster : restClusters
    appendEigenvalueCluster ([] : restClusters) eigenvalue = [eigenvalue] : restClusters

eigenvalueGapInCluster :: Double -> SelectedEigenvalue -> SelectedEigenvalue -> Bool
eigenvalueGapInCluster !matrixScale leftValue rightValue =
  abs (selectedEigenvalueValue leftValue - selectedEigenvalueValue rightValue)
    <= eigenvalueClusterTolerance matrixScale (selectedEigenvalueValue leftValue) (selectedEigenvalueValue rightValue)

inverseIterationShiftSchedule :: Double -> Double -> Int -> [Double]
inverseIterationShiftSchedule !matrixScale !eigenvalue !ordinal =
  (eigenvalue +)
    <$> fmap
      (* inverseIterationShiftUnit matrixScale eigenvalue)
      (0.0 : concatMap signedShift [1 .. inverseIterationShiftAttemptLimit])
  where
    signedShift attemptIndex =
      let !shiftMagnitude = fromIntegral attemptIndex
       in if even (ordinal + attemptIndex)
            then [shiftMagnitude, negate shiftMagnitude]
            else [negate shiftMagnitude, shiftMagnitude]

firstSuccessfulAttempt :: SelectedTridiagonalPairObstruction -> [Either SelectedTridiagonalPairObstruction value] -> Either SelectedTridiagonalPairObstruction value
firstSuccessfulAttempt fallbackObstruction =
  foldr
    ( \attemptValue remainingAttempts ->
        case attemptValue of
          Right resultValue -> Right resultValue
          Left _ -> remainingAttempts
    )
    (Left fallbackObstruction)

selectedTridiagonalPairResultToEither :: Either SelectedTridiagonalPairObstruction value -> Either MoonlightError value
selectedTridiagonalPairResultToEither resultValue =
  case resultValue of
    Right value -> Right value
    Left obstruction -> Left (InvariantViolation (renderSelectedTridiagonalPairObstruction obstruction))

renderSelectedTridiagonalPairObstruction :: SelectedTridiagonalPairObstruction -> String
renderSelectedTridiagonalPairObstruction obstruction =
  case obstruction of
    SelectedTridiagonalInverseIterationNonConverged ordinal eigenvalue residualNorm ->
      "selected tridiagonal inverse iteration did not converge at ordinal "
        <> show ordinal
        <> " for eigenvalue "
        <> show eigenvalue
        <> " with residual "
        <> show residualNorm
    SelectedTridiagonalSolveNonFinite ordinal eigenvalue ->
      "selected tridiagonal inverse iteration produced a non-finite solve at ordinal "
        <> show ordinal
        <> " for eigenvalue "
        <> show eigenvalue
    SelectedTridiagonalVectorDegenerate ordinal eigenvalue ->
      "selected tridiagonal inverse iteration produced a degenerate vector at ordinal "
        <> show ordinal
        <> " for eigenvalue "
        <> show eigenvalue
    SelectedTridiagonalClusterBasisUnstable ordinal eigenvalue ->
      "selected tridiagonal clustered basis could not be stabilized at ordinal "
        <> show ordinal
        <> " for eigenvalue "
        <> show eigenvalue

inverseIterationStepLimit :: Int
inverseIterationStepLimit = 16
{-# INLINE inverseIterationStepLimit #-}

inverseIterationShiftAttemptLimit :: Int
inverseIterationShiftAttemptLimit = 4
{-# INLINE inverseIterationShiftAttemptLimit #-}

inverseIterationShiftUnit :: Double -> Double -> Double
inverseIterationShiftUnit !matrixScale !eigenvalue =
  16.0 * epsDouble * max 1.0 (max matrixScale (abs eigenvalue))
{-# INLINE inverseIterationShiftUnit #-}

inverseIterationResidualTolerance :: Double -> Double -> SymmetricTridiagonal -> Double
inverseIterationResidualTolerance !matrixScale !eigenvalue tridiagonalValue =
  inverseIterationResidualToleranceBound matrixScale eigenvalue (symmetricTridiagonalDimension tridiagonalValue)
{-# INLINE inverseIterationResidualTolerance #-}

inverseIterationResidualToleranceBound :: Double -> Double -> Int -> Double
inverseIterationResidualToleranceBound !matrixScale !eigenvalue !dimension =
  1.0e7
    * epsDouble
    * max 1.0 (fromIntegral dimension)
    * max 1.0 (max matrixScale (abs eigenvalue))
{-# INLINE inverseIterationResidualToleranceBound #-}

eigenvalueClusterTolerance :: Double -> Double -> Double -> Double
eigenvalueClusterTolerance !matrixScale !leftValue !rightValue =
  64.0 * sqrt epsDouble * max 1.0 (maximum [matrixScale, abs leftValue, abs rightValue])
{-# INLINE eigenvalueClusterTolerance #-}

clusterOrthogonalityTolerance :: Double -> Int -> Double
clusterOrthogonalityTolerance _ !matrixSize =
  256.0 * sqrt epsDouble * max 1.0 (fromIntegral matrixSize)
{-# INLINE clusterOrthogonalityTolerance #-}

vectorNormTolerance :: Double -> Int -> Double
vectorNormTolerance !matrixScale !matrixSize =
  64.0 * safeMinimumDouble * max 1.0 matrixScale * max 1.0 (fromIntegral matrixSize)
{-# INLINE vectorNormTolerance #-}

safeTridiagonalSolvePivot :: Double -> Double -> Double
safeTridiagonalSolvePivot !matrixScale !pivotValue
  | abs pivotValue > solvePivotTolerance matrixScale = pivotValue
  | pivotValue < 0.0 = negate (solvePivotTolerance matrixScale)
  | otherwise = solvePivotTolerance matrixScale
{-# INLINE safeTridiagonalSolvePivot #-}

solvePivotTolerance :: Double -> Double
solvePivotTolerance !matrixScale =
  (128.0 * epsDouble * max 1.0 matrixScale) + safeMinimumDouble
{-# INLINE solvePivotTolerance #-}

sturmCountLessEqual :: Double -> SymmetricTridiagonal -> Double -> Int
sturmCountLessEqual !matrixScale tridiagonalValue !shiftValue =
  let diagonalEntries = symmetricTridiagonalDiagonalVector tridiagonalValue
      offDiagonalEntries = symmetricTridiagonalOffDiagonalVector tridiagonalValue
      !matrixSize = U.length diagonalEntries
      pivotAt !indexValue !previousPivot =
        let !diagonalPivot = (diagonalEntries `U.unsafeIndex` indexValue) - shiftValue
         in if indexValue == 0
              then diagonalPivot
              else
                let !offDiagonal = offDiagonalEntries `U.unsafeIndex` (indexValue - 1)
                    !safePreviousPivot = nonzeroSturmPivot matrixScale previousPivot
                 in diagonalPivot - (offDiagonal * offDiagonal / safePreviousPivot)
      countAt !indexValue !previousPivot !negativeCount
        | indexValue >= matrixSize = negativeCount
        | otherwise =
            let !pivotValue = pivotAt indexValue previousPivot
                !nextCount =
                  if pivotValue <= 0.0
                    then negativeCount + 1
                    else negativeCount
             in countAt (indexValue + 1) pivotValue nextCount
   in countAt 0 1.0 0

tridiagonalResidualNorm :: SymmetricTridiagonal -> Double -> U.Vector Double -> Double
tridiagonalResidualNorm tridiagonalValue !eigenvalue eigenvector =
  normU
    ( U.generate
        (U.length eigenvector)
        (tridiagonalResidualEntry tridiagonalValue eigenvalue eigenvector)
    )

tridiagonalRayleighQuotient :: SymmetricTridiagonal -> U.Vector Double -> Double
tridiagonalRayleighQuotient tridiagonalValue eigenvector =
  vectorDot eigenvector (tridiagonalApply tridiagonalValue eigenvector)

tridiagonalApply :: SymmetricTridiagonal -> U.Vector Double -> U.Vector Double
tridiagonalApply tridiagonalValue eigenvector =
  U.generate
    (U.length eigenvector)
    (tridiagonalApplyEntry tridiagonalValue eigenvector)

tridiagonalApplyEntry :: SymmetricTridiagonal -> U.Vector Double -> Int -> Double
tridiagonalApplyEntry tridiagonalValue eigenvector !entryIndex =
  let diagonalEntries = symmetricTridiagonalDiagonalVector tridiagonalValue
      offDiagonalEntries = symmetricTridiagonalOffDiagonalVector tridiagonalValue
      !matrixSize = U.length diagonalEntries
      !centerValue = eigenvector `U.unsafeIndex` entryIndex
      !leftValue =
        if entryIndex <= 0
          then 0.0
          else (offDiagonalEntries `U.unsafeIndex` (entryIndex - 1)) * (eigenvector `U.unsafeIndex` (entryIndex - 1))
      !rightValue =
        if entryIndex + 1 >= matrixSize
          then 0.0
          else (offDiagonalEntries `U.unsafeIndex` entryIndex) * (eigenvector `U.unsafeIndex` (entryIndex + 1))
   in leftValue + (diagonalEntries `U.unsafeIndex` entryIndex) * centerValue + rightValue
{-# INLINE tridiagonalApplyEntry #-}

tridiagonalResidualEntry :: SymmetricTridiagonal -> Double -> U.Vector Double -> Int -> Double
tridiagonalResidualEntry tridiagonalValue !eigenvalue eigenvector !entryIndex =
  let diagonalEntries = symmetricTridiagonalDiagonalVector tridiagonalValue
      offDiagonalEntries = symmetricTridiagonalOffDiagonalVector tridiagonalValue
      !matrixSize = U.length diagonalEntries
      !centerValue = eigenvector `U.unsafeIndex` entryIndex
      !leftValue =
        if entryIndex <= 0
          then 0.0
          else (offDiagonalEntries `U.unsafeIndex` (entryIndex - 1)) * (eigenvector `U.unsafeIndex` (entryIndex - 1))
      !rightValue =
        if entryIndex + 1 >= matrixSize
          then 0.0
          else (offDiagonalEntries `U.unsafeIndex` entryIndex) * (eigenvector `U.unsafeIndex` (entryIndex + 1))
      !imageValue = leftValue + (diagonalEntries `U.unsafeIndex` entryIndex) * centerValue + rightValue
   in imageValue - eigenvalue * centerValue
{-# INLINE tridiagonalResidualEntry #-}

gershgorinBounds :: SymmetricTridiagonal -> (Double, Double)
gershgorinBounds tridiagonalValue =
  let diagonalEntries = symmetricTridiagonalDiagonalVector tridiagonalValue
      offDiagonalEntries = symmetricTridiagonalOffDiagonalVector tridiagonalValue
      !matrixSize = U.length diagonalEntries
      rowLowerBound !rowIndex =
        let !radius = offDiagonalRadius offDiagonalEntries matrixSize rowIndex
         in (diagonalEntries `U.unsafeIndex` rowIndex) - radius
      rowUpperBound !rowIndex =
        let !radius = offDiagonalRadius offDiagonalEntries matrixSize rowIndex
         in (diagonalEntries `U.unsafeIndex` rowIndex) + radius
      lowerBound = U.minimum (U.generate matrixSize rowLowerBound)
      upperBound = U.maximum (U.generate matrixSize rowUpperBound)
      margin = 16.0 * eigenTolerance (tridiagonalInfinityNormBound tridiagonalValue) lowerBound upperBound
   in (lowerBound - margin, upperBound + margin)

tridiagonalInfinityNormBound :: SymmetricTridiagonal -> Double
tridiagonalInfinityNormBound tridiagonalValue =
  let diagonalEntries = symmetricTridiagonalDiagonalVector tridiagonalValue
      offDiagonalEntries = symmetricTridiagonalOffDiagonalVector tridiagonalValue
      !matrixSize = U.length diagonalEntries
   in if matrixSize <= 0
        then 0.0
        else
          U.maximum
            ( U.generate
                matrixSize
                ( \rowIndex ->
                    abs (diagonalEntries `U.unsafeIndex` rowIndex)
                      + offDiagonalRadius offDiagonalEntries matrixSize rowIndex
                )
            )

offDiagonalRadius :: U.Vector Double -> Int -> Int -> Double
offDiagonalRadius offDiagonalEntries !matrixSize !rowIndex =
  ( if rowIndex <= 0
      then 0.0
      else abs (offDiagonalEntries `U.unsafeIndex` (rowIndex - 1))
  )
    + ( if rowIndex + 1 >= matrixSize
          then 0.0
          else abs (offDiagonalEntries `U.unsafeIndex` rowIndex)
      )
{-# INLINE offDiagonalRadius #-}

sortForSpectrum :: SpectrumEnd -> [(Int, Double)] -> [(Int, Double)]
sortForSpectrum spectrumEnd =
  sortBy
    ( case spectrumEnd of
        SmallestEigenvalues -> comparing snd
        LargestEigenvalues -> flip (comparing snd)
    )

unitVector :: Int -> Int -> U.Vector Double
unitVector !matrixSize !selectedIndex =
  U.generate matrixSize (\entryIndex -> if entryIndex == selectedIndex then 1.0 else 0.0)

midpoint :: Double -> Double -> Double
midpoint !leftValue !rightValue =
  leftValue + 0.5 * (rightValue - leftValue)
{-# INLINE midpoint #-}

clamp :: Ord value => value -> value -> value -> value
clamp lowerValue upperValue value =
  max lowerValue (min upperValue value)
{-# INLINE clamp #-}

eigenTolerance :: Double -> Double -> Double -> Double
eigenTolerance !matrixScale !leftValue !rightValue =
  sqrt epsDouble * max 1.0 (maximum [matrixScale, abs leftValue, abs rightValue])
{-# INLINE eigenTolerance #-}

nonzeroSturmPivot :: Double -> Double -> Double
nonzeroSturmPivot !matrixScale !pivotValue
  | abs pivotValue > sturmPivotTolerance matrixScale = pivotValue
  | pivotValue > 0.0 = sturmPivotTolerance matrixScale
  | otherwise = negate (sturmPivotTolerance matrixScale)
{-# INLINE nonzeroSturmPivot #-}

sturmPivotTolerance :: Double -> Double
sturmPivotTolerance !matrixScale =
  (64.0 * epsDouble * max 1.0 matrixScale) + safeMinimumDouble
{-# INLINE sturmPivotTolerance #-}
