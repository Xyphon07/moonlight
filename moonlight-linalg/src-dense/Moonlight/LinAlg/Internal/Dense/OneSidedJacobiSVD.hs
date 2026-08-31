{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}

-- | One-sided Jacobi SVD over sealed flat Double workspaces.
module Moonlight.LinAlg.Internal.Dense.OneSidedJacobiSVD
  ( ThinSvdFailure (..),
    ThinSvdResult (..),
    thinSvdFullColumnRank,
  )
where

import Control.Monad.ST (ST, runST)
import Data.Bifunctor (first)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Vector.Storable qualified as S
import Data.Vector.Storable.Mutable qualified as SM
import Moonlight.Core (checkedNonNegativeProduct)
import Moonlight.LinAlg.Internal.Eigen.DenseWork
  ( MutableDenseWork (..),
    dotDenseColumns,
    newDenseWork,
    readDenseWork,
    setIdentityDenseWork,
    writeDenseWork,
  )
import Moonlight.LinAlg.Internal.Eigen.Kernels
  ( epsDouble,
    finiteDouble,
    forIndex,
    hypotStable,
  )
import Moonlight.LinAlg.Pure.Dense.Flat
  ( DenseDoubleMatrix,
    denseDoubleMatrixShape,
    denseDoubleMatrixToRowMajorVector,
    trustedDenseDoubleMatrixRowMajor,
  )
import Prelude

data ThinSvdFailure
  = ThinSvdNonFiniteInput
  | ThinSvdDimensionViolation !String
  | ThinSvdRankDeficient !Int !Double
  | ThinSvdSweepBudgetNonConvergence !Int !Double
  deriving stock (Eq, Show)

data ThinSvdResult = ThinSvdResult
  { thinSvdLeftSingularVectors :: !DenseDoubleMatrix,
    thinSvdSingularValues :: !(S.Vector Double),
    thinSvdRightSingularVectorsTransposed :: !DenseDoubleMatrix
  }
  deriving stock (Eq, Show)

thinSvdFullColumnRank :: DenseDoubleMatrix -> Either ThinSvdFailure ThinSvdResult
thinSvdFullColumnRank matrixValue
  | rowCount < columnCount =
      Left (ThinSvdDimensionViolation "thin Jacobi SVD requires row count greater than or equal to column count")
  | columnCount > 64 =
      Left (ThinSvdDimensionViolation "thin Jacobi SVD supports at most 64 columns")
  | S.any (not . finiteDouble) payload =
      Left ThinSvdNonFiniteInput
  | otherwise = do
      matrixEntryCount <- checkedThinSvdCardinality "thin Jacobi SVD matrix" rowCount columnCount
      rightEntryCount <- checkedThinSvdCardinality "thin Jacobi SVD right singular vectors" columnCount columnCount
      if S.length payload /= matrixEntryCount
        then Left (ThinSvdDimensionViolation "thin Jacobi SVD payload does not match its declared matrix shape")
        else runST (thinSvdFullColumnRankST rowCount columnCount matrixEntryCount rightEntryCount matrixValue)
  where
    !(rowCount, columnCount) = denseDoubleMatrixShape matrixValue
    payload = denseDoubleMatrixToRowMajorVector matrixValue

checkedThinSvdCardinality :: String -> Int -> Int -> Either ThinSvdFailure Int
checkedThinSvdCardinality context leftCount rightCount =
  first
    (const (ThinSvdDimensionViolation (context <> " cardinality exceeds non-negative Int range")))
    (checkedNonNegativeProduct leftCount rightCount)

thinSvdFullColumnRankST :: Int -> Int -> Int -> Int -> DenseDoubleMatrix -> ST s (Either ThinSvdFailure ThinSvdResult)
thinSvdFullColumnRankST !rowCount !columnCount !matrixEntryCount !rightEntryCount matrixValue = do
  leftColumns <- newDenseWork rowCount columnCount
  rightVectors <- newDenseWork columnCount columnCount
  setIdentityDenseWork rightVectors
  copyRowMajorToColumns rowCount columnCount matrixValue leftColumns
  sweepResult <- runJacobiSweeps rowCount columnCount leftColumns rightVectors
  case sweepResult of
    Left err -> pure (Left err)
    Right () -> do
      singularValues <- singularValuesFromColumns columnCount leftColumns
      let !maximumSingular = maximumSingularValue singularValues
          !rankTolerance = fromIntegral (max 1 rowCount) * epsDouble * max 1.0 maximumSingular
      case firstRankDeficiency rankTolerance singularValues of
        Just (columnIndex, singularValue) -> pure (Left (ThinSvdRankDeficient columnIndex singularValue))
        Nothing -> Right <$> projectThinSvdResult rowCount columnCount matrixEntryCount rightEntryCount leftColumns rightVectors singularValues

copyRowMajorToColumns :: Int -> Int -> DenseDoubleMatrix -> MutableDenseWork s -> ST s ()
copyRowMajorToColumns !rowCount !columnCount matrixValue columns =
  forIndex 0 rowCount $ \rowIndex ->
    forIndex 0 columnCount $ \columnIndex ->
      writeDenseWork columns rowIndex columnIndex (payload `S.unsafeIndex` (rowIndex * columnCount + columnIndex))
  where
    payload = denseDoubleMatrixToRowMajorVector matrixValue
{-# INLINE copyRowMajorToColumns #-}

runJacobiSweeps ::
  Int ->
  Int ->
  MutableDenseWork s ->
  MutableDenseWork s ->
  ST s (Either ThinSvdFailure ())
runJacobiSweeps !rowCount !columnCount leftColumns rightVectors = sweepAt 0
  where
    !sweepBudget = max 8 (12 * max 1 columnCount)
    !pairTolerance = 64.0 * epsDouble

    sweepAt !sweepIndex
      | columnCount <= 1 = pure (Right ())
      | sweepIndex >= sweepBudget = do
          finalCross <- maximumNormalizedCross columnCount leftColumns
          pure (Left (ThinSvdSweepBudgetNonConvergence sweepBudget finalCross))
      | otherwise = do
          summary <- sweepColumnPairs rowCount columnCount pairTolerance leftColumns rightVectors
          if sweepMaximumCross summary <= pairTolerance
            then pure (Right ())
            else sweepAt (sweepIndex + 1)

data SweepSummary = SweepSummary
  { sweepMaximumCross :: !Double
  }

sweepColumnPairs ::
  Int ->
  Int ->
  Double ->
  MutableDenseWork s ->
  MutableDenseWork s ->
  ST s SweepSummary
sweepColumnPairs !rowCount !columnCount !pairTolerance leftColumns rightVectors =
  goLeft 0 0.0
  where
    goLeft !leftColumn !maximumCross
      | leftColumn >= columnCount - 1 = pure (SweepSummary maximumCross)
      | otherwise = do
          nextMaximum <- goRight leftColumn (leftColumn + 1) maximumCross
          goLeft (leftColumn + 1) nextMaximum

    goRight !leftColumn !rightColumn !maximumCross
      | rightColumn >= columnCount = pure maximumCross
      | otherwise = do
          alpha <- dotDenseColumns leftColumns leftColumn leftColumn
          beta <- dotDenseColumns leftColumns rightColumn rightColumn
          gamma <- dotDenseColumns leftColumns leftColumn rightColumn
          let !crossValue = normalizedCross alpha beta gamma
          if crossValue > pairTolerance
            then do
              rotateJacobiColumns rowCount leftColumns rightVectors leftColumn rightColumn alpha beta gamma
              goRight leftColumn (rightColumn + 1) (max maximumCross crossValue)
            else goRight leftColumn (rightColumn + 1) (max maximumCross crossValue)

maximumNormalizedCross :: Int -> MutableDenseWork s -> ST s Double
maximumNormalizedCross !columnCount leftColumns = goLeft 0 0.0
  where
    goLeft !leftColumn !maximumCross
      | leftColumn >= columnCount - 1 = pure maximumCross
      | otherwise = do
          nextMaximum <- goRight leftColumn (leftColumn + 1) maximumCross
          goLeft (leftColumn + 1) nextMaximum

    goRight !leftColumn !rightColumn !maximumCross
      | rightColumn >= columnCount = pure maximumCross
      | otherwise = do
          alpha <- dotDenseColumns leftColumns leftColumn leftColumn
          beta <- dotDenseColumns leftColumns rightColumn rightColumn
          gamma <- dotDenseColumns leftColumns leftColumn rightColumn
          goRight leftColumn (rightColumn + 1) (max maximumCross (normalizedCross alpha beta gamma))

normalizedCross :: Double -> Double -> Double -> Double
normalizedCross !alpha !beta !gamma =
  let !denominator = sqrt (max 0.0 alpha * max 0.0 beta)
   in if denominator <= 0.0
        then 0.0
        else abs gamma / denominator
{-# INLINE normalizedCross #-}

rotateJacobiColumns ::
  Int ->
  MutableDenseWork s ->
  MutableDenseWork s ->
  Int ->
  Int ->
  Double ->
  Double ->
  Double ->
  ST s ()
rotateJacobiColumns !rowCount leftColumns rightVectors !leftColumn !rightColumn !alpha !beta !gamma = do
  let !(cosineValue, sineValue) = jacobiRotation alpha beta gamma
  rotateColumnPair rowCount leftColumns leftColumn rightColumn cosineValue sineValue
  rotateColumnPair columnCount rightVectors leftColumn rightColumn cosineValue sineValue
  where
    MutableDenseWork columnCount _ _ = rightVectors

jacobiRotation :: Double -> Double -> Double -> (Double, Double)
jacobiRotation !alpha !beta !gamma =
  let !tauValue = (beta - alpha) / (2.0 * gamma)
      !tangentValue =
        if tauValue < 0.0
          then (-1.0) / ((-tauValue) + hypotStable tauValue 1.0)
          else 1.0 / (tauValue + hypotStable tauValue 1.0)
      !cosineValue = 1.0 / hypotStable 1.0 tangentValue
      !sineValue = tangentValue * cosineValue
   in (cosineValue, sineValue)
{-# INLINE jacobiRotation #-}

rotateColumnPair :: Int -> MutableDenseWork s -> Int -> Int -> Double -> Double -> ST s ()
rotateColumnPair !rowCount work !leftColumn !rightColumn !cosineValue !sineValue =
  forIndex 0 rowCount $ \rowIndex -> do
    leftValue <- readDenseWork work rowIndex leftColumn
    rightValue <- readDenseWork work rowIndex rightColumn
    writeDenseWork work rowIndex leftColumn (cosineValue * leftValue - sineValue * rightValue)
    writeDenseWork work rowIndex rightColumn (sineValue * leftValue + cosineValue * rightValue)
{-# INLINE rotateColumnPair #-}

singularValuesFromColumns :: Int -> MutableDenseWork s -> ST s (S.Vector Double)
singularValuesFromColumns !columnCount leftColumns = do
  singularValueBuffer <- SM.new columnCount
  forIndex 0 columnCount $ \columnIndex -> do
    normSquared <- dotDenseColumns leftColumns columnIndex columnIndex
    SM.write singularValueBuffer columnIndex (sqrt (max 0.0 normSquared))
  S.unsafeFreeze singularValueBuffer

maximumSingularValue :: S.Vector Double -> Double
maximumSingularValue singularValues =
  S.foldl' max 0.0 singularValues

firstRankDeficiency :: Double -> S.Vector Double -> Maybe (Int, Double)
firstRankDeficiency !rankTolerance singularValues = go 0
  where
    go !columnIndex
      | columnIndex >= S.length singularValues = Nothing
      | otherwise =
          let !singularValue = singularValues `S.unsafeIndex` columnIndex
           in if singularValue <= rankTolerance
                then Just (columnIndex, singularValue)
                else go (columnIndex + 1)

projectThinSvdResult ::
  Int ->
  Int ->
  Int ->
  Int ->
  MutableDenseWork s ->
  MutableDenseWork s ->
  S.Vector Double ->
  ST s ThinSvdResult
projectThinSvdResult !rowCount !columnCount !matrixEntryCount !rightEntryCount leftColumns rightVectors singularValues = do
  uBuffer <- SM.new matrixEntryCount
  sigmaBuffer <- SM.new columnCount
  vtBuffer <- SM.new rightEntryCount
  let orderedColumns =
        fmap fst
          . sortBy (flip (comparing snd))
          $ [(columnIndex, singularValues `S.unsafeIndex` columnIndex) | columnIndex <- [0 .. columnCount - 1]]
  writeOrderedColumns orderedColumns 0 uBuffer sigmaBuffer vtBuffer
  uValues <- S.unsafeFreeze uBuffer
  sigmaValues <- S.unsafeFreeze sigmaBuffer
  vtValues <- S.unsafeFreeze vtBuffer
  pure
    ThinSvdResult
      { thinSvdLeftSingularVectors = trustedDenseDoubleMatrixRowMajor rowCount columnCount uValues,
        thinSvdSingularValues = sigmaValues,
        thinSvdRightSingularVectorsTransposed = trustedDenseDoubleMatrixRowMajor columnCount columnCount vtValues
      }
  where
    writeOrderedColumns orderedColumns !targetColumn uBuffer sigmaBuffer vtBuffer =
      case orderedColumns of
        [] -> pure ()
        sourceColumn : remainingColumns -> do
          let !singularValue = singularValues `S.unsafeIndex` sourceColumn
              !inverseSingular = 1.0 / singularValue
          SM.write sigmaBuffer targetColumn singularValue
          forIndex 0 rowCount $ \rowIndex -> do
            leftEntry <- readDenseWork leftColumns rowIndex sourceColumn
            SM.write uBuffer (rowIndex * columnCount + targetColumn) (leftEntry * inverseSingular)
          forIndex 0 columnCount $ \columnIndex -> do
            rightEntry <- readDenseWork rightVectors columnIndex sourceColumn
            SM.write vtBuffer (targetColumn * columnCount + columnIndex) rightEntry
          writeOrderedColumns remainingColumns (targetColumn + 1) uBuffer sigmaBuffer vtBuffer
