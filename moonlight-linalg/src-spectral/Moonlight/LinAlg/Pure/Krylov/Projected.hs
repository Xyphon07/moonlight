{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}

module Moonlight.LinAlg.Pure.Krylov.Projected
  ( SpectrumEnd (..),
    SymmetricProjectedOperator (..),
    symmetricProjectedOperatorDimension,
    applySymmetricProjectedOperatorU,
    ProjectedSubspace,
    projectedSubspaceDimension,
    mkStructuredProjectedSubspace,
    projectedSubspaceBasisColumns,
    projectedSubspaceOperator,
    projectedSubspaceFromLanczos,
    projectedSubspaceFromBlockLanczos,
    projectedEigenvalues,
    projectedEigenpairs,
    projectedEigenvaluesFromRestartedLanczos,
    projectedEigenpairsFromRestartedLanczos,
  )
where

import Data.Kind (Type)
import qualified Data.Vector as Box
import qualified Data.Vector.Unboxed as U
import Moonlight.Core (MoonlightError (..))
import Moonlight.LinAlg.Internal.Eigen.Kernels (epsDouble, finiteDouble)
import Moonlight.LinAlg.Internal.VectorOps (normU, scaleU, subScaledU)
import Moonlight.LinAlg.Pure.Krylov.Config (LanczosConfig, lanczosTolerance)
import Moonlight.LinAlg.Pure.Krylov.Decomposition
import Moonlight.LinAlg.Pure.Krylov.Lanczos
  ( lanczosRestartProjectionBasisColumns,
    lanczosRestartProjectionProjectedPairs,
    lanczosRestartedProjection,
    lanczosSymmetric,
    ritzLockThreshold,
  )
import Moonlight.LinAlg.Pure.Krylov.Internal (linearCombinationColumnsU)
import Moonlight.LinAlg.Pure.Spectral.Result
  ( Eigenpairs,
    eigenpairCount,
    eigenpairResidualNorms,
    eigenpairValues,
    eigenpairVectorAt,
    mkEigenpairs,
  )
import Moonlight.LinAlg.Pure.Operator
  ( LinearOperator,
    OperatorSymmetry (SelfAdjointOperator),
    operatorShape,
    runOperatorU,
  )
import Moonlight.LinAlg.Pure.Krylov.Selection (SpectrumEnd (..))
import Moonlight.LinAlg.Pure.Krylov.SelectedTridiagonal
  ( selectedSymmetricTridiagonalEigenpairsDirect,
    selectedSymmetricTridiagonalEigenvaluesDirect,
  )
import Moonlight.LinAlg.Pure.Structured.BlockTridiagonal
  ( SymmetricBlockTridiagonal,
    applySymmetricBlockTridiagonalU,
    symmetricBlockTridiagonalDimension,
  )
import Moonlight.LinAlg.Pure.Structured.Tridiagonal
  ( SymmetricTridiagonal,
    applySymmetricTridiagonalU,
    symmetricTridiagonalDimension,
  )
import Prelude

type SymmetricProjectedOperator :: Type
data SymmetricProjectedOperator
  = TridiagonalProjectedOperator !SymmetricTridiagonal
  | BlockTridiagonalProjectedOperator !SymmetricBlockTridiagonal
  deriving stock (Eq, Show)

symmetricProjectedOperatorDimension :: SymmetricProjectedOperator -> Int
symmetricProjectedOperatorDimension projectedOperator =
  case projectedOperator of
    TridiagonalProjectedOperator tridiagonalValue -> symmetricTridiagonalDimension tridiagonalValue
    BlockTridiagonalProjectedOperator blockTridiagonalValue -> symmetricBlockTridiagonalDimension blockTridiagonalValue

applySymmetricProjectedOperatorU ::
  SymmetricProjectedOperator ->
  U.Vector Double ->
  Either MoonlightError (U.Vector Double)
applySymmetricProjectedOperatorU projectedOperator inputVector =
  let dimension = symmetricProjectedOperatorDimension projectedOperator
   in if U.length inputVector /= dimension
        then
          Left
            ( InvariantViolation
                ( "Projected operator input dimension mismatch: expected "
                    <> show dimension
                    <> " but received "
                    <> show (U.length inputVector)
                )
            )
        else
          case projectedOperator of
            TridiagonalProjectedOperator tridiagonalValue ->
              Right (applySymmetricTridiagonalU tridiagonalValue inputVector)
            BlockTridiagonalProjectedOperator blockTridiagonalValue ->
              applySymmetricBlockTridiagonalU blockTridiagonalValue inputVector

type ProjectedSubspace :: Type
data ProjectedSubspace = ProjectedSubspace
  { subspaceBasisColumns :: !(Box.Vector (U.Vector Double)),
    subspaceOperatorValue :: !SymmetricProjectedOperator
  }
  deriving stock (Eq, Show)

projectedSubspaceDimension :: ProjectedSubspace -> Int
projectedSubspaceDimension = symmetricProjectedOperatorDimension . projectedSubspaceOperator

mkStructuredProjectedSubspace ::
  Box.Vector (U.Vector Double) ->
  SymmetricProjectedOperator ->
  Either MoonlightError ProjectedSubspace
mkStructuredProjectedSubspace basisColumns projectedOperator =
  let basisCount = symmetricProjectedOperatorDimension projectedOperator
      basisDimensions = U.length <$> Box.toList basisColumns
      basisDimension =
        case basisDimensions of
          [] -> 0
          firstDimension : _ -> firstDimension
   in if Box.length basisColumns /= basisCount
        then Left (InvariantViolation "Projected subspace basis column count must match the projected dimension witness")
        else
          if any (/= basisDimension) basisDimensions
            then Left (InvariantViolation "Projected subspace basis columns must have equal length")
            else Right (ProjectedSubspace basisColumns projectedOperator)

projectedSubspaceBasisColumns :: ProjectedSubspace -> Box.Vector (U.Vector Double)
projectedSubspaceBasisColumns = subspaceBasisColumns

projectedSubspaceOperator :: ProjectedSubspace -> SymmetricProjectedOperator
projectedSubspaceOperator = subspaceOperatorValue

projectedSubspaceFromLanczos :: LanczosDecomposition -> ProjectedSubspace
projectedSubspaceFromLanczos decomposition =
  let basisColumns = lanczosBasisColumns decomposition
      projectedTridiagonal = lanczosProjectedTridiagonal decomposition
   in ProjectedSubspace
        { subspaceBasisColumns = basisColumns,
          subspaceOperatorValue = TridiagonalProjectedOperator projectedTridiagonal
        }

projectedSubspaceFromBlockLanczos :: BlockLanczosDecomposition -> ProjectedSubspace
projectedSubspaceFromBlockLanczos decomposition =
  let basisColumns = blockLanczosBasisColumns decomposition
      projectedBlockTridiagonal = blockLanczosProjectedBlockTridiagonal decomposition
   in ProjectedSubspace
        { subspaceBasisColumns = basisColumns,
          subspaceOperatorValue = BlockTridiagonalProjectedOperator projectedBlockTridiagonal
        }

projectedEigenvalues ::
  SpectrumEnd ->
  Int ->
  LinearOperator 'SelfAdjointOperator ->
  ProjectedSubspace ->
  Either MoonlightError (U.Vector Double)
projectedEigenvalues spectrumEnd requestedCount op subspace
  | requestedCount <= 0 =
      Left (InvariantViolation "Projected eigenvalue count must be positive")
  | otherwise = do
      basisCount <- validateProjectedSubspace op subspace
      validateProjectedRequestedCount "Projected eigenvalue" requestedCount basisCount
      symmetricProjectedEigenvalues spectrumEnd requestedCount (projectedSubspaceOperator subspace)

projectedEigenpairs ::
  SpectrumEnd ->
  Int ->
  LinearOperator 'SelfAdjointOperator ->
  ProjectedSubspace ->
  Either MoonlightError Eigenpairs
projectedEigenpairs spectrumEnd requestedCount op subspace
  | requestedCount <= 0 =
      Left (InvariantViolation "Projected eigenpair count must be positive")
  | otherwise = do
      basisCount <- validateProjectedSubspace op subspace
      validateProjectedRequestedCount "Projected eigenpair" requestedCount basisCount
      projectedPairs <- symmetricProjectedEigenpairs spectrumEnd requestedCount (projectedSubspaceOperator subspace)
      liftProjectedEigenpairs op (projectedSubspaceBasisColumns subspace) projectedPairs

projectedEigenvaluesFromRestartedLanczos ::
  LanczosConfig ->
  SpectrumEnd ->
  Int ->
  LinearOperator 'SelfAdjointOperator ->
  U.Vector Double ->
  Either MoonlightError (U.Vector Double)
projectedEigenvaluesFromRestartedLanczos config spectrumEnd requestedCount op seedVector =
  case singleCycleCertifiedEigenpairs config spectrumEnd requestedCount op seedVector of
    Just certifiedPairs -> Right (eigenpairValues certifiedPairs)
    Nothing -> do
      restartProjection <- lanczosRestartedProjection config spectrumEnd requestedCount op seedVector
      pure (eigenpairValues (lanczosRestartProjectionProjectedPairs restartProjection))

projectedEigenpairsFromRestartedLanczos ::
  LanczosConfig ->
  SpectrumEnd ->
  Int ->
  LinearOperator 'SelfAdjointOperator ->
  U.Vector Double ->
  Either MoonlightError Eigenpairs
projectedEigenpairsFromRestartedLanczos config spectrumEnd requestedCount op seedVector =
  case singleCycleCertifiedEigenpairs config spectrumEnd requestedCount op seedVector of
    Just certifiedPairs -> Right certifiedPairs
    Nothing -> do
      restartProjection <- lanczosRestartedProjection config spectrumEnd requestedCount op seedVector
      liftProjectedEigenpairs
        op
        (lanczosRestartProjectionBasisColumns restartProjection)
        (lanczosRestartProjectionProjectedPairs restartProjection)

singleCycleCertifiedEigenpairs ::
  LanczosConfig ->
  SpectrumEnd ->
  Int ->
  LinearOperator 'SelfAdjointOperator ->
  U.Vector Double ->
  Maybe Eigenpairs
singleCycleCertifiedEigenpairs config spectrumEnd requestedCount op seedVector =
  case lanczosSymmetric config op seedVector of
    Left _ -> Nothing
    Right decomposition ->
      let subspace = projectedSubspaceFromLanczos decomposition
          basisCount = Box.length (projectedSubspaceBasisColumns subspace)
       in if requestedCount <= 0 || requestedCount > basisCount
            then Nothing
            else case projectedEigenpairs spectrumEnd requestedCount op subspace of
              Left _ -> Nothing
              Right liftedPairs ->
                let (_, ambientDimension) = operatorShape op
                    tolerance = lanczosTolerance config
                    pairCertified eigenvalue residualNorm =
                      residualNorm <= ritzLockThreshold tolerance ambientDimension eigenvalue
                    allCertified =
                      U.and
                        ( U.zipWith
                            pairCertified
                            (eigenpairValues liftedPairs)
                            (eigenpairResidualNorms liftedPairs)
                        )
                 in if allCertified then Just liftedPairs else Nothing

symmetricProjectedEigenvalues ::
  SpectrumEnd ->
  Int ->
  SymmetricProjectedOperator ->
  Either MoonlightError (U.Vector Double)
symmetricProjectedEigenvalues spectrumEnd requestedCount projectedOperator = do
  let operatorDimension = symmetricProjectedOperatorDimension projectedOperator
  if requestedCount <= 0
    then Left (InvariantViolation "Projected eigensolve requires a positive requested count")
    else if requestedCount > operatorDimension
      then Left (InvariantViolation "Projected eigensolve requested count exceeds projected dimension")
    else
      case projectedOperator of
        TridiagonalProjectedOperator tridiagonalValue ->
          selectedSymmetricTridiagonalEigenvaluesDirect spectrumEnd requestedCount tridiagonalValue
        BlockTridiagonalProjectedOperator _ -> Left blockProjectedSpectralObstruction

symmetricProjectedEigenpairs ::
  SpectrumEnd ->
  Int ->
  SymmetricProjectedOperator ->
  Either MoonlightError Eigenpairs
symmetricProjectedEigenpairs spectrumEnd requestedCount projectedOperator = do
  let operatorDimension = symmetricProjectedOperatorDimension projectedOperator
  if requestedCount <= 0
    then Left (InvariantViolation "Projected eigensolve requires a positive requested count")
    else if requestedCount > operatorDimension
      then Left (InvariantViolation "Projected eigensolve requested count exceeds projected dimension")
    else
      case projectedOperator of
        TridiagonalProjectedOperator tridiagonalValue ->
          selectedSymmetricTridiagonalEigenpairsDirect spectrumEnd requestedCount tridiagonalValue
        BlockTridiagonalProjectedOperator _ -> Left blockProjectedSpectralObstruction

blockProjectedSpectralObstruction :: MoonlightError
blockProjectedSpectralObstruction =
  InvariantViolation "pure block-projected eigensolve has no exact block backend; use the native symmetric-band EigenRequest executor"

validateProjectedSubspace :: LinearOperator 'SelfAdjointOperator -> ProjectedSubspace -> Either MoonlightError Int
validateProjectedSubspace op subspace =
  let basisColumns = projectedSubspaceBasisColumns subspace
      basisDimension =
        case Box.toList basisColumns of
          [] -> 0
          firstBasisColumn : _ -> U.length firstBasisColumn
      basisCount = projectedSubspaceDimension subspace
      (rowCount, columnCount) = operatorShape op
   in if rowCount /= columnCount || basisDimension /= columnCount
        then
          Left
            ( InvariantViolation
                ( "Projected eigensolve basis dimension mismatch: operator "
                    <> show (rowCount, columnCount)
                    <> " basis vectors of length "
                    <> show basisDimension
                )
            )
        else Right basisCount

validateProjectedRequestedCount :: String -> Int -> Int -> Either MoonlightError ()
validateProjectedRequestedCount context requestedCount projectedDimension =
  if requestedCount > projectedDimension
    then
      Left
        ( InvariantViolation
            ( context
                <> " count exceeds projected dimension: requested "
                <> show requestedCount
                <> " from "
                <> show projectedDimension
            )
        )
    else Right ()

liftProjectedEigenpairs ::
  LinearOperator 'SelfAdjointOperator ->
  Box.Vector (U.Vector Double) ->
  Eigenpairs ->
  Either MoonlightError Eigenpairs
liftProjectedEigenpairs op basisColumns projectedPairs = do
  let ambientDimension = snd (operatorShape op)
      projectedValues = eigenpairValues projectedPairs
      projectedCount = eigenpairCount projectedPairs
  liftedColumns <- Box.generateM projectedCount (liftProjectedColumn op basisColumns projectedPairs)
  liftedVectors <- flattenLiftedColumns ambientDimension projectedCount liftedColumns
  liftedResiduals <- projectedResidualVector projectedCount liftedColumns
  mkEigenpairs ambientDimension projectedValues liftedVectors liftedResiduals

liftProjectedColumn ::
  LinearOperator 'SelfAdjointOperator ->
  Box.Vector (U.Vector Double) ->
  Eigenpairs ->
  Int ->
  Either MoonlightError (U.Vector Double, Double)
liftProjectedColumn op basisColumns projectedPairs columnIndex = do
  eigenvalue <-
    case eigenpairValues projectedPairs U.!? columnIndex of
      Nothing -> Left (InvariantViolation "projected eigenpair value index out of bounds")
      Just value -> Right value
  projectedResidualNorm <-
    case eigenpairResidualNorms projectedPairs U.!? columnIndex of
      Nothing -> Left (InvariantViolation "projected eigenpair residual index out of bounds")
      Just value -> Right value
  projectedVector <- eigenpairVectorAt columnIndex projectedPairs
  liftProjectedMode op basisColumns eigenvalue projectedResidualNorm projectedVector

liftProjectedMode ::
  LinearOperator 'SelfAdjointOperator ->
  Box.Vector (U.Vector Double) ->
  Double ->
  Double ->
  U.Vector Double ->
  Either MoonlightError (U.Vector Double, Double)
liftProjectedMode op basisColumns eigenvalue projectedResidualNorm projectedVector
  | not (finiteDouble eigenvalue) =
      Left (InvariantViolation "projected eigensolve produced a non-finite projected eigenvalue")
  | not (finiteDouble projectedResidualNorm) =
      Left (InvariantViolation "projected eigensolve produced a non-finite projected residual")
  | otherwise = do
      liftedVector <- linearCombinationColumnsU basisColumns projectedVector
      let liftedNorm = normU liftedVector
          breakdownThreshold = eigenvectorBreakdownThreshold (U.length liftedVector)
      if not (finiteDouble liftedNorm)
        then Left (InvariantViolation "projected eigensolve produced a non-finite lifted projected eigenvector norm")
        else
          if liftedNorm <= breakdownThreshold
            then
              Left
                ( InvariantViolation
                    ( "projected eigensolve produced a numerically zero lifted projected eigenvector; norm="
                        <> show liftedNorm
                        <> ", threshold="
                        <> show breakdownThreshold
                    )
                )
            else do
              let normalizedVector = scaleU (1.0 / liftedNorm) liftedVector
              imageVector <- runOperatorU op normalizedVector
              residualVector <- subScaledU imageVector eigenvalue normalizedVector
              let residualNorm = max projectedResidualNorm (normU residualVector)
              if finiteDouble residualNorm
                then
                  pure (normalizedVector, residualNorm)
                else Left (InvariantViolation "projected eigensolve produced a non-finite projected eigen residual")

flattenLiftedColumns :: Int -> Int -> Box.Vector (U.Vector Double, Double) -> Either MoonlightError (U.Vector Double)
flattenLiftedColumns ambientDimension projectedCount liftedColumns =
  U.generateM
    (ambientDimension * projectedCount)
    ( \offset ->
        let (columnIndex, rowIndex) = offset `quotRem` ambientDimension
         in case liftedColumns Box.!? columnIndex of
              Nothing -> Left (InvariantViolation "lifted projected eigenpair column index out of bounds")
              Just (liftedVector, _) ->
                case liftedVector U.!? rowIndex of
                  Nothing -> Left (InvariantViolation "lifted projected eigenpair row index out of bounds")
                  Just entryValue -> Right entryValue
    )

projectedResidualVector :: Int -> Box.Vector (U.Vector Double, Double) -> Either MoonlightError (U.Vector Double)
projectedResidualVector projectedCount liftedColumns =
  U.generateM
    projectedCount
    ( \columnIndex ->
        case liftedColumns Box.!? columnIndex of
          Nothing -> Left (InvariantViolation "lifted projected eigenpair residual index out of bounds")
          Just (_, residualNorm) -> Right residualNorm
    )

eigenvectorBreakdownThreshold :: Int -> Double
eigenvectorBreakdownThreshold ambientDimension =
  128.0 * epsDouble * sqrt (fromIntegral (max 1 ambientDimension) :: Double)
