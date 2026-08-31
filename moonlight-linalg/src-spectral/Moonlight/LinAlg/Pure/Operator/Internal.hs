{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StrictData #-}

module Moonlight.LinAlg.Pure.Operator.Internal
  ( OperatorSymmetry (..),
    LinearOperator (..),
    OperatorSource (..),
    ApplyU,
    operatorShape,
    operatorDimension,
    mkVectorLinearOperator,
    declaredSelfAdjointVectorLinearOperator,
    runOperatorU,
    csrLinearOperator,
    selfAdjointCSRLinearOperator,
    graphLaplacianLinearOperator,
    diagonalLinearOperator,
    pathLaplacianLinearOperator,
    symmetricTridiagonalLinearOperator,
    packedSparseLinearOperator,
    scaleLinearOperator,
    addScaledIdentity,
    sigmaIdentityMinus,
    applyOperatorSource,
    operatorSourceShape,
  )
where

import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Vector.Unboxed qualified as U
import Moonlight.Core (MoonlightError (..), fieldValueValid)
import Moonlight.LinAlg.Internal.VectorOps (csrMatVecValidatedU)
import Moonlight.LinAlg.Pure.Sparse.Packed
  ( PackedSparseApplyError (..),
    PackedSparseOperator,
    applyPackedSparseOperatorDense,
    packedSparseOperatorSourceCardinality,
    packedSparseOperatorTargetCardinality,
  )
import Moonlight.LinAlg.Pure.Sparse.Structured
  ( GraphEdge,
    graphLaplacianCSR,
    symmetricTridiagonalFromCSR,
  )
import Moonlight.LinAlg.Pure.Sparse.Types
  ( SparseCSR,
    cooEntries,
    csrCols,
    csrColumnIndicesVector,
    csrRows,
    csrRowOffsetsVector,
    csrToCOO,
    csrValuesVector,
  )
import Moonlight.LinAlg.Pure.Structured.Tridiagonal
  ( SymmetricTridiagonal,
    applyPathLaplacianValidatedU,
    applySymmetricTridiagonalValidatedU,
    isPathLaplacianTridiagonal,
    symmetricTridiagonalDimension,
  )
import Prelude

data OperatorSymmetry
  = GeneralOperator
  | SelfAdjointOperator

type ApplyU :: Type
type ApplyU = U.Vector Double -> Either MoonlightError (U.Vector Double)

type OperatorSource :: OperatorSymmetry -> Type
data OperatorSource symmetry where
  OpaqueGeneralSource ::
    !Int ->
    !Int ->
    !ApplyU ->
    OperatorSource 'GeneralOperator
  DeclaredSelfAdjointSource ::
    !Int ->
    !ApplyU ->
    OperatorSource 'SelfAdjointOperator
  CSRSource ::
    !(SparseCSR Double) ->
    OperatorSource 'GeneralOperator
  SelfAdjointCSRSource ::
    !(SparseCSR Double) ->
    OperatorSource 'SelfAdjointOperator
  GraphLaplacianCSRSource ::
    !(SparseCSR Double) ->
    OperatorSource 'SelfAdjointOperator
  DiagonalSource ::
    !(U.Vector Double) ->
    OperatorSource 'SelfAdjointOperator
  PathLaplacianSource ::
    !Int ->
    OperatorSource 'SelfAdjointOperator
  SymmetricTridiagonalSource ::
    !SymmetricTridiagonal ->
    OperatorSource 'SelfAdjointOperator
  PackedSparseSource ::
    !(PackedSparseOperator Double) ->
    OperatorSource 'GeneralOperator

type LinearOperator :: OperatorSymmetry -> Type
data LinearOperator symmetry = LinearOperator
  { operatorSourceScale :: !Double,
    operatorIdentityShift :: !Double,
    operatorSource :: !(OperatorSource symmetry)
  }

operatorShape :: LinearOperator symmetry -> (Int, Int)
operatorShape = operatorSourceShape . operatorSource

operatorDimension :: LinearOperator 'SelfAdjointOperator -> Int
operatorDimension operatorValue =
  case operatorShape operatorValue of
    (rowCount, _) -> rowCount

operatorSourceShape :: OperatorSource symmetry -> (Int, Int)
operatorSourceShape sourceValue =
  case sourceValue of
    OpaqueGeneralSource rowCount columnCount _ -> (rowCount, columnCount)
    DeclaredSelfAdjointSource dimension _ -> (dimension, dimension)
    CSRSource csrValue -> (csrRows csrValue, csrCols csrValue)
    SelfAdjointCSRSource csrValue -> (csrRows csrValue, csrCols csrValue)
    GraphLaplacianCSRSource csrValue -> (csrRows csrValue, csrCols csrValue)
    DiagonalSource diagonalEntries -> (U.length diagonalEntries, U.length diagonalEntries)
    PathLaplacianSource dimension -> (dimension, dimension)
    SymmetricTridiagonalSource tridiagonalValue ->
      let dimension = symmetricTridiagonalDimension tridiagonalValue
       in (dimension, dimension)
    PackedSparseSource packedOperator ->
      ( packedSparseOperatorTargetCardinality packedOperator,
        packedSparseOperatorSourceCardinality packedOperator
      )

mkVectorLinearOperator :: Int -> Int -> ApplyU -> Either MoonlightError (LinearOperator 'GeneralOperator)
mkVectorLinearOperator rowCount columnCount applyVector =
  validateRectangularDimensions "linear operator" rowCount columnCount
    *> pure
      LinearOperator
        { operatorSourceScale = 1.0,
          operatorIdentityShift = 0.0,
          operatorSource = OpaqueGeneralSource rowCount columnCount (checkedApply rowCount columnCount applyVector)
        }

declaredSelfAdjointVectorLinearOperator :: Int -> ApplyU -> Either MoonlightError (LinearOperator 'SelfAdjointOperator)
declaredSelfAdjointVectorLinearOperator dimension applyVector =
  validateDimension "declared self-adjoint linear operator" dimension
    *> pure
      LinearOperator
        { operatorSourceScale = 1.0,
          operatorIdentityShift = 0.0,
          operatorSource = DeclaredSelfAdjointSource dimension (checkedApply dimension dimension applyVector)
        }

csrLinearOperator :: SparseCSR Double -> LinearOperator 'GeneralOperator
csrLinearOperator csrValue =
  LinearOperator
    { operatorSourceScale = 1.0,
      operatorIdentityShift = 0.0,
      operatorSource = CSRSource csrValue
    }

selfAdjointCSRLinearOperator :: SparseCSR Double -> Either MoonlightError (LinearOperator 'SelfAdjointOperator)
selfAdjointCSRLinearOperator csrValue = do
  validateSelfAdjointCSR csrValue
  sourceValue <-
    classifyStructuredSelfAdjointCSR
      SelfAdjointCSRSource
      csrValue
  pure
    LinearOperator
      { operatorSourceScale = 1.0,
        operatorIdentityShift = 0.0,
        operatorSource = sourceValue
      }

graphLaplacianLinearOperator ::
  (Ord vertex, Show vertex) =>
  [vertex] ->
  [GraphEdge vertex] ->
  Either MoonlightError (LinearOperator 'SelfAdjointOperator)
graphLaplacianLinearOperator vertexOrder graphEdges = do
  csrValue <- graphLaplacianCSR vertexOrder graphEdges
  sourceValue <-
    classifyStructuredSelfAdjointCSR
      GraphLaplacianCSRSource
      csrValue
  pure
    LinearOperator
      { operatorSourceScale = 1.0,
        operatorIdentityShift = 0.0,
        operatorSource = sourceValue
      }

classifyStructuredSelfAdjointCSR ::
  (SparseCSR Double -> OperatorSource 'SelfAdjointOperator) ->
  SparseCSR Double ->
  Either MoonlightError (OperatorSource 'SelfAdjointOperator)
classifyStructuredSelfAdjointCSR fallbackSource csrValue = do
  classifiedStructure <- symmetricTridiagonalFromCSR csrValue
  pure
    ( case classifiedStructure of
        Right tridiagonalValue
          | isPathLaplacianTridiagonal tridiagonalValue ->
              PathLaplacianSource
                (symmetricTridiagonalDimension tridiagonalValue)
          | otherwise ->
              SymmetricTridiagonalSource tridiagonalValue
        Left _ -> fallbackSource csrValue
    )

diagonalLinearOperator :: U.Vector Double -> Either MoonlightError (LinearOperator 'SelfAdjointOperator)
diagonalLinearOperator diagonalEntries =
  if U.any (not . fieldValueValid) diagonalEntries
    then Left (InvariantViolation "diagonal linear operator requires finite entries")
    else
      pure
        LinearOperator
          { operatorSourceScale = 1.0,
            operatorIdentityShift = 0.0,
            operatorSource = DiagonalSource diagonalEntries
          }

pathLaplacianLinearOperator :: Int -> Either MoonlightError (LinearOperator 'SelfAdjointOperator)
pathLaplacianLinearOperator dimension =
  validateDimension "path Laplacian linear operator" dimension
    *> pure
      LinearOperator
        { operatorSourceScale = 1.0,
          operatorIdentityShift = 0.0,
          operatorSource = PathLaplacianSource dimension
        }

symmetricTridiagonalLinearOperator :: SymmetricTridiagonal -> LinearOperator 'SelfAdjointOperator
symmetricTridiagonalLinearOperator tridiagonalValue =
  LinearOperator
    { operatorSourceScale = 1.0,
      operatorIdentityShift = 0.0,
      operatorSource = SymmetricTridiagonalSource tridiagonalValue
    }

packedSparseLinearOperator :: PackedSparseOperator Double -> LinearOperator 'GeneralOperator
packedSparseLinearOperator packedOperator =
  LinearOperator
    { operatorSourceScale = 1.0,
      operatorIdentityShift = 0.0,
      operatorSource = PackedSparseSource packedOperator
    }

scaleLinearOperator :: Double -> LinearOperator symmetry -> LinearOperator symmetry
scaleLinearOperator scaleValue operatorValue =
  operatorValue {operatorSourceScale = scaleValue * operatorSourceScale operatorValue, operatorIdentityShift = scaleValue * operatorIdentityShift operatorValue}

addScaledIdentity :: Double -> LinearOperator 'SelfAdjointOperator -> LinearOperator 'SelfAdjointOperator
addScaledIdentity shiftValue operatorValue =
  operatorValue {operatorIdentityShift = shiftValue + operatorIdentityShift operatorValue}

sigmaIdentityMinus :: Double -> LinearOperator 'SelfAdjointOperator -> LinearOperator 'SelfAdjointOperator
sigmaIdentityMinus sigma operatorValue =
  operatorValue
    { operatorSourceScale = negate (operatorSourceScale operatorValue),
      operatorIdentityShift = sigma - operatorIdentityShift operatorValue
    }

runOperatorU :: LinearOperator symmetry -> U.Vector Double -> Either MoonlightError (U.Vector Double)
runOperatorU operatorValue inputVector = do
  sourceImage <- applyOperatorSource (operatorSource operatorValue) inputVector
  applyAffineImage (operatorSourceScale operatorValue) (operatorIdentityShift operatorValue) sourceImage inputVector

applyOperatorSource :: OperatorSource symmetry -> U.Vector Double -> Either MoonlightError (U.Vector Double)
applyOperatorSource sourceValue inputVector =
  case sourceValue of
    OpaqueGeneralSource rowCount columnCount applyVector -> checkedApply rowCount columnCount applyVector inputVector
    DeclaredSelfAdjointSource dimension applyVector -> checkedApply dimension dimension applyVector inputVector
    CSRSource csrValue -> applyCSR csrValue inputVector
    SelfAdjointCSRSource csrValue -> applyCSR csrValue inputVector
    GraphLaplacianCSRSource csrValue -> applyCSR csrValue inputVector
    DiagonalSource diagonalEntries -> applyDiagonal diagonalEntries inputVector
    PathLaplacianSource dimension ->
      if U.length inputVector == dimension
        then Right (applyPathLaplacianValidatedU dimension inputVector)
        else
          Left
            ( InvariantViolation
                ( "path Laplacian input dimension mismatch: expected "
                    <> show dimension
                    <> " but received "
                    <> show (U.length inputVector)
                )
            )
    SymmetricTridiagonalSource tridiagonalValue ->
      let dimension = symmetricTridiagonalDimension tridiagonalValue
       in if U.length inputVector == dimension
            then
              Right
                ( applySymmetricTridiagonalValidatedU
                    tridiagonalValue
                    inputVector
                )
            else
              Left
                ( InvariantViolation
                    ( "symmetric tridiagonal operator input dimension mismatch: expected "
                        <> show dimension
                        <> " but received "
                        <> show (U.length inputVector)
                    )
                )
    PackedSparseSource packedOperator ->
      case applyPackedSparseOperatorDense packedOperator inputVector of
        Right output -> Right output
        Left applyError -> Left (packedSparseApplyErrorToMoonlightError applyError)

applyAffineImage :: Double -> Double -> U.Vector Double -> U.Vector Double -> Either MoonlightError (U.Vector Double)
applyAffineImage scaleValue shiftValue sourceImage inputVector
  | shiftValue == 0.0 && scaleValue == 1.0 = Right sourceImage
  | shiftValue == 0.0 = Right (U.map (scaleValue *) sourceImage)
  | U.length sourceImage == U.length inputVector =
      Right (U.zipWith (\imageEntry inputEntry -> scaleValue * imageEntry + shiftValue * inputEntry) sourceImage inputVector)
  | otherwise = Left (InvariantViolation "identity shift requires a square operator image")

applyCSR :: SparseCSR Double -> U.Vector Double -> Either MoonlightError (U.Vector Double)
applyCSR csrValue inputVector =
  if U.length inputVector /= csrCols csrValue
    then Left (InvariantViolation ("CSR matvec dimension mismatch: expected " <> show (csrCols csrValue) <> " but received " <> show (U.length inputVector)))
    else Right (csrMatVecValidatedU (csrRows csrValue) (csrRowOffsetsVector csrValue) (csrColumnIndicesVector csrValue) (csrValuesVector csrValue) inputVector)

applyDiagonal :: U.Vector Double -> U.Vector Double -> Either MoonlightError (U.Vector Double)
applyDiagonal diagonalEntries inputVector =
  if U.length inputVector /= U.length diagonalEntries
    then Left (InvariantViolation ("diagonal operator input dimension mismatch: expected " <> show (U.length diagonalEntries) <> " but received " <> show (U.length inputVector)))
    else Right (U.zipWith (*) diagonalEntries inputVector)

checkedApply :: Int -> Int -> ApplyU -> ApplyU
checkedApply rowCount columnCount applyVector inputVector =
  if U.length inputVector /= columnCount
    then Left (InvariantViolation ("linear operator input dimension mismatch: expected " <> show columnCount <> " but received " <> show (U.length inputVector)))
    else do
      outputVector <- applyVector inputVector
      if U.length outputVector == rowCount
        then Right outputVector
        else Left (InvariantViolation ("linear operator output dimension mismatch: expected " <> show rowCount <> " but received " <> show (U.length outputVector)))

validateRectangularDimensions :: String -> Int -> Int -> Either MoonlightError ()
validateRectangularDimensions label rowCount columnCount =
  if rowCount < 0 || columnCount < 0
    then Left (InvariantViolation (label <> " dimensions must be non-negative"))
    else Right ()

validateDimension :: String -> Int -> Either MoonlightError ()
validateDimension label dimension =
  if dimension <= 0
    then Left (InvariantViolation (label <> " dimension must be positive, received " <> show dimension))
    else Right ()

validateSelfAdjointCSR :: SparseCSR Double -> Either MoonlightError ()
validateSelfAdjointCSR csrValue = do
  if csrRows csrValue /= csrCols csrValue
    then Left (InvariantViolation "self-adjoint CSR operator requires a square matrix")
    else do
      cooValue <- csrToCOO csrValue
      let entryMap = Map.fromList (((\(rowIndex, columnIndex, value) -> ((rowIndex, columnIndex), value)) <$> cooEntries cooValue))
          symmetricEntry ((rowIndex, columnIndex), value) = Map.lookup (columnIndex, rowIndex) entryMap == Just value
      if all symmetricEntry (Map.toList entryMap)
        then Right ()
        else Left (InvariantViolation "self-adjoint CSR operator requires exact symmetric storage")

packedSparseApplyErrorToMoonlightError :: PackedSparseApplyError -> MoonlightError
packedSparseApplyErrorToMoonlightError errorValue =
  case errorValue of
    PackedSparseInputLengthMismatch expectedLength actualLength ->
      InvariantViolation ("packed sparse operator input dimension mismatch: expected " <> show expectedLength <> " but received " <> show actualLength)
