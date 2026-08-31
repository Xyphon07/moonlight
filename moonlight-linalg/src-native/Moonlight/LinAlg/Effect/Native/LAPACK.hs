{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}

module Moonlight.LinAlg.Effect.Native.LAPACK
  ( denseDoubleLinearSolveLapack,
    denseDoubleMatrixProductBlas,
    denseDoubleSymmetricEigenpairsRawLapack,
    leastSquaresLapack,
    selectedSymmetricEigenValuesLapack,
    selectedSymmetricEigenPairsLapack,
    selectedSymmetricBlockTridiagonalEigenValuesLapack,
    selectedSymmetricBlockTridiagonalEigenPairsLapack,
    selectedSymmetricTridiagonalEigenValuesLapack,
    selectedSymmetricTridiagonalEigenPairsLapack,
  )
where

import Control.Monad.ST (ST, runST)
import Data.Bifunctor (first)
import Data.Kind (Type)
import Data.List (transpose)
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as MS
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Foreign
  ( Ptr,
    alloca,
    allocaArray,
    castPtr,
    peek,
    peekArray,
    peekElemOff,
    poke,
    pokeElemOff,
    with,
    withArray,
  )
import Foreign.C.String (castCharToCChar)
import Foreign.C.Types (CChar, CDouble (..), CInt (..))
import Foreign.ForeignPtr (mallocForeignPtrArray, withForeignPtr)
import Moonlight.Core
  ( MoonlightError (..),
    checkedNonNegativeProduct,
    checkedNonNegativeSum,
    fieldValueValid,
  )
import Moonlight.LinAlg.Internal.Storage (chunkRows)
import Moonlight.LinAlg.Pure.Dense.Dynamic
  ( DynMatrix,
    DynVector,
    dynMatrixShape,
    dynMatrixToList,
    dynMatrixToRows,
    dynVectorToList,
  )
import Moonlight.LinAlg.Pure.Dense.Flat
  ( DenseDoubleMatrix,
    denseDoubleMatrixShape,
    denseDoubleMatrixToRowMajorVector,
    trustedDenseDoubleMatrixRowMajor,
  )
import Moonlight.LinAlg.Pure.Krylov.Selection (SpectrumEnd (..))
import Moonlight.LinAlg.Pure.Structured.BlockTridiagonal
  ( SymmetricBlockTridiagonal,
    blockOffsets,
    couplingPayloadOffsets,
    diagonalLowerPacked,
    diagonalPayloadOffsets,
    lowerCouplingPayload,
    symmetricBlockTridiagonalBandwidth,
    symmetricBlockTridiagonalBlockCount,
    symmetricBlockTridiagonalDimension,
  )
import Moonlight.LinAlg.Pure.Structured.Tridiagonal
  ( SymmetricTridiagonal,
    symmetricTridiagonalDiagonalEntries,
    symmetricTridiagonalOffDiagonalEntries,
  )
import Prelude

type FortranIndexRange :: Type
data FortranIndexRange = FortranIndexRange
  { fortranIndexRangeLower :: !Int,
    fortranIndexRangeUpper :: !Int
  }
  deriving stock (Eq, Show)

mkFortranIndexRange :: Int -> Int -> Either MoonlightError FortranIndexRange
mkFortranIndexRange lowerIndex upperIndex
  | lowerIndex < 1 =
      Left (InvariantViolation "Fortran index range lower bound must be positive")
  | upperIndex < lowerIndex =
      Left (InvariantViolation "Fortran index range upper bound must be at least the lower bound")
  | otherwise =
      Right
        FortranIndexRange
          { fortranIndexRangeLower = lowerIndex,
            fortranIndexRangeUpper = upperIndex
          }

denseSelectedEigenIndexRange :: SpectrumEnd -> Int -> Int -> Int -> Either MoonlightError FortranIndexRange
denseSelectedEigenIndexRange spectrumEnd requestedCount rowCount columnCount
  | rowCount /= columnCount =
      Left (InvariantViolation "native dense symmetric eigensolve requires a square matrix")
  | otherwise =
      selectedEigenIndexRange spectrumEnd requestedCount rowCount

selectedEigenIndexRange :: SpectrumEnd -> Int -> Int -> Either MoonlightError FortranIndexRange
selectedEigenIndexRange spectrumEnd requestedCount dimension =
  case selectedNativeIndexBounds spectrumEnd requestedCount dimension of
    Left err -> Left err
    Right (lowerIndex, upperIndex) -> mkFortranIndexRange lowerIndex upperIndex

selectedNativeIndexBounds :: SpectrumEnd -> Int -> Int -> Either MoonlightError (Int, Int)
selectedNativeIndexBounds spectrumEnd requestedCount dimension
  | requestedCount <= 0 = Left (InvariantViolation "native eigen request count must be positive")
  | requestedCount > dimension = Left (InvariantViolation "native eigen request count exceeds matrix dimension")
  | otherwise =
      Right
        ( case spectrumEnd of
            SmallestEigenvalues -> (1, requestedCount)
            LargestEigenvalues -> (dimension - requestedCount + 1, dimension)
        )

foreign import ccall unsafe "dsyev_"
  lapackDsyev ::
    Ptr CChar ->
    Ptr CChar ->
    Ptr CInt ->
    Ptr CDouble ->
    Ptr CInt ->
    Ptr CDouble ->
    Ptr CDouble ->
    Ptr CInt ->
    Ptr CInt ->
    IO ()

foreign import ccall unsafe "moonlight_dgemm_row_major"
  moonlightDgemmRowMajor ::
    CInt ->
    CInt ->
    CInt ->
    Ptr CDouble ->
    Ptr CDouble ->
    Ptr CDouble ->
    IO ()

foreign import ccall unsafe "dgesv_"
  lapackDgesv ::
    Ptr CInt ->
    Ptr CInt ->
    Ptr CDouble ->
    Ptr CInt ->
    Ptr CInt ->
    Ptr CDouble ->
    Ptr CInt ->
    Ptr CInt ->
    IO ()

foreign import ccall unsafe "dsyevx_"
  lapackDsyevx ::
    Ptr CChar ->
    Ptr CChar ->
    Ptr CChar ->
    Ptr CInt ->
    Ptr CDouble ->
    Ptr CInt ->
    Ptr CDouble ->
    Ptr CDouble ->
    Ptr CInt ->
    Ptr CInt ->
    Ptr CDouble ->
    Ptr CInt ->
    Ptr CDouble ->
    Ptr CDouble ->
    Ptr CInt ->
    Ptr CDouble ->
    Ptr CInt ->
    Ptr CInt ->
    Ptr CInt ->
    Ptr CInt ->
    IO ()

foreign import ccall unsafe "dgels_"
  lapackDgels ::
    Ptr CChar ->
    Ptr CInt ->
    Ptr CInt ->
    Ptr CInt ->
    Ptr CDouble ->
    Ptr CInt ->
    Ptr CDouble ->
    Ptr CInt ->
    Ptr CDouble ->
    Ptr CInt ->
    Ptr CInt ->
    IO ()

foreign import ccall unsafe "dstemr_"
  lapackDstemr ::
    Ptr CChar ->
    Ptr CChar ->
    Ptr CInt ->
    Ptr CDouble ->
    Ptr CDouble ->
    Ptr CDouble ->
    Ptr CDouble ->
    Ptr CInt ->
    Ptr CInt ->
    Ptr CInt ->
    Ptr CDouble ->
    Ptr CDouble ->
    Ptr CInt ->
    Ptr CInt ->
    Ptr CInt ->
    Ptr CInt ->
    Ptr CDouble ->
    Ptr CInt ->
    Ptr CInt ->
    Ptr CInt ->
    Ptr CInt ->
    IO ()

foreign import ccall unsafe "dsbevx_"
  lapackDsbevx ::
    Ptr CChar ->
    Ptr CChar ->
    Ptr CChar ->
    Ptr CInt ->
    Ptr CInt ->
    Ptr CDouble ->
    Ptr CInt ->
    Ptr CDouble ->
    Ptr CInt ->
    Ptr CDouble ->
    Ptr CDouble ->
    Ptr CInt ->
    Ptr CInt ->
    Ptr CDouble ->
    Ptr CInt ->
    Ptr CDouble ->
    Ptr CDouble ->
    Ptr CInt ->
    Ptr CDouble ->
    Ptr CInt ->
    Ptr CInt ->
    Ptr CInt ->
    IO ()

denseDoubleMatrixProductBlas :: DenseDoubleMatrix -> DenseDoubleMatrix -> IO (Either MoonlightError DenseDoubleMatrix)
denseDoubleMatrixProductBlas leftMatrix rightMatrix =
  let (leftRows, leftColumns) = denseDoubleMatrixShape leftMatrix
      (rightRows, rightColumns) = denseDoubleMatrixShape rightMatrix
   in case validateDenseProductInput leftRows leftColumns rightRows rightColumns of
        Left err -> pure (Left err)
        Right ()
          | otherwise ->
              case checkedProduct "BLAS dense matrix product output entry count" leftRows rightColumns of
                Left err -> pure (Left err)
                Right outputLength
                  | leftRows == 0 || rightColumns == 0 ->
                      pure (Right (trustedDenseDoubleMatrixRowMajor leftRows rightColumns S.empty))
                  | leftColumns == 0 ->
                      pure
                        ( Right
                            ( trustedDenseDoubleMatrixRowMajor
                                leftRows
                                rightColumns
                                (S.replicate outputLength 0.0)
                            )
                        )
                  | otherwise ->
                      case traverse matrixSizeAsLapackInt [leftRows, rightColumns, leftColumns] of
                        Left err -> pure (Left err)
                        Right [lapackLeftRows, lapackRightColumns, lapackInner] ->
                          solveDenseProductBlas lapackLeftRows lapackRightColumns lapackInner leftRows rightColumns outputLength leftPayload rightPayload
                        Right _ -> pure (Left (InvariantViolation "BLAS dense matrix product internal dimension arity mismatch"))
  where
    leftPayload = denseDoubleMatrixToRowMajorVector leftMatrix
    rightPayload = denseDoubleMatrixToRowMajorVector rightMatrix

denseDoubleLinearSolveLapack :: DenseDoubleMatrix -> S.Vector Double -> IO (Either MoonlightError (S.Vector Double))
denseDoubleLinearSolveLapack matrixValue rightHandSide =
  let (rowCount, columnCount) = denseDoubleMatrixShape matrixValue
      matrixPayload = denseDoubleMatrixToRowMajorVector matrixValue
   in case validateDenseLinearSolveInput rowCount columnCount rightHandSide of
        Left err -> pure (Left err)
        Right () ->
          case matrixSizeAsLapackInt rowCount of
            Left err -> pure (Left err)
            Right lapackSize ->
              solveDenseLinearSystemLapack lapackSize rowCount matrixPayload rightHandSide

denseDoubleSymmetricEigenpairsRawLapack :: DenseDoubleMatrix -> IO (Either MoonlightError (S.Vector Double, S.Vector Double))
denseDoubleSymmetricEigenpairsRawLapack matrixValue =
  let (rowCount, columnCount) = denseDoubleMatrixShape matrixValue
      matrixPayload = denseDoubleMatrixToRowMajorVector matrixValue
   in case validateDenseSymmetricEigenInput rowCount columnCount of
        Left err -> pure (Left err)
        Right () ->
          case matrixSizeAsLapackInt rowCount of
            Left err -> pure (Left err)
            Right lapackSize ->
              solveDenseSymmetricEigenpairsRawLapack lapackSize rowCount matrixPayload

validateDenseProductInput ::
  Int ->
  Int ->
  Int ->
  Int ->
  Either MoonlightError ()
validateDenseProductInput leftRows leftColumns rightRows rightColumns
  | leftColumns /= rightRows =
      Left
        ( InvariantViolation
            ( "BLAS dense matrix product shape mismatch: left "
                <> show (leftRows, leftColumns)
                <> " right "
                <> show (rightRows, rightColumns)
            )
        )
  | otherwise = Right ()

validateDenseLinearSolveInput ::
  Int ->
  Int ->
  S.Vector Double ->
  Either MoonlightError ()
validateDenseLinearSolveInput rowCount columnCount rightHandSide
  | rowCount /= columnCount =
      Left (InvariantViolation "LAPACK dense linear solve requires a square matrix")
  | rowCount <= 0 =
      Left (InvariantViolation "LAPACK dense linear solve requires a positive dimension")
  | S.length rightHandSide /= rowCount =
      Left
        ( InvariantViolation
            ( "LAPACK dense linear solve right-hand side length mismatch: matrix dimension "
                <> show rowCount
                <> ", vector "
                <> show (S.length rightHandSide)
            )
        )
  | S.any (not . fieldValueValid) rightHandSide =
      Left (InvariantViolation "LAPACK dense linear solve requires finite right-hand side entries")
  | otherwise = Right ()

validateDenseSymmetricEigenInput ::
  Int ->
  Int ->
  Either MoonlightError ()
validateDenseSymmetricEigenInput rowCount columnCount
  | rowCount /= columnCount =
      Left (InvariantViolation "LAPACK dense symmetric eigensolve requires a square matrix")
  | rowCount <= 0 =
      Left (InvariantViolation "LAPACK dense symmetric eigensolve requires a positive dimension")
  | otherwise = Right ()

solveDenseProductBlas ::
  CInt ->
  CInt ->
  CInt ->
  Int ->
  Int ->
  Int ->
  S.Vector Double ->
  S.Vector Double ->
  IO (Either MoonlightError DenseDoubleMatrix)
solveDenseProductBlas !lapackLeftRows !lapackRightColumns !lapackInner !leftRows !rightColumns !outputLength leftPayload rightPayload = do
  outputPayload <- MS.unsafeNew outputLength
  S.unsafeWith leftPayload $ \leftPointer ->
    S.unsafeWith rightPayload $ \rightPointer ->
      MS.unsafeWith outputPayload $ \outputPointer ->
        moonlightDgemmRowMajor
          lapackLeftRows
          lapackRightColumns
          lapackInner
          (castPtr leftPointer)
          (castPtr rightPointer)
          (castPtr outputPointer)
  frozenOutput <- S.unsafeFreeze outputPayload
  pure
    ( if S.any (not . fieldValueValid) frozenOutput
        then Left (InvariantViolation "BLAS dense matrix product produced non-finite entries")
        else
          Right
            ( trustedDenseDoubleMatrixRowMajor
                leftRows
                rightColumns
                frozenOutput
            )
    )

solveDenseLinearSystemLapack ::
  CInt ->
  Int ->
  S.Vector Double ->
  S.Vector Double ->
  IO (Either MoonlightError (S.Vector Double))
solveDenseLinearSystemLapack !lapackSize !matrixSize matrixPayload rightHandSide =
  with lapackSize $ \sizePointer ->
    with (1 :: CInt) $ \rightHandSideCountPointer ->
      with lapackSize $ \leadingDimensionPointer ->
        with lapackSize $ \rightHandSideLeadingDimensionPointer ->
          allocaArray matrixSize $ \pivotPointer ->
            alloca $ \infoPointer -> do
              matrixWork <- S.thaw (denseRowMajorToColumnMajorSquare matrixSize matrixPayload)
              rightHandSideWork <- S.thaw rightHandSide
              MS.unsafeWith matrixWork $ \matrixPointer ->
                MS.unsafeWith rightHandSideWork $ \rightHandSidePointer -> do
                  poke infoPointer 0
                  lapackDgesv
                    sizePointer
                    rightHandSideCountPointer
                    (castPtr matrixPointer)
                    leadingDimensionPointer
                    pivotPointer
                    (castPtr rightHandSidePointer)
                    rightHandSideLeadingDimensionPointer
                    infoPointer
              infoValue <- peek infoPointer
              if infoValue /= 0
                then pure (Left (lapackLinearSolveInfoError infoValue))
                else do
                  solution <- S.unsafeFreeze rightHandSideWork
                  pure
                    ( if S.any (not . fieldValueValid) solution
                        then Left (InvariantViolation "LAPACK dense linear solve produced non-finite entries")
                        else Right solution
                    )

solveDenseSymmetricEigenpairsRawLapack ::
  CInt ->
  Int ->
  S.Vector Double ->
  IO (Either MoonlightError (S.Vector Double, S.Vector Double))
solveDenseSymmetricEigenpairsRawLapack !lapackSize !matrixSize matrixPayload =
  withLapackChar 'V' $ \jobPointer ->
    withLapackChar 'U' $ \uploPointer ->
      with lapackSize $ \sizePointer ->
        with lapackSize $ \leadingDimensionPointer ->
          alloca $ \infoPointer -> do
            matrixWork <- S.thaw matrixPayload
            eigenvalueWork <- MS.replicate matrixSize 0.0
            MS.unsafeWith matrixWork $ \matrixPointer ->
              MS.unsafeWith eigenvalueWork $ \eigenvaluePointer ->
                validateSymmetricDenseBuffer matrixSize (castPtr matrixPointer) >>= \case
                  Left err -> pure (Left err)
                  Right () ->
                    queryWorkspace
                      jobPointer
                      uploPointer
                      sizePointer
                      (castPtr matrixPointer)
                      leadingDimensionPointer
                      (castPtr eigenvaluePointer)
                      infoPointer
                      >>= \case
                        Left err -> pure (Left err)
                        Right workspaceSize ->
                          allocaArray workspaceSize $ \workspacePointer -> do
                            poke infoPointer 0
                            with (fromIntegral workspaceSize) $ \workspaceSizePointer ->
                              lapackDsyev
                                jobPointer
                                uploPointer
                                sizePointer
                                (castPtr matrixPointer)
                                leadingDimensionPointer
                                (castPtr eigenvaluePointer)
                                workspacePointer
                                workspaceSizePointer
                                infoPointer
                            decodeDenseSymmetricEigenpairsRaw matrixWork eigenvalueWork infoPointer

denseRowMajorToColumnMajorSquare :: Int -> S.Vector Double -> S.Vector Double
denseRowMajorToColumnMajorSquare matrixSize matrixPayload =
  S.generate
    (S.length matrixPayload)
    ( \payloadIndex ->
        let (!columnIndex, !rowIndex) = payloadIndex `quotRem` matrixSize
         in matrixPayload `S.unsafeIndex` (rowIndex * matrixSize + columnIndex)
    )
{-# INLINE denseRowMajorToColumnMajorSquare #-}

decodeDenseSymmetricEigenpairsRaw ::
  MS.IOVector Double ->
  MS.IOVector Double ->
  Ptr CInt ->
  IO (Either MoonlightError (S.Vector Double, S.Vector Double))
decodeDenseSymmetricEigenpairsRaw matrixWork eigenvalueWork infoPointer = do
  infoValue <- peek infoPointer
  if infoValue /= 0
    then pure (Left (lapackInfoError "LAPACK DSYEV" infoValue))
    else do
      eigenvalues <- S.unsafeFreeze eigenvalueWork
      eigenvectors <- S.unsafeFreeze matrixWork
      pure
        ( if S.any (not . fieldValueValid) eigenvalues || S.any (not . fieldValueValid) eigenvectors
            then Left (InvariantViolation "LAPACK dense symmetric eigensolve produced non-finite entries")
            else Right (eigenvalues, eigenvectors)
        )

selectedSymmetricEigenPairsLapack ::
  SpectrumEnd ->
  Int ->
  DynMatrix Double ->
  IO
    ( Either
        MoonlightError
        (U.Vector Double, U.Vector Double)
    )
selectedSymmetricEigenPairsLapack spectrumEnd requestedCount matrixValue =
  let (rowCount, columnCount) = dynMatrixShape matrixValue
   in case
        denseSelectedEigenIndexRange
          spectrumEnd
          requestedCount
          rowCount
          columnCount of
        Left err -> pure (Left err)
        Right indexRange ->
          withSelectedSymmetricDenseBuffer
            matrixValue
            ( \matrixSize lapackSize matrixPointer ->
                solveSelectedSymmetricPairsLapack
                  lapackSize
                  matrixSize
                  (fortranIndexRangeLower indexRange)
                  (fortranIndexRangeUpper indexRange)
                  matrixPointer
            )

selectedSymmetricEigenValuesLapack ::
  SpectrumEnd ->
  Int ->
  DynMatrix Double ->
  IO (Either MoonlightError (U.Vector Double))
selectedSymmetricEigenValuesLapack spectrumEnd requestedCount matrixValue =
  let (rowCount, columnCount) = dynMatrixShape matrixValue
   in case
        denseSelectedEigenIndexRange
          spectrumEnd
          requestedCount
          rowCount
          columnCount of
        Left err -> pure (Left err)
        Right indexRange ->
          withSelectedSymmetricDenseBuffer
            matrixValue
            ( \matrixSize lapackSize matrixPointer ->
                let !lowerIndex = fortranIndexRangeLower indexRange
                    !upperIndex = fortranIndexRangeUpper indexRange
                 in if matrixSize <= smallDenseValuesFullThreshold
                      then
                        solveSelectedSymmetricValuesDsyev
                          lapackSize
                          matrixSize
                          lowerIndex
                          upperIndex
                          matrixPointer
                      else
                        solveSelectedSymmetricValuesLapack
                          lapackSize
                          matrixSize
                          lowerIndex
                          upperIndex
                          matrixPointer
            )

smallDenseValuesFullThreshold :: Int
smallDenseValuesFullThreshold = 32
{-# INLINE smallDenseValuesFullThreshold #-}

withSelectedSymmetricDenseBuffer ::
  DynMatrix Double ->
  (Int -> CInt -> Ptr CDouble -> IO (Either MoonlightError result)) ->
  IO (Either MoonlightError result)
withSelectedSymmetricDenseBuffer matrixValue useBuffer =
  case validateSelectedDenseStorage matrixValue of
    Left err -> pure (Left err)
    Right (matrixSize, lapackSize, entryCount) -> do
      matrixForeignPointer <- mallocForeignPtrArray entryCount
      withForeignPtr matrixForeignPointer $ \matrixPointer -> do
        copiedPayload <-
          copyFiniteDensePayload
            entryCount
            (dynMatrixToList matrixValue)
            matrixPointer
        case copiedPayload of
          Left err -> pure (Left err)
          Right () -> do
            symmetryResult <-
              validateSymmetricDenseBuffer
                matrixSize
                matrixPointer
            case symmetryResult of
              Left err -> pure (Left err)
              Right () ->
                useBuffer matrixSize lapackSize matrixPointer

validateSelectedDenseStorage ::
  DynMatrix Double ->
  Either MoonlightError (Int, CInt, Int)
validateSelectedDenseStorage matrixValue = do
  let (rowCount, columnCount) = dynMatrixShape matrixValue
  if rowCount /= columnCount
    then
      Left
        ( InvariantViolation
            "LAPACK selected symmetric eigensolve requires a square matrix"
        )
    else pure ()
  if rowCount <= 0
    then
      Left
        ( InvariantViolation
            "LAPACK selected symmetric eigensolve requires a positive dimension"
        )
    else pure ()
  entryCount <-
    checkedProduct
      "LAPACK selected symmetric matrix entry count"
      rowCount
      columnCount
  lapackSize <- matrixSizeAsLapackInt rowCount
  pure (rowCount, lapackSize, entryCount)

copyFiniteDensePayload ::
  Int ->
  [Double] ->
  Ptr CDouble ->
  IO (Either MoonlightError ())
copyFiniteDensePayload expectedCount values targetPointer =
  go 0 values
  where
    go !entryIndex remainingValues
      | entryIndex >= expectedCount =
          case remainingValues of
            [] -> pure (Right ())
            _ ->
              pure
                ( Left
                    ( InvariantViolation
                        "LAPACK selected symmetric matrix payload contains excess entries"
                    )
                )
      | otherwise =
          case remainingValues of
            [] ->
              pure
                ( Left
                    ( InvariantViolation
                        ( "LAPACK selected symmetric matrix payload ended at offset "
                            <> show entryIndex
                        )
                    )
                )
            entryValue : rest
              | not (fieldValueValid entryValue) ->
                  pure
                    ( Left
                        ( InvariantViolation
                            ( "LAPACK selected symmetric eigensolve requires finite entries; invalid offset "
                                <> show entryIndex
                            )
                        )
                    )
              | otherwise -> do
                  pokeElemOff targetPointer entryIndex (CDouble entryValue)
                  go (entryIndex + 1) rest

validateSymmetricDenseBuffer ::
  Int ->
  Ptr CDouble ->
  IO (Either MoonlightError ())
validateSymmetricDenseBuffer matrixSize matrixPointer =
  validateRow 0
  where
    !tolerance = 1.0e-6

    validateRow !rowIndex
      | rowIndex >= matrixSize = pure (Right ())
      | otherwise = validateColumn rowIndex (rowIndex + 1)

    validateColumn !rowIndex !columnIndex
      | columnIndex >= matrixSize = validateRow (rowIndex + 1)
      | otherwise = do
          CDouble upperValue <-
            peekElemOff
              matrixPointer
              (rowIndex * matrixSize + columnIndex)
          CDouble lowerValue <-
            peekElemOff
              matrixPointer
              (columnIndex * matrixSize + rowIndex)
          if abs (upperValue - lowerValue) <= tolerance
            then validateColumn rowIndex (columnIndex + 1)
            else
              pure
                ( Left
                    ( InvariantViolation
                        ( "LAPACK selected symmetric eigensolve requires a symmetric matrix; mismatch at "
                            <> show (rowIndex, columnIndex)
                        )
                    )
                )

leastSquaresLapack :: DynMatrix Double -> DynVector Double -> IO (Either MoonlightError [Double])
leastSquaresLapack matrixValue rightHandSideValue =
  case dynMatrixToRows matrixValue of
    Left err -> pure (Left err)
    Right matrixToRows ->
      let (rowCount, columnCount) = dynMatrixShape matrixValue
          rightHandSide = dynVectorToList rightHandSideValue
       in case validateLeastSquaresInput rowCount columnCount matrixToRows rightHandSide of
            Left err -> pure (Left err)
            Right () ->
              case (matrixSizeAsLapackInt rowCount, matrixSizeAsLapackInt columnCount) of
                (Right lapackRows, Right lapackColumns) ->
                  solveLeastSquaresLapack lapackRows lapackColumns rowCount columnCount matrixToRows rightHandSide
                (Left err, _) -> pure (Left err)
                (_, Left err) -> pure (Left err)

selectedSymmetricTridiagonalEigenPairsLapack ::
  SpectrumEnd ->
  Int ->
  SymmetricTridiagonal ->
  IO (Either MoonlightError [(Double, [Double])])
selectedSymmetricTridiagonalEigenPairsLapack spectrumEnd requestedCount tridiagonalValue =
  let diagonalValues = symmetricTridiagonalDiagonalEntries tridiagonalValue
      offDiagonalValues = symmetricTridiagonalOffDiagonalEntries tridiagonalValue
   in case selectedEigenIndexRange spectrumEnd requestedCount (length diagonalValues) of
        Left err -> pure (Left err)
        Right indexRange ->
          let lowerIndex = fortranIndexRangeLower indexRange
              upperIndex = fortranIndexRangeUpper indexRange
           in case validateSelectedTridiagonalInput lowerIndex upperIndex diagonalValues offDiagonalValues of
                Left err -> pure (Left err)
                Right matrixSize ->
                  case matrixSizeAsLapackInt matrixSize of
                    Left err -> pure (Left err)
                    Right lapackSize ->
                      solveSelectedTridiagonalLapack lapackSize matrixSize lowerIndex upperIndex diagonalValues offDiagonalValues

selectedSymmetricTridiagonalEigenValuesLapack ::
  SpectrumEnd ->
  Int ->
  SymmetricTridiagonal ->
  IO (Either MoonlightError (U.Vector Double))
selectedSymmetricTridiagonalEigenValuesLapack spectrumEnd requestedCount tridiagonalValue =
  let diagonalValues = symmetricTridiagonalDiagonalEntries tridiagonalValue
      offDiagonalValues = symmetricTridiagonalOffDiagonalEntries tridiagonalValue
   in case selectedEigenIndexRange spectrumEnd requestedCount (length diagonalValues) of
        Left err -> pure (Left err)
        Right indexRange ->
          let lowerIndex = fortranIndexRangeLower indexRange
              upperIndex = fortranIndexRangeUpper indexRange
           in case validateSelectedTridiagonalInput lowerIndex upperIndex diagonalValues offDiagonalValues of
                Left err -> pure (Left err)
                Right matrixSize ->
                  case matrixSizeAsLapackInt matrixSize of
                    Left err -> pure (Left err)
                    Right lapackSize ->
                      solveSelectedTridiagonalValuesLapack lapackSize matrixSize lowerIndex upperIndex diagonalValues offDiagonalValues

selectedSymmetricBlockTridiagonalEigenPairsLapack ::
  SpectrumEnd ->
  Int ->
  SymmetricBlockTridiagonal ->
  IO (Either MoonlightError [(Double, [Double])])
selectedSymmetricBlockTridiagonalEigenPairsLapack spectrumEnd requestedCount blockValue =
  let matrixSize = symmetricBlockTridiagonalDimension blockValue
      bandwidth = symmetricBlockTridiagonalBandwidth blockValue
   in case selectedEigenIndexRange spectrumEnd requestedCount matrixSize of
        Left err -> pure (Left err)
        Right indexRange ->
          let lowerIndex = fortranIndexRangeLower indexRange
              upperIndex = fortranIndexRangeUpper indexRange
           in case symmetricBlockTridiagonalLowerBandPayload blockValue of
                Left err -> pure (Left err)
                Right lowerBandPayload ->
                  case validateSelectedBandInput lowerIndex upperIndex matrixSize bandwidth lowerBandPayload of
                    Left err -> pure (Left err)
                    Right leadingDimensionValue ->
                      case (matrixSizeAsLapackInt matrixSize, matrixSizeAsLapackInt bandwidth, matrixSizeAsLapackInt leadingDimensionValue) of
                        (Right lapackSize, Right lapackBandwidth, Right leadingDimension) ->
                          solveSelectedBandPairsLapack lapackSize lapackBandwidth leadingDimension matrixSize lowerIndex upperIndex lowerBandPayload
                        (Left err, _, _) -> pure (Left err)
                        (_, Left err, _) -> pure (Left err)
                        (_, _, Left err) -> pure (Left err)

selectedSymmetricBlockTridiagonalEigenValuesLapack ::
  SpectrumEnd ->
  Int ->
  SymmetricBlockTridiagonal ->
  IO (Either MoonlightError (U.Vector Double))
selectedSymmetricBlockTridiagonalEigenValuesLapack spectrumEnd requestedCount blockValue =
  let matrixSize = symmetricBlockTridiagonalDimension blockValue
      bandwidth = symmetricBlockTridiagonalBandwidth blockValue
   in case selectedEigenIndexRange spectrumEnd requestedCount matrixSize of
        Left err -> pure (Left err)
        Right indexRange ->
          let lowerIndex = fortranIndexRangeLower indexRange
              upperIndex = fortranIndexRangeUpper indexRange
           in case symmetricBlockTridiagonalLowerBandPayload blockValue of
                Left err -> pure (Left err)
                Right lowerBandPayload ->
                  case validateSelectedBandInput lowerIndex upperIndex matrixSize bandwidth lowerBandPayload of
                    Left err -> pure (Left err)
                    Right leadingDimensionValue ->
                      case (matrixSizeAsLapackInt matrixSize, matrixSizeAsLapackInt bandwidth, matrixSizeAsLapackInt leadingDimensionValue) of
                        (Right lapackSize, Right lapackBandwidth, Right leadingDimension) ->
                          solveSelectedBandValuesLapack lapackSize lapackBandwidth leadingDimension matrixSize lowerIndex upperIndex lowerBandPayload
                        (Left err, _, _) -> pure (Left err)
                        (_, Left err, _) -> pure (Left err)
                        (_, _, Left err) -> pure (Left err)

symmetricBlockTridiagonalLowerBandPayload :: SymmetricBlockTridiagonal -> Either MoonlightError (U.Vector Double)
symmetricBlockTridiagonalLowerBandPayload blockValue = do
  let dimension = symmetricBlockTridiagonalDimension blockValue
  leadingDimension <-
    checkedSum
      "LAPACK symmetric block-tridiagonal leading dimension"
      (symmetricBlockTridiagonalBandwidth blockValue)
      1
  payloadLength <-
    checkedProduct
      "LAPACK symmetric block-tridiagonal band payload"
      leadingDimension
      dimension
  Right $ runST $ do
    bandPayload <- MU.replicate payloadLength 0.0
    U.foldM'
      (writeDiagonalBandBlock blockValue leadingDimension bandPayload)
      ()
      (U.enumFromN 0 (symmetricBlockTridiagonalBlockCount blockValue))
    U.foldM'
      (writeCouplingBandBlock blockValue leadingDimension bandPayload)
      ()
      (U.enumFromN 0 (max 0 (symmetricBlockTridiagonalBlockCount blockValue - 1)))
    U.unsafeFreeze bandPayload

writeDiagonalBandBlock ::
  SymmetricBlockTridiagonal ->
  Int ->
  MU.MVector s Double ->
  () ->
  Int ->
  ST s ()
writeDiagonalBandBlock blockValue leadingDimension bandPayload () blockIndex =
  U.foldM'
    (writeDiagonalBandRow blockValue leadingDimension bandPayload blockIndex blockStart)
    ()
    (U.enumFromN 0 (nativeBlockSizeAt blockValue blockIndex))
  where
    blockStart = nativeIntAt (blockOffsets blockValue) blockIndex
{-# INLINE writeDiagonalBandBlock #-}

writeDiagonalBandRow ::
  SymmetricBlockTridiagonal ->
  Int ->
  MU.MVector s Double ->
  Int ->
  Int ->
  () ->
  Int ->
  ST s ()
writeDiagonalBandRow blockValue leadingDimension bandPayload blockIndex blockStart () localRow =
  U.foldM'
    (writeDiagonalBandEntry blockValue leadingDimension bandPayload blockIndex blockStart localRow)
    ()
    (U.enumFromN 0 (localRow + 1))
{-# INLINE writeDiagonalBandRow #-}

writeDiagonalBandEntry ::
  SymmetricBlockTridiagonal ->
  Int ->
  MU.MVector s Double ->
  Int ->
  Int ->
  Int ->
  () ->
  Int ->
  ST s ()
writeDiagonalBandEntry blockValue leadingDimension bandPayload blockIndex blockStart localRow () localColumn =
  writeLowerBandEntry
    leadingDimension
    bandPayload
    (blockStart + localRow)
    (blockStart + localColumn)
    (nativeDiagonalEntry blockValue blockIndex localRow localColumn)
{-# INLINE writeDiagonalBandEntry #-}

writeCouplingBandBlock ::
  SymmetricBlockTridiagonal ->
  Int ->
  MU.MVector s Double ->
  () ->
  Int ->
  ST s ()
writeCouplingBandBlock blockValue leadingDimension bandPayload () couplingIndex =
  U.foldM'
    (writeCouplingBandRow blockValue leadingDimension bandPayload couplingIndex upperBlockStart lowerBlockStart)
    ()
    (U.enumFromN 0 (nativeBlockSizeAt blockValue (couplingIndex + 1)))
  where
    upperBlockStart = nativeIntAt (blockOffsets blockValue) couplingIndex
    lowerBlockStart = nativeIntAt (blockOffsets blockValue) (couplingIndex + 1)
{-# INLINE writeCouplingBandBlock #-}

writeCouplingBandRow ::
  SymmetricBlockTridiagonal ->
  Int ->
  MU.MVector s Double ->
  Int ->
  Int ->
  Int ->
  () ->
  Int ->
  ST s ()
writeCouplingBandRow blockValue leadingDimension bandPayload couplingIndex upperBlockStart lowerBlockStart () localRow =
  U.foldM'
    (writeCouplingBandEntry blockValue leadingDimension bandPayload couplingIndex upperBlockStart lowerBlockStart localRow)
    ()
    (U.enumFromN 0 (nativeBlockSizeAt blockValue couplingIndex))
{-# INLINE writeCouplingBandRow #-}

writeCouplingBandEntry ::
  SymmetricBlockTridiagonal ->
  Int ->
  MU.MVector s Double ->
  Int ->
  Int ->
  Int ->
  Int ->
  () ->
  Int ->
  ST s ()
writeCouplingBandEntry blockValue leadingDimension bandPayload couplingIndex _ lowerBlockStart localRow () localColumn =
  writeLowerBandEntry
    leadingDimension
    bandPayload
    (lowerBlockStart + localRow)
    (nativeIntAt (blockOffsets blockValue) couplingIndex + localColumn)
    (nativeCouplingEntry blockValue couplingIndex localRow localColumn)
{-# INLINE writeCouplingBandEntry #-}

writeLowerBandEntry :: Int -> MU.MVector s Double -> Int -> Int -> Double -> ST s ()
writeLowerBandEntry leadingDimension bandPayload rowIndex columnIndex entryValue =
  MU.unsafeWrite bandPayload (columnIndex * leadingDimension + rowIndex - columnIndex) entryValue
{-# INLINE writeLowerBandEntry #-}

nativeBlockSizeAt :: SymmetricBlockTridiagonal -> Int -> Int
nativeBlockSizeAt blockValue blockIndex =
  nativeIntAt (blockOffsets blockValue) (blockIndex + 1)
    - nativeIntAt (blockOffsets blockValue) blockIndex
{-# INLINE nativeBlockSizeAt #-}

nativeDiagonalEntry :: SymmetricBlockTridiagonal -> Int -> Int -> Int -> Double
nativeDiagonalEntry blockValue blockIndex localRow localColumn
  | localColumn <= localRow =
      nativeDoubleAt (diagonalLowerPacked blockValue) (nativeDiagonalPayloadStart blockValue blockIndex + nativePackedLowerIndex localRow localColumn)
  | otherwise =
      nativeDoubleAt (diagonalLowerPacked blockValue) (nativeDiagonalPayloadStart blockValue blockIndex + nativePackedLowerIndex localColumn localRow)
{-# INLINE nativeDiagonalEntry #-}

nativeCouplingEntry :: SymmetricBlockTridiagonal -> Int -> Int -> Int -> Double
nativeCouplingEntry blockValue couplingIndex localRow localColumn =
  let couplingStart = nativeIntAt (couplingPayloadOffsets blockValue) couplingIndex
      couplingColumns = nativeBlockSizeAt blockValue couplingIndex
   in nativeDoubleAt (lowerCouplingPayload blockValue) (couplingStart + localRow * couplingColumns + localColumn)
{-# INLINE nativeCouplingEntry #-}

nativeDiagonalPayloadStart :: SymmetricBlockTridiagonal -> Int -> Int
nativeDiagonalPayloadStart blockValue blockIndex =
  nativeIntAt (diagonalPayloadOffsets blockValue) blockIndex
{-# INLINE nativeDiagonalPayloadStart #-}

nativePackedLowerIndex :: Int -> Int -> Int
nativePackedLowerIndex rowIndex columnIndex =
  rowIndex * (rowIndex + 1) `quot` 2 + columnIndex
{-# INLINE nativePackedLowerIndex #-}

nativeIntAt :: U.Vector Int -> Int -> Int
nativeIntAt values indexValue =
  maybe 0 id (values U.!? indexValue)
{-# INLINE nativeIntAt #-}

nativeDoubleAt :: U.Vector Double -> Int -> Double
nativeDoubleAt values indexValue =
  maybe 0.0 id (values U.!? indexValue)
{-# INLINE nativeDoubleAt #-}

solveSelectedSymmetricPairsLapack ::
  CInt ->
  Int ->
  Int ->
  Int ->
  Ptr CDouble ->
  IO
    ( Either
        MoonlightError
        (U.Vector Double, U.Vector Double)
    )
solveSelectedSymmetricPairsLapack
  !lapackSize
  !matrixSize
  !lowerIndex
  !upperIndex
  matrixPointer =
  case
      (,,)
        <$> checkedProduct "LAPACK selected symmetric eigenvector workspace" matrixSize selectedCount
        <*> checkedProduct "LAPACK DSYEVX floating workspace" 8 matrixSize
        <*> checkedProduct "LAPACK DSYEVX integer workspace" 5 matrixSize
    of
      Left err -> pure (Left err)
      Right (eigenvectorEntryCount, workspaceCount, integerWorkspaceCount) -> withLapackChar 'V' $ \jobPointer ->
        withLapackChar 'I' $ \rangePointer ->
          -- The row-major payload is the column-major payload of A^T.  Reading
          -- its upper triangle therefore preserves the original lower triangle.
          withLapackChar 'U' $ \uploPointer ->
            with lapackSize $ \sizePointer ->
              with lapackSize $ \leadingDimensionPointer ->
                with 0.0 $ \lowerValuePointer ->
                  with 0.0 $ \upperValuePointer ->
                    with (fromIntegral lowerIndex) $ \lowerIndexPointer ->
                      with (fromIntegral upperIndex) $ \upperIndexPointer ->
                        with 0.0 $ \absoluteTolerancePointer ->
                          alloca $ \foundCountPointer ->
                            allocaArray matrixSize $ \eigenvaluePointer ->
                              allocaArray eigenvectorEntryCount $ \eigenvectorPointer ->
                                with lapackSize $ \eigenvectorLeadingDimensionPointer ->
                                  allocaArray workspaceCount $ \workspacePointer ->
                                    with (fromIntegral workspaceCount) $ \workspaceSizePointer ->
                                      allocaArray integerWorkspaceCount $ \integerWorkspacePointer ->
                                        allocaArray matrixSize $ \failedVectorPointer ->
                                          alloca $ \infoPointer -> do
                                            poke foundCountPointer 0
                                            poke infoPointer 0
                                            lapackDsyevx
                                              jobPointer
                                              rangePointer
                                              uploPointer
                                              sizePointer
                                              matrixPointer
                                              leadingDimensionPointer
                                              lowerValuePointer
                                              upperValuePointer
                                              lowerIndexPointer
                                              upperIndexPointer
                                              absoluteTolerancePointer
                                              foundCountPointer
                                              eigenvaluePointer
                                              eigenvectorPointer
                                              eigenvectorLeadingDimensionPointer
                                              workspacePointer
                                              workspaceSizePointer
                                              integerWorkspacePointer
                                              failedVectorPointer
                                              infoPointer
                                            decodeSelectedSymmetricColumns
                                              selectedCount
                                              eigenvectorEntryCount
                                              eigenvaluePointer
                                              eigenvectorPointer
                                              foundCountPointer
                                              infoPointer
  where
    !selectedCount = upperIndex - lowerIndex + 1

solveSelectedSymmetricValuesLapack ::
  CInt ->
  Int ->
  Int ->
  Int ->
  Ptr CDouble ->
  IO (Either MoonlightError (U.Vector Double))
solveSelectedSymmetricValuesLapack
  !lapackSize
  !matrixSize
  !lowerIndex
  !upperIndex
  matrixPointer =
  case
      (,)
        <$> checkedProduct "LAPACK DSYEVX values floating workspace" 8 matrixSize
        <*> checkedProduct "LAPACK DSYEVX values integer workspace" 5 matrixSize
    of
      Left err -> pure (Left err)
      Right (workspaceCount, integerWorkspaceCount) -> withLapackChar 'N' $ \jobPointer ->
        withLapackChar 'I' $ \rangePointer ->
          withLapackChar 'U' $ \uploPointer ->
            with lapackSize $ \sizePointer ->
              with lapackSize $ \leadingDimensionPointer ->
                with 0.0 $ \lowerValuePointer ->
                  with 0.0 $ \upperValuePointer ->
                    with (fromIntegral lowerIndex) $ \lowerIndexPointer ->
                      with (fromIntegral upperIndex) $ \upperIndexPointer ->
                        with 0.0 $ \absoluteTolerancePointer ->
                          alloca $ \foundCountPointer ->
                            allocaArray matrixSize $ \eigenvaluePointer ->
                              allocaArray 1 $ \eigenvectorPointer ->
                                with (1 :: CInt) $ \eigenvectorLeadingDimensionPointer ->
                                  allocaArray workspaceCount $ \workspacePointer ->
                                    with (fromIntegral workspaceCount) $ \workspaceSizePointer ->
                                      allocaArray integerWorkspaceCount $ \integerWorkspacePointer ->
                                        allocaArray 1 $ \failedVectorPointer ->
                                          alloca $ \infoPointer -> do
                                            poke foundCountPointer 0
                                            poke infoPointer 0
                                            lapackDsyevx
                                              jobPointer
                                              rangePointer
                                              uploPointer
                                              sizePointer
                                              matrixPointer
                                              leadingDimensionPointer
                                              lowerValuePointer
                                              upperValuePointer
                                              lowerIndexPointer
                                              upperIndexPointer
                                              absoluteTolerancePointer
                                              foundCountPointer
                                              eigenvaluePointer
                                              eigenvectorPointer
                                              eigenvectorLeadingDimensionPointer
                                              workspacePointer
                                              workspaceSizePointer
                                              integerWorkspacePointer
                                              failedVectorPointer
                                              infoPointer
                                            decodeSelectedSymmetricValues
                                              selectedCount
                                              eigenvaluePointer
                                              foundCountPointer
                                              infoPointer
  where
    !selectedCount = upperIndex - lowerIndex + 1

solveSelectedSymmetricValuesDsyev ::
  CInt ->
  Int ->
  Int ->
  Int ->
  Ptr CDouble ->
  IO (Either MoonlightError (U.Vector Double))
solveSelectedSymmetricValuesDsyev
  !lapackSize
  !matrixSize
  !lowerIndex
  !upperIndex
  matrixPointer =
  case checkedProduct "LAPACK DSYEV values workspace" 66 matrixSize of
    Left err -> pure (Left err)
    Right workspaceCount -> withLapackChar 'N' $ \jobPointer ->
        withLapackChar 'U' $ \uploPointer ->
          with lapackSize $ \sizePointer ->
            with lapackSize $ \leadingDimensionPointer ->
              allocaArray matrixSize $ \eigenvaluePointer ->
                allocaArray workspaceCount $ \workspacePointer ->
                  with (fromIntegral workspaceCount) $ \workspaceSizePointer ->
                    alloca $ \infoPointer -> do
                      poke infoPointer 0
                      lapackDsyev
                        jobPointer
                        uploPointer
                        sizePointer
                        matrixPointer
                        leadingDimensionPointer
                        eigenvaluePointer
                        workspacePointer
                        workspaceSizePointer
                        infoPointer
                      infoValue <- peek infoPointer
                      if infoValue /= 0
                        then pure (Left (lapackInfoError "LAPACK DSYEV" infoValue))
                        else
                          Right
                            <$> peekCDoubleVectorSlice
                              (lowerIndex - 1)
                              selectedCount
                              eigenvaluePointer
  where
    !selectedCount = upperIndex - lowerIndex + 1

queryWorkspace ::
  Ptr CChar ->
  Ptr CChar ->
  Ptr CInt ->
  Ptr CDouble ->
  Ptr CInt ->
  Ptr CDouble ->
  Ptr CInt ->
  IO (Either MoonlightError Int)
queryWorkspace jobPointer uploPointer sizePointer matrixPointer leadingDimensionPointer eigenvaluePointer infoPointer =
  alloca $ \workspaceQueryPointer ->
    with (-1) $ \workspaceSizePointer -> do
      poke infoPointer 0
      lapackDsyev
        jobPointer
        uploPointer
        sizePointer
        matrixPointer
        leadingDimensionPointer
        eigenvaluePointer
        workspaceQueryPointer
        workspaceSizePointer
        infoPointer
      infoValue <- peek infoPointer
      workspaceQuery <- peek workspaceQueryPointer
      pure
        ( if infoValue == 0
            then checkedLapackWorkspaceQuery "LAPACK DSYEV workspace query" (realToFrac workspaceQuery)
            else Left (lapackInfoError "LAPACK DSYEV workspace query" infoValue)
        )

solveLeastSquaresLapack :: CInt -> CInt -> Int -> Int -> [[Double]] -> [Double] -> IO (Either MoonlightError [Double])
solveLeastSquaresLapack !lapackRows !lapackColumns !rowCount !columnCount matrixToRows rightHandSide =
  withLapackChar 'N' $ \transPointer ->
    with lapackRows $ \rowPointer ->
      with lapackColumns $ \columnPointer ->
        with (1 :: CInt) $ \rightHandSideCountPointer ->
          with lapackRows $ \leadingDimensionPointer ->
            with (max lapackRows lapackColumns) $ \rightHandSideLeadingDimensionPointer ->
              withArray (toColumnMajor matrixToRows) $ \matrixPointer ->
                withArray (leastSquaresRightHandSidePayload rowCount columnCount rightHandSide) $ \rightHandSidePointer ->
                  alloca $ \infoPointer ->
                    queryLeastSquaresWorkspace
                      transPointer
                      rowPointer
                      columnPointer
                      rightHandSideCountPointer
                      matrixPointer
                      leadingDimensionPointer
                      rightHandSidePointer
                      rightHandSideLeadingDimensionPointer
                      infoPointer
                      >>= \case
                        Left err -> pure (Left err)
                        Right workspaceSize ->
                          allocaArray workspaceSize $ \workspacePointer -> do
                            poke infoPointer 0
                            with (fromIntegral workspaceSize) $ \workspaceSizePointer -> do
                              lapackDgels
                                transPointer
                                rowPointer
                                columnPointer
                                rightHandSideCountPointer
                                matrixPointer
                                leadingDimensionPointer
                                rightHandSidePointer
                                rightHandSideLeadingDimensionPointer
                                workspacePointer
                                workspaceSizePointer
                                infoPointer
                              decodeLeastSquares columnCount rightHandSidePointer infoPointer

queryLeastSquaresWorkspace ::
  Ptr CChar ->
  Ptr CInt ->
  Ptr CInt ->
  Ptr CInt ->
  Ptr CDouble ->
  Ptr CInt ->
  Ptr CDouble ->
  Ptr CInt ->
  Ptr CInt ->
  IO (Either MoonlightError Int)
queryLeastSquaresWorkspace transPointer rowPointer columnPointer rightHandSideCountPointer matrixPointer leadingDimensionPointer rightHandSidePointer rightHandSideLeadingDimensionPointer infoPointer =
  alloca $ \workspaceQueryPointer ->
    with (-1) $ \workspaceSizePointer -> do
      poke infoPointer 0
      lapackDgels
        transPointer
        rowPointer
        columnPointer
        rightHandSideCountPointer
        matrixPointer
        leadingDimensionPointer
        rightHandSidePointer
        rightHandSideLeadingDimensionPointer
        workspaceQueryPointer
        workspaceSizePointer
        infoPointer
      infoValue <- peek infoPointer
      workspaceQuery <- peek workspaceQueryPointer
      pure
        ( if infoValue == 0
            then checkedLapackWorkspaceQuery "LAPACK DGELS workspace query" (realToFrac workspaceQuery)
            else Left (lapackInfoError "LAPACK DGELS workspace query" infoValue)
        )

checkedLapackWorkspaceQuery :: String -> Double -> Either MoonlightError Int
checkedLapackWorkspaceQuery context workspaceQuery
  | not (fieldValueValid workspaceQuery) || workspaceQuery < 0.0 =
      Left (InvariantViolation (context <> " returned an invalid workspace cardinality"))
  | roundedWorkspace > toInteger (maxBound :: Int) =
      Left (InvariantViolation (context <> " exceeds Int storage range"))
  | otherwise =
      Right (max 1 (fromInteger roundedWorkspace))
  where
    roundedWorkspace = ceiling workspaceQuery :: Integer

decodeLeastSquares :: Int -> Ptr CDouble -> Ptr CInt -> IO (Either MoonlightError [Double])
decodeLeastSquares !columnCount rightHandSidePointer infoPointer = do
  infoValue <- peek infoPointer
  if infoValue /= 0
    then pure (Left (lapackLeastSquaresInfoError infoValue))
    else Right . take columnCount . fromCDoubleList <$> peekArray columnCount rightHandSidePointer

solveSelectedTridiagonalLapack :: CInt -> Int -> Int -> Int -> [Double] -> [Double] -> IO (Either MoonlightError [(Double, [Double])])
solveSelectedTridiagonalLapack !lapackSize !matrixSize !lowerIndex !upperIndex diagonalValues offDiagonalValues =
  case
      (,,,)
        <$> checkedProduct "LAPACK DSTEMR eigenvector workspace" matrixSize selectedCount
        <*> checkedProduct "LAPACK DSTEMR support workspace" 2 selectedCount
        <*> checkedProduct "LAPACK DSTEMR floating workspace" 18 matrixSize
        <*> checkedProduct "LAPACK DSTEMR integer workspace" 10 matrixSize
    of
      Left err -> pure (Left err)
      Right (eigenvectorEntryCount, supportCount, workspaceCount, integerWorkspaceCount) -> withLapackChar 'V' $ \jobPointer ->
        withLapackChar 'I' $ \rangePointer ->
          with lapackSize $ \sizePointer ->
            withArray (toCDoubleList diagonalValues) $ \diagonalPointer ->
              withArray (toCDoubleList (offDiagonalValues <> [0.0])) $ \offDiagonalPointer ->
                with 0.0 $ \lowerValuePointer ->
                  with 0.0 $ \upperValuePointer ->
                    with (fromIntegral lowerIndex) $ \lowerIndexPointer ->
                      with (fromIntegral upperIndex) $ \upperIndexPointer ->
                        alloca $ \foundCountPointer ->
                          allocaArray matrixSize $ \eigenvaluePointer ->
                            allocaArray eigenvectorEntryCount $ \eigenvectorPointer ->
                              with lapackSize $ \eigenvectorLeadingDimensionPointer ->
                                with (fromIntegral selectedCount) $ \eigenvectorColumnCountPointer ->
                                  allocaArray supportCount $ \supportPointer ->
                                    with (0 :: CInt) $ \tryRacPointer ->
                                      allocaArray workspaceCount $ \workspacePointer ->
                                        with (fromIntegral workspaceCount) $ \workspaceSizePointer ->
                                          allocaArray integerWorkspaceCount $ \integerWorkspacePointer ->
                                            with (fromIntegral integerWorkspaceCount) $ \integerWorkspaceSizePointer ->
                                              alloca $ \infoPointer -> do
                                                poke foundCountPointer 0
                                                poke infoPointer 0
                                                lapackDstemr
                                                  jobPointer
                                                  rangePointer
                                                  sizePointer
                                                  diagonalPointer
                                                  offDiagonalPointer
                                                  lowerValuePointer
                                                  upperValuePointer
                                                  lowerIndexPointer
                                                  upperIndexPointer
                                                  foundCountPointer
                                                  eigenvaluePointer
                                                  eigenvectorPointer
                                                  eigenvectorLeadingDimensionPointer
                                                  eigenvectorColumnCountPointer
                                                  supportPointer
                                                  tryRacPointer
                                                  workspacePointer
                                                  workspaceSizePointer
                                                  integerWorkspacePointer
                                                  integerWorkspaceSizePointer
                                                  infoPointer
                                                decodeSelectedTridiagonal matrixSize selectedCount eigenvectorEntryCount eigenvaluePointer eigenvectorPointer foundCountPointer infoPointer
  where
    selectedCount = upperIndex - lowerIndex + 1

solveSelectedTridiagonalValuesLapack :: CInt -> Int -> Int -> Int -> [Double] -> [Double] -> IO (Either MoonlightError (U.Vector Double))
solveSelectedTridiagonalValuesLapack !lapackSize !matrixSize !lowerIndex !upperIndex diagonalValues offDiagonalValues =
  case
      (,)
        <$> checkedProduct "LAPACK DSTEMR values floating workspace" 18 matrixSize
        <*> checkedProduct "LAPACK DSTEMR values integer workspace" 10 matrixSize
    of
      Left err -> pure (Left err)
      Right (workspaceCount, integerWorkspaceCount) -> withLapackChar 'N' $ \jobPointer ->
        withLapackChar 'I' $ \rangePointer ->
          with lapackSize $ \sizePointer ->
            withArray (toCDoubleList diagonalValues) $ \diagonalPointer ->
              withArray (toCDoubleList (offDiagonalValues <> [0.0])) $ \offDiagonalPointer ->
                with 0.0 $ \lowerValuePointer ->
                  with 0.0 $ \upperValuePointer ->
                    with (fromIntegral lowerIndex) $ \lowerIndexPointer ->
                      with (fromIntegral upperIndex) $ \upperIndexPointer ->
                        alloca $ \foundCountPointer ->
                          allocaArray matrixSize $ \eigenvaluePointer ->
                            allocaArray 1 $ \eigenvectorPointer ->
                              with (1 :: CInt) $ \eigenvectorLeadingDimensionPointer ->
                                with (1 :: CInt) $ \eigenvectorColumnCountPointer ->
                                  allocaArray 1 $ \supportPointer ->
                                    with (0 :: CInt) $ \tryRacPointer ->
                                      allocaArray workspaceCount $ \workspacePointer ->
                                        with (fromIntegral workspaceCount) $ \workspaceSizePointer ->
                                          allocaArray integerWorkspaceCount $ \integerWorkspacePointer ->
                                            with (fromIntegral integerWorkspaceCount) $ \integerWorkspaceSizePointer ->
                                              alloca $ \infoPointer -> do
                                                poke foundCountPointer 0
                                                poke infoPointer 0
                                                lapackDstemr
                                                  jobPointer
                                                  rangePointer
                                                  sizePointer
                                                  diagonalPointer
                                                  offDiagonalPointer
                                                  lowerValuePointer
                                                  upperValuePointer
                                                  lowerIndexPointer
                                                  upperIndexPointer
                                                  foundCountPointer
                                                  eigenvaluePointer
                                                  eigenvectorPointer
                                                  eigenvectorLeadingDimensionPointer
                                                  eigenvectorColumnCountPointer
                                                  supportPointer
                                                  tryRacPointer
                                                  workspacePointer
                                                  workspaceSizePointer
                                                  integerWorkspacePointer
                                                  integerWorkspaceSizePointer
                                                  infoPointer
                                                decodeSelectedTridiagonalValues selectedCount eigenvaluePointer foundCountPointer infoPointer
  where
    selectedCount = upperIndex - lowerIndex + 1

solveSelectedBandPairsLapack ::
  CInt ->
  CInt ->
  CInt ->
  Int ->
  Int ->
  Int ->
  U.Vector Double ->
  IO (Either MoonlightError [(Double, [Double])])
solveSelectedBandPairsLapack !lapackSize !lapackBandwidth !leadingDimension !matrixSize !lowerIndex !upperIndex lowerBandPayload =
  case
      (,,,)
        <$> checkedProduct "LAPACK selected band orthogonal workspace" matrixSize matrixSize
        <*> checkedProduct "LAPACK selected band eigenvector workspace" matrixSize selectedCount
        <*> checkedProduct "LAPACK DSBEVX floating workspace" 7 matrixSize
        <*> checkedProduct "LAPACK DSBEVX integer workspace" 5 matrixSize
    of
      Left err -> pure (Left err)
      Right (orthogonalEntryCount, eigenvectorEntryCount, workspaceCount, integerWorkspaceCount) -> withLapackChar 'V' $ \jobPointer ->
        withLapackChar 'I' $ \rangePointer ->
          withLapackChar 'L' $ \uploPointer ->
            with lapackSize $ \sizePointer ->
              with lapackBandwidth $ \bandwidthPointer ->
                withArray (toCDoubleList (U.toList lowerBandPayload)) $ \bandPointer ->
                  with leadingDimension $ \leadingDimensionPointer ->
                    allocaArray orthogonalEntryCount $ \orthogonalMatrixPointer ->
                      with lapackSize $ \orthogonalLeadingDimensionPointer ->
                        with 0.0 $ \lowerValuePointer ->
                          with 0.0 $ \upperValuePointer ->
                            with (fromIntegral lowerIndex) $ \lowerIndexPointer ->
                              with (fromIntegral upperIndex) $ \upperIndexPointer ->
                                with 0.0 $ \absoluteTolerancePointer ->
                                  alloca $ \foundCountPointer ->
                                    allocaArray matrixSize $ \eigenvaluePointer ->
                                      allocaArray eigenvectorEntryCount $ \eigenvectorPointer ->
                                        with lapackSize $ \eigenvectorLeadingDimensionPointer ->
                                          allocaArray workspaceCount $ \workspacePointer ->
                                            allocaArray integerWorkspaceCount $ \integerWorkspacePointer ->
                                              allocaArray matrixSize $ \failedVectorPointer ->
                                                alloca $ \infoPointer -> do
                                                  poke foundCountPointer 0
                                                  poke infoPointer 0
                                                  lapackDsbevx
                                                    jobPointer
                                                    rangePointer
                                                    uploPointer
                                                    sizePointer
                                                    bandwidthPointer
                                                    bandPointer
                                                    leadingDimensionPointer
                                                    orthogonalMatrixPointer
                                                    orthogonalLeadingDimensionPointer
                                                    lowerValuePointer
                                                    upperValuePointer
                                                    lowerIndexPointer
                                                    upperIndexPointer
                                                    absoluteTolerancePointer
                                                    foundCountPointer
                                                    eigenvaluePointer
                                                    eigenvectorPointer
                                                    eigenvectorLeadingDimensionPointer
                                                    workspacePointer
                                                    integerWorkspacePointer
                                                    failedVectorPointer
                                                    infoPointer
                                                  decodeSelectedBand matrixSize selectedCount eigenvectorEntryCount eigenvaluePointer eigenvectorPointer foundCountPointer infoPointer
  where
    selectedCount = upperIndex - lowerIndex + 1

solveSelectedBandValuesLapack ::
  CInt ->
  CInt ->
  CInt ->
  Int ->
  Int ->
  Int ->
  U.Vector Double ->
  IO (Either MoonlightError (U.Vector Double))
solveSelectedBandValuesLapack !lapackSize !lapackBandwidth !leadingDimension !matrixSize !lowerIndex !upperIndex lowerBandPayload =
  case
      (,)
        <$> checkedProduct "LAPACK DSBEVX values floating workspace" 7 matrixSize
        <*> checkedProduct "LAPACK DSBEVX values integer workspace" 5 matrixSize
    of
      Left err -> pure (Left err)
      Right (workspaceCount, integerWorkspaceCount) -> withLapackChar 'N' $ \jobPointer ->
        withLapackChar 'I' $ \rangePointer ->
          withLapackChar 'L' $ \uploPointer ->
            with lapackSize $ \sizePointer ->
              with lapackBandwidth $ \bandwidthPointer ->
                withArray (toCDoubleList (U.toList lowerBandPayload)) $ \bandPointer ->
                  with leadingDimension $ \leadingDimensionPointer ->
                    allocaArray 1 $ \orthogonalMatrixPointer ->
                      with (1 :: CInt) $ \orthogonalLeadingDimensionPointer ->
                        with 0.0 $ \lowerValuePointer ->
                          with 0.0 $ \upperValuePointer ->
                            with (fromIntegral lowerIndex) $ \lowerIndexPointer ->
                              with (fromIntegral upperIndex) $ \upperIndexPointer ->
                                with 0.0 $ \absoluteTolerancePointer ->
                                  alloca $ \foundCountPointer ->
                                    allocaArray matrixSize $ \eigenvaluePointer ->
                                      allocaArray 1 $ \eigenvectorPointer ->
                                        with (1 :: CInt) $ \eigenvectorLeadingDimensionPointer ->
                                          allocaArray workspaceCount $ \workspacePointer ->
                                            allocaArray integerWorkspaceCount $ \integerWorkspacePointer ->
                                              allocaArray 1 $ \failedVectorPointer ->
                                                alloca $ \infoPointer -> do
                                                  poke foundCountPointer 0
                                                  poke infoPointer 0
                                                  lapackDsbevx
                                                    jobPointer
                                                    rangePointer
                                                    uploPointer
                                                    sizePointer
                                                    bandwidthPointer
                                                    bandPointer
                                                    leadingDimensionPointer
                                                    orthogonalMatrixPointer
                                                    orthogonalLeadingDimensionPointer
                                                    lowerValuePointer
                                                    upperValuePointer
                                                    lowerIndexPointer
                                                    upperIndexPointer
                                                    absoluteTolerancePointer
                                                    foundCountPointer
                                                    eigenvaluePointer
                                                    eigenvectorPointer
                                                    eigenvectorLeadingDimensionPointer
                                                    workspacePointer
                                                    integerWorkspacePointer
                                                    failedVectorPointer
                                                    infoPointer
                                                  decodeSelectedBandValues selectedCount eigenvaluePointer foundCountPointer infoPointer
  where
    selectedCount = upperIndex - lowerIndex + 1

decodeSelectedTridiagonal ::
  Int ->
  Int ->
  Int ->
  Ptr CDouble ->
  Ptr CDouble ->
  Ptr CInt ->
  Ptr CInt ->
  IO (Either MoonlightError [(Double, [Double])])
decodeSelectedTridiagonal !matrixSize !selectedCount !eigenvectorEntryCount eigenvaluePointer eigenvectorPointer foundCountPointer infoPointer = do
  infoValue <- peek infoPointer
  foundCount <- fromIntegral <$> peek foundCountPointer
  if infoValue /= 0
    then pure (Left (lapackInfoError "LAPACK DSTEMR" infoValue))
    else
      if foundCount /= selectedCount
        then pure (Left (InvariantViolation ("LAPACK DSTEMR returned " <> show foundCount <> " eigenpairs; expected " <> show selectedCount)))
        else do
          eigenvalues <- fromCDoubleList <$> peekArray selectedCount eigenvaluePointer
          eigenvectorPayload <- fromCDoubleList <$> peekArray eigenvectorEntryCount eigenvectorPointer
          pure
            ( do
                eigenvectors <- take selectedCount <$> chunkRows matrixSize eigenvectorPayload
                Right (zip eigenvalues eigenvectors)
            )

decodeSelectedTridiagonalValues ::
  Int ->
  Ptr CDouble ->
  Ptr CInt ->
  Ptr CInt ->
  IO (Either MoonlightError (U.Vector Double))
decodeSelectedTridiagonalValues !selectedCount eigenvaluePointer foundCountPointer infoPointer = do
  infoValue <- peek infoPointer
  foundCount <- fromIntegral <$> peek foundCountPointer
  if infoValue /= 0
    then pure (Left (lapackInfoError "LAPACK DSTEMR" infoValue))
    else
      if foundCount /= selectedCount
        then pure (Left (InvariantViolation ("LAPACK DSTEMR returned " <> show foundCount <> " eigenvalues; expected " <> show selectedCount)))
        else Right . U.fromList . fromCDoubleList <$> peekArray selectedCount eigenvaluePointer

decodeSelectedSymmetricColumns ::
  Int ->
  Int ->
  Ptr CDouble ->
  Ptr CDouble ->
  Ptr CInt ->
  Ptr CInt ->
  IO
    ( Either
        MoonlightError
        (U.Vector Double, U.Vector Double)
    )
decodeSelectedSymmetricColumns
  !selectedCount
  !eigenvectorEntryCount
  eigenvaluePointer
  eigenvectorPointer
  foundCountPointer
  infoPointer = do
    infoValue <- peek infoPointer
    foundCount <- fromIntegral <$> peek foundCountPointer
    if infoValue /= 0
      then pure (Left (lapackInfoError "LAPACK DSYEVX" infoValue))
      else
        if foundCount /= selectedCount
          then
            pure
              ( Left
                  ( InvariantViolation
                      ( "LAPACK DSYEVX returned "
                          <> show foundCount
                          <> " eigenpairs; expected "
                          <> show selectedCount
                      )
                  )
              )
          else do
            eigenvalues <-
              peekCDoubleVectorSlice
                0
                selectedCount
                eigenvaluePointer
            eigenvectors <-
              peekCDoubleVectorSlice
                0
                eigenvectorEntryCount
                eigenvectorPointer
            pure (Right (eigenvalues, eigenvectors))

decodeSelectedSymmetricValues ::
  Int ->
  Ptr CDouble ->
  Ptr CInt ->
  Ptr CInt ->
  IO (Either MoonlightError (U.Vector Double))
decodeSelectedSymmetricValues
  !selectedCount
  eigenvaluePointer
  foundCountPointer
  infoPointer = do
    infoValue <- peek infoPointer
    foundCount <- fromIntegral <$> peek foundCountPointer
    if infoValue /= 0
      then pure (Left (lapackInfoError "LAPACK DSYEVX" infoValue))
      else
        if foundCount /= selectedCount
          then
            pure
              ( Left
                  ( InvariantViolation
                      ( "LAPACK DSYEVX returned "
                          <> show foundCount
                          <> " eigenvalues; expected "
                          <> show selectedCount
                      )
                  )
              )
          else
            Right
              <$> peekCDoubleVectorSlice
                0
                selectedCount
                eigenvaluePointer

peekCDoubleVectorSlice ::
  Int ->
  Int ->
  Ptr CDouble ->
  IO (U.Vector Double)
peekCDoubleVectorSlice !sourceOffset !elementCount sourcePointer =
  U.generateM elementCount $ \entryIndex -> do
    CDouble entryValue <-
      peekElemOff
        sourcePointer
        (sourceOffset + entryIndex)
    pure entryValue
{-# INLINE peekCDoubleVectorSlice #-}

checkedProduct ::
  String ->
  Int ->
  Int ->
  Either MoonlightError Int
checkedProduct context leftCount rightCount =
  first
    (const (InvariantViolation (context <> " exceeds non-negative Int storage range")))
    (checkedNonNegativeProduct leftCount rightCount)

checkedSum :: String -> Int -> Int -> Either MoonlightError Int
checkedSum context leftCount rightCount =
  first
    (const (InvariantViolation (context <> " exceeds non-negative Int storage range")))
    (checkedNonNegativeSum leftCount rightCount)

decodeSelectedBand ::
  Int ->
  Int ->
  Int ->
  Ptr CDouble ->
  Ptr CDouble ->
  Ptr CInt ->
  Ptr CInt ->
  IO (Either MoonlightError [(Double, [Double])])
decodeSelectedBand !matrixSize !selectedCount !eigenvectorEntryCount eigenvaluePointer eigenvectorPointer foundCountPointer infoPointer = do
  infoValue <- peek infoPointer
  foundCount <- fromIntegral <$> peek foundCountPointer
  if infoValue /= 0
    then pure (Left (lapackInfoError "LAPACK DSBEVX" infoValue))
    else
      if foundCount /= selectedCount
        then pure (Left (InvariantViolation ("LAPACK DSBEVX returned " <> show foundCount <> " eigenpairs; expected " <> show selectedCount)))
        else do
          eigenvalues <- fromCDoubleList <$> peekArray selectedCount eigenvaluePointer
          eigenvectorPayload <- fromCDoubleList <$> peekArray eigenvectorEntryCount eigenvectorPointer
          pure
            ( do
                eigenvectors <- take selectedCount <$> chunkRows matrixSize eigenvectorPayload
                Right (zip eigenvalues eigenvectors)
            )

decodeSelectedBandValues ::
  Int ->
  Ptr CDouble ->
  Ptr CInt ->
  Ptr CInt ->
  IO (Either MoonlightError (U.Vector Double))
decodeSelectedBandValues !selectedCount eigenvaluePointer foundCountPointer infoPointer = do
  infoValue <- peek infoPointer
  foundCount <- fromIntegral <$> peek foundCountPointer
  if infoValue /= 0
    then pure (Left (lapackInfoError "LAPACK DSBEVX" infoValue))
    else
      if foundCount /= selectedCount
        then pure (Left (InvariantViolation ("LAPACK DSBEVX returned " <> show foundCount <> " eigenvalues; expected " <> show selectedCount)))
        else Right . U.fromList . fromCDoubleList <$> peekArray selectedCount eigenvaluePointer

withLapackChar :: Char -> (Ptr CChar -> IO value) -> IO value
withLapackChar charValue onPointer =
  with (castCharToCChar charValue) onPointer

toColumnMajor :: [[Double]] -> [CDouble]
toColumnMajor =
  toCDoubleList . concat . transpose

toCDoubleList :: [Double] -> [CDouble]
toCDoubleList =
  fmap CDouble

fromCDoubleList :: [CDouble] -> [Double]
fromCDoubleList =
  fmap realToFrac

matrixSizeAsLapackInt :: Int -> Either MoonlightError CInt
matrixSizeAsLapackInt matrixSize
  | matrixSize < 0 =
      Left (InvariantViolation "LAPACK matrix size must be non-negative")
  | matrixSize > fromIntegral (maxBound :: CInt) =
      Left (InvariantViolation "LAPACK matrix size exceeds CInt range")
  | otherwise = Right (fromIntegral matrixSize)

lapackInfoError :: String -> CInt -> MoonlightError
lapackInfoError context infoValue
  | infoValue < 0 = InvariantViolation (context <> " rejected argument " <> show (negate infoValue))
  | otherwise = InvariantViolation (context <> " failed to converge; info=" <> show infoValue)

lapackLeastSquaresInfoError :: CInt -> MoonlightError
lapackLeastSquaresInfoError infoValue
  | infoValue < 0 = InvariantViolation ("LAPACK DGELS rejected argument " <> show (negate infoValue))
  | otherwise = InvariantViolation ("LAPACK DGELS detected exact rank deficiency at triangular factor diagonal " <> show infoValue)

lapackLinearSolveInfoError :: CInt -> MoonlightError
lapackLinearSolveInfoError infoValue
  | infoValue < 0 = InvariantViolation ("LAPACK DGESV rejected argument " <> show (negate infoValue))
  | otherwise = InvariantViolation ("LAPACK DGESV detected exact singularity at U diagonal " <> show infoValue)

leastSquaresRightHandSidePayload :: Int -> Int -> [Double] -> [CDouble]
leastSquaresRightHandSidePayload rowCount columnCount rightHandSide =
  toCDoubleList (rightHandSide <> replicate (max 0 (columnCount - rowCount)) 0.0)

validateLeastSquaresInput :: Int -> Int -> [[Double]] -> [Double] -> Either MoonlightError ()
validateLeastSquaresInput rowCount columnCount matrixToRows rightHandSide
  | rowCount < 0 || columnCount < 0 =
      Left (InvariantViolation "LAPACK least-squares dimensions must be non-negative")
  | rowCount == 0 || columnCount == 0 =
      Left (InvariantViolation "LAPACK least-squares requires positive dimensions")
  | length matrixToRows /= rowCount =
      Left (InvariantViolation "LAPACK least-squares row count mismatch")
  | any ((/= columnCount) . length) matrixToRows =
      Left (InvariantViolation "LAPACK least-squares requires rectangular rows")
  | length rightHandSide /= rowCount =
      Left (InvariantViolation "LAPACK least-squares RHS length mismatch")
  | not (all fieldValueValid (concat matrixToRows <> rightHandSide)) =
      Left (InvariantViolation "LAPACK least-squares requires finite entries")
  | otherwise = Right ()

validateSelectedTridiagonalInput :: Int -> Int -> [Double] -> [Double] -> Either MoonlightError Int
validateSelectedTridiagonalInput lowerIndex upperIndex diagonalValues offDiagonalValues
  | matrixSize <= 0 =
      Left (InvariantViolation "LAPACK selected tridiagonal eigensolve requires a positive dimension")
  | length offDiagonalValues /= matrixSize - 1 =
      Left (InvariantViolation "LAPACK selected tridiagonal eigensolve off-diagonal length mismatch")
  | lowerIndex < 1 || upperIndex < lowerIndex || upperIndex > matrixSize =
      Left (InvariantViolation "LAPACK selected tridiagonal eigensolve index range is out of bounds")
  | not (all fieldValueValid (diagonalValues <> offDiagonalValues)) =
      Left (InvariantViolation "LAPACK selected tridiagonal eigensolve requires finite entries")
  | otherwise = Right matrixSize
  where
    matrixSize = length diagonalValues

validateSelectedBandInput :: Int -> Int -> Int -> Int -> U.Vector Double -> Either MoonlightError Int
validateSelectedBandInput lowerIndex upperIndex matrixSize bandwidth lowerBandPayload = do
  if matrixSize <= 0
    then Left (InvariantViolation "LAPACK selected symmetric-band eigensolve requires a positive dimension")
    else Right ()
  if bandwidth < 0
    then Left (InvariantViolation "LAPACK selected symmetric-band eigensolve bandwidth must be non-negative")
    else Right ()
  if bandwidth >= matrixSize
    then Left (InvariantViolation "LAPACK selected symmetric-band eigensolve bandwidth must be smaller than dimension")
    else Right ()
  leadingDimension <- checkedSum "LAPACK selected symmetric-band leading dimension" bandwidth 1
  expectedPayloadLength <- checkedProduct "LAPACK selected symmetric-band payload" leadingDimension matrixSize
  if U.length lowerBandPayload /= expectedPayloadLength
    then
      Left
        ( InvariantViolation
            ( "LAPACK selected symmetric-band payload length mismatch: expected "
                <> show expectedPayloadLength
                <> " but received "
                <> show (U.length lowerBandPayload)
            )
        )
    else Right ()
  if lowerIndex < 1 || upperIndex < lowerIndex || upperIndex > matrixSize
    then Left (InvariantViolation "LAPACK selected symmetric-band eigensolve index range is out of bounds")
    else Right ()
  if U.any (not . fieldValueValid) lowerBandPayload
    then Left (InvariantViolation "LAPACK selected symmetric-band eigensolve requires finite entries")
    else Right leadingDimension
