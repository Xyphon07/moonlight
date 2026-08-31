{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}

-- | Native spectral dispatch over LAPACK result sections.
module Moonlight.LinAlg.Effect.Native.Dispatch
  ( denseDoubleLinearSolveLapack,
    denseDoubleMatrixProductBlas,
    denseDoubleSymmetricEigenpairsLapack,
    leastSquaresLapack,
    symmetricEigenRequestLapack,
    selectedSymmetricTridiagonalEigenRequestLapack,
    selectedSymmetricBlockTridiagonalEigenRequestLapack,
  )
where

import Data.Bifunctor (first)
import Data.Vector.Storable qualified as S
import Data.Vector.Unboxed qualified as U
import Moonlight.Core
  ( MoonlightError (..),
    checkedNonNegativeProduct,
  )
import Moonlight.LinAlg.Effect.Native.LAPACK
  ( denseDoubleLinearSolveLapack,
    denseDoubleMatrixProductBlas,
    denseDoubleSymmetricEigenpairsRawLapack,
    leastSquaresLapack,
    selectedSymmetricEigenPairsLapack,
    selectedSymmetricEigenValuesLapack,
    selectedSymmetricBlockTridiagonalEigenPairsLapack,
    selectedSymmetricBlockTridiagonalEigenValuesLapack,
    selectedSymmetricTridiagonalEigenPairsLapack,
    selectedSymmetricTridiagonalEigenValuesLapack,
  )
import Moonlight.LinAlg.Internal.Eigen.Kernels (epsDouble, finiteDouble)
import Moonlight.LinAlg.Internal.Eigen.Symmetric
  ( CertifiedSymmetricEigenResult (..),
    SymmetricEigenCertificationFailure,
    SymmetricEigenResult (..),
    certifySymmetricEigenResult,
  )
import Moonlight.LinAlg.Internal.VectorOps (normU, scaleU, subU)
import Moonlight.LinAlg.Pure.Dense.Dynamic
  ( DynMatrix,
    dynMatrixShape,
    dynMatrixToList,
  )
import Moonlight.LinAlg.Pure.Dense.Flat
  ( DenseDoubleMatrix,
    denseDoubleMatrixShape,
    denseDoubleMatrixToRowMajorVector,
    trustedDenseDoubleMatrixRowMajor,
  )
import Moonlight.LinAlg.Pure.Krylov.Config (positiveCountValue)
import Moonlight.LinAlg.Pure.Krylov.Selection
  ( SpectrumEnd (..),
    sortRawPairsForSpectrum,
  )
import Moonlight.LinAlg.Pure.Spectral.Request (EigenRequest (..))
import Moonlight.LinAlg.Pure.Spectral.Result
  ( Eigenpairs,
    mkEigenpairs,
  )
import Moonlight.LinAlg.Pure.Structured.BlockTridiagonal
  ( SymmetricBlockTridiagonal,
    applySymmetricBlockTridiagonalU,
    symmetricBlockTridiagonalDimension,
    symmetricBlockTridiagonalFrobeniusNorm,
  )
import Moonlight.LinAlg.Pure.Structured.Tridiagonal
  ( SymmetricTridiagonal,
    symmetricTridiagonalDiagonalEntries,
    symmetricTridiagonalDimension,
    symmetricTridiagonalOffDiagonalEntries,
  )
import Prelude

symmetricEigenRequestLapack ::
  EigenRequest result ->
  DynMatrix Double ->
  IO (Either MoonlightError result)
symmetricEigenRequestLapack requestValue matrixValue =
  case requestValue of
    EigenvaluesRequest spectrumEnd countValue ->
      fmap (fmap (orderNativeValues spectrumEnd)) $
        selectedSymmetricEigenValuesLapack spectrumEnd (positiveCountValue countValue) matrixValue
    EigenpairsRequest spectrumEnd countValue ->
      fmap (>>= denseEigenpairsFromRawColumns spectrumEnd matrixValue) $
        selectedSymmetricEigenPairsLapack spectrumEnd (positiveCountValue countValue) matrixValue

selectedSymmetricTridiagonalEigenRequestLapack ::
  EigenRequest result ->
  SymmetricTridiagonal ->
  IO (Either MoonlightError result)
selectedSymmetricTridiagonalEigenRequestLapack requestValue tridiagonalValue =
  case requestValue of
    EigenvaluesRequest spectrumEnd countValue ->
      fmap (fmap (orderNativeValues spectrumEnd)) $
        selectedSymmetricTridiagonalEigenValuesLapack spectrumEnd (positiveCountValue countValue) tridiagonalValue
    EigenpairsRequest spectrumEnd countValue ->
      fmap (>>= tridiagonalEigenpairsFromRawPairs spectrumEnd tridiagonalValue) $
        selectedSymmetricTridiagonalEigenPairsLapack spectrumEnd (positiveCountValue countValue) tridiagonalValue

selectedSymmetricBlockTridiagonalEigenRequestLapack ::
  EigenRequest result ->
  SymmetricBlockTridiagonal ->
  IO (Either MoonlightError result)
selectedSymmetricBlockTridiagonalEigenRequestLapack requestValue blockValue =
  case requestValue of
    EigenvaluesRequest spectrumEnd countValue ->
      fmap (fmap (orderNativeValues spectrumEnd)) $
        selectedSymmetricBlockTridiagonalEigenValuesLapack spectrumEnd (positiveCountValue countValue) blockValue
    EigenpairsRequest spectrumEnd countValue ->
      fmap (>>= blockTridiagonalEigenpairsFromRawPairs spectrumEnd blockValue) $
        selectedSymmetricBlockTridiagonalEigenPairsLapack spectrumEnd (positiveCountValue countValue) blockValue

denseDoubleSymmetricEigenpairsLapack :: DenseDoubleMatrix -> IO (Either MoonlightError Eigenpairs)
denseDoubleSymmetricEigenpairsLapack matrixValue =
  fmap (>>= denseDoubleEigenpairsFromRawColumns matrixValue) $
    denseDoubleSymmetricEigenpairsRawLapack matrixValue

denseDoubleEigenpairsFromRawColumns ::
  DenseDoubleMatrix ->
  (S.Vector Double, S.Vector Double) ->
  Either MoonlightError Eigenpairs
denseDoubleEigenpairsFromRawColumns matrixValue rawColumns = do
  certifiedResult <- denseCertifiedEigenResultFromRawColumns matrixValue rawColumns
  let resultValue = certifiedSymmetricEigenResult certifiedResult
      !dimension = fst (denseDoubleMatrixShape matrixValue)
      !eigenvalues =
        storableVectorToUnboxed
          (symmetricEigenResultValues resultValue)
      !eigenvectors =
        denseEigenvectorsColumnMajorUnboxed
          dimension
          (symmetricEigenResultVectors resultValue)
      !residuals =
        denseCertifiedPairResiduals
          dimension
          matrixValue
          eigenvalues
          eigenvectors
  mkEigenpairs
    dimension
    eigenvalues
    eigenvectors
    residuals

denseCertifiedEigenResultFromRawColumns ::
  DenseDoubleMatrix ->
  (S.Vector Double, S.Vector Double) ->
  Either MoonlightError CertifiedSymmetricEigenResult
denseCertifiedEigenResultFromRawColumns matrixValue (rawEigenvalues, rawEigenvectors) = do
  let (rowCount, columnCount) = denseDoubleMatrixShape matrixValue
  if rowCount /= columnCount
    then Left (InvariantViolation "native dense Double eigenpairs require a square matrix")
    else do
      expectedVectorCount <-
        checkedColumnPayloadLength
          rowCount
          (S.length rawEigenvalues)
      if S.length rawEigenvectors /= expectedVectorCount
        then
          Left
            ( InvariantViolation
                ( "native dense Double eigenvector payload mismatch: expected "
                    <> show expectedVectorCount
                    <> " entries but received "
                    <> show (S.length rawEigenvectors)
                )
            )
        else
          case certifySymmetricEigenResult matrixValue (rawSymmetricEigenResult rowCount rawEigenvalues rawEigenvectors) of
            Left failureValue ->
              Left
                ( nativeCertificationFailure
                    "native dense Double symmetric eigensolve"
                    failureValue
                )
            Right certifiedResult ->
              Right certifiedResult

rawSymmetricEigenResult ::
  Int ->
  S.Vector Double ->
  S.Vector Double ->
  SymmetricEigenResult
rawSymmetricEigenResult dimension eigenvalues eigenvectors =
  SymmetricEigenResult
    { symmetricEigenResultValues = eigenvalues,
      symmetricEigenResultVectors =
        trustedDenseDoubleMatrixRowMajor
          dimension
          dimension
          (lapackColumnMajorEigenvectorsToRowMajor dimension eigenvectors)
    }

lapackColumnMajorEigenvectorsToRowMajor ::
  Int ->
  S.Vector Double ->
  S.Vector Double
lapackColumnMajorEigenvectorsToRowMajor dimension eigenvectors =
  S.generate
    (S.length eigenvectors)
    ( \payloadIndex ->
        let (!rowIndex, !columnIndex) = payloadIndex `quotRem` dimension
         in eigenvectors `S.unsafeIndex` (columnIndex * dimension + rowIndex)
    )

denseEigenvectorsColumnMajorUnboxed ::
  Int ->
  DenseDoubleMatrix ->
  U.Vector Double
denseEigenvectorsColumnMajorUnboxed dimension eigenvectors =
  U.generate
    (S.length eigenvectorPayload)
    ( \payloadIndex ->
        let (!columnIndex, !rowIndex) = payloadIndex `quotRem` dimension
         in eigenvectorPayload `S.unsafeIndex` (rowIndex * dimension + columnIndex)
    )
  where
    eigenvectorPayload = denseDoubleMatrixToRowMajorVector eigenvectors

denseCertifiedPairResiduals ::
  Int ->
  DenseDoubleMatrix ->
  U.Vector Double ->
  U.Vector Double ->
  U.Vector Double
denseCertifiedPairResiduals dimension matrixValue eigenvalues eigenvectors =
  let !matrixPayload =
        storableVectorToUnboxed
          (denseDoubleMatrixToRowMajorVector matrixValue)
   in U.generate
        (U.length eigenvalues)
        ( denseResidualNormAt
            dimension
            matrixPayload
            eigenvalues
            eigenvectors
        )

nativeCertificationFailure ::
  String ->
  SymmetricEigenCertificationFailure ->
  MoonlightError
nativeCertificationFailure context failureValue =
  InvariantViolation
    ( context
        <> " certification failed: "
        <> show failureValue
    )

denseEigenpairsFromRawColumns ::
  SpectrumEnd ->
  DynMatrix Double ->
  (U.Vector Double, U.Vector Double) ->
  Either MoonlightError Eigenpairs
denseEigenpairsFromRawColumns
  spectrumEnd
  matrixValue
  rawColumns = do
    let !dimension = dynMatrixDimension matrixValue
        !matrixPayload = U.fromList (dynMatrixToList matrixValue)
    (eigenvalues, eigenvectors) <-
      orderNativeColumns
        spectrumEnd
        dimension
        rawColumns
    let !pairCount = U.length eigenvalues
        !residuals =
          U.generate
            pairCount
            ( denseResidualNormAt
                dimension
                matrixPayload
                eigenvalues
                eigenvectors
            )
    validateNativeResidualNorms
      "native dense symmetric eigensolve"
      dimension
      (frobeniusNormU matrixPayload)
      residuals
    mkEigenpairs
      dimension
      eigenvalues
      eigenvectors
      residuals

orderNativeColumns ::
  SpectrumEnd ->
  Int ->
  (U.Vector Double, U.Vector Double) ->
  Either
    MoonlightError
    (U.Vector Double, U.Vector Double)
orderNativeColumns spectrumEnd dimension (eigenvalues, eigenvectors) = do
  expectedVectorCount <-
    checkedColumnPayloadLength
      dimension
      (U.length eigenvalues)
  if U.length eigenvectors /= expectedVectorCount
    then
      Left
        ( InvariantViolation
            ( "native eigenvector payload mismatch: expected "
                <> show expectedVectorCount
                <> " entries but received "
                <> show (U.length eigenvectors)
            )
        )
    else
      case spectrumEnd of
        SmallestEigenvalues -> Right (eigenvalues, eigenvectors)
        LargestEigenvalues ->
          Right
            ( U.reverse eigenvalues,
              reverseEigenvectorColumns
                dimension
                (U.length eigenvalues)
                eigenvectors
            )

checkedColumnPayloadLength ::
  Int ->
  Int ->
  Either MoonlightError Int
checkedColumnPayloadLength dimension columnCount
  | dimension <= 0 =
      Left
        ( InvariantViolation
            "native eigenpairs require a positive dimension"
        )
  | columnCount < 0 =
      Left
        ( InvariantViolation
            "native eigenpair count cannot be negative"
        )
  | otherwise =
      first
        (const (InvariantViolation "native eigenvector payload exceeds Int range"))
        (checkedNonNegativeProduct dimension columnCount)

reverseEigenvectorColumns ::
  Int ->
  Int ->
  U.Vector Double ->
  U.Vector Double
reverseEigenvectorColumns dimension columnCount eigenvectors =
  U.generate
    (U.length eigenvectors)
    ( \payloadIndex ->
        let (!targetColumn, !rowIndex) =
              payloadIndex `quotRem` dimension
            !sourceColumn = columnCount - targetColumn - 1
         in eigenvectors
              `U.unsafeIndex`
                (sourceColumn * dimension + rowIndex)
    )

storableVectorToUnboxed :: S.Vector Double -> U.Vector Double
storableVectorToUnboxed values =
  U.generate (S.length values) (values `S.unsafeIndex`)
{-# INLINE storableVectorToUnboxed #-}

tridiagonalEigenpairsFromRawPairs ::
  SpectrumEnd ->
  SymmetricTridiagonal ->
  [(Double, [Double])] ->
  Either MoonlightError Eigenpairs
tridiagonalEigenpairsFromRawPairs spectrumEnd tridiagonalValue rawPairs =
  let sortedPairs = sortRawPairsForSpectrum spectrumEnd rawPairs
      dimension = symmetricTridiagonalDimension tridiagonalValue
      eigenvalues = U.fromList (fst <$> sortedPairs)
      eigenvectors = U.fromList (sortedPairs >>= snd)
      diagonalValues = U.fromList (symmetricTridiagonalDiagonalEntries tridiagonalValue)
      offDiagonalValues = U.fromList (symmetricTridiagonalOffDiagonalEntries tridiagonalValue)
      residuals =
        U.generate
          (length sortedPairs)
          (tridiagonalResidualNormAt dimension diagonalValues offDiagonalValues eigenvalues eigenvectors)
   in validateNativeResidualNorms "native tridiagonal eigensolve" dimension (tridiagonalFrobeniusNorm diagonalValues offDiagonalValues) residuals
        *> mkEigenpairs dimension eigenvalues eigenvectors residuals

blockTridiagonalEigenpairsFromRawPairs ::
  SpectrumEnd ->
  SymmetricBlockTridiagonal ->
  [(Double, [Double])] ->
  Either MoonlightError Eigenpairs
blockTridiagonalEigenpairsFromRawPairs spectrumEnd blockValue rawPairs = do
  let sortedPairs = sortRawPairsForSpectrum spectrumEnd rawPairs
      dimension = symmetricBlockTridiagonalDimension blockValue
      eigenvalues = U.fromList (fst <$> sortedPairs)
      eigenvectors = U.fromList (sortedPairs >>= snd)
  residuals <- U.fromList <$> traverse (blockResidualNorm blockValue) sortedPairs
  let matrixNorm = symmetricBlockTridiagonalFrobeniusNorm blockValue
  validateNativeResidualNorms "native symmetric-band eigensolve" dimension matrixNorm residuals
  mkEigenpairs dimension eigenvalues eigenvectors residuals

orderNativeValues :: SpectrumEnd -> U.Vector Double -> U.Vector Double
orderNativeValues spectrumEnd values =
  case spectrumEnd of
    SmallestEigenvalues -> values
    LargestEigenvalues -> U.reverse values

denseResidualNormAt ::
  Int ->
  U.Vector Double ->
  U.Vector Double ->
  U.Vector Double ->
  Int ->
  Double
denseResidualNormAt
  dimension
  matrixPayload
  eigenvalues
  eigenvectors
  columnIndex =
    sqrt (rowLoop 0 0.0)
  where
    !eigenvalue = eigenvalues `U.unsafeIndex` columnIndex
    !vectorStart = columnIndex * dimension

    rowLoop !rowIndex !sumSquares
      | rowIndex >= dimension = sumSquares
      | otherwise =
          let !imageEntry = matrixRowDot rowIndex 0 0.0
              !vectorEntry =
                eigenvectors
                  `U.unsafeIndex`
                    (vectorStart + rowIndex)
              !residualEntry =
                imageEntry - eigenvalue * vectorEntry
           in rowLoop
                (rowIndex + 1)
                (sumSquares + residualEntry * residualEntry)

    matrixRowDot !rowIndex !columnIndexValue !accumulator
      | columnIndexValue >= dimension = accumulator
      | otherwise =
          let !matrixEntry =
                matrixPayload
                  `U.unsafeIndex`
                    (rowIndex * dimension + columnIndexValue)
              !vectorEntry =
                eigenvectors
                  `U.unsafeIndex`
                    (vectorStart + columnIndexValue)
           in matrixRowDot
                rowIndex
                (columnIndexValue + 1)
                (accumulator + matrixEntry * vectorEntry)

tridiagonalResidualNormAt ::
  Int ->
  U.Vector Double ->
  U.Vector Double ->
  U.Vector Double ->
  U.Vector Double ->
  Int ->
  Double
tridiagonalResidualNormAt dimension diagonalValues offDiagonalValues eigenvalues eigenvectors columnIndex =
  sqrt (U.sum (U.generate dimension entryResidualSquare))
  where
    eigenvalue = eigenvalues `U.unsafeIndex` columnIndex
    vectorStart = columnIndex * dimension

    vectorEntry rowIndex =
      eigenvectors `U.unsafeIndex` (vectorStart + rowIndex)

    offDiagonalEntry rowIndex =
      offDiagonalValues `U.unsafeIndex` rowIndex

    entryResidualSquare rowIndex =
      let mainEntry = diagonalValues `U.unsafeIndex` rowIndex
          currentValue = vectorEntry rowIndex
          lowerContribution =
            if rowIndex <= 0
              then 0.0
              else offDiagonalEntry (rowIndex - 1) * vectorEntry (rowIndex - 1)
          upperContribution =
            if rowIndex + 1 >= dimension
              then 0.0
              else offDiagonalEntry rowIndex * vectorEntry (rowIndex + 1)
          residualValue =
            lowerContribution
              + mainEntry * currentValue
              + upperContribution
              - eigenvalue * currentValue
       in residualValue * residualValue

blockResidualNorm :: SymmetricBlockTridiagonal -> (Double, [Double]) -> Either MoonlightError Double
blockResidualNorm blockValue (eigenvalue, eigenvector) = do
  let vectorValue = U.fromList eigenvector
  imageVector <- applySymmetricBlockTridiagonalU blockValue vectorValue
  residualVector <- subU imageVector (scaleU eigenvalue vectorValue)
  pure (normU residualVector)

dynMatrixDimension :: DynMatrix Double -> Int
dynMatrixDimension matrixValue =
  case dynMatrixShape matrixValue of
    (rowCount, _) -> rowCount

validateNativeResidualNorms :: String -> Int -> Double -> U.Vector Double -> Either MoonlightError ()
validateNativeResidualNorms context dimension matrixNorm residuals =
  let residualLimit =
        1.0e7
          * max 1.0 matrixNorm
          * max 1.0 (fromIntegral dimension)
          * epsDouble
      accepted residualValue =
        finiteDouble residualValue && residualValue <= residualLimit
   in if U.all accepted residuals
        then Right ()
        else
          Left
            ( InvariantViolation
                ( context
                    <> " residual exceeded tolerance; limit="
                    <> show residualLimit
                    <> ", residuals="
                    <> show (U.toList residuals)
                )
            )

frobeniusNormU :: U.Vector Double -> Double
frobeniusNormU values =
  sqrt
    ( U.foldl'
        (\accumulator entryValue -> accumulator + entryValue * entryValue)
        0.0
        values
    )

tridiagonalFrobeniusNorm :: U.Vector Double -> U.Vector Double -> Double
tridiagonalFrobeniusNorm diagonalValues offDiagonalValues =
  sqrt
    ( U.sum (U.map (\entryValue -> entryValue * entryValue) diagonalValues)
        + 2.0 * U.sum (U.map (\entryValue -> entryValue * entryValue) offDiagonalValues)
    )
