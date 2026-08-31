{-# LANGUAGE StrictData #-}

module Moonlight.LinAlg.Pure.Spectral.Result
  ( Eigenpairs,
    CertifiedSelectedEigenpairResult (..),
    SelectedEigenpairCertificationFailure (..),
    SelectedEigenpairOrthonormalityEvidence (..),
    SelectedEigenpairRequestOrderingEvidence (..),
    SelectedEigenpairResidualEvidence (..),
    mkEigenpairs,
    certifySelectedEigenpairResult,
    eigenpairDimension,
    eigenpairValues,
    eigenpairVectorsColumnMajor,
    eigenpairResidualNorms,
    eigenpairCount,
    eigenpairVectorAt,
    eigenpairsFromColumns,
    mapEigenpairValues,
  )
where

import Data.Bifunctor (first)
import Data.Kind (Type)
import Data.Vector.Unboxed qualified as U
import Moonlight.Core
  ( MoonlightError (..),
    checkedNonNegativeProduct,
    fieldValueValid,
  )
import Moonlight.LinAlg.Pure.Krylov.Selection (SpectrumEnd (..))
import Prelude

type Eigenpairs :: Type
data Eigenpairs = Eigenpairs
  { eigenpairDimension :: !Int,
    eigenpairValues :: !(U.Vector Double),
    eigenpairVectorsColumnMajor :: !(U.Vector Double),
    eigenpairResidualNorms :: !(U.Vector Double)
  }
  deriving stock (Eq, Show)

type SelectedEigenpairResidualEvidence :: Type
data SelectedEigenpairResidualEvidence = SelectedEigenpairResidualEvidence
  { selectedEigenpairResidualBound :: !Double,
    selectedEigenpairMaxResidualNorm :: !Double
  }
  deriving stock (Eq, Show)

type SelectedEigenpairOrthonormalityEvidence :: Type
data SelectedEigenpairOrthonormalityEvidence = SelectedEigenpairOrthonormalityEvidence
  { selectedEigenpairOrthonormalityBound :: !Double,
    selectedEigenpairMaxOrthonormalityDeviation :: !Double
  }
  deriving stock (Eq, Show)

type SelectedEigenpairRequestOrderingEvidence :: Type
data SelectedEigenpairRequestOrderingEvidence = SelectedEigenpairRequestOrderingEvidence
  { selectedEigenpairRequestedCount :: !Int,
    selectedEigenpairCertifiedCount :: !Int,
    selectedEigenpairCertifiedOrdering :: !SpectrumEnd
  }
  deriving stock (Eq, Show)

type CertifiedSelectedEigenpairResult :: Type
data CertifiedSelectedEigenpairResult = CertifiedSelectedEigenpairResult
  { certifiedSelectedEigenpairResult :: !Eigenpairs,
    certifiedSelectedEigenpairResidualEvidence :: !SelectedEigenpairResidualEvidence,
    certifiedSelectedEigenpairOrthonormalityEvidence :: !SelectedEigenpairOrthonormalityEvidence,
    certifiedSelectedEigenpairRequestOrderingEvidence :: !SelectedEigenpairRequestOrderingEvidence
  }
  deriving stock (Eq, Show)

type SelectedEigenpairCertificationFailure :: Type
data SelectedEigenpairCertificationFailure
  = SelectedEigenpairCertificationInvalidRequest !String
  | SelectedEigenpairCertificationRequestedCountMismatch !Int !Int
  | SelectedEigenpairCertificationResidualExceeded !Int !Double !Double
  | SelectedEigenpairCertificationOrthonormalityExceeded !Int !Int !Double !Double
  | SelectedEigenpairCertificationOrderingViolation !SpectrumEnd !Int !Double !Double
  | SelectedEigenpairCertificationShapeMismatch !String
  deriving stock (Eq, Show)

mkEigenpairs :: Int -> U.Vector Double -> U.Vector Double -> U.Vector Double -> Either MoonlightError Eigenpairs
mkEigenpairs dimension values vectors residuals
  | dimension <= 0 = Left (InvariantViolation "Eigenpairs require a positive ambient dimension")
  | U.length residuals /= U.length values =
      Left (InvariantViolation "Eigenpair residual count must match eigenvalue count")
  | otherwise = do
      expectedVectorCount <-
        first
          (const (InvariantViolation "Eigenpair vector payload cardinality exceeds Int range"))
          (checkedNonNegativeProduct dimension (U.length values))
      if U.length vectors /= expectedVectorCount
        then Left (InvariantViolation "Eigenpair vector payload length must equal dimension * eigenvalue count")
        else
          Right
            Eigenpairs
              { eigenpairDimension = dimension,
                eigenpairValues = values,
                eigenpairVectorsColumnMajor = vectors,
                eigenpairResidualNorms = residuals
              }

certifySelectedEigenpairResult ::
  SpectrumEnd ->
  Int ->
  Double ->
  Double ->
  Eigenpairs ->
  Either SelectedEigenpairCertificationFailure CertifiedSelectedEigenpairResult
certifySelectedEigenpairResult spectrumEnd requestedCount residualBound orthonormalityBound pairs
  | requestedCount <= 0 =
      Left (SelectedEigenpairCertificationInvalidRequest ("selected eigenpair certification requires a positive requested count, received " <> show requestedCount))
  | not (finiteNonNegative residualBound) =
      Left (SelectedEigenpairCertificationInvalidRequest ("selected eigenpair residual bound must be finite and non-negative, received " <> show residualBound))
  | not (finiteNonNegative orthonormalityBound) =
      Left (SelectedEigenpairCertificationInvalidRequest ("selected eigenpair orthonormality bound must be finite and non-negative, received " <> show orthonormalityBound))
  | eigenpairCount pairs /= requestedCount =
      Left (SelectedEigenpairCertificationRequestedCountMismatch requestedCount (eigenpairCount pairs))
  | otherwise = do
      residualEvidence <- certifySelectedEigenpairResiduals residualBound pairs
      columns <-
        case traverse (`eigenpairVectorAt` pairs) [0 .. eigenpairCount pairs - 1] of
          Left err -> Left (SelectedEigenpairCertificationShapeMismatch (show err))
          Right columnValues -> Right columnValues
      orthonormalityEvidence <- certifySelectedEigenpairOrthonormality orthonormalityBound columns
      requestOrderingEvidence <- certifySelectedEigenpairOrdering spectrumEnd requestedCount pairs
      Right
        CertifiedSelectedEigenpairResult
          { certifiedSelectedEigenpairResult = pairs,
            certifiedSelectedEigenpairResidualEvidence = residualEvidence,
            certifiedSelectedEigenpairOrthonormalityEvidence = orthonormalityEvidence,
            certifiedSelectedEigenpairRequestOrderingEvidence = requestOrderingEvidence
          }

eigenpairCount :: Eigenpairs -> Int
eigenpairCount = U.length . eigenpairValues

eigenpairVectorAt :: Int -> Eigenpairs -> Either MoonlightError (U.Vector Double)
eigenpairVectorAt columnIndex pairs
  | columnIndex < 0 || columnIndex >= eigenpairCount pairs =
      Left (InvariantViolation "eigenpair vector index out of bounds")
  | otherwise =
      Right
        ( U.slice
            (columnIndex * eigenpairDimension pairs)
            (eigenpairDimension pairs)
            (eigenpairVectorsColumnMajor pairs)
        )

eigenpairsFromColumns :: Int -> [(Double, U.Vector Double, Double)] -> Either MoonlightError Eigenpairs
eigenpairsFromColumns dimension columns =
  let values = U.fromList ((\(value, _, _) -> value) <$> columns)
      vectors = U.concat ((\(_, vector, _) -> vector) <$> columns)
      residuals = U.fromList ((\(_, _, residual) -> residual) <$> columns)
   in mkEigenpairs dimension values vectors residuals

mapEigenpairValues :: (Double -> Double) -> (Double -> Double) -> Eigenpairs -> Either MoonlightError Eigenpairs
mapEigenpairValues mapValue mapResidual pairs =
  mkEigenpairs
    (eigenpairDimension pairs)
    (U.map mapValue (eigenpairValues pairs))
    (eigenpairVectorsColumnMajor pairs)
    (U.map mapResidual (eigenpairResidualNorms pairs))

certifySelectedEigenpairResiduals ::
  Double ->
  Eigenpairs ->
  Either SelectedEigenpairCertificationFailure SelectedEigenpairResidualEvidence
certifySelectedEigenpairResiduals residualBound pairs =
  case filter (\(_, residualValue) -> not (fieldValueValid residualValue) || residualValue < 0.0 || residualValue > residualBound) indexedResiduals of
    [] ->
      Right
        SelectedEigenpairResidualEvidence
          { selectedEigenpairResidualBound = residualBound,
            selectedEigenpairMaxResidualNorm = foldr max 0.0 (abs . snd <$> indexedResiduals)
          }
    (columnIndex, residualValue) : _ ->
      Left (SelectedEigenpairCertificationResidualExceeded columnIndex residualValue residualBound)
  where
    indexedResiduals = zip [0 :: Int ..] (U.toList (eigenpairResidualNorms pairs))

certifySelectedEigenpairOrthonormality ::
  Double ->
  [U.Vector Double] ->
  Either SelectedEigenpairCertificationFailure SelectedEigenpairOrthonormalityEvidence
certifySelectedEigenpairOrthonormality orthonormalityBound columns =
  case filter (\(_, _, deviationValue) -> not (fieldValueValid deviationValue) || abs deviationValue > orthonormalityBound) deviations of
    [] ->
      Right
        SelectedEigenpairOrthonormalityEvidence
          { selectedEigenpairOrthonormalityBound = orthonormalityBound,
            selectedEigenpairMaxOrthonormalityDeviation = foldr max 0.0 (abs . thirdEntry <$> deviations)
          }
    (leftIndex, rightIndex, deviationValue) : _ ->
      Left (SelectedEigenpairCertificationOrthonormalityExceeded leftIndex rightIndex deviationValue orthonormalityBound)
  where
    deviations =
      [ (leftIndex, rightIndex, vectorDotU leftColumn rightColumn - expectedInnerProduct leftIndex rightIndex)
        | (leftIndex, leftColumn) <- zip [0 :: Int ..] columns,
          (rightIndex, rightColumn) <- zip [0 :: Int ..] columns,
          leftIndex <= rightIndex
      ]

certifySelectedEigenpairOrdering ::
  SpectrumEnd ->
  Int ->
  Eigenpairs ->
  Either SelectedEigenpairCertificationFailure SelectedEigenpairRequestOrderingEvidence
certifySelectedEigenpairOrdering spectrumEnd requestedCount pairs =
  case filter (not . orderedAdjacent spectrumEnd) adjacentValues of
    [] ->
      Right
        SelectedEigenpairRequestOrderingEvidence
          { selectedEigenpairRequestedCount = requestedCount,
            selectedEigenpairCertifiedCount = eigenpairCount pairs,
            selectedEigenpairCertifiedOrdering = spectrumEnd
          }
    (leftIndex, leftValue, rightValue) : _ ->
      Left (SelectedEigenpairCertificationOrderingViolation spectrumEnd leftIndex leftValue rightValue)
  where
    values = U.toList (eigenpairValues pairs)
    adjacentValues = zipWith (\indexValue (leftValue, rightValue) -> (indexValue, leftValue, rightValue)) [0 :: Int ..] (zip values (drop 1 values))

orderedAdjacent :: SpectrumEnd -> (Int, Double, Double) -> Bool
orderedAdjacent spectrumEnd (_, leftValue, rightValue) =
  fieldValueValid leftValue
    && fieldValueValid rightValue
    && spectrumValuesOrdered spectrumEnd leftValue rightValue

spectrumValuesOrdered :: SpectrumEnd -> Double -> Double -> Bool
spectrumValuesOrdered spectrumEnd leftValue rightValue =
  case spectrumEnd of
    SmallestEigenvalues -> leftValue <= rightValue
    LargestEigenvalues -> leftValue >= rightValue

expectedInnerProduct :: Int -> Int -> Double
expectedInnerProduct leftIndex rightIndex =
  if leftIndex == rightIndex
    then 1.0
    else 0.0

vectorDotU :: U.Vector Double -> U.Vector Double -> Double
vectorDotU leftVector rightVector =
  U.sum (U.zipWith (*) leftVector rightVector)

thirdEntry :: (left, right, value) -> value
thirdEntry (_, _, value) = value

finiteNonNegative :: Double -> Bool
finiteNonNegative value =
  fieldValueValid value && value >= 0.0
