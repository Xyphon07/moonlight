module Moonlight.LinAlg.Effect.Harness.Field
  ( pluReconstructsInputLaw,
    rankKernelNullityLaw,
    kernelVectorsAnnihilatedLaw,
    packedLinearMapIdentityLaw,
    packedLinearMapCompositionLaw,
    gf2PackedInverseTwoSidedLaw,
  )
where

import Data.Bifunctor (first)
import Data.Vector qualified as V
import Moonlight.LinAlg
  ( GF2 (..),
    applyPackedLinearMap,
    composePackedLinearMaps,
    fromListMatrix,
    gf2PackedMatrixLinearMap,
    identityPackedLinearMap,
    inverseGF2PackedMatrix,
    kernel,
    kernelBasisVectors,
    mult,
    packedLinearMapColumns,
    packedLinearMapFromEntries,
    packedRowFromIndices,
    packedRowIndices,
    pluDecompFullRank,
    pluLower,
    pluPermutation,
    pluUpper,
    rank,
    toListMatrix,
    toListVector,
    mkGF2PackedMatrixFromRowMajor,
  )
import Moonlight.LinAlg.Effect.Harness.Core (assertRightProperty)
import Test.Tasty.QuickCheck qualified as QC

newtype InvertibleRational2 = InvertibleRational2 [Rational]
  deriving stock (Eq, Show)

newtype RationalMatrix23 = RationalMatrix23 [Rational]
  deriving stock (Eq, Show)

newtype PackedRow3 = PackedRow3 [Int]
  deriving stock (Eq, Show)

instance QC.Arbitrary InvertibleRational2 where
  arbitrary =
    InvertibleRational2
      <$> QC.suchThat
        (QC.vectorOf 4 (fromIntegral <$> QC.chooseInt (-5, 5)))
        invertible2

instance QC.Arbitrary RationalMatrix23 where
  arbitrary =
    RationalMatrix23
      <$> QC.vectorOf 6 (fromIntegral <$> QC.chooseInt (-5, 5))

instance QC.Arbitrary PackedRow3 where
  arbitrary =
    PackedRow3
      <$> QC.sublistOf [0, 1, 2]

pluReconstructsInputLaw :: QC.Property
pluReconstructsInputLaw =
  QC.property pluReconstructsInputLawProperty

rankKernelNullityLaw :: QC.Property
rankKernelNullityLaw =
  QC.property rankKernelNullityLawProperty

kernelVectorsAnnihilatedLaw :: QC.Property
kernelVectorsAnnihilatedLaw =
  QC.property kernelVectorsAnnihilatedLawProperty

packedLinearMapIdentityLaw :: QC.Property
packedLinearMapIdentityLaw =
  QC.property packedLinearMapIdentityLawProperty

packedLinearMapCompositionLaw :: QC.Property
packedLinearMapCompositionLaw =
  QC.property packedLinearMapCompositionLawProperty

invertible2 :: [Rational] -> Bool
invertible2 entries =
  case entries of
    [a, b, c, d] -> a * d - b * c /= 0
    _ -> False

pluReconstructsInputLawProperty :: InvertibleRational2 -> QC.Property
pluReconstructsInputLawProperty (InvertibleRational2 entries) =
  assertRightProperty $ do
    matrixValue <- fromListMatrix @2 @2 entries
    pluValue <- pluDecompFullRank matrixValue
    leftSide <- mult (pluPermutation pluValue) matrixValue
    rightSide <- mult (pluLower pluValue) (pluUpper pluValue)
    pure (toListMatrix leftSide == toListMatrix rightSide)

rankKernelNullityLawProperty :: RationalMatrix23 -> QC.Property
rankKernelNullityLawProperty (RationalMatrix23 entries) =
  assertRightProperty $ do
    matrixValue <- fromListMatrix @2 @3 entries
    rankValue <- rank matrixValue
    kernelValue <- kernel matrixValue
    pure (rankValue + length (kernelBasisVectors kernelValue) == 3)

kernelVectorsAnnihilatedLawProperty :: RationalMatrix23 -> QC.Property
kernelVectorsAnnihilatedLawProperty (RationalMatrix23 entries) =
  assertRightProperty $ do
    matrixValue <- fromListMatrix @2 @3 entries
    kernelValue <- kernel matrixValue
    annihilated <-
      traverse
        ( \basisVector -> do
            columnMatrix <- fromListMatrix @3 @1 (toListVector basisVector)
            resultMatrix <- mult matrixValue columnMatrix
            pure (all (== 0) (toListMatrix resultMatrix))
        )
        (kernelBasisVectors kernelValue)
    pure (and annihilated)

packedLinearMapIdentityLawProperty :: PackedRow3 -> QC.Property
packedLinearMapIdentityLawProperty (PackedRow3 indices) =
  assertRightProperty $ do
    rowValue <- packedRowFromIndices "packed identity law row" 3 indices
    identityMap <- identityPackedLinearMap "packed identity law map" 3
    image <- applyPackedLinearMap "packed identity law apply" identityMap rowValue
    pure (packedRowIndices image == packedRowIndices rowValue)

packedLinearMapCompositionLawProperty :: PackedRow3 -> QC.Property
packedLinearMapCompositionLawProperty (PackedRow3 indices) =
  assertRightProperty $ do
    rowValue <- packedRowFromIndices "packed composition law row" 3 indices
    leftMap <-
      packedLinearMapFromEntries
        "packed composition law left"
        3
        3
        [(0, 0), (1, 0), (1, 2), (2, 1)]
    rightMap <-
      packedLinearMapFromEntries
        "packed composition law right"
        3
        3
        [(0, 1), (1, 2), (2, 0), (2, 2)]
    composedMap <- composePackedLinearMaps "packed composition law composed" leftMap rightMap
    directImage <- applyPackedLinearMap "packed composition law direct" composedMap rowValue
    stagedRight <- applyPackedLinearMap "packed composition law first" rightMap rowValue
    stagedImage <- applyPackedLinearMap "packed composition law second" leftMap stagedRight
    pure (packedRowIndices directImage == packedRowIndices stagedImage)

gf2PackedInverseTwoSidedLaw :: QC.Property
gf2PackedInverseTwoSidedLaw =
  assertRightProperty $ do
    matrixValue <- first show (mkGF2PackedMatrixFromRowMajor 3 3 [GF2One, GF2One, GF2Zero, GF2Zero, GF2One, GF2One, GF2Zero, GF2Zero, GF2One])
    matrixMap <- first show (gf2PackedMatrixLinearMap matrixValue)
    maybeInverse <- first show (inverseGF2PackedMatrix matrixValue)
    case maybeInverse of
      Nothing -> pure False
      Just inverseMap -> do
        identityMap <- first show (identityPackedLinearMap "packed inverse law identity" 3)
        leftIdentity <- first show (composePackedLinearMaps "packed inverse law left" inverseMap matrixMap)
        rightIdentity <- first show (composePackedLinearMaps "packed inverse law right" matrixMap inverseMap)
        let identityColumns = packedRowIndices <$> V.toList (packedLinearMapColumns identityMap)
            leftColumns = packedRowIndices <$> V.toList (packedLinearMapColumns leftIdentity)
            rightColumns = packedRowIndices <$> V.toList (packedLinearMapColumns rightIdentity)
        pure (leftColumns == identityColumns && rightColumns == identityColumns)
