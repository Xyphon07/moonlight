module Moonlight.LinAlg.Effect.Harness.Decomposition
  ( qrReconstructsInputLaw,
    qrOrthonormalColumnsLaw,
    choleskyReconstructsSpdLaw,
    symmetricEigenReconstructsLaw,
    symmetricEigenOrthonormalLaw,
    symmetricEigenUncheckedPassesCertificationLaw,
    thinSvdReconstructsLaw,
    thinSvdOrthonormalFactorsLaw,
    thinSvdSingularValuesOrderedNonnegativeLaw,
  )
where

import Data.Bifunctor (first)
import Data.Vector.Storable qualified as S
import Moonlight.LinAlg
  ( choleskyDecomp,
    fromListMatrix,
    mult,
    qrDecompFullColumnRank,
    symmetricEigen,
    thinSvdFullColumnRank,
    toListMatrix,
    toListVector,
    transpose,
  )
import Moonlight.LinAlg.Effect.Harness.Core
  ( approxTolerance,
    assertApproxList,
    assertRightProperty,
    maxAbsDifference,
  )
import Moonlight.LinAlg.Internal.Eigen.Symmetric
  ( certifySymmetricEigenResult,
    symmetricEigenPairsDenseUnchecked,
  )
import Moonlight.LinAlg.Pure.Dense.Flat (mkDenseDoubleMatrixRowMajor)
import Test.Tasty.QuickCheck qualified as QC

newtype FullRankMatrix43 = FullRankMatrix43 [Double]
  deriving stock (Eq, Show)

newtype SpdMatrix3 = SpdMatrix3 [Double]
  deriving stock (Eq, Show)

newtype SymmetricMatrix3 = SymmetricMatrix3 [Double]
  deriving stock (Eq, Show)

newtype FullRankMatrix32 = FullRankMatrix32 [Double]
  deriving stock (Eq, Show)

instance QC.Arbitrary FullRankMatrix43 where
  arbitrary =
    FullRankMatrix43 <$> anchoredOrGenerated fullRankMatrix43Anchors generateFullRankMatrix43

instance QC.Arbitrary SpdMatrix3 where
  arbitrary =
    SpdMatrix3 <$> anchoredOrGenerated spdMatrix3Anchors generateSpdMatrix3

instance QC.Arbitrary SymmetricMatrix3 where
  arbitrary =
    SymmetricMatrix3 <$> anchoredOrGenerated symmetricMatrix3Anchors generateSymmetricMatrix3

instance QC.Arbitrary FullRankMatrix32 where
  arbitrary =
    FullRankMatrix32 <$> anchoredOrGenerated fullRankMatrix32Anchors generateFullRankMatrix32

anchoredOrGenerated :: [[Double]] -> QC.Gen [Double] -> QC.Gen [Double]
anchoredOrGenerated anchors generatedValues =
  QC.frequency
    [ (1, QC.elements anchors),
      (9, generatedValues)
    ]

generateFullRankMatrix43 :: QC.Gen [Double]
generateFullRankMatrix43 =
  fullRankMatrix43Entries
    <$> generatedTriple generatedNonZeroEntry
    <*> generatedTriple generatedEntry
    <*> generatedTriple generatedEntry

generateSpdMatrix3 :: QC.Gen [Double]
generateSpdMatrix3 =
  spdMatrix3Entries
    <$> generatedTriple (QC.choose (1.0, 3.0))
    <*> generatedTriple (QC.choose (-1.0, 1.0))

generateSymmetricMatrix3 :: QC.Gen [Double]
generateSymmetricMatrix3 =
  symmetricMatrix3Entries
    <$> generatedTriple generatedEntry
    <*> generatedTriple generatedEntry

generateFullRankMatrix32 :: QC.Gen [Double]
generateFullRankMatrix32 =
  fullRankMatrix32Entries
    <$> ((,) <$> generatedNonZeroEntry <*> generatedNonZeroEntry)
    <*> generatedTriple generatedEntry

generatedTriple :: QC.Gen value -> QC.Gen (value, value, value)
generatedTriple generatedValue =
  (,,) <$> generatedValue <*> generatedValue <*> generatedValue

fullRankMatrix43Entries :: (Double, Double, Double) -> (Double, Double, Double) -> (Double, Double, Double) -> [Double]
fullRankMatrix43Entries (d0, d1, d2) (l10, l20, l21) (r0, r1, r2) =
  [d0, 0.0, 0.0, l10, d1, 0.0, l20, l21, d2, r0, r1, r2]

spdMatrix3Entries :: (Double, Double, Double) -> (Double, Double, Double) -> [Double]
spdMatrix3Entries (d0, d1, d2) (l10, l20, l21) =
  symmetricMatrix3Entries
    (d0 * d0, l10 * l10 + d1 * d1, l20 * l20 + l21 * l21 + d2 * d2)
    (d0 * l10, d0 * l20, l10 * l20 + d1 * l21)

symmetricMatrix3Entries :: (Double, Double, Double) -> (Double, Double, Double) -> [Double]
symmetricMatrix3Entries (d0, d1, d2) (o01, o02, o12) =
  [d0, o01, o02, o01, d1, o12, o02, o12, d2]

fullRankMatrix32Entries :: (Double, Double) -> (Double, Double, Double) -> [Double]
fullRankMatrix32Entries (d0, d1) (l10, l20, l21) =
  [d0, 0.0, l10, d1, l20, l21]

generatedEntry :: QC.Gen Double
generatedEntry =
  QC.choose (-4.0, 4.0)

generatedNonZeroEntry :: QC.Gen Double
generatedNonZeroEntry =
  QC.elements [-4.0, -3.0, -2.0, -1.0, 1.0, 2.0, 3.0, 4.0]

fullRankMatrix43Anchors :: [[Double]]
fullRankMatrix43Anchors =
  [ [1.0, 0.0, 2.0, 0.0, 1.0, -1.0, 2.0, 1.0, 0.0, 1.0, -1.0, 1.0],
    [2.0, 1.0, 0.0, 1.0, 3.0, 1.0, 0.0, -1.0, 2.0, 1.0, 0.0, 1.0],
    [1.0, 2.0, 1.0, 2.0, 0.0, -1.0, 0.0, 1.0, 3.0, 1.0, -1.0, 0.0]
  ]

spdMatrix3Anchors :: [[Double]]
spdMatrix3Anchors =
  [ [6.0, 2.0, 1.0, 2.0, 5.0, 0.5, 1.0, 0.5, 4.0],
    [5.0, -1.0, 0.5, -1.0, 4.0, 1.0, 0.5, 1.0, 3.5],
    [9.0, 1.5, -0.5, 1.5, 7.0, 2.0, -0.5, 2.0, 6.0]
  ]

symmetricMatrix3Anchors :: [[Double]]
symmetricMatrix3Anchors =
  [ [4.0, 1.0, 2.0, 1.0, 3.0, 0.5, 2.0, 0.5, 5.0],
    [2.0, 0.0, 0.0, 0.0, 3.0, 0.0, 0.0, 0.0, 7.0],
    [1.0, 1.0e-6, 0.0, 1.0e-6, 1.0 + 1.0e-12, -1.0e-6, 0.0, -1.0e-6, 3.0]
  ]

fullRankMatrix32Anchors :: [[Double]]
fullRankMatrix32Anchors =
  [ [3.0, 0.0, 0.0, 2.0, 1.0, 1.0],
    [1.0, 2.0, 2.0, -1.0, 0.5, 3.0],
    [4.0, 1.0, 1.0, 3.0, -1.0, 2.0]
  ]

qrReconstructsInputLaw :: QC.Property
qrReconstructsInputLaw =
  QC.property qrReconstructsInputLawProperty

qrOrthonormalColumnsLaw :: QC.Property
qrOrthonormalColumnsLaw =
  QC.property qrOrthonormalColumnsLawProperty

choleskyReconstructsSpdLaw :: QC.Property
choleskyReconstructsSpdLaw =
  QC.property choleskyReconstructsSpdLawProperty

symmetricEigenReconstructsLaw :: QC.Property
symmetricEigenReconstructsLaw =
  QC.property symmetricEigenReconstructsLawProperty

symmetricEigenOrthonormalLaw :: QC.Property
symmetricEigenOrthonormalLaw =
  QC.property symmetricEigenOrthonormalLawProperty

symmetricEigenUncheckedPassesCertificationLaw :: QC.Property
symmetricEigenUncheckedPassesCertificationLaw =
  QC.property symmetricEigenUncheckedPassesCertificationLawProperty

thinSvdReconstructsLaw :: QC.Property
thinSvdReconstructsLaw =
  QC.property thinSvdReconstructsLawProperty

thinSvdOrthonormalFactorsLaw :: QC.Property
thinSvdOrthonormalFactorsLaw =
  QC.property thinSvdOrthonormalFactorsLawProperty

thinSvdSingularValuesOrderedNonnegativeLaw :: QC.Property
thinSvdSingularValuesOrderedNonnegativeLaw =
  QC.property thinSvdSingularValuesOrderedNonnegativeLawProperty

qrReconstructsInputLawProperty :: FullRankMatrix43 -> QC.Property
qrReconstructsInputLawProperty (FullRankMatrix43 entries) =
  assertRightProperty $ do
    matrixValue <- fromListMatrix @4 @3 entries
    (qMatrix, rMatrix) <- qrDecompFullColumnRank matrixValue
    reconstructed <- mult qMatrix rMatrix
    pure (maxAbsDifference entries (toListMatrix reconstructed) <= approxTolerance)

qrOrthonormalColumnsLawProperty :: FullRankMatrix43 -> QC.Property
qrOrthonormalColumnsLawProperty (FullRankMatrix43 entries) =
  assertRightProperty $ do
    matrixValue <- fromListMatrix @4 @3 entries
    (qMatrix, _) <- qrDecompFullColumnRank matrixValue
    transposedQ <- transpose qMatrix
    gramMatrix <- mult transposedQ qMatrix
    pure (assertApproxList identity3 (toListMatrix gramMatrix))

choleskyReconstructsSpdLawProperty :: SpdMatrix3 -> QC.Property
choleskyReconstructsSpdLawProperty (SpdMatrix3 entries) =
  assertRightProperty $ do
    matrixValue <- fromListMatrix @3 @3 entries
    lowerMatrix <- choleskyDecomp matrixValue
    transposedLower <- transpose lowerMatrix
    reconstructed <- mult lowerMatrix transposedLower
    pure (maxAbsDifference entries (toListMatrix reconstructed) <= approxTolerance)

symmetricEigenReconstructsLawProperty :: SymmetricMatrix3 -> QC.Property
symmetricEigenReconstructsLawProperty (SymmetricMatrix3 entries) =
  assertRightProperty $ do
    matrixValue <- fromListMatrix @3 @3 entries
    (eigenvalues, eigenvectors) <- symmetricEigen matrixValue
    diagonalMatrix <- fromListMatrix @3 @3 (diagonal3 (toListVector eigenvalues))
    weightedEigenvectors <- mult eigenvectors diagonalMatrix
    transposedEigenvectors <- transpose eigenvectors
    reconstructed <- mult weightedEigenvectors transposedEigenvectors
    pure (maxAbsDifference entries (toListMatrix reconstructed) <= approxTolerance)

symmetricEigenOrthonormalLawProperty :: SymmetricMatrix3 -> QC.Property
symmetricEigenOrthonormalLawProperty (SymmetricMatrix3 entries) =
  assertRightProperty $ do
    matrixValue <- fromListMatrix @3 @3 entries
    (_, eigenvectors) <- symmetricEigen matrixValue
    transposedEigenvectors <- transpose eigenvectors
    gramMatrix <- mult transposedEigenvectors eigenvectors
    pure (assertApproxList identity3 (toListMatrix gramMatrix))

symmetricEigenUncheckedPassesCertificationLawProperty :: SymmetricMatrix3 -> QC.Property
symmetricEigenUncheckedPassesCertificationLawProperty (SymmetricMatrix3 entries) =
  assertRightProperty $ do
    matrixValue <- first show (mkDenseDoubleMatrixRowMajor 3 3 (S.fromList entries))
    uncheckedResult <- first show (symmetricEigenPairsDenseUnchecked 3 matrixValue)
    _ <- first show (certifySymmetricEigenResult matrixValue uncheckedResult)
    pure True

thinSvdReconstructsLawProperty :: FullRankMatrix32 -> QC.Property
thinSvdReconstructsLawProperty (FullRankMatrix32 entries) =
  assertRightProperty $ do
    matrixValue <- fromListMatrix @3 @2 entries
    (uMatrix, sMatrix, vTMatrix) <- thinSvdFullColumnRank matrixValue
    usMatrix <- mult uMatrix sMatrix
    reconstructed <- mult usMatrix vTMatrix
    pure (maxAbsDifference entries (toListMatrix reconstructed) <= approxTolerance)

thinSvdOrthonormalFactorsLawProperty :: FullRankMatrix32 -> QC.Property
thinSvdOrthonormalFactorsLawProperty (FullRankMatrix32 entries) =
  assertRightProperty $ do
    matrixValue <- fromListMatrix @3 @2 entries
    (uMatrix, _, vTMatrix) <- thinSvdFullColumnRank matrixValue
    transposedU <- transpose uMatrix
    uGram <- mult transposedU uMatrix
    vMatrix <- transpose vTMatrix
    vGram <- mult vTMatrix vMatrix
    pure (assertApproxList identity2 (toListMatrix uGram) && assertApproxList identity2 (toListMatrix vGram))

thinSvdSingularValuesOrderedNonnegativeLawProperty :: FullRankMatrix32 -> QC.Property
thinSvdSingularValuesOrderedNonnegativeLawProperty (FullRankMatrix32 entries) =
  assertRightProperty $ do
    matrixValue <- fromListMatrix @3 @2 entries
    (_, sMatrix, _) <- thinSvdFullColumnRank matrixValue
    pure (orderedNonnegativeDiagonal2 (toListMatrix sMatrix))

identity2 :: [Double]
identity2 =
  [1.0, 0.0, 0.0, 1.0]

identity3 :: [Double]
identity3 =
  [1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0]

diagonal3 :: [Double] -> [Double]
diagonal3 values =
  case values of
    [d0, d1, d2] -> [d0, 0.0, 0.0, 0.0, d1, 0.0, 0.0, 0.0, d2]
    _ -> []

orderedNonnegativeDiagonal2 :: [Double] -> Bool
orderedNonnegativeDiagonal2 entries =
  case entries of
    [s0, z01, z10, s1] ->
      s0 >= 0.0 && s1 >= 0.0 && s0 >= s1 && assertApproxList [0.0, 0.0] [z01, z10]
    _ -> False
