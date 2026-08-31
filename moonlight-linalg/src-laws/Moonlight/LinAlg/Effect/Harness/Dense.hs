module Moonlight.LinAlg.Effect.Harness.Dense
  ( denseAddAssociativeLaw,
    denseAddCommutativeLaw,
    denseMultiplyAssociativeLaw,
    denseLeftDistributiveLaw,
    denseRightDistributiveLaw,
    denseTransposeInvolutionLaw,
    denseTransposeProductReversalLaw,
    denseMapCompositionLaw,
  )
where

import Moonlight.LinAlg (add, fromListMatrix, mapMatrix, mult, toListMatrix, transpose)
import Moonlight.LinAlg.Effect.Harness.Core (exactRightProperty)
import Test.Tasty.QuickCheck qualified as QC

newtype RationalMatrix2 = RationalMatrix2 [Rational]
  deriving stock (Eq, Show)

instance QC.Arbitrary RationalMatrix2 where
  arbitrary =
    RationalMatrix2
      <$> QC.vectorOf 4 (fromIntegral <$> QC.chooseInt (-8, 8))

denseAddAssociativeLaw :: QC.Property
denseAddAssociativeLaw =
  QC.property denseAddAssociativeLawProperty

denseAddCommutativeLaw :: QC.Property
denseAddCommutativeLaw =
  QC.property denseAddCommutativeLawProperty

denseMultiplyAssociativeLaw :: QC.Property
denseMultiplyAssociativeLaw =
  QC.property denseMultiplyAssociativeLawProperty

denseLeftDistributiveLaw :: QC.Property
denseLeftDistributiveLaw =
  QC.property denseLeftDistributiveLawProperty

denseRightDistributiveLaw :: QC.Property
denseRightDistributiveLaw =
  QC.property denseRightDistributiveLawProperty

denseTransposeInvolutionLaw :: QC.Property
denseTransposeInvolutionLaw =
  QC.property denseTransposeInvolutionLawProperty

denseTransposeProductReversalLaw :: QC.Property
denseTransposeProductReversalLaw =
  QC.property denseTransposeProductReversalLawProperty

denseMapCompositionLaw :: QC.Property
denseMapCompositionLaw =
  QC.property denseMapCompositionLawProperty

denseAddAssociativeLawProperty :: RationalMatrix2 -> RationalMatrix2 -> RationalMatrix2 -> QC.Property
denseAddAssociativeLawProperty (RationalMatrix2 leftEntries) (RationalMatrix2 middleEntries) (RationalMatrix2 rightEntries) =
  exactRightProperty leftAssociated rightAssociated
  where
    leftAssociated = do
      leftMatrix <- fromListMatrix @2 @2 leftEntries
      middleMatrix <- fromListMatrix @2 @2 middleEntries
      rightMatrix <- fromListMatrix @2 @2 rightEntries
      leftMiddle <- add leftMatrix middleMatrix
      fmap toListMatrix (add leftMiddle rightMatrix)
    rightAssociated = do
      leftMatrix <- fromListMatrix @2 @2 leftEntries
      middleMatrix <- fromListMatrix @2 @2 middleEntries
      rightMatrix <- fromListMatrix @2 @2 rightEntries
      middleRight <- add middleMatrix rightMatrix
      fmap toListMatrix (add leftMatrix middleRight)

denseAddCommutativeLawProperty :: RationalMatrix2 -> RationalMatrix2 -> QC.Property
denseAddCommutativeLawProperty (RationalMatrix2 leftEntries) (RationalMatrix2 rightEntries) =
  exactRightProperty leftRight rightLeft
  where
    leftRight = do
      leftMatrix <- fromListMatrix @2 @2 leftEntries
      rightMatrix <- fromListMatrix @2 @2 rightEntries
      fmap toListMatrix (add leftMatrix rightMatrix)
    rightLeft = do
      leftMatrix <- fromListMatrix @2 @2 leftEntries
      rightMatrix <- fromListMatrix @2 @2 rightEntries
      fmap toListMatrix (add rightMatrix leftMatrix)

denseMultiplyAssociativeLawProperty :: RationalMatrix2 -> RationalMatrix2 -> RationalMatrix2 -> QC.Property
denseMultiplyAssociativeLawProperty (RationalMatrix2 leftEntries) (RationalMatrix2 middleEntries) (RationalMatrix2 rightEntries) =
  exactRightProperty leftAssociated rightAssociated
  where
    leftAssociated = do
      leftMatrix <- fromListMatrix @2 @2 leftEntries
      middleMatrix <- fromListMatrix @2 @2 middleEntries
      rightMatrix <- fromListMatrix @2 @2 rightEntries
      leftMiddle <- mult leftMatrix middleMatrix
      fmap toListMatrix (mult leftMiddle rightMatrix)
    rightAssociated = do
      leftMatrix <- fromListMatrix @2 @2 leftEntries
      middleMatrix <- fromListMatrix @2 @2 middleEntries
      rightMatrix <- fromListMatrix @2 @2 rightEntries
      middleRight <- mult middleMatrix rightMatrix
      fmap toListMatrix (mult leftMatrix middleRight)

denseLeftDistributiveLawProperty :: RationalMatrix2 -> RationalMatrix2 -> RationalMatrix2 -> QC.Property
denseLeftDistributiveLawProperty (RationalMatrix2 leftEntries) (RationalMatrix2 middleEntries) (RationalMatrix2 rightEntries) =
  exactRightProperty distributed expanded
  where
    distributed = do
      leftMatrix <- fromListMatrix @2 @2 leftEntries
      middleMatrix <- fromListMatrix @2 @2 middleEntries
      rightMatrix <- fromListMatrix @2 @2 rightEntries
      middleRight <- add middleMatrix rightMatrix
      fmap toListMatrix (mult leftMatrix middleRight)
    expanded = do
      leftMatrix <- fromListMatrix @2 @2 leftEntries
      middleMatrix <- fromListMatrix @2 @2 middleEntries
      rightMatrix <- fromListMatrix @2 @2 rightEntries
      leftMiddle <- mult leftMatrix middleMatrix
      leftRight <- mult leftMatrix rightMatrix
      fmap toListMatrix (add leftMiddle leftRight)

denseRightDistributiveLawProperty :: RationalMatrix2 -> RationalMatrix2 -> RationalMatrix2 -> QC.Property
denseRightDistributiveLawProperty (RationalMatrix2 leftEntries) (RationalMatrix2 middleEntries) (RationalMatrix2 rightEntries) =
  exactRightProperty distributed expanded
  where
    distributed = do
      leftMatrix <- fromListMatrix @2 @2 leftEntries
      middleMatrix <- fromListMatrix @2 @2 middleEntries
      rightMatrix <- fromListMatrix @2 @2 rightEntries
      leftMiddle <- add leftMatrix middleMatrix
      fmap toListMatrix (mult leftMiddle rightMatrix)
    expanded = do
      leftMatrix <- fromListMatrix @2 @2 leftEntries
      middleMatrix <- fromListMatrix @2 @2 middleEntries
      rightMatrix <- fromListMatrix @2 @2 rightEntries
      leftRight <- mult leftMatrix rightMatrix
      middleRight <- mult middleMatrix rightMatrix
      fmap toListMatrix (add leftRight middleRight)

denseTransposeInvolutionLawProperty :: RationalMatrix2 -> QC.Property
denseTransposeInvolutionLawProperty (RationalMatrix2 entries) =
  exactRightProperty original transposedTwice
  where
    original = Right entries
    transposedTwice = do
      matrixValue <- fromListMatrix @2 @2 entries
      once <- transpose matrixValue
      twice <- transpose once
      pure (toListMatrix twice)

denseTransposeProductReversalLawProperty :: RationalMatrix2 -> RationalMatrix2 -> QC.Property
denseTransposeProductReversalLawProperty (RationalMatrix2 leftEntries) (RationalMatrix2 rightEntries) =
  exactRightProperty transposedProduct reversedProduct
  where
    transposedProduct = do
      leftMatrix <- fromListMatrix @2 @2 leftEntries
      rightMatrix <- fromListMatrix @2 @2 rightEntries
      productMatrix <- mult leftMatrix rightMatrix
      fmap toListMatrix (transpose productMatrix)
    reversedProduct = do
      leftMatrix <- fromListMatrix @2 @2 leftEntries
      rightMatrix <- fromListMatrix @2 @2 rightEntries
      leftTranspose <- transpose leftMatrix
      rightTranspose <- transpose rightMatrix
      fmap toListMatrix (mult rightTranspose leftTranspose)

denseMapCompositionLawProperty :: RationalMatrix2 -> QC.Property
denseMapCompositionLawProperty (RationalMatrix2 entries) =
  exactRightProperty staged composed
  where
    staged = do
      matrixValue <- fromListMatrix @2 @2 entries
      incremented <- mapMatrix (+ 1) matrixValue
      fmap toListMatrix (mapMatrix (* 3) incremented)
    composed = do
      matrixValue <- fromListMatrix @2 @2 entries
      fmap toListMatrix (mapMatrix ((* 3) . (+ 1)) matrixValue)
