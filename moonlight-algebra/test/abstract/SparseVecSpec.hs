module SparseVecSpec
  ( tests,
  )
where

import Moonlight.Algebra.Pure.SparseVec qualified as SparseVec
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( assertFailure,
    testCase,
    (@?=),
  )

tests :: TestTree
tests =
  testGroup
    "sparse vector compiled kernels"
    [ testCase "compiled linear map matches semantic extendLinear" compiledLinearMapMatchesExtendLinear,
      testCase "compiled linear map reports source indices outside the domain" compiledLinearMapRejectsOutOfBoundsSource,
      testCase "compiled linear map rejects a negative source count" compiledLinearMapRejectsNegativeSourceCount
    ]

compiledLinearMapMatchesExtendLinear :: IO ()
compiledLinearMapMatchesExtendLinear = do
  let sourceEntries = [(0, 3), (1, 4), (2, -4), (6, 5)]
      semanticResult =
        SparseVec.toEntries
          (SparseVec.extendLinear sparseBasisExpansion (SparseVec.fromEntries sourceEntries))
      compiledSource =
        SparseVec.sparseIxVecFromEntries sourceEntries
  case SparseVec.compileSparseLinearMap 8 sparseBasisExpansionEntries of
    Left failure ->
      assertFailure ("unexpected sparse-map compile failure: " <> show failure)
    Right compiledMap ->
      case SparseVec.applySparseLinearMap compiledMap compiledSource of
        Left failure ->
          assertFailure ("unexpected compiled sparse-map failure: " <> show failure)
        Right compiledResult ->
          SparseVec.sparseIxVecToEntries compiledResult @?= semanticResult

compiledLinearMapRejectsOutOfBoundsSource :: IO ()
compiledLinearMapRejectsOutOfBoundsSource =
  case SparseVec.compileSparseLinearMap 3 sparseBasisExpansionEntries of
    Left failure ->
      assertFailure ("unexpected sparse-map compile failure: " <> show failure)
    Right compiledMap ->
      SparseVec.applySparseLinearMap compiledMap compiledSource
        @?= Left (SparseVec.SparseLinearMapSourceOutOfBounds 4)
  where
    compiledSource =
      SparseVec.sparseIxVecFromEntries [(4, 1)]

compiledLinearMapRejectsNegativeSourceCount :: IO ()
compiledLinearMapRejectsNegativeSourceCount =
  SparseVec.compileSparseLinearMap (-1) sparseBasisExpansionEntries
    @?= Left (SparseVec.SparseLinearMapNegativeSourceCount (-1))

sparseBasisExpansion :: Int -> SparseVec.SparseVec Int Int
sparseBasisExpansion =
  SparseVec.fromEntries . sparseBasisExpansionEntries

sparseBasisExpansionEntries :: Int -> [(Int, Int)]
sparseBasisExpansionEntries basisValue =
  [ (basisValue, 1),
    (basisValue + 1, 2)
  ]
