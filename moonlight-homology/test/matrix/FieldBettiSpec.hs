{-# LANGUAGE DataKinds #-}

module FieldBettiSpec
  ( tests,
  )
where

import Moonlight.Homology
import Moonlight.Homology.Boundary.Finite (mkFiniteChainComplex)
import TestFixtures
  ( mooreComplex,
  )
import Moonlight.LinAlg.Pure.Dense.GF2 (GF2)
import Numeric.Natural (Natural)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertEqual,
    assertFailure,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "homology backend substrate"
    [ testCase "GF2 Betti uses linalg rank for zero and identity matrices" testGF2ZeroAndIdentityBetti,
      testCase "GF2 Betti handles duplicate rows and duplicate-entry cancellation" testGF2DuplicateRowsAndEntryCancellation,
      testCase "GF2 coefficient mapping uses parity for signed integral boundaries" testGF2ParityConversion,
      testCase "typed homology backend dispatches Smith, Rational, and GF2 ranks" testHomologyBackendDispatch,
      testCase "field Betti computes isolated points and an interval through Phase 2" testFieldBettiBasicComplexes,
      testCase "field Betti rejects malformed and non-nilpotent chains through Phase 2" testFieldBettiRejectsInvalidChains,
      testCase "checked constructor refuses shape and nilpotence violations at the seal" testCheckedConstructorSeal
    ]

testGF2ZeroAndIdentityBetti :: Assertion
testGF2ZeroAndIdentityBetti = do
  zeroComplex <- expectRight (gf2OneBoundaryComplex 3 2 [])
  zeroGroups <- gf2Betti zeroComplex
  assertEqual "zero differential H0" [2, 3] (fmap freeRank zeroGroups)
  identityComplex <-
    expectRight $
      gf2OneBoundaryComplex
        3
        3
        [ mkBoundaryEntry 0 0 1,
          mkBoundaryEntry 1 1 1,
          mkBoundaryEntry 2 2 1
        ]
  identityGroups <- gf2Betti identityComplex
  assertEqual "identity differential is acyclic" [0, 0] (fmap freeRank identityGroups)

testGF2DuplicateRowsAndEntryCancellation :: Assertion
testGF2DuplicateRowsAndEntryCancellation = do
  duplicateRowComplex <-
    expectRight $
      gf2OneBoundaryComplex
        1
        2
        [ mkBoundaryEntry 0 0 1,
          mkBoundaryEntry 0 1 1
        ]
  duplicateRowGroups <- gf2Betti duplicateRowComplex
  assertEqual "duplicate row rank contributes once" [1, 0] (fmap freeRank duplicateRowGroups)
  cancelledComplex <-
    expectRight $
      gf2OneBoundaryComplex
        1
        1
        [ mkBoundaryEntry 0 0 1,
          mkBoundaryEntry 0 0 1
        ]
  cancelledGroups <- gf2Betti cancelledComplex
  assertEqual "duplicate GF2 entries cancel" [1, 1] (fmap freeRank cancelledGroups)

testGF2ParityConversion :: Assertion
testGF2ParityConversion = do
  integralBoundary <-
    expectRight $
      mkBoundaryIncidence
        4
        1
        [ mkBoundaryEntry 0 0 (2 :: Int),
          mkBoundaryEntry 1 0 (-2 :: Int),
          mkBoundaryEntry 2 0 (1 :: Int),
          mkBoundaryEntry 3 0 (-1 :: Int)
        ]
  let parityBoundary =
        mapBoundaryCoefficients fromIntegral integralBoundary
      parityComplex =
        mkFiniteChainComplex (HomologicalDegree 1) $ \degreeValue ->
          case degreeValue of
            HomologicalDegree 1 ->
              parityBoundary
            HomologicalDegree 0 ->
              emptyBoundaryIncidenceOf 1 0
            _ ->
              emptyBoundaryIncidence
  parityGroups <- gf2Betti parityComplex
  assertEqual "even entries vanish and odd signs survive" [0, 3] (fmap freeRank parityGroups)

testHomologyBackendDispatch :: Assertion
testHomologyBackendDispatch = do
  integralGroups <-
    expectRight
      (runHomologyBackend (IntegralSmithBackend :: HomologyBackend Integer Integer) mooreComplex)
  assertEqual "Smith backend preserves Moore torsion" [[], [2], []] (fmap torsionInvariants integralGroups)
  assertEqual "Smith backend tag" IntegralSmithBackendTag (homologyBackendTag (IntegralSmithBackend :: HomologyBackend Integer Integer))
  rationalComplex <- expectRight rationalIntervalComplex
  rationalGroups <- expectRight (runHomologyBackend RationalRankBackend rationalComplex)
  assertEqual "Rational backend interval Betti" [1, 0] (fmap freeRank rationalGroups)
  assertEqual "Rational backend tag" RationalRankBackendTag (homologyBackendTag RationalRankBackend)
  gf2Complex <- expectRight (gf2OneBoundaryComplex 1 1 [mkBoundaryEntry 0 0 1])
  gf2Groups <- expectRight (runHomologyBackend GF2RankBackend gf2Complex)
  assertEqual "GF2 backend acyclic interval" [0, 0] (fmap freeRank gf2Groups)
  assertEqual "GF2 backend tag" GF2RankBackendTag (homologyBackendTag GF2RankBackend)

testFieldBettiBasicComplexes :: Assertion
testFieldBettiBasicComplexes = do
  let isolatedPoints =
        mkFiniteChainComplex
          (HomologicalDegree 0)
          (const (emptyBoundaryIncidenceOf 4 0 :: BoundaryIncidence Rational))
  isolatedGroups <- rationalBetti isolatedPoints
  assertEqual "four isolated points H0" [4] (fmap freeRank isolatedGroups)
  assertEqual "degree cardinality comes from d0 source dimension" 4 (degreeCardinality isolatedPoints (HomologicalDegree 0))
  intervalComplex <- expectRight rationalIntervalComplex
  intervalGroups <- rationalBetti intervalComplex
  assertEqual "interval Betti" [1, 0] (fmap freeRank intervalGroups)

testFieldBettiRejectsInvalidChains :: Assertion
testFieldBettiRejectsInvalidChains = do
  malformedComplex <- expectRight malformedShapeComplex
  nonNilpotentValue <- expectRight nonNilpotentComplex
  case rationalBettiResult malformedComplex of
    Left (ChainComplexShapeMismatch 1 2 3) ->
      pure ()
    otherResult ->
      assertFailure ("expected malformed shape rejection, got " <> show otherResult)
  case rationalBettiResult nonNilpotentValue of
    Left (ChainComplexNilpotenceViolation 1) ->
      pure ()
    otherResult ->
      assertFailure ("expected non-nilpotent rejection, got " <> show otherResult)

testCheckedConstructorSeal :: Assertion
testCheckedConstructorSeal = do
  malformedComplex <- expectRight malformedShapeComplex
  nonNilpotentValue <- expectRight nonNilpotentComplex
  intervalComplex <- expectRight rationalIntervalComplex
  case resealComplex malformedComplex of
    Left (ChainComplexShapeMismatch 1 2 3) ->
      pure ()
    otherResult ->
      assertFailure ("expected shape refusal at the seal, got " <> show (() <$ otherResult))
  case resealComplex nonNilpotentValue of
    Left (ChainComplexNilpotenceViolation 1) ->
      pure ()
    otherResult ->
      assertFailure ("expected nilpotence refusal at the seal, got " <> show (() <$ otherResult))
  resealedInterval <- expectRight (resealComplex intervalComplex)
  intervalGroups <- rationalBetti resealedInterval
  assertEqual "sealed interval Betti" [1, 0] (fmap freeRank intervalGroups)

resealComplex :: FiniteChainComplex Rational -> Either HomologyFailure (FiniteChainComplex Rational)
resealComplex finite =
  mkFiniteChainComplexChecked (maxHomologicalDegree finite) (incidenceMatrixAt finite)

gf2Betti :: FiniteChainComplex GF2 -> IO [HomologyGroup GF2]
gf2Betti =
  expectRight . computeBettiNumbers (fieldBettiCapability GF2FieldRankBackend :: BettiCapability 'Phase2 GF2)

gf2OneBoundaryComplex ::
  Natural ->
  Natural ->
  [BoundaryEntry GF2] ->
  Either BoundaryIncidenceShapeError (FiniteChainComplex GF2)
gf2OneBoundaryComplex sourceDimension targetDimension entries = do
  boundaryIncidence <- mkBoundaryIncidence sourceDimension targetDimension entries
  pure $
    mkFiniteChainComplex (HomologicalDegree 1) $ \degreeValue ->
      case degreeValue of
        HomologicalDegree 1 ->
          boundaryIncidence
        HomologicalDegree 0 ->
          emptyBoundaryIncidenceOf targetDimension 0
        _ ->
          emptyBoundaryIncidence

rationalBetti :: FiniteChainComplex Rational -> IO [HomologyGroup Rational]
rationalBetti =
  expectRight . rationalBettiResult

rationalBettiResult :: FiniteChainComplex Rational -> Either HomologyFailure [HomologyGroup Rational]
rationalBettiResult =
  computeBettiNumbers (fieldBettiCapability RationalFieldRankBackend :: BettiCapability 'Phase2 Rational)

rationalIntervalComplex :: Either BoundaryIncidenceShapeError (FiniteChainComplex Rational)
rationalIntervalComplex = do
  intervalBoundary <-
    mkBoundaryIncidence
      1
      2
      [ mkBoundaryEntry 0 0 (-1),
        mkBoundaryEntry 0 1 1
      ]
  pure $
    mkFiniteChainComplex (HomologicalDegree 1) $ \degreeValue ->
      case degreeValue of
        HomologicalDegree 1 ->
          intervalBoundary
        HomologicalDegree 0 ->
          emptyBoundaryIncidenceOf 2 0
        _ ->
          emptyBoundaryIncidence

malformedShapeComplex :: Either BoundaryIncidenceShapeError (FiniteChainComplex Rational)
malformedShapeComplex = do
  malformedBoundary <- mkBoundaryIncidence 1 3 []
  pure $
    mkFiniteChainComplex (HomologicalDegree 1) $ \degreeValue ->
      case degreeValue of
        HomologicalDegree 1 ->
          malformedBoundary
        HomologicalDegree 0 ->
          emptyBoundaryIncidenceOf 2 0
        _ ->
          emptyBoundaryIncidence

nonNilpotentComplex :: Either BoundaryIncidenceShapeError (FiniteChainComplex Rational)
nonNilpotentComplex = do
  upperBoundary <- mkBoundaryIncidence 1 1 [mkBoundaryEntry 0 0 1]
  lowerBoundary <- mkBoundaryIncidence 1 1 [mkBoundaryEntry 0 0 1]
  pure $
    mkFiniteChainComplex (HomologicalDegree 2) $ \degreeValue ->
      case degreeValue of
        HomologicalDegree 2 ->
          upperBoundary
        HomologicalDegree 1 ->
          lowerBoundary
        HomologicalDegree 0 ->
          emptyBoundaryIncidenceOf 1 0
        _ ->
          emptyBoundaryIncidence

expectRight ::
  Show left =>
  Either left right ->
  IO right
expectRight result =
  case result of
    Left failureValue ->
      assertFailure (show failureValue)
    Right value ->
      pure value
