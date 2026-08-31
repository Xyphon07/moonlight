{-# LANGUAGE DataKinds #-}

module PresentationSpec
  ( tests,
  )
where

import Moonlight.Homology
import Moonlight.Homology.Presentation
  ( ChainBuildError (..),
    ChainSpec (..),
    compileChain,
  )
import Moonlight.LinAlg.Pure.Dense.GF2 (GF2)
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
    "equational chain presentation"
    [ testCase "simplicial circle has Betti (1,1) over the rationals" testCircleAnchor,
      testCase "boundary of the tetrahedron has Betti (1,0,1) over the rationals" testSphereAnchor,
      testCase "CW torus has Betti (1,2,1) over the rationals" testTorusAnchor,
      testCase "CW projective plane separates rational and GF2 Betti" testProjectivePlaneAnchor,
      testCase "compile refuses ragged, out-of-bounds, and non-nilpotent specs" testCompileRefusals
    ]

testCircleAnchor :: Assertion
testCircleAnchor = do
  circleComplex <- expectCompiled (compileChain simplicialCircle)
  circleGroups <- rationalBetti circleComplex
  assertEqual "circle Betti" [1, 1] (fmap freeRank circleGroups)

testSphereAnchor :: Assertion
testSphereAnchor = do
  sphereComplex <- expectCompiled (compileChain tetrahedronBoundary)
  sphereGroups <- rationalBetti sphereComplex
  assertEqual "sphere Betti" [1, 0, 1] (fmap freeRank sphereGroups)

testTorusAnchor :: Assertion
testTorusAnchor = do
  torusComplex <- expectCompiled (compileChain cwTorus)
  torusGroups <- rationalBetti torusComplex
  assertEqual "torus Betti" [1, 2, 1] (fmap freeRank torusGroups)

testProjectivePlaneAnchor :: Assertion
testProjectivePlaneAnchor = do
  rationalComplex <- expectCompiled (compileChain (cwProjectivePlane :: ChainSpec Rational))
  rationalGroups <- rationalBetti rationalComplex
  assertEqual "rational projective plane Betti" [1, 0, 0] (fmap freeRank rationalGroups)
  parityComplex <- expectCompiled (compileChain (cwProjectivePlane :: ChainSpec GF2))
  parityGroups <- gf2Betti parityComplex
  assertEqual "GF2 projective plane Betti" [1, 1, 1] (fmap freeRank parityGroups)

testCompileRefusals :: Assertion
testCompileRefusals = do
  expectRefusal
    (ChainBuildBoundaryCountMismatch 1 0)
    (compileChain (ChainSpec {chainCellCounts = [2, 2], chainBoundaries = []} :: ChainSpec Rational))
  case compileChain (ChainSpec {chainCellCounts = [1, 1], chainBoundaries = [[(0, 5, 1)]]} :: ChainSpec Rational) of
    Left (ChainBuildIncidenceFault 1 _) ->
      pure ()
    otherResult ->
      assertFailure ("expected incidence refusal, got " <> show (() <$ otherResult))
  expectRefusal
    (ChainBuildComplexFault (ChainComplexNilpotenceViolation 1))
    ( compileChain
        ( ChainSpec
            { chainCellCounts = [1, 1, 1],
              chainBoundaries = [[(0, 0, 1)], [(0, 0, 1)]]
            } ::
            ChainSpec Rational
        )
    )
  where
    expectRefusal :: ChainBuildError -> Either ChainBuildError (FiniteChainComplex Rational) -> Assertion
    expectRefusal expectedFault result =
      case result of
        Left observedFault ->
          assertEqual "refusal fault" expectedFault observedFault
        Right _ ->
          assertFailure "expected refusal, got acceptance"

simplicialCircle :: ChainSpec Rational
simplicialCircle =
  ChainSpec
    { chainCellCounts = [3, 3],
      chainBoundaries =
        [ [ (0, 1, 1),
            (0, 0, -1),
            (1, 2, 1),
            (1, 0, -1),
            (2, 2, 1),
            (2, 1, -1)
          ]
        ]
    }

tetrahedronBoundary :: ChainSpec Rational
tetrahedronBoundary =
  ChainSpec
    { chainCellCounts = [4, 6, 4],
      chainBoundaries =
        [ [ (0, 1, 1),
            (0, 0, -1),
            (1, 2, 1),
            (1, 0, -1),
            (2, 3, 1),
            (2, 0, -1),
            (3, 2, 1),
            (3, 1, -1),
            (4, 3, 1),
            (4, 1, -1),
            (5, 3, 1),
            (5, 2, -1)
          ],
          [ (0, 3, 1),
            (0, 1, -1),
            (0, 0, 1),
            (1, 4, 1),
            (1, 2, -1),
            (1, 0, 1),
            (2, 5, 1),
            (2, 2, -1),
            (2, 1, 1),
            (3, 5, 1),
            (3, 4, -1),
            (3, 3, 1)
          ]
        ]
    }

cwTorus :: ChainSpec Rational
cwTorus =
  ChainSpec
    { chainCellCounts = [1, 2, 1],
      chainBoundaries = [[], []]
    }

cwProjectivePlane :: Num r => ChainSpec r
cwProjectivePlane =
  ChainSpec
    { chainCellCounts = [1, 1, 1],
      chainBoundaries = [[], [(0, 0, 2)]]
    }

rationalBetti :: FiniteChainComplex Rational -> IO [HomologyGroup Rational]
rationalBetti =
  expectRight
    . computeBettiNumbers (fieldBettiCapability RationalFieldRankBackend :: BettiCapability 'Phase2 Rational)

gf2Betti :: FiniteChainComplex GF2 -> IO [HomologyGroup GF2]
gf2Betti =
  expectRight
    . computeBettiNumbers (fieldBettiCapability GF2FieldRankBackend :: BettiCapability 'Phase2 GF2)

expectCompiled :: Either ChainBuildError (FiniteChainComplex r) -> IO (FiniteChainComplex r)
expectCompiled =
  either (assertFailure . ("compileChain refused a lawful spec: " <>) . show) pure

expectRight :: Show left => Either left right -> IO right
expectRight =
  either (assertFailure . show) pure
