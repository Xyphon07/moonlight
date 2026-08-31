module BlockSchurSpec
  ( tests,
  )
where

import Data.Ratio ((%))
import Moonlight.Core (Semiring)
import Moonlight.Homology
  ( BasisBlock (..),
    BlockSchurFailure (..),
    BlockSchurPivot (..),
    BlockSchurReduction (..),
    BlockSchurTranscript (..),
    BoundaryEntry,
    BoundaryIncidence,
    FiniteChainComplex,
    HomologicalDegree (..),
    HomologyBackend (..),
    blockSchurReduceWith,
    boundaryCoefficient,
    boundaryEntries,
    emptyBoundaryIncidenceOf,
    freeRank,
    gf2BlockPivotOps,
    integerUnimodularBlockPivotOps,
    incidenceMatrixAt,
    mkBoundaryEntry,
    mkBoundaryIncidenceFromOrderedEntries,
    rationalBlockPivotOps,
    sourceCardinality,
    sourceIndex,
    targetIndex,
  )
import Moonlight.Homology.Boundary.Finite (mkFiniteChainComplex)
import Moonlight.Homology.Effect.Laws
  ( BlockSchurHomologyAgreement (..),
    checkBlockSchurHomologyAgreement,
  )
import Moonlight.LinAlg.Pure.Dense.Block
  ( BlockMatrixFailure (..),
  )
import Moonlight.LinAlg.Pure.Dense.GF2 (GF2 (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "block Schur reduction"
    [ testCase "rank-2 block interval cancels in one pivot" testRankTwoBlockInterval,
      testCase "Schur residual uses C - B P^-1 A" testSchurResidual,
      testCase "integer non-unimodular pivot is rejected" testIntegerNonUnimodularRejected,
      testGroup
        "laws"
        [ testCase "rank-2 block interval preserves integral homology" testIntegralAgreementLaw,
          testCase "GF2 block pivot preserves field Betti" testGF2AgreementLaw
        ]
    ]

testRankTwoBlockInterval :: Assertion
testRankTwoBlockInterval = do
  reduction <- rankTwoBlockIntervalReduction
  assertEqual "one block pivot matrix" [[1, 0], [0, 1]] (bstPivotMatrix (bsrTranscript reduction))
  assertEqual "all degree-one sources are removed" 0 (sourceCardinality (incidenceMatrixAt (bsrReducedComplex reduction) (HomologicalDegree 1)))
  assertEqual "all degree-zero targets are removed" 0 (sourceCardinality (incidenceMatrixAt (bsrReducedComplex reduction) (HomologicalDegree 0)))

testSchurResidual :: Assertion
testSchurResidual = do
  boundary <- boundaryMatrix 2 2 [(0, 0, 1 :: Rational), (1, 0, 2), (0, 1, 3), (1, 1, 5)]
  reduction <-
    expectRight $
      blockSchurReduceWith
        rationalBlockPivotOps
        (oneBoundaryComplex 2 boundary)
        (pivotAt 1 [0] [0])
  assertEqual
    "residual boundary entry"
    [((0, 0), (-1) :: Rational)]
    (entrySummary <$> boundaryEntries (bstResidualBoundary (bsrTranscript reduction)))

testIntegerNonUnimodularRejected :: Assertion
testIntegerNonUnimodularRejected = do
  boundary <- boundaryMatrix 1 1 [(0, 0, 2 :: Integer)]
  case blockSchurReduceWith
    integerUnimodularBlockPivotOps
    (oneBoundaryComplex 1 boundary)
    (pivotAt 1 [0] [0]) of
    Left failure ->
      assertEqual
        "non-unimodular integer pivot rejected"
        (BlockSchurPivotMatrixFailed (BlockMatrixNonUnimodular [[1 % 2]]))
        failure
    Right _ -> assertFailure "non-unimodular block unexpectedly reduced"

testIntegralAgreementLaw :: Assertion
testIntegralAgreementLaw = do
  reduction <- rankTwoBlockIntervalReduction
  agreement <- expectRight $ checkBlockSchurHomologyAgreement IntegralSmithBackend reduction
  assertEqual "integral homology agreement is empty-rank" [0, 0] (fmap (freeRank . snd) (bshaGroupsByDegree agreement))

testGF2AgreementLaw :: Assertion
testGF2AgreementLaw = do
  reduction <- gf2SingleIntervalReduction
  agreement <- expectRight $ checkBlockSchurHomologyAgreement GF2RankBackend reduction
  assertEqual "GF2 homology agreement" [0, 0] (fmap (freeRank . snd) (bshaGroupsByDegree agreement))

rankTwoBlockIntervalReduction :: IO (BlockSchurReduction Integer)
rankTwoBlockIntervalReduction = do
  boundary <- boundaryMatrix 2 2 [(0, 0, 1 :: Integer), (1, 1, 1)]
  expectRight $
    blockSchurReduceWith
      integerUnimodularBlockPivotOps
      (oneBoundaryComplex 2 boundary)
      (pivotAt 1 [0, 1] [0, 1])

gf2SingleIntervalReduction :: IO (BlockSchurReduction GF2)
gf2SingleIntervalReduction = do
  boundary <- boundaryMatrix 1 1 [(0, 0, GF2One)]
  expectRight $
    blockSchurReduceWith
      gf2BlockPivotOps
      (oneBoundaryComplex 1 boundary)
      (pivotAt 1 [0] [0])

oneBoundaryComplex :: Int -> BoundaryIncidence coefficient -> FiniteChainComplex coefficient
oneBoundaryComplex degreeZeroDimension boundary =
  mkFiniteChainComplex
    (HomologicalDegree 1)
    ( \degreeValue ->
        case degreeValue of
          HomologicalDegree 0 -> emptyBoundaryIncidenceOf (fromIntegral degreeZeroDimension) 0
          HomologicalDegree 1 -> boundary
          _ -> emptyBoundaryIncidenceOf 0 0
    )

pivotAt :: Int -> [Int] -> [Int] -> BlockSchurPivot
pivotAt upperDegree upperIndices lowerIndices =
  BlockSchurPivot
    { bspUpperBlock = BasisBlock (HomologicalDegree upperDegree) upperIndices,
      bspLowerBlock = BasisBlock (HomologicalDegree (upperDegree - 1)) lowerIndices
    }

boundaryMatrix :: (Eq coefficient, Semiring coefficient) => Int -> Int -> [(Int, Int, coefficient)] -> IO (BoundaryIncidence coefficient)
boundaryMatrix sourceCount targetCount entries =
  expectRight $
    mkBoundaryIncidenceFromOrderedEntries
      (fromIntegral sourceCount)
      (fromIntegral targetCount)
      [ mkBoundaryEntry (fromIntegral sourceIndexValue) (fromIntegral targetIndexValue) coefficientValue
        | (sourceIndexValue, targetIndexValue, coefficientValue) <- entries
      ]

entrySummary :: BoundaryEntry coefficient -> ((Int, Int), coefficient)
entrySummary entry =
  ((sourceIndex entry, targetIndex entry), boundaryCoefficient entry)

expectRight :: Show failure => Either failure value -> IO value
expectRight result =
  case result of
    Right value -> pure value
    Left failureValue -> assertFailure ("unexpected failure: " <> show failureValue)
