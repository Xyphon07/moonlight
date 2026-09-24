
module Main (main) where

import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Kind (Type)
import Data.List.NonEmpty (toList)
import qualified Data.Map.Strict as Map
import Data.Vector qualified as Vector
import Moonlight.Core (mkCapability)
import Moonlight.Homology
import Moonlight.Homology.Boundary.Finite (mkFiniteChainComplex)
import Moonlight.Homology.Boundary.LinAlg
  ( boundaryEntriesBySource,
  )
import Moonlight.Homology.Effect.Laws
import Moonlight.Homology.Effect.Determinism
import BlockSchurSpec qualified
import CompileFailSpec qualified
import FieldBettiSpec qualified
import GF2GraphSpec qualified
import MorseSpec qualified
import PresentationSpec qualified
import ProfileSpec qualified
import RealizationSpec qualified
import SpectralSpec qualified
import TestFixtures
  ( GenuineGoldenCase (..),
    genuineGoldenCorpus,
    triangleCycleComplex,
  )
import TopologySpec qualified
import ZigzagSpec qualified
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import qualified Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "moonlight-homology"
    [ materializeBoundaryTest,
      orderedBoundaryIncidenceConstructorTests,
      FieldBettiSpec.tests,
      GF2GraphSpec.tests,
      BlockSchurSpec.tests,
      materializeBoundaryRejectsMissingTargetTest,
      incidenceScopeGuardrailTest,
      bettiPhase2CapabilityTest,
      spectralPhase4CapabilityTest,
      SpectralSpec.tests,
      CompileFailSpec.tests,
      lawSuiteTests,
      reductionWitnessValidationTests,
      effectiveHomologyConstructionTest,
      goldenCorpusTests,
      determinismHarnessTests,
      MorseSpec.tests,
      PresentationSpec.tests,
      ProfileSpec.tests,
      RealizationSpec.tests,
      TopologySpec.tests,
      ZigzagSpec.tests
    ]

emptyIntFiniteComplex :: FiniteChainComplex Int
emptyIntFiniteComplex =
  mkFiniteChainComplex (HomologicalDegree 0) (const emptyBoundaryIncidence)

identityIntReduction :: Reduction large small Int Int Int
identityIntReduction =
  Reduction
    { projection = ChainMap (\basisValue -> [(1 :: Int, basisValue)]),
      inclusion = ChainMap (\basisValue -> [(1 :: Int, basisValue)]),
      homotopy = ChainHomotopy (const [])
    }

emptyIntBoundary :: Int -> [(Int, Int)]
emptyIntBoundary = const []

failingIntBoundary :: Int -> [(Int, Int)]
failingIntBoundary basisValue = [(1 :: Int, basisValue)]

emptyReductionLawContext :: ReductionLawContext Int Int Int
emptyReductionLawContext =
  ReductionLawContext
    { sampledLargeBasis = [0 :: Int, 1],
      sampledSmallBasis = [0 :: Int, 1],
      largeBoundary = emptyIntBoundary,
      smallBoundary = emptyIntBoundary
    }

failingReductionLawContext :: ReductionLawContext Int Int Int
failingReductionLawContext =
  ReductionLawContext
    { sampledLargeBasis = [0 :: Int, 1],
      sampledSmallBasis = [0 :: Int, 1],
      largeBoundary = failingIntBoundary,
      smallBoundary = emptyIntBoundary
    }

materializeBoundaryTest :: TestTree
materializeBoundaryTest =
  testCase "materializeBoundary builds deterministic incidence entries" $ do
    let boundaryOf simplex =
          case simplex of
            (2 :: Int) -> [(1 :: Int, 10 :: Int), (1, 11)]
            _ -> []
        incidenceResult = materializeIncidenceBoundary boundaryOf [2] [10, 11]
        expectedEntries =
          [ mkBoundaryEntry 0 0 (1 :: Int),
            mkBoundaryEntry 0 1 (1 :: Int)
          ]
    case incidenceResult of
      Left failureValue -> assertFailure ("unexpected failure: " <> show failureValue)
      Right incidence -> assertEqual "incidence entries" expectedEntries (boundaryEntries incidence)

orderedBoundaryIncidenceConstructorTests :: TestTree
orderedBoundaryIncidenceConstructorTests =
  testGroup
    "ordered boundary incidence constructor"
    [ testCase "combines adjacent duplicate coordinates without map canonicalization" $ do
        let entries =
              [ mkBoundaryEntry 0 0 (1 :: Int),
                mkBoundaryEntry 0 0 (-1 :: Int),
                mkBoundaryEntry 0 1 (2 :: Int)
              ]
        assertEqual
          "ordered constructor preserves canonical semantics"
          (mkBoundaryIncidence 2 2 entries)
          (mkBoundaryIncidenceFromOrderedEntries 2 2 entries),
      testCase "falls back to canonical construction for unordered entries" $ do
        let entries =
              [ mkBoundaryEntry 1 0 (3 :: Int),
                mkBoundaryEntry 0 1 (5 :: Int)
              ]
        assertEqual
          "unordered entries still use canonical semantics"
          (mkBoundaryIncidence 2 2 entries)
          (mkBoundaryIncidenceFromOrderedEntries 2 2 entries),
      testCase "glues ordered columns to the canonical source projection" $ do
        let columns =
              Vector.fromList
                [ [mkBoundaryEntry 0 0 (2 :: Int), mkBoundaryEntry 0 1 3],
                  [mkBoundaryEntry 1 1 5]
                ]
            flattenedEntries = concat (Vector.toList columns)
        assertEqual
          "ordered columns retain the same canonical sections"
          (fmap boundaryEntriesBySource (mkBoundaryIncidence 2 2 flattenedEntries))
          (fmap boundaryEntriesBySource (mkBoundaryIncidenceFromOrderedColumns 2 2 columns)),
      testCase "canonicalizes a noncanonical column cover through the general owner" $ do
        let columns =
              Vector.fromList
                [ [mkBoundaryEntry 0 1 (3 :: Int), mkBoundaryEntry 0 0 2],
                  [mkBoundaryEntry 1 1 5]
                ]
            flattenedEntries = concat (Vector.toList columns)
        assertEqual
          "noncanonical columns descend to the canonical source sections"
          (fmap boundaryEntriesBySource (mkBoundaryIncidence 2 2 flattenedEntries))
          (fmap boundaryEntriesBySource (mkBoundaryIncidenceFromOrderedColumns 2 2 columns)),
      testCase "preserves typed bounds obstructions for malformed columns" $
        assertEqual
          "out-of-bounds column entry"
          (Left (BoundaryIncidenceEntryOutOfBounds 0 2 2 2))
          ( mkBoundaryIncidenceFromOrderedColumns
              2
              2
              ( Vector.fromList
                  [ [mkBoundaryEntry 0 2 (1 :: Int)],
                    []
                  ]
              )
          ),
      testCase "rejects out-of-bounds entries before fast construction" $
        assertEqual
          "out-of-bounds entry"
          (Left (BoundaryIncidenceEntryOutOfBounds 2 0 2 2))
          ( mkBoundaryIncidenceFromOrderedEntries
              2
              2
              [mkBoundaryEntry 2 0 (1 :: Int)]
          )
    ]

materializeBoundaryRejectsMissingTargetTest :: TestTree
materializeBoundaryRejectsMissingTargetTest =
  testCase "materializeBoundary rejects targets missing from the basis instead of dropping them" $ do
    let boundaryOf (_ :: Int) = [(1 :: Int, 12 :: Int)]
    assertEqual
      "missing target is rejected"
      (Left (InvalidBoundaryIncidence "boundary target is absent from the target basis"))
      (materializeIncidenceBoundary boundaryOf [2 :: Int] [10 :: Int, 11 :: Int])

incidenceScopeGuardrailTest :: TestTree
incidenceScopeGuardrailTest =
  testCase "portal edges are rejected by incidence materialization" $ do
    let boundaryOf (_ :: Int) =
          [(1 :: Int, ScopedBoundary PortalScope (10 :: Int))]
        result = materializeBoundary boundaryOf [2 :: Int] [10 :: Int]
    assertEqual "portal scope violation" (Left (LawViolation IncidenceScopeLaw)) result

bettiPhase2CapabilityTest :: TestTree
bettiPhase2CapabilityTest =
  testCase "phase 2 betti capability executes reducer" $ do
    assertEqual
      "betti output"
      (Right [HomologyGroup {freeRank = 1, torsionInvariants = [] :: [Int]}])
      ( computeBettiNumbers
          ( mkCapability
              @RequirePhase2
              @'Phase2
              (BettiReducer (\_ -> Right [HomologyGroup {freeRank = 1, torsionInvariants = [] :: [Int]}]))
          )
          emptyIntFiniteComplex
      )

spectralPhase4CapabilityTest :: TestTree
spectralPhase4CapabilityTest =
  testCase "phase 4 spectral capability advances page index" $ do
    let page0 =
          SpectralPage
            { pageIndex = 0,
              groupAt = \_ _ -> HomologyGroup {freeRank = 0, torsionInvariants = [] :: [Int]},
              diffMap = \_ _ -> FormalMap {formalMatrix = [], formalDomainBasis = [], formalCodomainBasis = []},
              pageEntryMap = Map.empty,
              pageDifferentialMap = Map.empty,
              pageAdvanceSource = Nothing,
              pageAdvanceState = Nothing
            }
    assertEqual
      "advanced index"
      (Right 1)
      ( pageIndex
          <$> nextPage
            (mkCapability @RequirePhase4 @'Phase4 (SpectralAdvance (\page -> Right (page {pageIndex = pageIndex page + 1}))))
            page0
      )

lawSuiteTests :: TestTree
lawSuiteTests =
  testGroup
    "laws"
    [ testCase "boundary nilpotence holds on oriented interval complex" $ do
        assertEqual "nilpotence" (Right ()) (checkBoundaryNilpotence orientedIntervalBoundary [V0, V1, E01]),
      testCase "reduction laws hold for identity reduction" $ do
        assertEqual "left inverse" (Right ()) (checkReductionLeftInverse identityIntReduction [0 :: Int, 1, 2])
        assertEqual "homotopy" (Right ()) (checkReductionHomotopy emptyIntBoundary identityIntReduction [0 :: Int, 1, 2]),
      testCase "reduction law harness accepts a real Morse reduction on the triangle cycle" realMorseReductionLawHarnessTest,
      QC.testProperty "normalization is idempotent" $
        \(terms :: [(Int, Int)]) ->
          normalizeCombination (normalizeCombination terms) == normalizeCombination terms
    ]

reductionWitnessValidationTests :: TestTree
reductionWitnessValidationTests =
  testCase "effective homology validation accumulates sampled law violations" $ do
    case mkEffectiveHomology "large" "small" identityIntReduction (mkReductionChecksFromSamples failingReductionLawContext) emptyIntFiniteComplex of
      Invalid violations ->
        let expectedViolations =
              [ ProjectionChainMapViolation (LawViolation ReductionProjectionChainMapLaw)
              , InclusionChainMapViolation (LawViolation ReductionInclusionChainMapLaw)
              ]
         in assertBool
              "expected both chain-map violations"
              (expectedViolations & all (`elem` toList violations))
      Valid _ -> assertFailure "expected sampled reduction validation failure"

effectiveHomologyConstructionTest :: TestTree
effectiveHomologyConstructionTest =
  testCase "effective homology constructor requires validated reduction" $ do
    case mkEffectiveHomology "large" "small" identityIntReduction (mkReductionChecksFromSamples emptyReductionLawContext) emptyIntFiniteComplex of
      Invalid violations ->
        assertFailure ("unexpected effective homology construction failure: " <> show (toList violations))
      Valid effective ->
        assertEqual "source retained" "large" (sourceComplex effective)

type IntervalCell :: Type
data IntervalCell
  = V0
  | V1
  | E01
  deriving stock (Eq, Ord, Show)

orientedIntervalBoundary :: IntervalCell -> [(Int, IntervalCell)]
orientedIntervalBoundary basis =
  case basis of
    V0 -> []
    V1 -> []
    E01 -> [(1 :: Int, V1), (-1, V0)]

goldenCorpusTests :: TestTree
goldenCorpusTests =
  testGroup
    "golden-corpus"
    [ testCase "genuine golden corpus includes interval, triangle cycle, and tetrahedron boundary" $
        assertEqual
          "space names"
          ["interval", "triangle cycle", "tetrahedron boundary"]
          (fmap genuineGoldenName genuineGoldenCorpus),
      testCase "genuine golden corpus Betti vectors are computed by the Smith backend" $
        traverse_ assertGenuineGoldenBetti genuineGoldenCorpus,
      testCase "Euler characteristic compares cell counts against pipeline Betti numbers" $
        traverse_ assertGenuineGoldenEuler genuineGoldenCorpus,
      testCase "genuine golden fixtures are not all-zero boundary theater" $
        assertEqual
          "nonzero boundary support"
          [True, True, True]
          (fmap (hasNonzeroBoundary . genuineGoldenComplex) genuineGoldenCorpus)
    ]

determinismHarnessTests :: TestTree
determinismHarnessTests =
  testGroup
    "determinism"
    [ testCase "boundary fingerprints are permutation invariant" $ do
        let basisFingerprintA = fingerprintBasis ([3 :: Int, 1, 2])
            basisFingerprintB = fingerprintBasis ([2 :: Int, 3, 1])
            reductionFingerprintA = fingerprintReductionImage [(2 :: Int, "alpha"), (3, "beta")]
            reductionFingerprintB = fingerprintReductionImage [(3 :: Int, "beta"), (2, "alpha")]
        assertEqual "basis fingerprint invariant" basisFingerprintA basisFingerprintB
        assertEqual "reduction fingerprint invariant" reductionFingerprintA reductionFingerprintB,
      testCase "genuine golden complex fingerprints are deterministic" $ do
        let stablePairs =
              genuineGoldenCorpus
                & fmap (\goldenCase -> let fingerprintValue = fingerprintFiniteChainComplex (genuineGoldenComplex goldenCase) in [fingerprintValue, fingerprintValue])
            pairResults =
              stablePairs
                & fmap verifyDeterministicFingerprints
                & all (either (const False) (const True))
        assertBool "pairwise deterministic verification" pairResults
    ]

realMorseReductionLawHarnessTest :: IO ()
realMorseReductionLawHarnessTest =
  case morseComplex triangleCycleComplex (acyclicMatching triangleCycleComplex (const 0)) of
    Left failureValue ->
      assertFailure ("unexpected Morse construction failure: " <> show failureValue)
    Right morseValue -> do
      let reduction =
            Reduction
              { projection = mcProjection morseValue,
                inclusion = mcInclusion morseValue,
                homotopy = mcHomotopy morseValue
              }
          reducedComplex = mcReducedComplex morseValue
          lawContext =
            ReductionLawContext
              { sampledLargeBasis = basisRefsOf triangleCycleComplex,
                sampledSmallBasis = basisRefsOf reducedComplex,
                largeBoundary = boundaryOfBasisRef triangleCycleComplex,
                smallBoundary = boundaryOfBasisRef reducedComplex
              }
      assertBool
        "Morse reduction must actually collapse cells"
        (length (sampledSmallBasis lawContext) < length (sampledLargeBasis lawContext))
      case mkReductionWitness reduction (mkReductionChecksFromSamples lawContext) of
        Invalid violations ->
          assertFailure ("unexpected Morse reduction law violations: " <> show (toList violations))
        Valid _ ->
          pure ()

assertGenuineGoldenBetti :: GenuineGoldenCase -> IO ()
assertGenuineGoldenBetti goldenCase = do
  groups <- expectHomologyGroups goldenCase
  assertEqual
    (genuineGoldenName goldenCase <> " Betti vector")
    (genuineGoldenBetti goldenCase)
    (fmap freeRank groups)

assertGenuineGoldenEuler :: GenuineGoldenCase -> IO ()
assertGenuineGoldenEuler goldenCase = do
  groups <- expectHomologyGroups goldenCase
  assertEqual
    (genuineGoldenName goldenCase <> " Euler characteristic")
    (alternatingSum (genuineGoldenCellCounts goldenCase))
    (alternatingSum (fmap freeRank groups))

expectHomologyGroups :: GenuineGoldenCase -> IO [HomologyGroup Integer]
expectHomologyGroups goldenCase =
  case runHomologyBackend (IntegralSmithBackend :: HomologyBackend Integer Integer) (genuineGoldenComplex goldenCase) of
    Left failureValue ->
      assertFailure ("unexpected homology failure for " <> genuineGoldenName goldenCase <> ": " <> show failureValue)
    Right groups ->
      pure groups

alternatingSum :: [Int] -> Int
alternatingSum values =
  sum (zipWith signedTerm [0 :: Int ..] values)

signedTerm :: Int -> Int -> Int
signedTerm indexValue value =
  if even indexValue
    then value
    else negate value

basisRefsOf :: FiniteChainComplex r -> [BasisCellRef]
basisRefsOf finite =
  case maxHomologicalDegree finite of
    HomologicalDegree maxDegreeValue ->
      [0 .. maxDegreeValue]
        >>= finiteChainBasisRefsAtDegree finite . HomologicalDegree

boundaryOfBasisRef :: FiniteChainComplex r -> BasisCellRef -> [(r, BasisCellRef)]
boundaryOfBasisRef finite basisCellRef =
  case cellDegree basisCellRef of
    HomologicalDegree degreeValue
      | degreeValue <= 0 -> []
      | otherwise ->
          incidenceMatrixAt finite (HomologicalDegree degreeValue)
            & boundaryEntries
            & filter ((== cellIndex basisCellRef) . sourceIndex)
            & fmap
              ( \entry ->
                  ( boundaryCoefficient entry,
                    BasisCellRef
                      { cellDegree = HomologicalDegree (degreeValue - 1),
                        cellIndex = targetIndex entry
                      }
                  )
              )

hasNonzeroBoundary :: FiniteChainComplex r -> Bool
hasNonzeroBoundary finite =
  basisRefsOf finite
    & any (not . null . boundaryOfBasisRef finite)
