{-# LANGUAGE DataKinds #-}

module MorseSpec
  ( tests,
  )
where

import Data.Function ((&))
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import qualified Data.Set as Set
import Moonlight.Core (Semiring)
import qualified Moonlight.Homology as H
import qualified Moonlight.Homology.Boundary.Finite as H (mkFiniteChainComplex)
import TestFixtures
  ( GenuineGoldenCase (..),
    genuineGoldenCorpus,
    intervalComplex,
    mooreComplex,
    tetrahedronBoundaryMissingFaceComplex,
    triangleCycleComplex,
  )
import Moonlight.LinAlg.Pure.Dense.GF2 (GF2 (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), Assertion, assertBool, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "morse"
    [ testCase "unit incidences collapse while non-units remain critical" testUnitIncidenceFiltering,
      testCase "localized matching collapses non-unit incidences over Q" testLocalizedMatching,
      testCase "localized edge reversal uses reciprocal weights" testLocalizedEdgeReversal,
      testCase "generic algebraic Morse accepts rational nonunit pivots through compact reduction" testGenericRationalMorse,
      testCase "generic algebraic Morse accepts GF2 pivots and preserves field Betti" testGF2MorsePreservesFieldBetti,
      testCase "refined transcript preserves the simple localized collapse" testRefinedLocalizedMatching,
      testCase "refined transcript captures a second-pass collapse" testRefinedMatchingCapturesSecondPassCollapse,
      testCase "refined Morse complex exposes final rational reduction" testRefinedMorseComplexExposesFinalReduction,
      testCase "filtered refined Morse only collapses filtration-compatible pairs" testFilteredRefinedMorseCompatibility,
      testCase "critical-basis filtration transfer reports missing provenance as a typed obstruction" testMissingCriticalBasisProvenance,
      testCase "refined transcript reports terminality from final critical degrees" testRefinedMatchingTerminality,
      testCase "refined transcript propagates reduction failure as a typed obstruction" testRefinedTranscriptFailurePropagation,
      testCase "cyclic modified Hasse matchings are rejected with witnesses" testAcyclicityRejection,
      testCase "Morse differential remains nilpotent on triangle cycle" (assertMorseNilpotence "triangle cycle" triangleCycleComplex),
      testCase "Morse differential remains nilpotent on interval" (assertMorseNilpotence "interval" intervalComplex),
      testCase "Morse exposes projection and inclusion chain maps on interval" testIntervalMorseMaps,
      testCase "Morse differential differs from boundary restriction on triangle cycle" testBoundaryRestrictionDifference,
      goldenBettiPreservationTests
    ]

testUnitIncidenceFiltering :: Assertion
testUnitIncidenceFiltering =
  let triangleMatching = H.acyclicMatching triangleCycleComplex (const 0)
      mooreMatching = H.acyclicMatching mooreComplex (const 0)
   in do
        assertBool
          "triangle complex should admit unit-incidence matches"
          (not (null (H.amPairs triangleMatching)))
        H.amPairs mooreMatching @?= []
        length (H.amCriticalCells mooreMatching) @?= 3

testLocalizedMatching :: Assertion
testLocalizedMatching =
  let matchingValue = H.acyclicMatchingLocalized mooreComplex (const 0)
      expectedPair =
        H.LocalizedAcyclicPair
          { H.lapLowerCell = basisCellAt 1 0,
            H.lapUpperCell = basisCellAt 2 0,
            H.lapIncidenceCoefficient = 2 % 1
          }
   in do
        H.lamPairs matchingValue @?= [expectedPair]
        assertBool
          "localized matcher should accept non-unit incidences"
          (H.isAcyclicMatchingLocalized mooreComplex matchingValue)
        case H.morseComplexLocalized mooreComplex matchingValue of
          Left failureValue ->
            assertFailure ("unexpected localized Morse construction failure: " <> show failureValue)
          Right morseValue -> do
            let reducedComplexValue = H.lmcReducedComplex morseValue
            H.sourceCardinality (H.incidenceMatrixAt reducedComplexValue (H.HomologicalDegree 2)) @?= 0
            H.sourceCardinality (H.incidenceMatrixAt reducedComplexValue (H.HomologicalDegree 1)) @?= 0
            H.sourceCardinality (H.incidenceMatrixAt reducedComplexValue (H.HomologicalDegree 0)) @?= 1
            H.lmcCriticalBasis morseValue
              @?= Map.fromList [(basisCellAt 0 0, basisCellAt 0 0)]

testLocalizedEdgeReversal :: Assertion
testLocalizedEdgeReversal =
  let lowerCell = basisCellAt 1 0
      upperCell = basisCellAt 2 0
      candidatePair =
        H.LocalizedAcyclicPair
          { H.lapLowerCell = lowerCell,
            H.lapUpperCell = upperCell,
            H.lapIncidenceCoefficient = 2 % 1
          }
      edgeMap = Map.fromList [((upperCell, lowerCell), 2 % 1)]
   in H.reverseCandidateEdgeLocalized candidatePair edgeMap
        @?= Map.fromList [((lowerCell, upperCell), (-1) % 2)]

testGenericRationalMorse :: Assertion
testGenericRationalMorse =
  let rationalComplex = H.rationalizeFiniteChainComplex mooreComplex
      matchingValue = H.acyclicMatchingWith H.rationalMorsePivotOps rationalComplex (const 0)
      expectedPair =
        H.LocalizedAcyclicPair
          { H.lapLowerCell = basisCellAt 1 0,
            H.lapUpperCell = basisCellAt 2 0,
            H.lapIncidenceCoefficient = 2 % 1
          }
   in do
        H.lamPairs matchingValue @?= [expectedPair]
        case H.morseComplexWith H.rationalMorsePivotOps rationalComplex matchingValue of
          Left failureValue ->
            assertFailure ("unexpected generic rational Morse construction failure: " <> show failureValue)
          Right morseValue -> do
            H.lmcCriticalBasis morseValue
              @?= Map.fromList [(basisCellAt 0 0, basisCellAt 0 0)]

testGF2MorsePreservesFieldBetti :: Assertion
testGF2MorsePreservesFieldBetti = do
  let matchingValue = H.acyclicMatchingWith H.gf2MorsePivotOps gf2IdentityComplex (const 0)
      expectedPair =
        H.LocalizedAcyclicPair
          { H.lapLowerCell = basisCellAt 0 0,
            H.lapUpperCell = basisCellAt 1 0,
            H.lapIncidenceCoefficient = GF2One
          }
  H.lamPairs matchingValue @?= [expectedPair]
  case H.morseComplexWith H.gf2MorsePivotOps gf2IdentityComplex matchingValue of
    Left failureValue ->
      assertFailure ("unexpected GF2 Morse construction failure: " <> show failureValue)
    Right morseValue -> do
      originalRanks <- gf2BettiRanks gf2IdentityComplex
      reducedRanks <- gf2BettiRanks (H.lmcReducedComplex morseValue)
      reducedRanks @?= originalRanks
      H.lmcCriticalBasis morseValue @?= Map.empty

testRefinedLocalizedMatching :: Assertion
testRefinedLocalizedMatching = do
  refinedTranscript <- expectRight (H.refinedAcyclicMatchingTranscript mooreComplex (const 0))
  let localizedMatching = H.acyclicMatchingLocalized mooreComplex (const 0)
      refinedMatching = H.flattenRefinedAcyclicMatching refinedTranscript
      refinedStagePairs = H.mapRefinedStages (H.lamPairs . H.refinedStageMatching) refinedTranscript
      refinedCriticalCells = H.refinedMatchingCriticalCells refinedTranscript
  H.lamPairs refinedMatching @?= H.lamPairs localizedMatching
  H.lamCriticalCells refinedMatching @?= H.lamCriticalCells localizedMatching
  H.lamObstructions refinedMatching @?= H.lamObstructions localizedMatching
  H.hasRefinedStages refinedTranscript @?= True
  H.refinedStageCount refinedTranscript @?= 1
  H.isTerminalRefinedMatching refinedTranscript @?= True
  H.finalRefinedCriticalDegrees refinedTranscript @?= [H.HomologicalDegree 0]
  H.finalRefinedCriticalCellCount refinedTranscript @?= 1
  H.finalRefinedCriticalDegreeHistogram refinedTranscript
    @?= Map.fromList [(H.HomologicalDegree 0, 1)]
  H.finalRefinedHomologicalSupport refinedTranscript
    @?= Set.fromList [H.HomologicalDegree 0]
  H.finalRefinedMaxCriticalDegree refinedTranscript
    @?= Just (H.HomologicalDegree 0)
  H.refinedMatchingSummary refinedTranscript
    @?= H.RefinedMatchingSummary
      { H.rmsStageCount = 1,
        H.rmsHasStages = True,
        H.rmsIsTerminal = True,
        H.rmsFinalCriticalCellCount = 1,
        H.rmsFinalCriticalDegreeHistogram = Map.fromList [(H.HomologicalDegree 0, 1)],
        H.rmsFinalHomologicalSupport = Set.fromList [H.HomologicalDegree 0],
        H.rmsFinalMaxCriticalDegree = Just (H.HomologicalDegree 0)
      }
  refinedCriticalCells @?= H.lamCriticalCells localizedMatching
  refinedStagePairs @?= [H.lamPairs localizedMatching]

testRefinedMatchingCapturesSecondPassCollapse :: Assertion
testRefinedMatchingCapturesSecondPassCollapse = do
  refinedTranscript <- expectRight (H.refinedAcyclicMatchingTranscript tetrahedronBoundaryMissingFaceComplex (const 0))
  let refinedMatching = H.flattenRefinedAcyclicMatching refinedTranscript
      (refinedStageSummaries, refinedCriticalCells) =
        H.summarizeRefinedMatching
          ( \stageValue ->
              [ ( length (H.lamPairs (H.refinedStageMatching stageValue)),
                  fmap H.cellDegree (H.lamCriticalCells (H.refinedStageMatching stageValue)),
                  maybe False (const True) (H.refinedStageReducedComplex stageValue)
                )
              ]
          )
          refinedTranscript
  H.hasRefinedStages refinedTranscript @?= True
  H.refinedStageCount refinedTranscript @?= 2
  H.isTerminalRefinedMatching refinedTranscript @?= True
  H.finalRefinedCriticalDegrees refinedTranscript @?= [H.HomologicalDegree 0]
  H.finalRefinedCriticalCellCount refinedTranscript @?= 1
  H.finalRefinedCriticalDegreeHistogram refinedTranscript
    @?= Map.fromList [(H.HomologicalDegree 0, 1)]
  H.finalRefinedHomologicalSupport refinedTranscript
    @?= Set.fromList [H.HomologicalDegree 0]
  H.finalRefinedMaxCriticalDegree refinedTranscript
    @?= Just (H.HomologicalDegree 0)
  refinedStageSummaries
    @?= [ (5, fmap H.HomologicalDegree [0, 1, 2], True),
          (1, [H.HomologicalDegree 0], True)
        ]
  length (H.lamPairs refinedMatching) @?= 6
  fmap H.cellDegree refinedCriticalCells
    @?= [H.HomologicalDegree 0]
  fmap H.cellDegree (H.lamCriticalCells refinedMatching)
    @?= [H.HomologicalDegree 0]

testRefinedMorseComplexExposesFinalReduction :: Assertion
testRefinedMorseComplexExposesFinalReduction =
  case H.refinedMorseComplex tetrahedronBoundaryMissingFaceComplex (const 0) of
    Left failureValue ->
      assertFailure ("unexpected refined Morse construction failure: " <> show failureValue)
    Right refinedComplex -> do
      let transcriptValue = H.rmcTranscript refinedComplex
          reducedComplexValue = H.rmcReducedComplex refinedComplex
          criticalBasisValue = H.rmcCriticalBasis refinedComplex
      H.refinedStageCount transcriptValue @?= 2
      H.sourceCardinality (H.incidenceMatrixAt reducedComplexValue (H.HomologicalDegree 0)) @?= 1
      H.sourceCardinality (H.incidenceMatrixAt reducedComplexValue (H.HomologicalDegree 1)) @?= 0
      H.sourceCardinality (H.incidenceMatrixAt reducedComplexValue (H.HomologicalDegree 2)) @?= 0
      Map.keys criticalBasisValue @?= [basisCellAt 0 0]
      fmap H.cellDegree (Map.elems criticalBasisValue) @?= [H.HomologicalDegree 0]

testFilteredRefinedMorseCompatibility :: Assertion
testFilteredRefinedMorseCompatibility =
  case H.filteredRefinedMorseComplex intervalComplex intervalFiltration (const 0) of
    Left failureValue ->
      assertFailure ("unexpected filtered refined Morse construction failure: " <> show failureValue)
    Right filteredComplex -> do
      let refinedComplex = H.frmcRefinedMorseComplex filteredComplex
          pairWitnesses = H.fmcPairWitnesses (H.frmcCompatibility filteredComplex)
          reducedFiltration = H.frmcReducedFiltrationByBasis filteredComplex
      pairWitnesses
        @?= [ H.FilteredMorsePairWitness
                { H.fmpwLowerCell = basisCellAt 0 1,
                  H.fmpwUpperCell = basisCellAt 1 0,
                  H.fmpwFiltrationLevel = 1
                }
            ]
      H.sourceCardinality (H.incidenceMatrixAt (H.rmcReducedComplex refinedComplex) (H.HomologicalDegree 0)) @?= 1
      reducedFiltration @?= Map.fromList [(basisCellAt 0 0, 0)]

testMissingCriticalBasisProvenance :: Assertion
testMissingCriticalBasisProvenance =
  case H.reducedFiltrationByCriticalBasis (H.rationalizeFiniteChainComplex intervalComplex) Map.empty intervalFiltration of
    Left (H.MissingCriticalBasisProvenance missingBasisRef) ->
      missingBasisRef @?= basisCellAt 0 0
    Left failureValue ->
      assertFailure ("expected missing-provenance obstruction, received " <> show failureValue)
    Right filtrationValue ->
      assertFailure ("expected missing-provenance obstruction, received filtration " <> show filtrationValue)

intervalFiltration :: H.BasisCellRef -> Int
intervalFiltration basisCellRef =
  case (H.cellDegree basisCellRef, H.cellIndex basisCellRef) of
    (H.HomologicalDegree 0, 0) -> 0
    (H.HomologicalDegree 0, 1) -> 1
    (H.HomologicalDegree 1, 0) -> 1
    _ -> 0

testRefinedMatchingTerminality :: Assertion
testRefinedMatchingTerminality = do
  refinedTranscript <- expectRight (H.refinedAcyclicMatchingTranscript triangleCycleComplex (const 0))
  H.hasRefinedStages refinedTranscript @?= True
  H.isTerminalRefinedMatching refinedTranscript @?= False
  H.finalRefinedCriticalCellCount refinedTranscript @?= 2
  List.sort (H.finalRefinedCriticalDegrees refinedTranscript)
    @?= [H.HomologicalDegree 0, H.HomologicalDegree 1]
  H.finalRefinedCriticalDegreeHistogram refinedTranscript
    @?= Map.fromList [(H.HomologicalDegree 0, 1), (H.HomologicalDegree 1, 1)]
  H.finalRefinedHomologicalSupport refinedTranscript
    @?= Set.fromList [H.HomologicalDegree 0, H.HomologicalDegree 1]
  H.finalRefinedMaxCriticalDegree refinedTranscript
    @?= Just (H.HomologicalDegree 1)
  H.refinedMatchingSummary refinedTranscript
    @?= H.RefinedMatchingSummary
      { H.rmsStageCount = 1,
        H.rmsHasStages = True,
        H.rmsIsTerminal = False,
        H.rmsFinalCriticalCellCount = 2,
        H.rmsFinalCriticalDegreeHistogram =
          Map.fromList [(H.HomologicalDegree 0, 1), (H.HomologicalDegree 1, 1)],
        H.rmsFinalHomologicalSupport = Set.fromList [H.HomologicalDegree 0, H.HomologicalDegree 1],
        H.rmsFinalMaxCriticalDegree = Just (H.HomologicalDegree 1)
      }

testRefinedTranscriptFailurePropagation :: Assertion
testRefinedTranscriptFailurePropagation =
  case H.refinedAcyclicMatchingTranscript nonNilpotentTwoStepComplex (const 0) of
    Left (H.LawViolation H.ChainNilpotenceLaw) -> pure ()
    Left otherFailure -> assertFailure ("unexpected failure: " <> show otherFailure)
    Right _ -> assertFailure "expected propagated law violation for non-nilpotent complex"

testAcyclicityRejection :: Assertion
testAcyclicityRejection =
  let cyclicMatching =
        H.AcyclicMatching
          { H.amPairs =
              [ pairAt 0 0 (-1),
                pairAt 1 1 (-1),
                pairAt 2 2 (-1)
              ],
            H.amCriticalCells = [],
            H.amObstructions = []
          }
      greedyMatching = H.acyclicMatching triangleCycleComplex (const 0)
   in do
        assertBool
          "manually cyclic matching must be rejected"
          (not (H.isAcyclicMatching triangleCycleComplex cyclicMatching))
        case H.amObstructions greedyMatching of
          obstructionValue : _ ->
            assertBool
              "cycle witness should expose a directed cycle"
              (length (H.coCycleWitness obstructionValue) >= 4)
          [] ->
            assertFailure "expected the greedy matcher to record a cycle obstruction"

assertMorseNilpotence :: String -> H.FiniteChainComplex Integer -> Assertion
assertMorseNilpotence label finite =
  withMorseComplex finite $ \morseValue ->
    let reducedComplexValue = H.mcReducedComplex morseValue
        H.HomologicalDegree maxDegreeValue = H.maxHomologicalDegree reducedComplexValue
     in mapM_
          ( \degreeValue ->
              case
                H.composeBoundaryIncidence
                  (H.incidenceMatrixAt reducedComplexValue (H.HomologicalDegree (degreeValue - 1)))
                  (H.incidenceMatrixAt reducedComplexValue (H.HomologicalDegree degreeValue))
              of
                Left shapeError ->
                  assertFailure
                    ("unexpected boundary shape failure for " <> label <> " at degree " <> show degreeValue <> ": " <> show shapeError)
                Right composedBoundary ->
                  assertBool
                    ("expected ∂∘∂ = 0 for " <> label <> " at degree " <> show degreeValue)
                    (all ((== 0) . H.boundaryCoefficient) (H.boundaryEntries composedBoundary))
          )
          [1 .. maxDegreeValue]

testBoundaryRestrictionDifference :: Assertion
testBoundaryRestrictionDifference =
  withMorseComplex triangleCycleComplex $ \morseValue -> do
    let reducedComplexValue = H.mcReducedComplex morseValue
        morseBoundary = H.incidenceMatrixAt reducedComplexValue (H.HomologicalDegree 1)
        restrictedBoundary = restrictedBoundaryIncidence triangleCycleComplex (H.mcCriticalBasis morseValue) 1
    assertBool
      "triangle Morse differential should not collapse to naive boundary restriction"
      (morseBoundary /= restrictedBoundary)

testIntervalMorseMaps :: Assertion
testIntervalMorseMaps =
  withMorseComplex intervalComplex $ \morseValue -> do
    let criticalVertex = basisCellAt 0 0
        collapsedVertex = basisCellAt 0 0
        retainedVertex = basisCellAt 0 1
        intervalEdge = basisCellAt 1 0
    H.runChainMap (H.mcProjection morseValue) collapsedVertex @?= [(1, criticalVertex)]
    H.runChainMap (H.mcProjection morseValue) retainedVertex @?= [(1, criticalVertex)]
    H.runChainMap (H.mcProjection morseValue) intervalEdge @?= []
    H.runChainMap (H.mcInclusion morseValue) criticalVertex @?= [(1, retainedVertex)]
    H.runChainHomotopy (H.mcHomotopy morseValue) collapsedVertex @?= [(-1, intervalEdge)]
    H.runChainHomotopy (H.mcHomotopy morseValue) retainedVertex @?= []
    H.runChainHomotopy (H.mcHomotopy morseValue) intervalEdge @?= []

goldenBettiPreservationTests :: TestTree
goldenBettiPreservationTests =
  testGroup
    "golden betti preservation"
    ( genuineGoldenCorpus
        & fmap
          ( \goldenCase ->
              testCase (genuineGoldenName goldenCase) $
                withMorseComplex (genuineGoldenComplex goldenCase) $ \morseValue -> do
                  H.freeBettiVector (genuineGoldenComplex goldenCase)
                    @?= genuineGoldenBetti goldenCase
                  H.freeBettiVector (H.mcReducedComplex morseValue)
                    @?= genuineGoldenBetti goldenCase
          )
    )

withMorseComplex :: H.FiniteChainComplex Integer -> (H.MorseComplex Integer -> Assertion) -> Assertion
withMorseComplex finite assertion =
  let matchingValue = H.acyclicMatching finite (const 0)
   in case H.morseComplex finite matchingValue of
        Left failureValue ->
          assertFailure ("unexpected Morse construction failure: " <> show failureValue)
        Right morseValue ->
          assertion morseValue

gf2IdentityComplex :: H.FiniteChainComplex GF2
gf2IdentityComplex =
  H.mkFiniteChainComplex (H.HomologicalDegree 1) $ \degreeValue ->
    case degreeValue of
      H.HomologicalDegree 1 ->
        validatedBoundaryIncidence 1 1 [H.mkBoundaryEntry 0 0 GF2One]
      H.HomologicalDegree 0 ->
        H.emptyBoundaryIncidenceOf 1 0
      _ ->
        H.emptyBoundaryIncidence

nonNilpotentTwoStepComplex :: H.FiniteChainComplex Integer
nonNilpotentTwoStepComplex =
  H.mkFiniteChainComplex (H.HomologicalDegree 2) $ \degreeValue ->
    case degreeValue of
      H.HomologicalDegree 2 ->
        validatedBoundaryIncidence 1 1 [H.mkBoundaryEntry 0 0 1]
      H.HomologicalDegree 1 ->
        validatedBoundaryIncidence 1 1 [H.mkBoundaryEntry 0 0 1]
      H.HomologicalDegree 0 ->
        H.emptyBoundaryIncidenceOf 1 0
      _ ->
        H.emptyBoundaryIncidence

gf2BettiRanks :: H.FiniteChainComplex GF2 -> IO [Int]
gf2BettiRanks finite =
  fmap (fmap H.freeRank) $
    expectRight
      ( H.computeBettiNumbers
          (H.fieldBettiCapability H.GF2FieldRankBackend :: H.BettiCapability 'H.Phase2 GF2)
          finite
      )

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

pairAt :: Int -> Int -> Int -> H.AcyclicPair
pairAt lowerIndexValue upperIndexValue coefficientValue =
  H.AcyclicPair
    { H.apLowerCell = basisCellAt 0 lowerIndexValue,
      H.apUpperCell = basisCellAt 1 upperIndexValue,
      H.apIncidenceCoefficient = coefficientValue
    }

basisCellAt :: Int -> Int -> H.BasisCellRef
basisCellAt degreeValue indexValue =
  H.BasisCellRef
    { H.cellDegree = H.HomologicalDegree degreeValue,
      H.cellIndex = indexValue
    }

restrictedBoundaryIncidence ::
  H.FiniteChainComplex Integer ->
  Map.Map H.BasisCellRef H.BasisCellRef ->
  Int ->
  H.BoundaryIncidence Integer
restrictedBoundaryIncidence finite reducedBasis degreeValue =
  let sourceReducedPairs = reducedPairsAt degreeValue reducedBasis
      targetReducedPairs =
        if degreeValue <= 0
          then []
          else reducedPairsAt (degreeValue - 1) reducedBasis
      incidenceEntries =
        incidenceEntryMap (H.incidenceMatrixAt finite (H.HomologicalDegree degreeValue))
          & Map.toList
          & List.sortOn fst
          & foldr
            ( \((sourceIndexValue, targetIndexValue), coefficientValue) accumulator ->
                let sourceOriginal = basisCellAt degreeValue sourceIndexValue
                    targetOriginal = basisCellAt (degreeValue - 1) targetIndexValue
                 in case (lookupReducedRef sourceOriginal sourceReducedPairs, lookupReducedRef targetOriginal targetReducedPairs) of
                      (Just sourceReducedRef, Just targetReducedRef) ->
                        H.mkBoundaryEntry
                          (fromIntegral (H.cellIndex sourceReducedRef))
                          (fromIntegral (H.cellIndex targetReducedRef))
                          coefficientValue :
                          accumulator
                      _ ->
                        accumulator
            )
            []
   in validatedBoundaryIncidence
        (length sourceReducedPairs)
        (length targetReducedPairs)
        incidenceEntries

reducedPairsAt ::
  Int ->
  Map.Map H.BasisCellRef H.BasisCellRef ->
  [(H.BasisCellRef, H.BasisCellRef)]
reducedPairsAt degreeValue reducedBasis =
  reducedBasis
    & Map.toList
    & filter ((== H.HomologicalDegree degreeValue) . H.cellDegree . fst)
    & List.sortOn (H.cellIndex . fst)

lookupReducedRef ::
  H.BasisCellRef ->
  [(H.BasisCellRef, H.BasisCellRef)] ->
  Maybe H.BasisCellRef
lookupReducedRef originalCellValue =
  fmap fst
    . List.find ((== originalCellValue) . snd)

validatedBoundaryIncidence :: (Eq coefficient, Semiring coefficient) => Int -> Int -> [H.BoundaryEntry coefficient] -> H.BoundaryIncidence coefficient
validatedBoundaryIncidence sourceDimension targetDimension entries =
  case H.mkBoundaryIncidence (fromIntegral sourceDimension) (fromIntegral targetDimension) entries of
    Left shapeError ->
      error ("invalid Morse test boundary: " <> show shapeError)
    Right incidence ->
      incidence

incidenceEntryMap :: Num r => H.BoundaryIncidence r -> Map.Map (Int, Int) r
incidenceEntryMap incidence =
  H.boundaryEntries incidence
    & fmap
      ( \boundaryEntry ->
          ( (H.sourceIndex boundaryEntry, H.targetIndex boundaryEntry),
            H.boundaryCoefficient boundaryEntry
          )
      )
    & Map.fromListWith (+)
