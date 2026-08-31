module SpectralSpec
  ( tests,
  )
where

import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Kind (Type)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Moonlight.Homology as H
import qualified Moonlight.Homology.Boundary.Finite as H (mkFiniteChainComplex)
import Moonlight.Homology.Pure.Matrix.SparseLinAlg (sparseRowFromDense, sparseSpanRank)
import qualified Moonlight.Homology.Pure.Sequence.Spectral.Build as SpectralBuild
import qualified Moonlight.Homology.Sequence as TopologySpectral
import qualified Moonlight.Homology.Matrix as Matrix
import TestFixtures (intervalComplex, triangleCycleComplex, widePathComplex)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "spectral sequence"
    [ weightedGraphShadowGapContractsTest,
      weightedGraphLeadingTransportStaysNormalizedTest,
      weightedGraphGapIgnoresInvalidEdgesTest,
      weightedGraphSparseModesAgreeWithDenseModesTest,
      trivialFiltrationConvergesToCohomologyTest,
      rationalSpectralPagesMatchManualRationalizationTest,
      filteredRefinedMorseSpectralPagesMatchUnreducedBaselineTest,
      filteredTriangleBuildsExpectedE0StrataTest,
      pathIndexFiltrationSpectralFamilyAnchorTest,
      delayedIntervalSpectralDifferentialAnchorTest,
      triangleCycleIndexFiltrationStabilizationTest,
      filteredSpectralLimitMatchesUnfilteredBettiTest,
      spectralPageDifferentialLawsTest,
      malformedBoundaryIsRejectedTest,
      validatedMatrixRejectsRaggedColumnsTest,
      spectralQuotientRejectsNonSubspaceDenominatorTest,
      filteredAnnulusStressTest,
      invertedFiltrationIsRejectedTest,
      monotoneFiltrationSucceedsTest,
      incompatibleTriangleFiltrationIsRejectedTest,
      boundaryMonotoneFiltrationOnPathGraphTest
    ]

weightedGraphShadowGapContractsTest :: TestTree
weightedGraphShadowGapContractsTest =
  testCase "weighted graph gap contracts when the structural edge weight is reduced" $
    case
        ( TopologySpectral.weightedGraphSpectralGap 2 2 [(0, 1, 1.0)],
          TopologySpectral.weightedGraphSpectralGap 2 2 [(0, 1, 0.25)]
        ) of
      (Right (Just fullGap), Right (Just contractedGap)) ->
        assertBool "expected lighter edge weighting to contract the spectral gap" (fullGap > contractedGap)
      (Left failureValue, _) ->
        assertFailure ("unexpected full-weight spectral failure: " <> show failureValue)
      (_, Left failureValue) ->
        assertFailure ("unexpected contracted-weight spectral failure: " <> show failureValue)
      unexpectedValue ->
        assertFailure ("expected concrete weighted spectral gaps, got: " <> show unexpectedValue)

weightedGraphLeadingTransportStaysNormalizedTest :: TestTree
weightedGraphLeadingTransportStaysNormalizedTest =
  testCase "leading mode transport stays normalized across equivalent weighted shadows" $
    case
        ( TopologySpectral.weightedGraphSpectralModes 2 2 [(0, 1, 1.0)],
          TopologySpectral.weightedGraphSpectralModes 2 2 [(0, 1, 0.25)]
        ) of
      (Right fullModes, Right contractedModes) ->
        case TopologySpectral.leadingModeTransport fullModes contractedModes of
          Just transportValue -> do
            assertBool "expected normalized transport" (transportValue >= 0.0 && transportValue <= 1.0)
            assertBool "expected the same one-edge carrier to preserve the leading eigenvector direction" (abs (transportValue - 1.0) < 1.0e-6)
          Nothing ->
            assertFailure "expected a transport value for nonempty weighted graph shadows"
      (Left failureValue, _) ->
        assertFailure ("unexpected full-weight spectral failure: " <> show failureValue)
      (_, Left failureValue) ->
        assertFailure ("unexpected contracted-weight spectral failure: " <> show failureValue)

weightedGraphGapIgnoresInvalidEdgesTest :: TestTree
weightedGraphGapIgnoresInvalidEdgesTest =
  testCase "weighted graph gap ignores invalid, self-loop, and nonpositive edges" $
    let cleanEdges = [(0, 1, 1.0)]
        noisyEdges =
          cleanEdges
            <> [(0, 0, 7.0), (1, 1, 5.0), (-1, 1, 3.0), (0, 2, 3.0), (0, 1, 0.0), (1, 0, -2.0)]
     in case
          ( TopologySpectral.weightedGraphSpectralGap 2 2 cleanEdges,
            TopologySpectral.weightedGraphSpectralGap 2 2 noisyEdges
          ) of
          (Right cleanGap, Right noisyGap) ->
            assertEqual "expected invalid edges to be ignored by weighted spectral gap construction" cleanGap noisyGap
          (Left failureValue, _) ->
            assertFailure ("unexpected clean-edge spectral failure: " <> show failureValue)
          (_, Left failureValue) ->
            assertFailure ("unexpected noisy-edge spectral failure: " <> show failureValue)

weightedGraphSparseModesAgreeWithDenseModesTest :: TestTree
weightedGraphSparseModesAgreeWithDenseModesTest =
  testCase "sparse Lanczos weighted graph modes agree with dense modes on a small carrier" $
    let weightedEdges =
          [ (0, 1, 1.0),
            (1, 2, 2.0),
            (2, 3, 1.5)
          ]
     in case
          ( TopologySpectral.weightedGraphSpectralModes 3 4 weightedEdges,
            TopologySpectral.weightedGraphSparseSpectralModes TopologySpectral.defaultSparseSpectralConfig 3 4 weightedEdges
          )
          of
          (Right denseModes, Right sparseModes) ->
            assertApproxList
              "expected sparse eigenvalues to track dense eigenvalues"
              (modeEigenvalues denseModes)
              (modeEigenvalues sparseModes)
          (Left failureValue, _) ->
            assertFailure ("unexpected dense spectral failure: " <> show failureValue)
          (_, Left failureValue) ->
            assertFailure ("unexpected sparse spectral failure: " <> show failureValue)

modeEigenvalues :: [H.GraphSpectralMode] -> [Double]
modeEigenvalues =
  List.sort . fmap H.spectralEigenvalue

assertApproxList :: String -> [Double] -> [Double] -> IO ()
assertApproxList label expectedValues actualValues = do
  assertEqual (label <> " length") (length expectedValues) (length actualValues)
  traverse_
    ( \(expectedValue, actualValue) ->
        assertBool
          (label <> ": expected " <> show expectedValue <> ", got " <> show actualValue)
          (abs (expectedValue - actualValue) < 1.0e-5)
    )
    (zip expectedValues actualValues)

trivialFiltrationConvergesToCohomologyTest :: TestTree
trivialFiltrationConvergesToCohomologyTest =
  testCase "trivial filtration converges at E1 to triangle cohomology" $ do
    case computeRationalizedSpectralPages triangleCycleComplex (const 0) of
      Left failureValue ->
        assertFailure ("unexpected spectral failure: " <> show failureValue)
      Right pages ->
        case lookupPage 1 pages of
          Nothing ->
            assertFailure "expected an E1 page for the trivial filtration"
          Just page1 -> do
            assertEqual "convergence depth" 1 (H.convergenceDepth pages)
            assertBool "one-pass parseable" (H.isKPassParseable 1 pages)
            assertBool "zero-pass not parseable" (not (H.isKPassParseable 0 pages))
            assertEqual "H^0 free rank" 1 (H.freeRank (H.groupAt page1 0 0))
            assertEqual "H^1 free rank" 1 (H.freeRank (H.groupAt page1 0 1))

rationalSpectralPagesMatchManualRationalizationTest :: TestTree
rationalSpectralPagesMatchManualRationalizationTest =
  testCase "rational spectral entrypoint matches manual rationalization" $ do
    let canonicalRationalTriangleComplex = H.rationalizeFiniteChainComplex triangleCycleComplex
        manualRationalTriangleComplex =
          H.mkFiniteChainComplex
            (H.maxHomologicalDegree triangleCycleComplex)
            (H.mapBoundaryCoefficients fromIntegral . H.incidenceMatrixAt triangleCycleComplex)
    case
        ( H.computeRationalSpectralPages canonicalRationalTriangleComplex triangleStepFiltration,
          H.computeRationalSpectralPages manualRationalTriangleComplex triangleStepFiltration
        )
      of
        (Right canonicalPages, Right manualPages) -> do
          assertEqual "page indices" (fmap H.pageIndex canonicalPages) (fmap H.pageIndex manualPages)
          assertEqual "page entries" (fmap H.pageEntryMap canonicalPages) (fmap H.pageEntryMap manualPages)
          assertEqual "page differentials" (fmap H.pageDifferentialMap canonicalPages) (fmap H.pageDifferentialMap manualPages)
        (Left failureValue, _) ->
          assertFailure ("unexpected canonical rational spectral failure: " <> show failureValue)
        (_, Left failureValue) ->
          assertFailure ("unexpected manual rational spectral failure: " <> show failureValue)

filteredRefinedMorseSpectralPagesMatchUnreducedBaselineTest :: TestTree
filteredRefinedMorseSpectralPagesMatchUnreducedBaselineTest =
  testCase "filtered refined Morse spectral pages match the unreduced rational baseline" $
    let complex = widePathComplex 4
     in case
          ( computeRationalizedSpectralPages complex pathGraphFiltration,
            H.filteredRefinedMorseComplex complex pathGraphFiltration (const 0)
          )
          of
          (Right unreducedPages, Right filteredComplex) -> do
            let refinedComplex = H.frmcRefinedMorseComplex filteredComplex
                reducedComplex = H.rmcReducedComplex refinedComplex
            case H.computeRationalSpectralPages reducedComplex (H.filteredReducedFiltration filteredComplex) of
              Left failureValue ->
                assertFailure ("unexpected filtered reduced spectral failure: " <> show failureValue)
              Right reducedPages -> do
                assertBool
                  "filtered refined Morse should actually collapse this path fixture"
                  (basisCellCount reducedComplex < basisCellCount (H.rationalizeFiniteChainComplex complex))
                assertBool
                  "filtered refined Morse should carry at least one compatibility witness"
                  (not (null (H.fmcPairWitnesses (H.frmcCompatibility filteredComplex))))
                case (lookupPage 1 unreducedPages, lookupPage 1 reducedPages) of
                  (Just unreducedPage1, Just reducedPage1) ->
                    assertEqual
                      "filtered reduced E1 nonzero groups must match the unreduced rational E1 nonzero groups"
                      (nonZeroPageGroupSummary unreducedPage1)
                      (nonZeroPageGroupSummary reducedPage1)
                  missingPage ->
                    let (unreducedPage, reducedPage) = missingPage
                     in assertFailure
                          ( "expected E1 pages in both spectral computations, received "
                              <> show (fmap H.pageIndex unreducedPage, fmap H.pageIndex reducedPage)
                          )
                case (expectStablePage unreducedPages, expectStablePage reducedPages) of
                  (Right unreducedStablePage, Right reducedStablePage) ->
                    assertEqual
                      "filtered reduced stable nonzero groups must match the unreduced rational stable nonzero groups"
                      (nonZeroPageGroupSummary unreducedStablePage)
                      (nonZeroPageGroupSummary reducedStablePage)
                  (Left failureMessage, _) ->
                    assertFailure failureMessage
                  (_, Left failureMessage) ->
                    assertFailure failureMessage
          (Left failureValue, _) ->
            assertFailure ("unexpected unreduced spectral failure: " <> show failureValue)
          (_, Left failureValue) ->
            assertFailure ("unexpected filtered refined Morse failure: " <> show failureValue)

filteredTriangleBuildsExpectedE0StrataTest :: TestTree
filteredTriangleBuildsExpectedE0StrataTest =
  testCase "triangle filtration yields graded E0 strata before convergence" $ do
    case computeRationalizedSpectralPages triangleCycleComplex triangleStepFiltration of
      Left failureValue ->
        assertFailure ("unexpected spectral failure: " <> show failureValue)
      Right pages ->
        case lookupPage 0 pages of
          Nothing ->
            assertFailure "expected an E0 page for the filtered triangle"
          Just page0 -> do
            assertEqual "page indices follow filtration width" [0, 1, 2, 3] (fmap H.pageIndex pages)
            assertEqual "vertex stratum rank" 3 (H.freeRank (H.groupAt page0 0 0))
            assertEqual "first edge stratum rank" 2 (H.freeRank (H.groupAt page0 1 0))
            assertEqual "late edge stratum rank" 1 (H.freeRank (H.groupAt page0 2 (-1)))
            assertBool "convergence stays within filtration width" (H.convergenceDepth pages <= 3)
            assertEqual "hand-derived stabilization index" 2 (H.convergenceDepth pages)
            case expectStablePage pages of
              Left failureMessage ->
                assertFailure failureMessage
              Right stablePage ->
                assertEqual
                  "stable Betti agrees with direct triangle cohomology"
                  [1, 1]
                  (spectralBettiVector 2 stablePage)

pathIndexFiltrationSpectralFamilyAnchorTest :: TestTree
pathIndexFiltrationSpectralFamilyAnchorTest =
  testCase "path vertex and edge index filtration converges to the path Betti vector within its width" $
    let complex = widePathComplex 7
        pathFiltrationWidth = filtrationWidthOf complex pathGraphFiltration
     in case computeRationalizedSpectralFamily complex pathGraphFiltration of
          Left failureValue ->
            assertFailure ("unexpected path spectral family failure: " <> show failureValue)
          Right family -> do
            assertEqual "path filtration width" 6 pathFiltrationWidth
            traverse_ assertSpectralDifferentialSquaresToZero (H.spectralFamilyPages family)
            assertBool
              "path spectral family stabilizes no later than filtration width plus one"
              (H.spectralFamilyStableFrom family <= pathFiltrationWidth + 1)
            assertEqual
              "path limit-page total-degree ranks"
              [1, 0]
              (spectralBettiVector 2 (H.spectralFamilyLimitPage family))

delayedIntervalSpectralDifferentialAnchorTest :: TestTree
delayedIntervalSpectralDifferentialAnchorTest =
  testCase "interval delayed-edge filtration has a nonzero d1 that kills the edge class" $
    case computeRationalizedSpectralFamily intervalComplex delayedIntervalFiltration of
      Left failureValue ->
        assertFailure ("unexpected delayed interval spectral family failure: " <> show failureValue)
      Right family ->
        case
            ( lookupPage 0 (H.spectralFamilyPages family),
              lookupPage 1 (H.spectralFamilyPages family),
              lookupPage 2 (H.spectralFamilyPages family)
            )
          of
          (Just page0, Just page1, Just page2) -> do
            assertEqual
              "delayed interval E0 ranks"
              (Map.fromList [((0, 0), 2), ((1, 0), 1)])
              (pageEntryRanks page0)
            assertEqual
              "delayed interval E1 ranks"
              (Map.fromList [((0, 0), 2), ((1, 0), 1)])
              (pageEntryRanks page1)
            assertEqual
              "delayed interval nonzero E1 differential source rank"
              (Map.fromList [((0, 0), 1)])
              (nonZeroDifferentialRanks page1)
            case Map.lookup (H.mkBidegree 0 0) (H.pageDifferentialMap page1) of
              Nothing ->
                assertFailure "expected delayed interval d1 at bidegree (0,0)"
              Just differentialValue -> do
                assertEqual "delayed interval d1 domain rank" 2 (length (H.formalDomainBasis differentialValue))
                assertEqual "delayed interval d1 codomain rank" 1 (length (H.formalCodomainBasis differentialValue))
                assertEqual "delayed interval d1 matrix rank" 1 (matrixRank (H.formalMatrix differentialValue))
            assertEqual
              "delayed interval d1 target bidegree"
              (1, 0)
              (H.bidegreeCoordinates (H.targetBidegreeAfterDifferential 1 (H.mkBidegree 0 0)))
            assertEqual
              "delayed interval E2 ranks"
              (Map.fromList [((0, 0), 1), ((1, 0), 0)])
              (pageEntryRanks page2)
            assertEqual "delayed interval stabilization index" 2 (H.spectralFamilyStableFrom family)
            assertEqual
              "delayed interval limit-page total-degree ranks"
              [1, 0]
              (spectralBettiVector 2 (H.spectralFamilyLimitPage family))
          missingPages ->
            let (page0, page1, page2) = missingPages
             in assertFailure
                  ( "expected E0, E1, and E2 pages for delayed interval, received "
                      <> show (fmap H.pageIndex page0, fmap H.pageIndex page1, fmap H.pageIndex page2)
                  )

triangleCycleIndexFiltrationStabilizationTest :: TestTree
triangleCycleIndexFiltrationStabilizationTest =
  testCase "triangle cycle index filtration stabilizes at the hand-derived page" $
    case computeRationalizedSpectralFamily triangleCycleComplex triangleCycleIndexFiltration of
      Left failureValue ->
        assertFailure ("unexpected triangle spectral family failure: " <> show failureValue)
      Right family -> do
        assertEqual "triangle index-filtration stabilization index" 1 (H.spectralFamilyStableFrom family)
        assertEqual
          "triangle limit-page total-degree ranks"
          [1, 1]
          (spectralBettiVector 2 (H.spectralFamilyLimitPage family))

filteredSpectralLimitMatchesUnfilteredBettiTest :: TestTree
filteredSpectralLimitMatchesUnfilteredBettiTest =
  testCase "filtered spectral limit ranks match unfiltered rational Betti numbers" $
    traverse_ assertFilteredSpectralAnchorConverges filteredSpectralAnchors

malformedBoundaryIsRejectedTest :: TestTree
malformedBoundaryIsRejectedTest =
  testCase "validated boundary constructors reject malformed incidence instead of padding or truncating" $
    case H.mkBoundaryIncidence 1 1 [H.mkBoundaryEntry 0 2 (1 :: Int)] of
      Left (H.BoundaryIncidenceEntryOutOfBounds 0 2 1 1) ->
        pure ()
      Left shapeError ->
        assertFailure ("expected explicit out-of-bounds rejection, got: " <> show shapeError)
      Right _ ->
        assertFailure "expected malformed incidence rejection"

validatedMatrixRejectsRaggedColumnsTest :: TestTree
validatedMatrixRejectsRaggedColumnsTest =
  testCase "validated matrices reject ragged columns instead of zero padding them into fake linear maps" $
    case Matrix.validatedMatrixFromColumns 3 [[1 :: Rational, 0, 0], [0, 1]] of
      Left (H.InvalidMatrixShape _) ->
        pure ()
      Left failureValue ->
        assertFailure ("expected a matrix-shape rejection, got: " <> show failureValue)
      Right _ ->
        assertFailure "expected ragged column rejection"

spectralQuotientRejectsNonSubspaceDenominatorTest :: TestTree
spectralQuotientRejectsNonSubspaceDenominatorTest =
  testCase "spectral quotient rejects denominator vectors outside the numerator subspace" $
    case SpectralBuild.buildEntryFromBases (H.mkBidegree 0 0) 2 (fmap sparseRowFromDense [[1, 0]]) (fmap sparseRowFromDense [[0, 1]]) of
      Left (H.SpectralQuotientDenominatorNotSubspace (0, 0) 2 [0, 1]) ->
        pure ()
      Left failureValue ->
        assertFailure ("expected denominator-subset obstruction, got: " <> show failureValue)
      Right _ ->
        assertFailure "expected unlawful quotient rejection"

filteredAnnulusStressTest :: TestTree
filteredAnnulusStressTest =
  testCase "filtered annulus stress test converges to the correct Betti vector on a realistic complex" $ do
    case buildAnnulusScenario 5 1 3 of
      Left failureValue ->
        assertFailure ("failed to build annulus scenario: " <> show failureValue)
      Right scenario -> do
        let expectedBetti = H.freeBettiVector (asComplex scenario)
        assertEqual "annulus Betti vector" [1, 1, 0] expectedBetti
        case computeRationalizedSpectralPages (asComplex scenario) (asFiltration scenario) of
          Left failureValue ->
            assertFailure ("unexpected spectral failure on annulus stress case: " <> show failureValue)
          Right pages -> do
            traverse_ assertSpectralPageWellShaped pages
            case expectStablePage pages of
              Left failureMessage ->
                assertFailure failureMessage
              Right stablePage -> do
                let stableBetti = spectralBettiVector (length expectedBetti) stablePage
                assertEqual "stable spectral Betti must equal direct cohomology" expectedBetti stableBetti

type AnnulusScenario :: Type
data AnnulusScenario = AnnulusScenario
  { asComplex :: H.FiniteChainComplex Integer,
    asFiltration :: H.FiltrationFunction,
    asVertexCount :: Int,
    asEdgeCount :: Int,
    asFaceCount :: Int
  }

buildAnnulusScenario :: Int -> Int -> Int -> Either H.HomologyFailure AnnulusScenario
buildAnnulusScenario outerSize holeLower holeUpper = do
  let faceBasis = annulusFaceBasis outerSize holeLower holeUpper
      edgeBasis = annulusEdgeBasis faceBasis
      vertexBasis = annulusVertexBasis edgeBasis
  faceBoundaryIncidence <- H.materializeIncidenceBoundary faceBoundary faceBasis edgeBasis
  edgeBoundaryIncidence <- H.materializeIncidenceBoundary edgeBoundary edgeBasis vertexBasis
  pure
    AnnulusScenario
      { asComplex =
          H.mkFiniteChainComplex (H.HomologicalDegree 2) $ \degreeValue ->
            case degreeValue of
              H.HomologicalDegree 2 -> faceBoundaryIncidence
              H.HomologicalDegree 1 -> edgeBoundaryIncidence
              H.HomologicalDegree 0 -> H.emptyBoundaryIncidenceOf (fromIntegral (length vertexBasis)) 0
              _ -> H.emptyBoundaryIncidence,
        asFiltration = annulusFiltration vertexBasis edgeBasis faceBasis,
        asVertexCount = length vertexBasis,
        asEdgeCount = length edgeBasis,
        asFaceCount = length faceBasis
      }

annulusFaceBasis :: Int -> Int -> Int -> [AnnulusFace]
annulusFaceBasis outerSize holeLower holeUpper =
  coordinateRange outerSize
    & concatMap
      (\xValue -> fmap (AnnulusFace xValue) (coordinateRange outerSize))
    & filter (not . faceInsideHole holeLower holeUpper)

annulusEdgeBasis :: [AnnulusFace] -> [AnnulusEdge]
annulusEdgeBasis =
  Set.toAscList
    . Set.fromList
    . concatMap faceBoundaryEdges

annulusVertexBasis :: [AnnulusEdge] -> [AnnulusVertex]
annulusVertexBasis =
  Set.toAscList
    . Set.fromList
    . concatMap edgeVertices

coordinateRange :: Int -> [Int]
coordinateRange outerSize = [0 .. outerSize - 1]

faceInsideHole :: Int -> Int -> AnnulusFace -> Bool
faceInsideHole holeLower holeUpper (AnnulusFace xValue yValue) =
  xValue >= holeLower
    && xValue < holeUpper
    && yValue >= holeLower
    && yValue < holeUpper

annulusFiltration :: [AnnulusVertex] -> [AnnulusEdge] -> [AnnulusFace] -> H.FiltrationFunction
annulusFiltration vertexBasis edgeBasis faceBasis basisCellRef =
  case H.cellDegree basisCellRef of
    H.HomologicalDegree 0 ->
      maybe 0 vertexFiltration (basisAt (H.cellIndex basisCellRef) vertexBasis)
    H.HomologicalDegree 1 ->
      maybe 0 edgeFiltration (basisAt (H.cellIndex basisCellRef) edgeBasis)
    H.HomologicalDegree 2 ->
      maybe 0 faceFiltration (basisAt (H.cellIndex basisCellRef) faceBasis)
    _ -> 0

basisAt :: Int -> [a] -> Maybe a
basisAt indexValue values
  | indexValue < 0 = Nothing
  | otherwise =
      case drop indexValue values of
        value : _ -> Just value
        [] -> Nothing

lookupPage :: Int -> [H.SpectralPage Rational] -> Maybe (H.SpectralPage Rational)
lookupPage targetIndex =
  List.find (\page -> H.pageIndex page == targetIndex)

computeRationalizedSpectralPages ::
  Integral r =>
  H.FiniteChainComplex r ->
  H.FiltrationFunction ->
  Either H.HomologyFailure [H.RationalSpectralPage]
computeRationalizedSpectralPages finiteComplex =
  H.computeRationalSpectralPages (H.rationalizeFiniteChainComplex finiteComplex)

computeRationalizedSpectralFamily ::
  Integral r =>
  H.FiniteChainComplex r ->
  H.FiltrationFunction ->
  Either H.HomologyFailure H.RationalSpectralFamily
computeRationalizedSpectralFamily finiteComplex =
  H.computeRationalSpectralFamily (H.rationalizeFiniteChainComplex finiteComplex)

expectStablePage :: [H.SpectralPage Rational] -> Either String (H.SpectralPage Rational)
expectStablePage pages =
  case H.stableSpectralPage pages of
    Just stablePage -> Right stablePage
    Nothing -> Left "spectral construction produced no stable page"

assertSpectralPageWellShaped :: H.SpectralPage Rational -> IO ()
assertSpectralPageWellShaped page =
  traverse_
    assertFormalMapWellShaped
    (Map.elems (H.pageDifferentialMap page))

assertFormalMapWellShaped :: H.FormalMap Rational -> IO ()
assertFormalMapWellShaped formalMapValue = do
  let matrixValue = H.formalMatrix formalMapValue
      expectedRowCount = length (H.formalCodomainBasis formalMapValue)
      expectedColumnCount = length (H.formalDomainBasis formalMapValue)
  assertEqual "formal map row count matches codomain basis size" expectedRowCount (length matrixValue)
  assertBool
    "formal map columns match domain basis size"
    (all ((== expectedColumnCount) . length) matrixValue)

spectralPageDifferentialLawsTest :: TestTree
spectralPageDifferentialLawsTest =
  testCase "filtered triangle pages satisfy d² = 0 and advance by homology of the previous page" $
    case computeRationalizedSpectralPages triangleCycleComplex triangleStepFiltration of
      Left failureValue ->
        assertFailure ("unexpected spectral failure: " <> show failureValue)
      Right pages -> do
        traverse_ assertSpectralDifferentialSquaresToZero pages
        traverse_ (uncurry assertNextPageIsDifferentialHomology) (zip pages (drop 1 pages))

assertSpectralDifferentialSquaresToZero :: H.SpectralPage Rational -> IO ()
assertSpectralDifferentialSquaresToZero page =
  traverse_
    (assertDifferentialSquareAt page)
    (Map.toAscList (H.pageDifferentialMap page))

assertDifferentialSquareAt :: H.SpectralPage Rational -> (H.Bidegree, H.FormalMap Rational) -> IO ()
assertDifferentialSquareAt page (sourceBidegree, firstMap) =
  let targetBidegree = H.targetBidegreeAfterDifferential (H.pageIndex page) sourceBidegree
      secondMap =
        Map.findWithDefault
          (zeroFormalMapOn (H.formalCodomainBasis firstMap))
          targetBidegree
          (H.pageDifferentialMap page)
   in case composeFormalMaps secondMap firstMap of
        Left failureMessage ->
          assertFailure failureMessage
        Right compositeMatrix ->
          assertBool
            ("expected d² = 0 at " <> show (H.bidegreeCoordinates sourceBidegree) <> " on page " <> show (H.pageIndex page))
            (zeroMatrixValue compositeMatrix)

assertNextPageIsDifferentialHomology :: H.SpectralPage Rational -> H.SpectralPage Rational -> IO ()
assertNextPageIsDifferentialHomology previousPage nextPage = do
  assertEqual
    ("page support at E" <> show (H.pageIndex nextPage))
    (Map.keysSet (H.pageEntryMap previousPage))
    (Map.keysSet (H.pageEntryMap nextPage))
  traverse_
    (assertNextEntryIsDifferentialHomology previousPage nextPage)
    (Map.toAscList (H.pageEntryMap nextPage))

assertNextEntryIsDifferentialHomology ::
  H.SpectralPage Rational ->
  H.SpectralPage Rational ->
  (H.Bidegree, H.SpectralEntry Rational) ->
  IO ()
assertNextEntryIsDifferentialHomology previousPage nextPage (bidegreeValue, nextEntry) =
  case Map.lookup bidegreeValue (H.pageDifferentialMap previousPage) of
    Nothing ->
      assertFailure
        ("previous page lacks outgoing differential at " <> show (H.bidegreeCoordinates bidegreeValue))
    Just outgoingMap -> do
      let incomingRank =
            Map.lookup (incomingSourceBidegree (H.pageIndex previousPage) bidegreeValue) (H.pageDifferentialMap previousPage)
              & maybe 0 (matrixRank . H.formalMatrix)
          outgoingRank = matrixRank (H.formalMatrix outgoingMap)
          domainDimension = length (H.formalDomainBasis outgoingMap)
          expectedRank = domainDimension - outgoingRank - incomingRank
          nextGroup = H.entryGroupValue nextEntry
      assertBool
        ("expected nonnegative page homology rank at " <> show (H.bidegreeCoordinates bidegreeValue))
        (expectedRank >= 0)
      assertEqual
        ( "E"
            <> show (H.pageIndex nextPage)
            <> " rank at "
            <> show (H.bidegreeCoordinates bidegreeValue)
        )
        expectedRank
        (H.freeRank nextGroup)
      assertEqual
        ("rational spectral entry has no torsion at " <> show (H.bidegreeCoordinates bidegreeValue))
        []
        (H.torsionInvariants nextGroup)

incomingSourceBidegree :: Int -> H.Bidegree -> H.Bidegree
incomingSourceBidegree pageNumber bidegreeValue =
  H.mkBidegree
    (H.bidegreeFiltrationDegree bidegreeValue - pageNumber)
    (H.bidegreeComplementaryDegree bidegreeValue + pageNumber - 1)

zeroFormalMapOn :: [H.RepresentativeCocycle Rational Int] -> H.FormalMap Rational
zeroFormalMapOn domainBasis =
  H.FormalMap
    { H.formalMatrix = [],
      H.formalDomainBasis = domainBasis,
      H.formalCodomainBasis = []
    }

composeFormalMaps :: H.FormalMap Rational -> H.FormalMap Rational -> Either String [[Rational]]
composeFormalMaps secondMap firstMap =
  let sharedDimension = length (H.formalCodomainBasis firstMap)
      sourceDimension = length (H.formalDomainBasis firstMap)
      targetDimension = length (H.formalCodomainBasis secondMap)
      firstMatrix = H.formalMatrix firstMap
      secondMatrix = H.formalMatrix secondMap
   in if H.formalDomainBasis secondMap /= H.formalCodomainBasis firstMap
        then Left "spectral formal map composition encountered incompatible bases"
        else
          if not (matrixHasShape sharedDimension sourceDimension firstMatrix)
            || not (matrixHasShape targetDimension sharedDimension secondMatrix)
            then Left "spectral formal map composition encountered malformed matrices"
            else Right (matrixProduct sourceDimension secondMatrix firstMatrix)

matrixProduct :: Int -> [[Rational]] -> [[Rational]] -> [[Rational]]
matrixProduct rightColumnCount leftRows rightRows =
  let rightColumns =
        case rightRows of
          [] -> replicate rightColumnCount []
          _ -> List.transpose rightRows
   in fmap
        ( \leftRow ->
            fmap (sum . zipWith (*) leftRow) rightColumns
        )
        leftRows

matrixHasShape :: Int -> Int -> [[Rational]] -> Bool
matrixHasShape expectedRowCount expectedColumnCount matrixValue =
  length matrixValue == expectedRowCount
    && all ((== expectedColumnCount) . length) matrixValue

matrixRank :: [[Rational]] -> Int
matrixRank = sparseSpanRank . fmap sparseRowFromDense

zeroMatrixValue :: [[Rational]] -> Bool
zeroMatrixValue =
  all (all (== 0))

pageEntryRanks :: H.SpectralPage Rational -> Map.Map (Int, Int) Int
pageEntryRanks page =
  H.pageEntryMap page
    & Map.toAscList
    & fmap
      ( \(bidegreeValue, entryValue) ->
          (H.bidegreeCoordinates bidegreeValue, H.freeRank (H.entryGroupValue entryValue))
      )
    & Map.fromList

nonZeroDifferentialRanks :: H.SpectralPage Rational -> Map.Map (Int, Int) Int
nonZeroDifferentialRanks page =
  H.pageDifferentialMap page
    & Map.toAscList
    & fmap
      ( \(bidegreeValue, formalMapValue) ->
          (H.bidegreeCoordinates bidegreeValue, matrixRank (H.formalMatrix formalMapValue))
      )
    & filter ((/= 0) . snd)
    & Map.fromList

spectralBettiVector :: Int -> H.SpectralPage Rational -> [Int]
spectralBettiVector degreeCount page =
  let ranksByTotalDegree =
        H.pageEntryMap page
          & Map.keys
          & fmap
            (\bidegreeValue ->
                ( totalDegreeIndex (H.bidegreeTotalDegree bidegreeValue),
                  H.freeRank
                    (H.groupAt page (H.bidegreeFiltrationDegree bidegreeValue) (H.bidegreeComplementaryDegree bidegreeValue))
                )
            )
          & Map.fromListWith (+)
   in fmap (\degreeValue -> Map.findWithDefault 0 degreeValue ranksByTotalDegree) [0 .. degreeCount - 1]

totalDegreeIndex :: H.HomologicalDegree -> Int
totalDegreeIndex (H.HomologicalDegree degreeValue) = degreeValue

nonZeroPageGroupSummary :: H.SpectralPage Rational -> Map.Map H.Bidegree (H.HomologyGroup Rational)
nonZeroPageGroupSummary =
  Map.filter nonZeroHomologyGroup . fmap H.entryGroupValue . H.pageEntryMap

nonZeroHomologyGroup :: H.HomologyGroup Rational -> Bool
nonZeroHomologyGroup groupValue =
  H.freeRank groupValue /= 0 || not (null (H.torsionInvariants groupValue))

basisCellCount :: H.FiniteChainComplex r -> Int
basisCellCount =
  length . basisRefsOfComplex

filtrationWidthOf :: H.FiniteChainComplex r -> H.FiltrationFunction -> Int
filtrationWidthOf finiteComplex filtration =
  case fmap filtration (basisRefsOfComplex finiteComplex) of
    [] -> 0
    firstLevel : remainingLevels ->
      List.foldl' max firstLevel remainingLevels - List.foldl' min firstLevel remainingLevels

basisRefsOfComplex :: H.FiniteChainComplex r -> [H.BasisCellRef]
basisRefsOfComplex finiteComplex =
  [0 .. totalDegreeIndex (H.maxHomologicalDegree finiteComplex)]
    >>= ( \degreeValue ->
            let homologicalDegreeValue = H.HomologicalDegree degreeValue
             in fmap
                  (H.BasisCellRef homologicalDegreeValue)
                  [0 .. H.sourceCardinality (H.incidenceMatrixAt finiteComplex homologicalDegreeValue) - 1]
       )

triangleStepFiltration :: H.FiltrationFunction
triangleStepFiltration basisCellRef =
  case (H.cellDegree basisCellRef, H.cellIndex basisCellRef) of
    (H.HomologicalDegree 0, _) -> 0
    (H.HomologicalDegree 1, 0) -> 1
    (H.HomologicalDegree 1, 1) -> 1
    (H.HomologicalDegree 1, 2) -> 2
    _ -> 0

triangleCycleIndexFiltration :: H.FiltrationFunction
triangleCycleIndexFiltration basisCellRef =
  case H.cellDegree basisCellRef of
    H.HomologicalDegree 0 -> H.cellIndex basisCellRef
    H.HomologicalDegree 1 -> H.cellIndex basisCellRef + 1
    _ -> 0

type AnnulusVertex :: Type
data AnnulusVertex = AnnulusVertex Int Int
  deriving stock (Eq, Ord, Show)

type AnnulusEdge :: Type
data AnnulusEdge
  = HorizontalEdge Int Int
  | VerticalEdge Int Int
  deriving stock (Eq, Ord, Show)

type AnnulusFace :: Type
data AnnulusFace = AnnulusFace Int Int
  deriving stock (Eq, Ord, Show)

vertexFiltration :: AnnulusVertex -> Int
vertexFiltration (AnnulusVertex xValue yValue) = xValue + yValue

edgeFiltration :: AnnulusEdge -> Int
edgeFiltration edgeValue =
  case edgeValue of
    HorizontalEdge xValue yValue -> xValue + yValue + 1
    VerticalEdge xValue yValue -> xValue + yValue + 1

faceFiltration :: AnnulusFace -> Int
faceFiltration (AnnulusFace xValue yValue) = xValue + yValue + 2

edgeVertices :: AnnulusEdge -> [AnnulusVertex]
edgeVertices edgeValue =
  case edgeValue of
    HorizontalEdge xValue yValue ->
      [ AnnulusVertex xValue yValue,
        AnnulusVertex (xValue + 1) yValue
      ]
    VerticalEdge xValue yValue ->
      [ AnnulusVertex xValue yValue,
        AnnulusVertex xValue (yValue + 1)
      ]

edgeBoundary :: AnnulusEdge -> [(Integer, AnnulusVertex)]
edgeBoundary edgeValue =
  case edgeValue of
    HorizontalEdge xValue yValue ->
      [ (-1, AnnulusVertex xValue yValue),
        (1, AnnulusVertex (xValue + 1) yValue)
      ]
    VerticalEdge xValue yValue ->
      [ (-1, AnnulusVertex xValue yValue),
        (1, AnnulusVertex xValue (yValue + 1))
      ]

faceBoundaryEdges :: AnnulusFace -> [AnnulusEdge]
faceBoundaryEdges (AnnulusFace xValue yValue) =
  [ HorizontalEdge xValue yValue,
    VerticalEdge (xValue + 1) yValue,
    HorizontalEdge xValue (yValue + 1),
    VerticalEdge xValue yValue
  ]

faceBoundary :: AnnulusFace -> [(Integer, AnnulusEdge)]
faceBoundary (AnnulusFace xValue yValue) =
  [ (1, HorizontalEdge xValue yValue),
    (1, VerticalEdge (xValue + 1) yValue),
    (-1, HorizontalEdge xValue (yValue + 1)),
    (-1, VerticalEdge xValue yValue)
  ]

invertedFiltrationIsRejectedTest :: TestTree
invertedFiltrationIsRejectedTest =
  testCase "filtration that inverts cochain filtration preservation is rejected at source construction" $
    let wideComplex = widePathComplex 5
     in case computeRationalizedSpectralPages wideComplex (invertedWideFiltration 5) of
          Left (H.FiltrationNotPreserved lowerCell upperCell lowerLevel upperLevel) -> do
            assertEqual "lower cell" (H.BasisCellRef (H.HomologicalDegree 0) 0) lowerCell
            assertEqual "upper cell" (H.BasisCellRef (H.HomologicalDegree 1) 0) upperCell
            assertEqual "lower level" 4 lowerLevel
            assertEqual "upper level" 0 upperLevel
          Left failureValue ->
            assertFailure ("expected filtration preservation failure, got: " <> show failureValue)
          Right _ ->
            assertFailure "expected inverted filtration rejection"

monotoneFiltrationSucceedsTest :: TestTree
monotoneFiltrationSucceedsTest =
  testCase "cochain-preserving filtration succeeds on the same complex" $
    case computeRationalizedSpectralPages triangleCycleComplex monotoneFiltration of
      Left failureValue ->
        assertFailure ("unexpected spectral failure with monotone filtration: " <> show failureValue)
      Right pages ->
        case expectStablePage pages of
          Left failureMessage -> assertFailure failureMessage
          Right stablePage ->
            assertEqual
              "stable spectral Betti must equal direct cohomology"
              (H.freeBettiVector triangleCycleComplex)
              (spectralBettiVector 2 stablePage)

invertedWideFiltration :: Int -> H.FiltrationFunction
invertedWideFiltration nodeCount basisCellRef =
  case H.cellDegree basisCellRef of
    H.HomologicalDegree 0 ->
      nodeCount - 1 - H.cellIndex basisCellRef
    H.HomologicalDegree 1 ->
      0
    _ -> 0

monotoneFiltration :: H.FiltrationFunction
monotoneFiltration basisCellRef =
  case (H.cellDegree basisCellRef, H.cellIndex basisCellRef) of
    (H.HomologicalDegree 0, 0) -> 0
    (H.HomologicalDegree 0, 1) -> 0
    (H.HomologicalDegree 0, 2) -> 0
    (H.HomologicalDegree 1, 0) -> 1
    (H.HomologicalDegree 1, 1) -> 1
    (H.HomologicalDegree 1, 2) -> 2
    _ -> 0

pathGraphFiltration :: H.FiltrationFunction
pathGraphFiltration basisCellRef =
  case H.cellDegree basisCellRef of
    H.HomologicalDegree 0 -> H.cellIndex basisCellRef
    H.HomologicalDegree 1 -> H.cellIndex basisCellRef + 1
    _ -> 0

delayedIntervalFiltration :: H.FiltrationFunction
delayedIntervalFiltration basisCellRef =
  case H.cellDegree basisCellRef of
    H.HomologicalDegree 0 -> 0
    H.HomologicalDegree 1 -> 1
    _ -> 0

type FilteredSpectralAnchor :: Type
data FilteredSpectralAnchor = FilteredSpectralAnchor
  { fsaName :: String,
    fsaComplex :: H.FiniteChainComplex Integer,
    fsaFiltration :: H.FiltrationFunction,
    fsaExpectedBetti :: [Int]
  }

filteredSpectralAnchors :: [FilteredSpectralAnchor]
filteredSpectralAnchors =
  [ FilteredSpectralAnchor
      { fsaName = "seven-vertex path",
        fsaComplex = widePathComplex 7,
        fsaFiltration = pathGraphFiltration,
        fsaExpectedBetti = [1, 0]
      },
    FilteredSpectralAnchor
      { fsaName = "delayed interval",
        fsaComplex = intervalComplex,
        fsaFiltration = delayedIntervalFiltration,
        fsaExpectedBetti = [1, 0]
      },
    FilteredSpectralAnchor
      { fsaName = "triangle cycle index filtration",
        fsaComplex = triangleCycleComplex,
        fsaFiltration = triangleCycleIndexFiltration,
        fsaExpectedBetti = [1, 1]
      }
  ]

assertFilteredSpectralAnchorConverges :: FilteredSpectralAnchor -> IO ()
assertFilteredSpectralAnchorConverges anchor =
  case computeRationalizedSpectralFamily (fsaComplex anchor) (fsaFiltration anchor) of
    Left failureValue ->
      assertFailure (fsaName anchor <> " spectral family failed: " <> show failureValue)
    Right family -> do
      let unfilteredBetti = H.freeBettiVector (fsaComplex anchor)
          limitBetti =
            spectralBettiVector
              (length (fsaExpectedBetti anchor))
              (H.spectralFamilyLimitPage family)
      assertEqual (fsaName anchor <> " hand Betti vector") (fsaExpectedBetti anchor) unfilteredBetti
      assertEqual (fsaName anchor <> " limit-page Betti vector") unfilteredBetti limitBetti

incompatibleTriangleFiltrationIsRejectedTest :: TestTree
incompatibleTriangleFiltrationIsRejectedTest =
  testCase "triangle filtration that violates cochain preservation is rejected before page construction" $ do
    case computeRationalizedSpectralPages triangleCycleComplex incompatibleTriangleFiltration of
      Left (H.FiltrationNotPreserved lowerCell upperCell lowerLevel upperLevel) -> do
        assertEqual "lower cell" (H.BasisCellRef (H.HomologicalDegree 0) 2) lowerCell
        assertEqual "upper cell" (H.BasisCellRef (H.HomologicalDegree 1) 2) upperCell
        assertEqual "lower level" 1 lowerLevel
        assertEqual "upper level" 0 upperLevel
      Left failureValue ->
        assertFailure ("expected filtration preservation failure, got: " <> show failureValue)
      Right _ ->
        assertFailure "expected incompatible triangle filtration rejection"

incompatibleTriangleFiltration :: H.FiltrationFunction
incompatibleTriangleFiltration basisCellRef =
  case (H.cellDegree basisCellRef, H.cellIndex basisCellRef) of
    (H.HomologicalDegree 0, 0) -> 0
    (H.HomologicalDegree 0, 1) -> 1
    (H.HomologicalDegree 0, 2) -> 1
    (H.HomologicalDegree 1, 0) -> 1
    (H.HomologicalDegree 1, 1) -> 1
    (H.HomologicalDegree 1, 2) -> 0
    _ -> 0

boundaryMonotoneFiltrationOnPathGraphTest :: TestTree
boundaryMonotoneFiltrationOnPathGraphTest =
  testCase "cochain-preserving filtration on path graph converges to correct Betti" $
    let complex = widePathComplex 4
     in case computeRationalizedSpectralPages complex pathGraphFiltration of
          Left failureValue ->
            assertFailure ("path filtration failed: " <> show failureValue)
          Right pages ->
            case expectStablePage pages of
              Left failureMessage -> assertFailure failureMessage
              Right stablePage -> do
                assertEqual "page indices use filtration-width cap" [0, 1, 2, 3, 4] (fmap H.pageIndex pages)
                assertEqual
                  "stable spectral Betti must equal direct cohomology"
                  (H.freeBettiVector complex)
                  (spectralBettiVector 2 stablePage)
