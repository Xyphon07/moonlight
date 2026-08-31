module LinearFeasibilitySpec
  ( tests,
  )
where

import Data.Bifunctor (first)
import Moonlight.LinAlg.Pure.Dense.Dynamic
  ( DynVector,
    dynVectorToList,
    mkDynMatrix,
    mkDynVector,
  )
import Moonlight.LinAlg.Pure.Domain.LinearFeasibility
  ( FeasibilityDecision (..),
    LinearOptimizationDecision (..),
    StandardFeasibility,
    StandardLinearProgram,
    decideLinearFeasibility,
    exactLinearOptimumValue,
    farkasCertificateVector,
    linearProgramUnboundedDirection,
    mkStandardFeasibility,
    mkStandardLinearProgram,
    nonNegativeSolutionVector,
    optimizeLinearProgram,
    verifyExactLinearOptimum,
    verifyFarkasCertificate,
    verifyLinearProgramUnboundedWitness,
    verifyNonNegativeSolution,
  )
import Moonlight.LinAlg.Pure.Domain.PolyhedralCone
  ( ConeError (..),
    ConeMembership (..),
    ConeSide (..),
    GeneratedCone,
    NontrivialConeIntersection (..),
    coneMember,
    commonRayPoint,
    coneSectionDistanceLInf,
    generatedConeGenerators,
    intersectGeneratedCones,
    linfSectionDistanceValue,
    commonRayLeftCoefficients,
    commonRayRightCoefficients,
    mkCommonPositiveGauge,
    mkGeneratedCone,
    verifyCommonRayWitness,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)
import Prelude

tests :: TestTree
tests =
  testGroup
    "exact linear feasibility"
    [ testCase "returns and verifies an exact primal witness" testFeasibleWitness,
      testCase "returns and verifies a normalized Farkas certificate" testFarkasCertificate,
      testCase "accepts redundant exact equations" testRedundantEquations,
      testCase "zero-variable inconsistency is certified" testZeroVariableInfeasible,
      testCase "rejects a mismatched right-hand side" testMismatchedRightHandSide,
      testCase "cone membership descends to nonnegative feasibility" testConeMembership,
      testCase "cone nonmembership returns an exact separator" testConeSeparation,
      testCase "positive-gauge sections recover a nonzero common ray" testConeIntersection,
      testCase "disjoint positive-gauge sections return a separator" testProjectiveSeparation,
      testCase "a common gauge must be strictly positive on every ray" testGaugeRejection,
      testCase "linear optimization returns exact primal and dual certificates" testLinearOptimum,
      testCase "linear optimization returns an exact unbounded direction" testLinearUnbounded,
      testCase "linear optimization retains Phase-I infeasibility certificates" testLinearProgramInfeasible,
      testCase "L-infinity section distance is zero exactly at intersection" testIntersectingSectionDistance,
      testCase "L-infinity section distance names the chosen norm" testSeparatedSectionDistance
    ]

testFeasibleWitness :: Assertion
testFeasibleWitness =
  withProblem 2 2 [1, 0, 0, 1] [2, 3] $ \problem ->
    case decideLinearFeasibility problem of
      Right (Feasible solution) -> do
        assertEqual "primal coefficients" [2, 3] (dynVectorToList (nonNegativeSolutionVector solution))
        assertBool "primal certificate verifies" (verifyNonNegativeSolution problem solution)
      Right (Infeasible certificate) ->
        assertFailure ("expected a primal witness, received " <> show (dynVectorToList (farkasCertificateVector certificate)))
      Left failure -> assertFailure (show failure)

testFarkasCertificate :: Assertion
testFarkasCertificate =
  withProblem 2 2 [1, 0, 0, 1] [-1, 1] $ \problem ->
    case decideLinearFeasibility problem of
      Right (Infeasible certificate) -> do
        let values = dynVectorToList (farkasCertificateVector certificate)
        assertEqual "certificate normalization" (-1) (sum (zipWith (*) [-1, 1] values))
        assertBool "Farkas certificate verifies" (verifyFarkasCertificate problem certificate)
      Right (Feasible solution) ->
        assertFailure ("expected an obstruction, received " <> show (dynVectorToList (nonNegativeSolutionVector solution)))
      Left failure -> assertFailure (show failure)

testRedundantEquations :: Assertion
testRedundantEquations =
  withProblem 2 2 [1, 1, 2, 2] [1, 2] $ \problem ->
    case decideLinearFeasibility problem of
      Right (Feasible solution) -> assertBool "redundant system witness verifies" (verifyNonNegativeSolution problem solution)
      other -> assertFailure ("expected feasible redundant system, received " <> show other)

testZeroVariableInfeasible :: Assertion
testZeroVariableInfeasible =
  withProblem 1 0 [] [2] $ \problem ->
    case decideLinearFeasibility problem of
      Right (Infeasible certificate) -> assertBool "zero-variable certificate verifies" (verifyFarkasCertificate problem certificate)
      other -> assertFailure ("expected zero-variable obstruction, received " <> show other)

testMismatchedRightHandSide :: Assertion
testMismatchedRightHandSide =
  case do
    matrixValue <- first show (mkDynMatrix 2 1 ([1, 2] :: [Rational]))
    vectorValue <- first show (mkDynVector 1 ([1] :: [Rational]))
    first show (mkStandardFeasibility matrixValue vectorValue) of
    Left _ -> pure ()
    Right _ -> assertFailure "mismatched RHS was admitted"

testConeMembership :: Assertion
testConeMembership =
  withCone 2 [[1, 0], [0, 1]] $ \cone ->
    withVector [2, 3] $ \target ->
      case coneMember cone target of
        Right (ConeMember _) -> pure ()
        Right (ConeSeparated _) -> assertFailure "positive-orthant member was separated"
        Left failure -> assertFailure (show failure)

testConeSeparation :: Assertion
testConeSeparation =
  withCone 2 [[1, 0], [0, 1]] $ \cone ->
    withVector [-1, 1] $ \target ->
      case coneMember cone target of
        Right (ConeSeparated _) -> pure ()
        Right (ConeMember _) -> assertFailure "nonmember received a nonnegative combination"
        Left failure -> assertFailure (show failure)

testConeIntersection :: Assertion
testConeIntersection =
  withConePair [[1, 0], [0, 1]] [[1, 1]] $ \leftCone rightCone ->
    withVector [1, 1] $ \gauge ->
      case mkCommonPositiveGauge leftCone rightCone gauge of
        Left failure -> assertFailure (show failure)
        Right commonGauge ->
          case intersectGeneratedCones commonGauge of
            Right (ConesIntersect witness) -> do
              assertEqual "gauge-normalized common ray" [1 / 2, 1 / 2] (dynVectorToList (commonRayPoint witness))
              assertEqual "left coefficients use original generators" [1 / 2, 1 / 2] (dynVectorToList (commonRayLeftCoefficients witness))
              assertEqual "right coefficient rescales the gauge-two generator" [1 / 2] (dynVectorToList (commonRayRightCoefficients witness))
              assertBool "common-ray reconstruction verifies" (verifyCommonRayWitness commonGauge witness)
            Right (ConesProjectivelySeparated _) -> assertFailure "intersecting sections were separated"
            Left failure -> assertFailure (show failure)

testProjectiveSeparation :: Assertion
testProjectiveSeparation =
  withConePair [[1, 0]] [[0, 1]] $ \leftCone rightCone ->
    withVector [1, 1] $ \gauge ->
      case mkCommonPositiveGauge leftCone rightCone gauge >>= intersectGeneratedCones of
        Right (ConesProjectivelySeparated _) -> pure ()
        Right (ConesIntersect _) -> assertFailure "disjoint projective sections intersected"
        Left failure -> assertFailure (show failure)

testGaugeRejection :: Assertion
testGaugeRejection =
  withConePair [[1, 0]] [[0, 1]] $ \leftCone rightCone ->
    withVector [1, 0] $ \gauge ->
      case mkCommonPositiveGauge leftCone rightCone gauge of
        Left (ConeGaugeNotStrictlyPositive RightCone 0 0) -> pure ()
        Left other -> assertFailure ("wrong gauge obstruction: " <> show other)
        Right _ -> assertFailure "nonpositive common gauge was admitted"

testLinearOptimum :: Assertion
testLinearOptimum =
  withLinearProgram 1 2 [1, 1] [1] [1, 1] $ \linearProgram ->
    case optimizeLinearProgram linearProgram of
      Right (LinearProgramOptimal optimum) -> do
        assertEqual "exact optimum" 1 (exactLinearOptimumValue optimum)
        assertBool "primal and dual certificates verify" (verifyExactLinearOptimum linearProgram optimum)
      Right _ -> assertFailure "bounded feasible program did not return an optimum"
      Left failure -> assertFailure (show failure)

testLinearUnbounded :: Assertion
testLinearUnbounded =
  withLinearProgram 1 2 [1, -1] [0] [1, 0] $ \linearProgram ->
    case optimizeLinearProgram linearProgram of
      Right (LinearProgramUnbounded witness) -> do
        assertEqual "normalized improving direction" [1, 1] (dynVectorToList (linearProgramUnboundedDirection witness))
        assertBool "feasible base and improving direction verify" (verifyLinearProgramUnboundedWitness linearProgram witness)
      Right _ -> assertFailure "unbounded program did not return a direction"
      Left failure -> assertFailure (show failure)

testLinearProgramInfeasible :: Assertion
testLinearProgramInfeasible =
  withProblem 1 1 [1] [-1] $ \problem ->
    withVector [1] $ \objective ->
      case mkStandardLinearProgram problem objective >>= optimizeLinearProgram of
        Right (LinearProgramInfeasible certificate) ->
          assertBool
            "Phase-I Farkas certificate remains independently valid"
            (verifyFarkasCertificate problem certificate)
        Right _ -> assertFailure "infeasible program did not return a Farkas certificate"
        Left failure -> assertFailure (show failure)

testIntersectingSectionDistance :: Assertion
testIntersectingSectionDistance =
  withConePair [[1, 0], [0, 1]] [[1, 1]] $ \leftCone rightCone ->
    withVector [1, 1] $ \gauge ->
      case mkCommonPositiveGauge leftCone rightCone gauge >>= coneSectionDistanceLInf of
        Right distance -> assertEqual "intersecting section distance" 0 (linfSectionDistanceValue distance)
        Left failure -> assertFailure (show failure)

testSeparatedSectionDistance :: Assertion
testSeparatedSectionDistance =
  withConePair [[1, 0]] [[0, 1]] $ \leftCone rightCone ->
    withVector [1, 1] $ \gauge ->
      case mkCommonPositiveGauge leftCone rightCone gauge >>= coneSectionDistanceLInf of
        Right distance -> assertEqual "coordinate L-infinity section distance" 1 (linfSectionDistanceValue distance)
        Left failure -> assertFailure (show failure)

withProblem :: Int -> Int -> [Rational] -> [Rational] -> (StandardFeasibility -> Assertion) -> Assertion
withProblem rowCount columnCount entries rhsValues continuation =
  case do
    matrixValue <- first show (mkDynMatrix rowCount columnCount entries)
    vectorValue <- first show (mkDynVector rowCount rhsValues)
    first show (mkStandardFeasibility matrixValue vectorValue) of
    Left failure -> assertFailure failure
    Right problem -> continuation problem

withLinearProgram ::
  Int ->
  Int ->
  [Rational] ->
  [Rational] ->
  [Rational] ->
  (StandardLinearProgram -> Assertion) ->
  Assertion
withLinearProgram rowCount columnCount entries rhsValues objectiveValues continuation =
  withProblem rowCount columnCount entries rhsValues $ \problem ->
    case do
      objective <- first show (mkDynVector columnCount objectiveValues)
      first show (mkStandardLinearProgram problem objective) of
      Left failure -> assertFailure failure
      Right linearProgram -> continuation linearProgram

withVector :: [Rational] -> (DynVector Rational -> Assertion) -> Assertion
withVector values continuation =
  case mkDynVector (length values) values of
    Left failure -> assertFailure (show failure)
    Right vectorValue -> continuation vectorValue

withCone :: Int -> [[Rational]] -> (GeneratedCone -> Assertion) -> Assertion
withCone ambientDimension generatorValues continuation =
  case do
    generators <- traverse (first show . mkDynVector ambientDimension) generatorValues
    first show (mkGeneratedCone ambientDimension generators) of
    Left failure -> assertFailure (show failure)
    Right cone -> do
      assertEqual "generator count" (length generatorValues) (length (generatedConeGenerators cone))
      continuation cone

withConePair :: [[Rational]] -> [[Rational]] -> (GeneratedCone -> GeneratedCone -> Assertion) -> Assertion
withConePair leftGenerators rightGenerators continuation =
  withCone 2 leftGenerators $ \leftCone ->
    withCone 2 rightGenerators $ \rightCone ->
      continuation leftCone rightCone
