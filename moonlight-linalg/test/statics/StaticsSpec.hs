module StaticsSpec
  ( tests,
  )
where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Moonlight.LinAlg
  ( Axis (..),
    EquationRef (..),
    EquilibriumResult (..),
    EquilibriumSolution (..),
    EquilibriumViolation (..),
    ForceNetwork,
    ForceSign (..),
    MemberRef,
    NetworkBuildError (..),
    NetworkDeclaration,
    NodeRef,
    UnknownForce (..),
    Vec3 (..),
    assembleEquilibriumEquations,
    checkEquilibrium,
    compiledEquationOrder,
    compiledFoundationOrder,
    compiledMemberOrder,
    compiledNodeOrder,
    compiledUnknownOrder,
    joint,
    load,
    member,
    mkMemberRef,
    mkSupportAxes,
    network,
    networkNodeMap,
    nodeLoad,
    nodeRef,
    support,
    supportOn,
  )
import Helpers (extractRight)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "Statics"
    [ testCase "assembleEquilibriumEquations uses canonical NodeRef ordering" testCanonicalAssembly,
      testCase "assembleEquilibriumEquations emits reactions only for supported axes" testAxisSpecificSupportUnknowns,
      testCase "network declarations are order-independent" testDeclarationOrderIndependence,
      testCase "network repeated loads accumulate exactly before rounding" testRepeatedLoadsUseOneRoundedExactSum,
      testCase "network rejects conflicting node positions" testConflictingPositionsFail,
      testCase "network rejects unknown member endpoints" testUnknownEndpointFails,
      testCase "network rejects degenerate members" testDegenerateMemberFails,
      testCase "checkEquilibrium resolves a compressive vertical support" testVerticalEquilibrium,
      testCase "checkEquilibrium rejects tension-only hanging support" testCompressionOnlyViolation,
      testCase "checkEquilibrium reports residual force for unsupported load" testResidualViolation
    ]

testCanonicalAssembly :: Assertion
testCanonicalAssembly = do
  leftMember <- expectMember "c" "a"
  rightMember <- expectMember "b" "c"
  nodeA <- expectNodeRef "a"
  nodeB <- expectNodeRef "b"
  nodeC <- expectNodeRef "c"
  networkValue <-
    expectNetwork
      [ member "c" "a",
        joint "c" (Vec3 0.0 1.0 0.0),
        support "a" (Vec3 (-1.0) 0.0 0.0),
        member "b" "c",
        support "b" (Vec3 1.0 0.0 0.0)
      ]
  extractRight (assembleEquilibriumEquations networkValue) $ \compiledValue -> do
    assertEqual "node order" [nodeA, nodeB, nodeC] (compiledNodeOrder compiledValue)
    assertEqual "foundation order" [nodeA, nodeB] (compiledFoundationOrder compiledValue)
    assertEqual "member order" [leftMember, rightMember] (compiledMemberOrder compiledValue)
    assertEqual
      "equation order begins with first node"
      [ EquationRef nodeA AxisX,
        EquationRef nodeA AxisY,
        EquationRef nodeA AxisZ
      ]
      (take 3 (compiledEquationOrder compiledValue))

testAxisSpecificSupportUnknowns :: Assertion
testAxisSpecificSupportUnknowns = do
  foundationRef <- expectNodeRef "foundation"
  networkValue <-
    expectNetwork
      [ supportOn
          "foundation"
          (Vec3 0.0 0.0 0.0)
          (mkSupportAxes [AxisY])
      ]
  extractRight (assembleEquilibriumEquations networkValue) $ \compiledValue ->
    assertEqual
      "axis-specific support emits only its declared reaction unknown"
      [ReactionUnknown foundationRef AxisY]
      (compiledUnknownOrder compiledValue)

testDeclarationOrderIndependence :: Assertion
testDeclarationOrderIndependence = do
  let declarations =
        [ support "a" (Vec3 0.0 0.0 0.0),
          load "b" (Vec3 0.0 1.0 0.0) (Vec3 0.0 (-4.0) 0.0),
          load "b" (Vec3 0.0 1.0 0.0) (Vec3 0.0 (-6.0) 0.0),
          member "a" "b"
        ]
  forwardNetwork <- expectNetwork declarations
  reverseNetwork <- expectNetwork (reverse declarations)
  assertEqual "declaration order" forwardNetwork reverseNetwork

testRepeatedLoadsUseOneRoundedExactSum :: Assertion
testRepeatedLoadsUseOneRoundedExactSum = do
  pointReference <- expectNodeRef "p"
  networkValue <-
    expectNetwork
      [ load "p" (Vec3 0.0 0.0 0.0) (Vec3 1.0e16 0.0 0.0),
        load "p" (Vec3 0.0 0.0 0.0) (Vec3 1.0 0.0 0.0),
        load "p" (Vec3 0.0 0.0 0.0) (Vec3 (-1.0e16) 0.0 0.0)
      ]
  case Map.lookup pointReference (networkNodeMap networkValue) of
    Nothing -> assertFailure "expected node p"
    Just nodeValue ->
      assertEqual
        "exactly accumulated load"
        (Vec3 1.0 0.0 0.0)
        (nodeLoad nodeValue)

testConflictingPositionsFail :: Assertion
testConflictingPositionsFail =
  case
    network
      [ joint "p" (Vec3 0.0 0.0 0.0),
        load "p" (Vec3 1.0 0.0 0.0) (Vec3 0.0 1.0 0.0)
      ]
    of
    Left (ConflictingNodePosition "p" (Vec3 0.0 0.0 0.0) (Vec3 1.0 0.0 0.0)) ->
      pure ()
    Left other ->
      assertFailure ("unexpected construction error: " <> show other)
    Right _ ->
      assertFailure "expected conflicting positions to fail"

testUnknownEndpointFails :: Assertion
testUnknownEndpointFails =
  case
    network
      [ joint "a" (Vec3 0.0 0.0 0.0),
        member "a" "b"
      ]
    of
    Left (UnknownMemberEndpoint "b") ->
      pure ()
    Left other ->
      assertFailure ("unexpected construction error: " <> show other)
    Right _ ->
      assertFailure "expected unknown member endpoint to fail"

testDegenerateMemberFails :: Assertion
testDegenerateMemberFails =
  case
    network
      [ joint "a" (Vec3 0.0 0.0 0.0),
        joint "b" (Vec3 0.0 0.0 0.0),
        member "a" "b"
      ]
    of
    Left (DegenerateMember "a" "b") ->
      pure ()
    Left other ->
      assertFailure ("unexpected construction error: " <> show other)
    Right _ ->
      assertFailure "expected zero-length member to fail"

testVerticalEquilibrium :: Assertion
testVerticalEquilibrium = do
  memberRefValue <- expectMember "foundation" "load"
  foundationRef <- expectNodeRef "foundation"
  networkValue <-
    expectNetwork
      [ load "load" (Vec3 0.0 1.0 0.0) (Vec3 0.0 (-10.0) 0.0),
        supportOn "foundation" (Vec3 0.0 0.0 0.0) (mkSupportAxes [AxisY]),
        member "foundation" "load"
      ]
  extractRight (checkEquilibrium networkValue) $ \equilibriumResult ->
    case equilibriumResult of
      InEquilibrium solutionValue -> do
        assertApprox "member force" 10.0 (Map.findWithDefault 0.0 memberRefValue (equilibriumMemberForces solutionValue))
        assertVec3Approx
          "foundation reaction"
          (Vec3 0.0 10.0 0.0)
          (Map.findWithDefault (Vec3 0.0 0.0 0.0) foundationRef (equilibriumReactionForces solutionValue))
      Disequilibrium violations ->
        assertBool ("expected equilibrium, got " <> show violations) False

testCompressionOnlyViolation :: Assertion
testCompressionOnlyViolation = do
  networkValue <-
    expectNetwork
      [ support "left" (Vec3 (-1.0) 1.0 0.0),
        support "right" (Vec3 1.0 1.0 0.0),
        load "load" (Vec3 0.0 0.0 0.0) (Vec3 0.0 (-10.0) 0.0),
        member "left" "load",
        member "right" "load"
      ]
  extractRight (checkEquilibrium networkValue) $ \equilibriumResult ->
    case equilibriumResult of
      InEquilibrium solutionValue ->
        assertBool ("expected compression-only violation, got " <> show solutionValue) False
      Disequilibrium violations ->
        assertBool
          "expected at least one tension violation"
          ( any
              ((== Just Tension) . violationMemberForceSign)
              (NonEmpty.toList violations)
          )

testResidualViolation :: Assertion
testResidualViolation = do
  networkValue <-
    expectNetwork
      [ supportOn "foundation" (Vec3 0.0 0.0 0.0) (mkSupportAxes [AxisY]),
        load "load" (Vec3 1.0 0.0 0.0) (Vec3 0.0 (-10.0) 0.0),
        member "foundation" "load"
      ]
  extractRight (checkEquilibrium networkValue) $ \equilibriumResult ->
    case equilibriumResult of
      InEquilibrium solutionValue ->
        assertBool ("expected residual violation, got " <> show solutionValue) False
      Disequilibrium violations ->
        assertBool
          "expected non-zero residual"
          ( any
              ((> 1.0e-6) . violationResidualMagnitude)
              (NonEmpty.toList violations)
          )

expectNetwork :: [NetworkDeclaration] -> IO ForceNetwork
expectNetwork declarations =
  case network declarations of
    Left buildError ->
      assertFailure ("expected valid network, got " <> show buildError)
    Right networkValue ->
      pure networkValue

expectNodeRef :: String -> IO NodeRef
expectNodeRef labelValue =
  case nodeRef labelValue of
    Left buildError ->
      assertFailure ("expected valid node reference, got " <> show buildError)
    Right nodeReference ->
      pure nodeReference

expectMember :: String -> String -> IO MemberRef
expectMember leftLabel rightLabel = do
  leftReference <- expectNodeRef leftLabel
  rightReference <- expectNodeRef rightLabel
  case mkMemberRef leftReference rightReference of
    Left err -> assertFailure ("expected valid member, got " <> show err)
    Right memberRefValue -> pure memberRefValue

assertApprox :: String -> Double -> Double -> Assertion
assertApprox message expected actual =
  assertBool
    (message <> ": expected " <> show expected <> " but received " <> show actual)
    (abs (expected - actual) <= 1.0e-6)

assertVec3Approx :: String -> Vec3 -> Vec3 -> Assertion
assertVec3Approx message expected actual = do
  assertApprox (message <> " x") (vecX expected) (vecX actual)
  assertApprox (message <> " y") (vecY expected) (vecY actual)
  assertApprox (message <> " z") (vecZ expected) (vecZ actual)
