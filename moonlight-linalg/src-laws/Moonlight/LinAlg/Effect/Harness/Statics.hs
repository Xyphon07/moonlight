module Moonlight.LinAlg.Effect.Harness.Statics
  ( networkDeclarationOrderInvariantLaw,
    repeatedLoadsAccumulateLaw,
    equilibriumAssemblyCanonicalOrderingLaw,
    equilibriumSolutionResidualBoundedLaw,
    unsupportedLoadProducesResidualViolationLaw,
  )
where

import Data.Bifunctor (first)
import Data.List (permutations)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Moonlight.LinAlg
  ( Axis (..),
    EquilibriumResult (..),
    EquilibriumSolution (..),
    EquilibriumViolation (..),
    Vec3 (..),
    assembleEquilibriumEquations,
    checkEquilibrium,
    compiledFoundationOrder,
    compiledMemberOrder,
    compiledNodeOrder,
    joint,
    load,
    member,
    mkMemberRef,
    network,
    networkNodeMap,
    nodeLoad,
    nodeRef,
    supportOn,
    mkSupportAxes,
  )
import Moonlight.LinAlg.Effect.Harness.Core (approxTolerance, assertApproxEqual, assertRightProperty)
import Test.Tasty.QuickCheck qualified as QC

newtype VerticalLoad = VerticalLoad Double
  deriving stock (Eq, Show)

newtype LoadPair = LoadPair (Double, Double)
  deriving stock (Eq, Show)

instance QC.Arbitrary VerticalLoad where
  arbitrary =
    VerticalLoad . fromIntegral <$> QC.chooseInt (1, 20)

instance QC.Arbitrary LoadPair where
  arbitrary =
    LoadPair
      <$> ((,) <$> (fromIntegral <$> QC.chooseInt (-20, 20)) <*> (fromIntegral <$> QC.chooseInt (-20, 20)))

networkDeclarationOrderInvariantLaw :: QC.Property
networkDeclarationOrderInvariantLaw =
  QC.property networkDeclarationOrderInvariantLawProperty

repeatedLoadsAccumulateLaw :: QC.Property
repeatedLoadsAccumulateLaw =
  QC.property repeatedLoadsAccumulateLawProperty

equilibriumAssemblyCanonicalOrderingLaw :: QC.Property
equilibriumAssemblyCanonicalOrderingLaw =
  QC.property equilibriumAssemblyCanonicalOrderingLawProperty

equilibriumSolutionResidualBoundedLaw :: QC.Property
equilibriumSolutionResidualBoundedLaw =
  QC.property equilibriumSolutionResidualBoundedLawProperty

unsupportedLoadProducesResidualViolationLaw :: QC.Property
unsupportedLoadProducesResidualViolationLaw =
  QC.property unsupportedLoadProducesResidualViolationLawProperty

networkDeclarationOrderInvariantLawProperty :: VerticalLoad -> QC.Property
networkDeclarationOrderInvariantLawProperty (VerticalLoad loadMagnitude) =
  let declarations =
        [ supportOn "a" (Vec3 0.0 0.0 0.0) (mkSupportAxes [AxisY]),
          load "b" (Vec3 0.0 1.0 0.0) (Vec3 0.0 (negate loadMagnitude) 0.0),
          member "a" "b"
        ]
      results = network <$> permutations declarations
   in QC.counterexample (show results) (allEqual results)

repeatedLoadsAccumulateLawProperty :: LoadPair -> QC.Property
repeatedLoadsAccumulateLawProperty (LoadPair (firstLoad, secondLoad)) =
  case (nodeRef "p", network declarations) of
    (Right pointRef, Right networkValue) ->
      case Map.lookup pointRef (networkNodeMap networkValue) of
        Just nodeValue ->
          QC.property (nodeLoad nodeValue == Vec3 (firstLoad + secondLoad) 0.0 0.0)
        Nothing -> QC.counterexample "missing generated node p" False
    other -> QC.counterexample (show other) False
  where
    declarations =
      [ load "p" (Vec3 0.0 0.0 0.0) (Vec3 firstLoad 0.0 0.0),
        load "p" (Vec3 0.0 0.0 0.0) (Vec3 secondLoad 0.0 0.0)
      ]

equilibriumAssemblyCanonicalOrderingLawProperty :: VerticalLoad -> QC.Property
equilibriumAssemblyCanonicalOrderingLawProperty (VerticalLoad loadMagnitude) =
  assertRightProperty $ do
    networkValue <-
      mapLeftShow $
        network
        [ member "c" "a",
          joint "c" (Vec3 0.0 1.0 0.0),
          supportOn "a" (Vec3 (-1.0) 0.0 0.0) (mkSupportAxes [AxisY]),
          member "b" "c",
          supportOn "b" (Vec3 1.0 0.0 0.0) (mkSupportAxes [AxisY]),
          load "c" (Vec3 0.0 1.0 0.0) (Vec3 0.0 (negate loadMagnitude) 0.0)
        ]
    compiledValue <- mapLeftShow (assembleEquilibriumEquations networkValue)
    nodeA <- mapLeftShow (nodeRef "a")
    nodeB <- mapLeftShow (nodeRef "b")
    nodeC <- mapLeftShow (nodeRef "c")
    leftMember <- mapLeftShow (mkMemberRef nodeA nodeC)
    rightMember <- mapLeftShow (mkMemberRef nodeB nodeC)
    pure
      ( compiledNodeOrder compiledValue == [nodeA, nodeB, nodeC]
          && compiledFoundationOrder compiledValue == [nodeA, nodeB]
          && compiledMemberOrder compiledValue == [leftMember, rightMember]
      )

equilibriumSolutionResidualBoundedLawProperty :: VerticalLoad -> QC.Property
equilibriumSolutionResidualBoundedLawProperty (VerticalLoad loadMagnitude) =
  assertRightProperty $ do
    networkValue <-
      mapLeftShow $
        network
        [ load "load" (Vec3 0.0 1.0 0.0) (Vec3 0.0 (negate loadMagnitude) 0.0),
          supportOn "foundation" (Vec3 0.0 0.0 0.0) (mkSupportAxes [AxisY]),
          member "foundation" "load"
        ]
    equilibriumResult <- mapLeftShow (checkEquilibrium networkValue)
    pure
      ( case equilibriumResult of
          InEquilibrium solutionValue ->
            all residualBounded (Map.elems (equilibriumResidualForces solutionValue))
          Disequilibrium _ -> False
      )

unsupportedLoadProducesResidualViolationLawProperty :: VerticalLoad -> QC.Property
unsupportedLoadProducesResidualViolationLawProperty (VerticalLoad loadMagnitude) =
  assertRightProperty $ do
    networkValue <-
      mapLeftShow $
        network
        [ supportOn "foundation" (Vec3 0.0 0.0 0.0) (mkSupportAxes [AxisY]),
          load "load" (Vec3 1.0 0.0 0.0) (Vec3 0.0 (negate loadMagnitude) 0.0),
          member "foundation" "load"
        ]
    equilibriumResult <- mapLeftShow (checkEquilibrium networkValue)
    pure
      ( case equilibriumResult of
          InEquilibrium _ -> False
          Disequilibrium violations ->
            any ((> approxTolerance) . violationResidualMagnitude) (NonEmpty.toList violations)
      )

residualBounded :: Vec3 -> Bool
residualBounded (Vec3 xValue yValue zValue) =
  assertApproxEqual 0.0 xValue
    && assertApproxEqual 0.0 yValue
    && assertApproxEqual 0.0 zValue

allEqual :: Eq value => [value] -> Bool
allEqual values =
  case values of
    [] -> True
    firstValue : remainingValues -> all (== firstValue) remainingValues

mapLeftShow :: Show failure => Either failure value -> Either String value
mapLeftShow = first show
