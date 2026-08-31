{-| A test-tree algebra for QuickCheck, Hedgehog, HUnit, and nested laws. -}
module Moonlight.Pale.Test.Laws.Suite
  ( LawSuite,
    quickCheckLaw,
    namedQuickCheckLaw,
    hedgehogLaw,
    namedHedgehogLaw,
    hUnitLaw,
    testTreeLaw,
    lawGroup,
    renderLawSuite,
  )
where

import Data.Kind (Type)
import qualified Hedgehog as HH
import Moonlight.Core (IsLawName (..))
import Prelude (Bool, Show, String, map, (.), (>>=))
import Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty.Hedgehog as TH
import Test.Tasty.HUnit (Assertion, testCase)
import qualified Test.Tasty.QuickCheck as QC

type LawSuite :: Type
data LawSuite
  = QuickCheckLaw !String !QC.Property
  | HedgehogLaw !String !HH.Property
  | HUnitLaw !String !Assertion
  | EmbeddedTest !TestTree
  | LawGroup !String ![LawSuite]

quickCheckLaw :: QC.Testable property => String -> property -> LawSuite
quickCheckLaw lawLabel lawProperty =
  QuickCheckLaw lawLabel (QC.property lawProperty)

namedQuickCheckLaw ::
  (IsLawName lawName, QC.Testable property) =>
  lawName ->
  property ->
  LawSuite
namedQuickCheckLaw lawName =
  quickCheckLaw (lawNameText lawName)

hedgehogLaw :: Show value => String -> HH.Gen value -> (value -> Bool) -> LawSuite
hedgehogLaw lawLabel generator predicate =
  HedgehogLaw lawLabel (HH.property (HH.forAll generator >>= HH.assert . predicate))

namedHedgehogLaw ::
  (IsLawName lawName, Show value) =>
  lawName ->
  HH.Gen value ->
  (value -> Bool) ->
  LawSuite
namedHedgehogLaw lawName =
  hedgehogLaw (lawNameText lawName)

hUnitLaw :: String -> Assertion -> LawSuite
hUnitLaw = HUnitLaw

testTreeLaw :: TestTree -> LawSuite
testTreeLaw = EmbeddedTest

lawGroup :: String -> [LawSuite] -> LawSuite
lawGroup = LawGroup

renderLawSuite :: LawSuite -> TestTree
renderLawSuite lawSuite =
  case lawSuite of
    QuickCheckLaw lawLabel lawProperty ->
      QC.testProperty lawLabel lawProperty
    HedgehogLaw lawLabel lawProperty ->
      TH.testProperty lawLabel lawProperty
    HUnitLaw lawLabel assertion ->
      testCase lawLabel assertion
    EmbeddedTest testTree ->
      testTree
    LawGroup groupLabel nestedLaws ->
      testGroup groupLabel (map renderLawSuite nestedLaws)
