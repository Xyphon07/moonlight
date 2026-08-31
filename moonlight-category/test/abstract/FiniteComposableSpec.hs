module FiniteComposableSpec
  ( tests,
  )
where

import Data.List.NonEmpty qualified as NonEmpty
import Moonlight.Category.Pure.Category (Category (..))
import Moonlight.Category.Effect.Fixture.FinCat (sampleFinCat)
import Moonlight.Category.Pure.FinCat
  ( FinObjectId (..),
    finCatHomMorphism,
    finObjectIdentityMor,
    mkFinObject,
  )
import Moonlight.Category.Pure.FiniteComposable
  ( FiniteComposableCategory (..),
    chainDimension,
    chainMorphisms,
    chainTerminalObject,
    chainVertices,
    mkComposableChain,
    singletonComposableChain,
  )
import Moonlight.Pale.Test.Assertions (expectRightWithLabel, expectSome)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "FiniteComposable"
    [ testCase "checked chains retain a total vertex sequence" testCheckedChain,
      testCase "Natural dimension bounds do not overflow through Int" testNaturalDimensionBound,
      testCase "identity construction requires a validated object" testCheckedIdentity
    ]

testCheckedChain :: Assertion
testCheckedChain = do
  object0 <- expectRightWithLabel "object 0" (mkFinObject sampleFinCat (FinObjectId 0))
  object1 <- expectRightWithLabel "object 1" (mkFinObject sampleFinCat (FinObjectId 1))
  object2 <- expectRightWithLabel "object 2" (mkFinObject sampleFinCat (FinObjectId 2))
  morphism01 <- expectSome "morphism 0 -> 1" (finCatHomMorphism sampleFinCat (FinObjectId 0) (FinObjectId 1))
  morphism12 <- expectSome "morphism 1 -> 2" (finCatHomMorphism sampleFinCat (FinObjectId 1) (FinObjectId 2))
  case mkComposableChain sampleFinCat object0 [morphism01, morphism12] of
    Left _ -> assertFailure "expected a composable chain"
    Right chainValue -> do
      chainDimension chainValue @?= 2
      chainTerminalObject chainValue @?= object2
      chainMorphisms chainValue @?= [morphism01, morphism12]
      chainVertices chainValue @?= object0 NonEmpty.:| [object1, object2]
      NonEmpty.last (chainVertices chainValue) @?= chainTerminalObject chainValue
      NonEmpty.length (chainVertices chainValue) @?= 3
      chainVertices (singletonComposableChain object0) @?= object0 NonEmpty.:| []

testNaturalDimensionBound :: Assertion
testNaturalDimensionBound =
  assertBool
    "a valid enormous Natural bound must retain the dimension-zero chains"
    (not (null (take 1 (enumerateComposableChains sampleFinCat (fromIntegral (maxBound :: Int))))))

testCheckedIdentity :: Assertion
testCheckedIdentity = do
  case mkFinObject sampleFinCat (FinObjectId 99) of
    Left _ -> pure ()
    Right _ -> assertFailure "an undeclared raw object id crossed the validated boundary"
  object0 <- expectRightWithLabel "object 0" (mkFinObject sampleFinCat (FinObjectId 0))
  identity sampleFinCat object0 @?= Right (finObjectIdentityMor object0)
