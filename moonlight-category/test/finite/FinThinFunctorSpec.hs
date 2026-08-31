module FinThinFunctorSpec
  ( tests,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Moonlight.Category.Effect.Fixture.FinCat (sampleFinCat)
import Moonlight.Category.Pure.FinCat
  ( FinGeneratorId (..),
    FinMorphismId (..),
    FinObjectId (..),
    mkFinCat,
  )
import Moonlight.Category.Pure.FinCat.Functor
  ( FinThinFunctorApplicationError (..),
    FinThinFunctorValidationError (..),
    applyFinThinFunctor,
    mkFinThinFunctor,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "finite thin functor"
    [ testCase "retains a validated total object action" testIdentityObjectAction,
      testCase "reports domain and codomain obstructions" testObjectMapObstructions,
      testCase "reports the first order obstruction" testOrderObstruction,
      testCase "rejects a non-thin source" testNonThinSource
    ]

identityObjectMap :: Map.Map FinObjectId FinObjectId
identityObjectMap =
  Map.fromList
    [ (FinObjectId 0, FinObjectId 0),
      (FinObjectId 1, FinObjectId 1),
      (FinObjectId 2, FinObjectId 2)
    ]

testIdentityObjectAction :: Assertion
testIdentityObjectAction =
  case mkFinThinFunctor sampleFinCat sampleFinCat identityObjectMap of
    Left validationError ->
      assertFailure ("expected identity object action to validate: " <> show validationError)
    Right functorValue -> do
      applyFinThinFunctor functorValue (FinObjectId 1) @?= Right (FinObjectId 1)
      applyFinThinFunctor functorValue (FinObjectId 99)
        @?= Left (FinThinFunctorUnknownSourceObject (FinObjectId 99))

testObjectMapObstructions :: Assertion
testObjectMapObstructions = do
  assertValidationError
    (FinThinFunctorMissingSourceObject (FinObjectId 2))
    (mkFinThinFunctor sampleFinCat sampleFinCat (Map.delete (FinObjectId 2) identityObjectMap))
  assertValidationError
    (FinThinFunctorTargetObjectAbsent (FinObjectId 0) (FinObjectId 99))
    ( mkFinThinFunctor
        sampleFinCat
        sampleFinCat
        (Map.insert (FinObjectId 0) (FinObjectId 99) identityObjectMap)
    )

testOrderObstruction :: Assertion
testOrderObstruction =
  assertValidationError
    ( FinThinFunctorOrderNotPreserved
        (FinObjectId 0)
        (FinObjectId 1)
        (FinObjectId 2)
        (FinObjectId 1)
    )
    ( mkFinThinFunctor
        sampleFinCat
        sampleFinCat
        ( Map.fromList
            [ (FinObjectId 0, FinObjectId 2),
              (FinObjectId 1, FinObjectId 1),
              (FinObjectId 2, FinObjectId 0)
            ]
        )
    )

testNonThinSource :: Assertion
testNonThinSource =
  case
    mkFinCat
      (Set.fromList [FinObjectId 0, FinObjectId 1])
      ( Map.singleton
          (FinObjectId 0, FinObjectId 1)
          [ FinGeneratorMorphismId (FinGeneratorId 20),
            FinGeneratorMorphismId (FinGeneratorId 21)
          ]
      )
      Map.empty
    of
      Left validationErrors ->
        assertFailure ("expected the parallel-pair fixture to be a category: " <> show validationErrors)
      Right nonThinCategory ->
        assertValidationError
          FinThinFunctorSourceNotThin
          ( mkFinThinFunctor
              nonThinCategory
              sampleFinCat
              (Map.fromList [(FinObjectId 0, FinObjectId 0), (FinObjectId 1, FinObjectId 1)])
          )

assertValidationError ::
  FinThinFunctorValidationError ->
  Either FinThinFunctorValidationError functorValue ->
  Assertion
assertValidationError expected result =
  case result of
    Left actual -> actual @?= expected
    Right _ -> assertFailure ("expected finite-thin-functor obstruction: " <> show expected)
