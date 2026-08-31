module HomotopySpec
  ( tests,
  )
where

import Data.Containers.ListUtils (nubOrd)
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Moonlight.Category.Effect.Fixture.FinCat (sampleFinCat)
import Moonlight.Category
  ( FinCat,
    FinGeneratorId (..),
    FinMorphismId (..),
    FinObjectId (..),
    mkFinCat,
  )
import Moonlight.Category.Simplicial
  ( automorphismGroupAt,
    automorphismGroupoidOfNerve,
    automorphismGroupoidObjects,
    coreGroupoidObjects,
    coreGroupoidMorphisms,
    coreGroupoidMorphismsBetween,
    coreGroupoidOfNerve,
    forgetAutomorphismGroupoidMorphism,
    forgetCoreGroupoidMorphism,
    pi0Nerve,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

disconnectedCategory :: FinCat
disconnectedCategory =
  case mkFinCat (Set.fromList [FinObjectId 0, FinObjectId 1]) Map.empty Map.empty of
    Right categoryValue -> categoryValue
    Left _ -> sampleFinCat

generatorMorphismId :: Int -> FinMorphismId
generatorMorphismId = FinGeneratorMorphismId . FinGeneratorId

identityMorphismId :: Int -> FinMorphismId
identityMorphismId = FinIdentityId . FinObjectId

isomorphicPairCategory :: FinCat
isomorphicPairCategory =
  case
    mkFinCat
      (Set.fromList [FinObjectId 0, FinObjectId 1])
      (Map.fromList [((FinObjectId 0, FinObjectId 1), [generatorMorphismId 10]), ((FinObjectId 1, FinObjectId 0), [generatorMorphismId 11])])
      (Map.fromList [((generatorMorphismId 11, generatorMorphismId 10), identityMorphismId 0), ((generatorMorphismId 10, generatorMorphismId 11), identityMorphismId 1)]) of
    Right categoryValue -> categoryValue
    Left _ -> sampleFinCat

tests :: TestTree
tests =
  testGroup
    "Homotopy"
    [ testCase "pi0 for sample category is connected" $
        assertEqual "sample pi0 components" 1 (length (pi0Nerve sampleFinCat)),
      testCase "pi0 separates disconnected category" $
        assertEqual "disconnected pi0 components" 2 (length (pi0Nerve disconnectedCategory)),
      testCase "invertible morphisms detect isomorphism pair" $
        let invertibles =
              coreGroupoidMorphisms (coreGroupoidOfNerve isomorphicPairCategory)
                & fmap forgetCoreGroupoidMorphism
         in assertBool "expected at least two non-identity invertibles" (length invertibles >= 4),
      testCase "invertible morphisms are unique" $
        let invertibles =
              coreGroupoidMorphisms (coreGroupoidOfNerve isomorphicPairCategory)
                & fmap forgetCoreGroupoidMorphism
         in assertEqual "invertible morphisms should not repeat" invertibles (nubOrd invertibles),
      testCase "core groupoid of nerve agrees with its endpoint decomposition" $
        let coreGroupoidValue = coreGroupoidOfNerve isomorphicPairCategory
            bucketedMorphisms =
              coreGroupoidObjects coreGroupoidValue
                >>= (\sourceObject ->
                       coreGroupoidObjects coreGroupoidValue
                         >>= (\targetObject ->
                                coreGroupoidMorphismsBetween coreGroupoidValue sourceObject targetObject
                            )
                    )
         in assertEqual
              "core groupoid morphisms should be the union of endpoint buckets"
              (coreGroupoidMorphisms coreGroupoidValue)
              (nubOrd bucketedMorphisms),
      testCase "core groupoid endpoint query isolates directed isomorphisms" $
        case coreGroupoidObjects (coreGroupoidOfNerve isomorphicPairCategory) of
          sourceObject : targetObject : _ ->
            let coreGroupoidValue = coreGroupoidOfNerve isomorphicPairCategory
                forwardMorphisms =
                  coreGroupoidMorphismsBetween coreGroupoidValue sourceObject targetObject
             in assertEqual "expected exactly one forward invertible generator" 1 (length forwardMorphisms)
          _ -> assertBool "expected two objects in isomorphic pair category" False,
      testCase "automorphism groupoid on sample category is identity loop" $
        case automorphismGroupoidObjects (automorphismGroupoidOfNerve sampleFinCat) of
          [] -> assertBool "expected base object" False
          baseObject : _ ->
            let automorphismGroupoidValue = automorphismGroupoidOfNerve sampleFinCat
             in assertEqual
                  "sample core automorphism size"
                  1
                  (length (fmap forgetAutomorphismGroupoidMorphism (automorphismGroupAt automorphismGroupoidValue baseObject)))
    ]
