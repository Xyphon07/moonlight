{-# LANGUAGE PatternSynonyms #-}

module Hie.TypeWordsSpec (tests) where

import Data.Array (Array, array)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word64)
import GHC.Iface.Ext.Types (HieArgs (..), HieType (..), HieTypeFlat, TypeIndex)
import GHC.Types.Name (Name, mkSystemName, nameUnique)
import GHC.Types.Name.Occurrence (mkTyVarOcc)
import GHC.Types.Unique (getKey, mkUnique)
import Language.Haskell.Syntax.Specificity (data Specified)
import Moonlight.Pale.Ghc.Hie.TypeWords
  ( TypeGraphObstruction (..),
    TypeWireFailure (..),
    TypeWord (..),
    hieTypeIndexTypeWords,
    hieTypeRootsTypeWords,
    typeWords,
    typeWordsList,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "pale.hie.typewords"
    [ testCase "forall binder names are alpha-normalized" $
        assertEqual
          "forall a. a -> a and forall b. b -> b encode identically"
          (hieTypeIndexTypeWords (forallIdentityTable "a") forallRoot)
          (hieTypeIndexTypeWords (forallIdentityTable "b") forallRoot),
      testCase "free type variables keep their identity" $
        assertBool
          "free a and free b encode differently"
          (hieTypeIndexTypeWords (freeVariableTable "a") freeRoot /= hieTypeIndexTypeWords (freeVariableTable "b") freeRoot),
      testCase "display-equivalent free variables retain exact Name identity" $
        assertBool
          "equal occurrence spelling with different uniques remains distinct"
          ( hieTypeIndexTypeWords (freeNameTable (testNameWithUnique "a" 1)) freeRoot
              /= hieTypeIndexTypeWords (freeNameTable (testNameWithUnique "a" 2)) freeRoot
          ),
      testCase "shared DAG nodes are emitted once rather than recursively unfolded" $
        case hieTypeIndexTypeWords (sharedDagTable 30) 30 of
          Left obstruction ->
            assertFailure ("unexpected graph obstruction: " <> show obstruction)
          Right wordsValue ->
            assertBool
              "a 31-node doubling DAG has a linear wire representation"
              (length (typeWordsList wordsValue) < 1000),
      testCase "diamond sharing emits the common child once" $
        case hieTypeIndexTypeWords diamondTable 3 of
          Left obstruction ->
            assertFailure ("unexpected graph obstruction: " <> show obstruction)
          Right wordsValue ->
            assertBool
              "the diamond remains a four-definition graph"
              (length (typeWordsList wordsValue) < 80),
      testCase "a root set is compiled by one shared graph pass" $
        let compiledRoots =
              hieTypeRootsTypeWords
                (sharedDagTable 30)
                (Set.fromList [29, 30])
         in assertBool
              "both distinct observed roots are present and successful"
              ( Map.size compiledRoots == 2
                  && all (either (const False) (const True)) compiledRoots
              ),
      testCase "missing type indices are typed obstructions" $
        assertEqual
          "the missing child is not encoded as a successful sentinel"
          (Left (MissingTypeIndex 1))
          (hieTypeIndexTypeWords (array (0, 0) [(0, HCastTy 1)]) 0),
      testCase "cyclic type indices are typed obstructions" $
        assertEqual
          "the cycle is not encoded as a successful sentinel"
          (Left (CyclicTypeIndex 0))
          (hieTypeIndexTypeWords (array (0, 0) [(0, HCastTy 0)]) 0),
      testCase "shared bound variables cannot escape their forall scope" $
        let binderName = testName "a"
         in assertEqual
              "the shared variable node is not silently reclassified as free"
              (Left (EscapedBoundTypeVariable 1 (getKey (nameUnique binderName))))
              (hieTypeIndexTypeWords (escapedBinderTable binderName) 3),
      testCase "independent roots retain independent binder-scope evidence" $
        let binderName = testName "a"
            compiledRoots =
              hieTypeRootsTypeWords
                (independentBinderRootsTable binderName)
                (Set.fromList [1, 2])
         in assertBool
              "a shared flat variable may be free in one root and bound in another"
              ( Map.size compiledRoots == 2
                  && all (either (const False) (const True)) compiledRoots
              ),
      testCase "an earlier root memo cannot mask an intra-root binder escape" $
        let binderName = testName "a"
            compiledRoots =
              hieTypeRootsTypeWords
                (escapedBinderTable binderName)
                (Set.fromList [1, 3])
         in assertEqual
              "the second root revalidates the memoized variable in its own scope"
              (Just (Left (EscapedBoundTypeVariable 1 (getKey (nameUnique binderName)))))
              (Map.lookup 3 compiledRoots),
      testCase "memoized composites replay descendant binder evidence" $
        let binderName = testName "a"
            compiledRoots =
              hieTypeRootsTypeWords
                (compositeEscapedBinderTable binderName)
                (Set.fromList [2, 4])
         in assertEqual
              "the cached cast cannot hide its free variable beneath the forall root"
              (Just (Left (EscapedBoundTypeVariable 1 (getKey (nameUnique binderName)))))
              (Map.lookup 4 compiledRoots),
      testCase "variable-rich doubling preserves exact free-variable identity" $
        case
            ( hieTypeIndexTypeWords
                (variableRichDoublingTable (testNameWithUnique "a" 1) (testNameWithUnique "b" 2))
                5,
              hieTypeIndexTypeWords
                (variableRichDoublingTable (testNameWithUnique "a" 1) (testNameWithUnique "b" 3))
                5
            )
          of
          (Right originalWords, Right changedWords) ->
            assertBool
              "hash-consed repeated sections retain every free Name identity"
              (originalWords /= changedWords)
          (originalResult, changedResult) ->
            assertFailure
              ( "unexpected variable-rich graph obstruction: "
                  <> show originalResult
                  <> " / "
                  <> show changedResult
              ),
      testCase "the public word constructor rejects unbounded wire values" $
        assertEqual
          "Natural-to-Word64 narrowing is checked"
          (Left (TypeNaturalExceedsWord64 (TypeArgumentCount (fromIntegral (maxBound :: Word64) + 1))))
          (typeWords [TypeArgumentCount (fromIntegral (maxBound :: Word64) + 1)])
    ]

forallRoot :: TypeIndex
forallRoot =
  4

freeRoot :: TypeIndex
freeRoot =
  0

forallIdentityTable :: String -> Array TypeIndex HieTypeFlat
forallIdentityTable nameText =
  let binderName = testName nameText
   in array
        (0, 4)
        [ (0, HCoercionTy),
          (1, HTyVarTy binderName),
          (2, HCoercionTy),
          (3, HFunTy 2 1 1),
          (4, HForAllTy ((binderName, 0), Specified) 3)
        ]

freeVariableTable :: String -> Array TypeIndex HieTypeFlat
freeVariableTable nameText =
  freeNameTable (testName nameText)

freeNameTable :: Name -> Array TypeIndex HieTypeFlat
freeNameTable nameValue =
  array (0, 0) [(0, HTyVarTy nameValue)]

escapedBinderTable :: Name -> Array TypeIndex HieTypeFlat
escapedBinderTable binderName =
  array
    (0, 3)
    [ (0, HCoercionTy),
      (1, HTyVarTy binderName),
      (2, HForAllTy ((binderName, 0), Specified) 1),
      (3, HAppTy 2 (HieArgs [(True, 1)]))
    ]

independentBinderRootsTable :: Name -> Array TypeIndex HieTypeFlat
independentBinderRootsTable binderName =
  array
    (0, 2)
    [ (0, HCoercionTy),
      (1, HTyVarTy binderName),
      (2, HForAllTy ((binderName, 0), Specified) 1)
    ]

compositeEscapedBinderTable :: Name -> Array TypeIndex HieTypeFlat
compositeEscapedBinderTable binderName =
  array
    (0, 4)
    [ (0, HCoercionTy),
      (1, HTyVarTy binderName),
      (2, HCastTy 1),
      (3, HForAllTy ((binderName, 0), Specified) 1),
      (4, HAppTy 3 (HieArgs [(True, 2)]))
    ]

variableRichDoublingTable ::
  Name ->
  Name ->
  Array TypeIndex HieTypeFlat
variableRichDoublingTable firstName secondName =
  array
    (0, 5)
    [ (0, HTyVarTy firstName),
      (1, HTyVarTy secondName),
      (2, HAppTy 0 (HieArgs [(True, 1)])),
      (3, HAppTy 2 (HieArgs [(True, 2)])),
      (4, HAppTy 3 (HieArgs [(True, 3)])),
      (5, HAppTy 4 (HieArgs [(True, 4)]))
    ]

sharedDagTable :: Int -> Array TypeIndex HieTypeFlat
sharedDagTable depth =
  array
    (0, depth)
    ( (0, HCoercionTy)
        : fmap
          (\typeIndex -> (typeIndex, HAppTy (typeIndex - 1) (HieArgs [(True, typeIndex - 1)])))
          [1 .. depth]
    )

diamondTable :: Array TypeIndex HieTypeFlat
diamondTable =
  array
    (0, 3)
    [ (0, HCoercionTy),
      (1, HCastTy 0),
      (2, HAppTy 1 (HieArgs [(True, 0)])),
      (3, HAppTy 2 (HieArgs [(True, 1)]))
    ]

testName :: String -> Name
testName nameText =
  mkSystemName (mkUnique 't' (fromIntegral (sum (fmap fromEnum nameText)))) (mkTyVarOcc nameText)

testNameWithUnique :: String -> Word64 -> Name
testNameWithUnique nameText uniqueValue =
  mkSystemName (mkUnique 'u' uniqueValue) (mkTyVarOcc nameText)
