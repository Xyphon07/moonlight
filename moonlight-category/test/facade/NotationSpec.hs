{-# LANGUAGE TypeFamilies #-}

module NotationSpec
  ( tests,
  )
where

import Moonlight.Category
  ( Category (..),
    FinCat,
    FinObjectId (..),
    finMorId,
    mkFinObject,
  )
import Moonlight.Category.Notation
  ( cod,
    codObj,
    composeIn,
    dom,
    domObj,
    hom,
    idOf,
    reachableIn,
  )
import Moonlight.Category.Presentation
  ( FinCatBuildError,
    after,
    arrow,
    below,
    equate,
    finCategory,
    object,
    objects,
  )
import Moonlight.Pale.Test.Assertions (expectRightWithLabel, expectSome, withResult)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Notation"
    [ testCase "dom and cod agree with the category's source and target" testDomCodAgreeWithCategory,
      testCase "composeIn realises the categorical composite" testComposeInRealisesComposite,
      testCase "reachableIn reads a preorder, identities included" testReachableInPreorder,
      testCase "idOf is the identity morphism the category provides" testIdOfIsIdentity
    ]

triangle :: Either FinCatBuildError FinCat
triangle =
  finCategory $ do
    a <- object "A"
    b <- object "B"
    c <- object "C"
    f <- arrow a b "f"
    g <- arrow b c "g"
    h <- arrow a c "h"
    equate (g `after` f) h

chain :: Either FinCatBuildError FinCat
chain =
  finCategory $ do
    [x, y, z] <- objects ["x", "y", "z"]
    below x y
    below y z

testDomCodAgreeWithCategory :: Assertion
testDomCodAgreeWithCategory =
  withResult triangle $ \category -> do
    f <- expectSome "the morphism 0 -> 1" (hom category (FinObjectId 0) (FinObjectId 1))
    dom f @?= FinObjectId 0
    cod f @?= FinObjectId 1
    source category f @?= Right (domObj f)
    target category f @?= Right (codObj f)

testComposeInRealisesComposite :: Assertion
testComposeInRealisesComposite =
  withResult triangle $ \category -> do
    f <- expectSome "the morphism 0 -> 1" (hom category (FinObjectId 0) (FinObjectId 1))
    g <- expectSome "the morphism 1 -> 2" (hom category (FinObjectId 1) (FinObjectId 2))
    h <- expectSome "the morphism 0 -> 2" (hom category (FinObjectId 0) (FinObjectId 2))
    fmap finMorId (composeIn category g f) @?= Right (finMorId h)

testReachableInPreorder :: Assertion
testReachableInPreorder =
  withResult chain $ \category -> do
    assertBool "0 reaches 2 transitively" (reachableIn category (FinObjectId 0) (FinObjectId 2))
    assertBool "2 does not reach 0" (not (reachableIn category (FinObjectId 2) (FinObjectId 0)))
    assertBool "0 reaches 0 via the identity" (reachableIn category (FinObjectId 0) (FinObjectId 0))

testIdOfIsIdentity :: Assertion
testIdOfIsIdentity =
  withResult triangle $ \category -> do
    object0 <- expectRightWithLabel "the object 0" (mkFinObject category (FinObjectId 0))
    let identityMorphism = idOf object0
    dom identityMorphism @?= FinObjectId 0
    cod identityMorphism @?= FinObjectId 0
    Right identityMorphism @?= identity category object0
