{-# LANGUAGE TypeApplications #-}

module IndexedSpec
  ( tests,
  )
where

import qualified Moonlight.Category.Indexed as Indexed
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

tests :: TestTree
tests =
  testGroup
    "indexed-category"
    [ testCase "identity functor maps typed arrows" $ do
        let mapped = Indexed.Id @(->) Indexed.% ((+ 1) :: Int -> Int)
        mapped 1 @?= 2,
      testCase "identity natural transformation exposes typed components" $ do
        let naturalIdentity = Indexed.natId (Indexed.Id @(->))
            component = naturalIdentity Indexed.! (id :: Int -> Int)
        component 7 @?= 7,
      testCase "identity adjunction derives a typed unit" $ do
        let unit = Indexed.adjunctionUnit (Indexed.idAdj @(->))
            component = unit Indexed.! (id :: Int -> Int)
        component 11 @?= 11
    ]
