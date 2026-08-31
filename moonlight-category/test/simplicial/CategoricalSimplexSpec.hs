{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module CategoricalSimplexSpec
  ( tests,
  )
where

import Data.Kind (Type)
import GHC.TypeNats (KnownNat)
import qualified Moonlight.Category.Indexed as Indexed
import Moonlight.Category.Indexed (Category ((.)))
import Moonlight.Category.Simplicial
  ( Coface (..),
    Codegeneracy (..),
    DeltaMorphism,
    Dimension (..),
    categoricalSimplexToDeltaMorphism,
    cofaceMorphism,
    codegeneracyMorphism,
    composeDeltaMorphism,
    mkFinOffset,
  )
import Moonlight.Category.Test.IndexedSimplexFixture
  ( codegeneracy0At0,
    codegeneracy0At1,
    codegeneracy0At2,
    codegeneracy1At1,
    codegeneracy1At2,
    codegeneracy2At2,
    coface0At0,
    coface0At1,
    coface0At2,
    coface1At0,
    coface1At1,
    coface1At2,
    coface2At1,
    coface2At2,
    coface3At2,
  )
import Numeric.Natural (Natural)
import Prelude hiding ((.))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

operationalCoface :: forall n. KnownNat n => Dimension n -> Natural -> Maybe DeltaMorphism
operationalCoface dimension indexValue =
  cofaceMorphism . CofaceMap <$> mkFinOffset @n @2 dimension indexValue

operationalCodegeneracy :: forall n. KnownNat n => Dimension n -> Natural -> Maybe DeltaMorphism
operationalCodegeneracy dimension indexValue =
  codegeneracyMorphism . CodegeneracyMap <$> mkFinOffset @n @1 dimension indexValue

assertLowering :: String -> Maybe DeltaMorphism -> DeltaMorphism -> IO ()
assertLowering label expected actual =
  case expected of
    Nothing -> assertFailure ("expected operational generator for " <> label)
    Just expectedMorphism -> actual @?= expectedMorphism

assertLoweredComposition ::
  Indexed.Simplex (b :: Type) (c :: Type) ->
  Indexed.Simplex (a :: Type) b ->
  IO ()
assertLoweredComposition outer inner =
  composeDeltaMorphism
    (categoricalSimplexToDeltaMorphism outer)
    (categoricalSimplexToDeltaMorphism inner)
    @?= Just (categoricalSimplexToDeltaMorphism (outer . inner))

tests :: TestTree
tests =
  testGroup
    "CategoricalSimplex"
    [ testCase "categorical cofaces lower to operational cofaces for dimensions 0, 1, and 2" $ do
        assertLowering "δ0[0]" (operationalCoface (Dimension @0) 0) (categoricalSimplexToDeltaMorphism coface0At0)
        assertLowering "δ1[0]" (operationalCoface (Dimension @0) 1) (categoricalSimplexToDeltaMorphism coface1At0)
        assertLowering "δ0[1]" (operationalCoface (Dimension @1) 0) (categoricalSimplexToDeltaMorphism coface0At1)
        assertLowering "δ1[1]" (operationalCoface (Dimension @1) 1) (categoricalSimplexToDeltaMorphism coface1At1)
        assertLowering "δ2[1]" (operationalCoface (Dimension @1) 2) (categoricalSimplexToDeltaMorphism coface2At1)
        assertLowering "δ0[2]" (operationalCoface (Dimension @2) 0) (categoricalSimplexToDeltaMorphism coface0At2)
        assertLowering "δ1[2]" (operationalCoface (Dimension @2) 1) (categoricalSimplexToDeltaMorphism coface1At2)
        assertLowering "δ2[2]" (operationalCoface (Dimension @2) 2) (categoricalSimplexToDeltaMorphism coface2At2)
        assertLowering "δ3[2]" (operationalCoface (Dimension @2) 3) (categoricalSimplexToDeltaMorphism coface3At2),
      testCase "categorical codegeneracies lower to operational codegeneracies for dimensions 0, 1, and 2" $ do
        assertLowering "σ0[0]" (operationalCodegeneracy (Dimension @0) 0) (categoricalSimplexToDeltaMorphism codegeneracy0At0)
        assertLowering "σ0[1]" (operationalCodegeneracy (Dimension @1) 0) (categoricalSimplexToDeltaMorphism codegeneracy0At1)
        assertLowering "σ1[1]" (operationalCodegeneracy (Dimension @1) 1) (categoricalSimplexToDeltaMorphism codegeneracy1At1)
        assertLowering "σ0[2]" (operationalCodegeneracy (Dimension @2) 0) (categoricalSimplexToDeltaMorphism codegeneracy0At2)
        assertLowering "σ1[2]" (operationalCodegeneracy (Dimension @2) 1) (categoricalSimplexToDeltaMorphism codegeneracy1At2)
        assertLowering "σ2[2]" (operationalCodegeneracy (Dimension @2) 2) (categoricalSimplexToDeltaMorphism codegeneracy2At2),
      testCase "lowering preserves representative categorical composition" $ do
        assertLoweredComposition coface1At1 coface0At0
        assertLoweredComposition codegeneracy0At1 coface2At1
        assertLoweredComposition codegeneracy0At0 codegeneracy1At1
    ]
