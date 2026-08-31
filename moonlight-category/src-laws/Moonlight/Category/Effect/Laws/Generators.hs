
{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Category.Effect.Laws.Generators
  ( SampleFinObject (..),
    SampleFinMorphism (..),
    SampleComposableFinTriple (..),
    SampleOrdinalLower (..),
    SampleOrdinalUpper (..),
    SampleLowerObject (..),
    SampleUpperObject (..),
    SampleLowerMorphism (..),
    SampleUpperMorphism (..),
    SampleUnitObject (..),
    SampleUnitMorphism (..),
    SampleUnitTwoMorphism (..),
    allPairs,
    lawSuites,
  )
where

import Data.Kind (Type)
import Data.Function ((&))
import Data.Maybe (mapMaybe)
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Moonlight.Category.Effect.Fixture.FinCat (sampleFinCat)
import Moonlight.Category.Pure.FinCat
  ( FinMor,
    FinObj,
    allMorphisms,
    allObjects,
  )
import Moonlight.Category.Pure.Category (Category (..))
import Moonlight.Category.Pure.Poset
  ( LowerMor,
    OrdinalLower (..),
    OrdinalUpper (..),
    PosetOb (..),
    UpperMor,
    mkLowerMor,
    mkUpperMor,
  )
import Moonlight.Category.Pure.Unit
  ( UnitMor (..),
    UnitObj (..),
    UnitTwoMor (..),
  )
import Moonlight.Pale.Test.Laws.Suite (LawSuite, hedgehogLaw, lawGroup)
import qualified Test.Tasty.QuickCheck as QC

type SampleFinObject :: Type
newtype SampleFinObject = SampleFinObject {unSampleFinObject :: FinObj}
  deriving stock (Show)

type SampleFinMorphism :: Type
newtype SampleFinMorphism = SampleFinMorphism {unSampleFinMorphism :: FinMor}
  deriving stock (Show)

type SampleComposableFinTriple :: Type
data SampleComposableFinTriple = SampleComposableFinTriple FinMor FinMor FinMor
  deriving stock (Show)

type SampleOrdinalLower :: Type
newtype SampleOrdinalLower = SampleOrdinalLower {unSampleOrdinalLower :: OrdinalLower}
  deriving stock (Show)

type SampleOrdinalUpper :: Type
newtype SampleOrdinalUpper = SampleOrdinalUpper {unSampleOrdinalUpper :: OrdinalUpper}
  deriving stock (Show)

type SampleLowerObject :: Type
newtype SampleLowerObject = SampleLowerObject {unSampleLowerObject :: PosetOb OrdinalLower}
  deriving stock (Show)

type SampleUpperObject :: Type
newtype SampleUpperObject = SampleUpperObject {unSampleUpperObject :: PosetOb OrdinalUpper}
  deriving stock (Show)

type SampleLowerMorphism :: Type
newtype SampleLowerMorphism = SampleLowerMorphism {unSampleLowerMorphism :: LowerMor}
  deriving stock (Show)

type SampleUpperMorphism :: Type
newtype SampleUpperMorphism = SampleUpperMorphism {unSampleUpperMorphism :: UpperMor}
  deriving stock (Show)

type SampleUnitObject :: Type
newtype SampleUnitObject = SampleUnitObject {unSampleUnitObject :: UnitObj}
  deriving stock (Show)

type SampleUnitMorphism :: Type
newtype SampleUnitMorphism = SampleUnitMorphism {unSampleUnitMorphism :: UnitMor}
  deriving stock (Show)

type SampleUnitTwoMorphism :: Type
newtype SampleUnitTwoMorphism = SampleUnitTwoMorphism {unSampleUnitTwoMorphism :: UnitTwoMor}
  deriving stock (Show)

sampleObjects :: [FinObj]
sampleObjects = allObjects sampleFinCat

sampleMorphisms :: [FinMor]
sampleMorphisms = allMorphisms sampleFinCat

sampleComposableFinTriples :: [SampleComposableFinTriple]
sampleComposableFinTriples =
  sampleMorphisms
    >>= ( \firstMorphism ->
            sampleMorphisms
              >>= ( \secondMorphism ->
                      sampleMorphisms
                        & foldMap
                          ( \thirdMorphism ->
                              case
                                ( target sampleFinCat firstMorphism,
                                  source sampleFinCat secondMorphism,
                                  target sampleFinCat secondMorphism,
                                  source sampleFinCat thirdMorphism
                                )
                                of
                                  (Right firstTarget, Right secondSource, Right secondTarget, Right thirdSource)
                                    | firstTarget == secondSource && secondTarget == thirdSource ->
                                        [SampleComposableFinTriple firstMorphism secondMorphism thirdMorphism]
                                  _ -> []
                          )
                  )
        )

lowerMorphismSamples :: [LowerMor]
lowerMorphismSamples =
  [0 .. 32]
    >>= ( \lower ->
            mapMaybe (mkLowerMor (OrdinalLower lower) . OrdinalLower) [lower .. 32]
        )

upperMorphismSamples :: [UpperMor]
upperMorphismSamples =
  [0 .. 64]
    >>= ( \lower ->
            mapMaybe (mkUpperMor (OrdinalUpper lower) . OrdinalUpper) [lower .. 64]
        )

instance QC.Arbitrary SampleFinObject where
  arbitrary = SampleFinObject <$> QC.elements sampleObjects
  shrink _ = []

instance QC.Arbitrary SampleFinMorphism where
  arbitrary = SampleFinMorphism <$> QC.elements sampleMorphisms
  shrink _ = []

instance QC.Arbitrary SampleComposableFinTriple where
  arbitrary = QC.elements sampleComposableFinTriples
  shrink _ = []

instance QC.Arbitrary SampleOrdinalLower where
  arbitrary = SampleOrdinalLower . OrdinalLower <$> QC.chooseInt (0, 32)
  shrink (SampleOrdinalLower (OrdinalLower value)) =
    map (SampleOrdinalLower . OrdinalLower) (QC.shrink value)

instance QC.Arbitrary SampleOrdinalUpper where
  arbitrary = SampleOrdinalUpper . OrdinalUpper <$> QC.chooseInt (0, 64)
  shrink (SampleOrdinalUpper (OrdinalUpper value)) =
    map (SampleOrdinalUpper . OrdinalUpper) (QC.shrink value)

instance QC.Arbitrary SampleLowerObject where
  arbitrary = SampleLowerObject . PosetOb . OrdinalLower <$> QC.chooseInt (0, 32)
  shrink (SampleLowerObject (PosetOb (OrdinalLower value))) =
    map (SampleLowerObject . PosetOb . OrdinalLower) (QC.shrink value)

instance QC.Arbitrary SampleUpperObject where
  arbitrary = SampleUpperObject . PosetOb . OrdinalUpper <$> QC.chooseInt (0, 64)
  shrink (SampleUpperObject (PosetOb (OrdinalUpper value))) =
    map (SampleUpperObject . PosetOb . OrdinalUpper) (QC.shrink value)

instance QC.Arbitrary SampleLowerMorphism where
  arbitrary = SampleLowerMorphism <$> QC.elements lowerMorphismSamples
  shrink _ = []

instance QC.Arbitrary SampleUpperMorphism where
  arbitrary = SampleUpperMorphism <$> QC.elements upperMorphismSamples
  shrink _ = []

instance QC.Arbitrary SampleUnitObject where
  arbitrary = pure (SampleUnitObject UnitObj)
  shrink _ = []

instance QC.Arbitrary SampleUnitMorphism where
  arbitrary = pure (SampleUnitMorphism UnitMor)
  shrink _ = []

instance QC.Arbitrary SampleUnitTwoMorphism where
  arbitrary = pure (SampleUnitTwoMorphism (UnitTwoMor UnitMor UnitMor))
  shrink _ = []

hedgehogSampleFinObject :: HH.Gen FinObj
hedgehogSampleFinObject = Gen.element sampleObjects

hedgehogSampleFinMorphism :: HH.Gen FinMor
hedgehogSampleFinMorphism = Gen.element sampleMorphisms

finObjectGeneratorSound :: FinObj -> Bool
finObjectGeneratorSound objectValue = objectValue `elem` sampleObjects

finMorphismGeneratorSound :: FinMor -> Bool
finMorphismGeneratorSound morphism = morphism `elem` sampleMorphisms

allPairs :: [a] -> [(a, a)]
allPairs values =
  values >>= (\leftValue -> fmap (\rightValue -> (leftValue, rightValue)) values)

lawSuites :: [LawSuite]
lawSuites =
  [ lawGroup
      "generators"
      [ hedgehogLaw "generator_fin_object_sound" hedgehogSampleFinObject finObjectGeneratorSound,
        hedgehogLaw "generator_fin_morphism_sound" hedgehogSampleFinMorphism finMorphismGeneratorSound,
        hedgehogLaw
          "generator_ordinal_lower_bounds"
          (OrdinalLower <$> Gen.int (Range.linear 0 32))
          (\(OrdinalLower value) -> value >= 0 && value <= 32),
        hedgehogLaw
          "generator_ordinal_upper_bounds"
          (OrdinalUpper <$> Gen.int (Range.linear 0 64))
          (\(OrdinalUpper value) -> value >= 0 && value <= 64)
      ]
  ]
