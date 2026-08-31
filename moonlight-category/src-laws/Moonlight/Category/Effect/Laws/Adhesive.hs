module Moonlight.Category.Effect.Laws.Adhesive
  ( lawSuites,
  )
where

import qualified Moonlight.Category.Effect.Harness as Harness
import Moonlight.Category.Effect.LawNames (LawName (..))
import Moonlight.Category.Effect.Laws.Generators (SampleUnitMorphism (..))
import Moonlight.Category.Pure.Adhesive (PBPOComplementWitness, PushoutComplementWitness)
import Moonlight.Category.Pure.Unit (UnitCat (..), UnitMor)
import Moonlight.Pale.Test.Laws.Suite (LawSuite, lawGroup, namedQuickCheckLaw)

adhesiveWitnessMonicSoundProp :: SampleUnitMorphism -> Bool
adhesiveWitnessMonicSoundProp (SampleUnitMorphism morphism) =
  Harness.adhesiveWitnessMonicSound @UnitCat UnitCat unitMorphismIsMonic morphism

pushoutComplementSquareProp :: SampleUnitMorphism -> Bool
pushoutComplementSquareProp (SampleUnitMorphism morphism) =
  Harness.pushoutComplementSquareCommutes @UnitCat UnitCat morphism morphism

pushoutComplementUniversalProp :: SampleUnitMorphism -> Bool
pushoutComplementUniversalProp (SampleUnitMorphism morphism) =
  Harness.pushoutComplementUniversal @UnitCat UnitCat unitPushoutComplementUniversal morphism morphism

pbpoPullbackSquareProp :: SampleUnitMorphism -> Bool
pbpoPullbackSquareProp (SampleUnitMorphism morphism) =
  Harness.pbpoPullbackSquareCommutes @UnitCat UnitCat morphism morphism

pbpoPushoutSquareProp :: SampleUnitMorphism -> Bool
pbpoPushoutSquareProp (SampleUnitMorphism morphism) =
  Harness.pbpoPushoutSquareCommutes @UnitCat UnitCat morphism morphism

pbpoComplementUniversalProp :: SampleUnitMorphism -> Bool
pbpoComplementUniversalProp (SampleUnitMorphism morphism) =
  Harness.pbpoComplementUniversal @UnitCat UnitCat unitPBPOComplementUniversal morphism morphism

unitMorphismIsMonic :: UnitMor -> Bool
unitMorphismIsMonic _ =
  True

unitPushoutComplementUniversal :: PushoutComplementWitness UnitCat -> Bool
unitPushoutComplementUniversal _ =
  True

unitPBPOComplementUniversal :: PBPOComplementWitness UnitCat -> Bool
unitPBPOComplementUniversal _ =
  True

lawSuites :: [LawSuite]
lawSuites =
  [ lawGroup
      "adhesive"
      [ namedQuickCheckLaw AdhesiveWitnessMonicSound adhesiveWitnessMonicSoundProp,
        namedQuickCheckLaw PushoutComplementSquareCommutes pushoutComplementSquareProp,
        namedQuickCheckLaw PushoutComplementUniversal pushoutComplementUniversalProp,
        namedQuickCheckLaw PBPOPullbackSquareCommutes pbpoPullbackSquareProp,
        namedQuickCheckLaw PBPOPushoutSquareCommutes pbpoPushoutSquareProp,
        namedQuickCheckLaw PBPOComplementUniversal pbpoComplementUniversalProp
      ]
  ]
