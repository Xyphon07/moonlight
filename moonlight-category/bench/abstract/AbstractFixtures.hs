{-# LANGUAGE TypeFamilies #-}

module AbstractFixtures
  ( BenchCategory (..),
    BenchObject (..),
    BenchMorphism (..),
    benchMorphism,
    benchObjectWeight,
    benchMorphismWeight,
    benchRuleLeg,
    benchMonicMatch,
    benchLeftCospanLeg,
    benchLeftCospanRightLeg,
    benchRightCospanLeftLeg,
    benchRightCospanRightLeg,
  )
where

import Moonlight.Category.Pure.Adhesive
  ( AdhesiveCategory (..),
    MonicMatchComponents (..),
    PBPOAdhesiveCategory,
    PushoutComplementComponents (..),
  )
import Moonlight.Category.Pure.Category (Category (..))
import Moonlight.Category.Pure.Limits (HasPullbacks (..), HasPushouts (..))

data BenchCategory = BenchCategory

data BenchObject
  = ObjectK
  | ObjectL
  | ObjectD
  | ObjectG
  | ObjectP
  | ObjectQ
  deriving stock (Eq, Ord, Show)

data BenchMorphism = BenchMorphism
  { benchMorphismSource :: !BenchObject,
    benchMorphismTarget :: !BenchObject
  }
  deriving stock (Eq, Ord, Show)

benchMorphism :: BenchObject -> BenchObject -> BenchMorphism
benchMorphism = BenchMorphism

benchRuleLeg :: BenchMorphism
benchRuleLeg = benchMorphism ObjectK ObjectL

benchMonicMatch :: BenchMorphism
benchMonicMatch = benchMorphism ObjectL ObjectG

benchLeftCospanLeg :: BenchMorphism
benchLeftCospanLeg = benchMorphism ObjectK ObjectD

benchLeftCospanRightLeg :: BenchMorphism
benchLeftCospanRightLeg = benchMorphism ObjectL ObjectD

benchRightCospanLeftLeg :: BenchMorphism
benchRightCospanLeftLeg = benchMorphism ObjectL ObjectG

benchRightCospanRightLeg :: BenchMorphism
benchRightCospanRightLeg = benchMorphism ObjectQ ObjectG

benchObjectWeight :: BenchObject -> Int
benchObjectWeight objectValue =
  case objectValue of
    ObjectK -> 1
    ObjectL -> 2
    ObjectD -> 3
    ObjectG -> 4
    ObjectP -> 5
    ObjectQ -> 6

benchMorphismWeight :: BenchMorphism -> Int
benchMorphismWeight morphism =
  benchObjectWeight (benchMorphismSource morphism)
    + benchObjectWeight (benchMorphismTarget morphism)

instance Category BenchCategory where
  type Ob BenchCategory = BenchObject
  type Mor BenchCategory = BenchMorphism

  identity _ objectValue =
    Right (benchMorphism objectValue objectValue)

  compose _ leftMorphism rightMorphism
    | benchMorphismTarget rightMorphism == benchMorphismSource leftMorphism =
        Right (benchMorphism (benchMorphismSource rightMorphism) (benchMorphismTarget leftMorphism), ())
    | otherwise =
        Left ()

  source _ =
    Right . benchMorphismSource

  target _ =
    Right . benchMorphismTarget

instance HasPullbacks BenchCategory where
  pullback _ leftMorphism rightMorphism
    | benchMorphismTarget leftMorphism == benchMorphismTarget rightMorphism =
        Just
          ( ObjectP,
            benchMorphism ObjectP (benchMorphismSource leftMorphism),
            benchMorphism ObjectP (benchMorphismSource rightMorphism)
          )
    | otherwise =
        Nothing

  pullbackMediator _ leftMorphism rightMorphism coneLeft coneRight
    | benchMorphismTarget leftMorphism == benchMorphismTarget rightMorphism
        && benchMorphismTarget coneLeft == benchMorphismSource leftMorphism
        && benchMorphismTarget coneRight == benchMorphismSource rightMorphism
        && benchMorphismSource coneLeft == benchMorphismSource coneRight =
        Just (benchMorphism (benchMorphismSource coneLeft) ObjectP)
    | otherwise =
        Nothing

instance HasPushouts BenchCategory where
  pushout _ leftMorphism rightMorphism
    | benchMorphismSource leftMorphism == benchMorphismSource rightMorphism =
        Just
          ( ObjectQ,
            benchMorphism (benchMorphismTarget leftMorphism) ObjectQ,
            benchMorphism (benchMorphismTarget rightMorphism) ObjectQ
          )
    | otherwise =
        Nothing

instance AdhesiveCategory BenchCategory where
  monicMatchComponents _ morphism =
    Just (MonicMatchComponents morphism)

  pushoutComplementComponents _ _ _ =
    Just
      PushoutComplementComponents
        { pushoutComplementComponentObject = ObjectD,
          pushoutComplementComponentBorrowedLeg = benchMorphism ObjectD ObjectG,
          pushoutComplementComponentResidualLeg = benchMorphism ObjectK ObjectD
        }

instance PBPOAdhesiveCategory BenchCategory
