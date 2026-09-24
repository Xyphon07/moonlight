{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Shared inputs for the exact sign law and its forced timing instrument.
-- Construction, rational normalization, and distribution naming stay outside
-- the predicate timings.
module Moonlight.Planar.LiftedSignCases
  ( LiftedSignFamily (..)
  , LiftedSignInput
  , liftedSignCases
  , candidateSign
  , magnitudeSign
  , permutedInputs
  , conventionInputs
  ) where

import Control.DeepSeq (NFData)
import Data.Bits (shiftL)
import Data.List (permutations)
import GHC.Generics (Generic)
import Moonlight.Planar.Internal.ExactRational
  ( ExactArithmeticError
  , exactRational
  , exactRationalFromDyadic
  , exactSignum
  )
import Moonlight.Planar.Internal.PowerDiagram.Generator
  ( ExactPowerGenerator (..)
  , exactGeneratorLiftedSign
  , exactGeneratorLiftedVolume
  )

data LiftedSignFamily
  = IntegralCoordinates
  | DyadicCoordinates
  | OddDenominators
  | DisparateExponents
  | ExactlyCoplanar
  | NearlyCoplanar
  | LargeRationalWeights
  | PowerSiteGenerators
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (NFData)

type LiftedSignInput =
  ( ExactPowerGenerator Int
  , ExactPowerGenerator Int
  , ExactPowerGenerator Int
  , ExactPowerGenerator Int
  )

liftedSignCases
  :: Either ExactArithmeticError [(LiftedSignFamily, [LiftedSignInput])]
liftedSignCases =
  traverse
    (\family -> (,) family <$> traverse (familyInput family) [1 .. 128])
    [minBound .. maxBound]

familyInput
  :: LiftedSignFamily
  -> Int
  -> Either ExactArithmeticError LiftedSignInput
familyInput family seed =
  (,,,)
    <$> familyGenerator family seed 0
    <*> familyGenerator family seed 1
    <*> familyGenerator family seed 2
    <*> familyGenerator family seed 3

familyGenerator
  :: LiftedSignFamily
  -> Int
  -> Int
  -> Either ExactArithmeticError (ExactPowerGenerator Int)
familyGenerator family seed label =
  let value axis = toInteger ((seed * (17 + axis * 11) + label * (31 + axis * 7)) `mod` 127 - 63)
      dyadic axis = exactRationalFromDyadic (value axis) (negate (3 + (seed + axis * 17 + label * 11) `mod` 54))
      oddCoordinate axis = exactRational (value axis) (toInteger (3 + 2 * ((seed + label * 7 + axis * 11) `mod` 61)))
      construct = ExactPowerGenerator label
   in case family of
        IntegralCoordinates ->
          pure (construct (fromInteger (value 0)) (fromInteger (value 1)) (fromInteger (value 2)))
        DyadicCoordinates -> pure (construct (dyadic 0) (dyadic 1) (dyadic 2))
        OddDenominators -> construct <$> oddCoordinate 0 <*> oddCoordinate 1 <*> oddCoordinate 2
        DisparateExponents ->
          let coordinateExponent :: Int -> Int
              coordinateExponent axis = case (label + axis + seed) `mod` 4 of
                0 -> -1074
                1 -> -513
                2 -> 17
                _ -> 1023
           in pure
                ( construct
                    (exactRationalFromDyadic (value 0) (coordinateExponent 0))
                    (exactRationalFromDyadic (value 1) (coordinateExponent 1))
                    (exactRationalFromDyadic (value 2) (coordinateExponent 2))
                )
        ExactlyCoplanar -> do
          coordinateX <- oddCoordinate 0
          coordinateY <- oddCoordinate 1
          pure (construct coordinateX coordinateY (3 * coordinateX - 2 * coordinateY + 7))
        NearlyCoplanar ->
          let coordinateX = fromIntegral (label `mod` 2)
              coordinateY = fromIntegral (label `quot` 2)
              perturbation =
                if label == 3
                  then exactRationalFromDyadic (if even seed then 1 else -1) (negate (seed * 9))
                  else 0
           in pure (construct coordinateX coordinateY (3 * coordinateX - 2 * coordinateY + 7 + perturbation))
        LargeRationalWeights -> do
          weight <-
            exactRational
              (((1 :: Integer) `shiftL` (1024 + seed)) + value 2)
              (((1 :: Integer) `shiftL` (127 + label * 13)) - 1)
          pure (construct (dyadic 0) (dyadic 1) weight)
        PowerSiteGenerators ->
          let positionX = dyadic 0
              positionY = dyadic 1
              weight = dyadic 2
           in pure (construct (2 * positionX) (2 * positionY) (weight - positionX * positionX - positionY * positionY))

candidateSign :: LiftedSignInput -> Ordering
candidateSign (firstGenerator, secondGenerator, thirdGenerator, fourthGenerator) =
  exactGeneratorLiftedSign firstGenerator secondGenerator thirdGenerator fourthGenerator

magnitudeSign :: LiftedSignInput -> Ordering
magnitudeSign (firstGenerator, secondGenerator, thirdGenerator, fourthGenerator) =
  exactSignum (exactGeneratorLiftedVolume firstGenerator secondGenerator thirdGenerator fourthGenerator)

-- | All row permutations, without a partial list-to-tuple conversion.
permutedInputs :: LiftedSignInput -> [LiftedSignInput]
permutedInputs (firstGenerator, secondGenerator, thirdGenerator, fourthGenerator) =
  [ (firstPermuted, secondPermuted, thirdPermuted, fourthPermuted)
  | [firstPermuted, secondPermuted, thirdPermuted, fourthPermuted] <-
      permutations [firstGenerator, secondGenerator, thirdGenerator, fourthGenerator]
  ]

-- | Leading homogeneous 1 gives a positive determinant for the unit
-- tetrahedron, hence an above-plane query conflicts with a CCW slope face.
conventionInputs :: [(Ordering, LiftedSignInput)]
conventionInputs =
  let origin = ExactPowerGenerator 0 0 0 0
      onX = ExactPowerGenerator 1 1 0 0
      onY = ExactPowerGenerator 2 0 1 0
      above = ExactPowerGenerator 3 0 0 1
      below = ExactPowerGenerator 3 0 0 (-1)
   in [ (GT, (origin, onX, onY, above))
      , (LT, (origin, onY, onX, above))
      , (LT, (origin, onX, onY, below))
      , (EQ, (origin, onX, onY, origin))
      ]
