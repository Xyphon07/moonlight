module Moonlight.Homology.Pure.Topology.MacroScaffold.Direction
  ( DirectionSymmetryOrder,
    DirectionSymmetryOrderError (..),
    unDirectionSymmetryOrder,
    mkDirectionSymmetryOrder,
    DirectionPhase,
    DirectionPhaseError (..),
    unDirectionPhase,
    mkDirectionPhase,
    DirectionCoefficient,
    DirectionCoefficientError (..),
    unDirectionCoefficient,
    mkDirectionCoefficient,
    DirectionFieldEncoding (..),
    DirectionField,
    DirectionFieldError (..),
    directionFieldCarrier,
    directionFieldSymmetryOrder,
    directionFieldEncoding,
    mkDirectionField,
    mkDirectionAngleField,
    mkDirectionCochainField,
  )
where

import Data.Fixed (mod')
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Core
  ( mkFiniteWith,
    mkPositiveIntWith,
  )
import Moonlight.Homology.Pure.Carrier
  ( BasisCellRef,
    CellCarrier,
    carrierCells,
  )

type DirectionSymmetryOrder :: Type
newtype DirectionSymmetryOrder = DirectionSymmetryOrder
  { unDirectionSymmetryOrder :: Int
  }
  deriving stock (Eq, Ord, Show)

type DirectionSymmetryOrderError :: Type
data DirectionSymmetryOrderError
  = NonPositiveDirectionSymmetryOrder Int
  deriving stock (Eq, Show)

mkDirectionSymmetryOrder :: Int -> Either DirectionSymmetryOrderError DirectionSymmetryOrder
mkDirectionSymmetryOrder =
  mkPositiveIntWith NonPositiveDirectionSymmetryOrder DirectionSymmetryOrder

type DirectionPhase :: Type
newtype DirectionPhase = DirectionPhase
  { unDirectionPhase :: Double
  }
  deriving stock (Eq, Ord, Show)

type DirectionPhaseError :: Type
data DirectionPhaseError
  = NonFiniteDirectionPhase Double
  deriving stock (Eq, Show)

mkDirectionPhase :: Double -> Either DirectionPhaseError DirectionPhase
mkDirectionPhase =
  mkFiniteWith NonFiniteDirectionPhase DirectionPhase

type DirectionCoefficient :: Type
newtype DirectionCoefficient = DirectionCoefficient
  { unDirectionCoefficient :: Double
  }
  deriving stock (Eq, Ord, Show)

type DirectionCoefficientError :: Type
data DirectionCoefficientError
  = NonFiniteDirectionCoefficient Double
  deriving stock (Eq, Show)

mkDirectionCoefficient :: Double -> Either DirectionCoefficientError DirectionCoefficient
mkDirectionCoefficient =
  mkFiniteWith NonFiniteDirectionCoefficient DirectionCoefficient

type DirectionFieldEncoding :: Type
data DirectionFieldEncoding
  = DirectionAngleEncoding (Map.Map BasisCellRef DirectionPhase)
  | DirectionCochainEncoding (Map.Map BasisCellRef DirectionCoefficient)
  deriving stock (Eq, Show)

type DirectionField :: Type
data DirectionField = DirectionField
  { directionFieldCarrier :: CellCarrier,
    directionFieldSymmetryOrder :: DirectionSymmetryOrder,
    directionFieldEncoding :: DirectionFieldEncoding
  }
  deriving stock (Eq, Show)

type DirectionFieldError :: Type
data DirectionFieldError = DirectionFieldCoverageMismatch
  { directionFieldMissingCells :: [BasisCellRef],
    directionFieldExtraneousCells :: [BasisCellRef]
  }
  deriving stock (Eq, Show)

mkDirectionField ::
  CellCarrier ->
  DirectionSymmetryOrder ->
  DirectionFieldEncoding ->
  Either DirectionFieldError DirectionField
mkDirectionField carrierValue symmetryOrderValue encodingValue =
  let normalizedEncoding = normalizeDirectionFieldEncoding symmetryOrderValue encodingValue
      carrierDomain = Set.fromList (carrierCells carrierValue)
      encodingDomain = directionFieldEncodingDomain normalizedEncoding
      missingCells = Set.toAscList (carrierDomain `Set.difference` encodingDomain)
      extraneousCells = Set.toAscList (encodingDomain `Set.difference` carrierDomain)
   in case (missingCells, extraneousCells) of
        ([], []) ->
          Right
            DirectionField
              { directionFieldCarrier = carrierValue,
                directionFieldSymmetryOrder = symmetryOrderValue,
                directionFieldEncoding = normalizedEncoding
              }
        _ ->
          Left
            DirectionFieldCoverageMismatch
              { directionFieldMissingCells = missingCells,
                directionFieldExtraneousCells = extraneousCells
              }

mkDirectionAngleField ::
  CellCarrier ->
  DirectionSymmetryOrder ->
  Map.Map BasisCellRef DirectionPhase ->
  Either DirectionFieldError DirectionField
mkDirectionAngleField carrierValue symmetryOrderValue phaseMap =
  mkDirectionField carrierValue symmetryOrderValue (DirectionAngleEncoding phaseMap)

mkDirectionCochainField ::
  CellCarrier ->
  DirectionSymmetryOrder ->
  Map.Map BasisCellRef DirectionCoefficient ->
  Either DirectionFieldError DirectionField
mkDirectionCochainField carrierValue symmetryOrderValue coefficientMap =
  mkDirectionField carrierValue symmetryOrderValue (DirectionCochainEncoding coefficientMap)

directionFieldEncodingDomain :: DirectionFieldEncoding -> Set.Set BasisCellRef
directionFieldEncodingDomain encodingValue =
  case encodingValue of
    DirectionAngleEncoding phaseMap ->
      Map.keysSet phaseMap
    DirectionCochainEncoding coefficientMap ->
      Map.keysSet coefficientMap

normalizeDirectionFieldEncoding :: DirectionSymmetryOrder -> DirectionFieldEncoding -> DirectionFieldEncoding
normalizeDirectionFieldEncoding symmetryOrderValue encodingValue =
  case encodingValue of
    DirectionAngleEncoding phaseMap ->
      DirectionAngleEncoding (Map.map (normalizeDirectionPhase symmetryOrderValue) phaseMap)
    DirectionCochainEncoding _ ->
      encodingValue

normalizeDirectionPhase :: DirectionSymmetryOrder -> DirectionPhase -> DirectionPhase
normalizeDirectionPhase symmetryOrderValue phaseValue =
  let period = fundamentalDirectionPeriod symmetryOrderValue
      normalizedPhase = unDirectionPhase phaseValue `mod'` period
   in DirectionPhase normalizedPhase

fundamentalDirectionPeriod :: DirectionSymmetryOrder -> Double
fundamentalDirectionPeriod symmetryOrderValue =
  (2 * pi) / fromIntegral (unDirectionSymmetryOrder symmetryOrderValue)
