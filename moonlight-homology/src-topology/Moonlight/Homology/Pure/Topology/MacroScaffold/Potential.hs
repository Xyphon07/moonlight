module Moonlight.Homology.Pure.Topology.MacroScaffold.Potential
  ( PotentialValue,
    PotentialValueError (..),
    unPotentialValue,
    mkPotentialValue,
    PotentialNormalization (..),
    ScalarPotentialField,
    ScalarPotentialFieldError (..),
    scalarPotentialCarrier,
    scalarPotentialNormalization,
    scalarPotentialSamples,
    mkScalarPotentialField,
    mkScalarPotentialFieldFromSamples,
  )
where

import Data.Bifunctor (first)
import Data.Function ((&))
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Core
  ( collectEither,
    mkFiniteWith,
  )
import Moonlight.Homology.Pure.Carrier
  ( BasisCellRef,
    CellCarrier,
    carrierCells,
  )

type PotentialValue :: Type
newtype PotentialValue = PotentialValue
  { unPotentialValue :: Double
  }
  deriving stock (Eq, Ord, Show)

type PotentialValueError :: Type
data PotentialValueError
  = NonFinitePotentialValue Double
  deriving stock (Eq, Show)

mkPotentialValue :: Double -> Either PotentialValueError PotentialValue
mkPotentialValue =
  mkFiniteWith NonFinitePotentialValue PotentialValue

type PotentialNormalization :: Type
data PotentialNormalization
  = NativePotentialScale
  | UnitIntervalPotentialScale
  deriving stock (Eq, Ord, Show)

type ScalarPotentialField :: Type
data ScalarPotentialField = ScalarPotentialField
  { scalarPotentialCarrier :: CellCarrier,
    scalarPotentialNormalization :: PotentialNormalization,
    scalarPotentialSamples :: Map.Map BasisCellRef PotentialValue
  }
  deriving stock (Eq, Show)

type ScalarPotentialFieldError :: Type
data ScalarPotentialFieldError
  = ScalarPotentialFieldCoverageMismatch [BasisCellRef] [BasisCellRef]
  | ScalarPotentialFieldInvalidSamples [(BasisCellRef, PotentialValueError)]
  deriving stock (Eq, Show)

mkScalarPotentialField ::
  CellCarrier ->
  PotentialNormalization ->
  Map.Map BasisCellRef PotentialValue ->
  Either ScalarPotentialFieldError ScalarPotentialField
mkScalarPotentialField carrierValue normalization sampleValues =
  let carrierDomain = Set.fromList (carrierCells carrierValue)
      sampleDomain = Map.keysSet sampleValues
      missingCells = Set.toAscList (carrierDomain `Set.difference` sampleDomain)
      extraneousCells = Set.toAscList (sampleDomain `Set.difference` carrierDomain)
   in case (missingCells, extraneousCells) of
        ([], []) ->
          Right
            ScalarPotentialField
              { scalarPotentialCarrier = carrierValue,
                scalarPotentialNormalization = normalization,
                scalarPotentialSamples = sampleValues
              }
        _ ->
          Left
            (ScalarPotentialFieldCoverageMismatch missingCells extraneousCells)

mkScalarPotentialFieldFromSamples ::
  CellCarrier ->
  PotentialNormalization ->
  Map.Map BasisCellRef Double ->
  Either ScalarPotentialFieldError ScalarPotentialField
mkScalarPotentialFieldFromSamples carrierValue normalization rawSampleValues = do
  validatedSamples <-
    first
      ScalarPotentialFieldInvalidSamples
      ( collectEither
          ( rawSampleValues
              & Map.toAscList
              & fmap
                ( \(cellRefValue, rawSampleValue) ->
                    first
                      (\potentialError -> [(cellRefValue, potentialError)])
                      (mkPotentialValue rawSampleValue)
                      & fmap ((,) cellRefValue)
                )
          )
      )
  mkScalarPotentialField carrierValue normalization (Map.fromList validatedSamples)
