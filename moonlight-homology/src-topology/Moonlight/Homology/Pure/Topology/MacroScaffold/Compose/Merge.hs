module Moonlight.Homology.Pure.Topology.MacroScaffold.Compose.Merge
  ( mergeScalarPotentialFields,
    mergeDirectionFields,
  )
where

import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict qualified as Map
import Moonlight.Homology.Pure.Carrier
  ( carrierCells,
    carrierDegree,
    mkCellCarrier,
  )
import Moonlight.Homology.Pure.Topology.MacroScaffold.Compose.Core
  ( MacroScaffoldCompositionError (..),
  )
import Moonlight.Homology.Pure.Topology.MacroScaffold.Direction
  ( DirectionField,
    DirectionFieldEncoding (..),
    directionFieldCarrier,
    directionFieldEncoding,
    directionFieldSymmetryOrder,
    mkDirectionField,
  )
import Moonlight.Homology.Pure.Topology.MacroScaffold.Potential
  ( ScalarPotentialField,
    mkScalarPotentialField,
    scalarPotentialCarrier,
    scalarPotentialNormalization,
    scalarPotentialSamples,
  )

mergeScalarPotentialFields :: NonEmpty ScalarPotentialField -> Either MacroScaffoldCompositionError ScalarPotentialField
mergeScalarPotentialFields fields = do
  normalization <- uniformValue MismatchedScalarPotentialNormalizations (scalarPotentialNormalization <$> fields)
  degreeValue <- uniformValue MismatchedScalarPotentialCarrierDegrees ((carrierDegree . scalarPotentialCarrier) <$> fields)
  carrierValue <-
    first
      InvalidComposedScalarPotentialCarrier
      (mkCellCarrier degreeValue (NonEmpty.toList fields >>= carrierCells . scalarPotentialCarrier))
  first
    InvalidComposedScalarPotential
    (mkScalarPotentialField carrierValue normalization (Map.unions (NonEmpty.toList (scalarPotentialSamples <$> fields))))

mergeDirectionFields :: NonEmpty DirectionField -> Either MacroScaffoldCompositionError DirectionField
mergeDirectionFields fields = do
  symmetryOrder <- uniformValue MismatchedDirectionSymmetryOrders (directionFieldSymmetryOrder <$> fields)
  degreeValue <- uniformValue MismatchedDirectionCarrierDegrees ((carrierDegree . directionFieldCarrier) <$> fields)
  encodingValue <- mergeDirectionEncodings (directionFieldEncoding <$> fields)
  carrierValue <-
    first
      InvalidComposedDirectionCarrier
      (mkCellCarrier degreeValue (NonEmpty.toList fields >>= carrierCells . directionFieldCarrier))
  first
    InvalidComposedDirectionField
    (mkDirectionField carrierValue symmetryOrder encodingValue)

mergeDirectionEncodings :: NonEmpty DirectionFieldEncoding -> Either MacroScaffoldCompositionError DirectionFieldEncoding
mergeDirectionEncodings encodings =
  case traverse angleMap encodings of
    Just angleMaps ->
      Right (DirectionAngleEncoding (Map.unions (NonEmpty.toList angleMaps)))
    Nothing ->
      case traverse cochainMap encodings of
        Just cochainMaps ->
          Right (DirectionCochainEncoding (Map.unions (NonEmpty.toList cochainMaps)))
        Nothing ->
          Left MismatchedDirectionEncodingFamilies
  where
    angleMap encodingValue =
      case encodingValue of
        DirectionAngleEncoding phaseMap -> Just phaseMap
        DirectionCochainEncoding {} -> Nothing

    cochainMap encodingValue =
      case encodingValue of
        DirectionAngleEncoding {} -> Nothing
        DirectionCochainEncoding coefficientMap -> Just coefficientMap

uniformValue :: Eq a => MacroScaffoldCompositionError -> NonEmpty a -> Either MacroScaffoldCompositionError a
uniformValue mismatchError (value :| rest) =
  if all (== value) rest
    then Right value
    else Left mismatchError
