module Moonlight.Homology.Pure.Carrier
  ( BasisCellRef (..),
    CellCarrier,
    CellCarrierError (..),
    carrierDegree,
    carrierCells,
    mkCellCarrier,
  )
where

import Data.Kind (Type)
import Data.Function ((&))
import Data.List qualified as List
import Data.Set qualified as Set
import Moonlight.Homology.Pure.Degree (HomologicalDegree (..))

type BasisCellRef :: Type
data BasisCellRef = BasisCellRef
  { cellDegree :: HomologicalDegree,
    cellIndex :: Int
  }
  deriving stock (Eq, Ord, Show)

type CellCarrier :: Type
data CellCarrier = CellCarrier
  { carrierDegree :: HomologicalDegree,
    carrierCells :: [BasisCellRef]
  }
  deriving stock (Eq, Show)

type CellCarrierError :: Type
data CellCarrierError
  = CellCarrierDegreeMismatch HomologicalDegree BasisCellRef
  | CellCarrierCellsNotDistinct [BasisCellRef]
  deriving stock (Eq, Show)

type DuplicateCellsAccumulator :: Type
data DuplicateCellsAccumulator = DuplicateCellsAccumulator
  { seenCellRefs :: !(Set.Set BasisCellRef),
    duplicateCellRefs :: !(Set.Set BasisCellRef)
  }

mkCellCarrier :: HomologicalDegree -> [BasisCellRef] -> Either CellCarrierError CellCarrier
mkCellCarrier degreeValue cells =
  case List.find ((/= degreeValue) . cellDegree) cells of
    Just invalidCell ->
      Left (CellCarrierDegreeMismatch degreeValue invalidCell)
    Nothing ->
      case duplicateCells cells of
        [] ->
          Right
            CellCarrier
              { carrierDegree = degreeValue,
                carrierCells = cells
              }
        duplicateCellValues ->
          Left (CellCarrierCellsNotDistinct duplicateCellValues)

duplicateCells :: [BasisCellRef] -> [BasisCellRef]
duplicateCells cells =
  cells
    & List.foldl' accumulateDuplicateCell emptyDuplicateCellsAccumulator
    & duplicateCellRefs
    & Set.toAscList

emptyDuplicateCellsAccumulator :: DuplicateCellsAccumulator
emptyDuplicateCellsAccumulator =
  DuplicateCellsAccumulator
    { seenCellRefs = Set.empty,
      duplicateCellRefs = Set.empty
    }

accumulateDuplicateCell :: DuplicateCellsAccumulator -> BasisCellRef -> DuplicateCellsAccumulator
accumulateDuplicateCell accumulator cellRefValue =
  if Set.member cellRefValue (seenCellRefs accumulator)
    then
      accumulator
        { duplicateCellRefs = Set.insert cellRefValue (duplicateCellRefs accumulator)
        }
    else
      accumulator
        { seenCellRefs = Set.insert cellRefValue (seenCellRefs accumulator)
        }
