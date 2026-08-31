{-# LANGUAGE StrictData #-}

module Moonlight.LinAlg.Pure.Sparse.Assembly
  ( canonicalCSRFromEntries,
    orderedCSRFromEntries,
  )
where

import Control.Monad (foldM)
import Data.Kind (Type)
import qualified Data.Vector.Unboxed as U
import Moonlight.Core (AdditiveGroup (..), AdditiveMonoid (..), MoonlightError (..))
import Moonlight.LinAlg.Pure.Sparse.Types
  ( SparseCSR,
    canonicalCSRFromValidEntriesUnchecked,
    mkSparseCSR,
    validateCOOEntries,
  )
import Prelude

canonicalCSRFromEntries ::
  (Eq a, AdditiveGroup a, U.Unbox a) =>
  Int ->
  Int ->
  [(Int, Int, a)] ->
  Either MoonlightError (SparseCSR a)
canonicalCSRFromEntries rowCount columnCount entries = do
  validateCOOEntries rowCount columnCount entries
  pure (canonicalCSRFromValidEntriesUnchecked rowCount columnCount entries)

orderedCSRFromEntries ::
  (Eq a, AdditiveMonoid a, U.Unbox a) =>
  Int ->
  Int ->
  [(Int, Int, a)] ->
  Either MoonlightError (SparseCSR a)
orderedCSRFromEntries rowCount columnCount entries
  | rowCount < 0 || columnCount < 0 =
      Left
        ( InvariantViolation
            ( "ordered CSR dimensions must be non-negative, received "
                <> show (rowCount, columnCount)
            )
        )
  | otherwise = do
      builtState <-
        foldM
          (appendOrderedEntry rowCount columnCount)
          initialOrderedCSRState
          entries
      let completedState = closeRows rowCount builtState
      mkSparseCSR
        rowCount
        columnCount
        (reverse (orderedOffsetsRev completedState))
        (reverse (orderedColumnsRev completedState))
        (reverse (orderedValuesRev completedState))

type OrderedCSRState :: Type -> Type
data OrderedCSRState a = OrderedCSRState
  { orderedCurrentRow :: !Int,
    orderedEntryCount :: !Int,
    orderedOffsetsRev :: [Int],
    orderedColumnsRev :: [Int],
    orderedValuesRev :: [a],
    orderedPreviousCoordinate :: Maybe (Int, Int)
  }

initialOrderedCSRState :: OrderedCSRState a
initialOrderedCSRState =
  OrderedCSRState
    { orderedCurrentRow = 0,
      orderedEntryCount = 0,
      orderedOffsetsRev = [0],
      orderedColumnsRev = [],
      orderedValuesRev = [],
      orderedPreviousCoordinate = Nothing
    }

appendOrderedEntry ::
  Int ->
  Int ->
  OrderedCSRState a ->
  (Int, Int, a) ->
  Either MoonlightError (OrderedCSRState a)
appendOrderedEntry rowCount columnCount stateValue (rowIndex, columnIndex, entryValue)
  | rowIndex < 0 || rowIndex >= rowCount || columnIndex < 0 || columnIndex >= columnCount =
      Left
        ( InvariantViolation
            ( "ordered CSR entry index out of bounds: "
                <> show (rowIndex, columnIndex)
                <> " for shape "
                <> show (rowCount, columnCount)
            )
        )
  | not (coordinateStrictlyAfter (orderedPreviousCoordinate stateValue) (rowIndex, columnIndex)) =
      Left
        ( InvariantViolation
            ( "ordered CSR entries must be in strictly increasing row-major order; encountered "
                <> show (rowIndex, columnIndex)
                <> " after "
                <> show (orderedPreviousCoordinate stateValue)
            )
        )
  | otherwise =
      let rowClosedState = closeRows rowIndex stateValue
       in Right
            rowClosedState
              { orderedEntryCount = orderedEntryCount rowClosedState + 1,
                orderedColumnsRev = columnIndex : orderedColumnsRev rowClosedState,
                orderedValuesRev = entryValue : orderedValuesRev rowClosedState,
                orderedPreviousCoordinate = Just (rowIndex, columnIndex)
              }

coordinateStrictlyAfter :: Maybe (Int, Int) -> (Int, Int) -> Bool
coordinateStrictlyAfter previousCoordinate currentCoordinate =
  case previousCoordinate of
    Nothing -> True
    Just previousValue -> previousValue < currentCoordinate

closeRows :: Int -> OrderedCSRState a -> OrderedCSRState a
closeRows targetRow stateValue
  | orderedCurrentRow stateValue >= targetRow = stateValue
  | otherwise =
      let closedRowCount = targetRow - orderedCurrentRow stateValue
       in stateValue
            { orderedCurrentRow = targetRow,
              orderedOffsetsRev =
                replicate closedRowCount (orderedEntryCount stateValue)
                  <> orderedOffsetsRev stateValue
            }
