{-# LANGUAGE AllowAmbiguousTypes #-}

module Moonlight.LinAlg.Pure.Domain.Bareiss
  ( BareissExactDivision (..),
    BareissExactDivisionObligation (..),
    BareissElimination (..),
    euclideanBareissExactDivision,
    bareissEliminationWith,
    bareissElimination,
    bareissRankWith,
    bareissRank,
    bareissDeterminantWith,
    bareissDeterminant,
    bareissEchelonWith,
    bareissEchelon,
  )
where

import Control.Monad (foldM)
import Data.Kind (Type)
import Data.Vector qualified as Box
import GHC.TypeNats (KnownNat, Nat)
import Moonlight.Algebra.Pure.Ring
  ( EuclideanDomain (..),
    IntegralDomain (..),
    mkNonZeroDivisor,
  )
import Moonlight.Core
  ( AdditiveGroup (..),
    AdditiveMonoid (..),
    MoonlightError (..),
    MultiplicativeMonoid (..),
  )
import Moonlight.LinAlg.Internal.Backend.RowStore
  ( RowStore,
    rowStoreFlatten,
    rowStoreFromRows,
    rowStoreRowAtInt,
    rowStoreShape,
    rowStoreValueAtInt,
    swapRowsStoreAtInt,
    traverseRowStoreWithIndex,
  )
import Moonlight.LinAlg.Pure.Dense.Types
  ( Matrix,
    fromListMatrix,
  )
import qualified Moonlight.LinAlg.Pure.Dense.Types as DenseTypes
import Prelude

type BareissExactDivisionObligation :: Type -> Type
data BareissExactDivisionObligation a = BareissExactDivisionObligation
  { bareissExactDivisionStep :: !Int,
    bareissExactDivisionRow :: !Int,
    bareissExactDivisionColumn :: !Int,
    bareissExactDividend :: a,
    bareissExactDivisor :: a
  }

type BareissExactDivision :: Type -> Type
newtype BareissExactDivision a = BareissExactDivision
  { runBareissExactDivision :: BareissExactDivisionObligation a -> Either MoonlightError a
  }

type BareissElimination :: Nat -> Nat -> Type -> Type
data BareissElimination r c a = BareissElimination
  { bareissResultRank :: !Int,
    bareissResultDeterminant :: !(Maybe a),
    bareissResultEchelon :: Matrix r c a
  }

type BareissState :: Type -> Type
data BareissState a = BareissState
  { bareissStateRows :: RowStore a,
    bareissStateRank :: !Int,
    bareissStatePreviousPivot :: a,
    bareissStateDetSign :: a
  }

euclideanBareissExactDivision :: EuclideanDomain a => BareissExactDivision a
euclideanBareissExactDivision =
  BareissExactDivision $ \obligation ->
    case mkNonZeroDivisor (bareissExactDivisor obligation) of
      Nothing -> Left (InvariantViolation "Bareiss exact division received a zero divisor")
      Just divisor ->
        let (quotientValue, remainderValue) =
              divideWithRemainder
                (bareissExactDividend obligation)
                divisor
         in if isZero remainderValue
              then Right quotientValue
              else
                Left
                  ( InvariantViolation
                      ( "Bareiss exact division obligation failed at "
                          <> show
                            ( bareissExactDivisionStep obligation,
                              bareissExactDivisionRow obligation,
                              bareissExactDivisionColumn obligation
                            )
                      )
                  )

bareissEliminationWith ::
  forall r c a.
  (KnownNat r, KnownNat c, IntegralDomain a) =>
  BareissExactDivision a ->
  Matrix r c a ->
  Either MoonlightError (BareissElimination r c a)
bareissEliminationWith division matrixValue = do
  initialRows <- DenseTypes.matrixToRows matrixValue
  let (rowCount, columnCount) = DenseTypes.matrixShape matrixValue
      initialState =
        BareissState
          { bareissStateRows = rowStoreFromRows initialRows,
            bareissStateRank = 0,
            bareissStatePreviousPivot = one,
            bareissStateDetSign = one
          }
  finalState <- foldlBareiss division columnCount initialState
  echelonMatrix <- fromListMatrix @r @c (rowStoreFlatten (bareissStateRows finalState))
  pure
    BareissElimination
      { bareissResultRank = bareissStateRank finalState,
        bareissResultDeterminant = determinantValue rowCount columnCount finalState,
        bareissResultEchelon = echelonMatrix
      }

bareissElimination ::
  forall r c a.
  (KnownNat r, KnownNat c, EuclideanDomain a) =>
  Matrix r c a ->
  Either MoonlightError (BareissElimination r c a)
bareissElimination =
  bareissEliminationWith euclideanBareissExactDivision

bareissRankWith ::
  forall r c a.
  (KnownNat r, KnownNat c, IntegralDomain a) =>
  BareissExactDivision a ->
  Matrix r c a ->
  Either MoonlightError Int
bareissRankWith division =
  fmap bareissResultRank . bareissEliminationWith division

bareissRank ::
  forall r c a.
  (KnownNat r, KnownNat c, EuclideanDomain a) =>
  Matrix r c a ->
  Either MoonlightError Int
bareissRank =
  bareissRankWith euclideanBareissExactDivision

bareissDeterminantWith ::
  forall n a.
  (KnownNat n, IntegralDomain a) =>
  BareissExactDivision a ->
  Matrix n n a ->
  Either MoonlightError a
bareissDeterminantWith division matrixValue =
  bareissEliminationWith division matrixValue
    >>= \result ->
      case bareissResultDeterminant result of
        Just determinant -> Right determinant
        Nothing -> Left (InvariantViolation "Bareiss square determinant produced no determinant")

bareissDeterminant ::
  forall n a.
  (KnownNat n, EuclideanDomain a) =>
  Matrix n n a ->
  Either MoonlightError a
bareissDeterminant =
  bareissDeterminantWith euclideanBareissExactDivision

bareissEchelonWith ::
  forall r c a.
  (KnownNat r, KnownNat c, IntegralDomain a) =>
  BareissExactDivision a ->
  Matrix r c a ->
  Either MoonlightError (Matrix r c a)
bareissEchelonWith division =
  fmap bareissResultEchelon . bareissEliminationWith division

bareissEchelon ::
  forall r c a.
  (KnownNat r, KnownNat c, EuclideanDomain a) =>
  Matrix r c a ->
  Either MoonlightError (Matrix r c a)
bareissEchelon =
  bareissEchelonWith euclideanBareissExactDivision

foldlBareiss ::
  IntegralDomain a =>
  BareissExactDivision a ->
  Int ->
  BareissState a ->
  Either MoonlightError (BareissState a)
foldlBareiss division columnCount initialState =
  foldM
    (\stateValue columnIndex -> bareissStep division columnIndex stateValue)
    initialState
    [0 .. columnCount - 1]

bareissStep ::
  IntegralDomain a =>
  BareissExactDivision a ->
  Int ->
  BareissState a ->
  Either MoonlightError (BareissState a)
bareissStep division pivotColumn stateValue
  | bareissStateRank stateValue >= rowCount = Right stateValue
  | otherwise = do
      pivotCandidate <- findPivotRowAt pivotRow pivotColumn rows
      case pivotCandidate of
        Nothing -> Right stateValue
        Just sourceRow -> do
          swappedRows <-
            if sourceRow == pivotRow
              then Right rows
              else
                swapRowsStoreAtInt
                  (InvariantViolation ("Bareiss row pivot swap failed at " <> show (pivotRow, sourceRow)))
                  pivotRow
                  sourceRow
                  rows
          pivotValue <-
            rowStoreValueAtInt
              (InvariantViolation ("Bareiss pivot lookup failed at " <> show (pivotRow, pivotColumn)))
              pivotRow
              pivotColumn
              swappedRows
          pivotRowValues <-
            rowStoreRowAtInt
              (InvariantViolation ("Bareiss pivot row lookup failed at " <> show pivotRow))
              pivotRow
              swappedRows
          eliminatedRows <-
            eliminateBareissColumn
              division
              pivotRow
              pivotColumn
              pivotValue
              (bareissStatePreviousPivot stateValue)
              pivotRowValues
              swappedRows
          Right
            stateValue
              { bareissStateRows = eliminatedRows,
                bareissStateRank = pivotRow + 1,
                bareissStatePreviousPivot = pivotValue,
                bareissStateDetSign =
                  if sourceRow == pivotRow
                    then bareissStateDetSign stateValue
                    else neg (bareissStateDetSign stateValue)
              }
  where
    rows = bareissStateRows stateValue
    (rowCount, _) = rowStoreShape rows
    pivotRow = bareissStateRank stateValue

findPivotRowAt :: IntegralDomain a => Int -> Int -> RowStore a -> Either MoonlightError (Maybe Int)
findPivotRowAt startRow columnIndex rows =
  fmap
    (fmap fst . firstNonzero)
    ( traverse
        ( \rowIndex ->
            fmap
              (\value -> (rowIndex, value))
              (rowStoreValueAtInt (InvariantViolation ("Bareiss pivot search failed at " <> show (rowIndex, columnIndex))) rowIndex columnIndex rows)
        )
        [startRow .. fst (rowStoreShape rows) - 1]
    )
  where
    firstNonzero :: IntegralDomain a => [(Int, a)] -> Maybe (Int, a)
    firstNonzero =
      foldr
        ( \candidate rest ->
            if isZero (snd candidate)
              then rest
              else Just candidate
        )
        Nothing

eliminateBareissColumn ::
  IntegralDomain a =>
  BareissExactDivision a ->
  Int ->
  Int ->
  a ->
  a ->
  Box.Vector a ->
  RowStore a ->
  Either MoonlightError (RowStore a)
eliminateBareissColumn division pivotRow pivotColumn pivotValue previousPivot pivotRowValues rows =
  traverseRowStoreWithIndex transformRow rows
  where
    transformRow rowIndex rowValues
      | rowIndex <= pivotRow = Right rowValues
      | otherwise = do
          targetPivotEntry <-
            maybe
              (Left (InvariantViolation ("Bareiss target pivot lookup failed at " <> show (rowIndex, pivotColumn))))
              Right
              (rowValues Box.!? pivotColumn)
          updatedRow <-
            Box.imapM
              ( \columnIndex entryValue ->
                  if columnIndex < pivotColumn
                    then Right entryValue
                    else
                      if columnIndex == pivotColumn
                        then Right zero
                        else do
                          pivotRowEntry <-
                            maybe
                              (Left (InvariantViolation ("Bareiss pivot row lookup failed at column " <> show columnIndex)))
                              Right
                              (pivotRowValues Box.!? columnIndex)
                          divideBareissEntry
                            division
                            pivotRow
                            rowIndex
                            columnIndex
                            ((pivotValue `mul` entryValue) `sub` (targetPivotEntry `mul` pivotRowEntry))
                            previousPivot
              )
              rowValues
          Right updatedRow

divideBareissEntry ::
  BareissExactDivision a ->
  Int ->
  Int ->
  Int ->
  a ->
  a ->
  Either MoonlightError a
divideBareissEntry division step rowIndex columnIndex dividend divisor =
  runBareissExactDivision
    division
    BareissExactDivisionObligation
      { bareissExactDivisionStep = step,
        bareissExactDivisionRow = rowIndex,
        bareissExactDivisionColumn = columnIndex,
        bareissExactDividend = dividend,
        bareissExactDivisor = divisor
      }

determinantValue :: IntegralDomain a => Int -> Int -> BareissState a -> Maybe a
determinantValue rowCount columnCount stateValue
  | rowCount /= columnCount = Nothing
  | rowCount == 0 = Just one
  | bareissStateRank stateValue < rowCount = Just zero
  | otherwise = Just (bareissStateDetSign stateValue `mul` bareissStatePreviousPivot stateValue)
