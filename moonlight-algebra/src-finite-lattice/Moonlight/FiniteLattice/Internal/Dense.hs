{-# LANGUAGE GHC2024 #-}

module Moonlight.FiniteLattice.Internal.Dense
  ( compileDensePlan,
  )
where

import Moonlight.FiniteLattice.Internal.Distributive
  ( ContextDistributiveRowsResult (..),
    distributivePlanFromRows,
  )
import Moonlight.FiniteLattice.Internal.Index
  ( ContextIndex (..),
    contextIndexValueForKey,
    decodeIndexKeys,
  )
import Moonlight.FiniteLattice.Internal.Key
  ( ContextKey (..),
    ContextKeySet,
    ContextKeyTable,
    contextKeySetToAscList,
    contextKeyTableGenerateM,
  )
import Moonlight.FiniteLattice.Internal.Layout
  ( checkedPairCellCount,
  )
import Moonlight.FiniteLattice.Internal.Plan
  ( ContextDenseRowsPlan (..),
    ContextDenseTablePlan (..),
    ContextMaskPlan (..),
    ContextPlan (..),
  )
import Moonlight.FiniteLattice.Internal.Relation
  ( ContextRowIndex,
    ContextRows,
    contextRowIndexFromRows,
    rowJoinCandidateKeys,
    rowJoinKeyMaybe,
    rowMeetCandidateKeys,
    rowMeetKeyMaybe,
  )
import Moonlight.FiniteLattice.Internal.Types
  ( ContextCompileLimits,
    ContextLatticeCompileError (..),
  )

compileDensePlan ::
  Ord c =>
  ContextCompileLimits ->
  ContextIndex c ->
  ContextKey ->
  ContextKey ->
  ContextRows ->
  ContextRows ->
  Either (ContextLatticeCompileError c) ContextPlan
compileDensePlan limits index topKey bottomKey upperRows lowerRows =
  case checkedPairCellCount limits (ciSize index) of
    Just pairCellCount -> do
      (joinTable, meetTable) <-
        compileDenseLatticeTables
          index
          pairCellCount
          upperRows
          lowerRows
          upperRowIndex
          lowerRowIndex
      pure
        ( DensePlan
            ContextDenseTablePlan
              { cdtpSize = ciSize index,
                cdtpUpperRows = upperRows,
                cdtpLowerRows = lowerRows,
                cdtpJoinTable = joinTable,
                cdtpMeetTable = meetTable
              }
        )
    Nothing ->
      case
        distributivePlanFromRows
          (ciSize index)
          topKey
          bottomKey
          upperRows
          lowerRows
          upperRowIndex
          lowerRowIndex
        of
        ContextRowJoinAbsent leftKey rightKey candidates ->
          Left
            ( ContextLatticeJoinDoesNotExist
                (contextIndexValueForKey index leftKey)
                (contextIndexValueForKey index rightKey)
                (decodeIndexKeys index candidates)
            )
        ContextRowMeetAbsent leftKey rightKey candidates ->
          Left
            ( ContextLatticeMeetDoesNotExist
                (contextIndexValueForKey index leftKey)
                (contextIndexValueForKey index rightKey)
                (decodeIndexKeys index candidates)
            )
        ContextDenseRowsValidated ->
          Right (denseRowsPlan upperRows lowerRows upperRowIndex lowerRowIndex)
        ContextDistributiveRowsPlan distributivePlan ->
          Right (MaskPlan (DistributivePlan distributivePlan))
  where
    upperRowIndex = contextRowIndexFromRows upperRows
    lowerRowIndex = contextRowIndexFromRows lowerRows

denseRowsPlan ::
  ContextRows ->
  ContextRows ->
  ContextRowIndex ->
  ContextRowIndex ->
  ContextPlan
denseRowsPlan upperRows lowerRows upperRowIndex lowerRowIndex =
  MaskPlan
    ( DenseRowsPlan
        ContextDenseRowsPlan
          { cdrpUpperRows = upperRows,
            cdrpLowerRows = lowerRows,
            cdrpUpperRowIndex = upperRowIndex,
            cdrpLowerRowIndex = lowerRowIndex
          }
    )

compileDenseLatticeTables ::
  Ord c =>
  ContextIndex c ->
  Int ->
  ContextRows ->
  ContextRows ->
  ContextRowIndex ->
  ContextRowIndex ->
  Either
    (ContextLatticeCompileError c)
    (ContextKeyTable, ContextKeyTable)
compileDenseLatticeTables index pairCellCount upperRows lowerRows upperRowIndex lowerRowIndex = do
  joinTable <-
    contextKeyTableGenerateM size pairCellCount $ \offset ->
      let (leftOrdinal, rightOrdinal) = offset `quotRem` size
       in leastUpperBoundKey
            index
            upperRows
            lowerRows
            upperRowIndex
            (ContextKey leftOrdinal)
            (ContextKey rightOrdinal)
  meetTable <-
    contextKeyTableGenerateM size pairCellCount $ \offset ->
      let (leftOrdinal, rightOrdinal) = offset `quotRem` size
       in greatestLowerBoundKey
            index
            upperRows
            lowerRows
            lowerRowIndex
            (ContextKey leftOrdinal)
            (ContextKey rightOrdinal)
  pure (joinTable, meetTable)
  where
    size = ciSize index

leastUpperBoundKey ::
  Ord c =>
  ContextIndex c ->
  ContextRows ->
  ContextRows ->
  ContextRowIndex ->
  ContextKey ->
  ContextKey ->
  Either (ContextLatticeCompileError c) ContextKey
leastUpperBoundKey index upperRows lowerRows upperRowIndex leftKey rightKey =
  case rowJoinKeyMaybe upperRows lowerRows upperRowIndex leftKey rightKey of
    Just joinKey -> Right joinKey
    Nothing ->
      selectUniqueJoinKey
        index
        leftKey
        rightKey
        (rowJoinCandidateKeys upperRows lowerRows leftKey rightKey)

greatestLowerBoundKey ::
  Ord c =>
  ContextIndex c ->
  ContextRows ->
  ContextRows ->
  ContextRowIndex ->
  ContextKey ->
  ContextKey ->
  Either (ContextLatticeCompileError c) ContextKey
greatestLowerBoundKey index upperRows lowerRows lowerRowIndex leftKey rightKey =
  case rowMeetKeyMaybe upperRows lowerRows lowerRowIndex leftKey rightKey of
    Just meetKey -> Right meetKey
    Nothing ->
      selectUniqueMeetKey
        index
        leftKey
        rightKey
        (rowMeetCandidateKeys upperRows lowerRows leftKey rightKey)

selectUniqueJoinKey ::
  Ord c =>
  ContextIndex c ->
  ContextKey ->
  ContextKey ->
  ContextKeySet ->
  Either (ContextLatticeCompileError c) ContextKey
selectUniqueJoinKey index leftKey rightKey candidates =
  case contextKeySetToAscList candidates of
    [keyOrdinal] -> Right (ContextKey keyOrdinal)
    _ ->
      Left
        ( ContextLatticeJoinDoesNotExist
            (contextIndexValueForKey index leftKey)
            (contextIndexValueForKey index rightKey)
            (decodeIndexKeys index candidates)
        )

selectUniqueMeetKey ::
  Ord c =>
  ContextIndex c ->
  ContextKey ->
  ContextKey ->
  ContextKeySet ->
  Either (ContextLatticeCompileError c) ContextKey
selectUniqueMeetKey index leftKey rightKey candidates =
  case contextKeySetToAscList candidates of
    [keyOrdinal] -> Right (ContextKey keyOrdinal)
    _ ->
      Left
        ( ContextLatticeMeetDoesNotExist
            (contextIndexValueForKey index leftKey)
            (contextIndexValueForKey index rightKey)
            (decodeIndexKeys index candidates)
        )
