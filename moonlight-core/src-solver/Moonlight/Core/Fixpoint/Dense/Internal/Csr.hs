-- | Compressed-sparse-row adjacency for finite @Int@ digraphs: construction
-- from rows, per-vertex target slices, out-degree, and transpose. A pure leaf.
module Moonlight.Core.Fixpoint.Dense.Internal.Csr
  ( CsrRole (..),
    Csr (..),
    GraphCsr,
    RowCsr,
    CsrAdmissionError (..),
    csrFromRows,
    csrFromPackedRows,
    csrFromBoundedRows,
    csrTargetsForKey,
    csrTargetsSet,
    csrOutDegree,
    csrTranspose,
    inBounds,
  )
where

import Control.Monad.ST (runST)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.List qualified as List
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import Prelude

data CsrRole
  = SquareGraph
  | RectangularRows

type Csr :: CsrRole -> Type
data Csr shape = Csr
  { csrVertexCount :: !Int,
    csrOffsets :: !(Vector Int),
    csrTargets :: !(Vector Int)
  }
  deriving stock (Eq, Show)

type GraphCsr = Csr 'SquareGraph

type RowCsr = Csr 'RectangularRows

-- | A malformed packed row representation. Indices identify the offending
-- offset or row; target identifiers have no square-graph bound.
data CsrAdmissionError
  = CsrNegativeRowCount !Int
  | CsrOffsetCountMismatch !Int !Int
  | CsrInitialOffsetNonzero !Int
  | CsrDecreasingOffset !Int !Int !Int
  | CsrFinalOffsetMismatch !Int !Int
  | CsrNonascendingRowTargets !Int !Int !Int
  deriving stock (Eq, Show)

-- | Admit already sorted, duplicate-free rectangular rows without copying or
-- normalizing either vector. In 'CsrOffsetCountMismatch', the fields are the
-- declared row count and actual offset count, respectively.
csrFromPackedRows :: Int -> Vector Int -> Vector Int -> Either CsrAdmissionError RowCsr
csrFromPackedRows rowCount offsets targets
  | rowCount < 0 = Left (CsrNegativeRowCount rowCount)
  | toInteger (U.length offsets) /= toInteger rowCount + 1 =
      Left (CsrOffsetCountMismatch rowCount (U.length offsets))
  | otherwise = do
      case offsets U.!? 0 of
        Just 0 -> Right ()
        Just initialOffset -> Left (CsrInitialOffsetNonzero initialOffset)
        Nothing -> Left (CsrOffsetCountMismatch rowCount 0)
      finalOffset <- U.ifoldM' admitOffset 0 offsets
      if finalOffset == U.length targets
        then Right ()
        else Left (CsrFinalOffsetMismatch finalOffset (U.length targets))
      U.ifoldM' admitRow () (U.zip offsets (U.drop 1 offsets))
      pure (Csr rowCount offsets targets)
  where
    admitOffset previous index offset
      | previous <= offset = Right offset
      | otherwise = Left (CsrDecreasingOffset index previous offset)
    admitRow () row (start, end) =
      () <$ U.foldM' (admitTarget row) Nothing (U.slice start (end - start) targets)
    admitTarget row previous target =
      case previous of
        Just prior | prior >= target -> Left (CsrNonascendingRowTargets row prior target)
        _ -> Right (Just target)
{-# INLINE csrFromPackedRows #-}

csrFromRows :: Int -> [[Int]] -> RowCsr
csrFromRows vertexCount rows =
  csrFromRowsWith (const True) vertexCount rows
{-# INLINE csrFromRows #-}

csrFromBoundedRows :: Int -> [[Int]] -> GraphCsr
csrFromBoundedRows vertexCount rows =
  csrFromRowsWith (inBounds n) n rows
  where
    n = max 0 vertexCount
{-# INLINE csrFromBoundedRows #-}

csrFromRowsWith :: (Int -> Bool) -> Int -> [[Int]] -> Csr shape
csrFromRowsWith keepTarget vertexCount rows =
  Csr
    { csrVertexCount = n,
      csrOffsets = U.fromList offsets,
      csrTargets = U.fromList targets
    }
  where
    n = max 0 vertexCount
    normalizedRows =
      fmap
        (IntSet.toAscList . IntSet.filter keepTarget . IntSet.fromList)
        (take n (rows <> repeat []))
    offsets = List.scanl' (+) 0 (fmap length normalizedRows)
    targets = concat normalizedRows
{-# INLINE csrFromRowsWith #-}

csrTranspose :: GraphCsr -> GraphCsr
csrTranspose csr =
  runST $ do
    incomingCounts <- UM.replicate vertexCount 0
    U.mapM_ (UM.modify incomingCounts (+ 1)) inputTargets
    frozenIncomingCounts <- U.unsafeFreeze incomingCounts
    let transposedOffsets = U.scanl' (+) 0 frozenIncomingCounts
    insertionOffsets <- U.thaw (U.init transposedOffsets)
    transposedTargets <- UM.new (U.length inputTargets)
    U.mapM_
      ( \source ->
          U.mapM_
            ( \target -> do
                insertionOffset <- UM.read insertionOffsets target
                UM.write transposedTargets insertionOffset source
                UM.write insertionOffsets target (insertionOffset + 1)
            )
            (csrTargetsForKey csr source)
      )
      (U.enumFromN 0 vertexCount)
    frozenTransposedTargets <- U.unsafeFreeze transposedTargets
    pure
      Csr
        { csrVertexCount = vertexCount,
          csrOffsets = transposedOffsets,
          csrTargets = frozenTransposedTargets
        }
  where
    vertexCount = csrVertexCount csr
    inputTargets = csrTargets csr
{-# INLINE csrTranspose #-}

csrTargetsForKey :: Csr shape -> Int -> Vector Int
csrTargetsForKey csr key =
  case (csrOffsets csr U.!? key, csrOffsets csr U.!? (key + 1)) of
    (Just offset, Just nextOffset) -> U.slice offset (nextOffset - offset) (csrTargets csr)
    _ -> U.empty
{-# INLINE csrTargetsForKey #-}

csrTargetsSet :: Csr shape -> Int -> IntSet
csrTargetsSet csr key =
  IntSet.fromDistinctAscList (U.toList (csrTargetsForKey csr key))
{-# INLINE csrTargetsSet #-}

csrOutDegree :: Csr shape -> Int -> Int
csrOutDegree csr key =
  U.length (csrTargetsForKey csr key)
{-# INLINE csrOutDegree #-}

inBounds :: Int -> Int -> Bool
inBounds n key = key >= 0 && key < n
{-# INLINE inBounds #-}
