{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Matched end-to-end alpha-persistence work and its canonical receipt.
-- Input generation remains outside the timed action; Delaunay construction,
-- exact alpha births, cellular lowering, one reduction, and all critical-birth
-- Betti queries remain inside it.
module Moonlight.Planar.Bench.SpadeCompare.Alpha
  ( AlphaPersistenceObstruction (..)
  , AlphaPersistenceReceipt (..)
  , alphaPersistenceReceipt
  , renderAlphaPersistenceReceipt
  ) where

import Control.DeepSeq (NFData)
import Data.Bifunctor (first)
import Data.Bits (xor)
import Data.List (intercalate)
import Data.Vector (Vector)
import Data.Vector.Unboxed qualified as UnboxedVector
import Data.Word (Word64)
import GHC.Generics (Generic)
import Moonlight.Homology.Boundary
  ( degreeCardinality
  )
import Moonlight.Homology.Chain
  ( HomologicalDegree (..)
  , HomologyFailure
  , PersistencePair (..)
  )
import Moonlight.Homology.Persistence
  ( criticalBettiTableValues
  , filteredBaseComplex
  , filteredCriticalValues
  , mod2PersistentPairsWithCriticalBettiTable
  )
import Moonlight.Planar.Alpha
  ( AlphaFiltrationError
  , alphaFiltration
  )
import Moonlight.Planar.BulkLoad (delaunayGeometry)
import Moonlight.Planar.CellComplex
  ( DCELError
  , filteredAlphaComplex
  )
import Moonlight.Planar.Types (BuildError)
import Moonlight.Planar.Point (Point)

data AlphaPersistenceObstruction
  = AlphaPersistenceBuildRefused !BuildError
  | AlphaPersistenceBirthRefused !AlphaFiltrationError
  | AlphaPersistenceLoweringRefused !DCELError
  | AlphaPersistenceReductionRefused !HomologyFailure
  deriving stock (Eq, Show)

data AlphaPersistenceReceipt = AlphaPersistenceReceipt
  { alphaReceiptCriticalBirths :: !Int
  , alphaReceiptVertexCells :: !Int
  , alphaReceiptEdgeCells :: !Int
  , alphaReceiptFaceCells :: !Int
  , alphaReceiptFinitePairs :: !Int
  , alphaReceiptEssentialPairs :: !Int
  , alphaReceiptProfileChecksum :: !Word64
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

alphaPersistenceReceipt
  :: Vector Point
  -> Either AlphaPersistenceObstruction AlphaPersistenceReceipt
alphaPersistenceReceipt points = do
  triangulation <-
    first AlphaPersistenceBuildRefused (delaunayGeometry points)
  filtration <-
    first AlphaPersistenceBirthRefused (alphaFiltration triangulation)
  filtered <-
    first AlphaPersistenceLoweringRefused (filteredAlphaComplex filtration)
  (pairs, criticalBettiTable) <-
    first AlphaPersistenceReductionRefused
      (mod2PersistentPairsWithCriticalBettiTable filtered)
  let criticalBirths = filteredCriticalValues filtered
      finite = filteredBaseComplex filtered
      isFinitePair = maybe False (const True) . persistenceDeath
  pure
    AlphaPersistenceReceipt
      { alphaReceiptCriticalBirths = length criticalBirths
      , alphaReceiptVertexCells = degreeCardinality finite (HomologicalDegree 0)
      , alphaReceiptEdgeCells = degreeCardinality finite (HomologicalDegree 1)
      , alphaReceiptFaceCells = degreeCardinality finite (HomologicalDegree 2)
      , alphaReceiptFinitePairs = length (filter isFinitePair pairs)
      , alphaReceiptEssentialPairs = length (filter (not . isFinitePair) pairs)
      , alphaReceiptProfileChecksum =
          UnboxedVector.foldl'
            mixChecksum
            fnvOffset
            (criticalBettiTableValues criticalBettiTable)
      }

mixChecksum :: Word64 -> Int -> Word64
mixChecksum accumulator value =
  (accumulator `xor` fromIntegral value) * fnvPrime

fnvOffset :: Word64
fnvOffset = 14695981039346656037

fnvPrime :: Word64
fnvPrime = 1099511628211

renderAlphaPersistenceReceipt :: AlphaPersistenceReceipt -> String
renderAlphaPersistenceReceipt receipt =
  intercalate
    ","
    ( fmap
        show
        [ fromIntegral (alphaReceiptCriticalBirths receipt) :: Word64
        , fromIntegral (alphaReceiptVertexCells receipt)
        , fromIntegral (alphaReceiptEdgeCells receipt)
        , fromIntegral (alphaReceiptFaceCells receipt)
        , fromIntegral (alphaReceiptFinitePairs receipt)
        , fromIntegral (alphaReceiptEssentialPairs receipt)
        , alphaReceiptProfileChecksum receipt
        ]
    )
