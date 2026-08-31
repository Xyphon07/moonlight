{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Homology.Pure.Rank.GF2
  ( GF2RankFailure (..),
    PreparedGF2Boundary,
    pgbIncidence,
    pgbPackedMatrix,
    prepareGF2Boundary,
    rankPreparedGF2Boundary,
    gf2BoundaryRank,
  )
where

import Data.Bifunctor
  ( first,
  )
import Data.Kind
  ( Type,
  )
import Data.Maybe
  ( mapMaybe,
  )
import Moonlight.Homology.Boundary.LinAlg
  ( BoundaryEntry,
    BoundaryIncidence,
    BoundaryIncidenceShapeError,
    boundaryCoefficient,
    boundaryEntries,
    mkBoundaryIncidence,
    sourceCardinality,
    sourceIndex,
    targetCardinality,
    targetIndex,
  )
import Moonlight.LinAlg.Pure.Dense.GF2
  ( GF2 (..),
    GF2MatrixEntry (..),
    GF2PackedMatrix,
    GF2PackedMatrixFailure,
    mkGF2PackedMatrix,
    rankGF2PackedMatrix,
  )
import Numeric.Natural
  ( Natural,
  )

type GF2RankFailure :: Type
data GF2RankFailure
  = GF2RankNegativeSourceCardinality !Int
  | GF2RankNegativeTargetCardinality !Int
  | GF2RankBoundaryShapeInvalid !BoundaryIncidenceShapeError
  | GF2RankPackedMatrixInvalid !GF2PackedMatrixFailure
  deriving stock (Eq, Show)

type PreparedGF2Boundary :: Type
data PreparedGF2Boundary = PreparedGF2Boundary
  { preparedBoundaryIncidence :: !(BoundaryIncidence GF2),
    preparedBoundaryPackedMatrix :: !GF2PackedMatrix
  }
  deriving stock (Eq, Show)

pgbIncidence :: PreparedGF2Boundary -> BoundaryIncidence GF2
pgbIncidence =
  preparedBoundaryIncidence

pgbPackedMatrix :: PreparedGF2Boundary -> GF2PackedMatrix
pgbPackedMatrix =
  preparedBoundaryPackedMatrix

prepareGF2Boundary ::
  BoundaryIncidence GF2 ->
  Either GF2RankFailure PreparedGF2Boundary
prepareGF2Boundary incidence = do
  sourceDimension <- sourceCardinalityNatural incidence
  targetDimension <- targetCardinalityNatural incidence
  canonicalIncidence <-
    first GF2RankBoundaryShapeInvalid $
      mkBoundaryIncidence sourceDimension targetDimension (boundaryEntries incidence)
  packedMatrix <-
    first GF2RankPackedMatrixInvalid $
      mkGF2PackedMatrix
        targetDimension
        sourceDimension
        (boundaryEntryMatrixEntry <$> nonzeroEntries canonicalIncidence)
  Right
    PreparedGF2Boundary
      { preparedBoundaryIncidence = canonicalIncidence,
        preparedBoundaryPackedMatrix = packedMatrix
      }
{-# INLINEABLE prepareGF2Boundary #-}

rankPreparedGF2Boundary :: PreparedGF2Boundary -> Int
rankPreparedGF2Boundary =
  rankGF2PackedMatrix . preparedBoundaryPackedMatrix
{-# INLINE rankPreparedGF2Boundary #-}

gf2BoundaryRank ::
  BoundaryIncidence GF2 ->
  Either GF2RankFailure Int
gf2BoundaryRank =
  fmap rankPreparedGF2Boundary . prepareGF2Boundary
{-# INLINEABLE gf2BoundaryRank #-}

nonzeroEntries :: BoundaryIncidence GF2 -> [BoundaryEntry GF2]
nonzeroEntries =
  mapMaybe nonzeroEntry . boundaryEntries
  where
    nonzeroEntry entry =
      case boundaryCoefficient entry of
        GF2Zero -> Nothing
        GF2One -> Just entry
{-# INLINE nonzeroEntries #-}

boundaryEntryMatrixEntry :: BoundaryEntry GF2 -> GF2MatrixEntry
boundaryEntryMatrixEntry entry =
  GF2MatrixEntry
    { gf2EntryRow = targetIndex entry,
      gf2EntryColumn = sourceIndex entry
    }
{-# INLINE boundaryEntryMatrixEntry #-}

sourceCardinalityNatural ::
  BoundaryIncidence r ->
  Either GF2RankFailure Natural
sourceCardinalityNatural incidence =
  nonnegativeNatural
    (GF2RankNegativeSourceCardinality (sourceCardinality incidence))
    (sourceCardinality incidence)
{-# INLINE sourceCardinalityNatural #-}

targetCardinalityNatural ::
  BoundaryIncidence r ->
  Either GF2RankFailure Natural
targetCardinalityNatural incidence =
  nonnegativeNatural
    (GF2RankNegativeTargetCardinality (targetCardinality incidence))
    (targetCardinality incidence)
{-# INLINE targetCardinalityNatural #-}

nonnegativeNatural ::
  failure ->
  Int ->
  Either failure Natural
nonnegativeNatural failureValue value =
  if value < 0
    then Left failureValue
    else Right (fromIntegral value)
{-# INLINE nonnegativeNatural #-}
