{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StrictData #-}

module Moonlight.Algebra.Pure.LaneVector
  ( LaneVector,
    laneCount,
    laneVectorZero,
    laneVectorFromLanes,
    laneVectorLanes,
  )
where

import Data.Vector.Unboxed qualified as UVector
import Data.Word (Word64)
import Moonlight.Core
  ( AdditiveGroup (..),
    AdditiveMonoid (..),
  )
import Prelude
  ( Eq,
    Int,
    Ordering (..),
    Show,
    compare,
    negate,
    (-),
    (+),
  )

newtype LaneVector = LaneVector (UVector.Vector Word64)
  deriving stock (Eq, Show)

laneCount :: Int
laneCount = 16
{-# INLINE laneCount #-}

laneVectorZero :: LaneVector
laneVectorZero =
  LaneVector (UVector.replicate laneCount 0)
{-# INLINE laneVectorZero #-}

laneVectorFromLanes :: UVector.Vector Word64 -> LaneVector
laneVectorFromLanes lanes =
  let inputLaneCount = UVector.length lanes
   in case compare inputLaneCount laneCount of
        LT ->
          LaneVector
            ( lanes
                UVector.++ UVector.replicate (laneCount - inputLaneCount) 0
            )
        EQ -> LaneVector lanes
        GT -> LaneVector (UVector.force (UVector.take laneCount lanes))
{-# INLINE laneVectorFromLanes #-}

laneVectorLanes :: LaneVector -> UVector.Vector Word64
laneVectorLanes (LaneVector lanes) =
  lanes
{-# INLINE laneVectorLanes #-}

instance AdditiveMonoid LaneVector where
  zero = laneVectorZero
  add (LaneVector left) (LaneVector right) =
    LaneVector (UVector.zipWith (+) left right)
  {-# INLINE zero #-}
  {-# INLINE add #-}

instance AdditiveGroup LaneVector where
  neg (LaneVector lanes) =
    LaneVector (UVector.map negate lanes)
  {-# INLINE neg #-}

  sub (LaneVector left) (LaneVector right) =
    LaneVector (UVector.zipWith (-) left right)
  {-# INLINE sub #-}
