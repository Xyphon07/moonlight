module Moonlight.LinAlg.Pure.Geometry.AABB2
  ( AABB2,
    singletonAabb2,
    mkAabb2,
    aabb2Min,
    aabb2Max,
    aabb2Dimensions,
    aabb2Center,
    aabb2HalfExtent,
    aabb2Radius,
    symmetricAabb2,
    unionAabb2,
    expandAabb2,
    intersectAabb2Maybe,
    translateAabb2,
    scaleAabb2,
    containsVec2,
  )
where

import Control.DeepSeq (NFData (..))
import Data.Kind (Type)
import Moonlight.Algebra.Pure.Lattice (JoinSemilattice (..))
import Moonlight.LinAlg.Pure.Geometry.Vec2
  ( Vec2 (..),
    addVec2,
    averageVec2,
    mapVec2,
    maxVec2,
    minVec2,
    mulVec2,
    scaleVec2,
    subVec2,
  )
import Prelude (Bool, Double, Eq, Maybe (..), Ord, Show, abs, seq, sqrt, (*), (+), (<=), (>=), (&&))

type AABB2 :: Type
data AABB2 = AABB2
  { aabb2Min :: {-# UNPACK #-} !Vec2,
    aabb2Max :: {-# UNPACK #-} !Vec2
  }
  deriving stock (Eq, Ord, Show)

instance NFData AABB2 where
  rnf aabbValue = aabbValue `seq` ()

instance JoinSemilattice AABB2 where
  join leftAabb rightAabb =
    AABB2
      { aabb2Min = minVec2 (aabb2Min leftAabb) (aabb2Min rightAabb),
        aabb2Max = maxVec2 (aabb2Max leftAabb) (aabb2Max rightAabb)
      }

singletonAabb2 :: Vec2 -> AABB2
singletonAabb2 point =
  AABB2
    { aabb2Min = point,
      aabb2Max = point
    }

mkAabb2 :: Vec2 -> Vec2 -> Maybe AABB2
mkAabb2 minimumCorner maximumCorner =
  let Vec2 minX minY = minimumCorner
      Vec2 maxX maxY = maximumCorner
   in if minX <= maxX && minY <= maxY
        then Just (AABB2 minimumCorner maximumCorner)
        else Nothing

aabb2Dimensions :: AABB2 -> Vec2
aabb2Dimensions aabbValue =
  subVec2 (aabb2Max aabbValue) (aabb2Min aabbValue)

aabb2Center :: AABB2 -> Vec2
aabb2Center aabbValue =
  averageVec2 (aabb2Min aabbValue) (aabb2Max aabbValue)

aabb2HalfExtent :: AABB2 -> Vec2
aabb2HalfExtent aabbValue =
  scaleVec2 0.5 (aabb2Dimensions aabbValue)

aabb2Radius :: AABB2 -> Double
aabb2Radius aabbValue =
  let Vec2 halfX halfY = aabb2HalfExtent aabbValue
   in sqrt (halfX * halfX + halfY * halfY)

symmetricAabb2 :: Double -> Double -> Maybe AABB2
symmetricAabb2 halfX halfY =
  mkAabb2
    (Vec2 (-halfX) (-halfY))
    (Vec2 halfX halfY)

unionAabb2 :: AABB2 -> AABB2 -> AABB2
unionAabb2 = join

expandAabb2 :: Double -> AABB2 -> Maybe AABB2
expandAabb2 radius aabbValue =
  if 0.0 <= radius
    then
      mkAabb2
        (mapVec2 (+ (-radius)) (aabb2Min aabbValue))
        (mapVec2 (+ radius) (aabb2Max aabbValue))
    else Nothing

intersectAabb2Maybe :: AABB2 -> AABB2 -> Maybe AABB2
intersectAabb2Maybe leftAabb rightAabb =
  mkAabb2
    (maxVec2 (aabb2Min leftAabb) (aabb2Min rightAabb))
    (minVec2 (aabb2Max leftAabb) (aabb2Max rightAabb))

translateAabb2 :: Vec2 -> AABB2 -> AABB2
translateAabb2 translationVector aabbValue =
  AABB2
    { aabb2Min = addVec2 translationVector (aabb2Min aabbValue),
      aabb2Max = addVec2 translationVector (aabb2Max aabbValue)
    }

scaleAabb2 :: Vec2 -> AABB2 -> AABB2
scaleAabb2 scaleVector aabbValue =
  let center = aabb2Center aabbValue
      halfExtent = aabb2HalfExtent aabbValue
      scaledCenter = mulVec2 scaleVector center
      scaledHalfExtent = mulVec2 (mapVec2 abs scaleVector) halfExtent
   in AABB2
        { aabb2Min = subVec2 scaledCenter scaledHalfExtent,
          aabb2Max = addVec2 scaledCenter scaledHalfExtent
        }

containsVec2 :: AABB2 -> Vec2 -> Bool
containsVec2 aabbValue (Vec2 px py) =
  let Vec2 minX minY = aabb2Min aabbValue
      Vec2 maxX maxY = aabb2Max aabbValue
   in px <= maxX && px >= minX && py <= maxY && py >= minY
