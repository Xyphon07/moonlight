module Moonlight.LinAlg.Pure.Geometry.AABB
  ( AABB,
    mkAabb,
    aabbMin,
    aabbMax,
    aabbDimensions,
    aabbCenter,
    aabbHalfExtent,
    aabbRadius,
    symmetricAabb,
    unionAabb,
    expandAabb,
    intersectAabbMaybe,
    translateAabb,
    transformAabb,
    scaleAabb,
  )
where

import Control.DeepSeq (NFData (..))
import Data.Kind (Type)
import Moonlight.Algebra.Pure.Lattice (JoinSemilattice (..))
import Moonlight.LinAlg.Pure.Geometry.Transform.Affine
  ( AffineTransform,
    affineMaxScale,
    atTranslation,
  )
import Moonlight.LinAlg.Pure.Geometry.Vec3
  ( Vec3 (..),
    addVec3,
    averageVec3,
    mapVec3,
    maxVec3,
    minVec3,
    mulVec3,
    scaleVec3,
    subVec3,
  )
import Prelude (Double, Eq, Maybe (..), Ord, Show, abs, seq, sqrt, (*), (+), (<=), (&&))

type AABB :: Type
data AABB = AABB
  { aabbMin :: {-# UNPACK #-} !Vec3,
    aabbMax :: {-# UNPACK #-} !Vec3
  }
  deriving stock (Eq, Ord, Show)

instance NFData AABB where
  rnf aabbValue = aabbValue `seq` ()

instance JoinSemilattice AABB where
  join leftAabb rightAabb =
    AABB
      { aabbMin = minVec3 (aabbMin leftAabb) (aabbMin rightAabb),
        aabbMax = maxVec3 (aabbMax leftAabb) (aabbMax rightAabb)
      }

mkAabb :: Vec3 -> Vec3 -> Maybe AABB
mkAabb minimumCorner maximumCorner =
  let Vec3 minX minY minZ = minimumCorner
      Vec3 maxX maxY maxZ = maximumCorner
   in if minX <= maxX && minY <= maxY && minZ <= maxZ
        then Just (AABB minimumCorner maximumCorner)
        else Nothing

aabbDimensions :: AABB -> Vec3
aabbDimensions aabbValue =
  subVec3 (aabbMax aabbValue) (aabbMin aabbValue)

aabbCenter :: AABB -> Vec3
aabbCenter aabbValue =
  averageVec3 (aabbMin aabbValue) (aabbMax aabbValue)

aabbHalfExtent :: AABB -> Vec3
aabbHalfExtent aabbValue =
  scaleVec3 0.5 (aabbDimensions aabbValue)

aabbRadius :: AABB -> Double
aabbRadius aabbValue =
  let Vec3 halfX halfY halfZ = aabbHalfExtent aabbValue
   in sqrt (halfX * halfX + halfY * halfY + halfZ * halfZ)

symmetricAabb :: Double -> Double -> Double -> Maybe AABB
symmetricAabb halfX halfY halfZ =
  mkAabb
    (Vec3 (-halfX) (-halfY) (-halfZ))
    (Vec3 halfX halfY halfZ)

unionAabb :: AABB -> AABB -> AABB
unionAabb = join

expandAabb :: Double -> AABB -> Maybe AABB
expandAabb radius aabbValue =
  if 0.0 <= radius
    then
      mkAabb
        (shiftVec3 (-radius) (aabbMin aabbValue))
        (shiftVec3 radius (aabbMax aabbValue))
    else Nothing

intersectAabbMaybe :: AABB -> AABB -> Maybe AABB
intersectAabbMaybe leftAabb rightAabb =
  mkAabb
    (maxVec3 (aabbMin leftAabb) (aabbMin rightAabb))
    (minVec3 (aabbMax leftAabb) (aabbMax rightAabb))

translateAabb :: Vec3 -> AABB -> AABB
translateAabb translationVector aabbValue =
  AABB
    { aabbMin = addVec3 translationVector (aabbMin aabbValue),
      aabbMax = addVec3 translationVector (aabbMax aabbValue)
    }

transformAabb :: AffineTransform -> AABB -> AABB
transformAabb affineTransform aabbValue =
  let radius = aabbRadius aabbValue * affineMaxScale affineTransform
      translatedCenter = addVec3 (atTranslation affineTransform) (aabbCenter aabbValue)
   in AABB
        { aabbMin = shiftVec3 (-radius) translatedCenter,
          aabbMax = shiftVec3 radius translatedCenter
        }

scaleAabb :: Vec3 -> AABB -> AABB
scaleAabb scaleVector aabbValue =
  let center = aabbCenter aabbValue
      halfExtent = aabbHalfExtent aabbValue
      scaledCenter = mulVec3 scaleVector center
      scaledHalfExtent = mulVec3 (mapVec3 abs scaleVector) halfExtent
   in AABB
        { aabbMin = subVec3 scaledCenter scaledHalfExtent,
          aabbMax = addVec3 scaledCenter scaledHalfExtent
        }

shiftVec3 :: Double -> Vec3 -> Vec3
shiftVec3 offset = mapVec3 (+ offset)
