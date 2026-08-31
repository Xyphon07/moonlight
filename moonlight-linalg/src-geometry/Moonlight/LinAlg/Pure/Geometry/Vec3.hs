module Moonlight.LinAlg.Pure.Geometry.Vec3
  ( Vec3 (..),
    Axis (..),
    addVec3,
    subVec3,
    scaleVec3,
    negateVec3,
    zipVec3,
    mapVec3,
    mulVec3,
    minVec3,
    maxVec3,
    vec3Zero,
    dotVec3,
    magnitudeVec3,
    normalizeVec3,
    normalizeVec3Safe,
    crossVec3,
    vec3FromList,
    vec3ToList,
    vec3ToTuple,
    maxAbsComponentVec3,
    axisComponent,
    axisVector,
    distanceVec3,
    averageVec3,
    rejectVec3,
    volumeLikeVec3,
  )
where

import Control.DeepSeq (NFData (..))
import Data.Kind (Type)
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as M
import Data.Vector.Unboxed qualified as U
import Foreign.Storable (Storable (..), peekByteOff, pokeByteOff)
import Moonlight.Algebra.Pure.Module (BilinearSpace (..), Module (..), VectorSpace)
import Moonlight.Core (AdditiveGroup (..), AdditiveMonoid (..), Metric (..), MoonlightError (..))
import Prelude (Bounded, Double, Either (..), Enum, Eq, Int, Ord, Read, Show, abs, length, max, min, seq, show, sqrt, (*), (+), (-), (/), (<$>), (<*>), (*>), (<>), (<=))

type Axis :: Type
data Axis
  = AxisX
  | AxisY
  | AxisZ
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

type Vec3 :: Type
data Vec3 = Vec3
  { vecX :: {-# UNPACK #-} !Double,
    vecY :: {-# UNPACK #-} !Double,
    vecZ :: {-# UNPACK #-} !Double
  }
  deriving stock (Eq, Ord, Show, Read)

instance NFData Vec3 where
  rnf vectorValue = vectorValue `seq` ()

instance Storable Vec3 where
  sizeOf _ = 3 * doubleStorageSize
  alignment _ = alignment (0.0 :: Double)
  peek pointerValue =
    Vec3
      <$> peekByteOff pointerValue 0
      <*> peekByteOff pointerValue doubleStorageSize
      <*> peekByteOff pointerValue (2 * doubleStorageSize)
  poke pointerValue (Vec3 xValue yValue zValue) =
    pokeByteOff pointerValue 0 xValue
      *> pokeByteOff pointerValue doubleStorageSize yValue
      *> pokeByteOff pointerValue (2 * doubleStorageSize) zValue

newtype instance U.MVector s Vec3 = MV_Vec3 (U.MVector s (Double, Double, Double))

newtype instance U.Vector Vec3 = V_Vec3 (U.Vector (Double, Double, Double))

instance U.Unbox Vec3

instance M.MVector U.MVector Vec3 where
  {-# INLINE basicLength #-}
  basicLength (MV_Vec3 vectorValue) = M.basicLength vectorValue

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice offset lengthValue (MV_Vec3 vectorValue) =
    MV_Vec3 (M.basicUnsafeSlice offset lengthValue vectorValue)

  {-# INLINE basicOverlaps #-}
  basicOverlaps (MV_Vec3 leftVector) (MV_Vec3 rightVector) =
    M.basicOverlaps leftVector rightVector

  {-# INLINE basicUnsafeNew #-}
  basicUnsafeNew lengthValue =
    MV_Vec3 <$> M.basicUnsafeNew lengthValue

  {-# INLINE basicInitialize #-}
  basicInitialize (MV_Vec3 vectorValue) =
    M.basicInitialize vectorValue

  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeReplicate lengthValue (Vec3 xValue yValue zValue) =
    MV_Vec3 <$> M.basicUnsafeReplicate lengthValue (xValue, yValue, zValue)

  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead (MV_Vec3 vectorValue) indexValue =
    tupleToVec3 <$> M.basicUnsafeRead vectorValue indexValue

  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite (MV_Vec3 vectorValue) indexValue vectorPayload =
    M.basicUnsafeWrite vectorValue indexValue (vec3ToTuple vectorPayload)

  {-# INLINE basicClear #-}
  basicClear (MV_Vec3 vectorValue) =
    M.basicClear vectorValue

  {-# INLINE basicSet #-}
  basicSet (MV_Vec3 vectorValue) vectorPayload =
    M.basicSet vectorValue (vec3ToTuple vectorPayload)

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MV_Vec3 targetVector) (MV_Vec3 sourceVector) =
    M.basicUnsafeCopy targetVector sourceVector

  {-# INLINE basicUnsafeMove #-}
  basicUnsafeMove (MV_Vec3 targetVector) (MV_Vec3 sourceVector) =
    M.basicUnsafeMove targetVector sourceVector

  {-# INLINE basicUnsafeGrow #-}
  basicUnsafeGrow (MV_Vec3 vectorValue) lengthValue =
    MV_Vec3 <$> M.basicUnsafeGrow vectorValue lengthValue

instance G.Vector U.Vector Vec3 where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze (MV_Vec3 vectorValue) =
    V_Vec3 <$> G.basicUnsafeFreeze vectorValue

  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw (V_Vec3 vectorValue) =
    MV_Vec3 <$> G.basicUnsafeThaw vectorValue

  {-# INLINE basicLength #-}
  basicLength (V_Vec3 vectorValue) = G.basicLength vectorValue

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice offset lengthValue (V_Vec3 vectorValue) =
    V_Vec3 (G.basicUnsafeSlice offset lengthValue vectorValue)

  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (V_Vec3 vectorValue) indexValue =
    tupleToVec3 <$> G.basicUnsafeIndexM vectorValue indexValue

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MV_Vec3 targetVector) (V_Vec3 sourceVector) =
    G.basicUnsafeCopy targetVector sourceVector

  {-# INLINE elemseq #-}
  elemseq _ (Vec3 xValue yValue zValue) resultValue =
    G.elemseq doubleVectorWitness xValue (G.elemseq doubleVectorWitness yValue (G.elemseq doubleVectorWitness zValue resultValue))

instance AdditiveMonoid Vec3 where
  zero = Vec3 0.0 0.0 0.0
  add (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 + x2) (y1 + y2) (z1 + z2)

instance AdditiveGroup Vec3 where
  neg (Vec3 x y z) = Vec3 (-x) (-y) (-z)

instance Module Double Vec3 where
  scale s (Vec3 x y z) = Vec3 (s * x) (s * y) (s * z)

instance VectorSpace Double Vec3

instance BilinearSpace Double Vec3 where
  bilinearForm (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

instance Metric Vec3 where
  type Magnitude Vec3 = Double
  magnitude v = sqrt (bilinearForm v v)

vec3Zero :: Vec3
vec3Zero = zero

addVec3 :: Vec3 -> Vec3 -> Vec3
addVec3 = add

subVec3 :: Vec3 -> Vec3 -> Vec3
subVec3 = sub

negateVec3 :: Vec3 -> Vec3
negateVec3 = neg

scaleVec3 :: Double -> Vec3 -> Vec3
scaleVec3 = scale

zipVec3 :: (Double -> Double -> Double) -> Vec3 -> Vec3 -> Vec3
zipVec3 combine (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
  Vec3 (combine x1 x2) (combine y1 y2) (combine z1 z2)

mapVec3 :: (Double -> Double) -> Vec3 -> Vec3
mapVec3 transform (Vec3 xValue yValue zValue) =
  Vec3 (transform xValue) (transform yValue) (transform zValue)

mulVec3 :: Vec3 -> Vec3 -> Vec3
mulVec3 = zipVec3 (*)

minVec3 :: Vec3 -> Vec3 -> Vec3
minVec3 = zipVec3 min

maxVec3 :: Vec3 -> Vec3 -> Vec3
maxVec3 = zipVec3 max

dotVec3 :: Vec3 -> Vec3 -> Double
dotVec3 = bilinearForm

magnitudeVec3 :: Vec3 -> Double
magnitudeVec3 = magnitude

normalizeVec3 :: Vec3 -> Either MoonlightError Vec3
normalizeVec3 v =
  let m = magnitudeVec3 v
   in if m <= 1.0e-12
        then Left (InvariantViolation "member direction cannot be normalized from a zero-length vector")
        else Right (scaleVec3 (1.0 / m) v)

normalizeVec3Safe :: Vec3 -> Vec3
normalizeVec3Safe v =
  let m = magnitudeVec3 v
   in if m <= 1.0e-12 then vec3Zero else scaleVec3 (1.0 / m) v

crossVec3 :: Vec3 -> Vec3 -> Vec3
crossVec3 (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
  Vec3
    (y1 * z2 - z1 * y2)
    (z1 * x2 - x1 * z2)
    (x1 * y2 - y1 * x2)

vec3FromList :: [Double] -> Either MoonlightError Vec3
vec3FromList xs =
  case xs of
    [x, y, z] -> Right (Vec3 x y z)
    _ ->
      Left
        ( InvariantViolation
            ( "Vec3 requires exactly 3 entries, received "
                <> show (length xs)
            )
        )

vec3ToList :: Vec3 -> [Double]
vec3ToList (Vec3 xValue yValue zValue) = [xValue, yValue, zValue]

vec3ToTuple :: Vec3 -> (Double, Double, Double)
vec3ToTuple (Vec3 xValue yValue zValue) = (xValue, yValue, zValue)

tupleToVec3 :: (Double, Double, Double) -> Vec3
tupleToVec3 (xValue, yValue, zValue) = Vec3 xValue yValue zValue

doubleStorageSize :: Int
doubleStorageSize = sizeOf (0.0 :: Double)

doubleVectorWitness :: U.Vector Double
doubleVectorWitness = U.empty

maxAbsComponentVec3 :: Vec3 -> Double
maxAbsComponentVec3 (Vec3 xValue yValue zValue) =
  max (abs xValue) (max (abs yValue) (abs zValue))

axisComponent :: Axis -> Vec3 -> Double
axisComponent AxisX (Vec3 x _ _) = x
axisComponent AxisY (Vec3 _ y _) = y
axisComponent AxisZ (Vec3 _ _ z) = z

axisVector :: Axis -> Double -> Vec3
axisVector AxisX m = Vec3 m 0.0 0.0
axisVector AxisY m = Vec3 0.0 m 0.0
axisVector AxisZ m = Vec3 0.0 0.0 m

distanceVec3 :: Vec3 -> Vec3 -> Double
distanceVec3 leftPosition rightPosition =
  magnitudeVec3 (subVec3 leftPosition rightPosition)

averageVec3 :: Vec3 -> Vec3 -> Vec3
averageVec3 leftValue rightValue =
  scaleVec3 0.5 (addVec3 leftValue rightValue)

rejectVec3 :: Vec3 -> Vec3 -> Vec3
rejectVec3 onto v =
  subVec3 v (scaleVec3 (dotVec3 onto v) onto)

volumeLikeVec3 :: Vec3 -> Double
volumeLikeVec3 (Vec3 x y z) = abs (x * y * z)
