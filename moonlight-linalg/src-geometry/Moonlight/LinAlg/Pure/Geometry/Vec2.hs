module Moonlight.LinAlg.Pure.Geometry.Vec2
  ( Vec2 (..),
    Axis2 (..),
    addVec2,
    subVec2,
    scaleVec2,
    negateVec2,
    zipVec2,
    mapVec2,
    mulVec2,
    minVec2,
    maxVec2,
    vec2Zero,
    dotVec2,
    magnitudeVec2,
    normalizeVec2,
    normalizeVec2Safe,
    normalizeVec2Or,
    vec2FromList,
    vec2ToList,
    vec2ToTuple,
    maxAbsComponentVec2,
    axis2Component,
    axis2Vector,
    distanceVec2,
    averageVec2,
    rejectVec2,
    areaLikeVec2,
    perpVec2,
    crossZVec2,
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

type Axis2 :: Type
data Axis2
  = Axis2X
  | Axis2Y
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

type Vec2 :: Type
data Vec2 = Vec2
  { vec2X :: {-# UNPACK #-} !Double,
    vec2Y :: {-# UNPACK #-} !Double
  }
  deriving stock (Eq, Ord, Show, Read)

instance NFData Vec2 where
  rnf vectorValue = vectorValue `seq` ()

instance Storable Vec2 where
  sizeOf _ = 2 * doubleStorageSize
  alignment _ = alignment (0.0 :: Double)
  peek pointerValue =
    Vec2
      <$> peekByteOff pointerValue 0
      <*> peekByteOff pointerValue doubleStorageSize
  poke pointerValue (Vec2 xValue yValue) =
    pokeByteOff pointerValue 0 xValue
      *> pokeByteOff pointerValue doubleStorageSize yValue

newtype instance U.MVector s Vec2 = MV_Vec2 (U.MVector s (Double, Double))

newtype instance U.Vector Vec2 = V_Vec2 (U.Vector (Double, Double))

instance U.Unbox Vec2

instance M.MVector U.MVector Vec2 where
  {-# INLINE basicLength #-}
  basicLength (MV_Vec2 vectorValue) = M.basicLength vectorValue

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice offset lengthValue (MV_Vec2 vectorValue) =
    MV_Vec2 (M.basicUnsafeSlice offset lengthValue vectorValue)

  {-# INLINE basicOverlaps #-}
  basicOverlaps (MV_Vec2 leftVector) (MV_Vec2 rightVector) =
    M.basicOverlaps leftVector rightVector

  {-# INLINE basicUnsafeNew #-}
  basicUnsafeNew lengthValue =
    MV_Vec2 <$> M.basicUnsafeNew lengthValue

  {-# INLINE basicInitialize #-}
  basicInitialize (MV_Vec2 vectorValue) =
    M.basicInitialize vectorValue

  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeReplicate lengthValue (Vec2 xValue yValue) =
    MV_Vec2 <$> M.basicUnsafeReplicate lengthValue (xValue, yValue)

  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead (MV_Vec2 vectorValue) indexValue =
    tupleToVec2 <$> M.basicUnsafeRead vectorValue indexValue

  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite (MV_Vec2 vectorValue) indexValue vectorPayload =
    M.basicUnsafeWrite vectorValue indexValue (vec2ToTuple vectorPayload)

  {-# INLINE basicClear #-}
  basicClear (MV_Vec2 vectorValue) =
    M.basicClear vectorValue

  {-# INLINE basicSet #-}
  basicSet (MV_Vec2 vectorValue) vectorPayload =
    M.basicSet vectorValue (vec2ToTuple vectorPayload)

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MV_Vec2 targetVector) (MV_Vec2 sourceVector) =
    M.basicUnsafeCopy targetVector sourceVector

  {-# INLINE basicUnsafeMove #-}
  basicUnsafeMove (MV_Vec2 targetVector) (MV_Vec2 sourceVector) =
    M.basicUnsafeMove targetVector sourceVector

  {-# INLINE basicUnsafeGrow #-}
  basicUnsafeGrow (MV_Vec2 vectorValue) lengthValue =
    MV_Vec2 <$> M.basicUnsafeGrow vectorValue lengthValue

instance G.Vector U.Vector Vec2 where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze (MV_Vec2 vectorValue) =
    V_Vec2 <$> G.basicUnsafeFreeze vectorValue

  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw (V_Vec2 vectorValue) =
    MV_Vec2 <$> G.basicUnsafeThaw vectorValue

  {-# INLINE basicLength #-}
  basicLength (V_Vec2 vectorValue) = G.basicLength vectorValue

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice offset lengthValue (V_Vec2 vectorValue) =
    V_Vec2 (G.basicUnsafeSlice offset lengthValue vectorValue)

  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (V_Vec2 vectorValue) indexValue =
    tupleToVec2 <$> G.basicUnsafeIndexM vectorValue indexValue

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MV_Vec2 targetVector) (V_Vec2 sourceVector) =
    G.basicUnsafeCopy targetVector sourceVector

  {-# INLINE elemseq #-}
  elemseq _ (Vec2 xValue yValue) resultValue =
    G.elemseq doubleVectorWitness xValue (G.elemseq doubleVectorWitness yValue resultValue)

instance AdditiveMonoid Vec2 where
  zero = Vec2 0.0 0.0
  add (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1 + x2) (y1 + y2)

instance AdditiveGroup Vec2 where
  neg (Vec2 xValue yValue) = Vec2 (-xValue) (-yValue)

instance Module Double Vec2 where
  scale scaleValue (Vec2 xValue yValue) = Vec2 (scaleValue * xValue) (scaleValue * yValue)

instance VectorSpace Double Vec2

instance BilinearSpace Double Vec2 where
  bilinearForm (Vec2 x1 y1) (Vec2 x2 y2) = x1 * x2 + y1 * y2

instance Metric Vec2 where
  type Magnitude Vec2 = Double
  magnitude vectorValue = sqrt (bilinearForm vectorValue vectorValue)

vec2Zero :: Vec2
vec2Zero = zero

addVec2 :: Vec2 -> Vec2 -> Vec2
addVec2 = add

subVec2 :: Vec2 -> Vec2 -> Vec2
subVec2 = sub

negateVec2 :: Vec2 -> Vec2
negateVec2 = neg

scaleVec2 :: Double -> Vec2 -> Vec2
scaleVec2 = scale

zipVec2 :: (Double -> Double -> Double) -> Vec2 -> Vec2 -> Vec2
zipVec2 combine (Vec2 x1 y1) (Vec2 x2 y2) =
  Vec2 (combine x1 x2) (combine y1 y2)

mapVec2 :: (Double -> Double) -> Vec2 -> Vec2
mapVec2 transform (Vec2 xValue yValue) =
  Vec2 (transform xValue) (transform yValue)

mulVec2 :: Vec2 -> Vec2 -> Vec2
mulVec2 = zipVec2 (*)

minVec2 :: Vec2 -> Vec2 -> Vec2
minVec2 = zipVec2 min

maxVec2 :: Vec2 -> Vec2 -> Vec2
maxVec2 = zipVec2 max

dotVec2 :: Vec2 -> Vec2 -> Double
dotVec2 = bilinearForm

magnitudeVec2 :: Vec2 -> Double
magnitudeVec2 = magnitude

normalizeVec2 :: Vec2 -> Either MoonlightError Vec2
normalizeVec2 vectorValue =
  let vectorMagnitude = magnitudeVec2 vectorValue
   in if vectorMagnitude <= 1.0e-12
        then Left (InvariantViolation "member direction cannot be normalized from a zero-length vector")
        else Right (scaleVec2 (1.0 / vectorMagnitude) vectorValue)

normalizeVec2Safe :: Vec2 -> Vec2
normalizeVec2Safe vectorValue =
  let vectorMagnitude = magnitudeVec2 vectorValue
   in if vectorMagnitude <= 1.0e-12 then vec2Zero else scaleVec2 (1.0 / vectorMagnitude) vectorValue

normalizeVec2Or :: Vec2 -> Vec2 -> Vec2
normalizeVec2Or fallback vectorValue =
  let vectorMagnitude = magnitudeVec2 vectorValue
   in if vectorMagnitude <= 1.0e-12 then fallback else scaleVec2 (1.0 / vectorMagnitude) vectorValue

vec2FromList :: [Double] -> Either MoonlightError Vec2
vec2FromList values =
  case values of
    [xValue, yValue] -> Right (Vec2 xValue yValue)
    _ ->
      Left
        ( InvariantViolation
            ( "Vec2 requires exactly 2 entries, received "
                <> show (length values)
            )
        )

vec2ToList :: Vec2 -> [Double]
vec2ToList (Vec2 xValue yValue) = [xValue, yValue]

vec2ToTuple :: Vec2 -> (Double, Double)
vec2ToTuple (Vec2 xValue yValue) = (xValue, yValue)

tupleToVec2 :: (Double, Double) -> Vec2
tupleToVec2 (xValue, yValue) = Vec2 xValue yValue

doubleStorageSize :: Int
doubleStorageSize = sizeOf (0.0 :: Double)

doubleVectorWitness :: U.Vector Double
doubleVectorWitness = U.empty

maxAbsComponentVec2 :: Vec2 -> Double
maxAbsComponentVec2 (Vec2 xValue yValue) =
  max (abs xValue) (abs yValue)

axis2Component :: Axis2 -> Vec2 -> Double
axis2Component Axis2X (Vec2 xValue _) = xValue
axis2Component Axis2Y (Vec2 _ yValue) = yValue

axis2Vector :: Axis2 -> Double -> Vec2
axis2Vector Axis2X magnitudeValue = Vec2 magnitudeValue 0.0
axis2Vector Axis2Y magnitudeValue = Vec2 0.0 magnitudeValue

distanceVec2 :: Vec2 -> Vec2 -> Double
distanceVec2 leftPosition rightPosition =
  magnitudeVec2 (subVec2 leftPosition rightPosition)

averageVec2 :: Vec2 -> Vec2 -> Vec2
averageVec2 leftValue rightValue =
  scaleVec2 0.5 (addVec2 leftValue rightValue)

rejectVec2 :: Vec2 -> Vec2 -> Vec2
rejectVec2 onto vectorValue =
  subVec2 vectorValue (scaleVec2 (dotVec2 onto vectorValue) onto)

areaLikeVec2 :: Vec2 -> Double
areaLikeVec2 (Vec2 xValue yValue) = abs (xValue * yValue)

perpVec2 :: Vec2 -> Vec2
perpVec2 (Vec2 xValue yValue) = Vec2 (-yValue) xValue

crossZVec2 :: Vec2 -> Vec2 -> Double
crossZVec2 (Vec2 x1 y1) (Vec2 x2 y2) = x1 * y2 - y1 * x2
