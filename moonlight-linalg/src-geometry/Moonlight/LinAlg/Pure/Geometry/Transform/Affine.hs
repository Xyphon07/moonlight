module Moonlight.LinAlg.Pure.Geometry.Transform.Affine
  ( AffineTransform (..),
    TransformMetricEffect (..),
    affineMaxScale,
    isIdentityScale,
    affineMetricEffect,
  )
where

import Control.DeepSeq (NFData (..))
import Data.Kind (Type)
import Moonlight.LinAlg.Pure.Geometry.Frame (OrthonormalFrame, orthonormalFrameColumns)
import Moonlight.LinAlg.Pure.Geometry.Vec3 (Vec3 (..), maxAbsComponentVec3, vec3ToTuple)
import Prelude (Bool, Double, Eq, Ord, Read, Show, abs, compare, otherwise, seq, (.), (==), (>), (&&))

type AffineTransform :: Type
data AffineTransform = AffineTransform
  { atTranslation :: {-# UNPACK #-} !Vec3,
    atRotationFrame :: {-# UNPACK #-} !OrthonormalFrame,
    atScale :: {-# UNPACK #-} !Vec3
  }
  deriving stock (Eq, Show)

instance NFData AffineTransform where
  rnf affineTransform = affineTransform `seq` ()

type TransformMetricEffect :: Type
data TransformMetricEffect
  = MetricIsometry
  | UniformMetricScale !Double
  | AnisotropicMetricDistortion
  deriving stock (Eq, Ord, Show, Read)

instance Ord AffineTransform where
  compare leftTransform rightTransform =
    compare
      ( vec3ToTuple (atTranslation leftTransform),
        frameTuple (atRotationFrame leftTransform),
        vec3ToTuple (atScale leftTransform)
      )
      ( vec3ToTuple (atTranslation rightTransform),
        frameTuple (atRotationFrame rightTransform),
        vec3ToTuple (atScale rightTransform)
      )

affineMaxScale :: AffineTransform -> Double
affineMaxScale = maxAbsComponentVec3 . atScale

isIdentityScale :: Vec3 -> Bool
isIdentityScale scaleVector = scaleVector == Vec3 1.0 1.0 1.0

affineMetricEffect :: AffineTransform -> TransformMetricEffect
affineMetricEffect affine =
  case atScale affine of
    Vec3 sx sy sz
      | ax == 1.0 && ay == 1.0 && az == 1.0 -> MetricIsometry
      | ax == ay && ay == az && ax > 0.0 -> UniformMetricScale ax
      | otherwise -> AnisotropicMetricDistortion
      where
        ax = abs sx
        ay = abs sy
        az = abs sz

frameTuple ::
  OrthonormalFrame ->
  ((Double, Double, Double), (Double, Double, Double), (Double, Double, Double))
frameTuple frameValue =
  case orthonormalFrameColumns frameValue of
    (axis1, axis2, axis3) ->
      (vec3ToTuple axis1, vec3ToTuple axis2, vec3ToTuple axis3)
