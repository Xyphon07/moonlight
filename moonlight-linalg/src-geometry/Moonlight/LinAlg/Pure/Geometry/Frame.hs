{-# LANGUAGE LambdaCase #-}

module Moonlight.LinAlg.Pure.Geometry.Frame
  ( OrthonormalFrame,
    identityOrthonormalFrame,
    orthonormalFrameColumns,
    orthonormalFrameFromColumns,
    orthonormalFrameFromMatrixEntries,
  )
where

import Control.DeepSeq (NFData (..))
import Data.Kind (Type)
import Moonlight.LinAlg.Pure.Geometry.Vec3
  ( Vec3 (..),
    dotVec3,
    magnitudeVec3,
  )
import Prelude (Bool, Double, Eq, Maybe (..), Read, Show, abs, all, seq, (-), (&&), (<=))

type OrthonormalFrame :: Type
data OrthonormalFrame
  = OrthonormalFrame
      {-# UNPACK #-} !Vec3
      {-# UNPACK #-} !Vec3
      {-# UNPACK #-} !Vec3
  deriving stock (Eq, Show, Read)

instance NFData OrthonormalFrame where
  rnf frameValue = frameValue `seq` ()

identityOrthonormalFrame :: OrthonormalFrame
identityOrthonormalFrame =
  OrthonormalFrame
    (Vec3 1.0 0.0 0.0)
    (Vec3 0.0 1.0 0.0)
    (Vec3 0.0 0.0 1.0)

orthonormalFrameColumns :: OrthonormalFrame -> (Vec3, Vec3, Vec3)
orthonormalFrameColumns (OrthonormalFrame axis1 axis2 axis3) = (axis1, axis2, axis3)

orthonormalFrameFromColumns :: [Vec3] -> Maybe OrthonormalFrame
orthonormalFrameFromColumns = \case
  axis1 : axis2 : axis3 : []
    | isOrthonormalTriple axis1 axis2 axis3 ->
        Just (OrthonormalFrame axis1 axis2 axis3)
  _ ->
    Nothing

orthonormalFrameFromMatrixEntries :: [Double] -> Maybe OrthonormalFrame
orthonormalFrameFromMatrixEntries = \case
  [ r1c1,
    r1c2,
    r1c3,
    r2c1,
    r2c2,
    r2c3,
    r3c1,
    r3c2,
    r3c3
    ] ->
      orthonormalFrameFromColumns
        [ Vec3 r1c1 r2c1 r3c1,
          Vec3 r1c2 r2c2 r3c2,
          Vec3 r1c3 r2c3 r3c3
        ]
  _ ->
    Nothing

isOrthonormalTriple :: Vec3 -> Vec3 -> Vec3 -> Bool
isOrthonormalTriple axis1 axis2 axis3 =
  all isUnitVector [axis1, axis2, axis3]
    && all
      orthogonalWithinTolerance
      [ (axis1, axis2),
        (axis1, axis3),
        (axis2, axis3)
      ]

isUnitVector :: Vec3 -> Bool
isUnitVector axisValue =
  abs (magnitudeVec3 axisValue - 1.0) <= frameTolerance

orthogonalWithinTolerance :: (Vec3, Vec3) -> Bool
orthogonalWithinTolerance (leftAxis, rightAxis) =
  abs (dotVec3 leftAxis rightAxis) <= frameTolerance

frameTolerance :: Double
frameTolerance = 1.0e-10
