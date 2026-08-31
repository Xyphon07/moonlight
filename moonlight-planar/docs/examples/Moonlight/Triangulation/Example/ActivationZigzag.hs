module Moonlight.Triangulation.Example.ActivationZigzag
  ( ActivationZigzagExampleError (..)
  , squareLoopAcrossDepths
  ) where

import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Vector qualified as Vector
import Moonlight.Homology.Chain (HomologicalDegree (..))
import Moonlight.Triangulation.Types
  ( Point (..)
  , RadiusSquaredError
  , mkRadiusSquared
  )
import Moonlight.Triangulation.Zigzag
  ( ActivationInterval
  , ActivationPoint (..)
  , ActivationSlice (..)
  , ActivationZigzagError
  , ZigzagInterval (..)
  , activationAlphaPersistence
  )

data ActivationZigzagExampleError
  = ActivationRadiusFailed !RadiusSquaredError
  | ActivationPersistenceFailed !(ActivationZigzagError Int String)
  deriving stock (Eq, Show)

-- | Observe a square before its boundary appears, while it carries one loop,
-- and after its two Delaunay faces fill that loop.  The returned interval uses
-- the expanded observation/union stages rather than pretending the slices are
-- a monotone filtration.
squareLoopAcrossDepths :: Either ActivationZigzagExampleError [ActivationInterval Int]
squareLoopAcrossDepths = do
  let depths :: NonEmpty Int
      depths = 0 :| [1, 2]
  thresholds <-
    traverse
      (first ActivationRadiusFailed . mkRadiusSquared . fromIntegral)
      depths
  intervals <-
    first ActivationPersistenceFailed
      ( activationAlphaPersistence
          ( NonEmpty.zipWith
              (\depthValue threshold -> ActivationSlice depthValue threshold squarePoints)
              depths
              thresholds
          )
      )
  pure (filter ((== HomologicalDegree 1) . zigzagIntervalDegree) intervals)

squarePoints :: Vector.Vector (ActivationPoint String)
squarePoints =
  Vector.fromList
    [ ActivationPoint "south-west" (Point (-1) (-1))
    , ActivationPoint "south-east" (Point 1 (-1))
    , ActivationPoint "north-east" (Point 1 1)
    , ActivationPoint "north-west" (Point (-1) 1)
    ]
