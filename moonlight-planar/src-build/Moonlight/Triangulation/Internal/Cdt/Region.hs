{-# LANGUAGE DataKinds #-}

-- | The two-sided reading of the constrained domain: which faces lie outside
-- the protected boundary and which lie within it.
module Moonlight.Triangulation.Internal.Cdt.Region
  ( outerRegionFaces
  , boundedRegionFaces
  ) where

import qualified Data.IntSet as IntSet
import qualified Moonlight.Triangulation.Dcel as Dcel
import Moonlight.Triangulation.FloodFillIterator (facesAtEvenBarrierDepth)
import Moonlight.Triangulation.Internal.HandleDefs
import Moonlight.Triangulation.Handles.Iterators.FixedIterators (innerFaces)
import Moonlight.Triangulation.Internal.Representation
import Moonlight.Triangulation.Internal.Types

-- | Inner faces at even minimum constraint-crossing depth from the outer face.
outerRegionFaces
  :: Triangulation 'Constrained vertex directed undirected face
  -> [FaceId]
-- Crossing depth, not bare reachability. A face lies outside the constrained
-- domain when the fewest constraints separating it from the outer face is even,
-- so the hole of an annulus (two crossings) is outside exactly as its exterior
-- (none) is. Reachability alone is the depth-zero layer and calls that hole
-- domain, which would mesh it.
outerRegionFaces triangulation =
  facesAtEvenBarrierDepth triangulation (Dcel.isConstraintEdge triangulation)

-- | Inner faces enclosed at odd constraint-crossing depth.
boundedRegionFaces
  :: Triangulation 'Constrained vertex directed undirected face
  -> [FaceId]
boundedRegionFaces triangulation =
  [ face
  | face <- innerFaces triangulation
  , not (IntSet.member (faceIdIndex face) outside)
  ]
 where
  outside = IntSet.fromList (fmap faceIdIndex (outerRegionFaces triangulation))
