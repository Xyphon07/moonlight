module Moonlight.Planar.Internal.FaceProbe
  ( BoundaryProbe (..)
  , probeBoundary
  ) where

import Moonlight.Planar.Internal.Predicates (onClosedSegment, orient2d)
import Moonlight.Planar.Point (Point)

-- | Classification of one oriented boundary of a triangular face. A
-- 'BoundaryCrossing' carries the half-edge whose incident face is the
-- destination of the walk; consumers must not reverse it again.
data BoundaryProbe edge vertex
  = BoundaryClear
  | BoundaryOnVertex !vertex
  | BoundaryOnEdge !edge
  | BoundaryCrossing !edge
  deriving stock (Eq, Ord, Show)

probeBoundary
  :: (edge -> edge)
  -> Point
  -> edge
  -> vertex
  -> Point
  -> vertex
  -> Point
  -> BoundaryProbe edge vertex
probeBoundary reverseBoundary query edge fromVertex from toVertex to
  | query == from = BoundaryOnVertex fromVertex
  | query == to = BoundaryOnVertex toVertex
  | otherwise =
      case orient2d from to query of
        EQ
          | onClosedSegment from to query -> BoundaryOnEdge edge
          | otherwise -> BoundaryClear
        LT -> BoundaryCrossing (reverseBoundary edge)
        GT -> BoundaryClear
{-# INLINE probeBoundary #-}
