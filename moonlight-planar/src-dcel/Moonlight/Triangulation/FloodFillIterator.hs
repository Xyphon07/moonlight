-- | Shape queries, face descent, and boundary gluing over an immutable mesh.
module Moonlight.Triangulation.FloodFillIterator
  ( DistanceMetric (..)
  , CircleMetric
  , CircleMetricError (..)
  , RectangleMetric
  , RectangleMetricError (..)
  , circleMetric
  , rectangleMetric
  , edgesInShape
  , verticesInShape
  , edgesInCircle
  , verticesInCircle
  , edgesInRectangle
  , verticesInRectangle
  , floodFillFaces
  , outerFaceFloodFill
  , facesAtEvenBarrierDepth
  , FaceComponent
  , faceComponentFaces
  , BoundaryOrientation (..)
  , BoundaryLoop
  , boundaryLoopOrientation
  , boundaryLoopResidentVertices
  , boundaryLoopVertices
  , RegionBoundary
  , regionBoundaryOuterLoop
  , regionBoundaryHoleLoops
  , BoundaryObstruction (..)
  , faceComponents
  , faceComponentsBy
  , labelledRegionBoundaries
  , componentBoundaryLoops
  , componentBoundary
  , componentBoundaryForFaces
  ) where

import Moonlight.Triangulation.Internal.FloodFillIterator
