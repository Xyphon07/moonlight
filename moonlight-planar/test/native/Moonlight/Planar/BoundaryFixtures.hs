-- | Resident-boundary observations shared by region and alpha laws.
module Moonlight.Planar.BoundaryFixtures
  ( requireLabelledComponent
  , requireComponentBoundary
  , assertLoopWinding
  , assertBoundaryShape
  ) where

import Data.Foldable ( traverse_ )
import Data.List.NonEmpty ( NonEmpty(..) )
import Moonlight.Planar.Dcel ( vertexPoint )
import Moonlight.Planar.FloodFillIterator ( componentBoundary, faceComponents,
  BoundaryLoop(boundaryLoopVertices, boundaryLoopOrientation),
  BoundaryOrientation(BoundaryClockwise, BoundaryCounterClockwise), FaceComponent,
  RegionBoundary(..) )
import Moonlight.Planar.Internal.HandleDefs ( FaceId )
import Moonlight.Planar.Point (Point(Point))
import Moonlight.Planar.Types (Triangulation)
import Support ( assertEqual, requireRight )


requireLabelledComponent
  :: (Eq label, Show label)
  => String
  -> label
  -> [(label, FaceComponent)]
  -> IO FaceComponent
requireLabelledComponent label expected components =
  case [component | (actual, component) <- components, actual == expected] of
    [component] -> pure component
    matches ->
      fail
        ( label
            <> ": expected one component for label "
            <> show expected
            <> ", got "
            <> show (length matches)
        )

requireComponentBoundary
  :: (Eq label, Show label)
  => String
  -> label
  -> Triangulation mode vertex directed undirected face
  -> (FaceId -> label)
  -> IO RegionBoundary
requireComponentBoundary label expected triangulation labelFace = do
  component <-
    requireLabelledComponent
      (label <> " component")
      expected
      (faceComponents triangulation labelFace)
  requireRight (label <> " boundary") (componentBoundary triangulation component)

assertLoopWinding
  :: String
  -> Ordering
  -> Triangulation mode vertex directed undirected face
  -> BoundaryLoop
  -> IO ()
assertLoopWinding label expected triangulation loop =
  let first :| remaining = fmap (vertexPoint triangulation) (boundaryLoopVertices loop)
      points = first : remaining
      twiceArea =
        sum
          ( zipWith
              (\(Point ax ay) (Point bx by) -> ax * by - ay * bx)
              points
              (remaining <> [first])
          )
   in assertEqual label expected (compare twiceArea 0)

assertBoundaryShape
  :: String
  -> Triangulation mode vertex directed undirected face
  -> Int
  -> [Int]
  -> RegionBoundary
  -> IO ()
assertBoundaryShape label triangulation outerVertexCount holeVertexCounts boundary = do
  assertEqual
    (label <> " outer orientation")
    BoundaryCounterClockwise
    (boundaryLoopOrientation (regionBoundaryOuterLoop boundary))
  assertEqual (label <> " outer vertex count") outerVertexCount
    (length (boundaryLoopVertices (regionBoundaryOuterLoop boundary)))
  assertLoopWinding (label <> " outer winding") GT triangulation
    (regionBoundaryOuterLoop boundary)
  let holes = regionBoundaryHoleLoops boundary
  assertEqual (label <> " hole count") (length holeVertexCounts) (length holes)
  traverse_
    (\(vertexCount, hole) -> do
       assertEqual
         (label <> " hole orientation")
         BoundaryClockwise
         (boundaryLoopOrientation hole)
       assertEqual (label <> " hole vertex count") vertexCount
         (length (boundaryLoopVertices hole))
       assertLoopWinding (label <> " hole winding") LT triangulation hole)
    (zip holeVertexCounts holes)
