{-# LANGUAGE BangPatterns #-}

-- | Walk location over the finite mesh; the hinted form takes a starting point
-- and reports the steps it spent.
module Moonlight.Triangulation.PointLocation
  ( locatePoint
  , locatePointWithHint
  ) where

import Data.List (find)
import Moonlight.Triangulation.Dcel
import Moonlight.Triangulation.Internal.HandleDefs
import Moonlight.Triangulation.Handles.Iterators.FixedIterators
import Moonlight.Triangulation.Internal.FaceProbe
import Moonlight.Triangulation.Internal.Predicates
import Moonlight.Triangulation.Types

-- | Locate an admitted point without an initial topology hint.
locatePoint :: Triangulation mode vertex directed undirected face -> QueryPoint -> Location
locatePoint triangulation query = fst (locatePointWithHint triangulation Nothing query)

-- | Locate an admitted point from an optional starting cell and report the
-- descent work.
locatePointWithHint :: Triangulation mode vertex directed undirected face -> Maybe LocationHint -> QueryPoint -> (Location, LocationStats)
locatePointWithHint triangulation hint queryPoint
  | numVertices triangulation == 0 = (EmptyTriangulation, emptyLocationStats)
  | numInnerFaces triangulation == 0 = (locateDegenerate triangulation query, emptyLocationStats)
  | otherwise = walk budget 0 startFace
 where
  !query = queryPointValue queryPoint
  !budget = max 8 (numFaces triangulation + numUndirectedEdges triangulation + 4)
  !startFace = chooseStart triangulation hint

  walk !remaining !steps !face
    | remaining <= 0 = scanAll steps
    | face == outerFace =
        (outsideLocation triangulation query, LocationStats steps False)
    | otherwise =
        case probeFace triangulation face query of
          FaceHit location -> (location, LocationStats (steps + 1) False)
          FaceCross edge ->
            let adjacent = incidentFace triangulation edge
             in if adjacent == outerFace
                  then (OutsideConvexHull (Just edge), LocationStats (steps + 1) False)
                  else walk (remaining - 1) (steps + 1) adjacent
          FaceMiss -> scanAll (steps + 1)

  scanAll !steps =
    case findHit (innerFaces triangulation) of
      Just location -> (location, LocationStats steps True)
      Nothing -> (outsideLocation triangulation query, LocationStats steps True)

  findHit [] = Nothing
  findHit (face : remaining) =
    case probeFace triangulation face query of
      FaceHit location -> Just location
      _ -> findHit remaining

data FaceProbe = FaceHit !Location | FaceCross !DirectedEdgeId | FaceMiss

probeFace :: Triangulation mode vertex directed undirected face -> FaceId -> Point -> FaceProbe
probeFace triangulation face query =
  case innerFaceDirectedEdges triangulation face of
    Nothing -> FaceMiss
    Just (e0, e1, e2) -> classify [e0, e1, e2] Nothing
 where
  classify [] Nothing = FaceHit (InFace face)
  classify [] (Just edge) = FaceCross edge
  classify (edge : remaining) crossing =
    let fromVertex = origin triangulation edge
        toVertex = destination triangulation edge
        from = vertexPoint triangulation fromVertex
        to = vertexPoint triangulation toVertex
     in case probeBoundary reverseEdge query edge fromVertex from toVertex to of
          BoundaryClear -> classify remaining crossing
          BoundaryOnVertex vertex -> FaceHit (OnVertex vertex)
          BoundaryOnEdge boundary -> FaceHit (OnEdge boundary)
          BoundaryCrossing boundary -> classify remaining (Just boundary)

locateDegenerate :: Triangulation mode vertex directed undirected face -> Point -> Location
locateDegenerate triangulation query =
  case find ((== query) . vertexPoint triangulation) (vertices triangulation) of
    Just vertex -> OnVertex vertex
    Nothing ->
      case find contains (undirectedEdges triangulation) of
        Just edge -> OnEdge (normalizedDirected edge)
        Nothing ->
          case undirectedEdges triangulation of
            [] -> OutsideConvexHull Nothing
            edge : _ ->
              let forward = normalizedDirected edge
                  from = vertexPoint triangulation (origin triangulation forward)
                  to = vertexPoint triangulation (destination triangulation forward)
                  directed = case orient2d from to query of
                    GT -> forward
                    LT -> reverseEdge forward
                    EQ -> forward
               in OutsideConvexHull (Just directed)
 where
  contains edge =
    let forward = normalizedDirected edge
     in onClosedSegment
          (vertexPoint triangulation (origin triangulation forward))
          (vertexPoint triangulation (destination triangulation forward))
          query

outsideLocation :: Triangulation mode vertex directed undirected face -> Point -> Location
outsideLocation triangulation query =
  OutsideConvexHull (find visible (faceDirectedEdges triangulation outerFace))
 where
  visible edge =
    orient2d
      (vertexPoint triangulation (origin triangulation edge))
      (vertexPoint triangulation (destination triangulation edge))
      query
      /= LT

chooseStart :: Triangulation mode vertex directed undirected face -> Maybe LocationHint -> FaceId
chooseStart triangulation hint =
  case hint of
    Just (FaceHint face)
      | face /= outerFace && faceIdIndex face < numFaces triangulation -> face
    Just (VertexHint vertex@(VertexId index))
      | toInteger index < toInteger (numVertices triangulation) ->
          case [face | edge <- vertexOutgoingEdges triangulation vertex, let face = incidentFace triangulation edge, face /= outerFace] of
            face : _ -> face
            [] -> FaceId 1
      | otherwise -> FaceId 1
    _ -> FaceId 1
