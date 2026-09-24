{-# LANGUAGE DataKinds #-}

-- | Mesh construction and coordinate-keyed observations shared by native and algebra laws.
module Moonlight.Planar.MeshFixtures
  ( requirePointBuild
  , pointMeshOf
  , pointKey
  , siteKey
  , faceKeyOf
  , edgeKeys
  ) where

import Data.List ( sort )
import Moonlight.Planar.BulkLoad ( delaunay )
import Moonlight.Planar.Dcel ( faceVertices, undirectedEndpoints, vertexPoint )
import Moonlight.Planar.Handles.HandleDefs ( FaceId, VertexId )
import Moonlight.Planar.Handles.Iterators.FixedIterators ( undirectedEdges )
import Moonlight.Planar.Types (BuildResult, ConstraintMode(..), DelaunayTriangulation, Triangulation, buildTriangulation, unitElementDefaults)
import Moonlight.Planar.Point (Point(..))
import Support ( requireRight )
import qualified Data.Set as Set
import qualified Data.Vector as V


requirePointBuild :: String -> [Point] -> IO (BuildResult 'Unconstrained Point () () ())
requirePointBuild label points = requireRight label (delaunay unitElementDefaults (V.fromList points))

pointMeshOf :: String -> [Point] -> IO (DelaunayTriangulation Point)
pointMeshOf label points = buildTriangulation <$> requirePointBuild label points

pointKey :: Point -> (Double, Double)
pointKey (Point x y) = (x, y)

siteKey :: Triangulation mode vertex directed undirected face -> VertexId -> (Double, Double)
siteKey mesh vertex = pointKey (vertexPoint mesh vertex)

edgeKeys
  :: Triangulation mode vertex directed undirected face
  -> Set.Set ((Double, Double), (Double, Double))
edgeKeys mesh =
  Set.fromList
    [ if left <= right then (left, right) else (right, left)
    | edge <- undirectedEdges mesh
    , let (from, to) = undirectedEndpoints mesh edge
    , let left = siteKey mesh from
    , let right = siteKey mesh to
    ]

faceKeyOf
  :: Triangulation mode vertex directed undirected face
  -> FaceId
  -> [Point]
faceKeyOf triangulation face = sort (map (vertexPoint triangulation) (faceVertices triangulation face))
