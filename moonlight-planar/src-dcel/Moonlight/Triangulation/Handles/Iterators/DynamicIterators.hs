{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}

-- | Whole-mesh traversal through topology-bound dynamic handles.
module Moonlight.Triangulation.Handles.Iterators.DynamicIterators
  ( vertexHandles
  , directedEdgeHandles
  , undirectedEdgeHandles
  , allFaceHandles
  , innerFaceHandles
  , hullEdgeHandles
  , hullVertexHandles
  , foldVertexHandles'
  , foldDirectedEdgeHandles'
  , foldUndirectedEdgeHandles'
  , foldAllFaceHandles'
  , foldInnerFaceHandles'
  , foldHullEdgeHandles'
  , foldHullVertexHandles'
  ) where

import Moonlight.Triangulation.Handles.Dynamic
import Moonlight.Triangulation.Handles.Iterators.FixedIterators qualified as Fixed
import Moonlight.Triangulation.Handles.Iterators.HullIterator qualified as Hull
import Moonlight.Triangulation.Types

-- | Dynamic handles in fixed-index order. The list spine is lazy; use the
-- strict folds below for allocation-free traversal in hot paths.
vertexHandles
  :: Triangulation mode vertex directed undirected face
  -> [VertexHandle mode vertex directed undirected face]
vertexHandles triangulation =
  mapValid (vertexHandle triangulation) (Fixed.vertices triangulation)

-- | Every directed-edge handle in fixed-index order.
directedEdgeHandles
  :: Triangulation mode vertex directed undirected face
  -> [DirectedEdgeHandle mode vertex directed undirected face]
directedEdgeHandles triangulation =
  mapValid (directedEdgeHandle triangulation) (Fixed.directedEdges triangulation)

-- | Every undirected-edge handle in fixed-index order.
undirectedEdgeHandles
  :: Triangulation mode vertex directed undirected face
  -> [UndirectedEdgeHandle mode vertex directed undirected face]
undirectedEdgeHandles triangulation =
  mapValid (undirectedEdgeHandle triangulation) (Fixed.undirectedEdges triangulation)

-- | Every face handle, including the outer face.
allFaceHandles
  :: Triangulation mode vertex directed undirected face
  -> [FaceHandle PossiblyOuterTag mode vertex directed undirected face]
allFaceHandles triangulation =
  mapValid (faceHandle triangulation) (Fixed.allFaces triangulation)

-- | Every inner-face handle.
innerFaceHandles
  :: Triangulation mode vertex directed undirected face
  -> [FaceHandle InnerTag mode vertex directed undirected face]
innerFaceHandles triangulation =
  mapValid (innerFaceHandle triangulation) (Fixed.innerFaces triangulation)

-- | Hull directed-edge handles in boundary order.
hullEdgeHandles
  :: Triangulation mode vertex directed undirected face
  -> [DirectedEdgeHandle mode vertex directed undirected face]
hullEdgeHandles triangulation =
  mapValid (directedEdgeHandle triangulation) (Hull.hullEdges triangulation)

-- | Hull vertex handles in boundary order.
hullVertexHandles
  :: Triangulation mode vertex directed undirected face
  -> [VertexHandle mode vertex directed undirected face]
hullVertexHandles triangulation =
  map directedEdgeFrom (hullEdgeHandles triangulation)

-- | Strict fold over every vertex handle.
foldVertexHandles'
  :: Triangulation mode vertex directed undirected face
  -> (accumulator -> VertexHandle mode vertex directed undirected face -> accumulator)
  -> accumulator
  -> accumulator
foldVertexHandles' triangulation step =
  Fixed.foldVertices' triangulation (applyValid (vertexHandle triangulation) step)

-- | Strict fold over every directed-edge handle.
foldDirectedEdgeHandles'
  :: Triangulation mode vertex directed undirected face
  -> (accumulator -> DirectedEdgeHandle mode vertex directed undirected face -> accumulator)
  -> accumulator
  -> accumulator
foldDirectedEdgeHandles' triangulation step =
  Fixed.foldDirectedEdges' triangulation (applyValid (directedEdgeHandle triangulation) step)

-- | Strict fold over every undirected-edge handle.
foldUndirectedEdgeHandles'
  :: Triangulation mode vertex directed undirected face
  -> (accumulator -> UndirectedEdgeHandle mode vertex directed undirected face -> accumulator)
  -> accumulator
  -> accumulator
foldUndirectedEdgeHandles' triangulation step =
  Fixed.foldUndirectedEdges' triangulation (applyValid (undirectedEdgeHandle triangulation) step)

-- | Strict fold over every face handle, including the outer face.
foldAllFaceHandles'
  :: Triangulation mode vertex directed undirected face
  -> (accumulator -> FaceHandle PossiblyOuterTag mode vertex directed undirected face -> accumulator)
  -> accumulator
  -> accumulator
foldAllFaceHandles' triangulation step =
  Fixed.foldAllFaces' triangulation (applyValid (faceHandle triangulation) step)

-- | Strict fold over every inner-face handle.
foldInnerFaceHandles'
  :: Triangulation mode vertex directed undirected face
  -> (accumulator -> FaceHandle InnerTag mode vertex directed undirected face -> accumulator)
  -> accumulator
  -> accumulator
foldInnerFaceHandles' triangulation step =
  Fixed.foldInnerFaces' triangulation (applyValid (innerFaceHandle triangulation) step)

-- | Strict fold over hull directed-edge handles.
foldHullEdgeHandles'
  :: Triangulation mode vertex directed undirected face
  -> (accumulator -> DirectedEdgeHandle mode vertex directed undirected face -> accumulator)
  -> accumulator
  -> accumulator
foldHullEdgeHandles' triangulation step =
  Hull.foldHullEdges' triangulation (applyValid (directedEdgeHandle triangulation) step)

-- | Strict fold over hull vertex handles.
foldHullVertexHandles'
  :: Triangulation mode vertex directed undirected face
  -> (accumulator -> VertexHandle mode vertex directed undirected face -> accumulator)
  -> accumulator
  -> accumulator
foldHullVertexHandles' triangulation step =
  foldHullEdgeHandles' triangulation (\accumulator edge -> step accumulator (directedEdgeFrom edge))

mapValid :: (fixed -> Maybe dynamic) -> [fixed] -> [dynamic]
mapValid make = foldr collect []
 where
  collect fixed rest = case make fixed of
    Just dynamic -> dynamic : rest
    Nothing -> rest

applyValid
  :: (fixed -> Maybe dynamic)
  -> (accumulator -> dynamic -> accumulator)
  -> accumulator
  -> fixed
  -> accumulator
applyValid make step !accumulator fixed = case make fixed of
  Just dynamic -> step accumulator dynamic
  Nothing -> accumulator
