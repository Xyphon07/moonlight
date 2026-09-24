{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}

-- | Read-only interrogation of the constrained layer, over both the published
-- mesh and the thawed one, plus the endpoint validity the verbs share.
module Moonlight.Planar.Internal.Cdt.Query
  ( constraintEdges
  , constraintStorageBytes
  , existsConstraint
  , canAddConstraint
  , intersectsConstraint
  , getConflictingEdgesBetweenPoints
  , getConflictingEdgesBetweenVertices
  , findDirectedEdge
  , findMutableEdge
  , validateEndpoints
  , validVertex
  ) where

import Control.Monad.ST (ST)
import Data.Bits (xor)
import qualified Data.IntSet as IntSet
import qualified Moonlight.Planar.Dcel as Dcel
import Moonlight.Planar.Internal.HandleDefs
import Moonlight.Planar.IntersectionIterator
import Moonlight.Planar.Internal.Cdt.Admission
  ( ConstraintAdmission (..)
  , constraintAdmission
  )
import Moonlight.Planar.Internal.Cdt.Types (CdtError (..))
import Moonlight.Planar.Internal.Mutable
import Moonlight.Planar.Internal.Paged (pagedLength)
import Moonlight.Planar.Internal.Representation
import Moonlight.Planar.Internal.Types
import Moonlight.Planar.Point (QueryPoint)

-- | Marked undirected edges in ascending identifier order.
constraintEdges
  :: Triangulation 'Constrained vertex directed undirected face
  -> [UndirectedEdgeId]
constraintEdges =
  fmap (UndirectedEdgeId . fromIntegral)
    . IntSet.toAscList
    . triConstraintEdges
{-# INLINE constraintEdges #-}

-- | Bytes occupied by the dense constraint marker plane.
constraintStorageBytes
  :: Triangulation 'Constrained vertex directed undirected face
  -> Integer
constraintStorageBytes = toInteger . pagedLength . triConstraint

-- | Whether a marked edge directly joins two admitted vertices.
existsConstraint
  :: Triangulation 'Constrained vertex directed undirected face
  -> VertexId
  -> VertexId
  -> Bool
existsConstraint triangulation from to =
  case findDirectedEdge triangulation from to of
    Just edge -> Dcel.isConstraintEdge triangulation (asUndirected edge)
    Nothing -> False

-- | Whether a segment can be admitted without crossing a resident constraint.
canAddConstraint
  :: Triangulation 'Constrained vertex directed undirected face
  -> VertexId
  -> VertexId
  -> Bool
canAddConstraint triangulation from to =
  from /= to
    && validVertex triangulation from
    && validVertex triangulation to
    && case constraintAdmission triangulation from to of
      ConstraintBlocked _ -> False
      ConstraintAdmitted -> True

-- | Whether a query segment properly crosses a resident constraint.
intersectsConstraint
  :: Triangulation 'Constrained vertex directed undirected face
  -> QueryPoint
  -> QueryPoint
  -> Bool
intersectsConstraint triangulation from to =
  case foldCorridorBetweenPoints triangulation from to firstBlocking () of
    Just (Left ()) -> True
    Just (Right ()) -> False
    Nothing -> not (null (getConflictingEdgesBetweenPoints triangulation from to))
 where
  firstBlocking :: () -> Intersection -> Either () ()
  firstBlocking _ (EdgeIntersection edge)
    | Dcel.isConstraintEdge triangulation (asUndirected edge) = Left ()
  firstBlocking _ _ = Right ()

-- | Resident constrained edges properly crossed by a query segment.
getConflictingEdgesBetweenPoints
  :: Triangulation 'Constrained vertex directed undirected face
  -> QueryPoint
  -> QueryPoint
  -> [DirectedEdgeId]
getConflictingEdgesBetweenPoints triangulation from to =
  [ edge
  | EdgeIntersection edge <- lineIntersections triangulation from to
  , Dcel.isConstraintEdge triangulation (asUndirected edge)
  ]

-- | Resident constrained edges crossed between two admitted vertices.
getConflictingEdgesBetweenVertices
  :: Triangulation 'Constrained vertex directed undirected face
  -> VertexId
  -> VertexId
  -> [DirectedEdgeId]
getConflictingEdgesBetweenVertices triangulation from to
  | not (validVertex triangulation from && validVertex triangulation to) = []
  | otherwise =
      [ edge
      | EdgeIntersection edge <- lineIntersectionsBetweenVertices triangulation from to
      , Dcel.isConstraintEdge triangulation (asUndirected edge)
      ]

-- | Find an oriented edge joining two admitted vertices.
findDirectedEdge :: Triangulation 'Constrained vertex directed undirected face -> VertexId -> VertexId -> Maybe DirectedEdgeId
findDirectedEdge triangulation from to =
  case filter ((== to) . Dcel.destination triangulation) (Dcel.vertexOutgoingEdges triangulation from) of
    edge : _ -> Just edge
    [] -> Nothing

-- | Find an oriented edge in a thawed triangulation.
findMutableEdge
  :: forall s vertex directed undirected face.
     MutableDcel s vertex directed undirected face
  -> Int
  -> Int
  -> ST s (Maybe Int)
findMutableEdge mutable from to = do
  start <- readVertexOut mutable from
  halfEdges <- directedEdgeCount mutable
  if start < 0
    then pure Nothing
    else go (halfEdges + 1) start start False
 where
  go
    :: Int
    -> Int
    -> Int
    -> Bool
    -> ST s (Maybe Int)
  go !remaining !start !edge !visited
    | remaining <= 0 = pure Nothing
    | visited && edge == start = pure Nothing
    | otherwise = do
        destination <- readOrigin mutable (edge `xor` 1)
        if destination == to
          then pure (Just edge)
          else do
            previousEdge <- readPrevious mutable edge
            go (remaining - 1) start (previousEdge `xor` 1) True

-- | Validate that both constraint endpoints belong to the mesh.
validateEndpoints :: Triangulation 'Constrained vertex directed undirected face -> VertexId -> VertexId -> Either (CdtError) ()
validateEndpoints triangulation from to
  | not (validVertex triangulation from) = Left (InvalidConstraintVertex from)
  | not (validVertex triangulation to) = Left (InvalidConstraintVertex to)
  | otherwise = Right ()

-- | Whether a vertex handle is admitted by the triangulation.
validVertex :: Triangulation 'Constrained vertex directed undirected face -> VertexId -> Bool
validVertex triangulation (VertexId vertex) = fromIntegral vertex < Dcel.numVertices triangulation
