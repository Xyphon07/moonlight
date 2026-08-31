{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}

-- | Immutable admission of one requested segment against the sparse
-- constrained-edge section, decided without thawing topology.
module Moonlight.Triangulation.Internal.Cdt.Admission
  ( ConstraintAdmission (..)
  , constraintAdmission
  , segmentBoxesAreDisjoint
  ) where

import qualified Data.IntSet as IntSet
import qualified Moonlight.Triangulation.Dcel as Dcel
import Moonlight.Triangulation.Internal.HandleDefs
import Moonlight.Triangulation.Internal.Predicates (segmentsProperlyCross)
import Moonlight.Triangulation.Internal.Representation
import Moonlight.Triangulation.Internal.Types

-- | Immutable admission descends through the sparse constrained-edge section,
-- not through every ordinary edge in the requested corridor. Constraint
-- cardinality is the lawful index here: rejection depends only on a proper
-- crossing with an existing protected segment, while shared endpoints,
-- duplicate segments, and collinear overlap remain recoverable by the mutable
-- corridor algebra. Accepted singleton requests descend once more inside their
-- sealed transaction; the overwhelmingly common rejected request stops here
-- without thawing topology.
data ConstraintAdmission
  = ConstraintBlocked !UndirectedEdgeId
  | ConstraintAdmitted

constraintAdmission
  :: Triangulation 'Constrained vertex directed undirected face
  -> VertexId
  -> VertexId
  -> ConstraintAdmission
constraintAdmission triangulation from to =
  IntSet.foldr firstBlocking ConstraintAdmitted (triConstraintEdges triangulation)
 where
  !requestFrom = Dcel.vertexPoint triangulation from
  !requestTo = Dcel.vertexPoint triangulation to

  firstBlocking raw later =
    let !edge = UndirectedEdgeId (fromIntegral raw)
        (!edgeFromId, !edgeToId) = Dcel.undirectedEndpoints triangulation edge
     in if
          from == edgeFromId
            || from == edgeToId
            || to == edgeFromId
            || to == edgeToId
          then later
          else
            let !edgeFrom = Dcel.vertexPoint triangulation edgeFromId
                !edgeTo = Dcel.vertexPoint triangulation edgeToId
             in if segmentBoxesAreDisjoint requestFrom requestTo edgeFrom edgeTo
                  then later
                  else
                    if segmentsProperlyCross requestFrom requestTo edgeFrom edgeTo
                      then ConstraintBlocked edge
                      else later

segmentBoxesAreDisjoint
  :: Point
  -> Point
  -> Point
  -> Point
  -> Bool
segmentBoxesAreDisjoint (Point ax ay) (Point bx by) (Point cx cy) (Point dx dy) =
  max ax bx < min cx dx
    || max cx dx < min ax bx
    || max ay by < min cy dy
    || max cy dy < min ay by
{-# INLINE segmentBoxesAreDisjoint #-}
