{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}

-- | Immutable admission of one requested segment against the sparse
-- constrained-edge section, decided without thawing topology.
module Moonlight.Planar.Internal.Cdt.Admission
  ( ConstraintAdmission (..)
  , constraintAdmission
  , segmentBoxesAreDisjoint
  ) where

import qualified Data.IntSet as IntSet
import qualified Moonlight.Planar.Dcel as Dcel
import Moonlight.Planar.Internal.HandleDefs
import Moonlight.Planar.Internal.Predicates (segmentsProperlyCross)
import Moonlight.Planar.Internal.Representation
import Moonlight.Planar.Internal.Types
import Moonlight.Planar.Point (Point (..))

-- | Immutable admission descends through the sparse constrained-edge section,
-- not through every ordinary edge in the requested corridor. Constraint
-- cardinality is the lawful index here: rejection depends only on a proper
-- crossing with an existing protected segment, while shared endpoints,
-- duplicate segments, and collinear overlap remain recoverable by the mutable
-- corridor algebra. This is the read-only answer behind 'canAddConstraint';
-- the singleton verbs walk the corridor directly and refuse on the first
-- resident constraint it meets, which is the witness they publish.
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

  firstBlocking
    :: Int
    -> ConstraintAdmission
    -> ConstraintAdmission
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
