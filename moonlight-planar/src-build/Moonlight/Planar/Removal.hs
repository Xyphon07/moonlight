{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Persistent removal: one excision, published as its own triangulation.
module Moonlight.Planar.Removal
  ( RemovalOutcome (..)
  , RemovalResult (..)
  , removeVertex
  , locateAndRemove
  ) where

import Control.DeepSeq (NFData)
import Moonlight.Planar.Internal.HandleDefs (VertexId (..))
import Moonlight.Planar.Internal.PointIndex (lookupPointIndex)
import Moonlight.Planar.Internal.Representation (Triangulation (..))
import Moonlight.Planar.Internal.Types (BuildError)
import Moonlight.Planar.BuildStats (BuildStats)
import Moonlight.Planar.Point (Point, queryPointValue)
import Moonlight.Planar.Internal.Types (validatePoint)
import Moonlight.Planar.Internal.Session (RemovalOutcome (..), excise, withLocalSession)
import GHC.Generics (Generic)

-- | One removal and its publication: the frozen triangulation, the outcome
-- record, and the operation's counters. The persistent entries publish all
-- three; a session publishes the outcome per call and the counters once.
data RemovalResult mode vertex directed undirected face = RemovalResult
  { removalTriangulation :: !(Triangulation mode vertex directed undirected face)
  , removalOutcome :: !(RemovalOutcome vertex)
  , removalStats :: !BuildStats
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

deriving stock instance
  (Eq vertex, Eq directed, Eq undirected, Eq face)
  => Eq (RemovalResult mode vertex directed undirected face)
deriving stock instance
  (Show vertex, Show directed, Show undirected, Show face)
  => Show (RemovalResult mode vertex directed undirected face)

-- | Remove the vertex a handle names. The triangulation the caller passed in
-- still denotes the mesh it always did; the removal is in the returned one.
--
-- This is a session over a single 'excise', which is what makes replacing a
-- fold of it with one session sound: the two agree on every mesh, and differ
-- only in how many of the intermediate ones are published. A fold publishes
-- @k@ meshes and pays a thaw for each, so it runs in Θ(n·k); the session pays
-- one thaw and runs in O(k·deg).
removeVertex
  :: Triangulation mode vertex directed undirected face
  -> VertexId
  -> Either BuildError (RemovalResult mode vertex directed undirected face)
removeVertex triangulation requested = do
  (outcome, frozen, stats) <- withLocalSession triangulation 0 (excise requested)
  pure
    RemovalResult
      { removalTriangulation = frozen
      , removalOutcome = outcome
      , removalStats = stats
      }

-- | Resolve a point through the mesh's exact derived identity section, then
-- remove the handle it proves. A hash is only a rejection filter; the lookup
-- confirms both authoritative coordinate planes before the topology is
-- opened. A point that sites no vertex answers 'Nothing' without publishing a
-- cosmetically different mesh.
--
-- See 'removeVertex' on the cost of folding this rather than opening one
-- session over @removeAt@.
locateAndRemove
  :: Triangulation mode vertex directed undirected face
  -> Point
  -> Either BuildError (Maybe (RemovalResult mode vertex directed undirected face))
locateAndRemove triangulation point = do
  queryPoint <- validatePoint Nothing point
  case
      lookupPointIndex
        (triPointX triangulation)
        (triPointY triangulation)
        (triPointIndex triangulation)
        -- Stored coordinates are canonical; the query must be too, or any
        -- coordinate whose canonicalization exceeds signed zero misses a vertex
        -- the session verbs would find.
        (queryPointValue queryPoint) of
    Nothing -> Right Nothing
    Just rawVertex ->
      Just <$> removeVertex triangulation (VertexId (fromIntegral rawVertex))

-- Polymorphic entries consumed from other packages; without exposed
-- unfoldings they run boxed through an imported boundary.
