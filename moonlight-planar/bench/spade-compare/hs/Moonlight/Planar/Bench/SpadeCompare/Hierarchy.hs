{-# LANGUAGE DataKinds #-}

-- | Hierarchy-hint lanes, their gates, and the maintained-hint step.
module Moonlight.Planar.Bench.SpadeCompare.Hierarchy where

import System.FilePath ((</>))
import qualified Data.Vector as V
import Moonlight.Planar.BulkLoad
import Moonlight.Planar.Types (DelaunayTriangulation, InsertionDisposition (..), unitElementDefaults)
import Moonlight.Planar.Point (Point (..))
import Moonlight.Planar.Telemetry (LocationHint (..), Session, insertVertexAt, insertVertexAtNearVertex, refuse, withSession)
import Moonlight.Planar.HintGenerator
import Moonlight.Planar.Internal.Types (validatePoint)
import Moonlight.Planar.Session
import Moonlight.Planar.Bench.SpadeCompare.Support
import Moonlight.Planar.Bench.SpadeCompare.Input (randomPoints)

-- | The step both insertion lanes run: one insert, one hierarchy update, and
-- nothing between them. It is what spade's @insert@ does internally through
-- @notify_vertex_inserted@, and both sides carry branch factor 16.
--
-- The point handed to 'updateHierarchyAfterInsertion' is the same binding
-- handed to 'insertVertexAt', never one read back out of the harness's input
-- vector. The mesh stores 'canonicalPoint' of what it was given, so a
-- harness-side copy would part company from it on a signed zero, seed a
-- different nearest neighbour, and move the gate against spade. Same function,
-- same argument, same bits.
hierarchyStep
  :: HierarchyHint
  -> Point
  -> Session s Point () () () HierarchyHint
hierarchyStep hierarchy point = do
  query <- either refuse pure (validatePoint Nothing point)
  (vertex, disposition) <-
    case hierarchyHint hierarchy query of
      Just (VertexHint seed) -> insertVertexAtNearVertex seed point point
      _ -> insertVertexAt point point
  either refuse pure (updateHierarchyAfterInsertion hierarchy point vertex disposition)

-- | Arrival-order insertion with the hierarchy maintained alongside it.
--
-- One session spans the whole run. The hierarchy no longer asks for a
-- triangulation, so the per-point publication that existed only to hand it one
-- is gone: the mesh is thawed once and published once for the program, where
-- before it was once per point. The measured operation is unchanged — still an
-- insert per point with the hint maintained against it, which is spade's loop.
insertAllWithHierarchy
  :: V.Vector Point
  -> IO (DelaunayTriangulation Point, HierarchyHint)
insertAllWithHierarchy points = do
  let base = empty unitElementDefaults
  initial <- require (buildHierarchyHint defaultHierarchyBranchFactor base)
  (hierarchy, mesh, _) <-
    require (withSession base (V.length points) (V.foldM' hierarchyStep initial points))
  pure (mesh, hierarchy)

-- | The same step over a mesh that already holds every point, which is the
-- duplicate lane. Each insert locates a site that is already there and answers
-- 'AlreadyPresent', so the hierarchy comes back unexamined and the level walk
-- is never entered. What remains on the clock is the locate and the
-- disposition test, with no rebuild standing behind either.
reinsertAllWithHierarchy
  :: (DelaunayTriangulation Point, HierarchyHint)
  -> V.Vector Point
  -> IO (DelaunayTriangulation Point, HierarchyHint)
reinsertAllWithHierarchy (triangulation, hierarchy) points =
  case withSession triangulation (V.length points) (V.foldM' hierarchyStep hierarchy points) of
    Left refusal -> fail (show refusal)
    Right (updated, mesh, _) -> pure (mesh, updated)

-- | Remove a program of coordinates through the hierarchy: every locate
-- starts at the hierarchy's nearest sample, one base session publishes once,
-- and the hierarchy of the resulting value is published once.
--
-- No query observes an intermediate hierarchy in this lane. A triangulation
-- and its hierarchy denote their current values rather than the edit history,
-- so the lawful physical schedule is one base session followed by one
-- hierarchy publication — the shape of the strict side's
-- @HierarchyTriangulation@, whose removals also locate through the hierarchy.
removeAllWithHierarchy
  :: (DelaunayTriangulation Point, HierarchyHint)
  -> V.Vector Point
  -> IO (DelaunayTriangulation Point, HierarchyHint)
removeAllWithHierarchy (triangulation, hierarchy) points =
  case removeManyWithHierarchy hierarchy triangulation points of
    Left refusal -> fail (show refusal)
    Right (outcomes, surviving, repaired) -> do
      V.imapM_ checkRemoved outcomes
      pure (surviving, repaired)
 where
  checkRemoved index outcome = case outcome of
    Nothing ->
      error
        ( "removal target is not a vertex: index="
            <> show index
            <> " point="
            <> ( case points V.! index of
                  Point x y -> hex64 x <> "," <> hex64 y
               )
        )
    Just _ -> pure ()

-- | The hierarchy lanes' gate, in two parts. The base mesh alone would not see
-- a hierarchy whose nesting law has drifted, because a search started from a
-- bad hint still walks to the right answer; a hint naming a vertex that does
-- not exist, or one the search cannot walk out of, moves the answers. Both are
-- pinned.
hierarchyGateQueries :: Int
hierarchyGateQueries = 1000

writeHierarchyGate
  :: FilePath
  -> String
  -> (DelaunayTriangulation Point, HierarchyHint)
  -> IO ()
writeHierarchyGate directory label (triangulation, hierarchy) = do
  queries <- requireQueryPoints (randomPoints 0x3141592653589793 hierarchyGateQueries)
  writeFile
    (directory </> (label <> "-edges.txt"))
    (unlines (canonicalEdges triangulation))
  writeFile
    (directory </> (label <> "-nearest.txt"))
    ( unlines
        ( map
            (nearestAnswer triangulation hierarchy)
            queries
        )
    )

writeHierarchyIncrementalGate :: FilePath -> Int -> IO ()
writeHierarchyIncrementalGate directory pointCount = do
  built <- insertAllWithHierarchy (V.fromList (randomPoints 0x9e3779b97f4a7c15 pointCount))
  writeHierarchyGate directory ("hierarchy-incremental-" <> show pointCount) built

writeHierarchyDuplicateGate :: FilePath -> Int -> Int -> IO ()
writeHierarchyDuplicateGate directory pointCount duplicateCount = do
  base <- hierarchyOf pointCount
  built <-
    reinsertAllWithHierarchy
      base
      (V.fromList (randomPoints 0x9e3779b97f4a7c15 duplicateCount))
  writeHierarchyGate
    directory
    ("hierarchy-duplicate-" <> show pointCount <> "-" <> show duplicateCount)
    built

writeHierarchyRemovalGate :: FilePath -> Int -> Int -> IO ()
writeHierarchyRemovalGate directory pointCount removalCount = do
  base <- hierarchyOf pointCount
  built <-
    removeAllWithHierarchy
      base
      (V.fromList (take removalCount (randomPoints 0x9e3779b97f4a7c15 pointCount)))
  writeHierarchyGate
    directory
    ("hierarchy-removal-" <> show pointCount <> "-" <> show removalCount)
    built

-- | A bulk-loaded mesh and the hierarchy over it, which is where the duplicate
-- and removal lanes start. The strict side reaches the same pair through
-- @HierarchyTriangulation::bulk_load@.
hierarchyOf
  :: Int
  -> IO (DelaunayTriangulation Point, HierarchyHint)
hierarchyOf count = do
  triangulation <- delaunayOf count
  hierarchy <- require (buildHierarchyHint defaultHierarchyBranchFactor triangulation)
  pure (triangulation, hierarchy)
