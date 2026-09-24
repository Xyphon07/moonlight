{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Reusable point-location hints and their topology-preserving maintenance.
module Moonlight.Planar.HintGenerator
  ( LastUsedHint
  , emptyLastUsedHint
  , lastUsedHint
  , rememberVertex
  , HierarchyHint
  , defaultHierarchyBranchFactor
  , buildHierarchyHint
  , hierarchyHint
  , hierarchyBranchFactor
  , hierarchyBaseCount
  , hierarchyLevelCount
  , hierarchyVertexCount
  , updateHierarchyAfterInsertion
  , updateHierarchyAfterRemoval
  , rebuildHierarchyHint
  , removeManyWithHierarchy
  ) where

import qualified Data.Vector as V
import Control.DeepSeq (NFData)
import Data.List (sort)
import Data.Word (Word32)
import Moonlight.Planar.BulkLoad (delaunay, insert)
import Moonlight.Planar.Dcel (numUndirectedEdges, numVertices, undirectedEndpoints, vertexPoint)
import Moonlight.Planar.Internal.HandleDefs
  ( UndirectedEdgeId (..)
  , VertexId (..)
  , vertexIdIndex
  )
import Moonlight.Planar.Interpolation (nearestNeighbor)
import Moonlight.Planar.Point (canonicalPoint)
import Moonlight.Planar.Internal.Types (validatePoint)
import Moonlight.Planar.Removal (RemovalOutcome, removalTriangulation, removeVertex)
import Moonlight.Planar.Session (removeManyAtNear, withSession)
import Moonlight.Planar.Types
import Moonlight.Planar.Point (Point (..), QueryPoint)
import GHC.Generics (Generic)

-- | Most recently admitted vertex, suitable as the next descent seed.
newtype LastUsedHint = LastUsedHint (Maybe VertexId)
  deriving stock (Show)
  deriving newtype (Eq, Ord)

-- | A last-used hint with no remembered vertex.
emptyLastUsedHint :: LastUsedHint
emptyLastUsedHint = LastUsedHint Nothing

-- | Project a remembered vertex into a location hint.
lastUsedHint :: LastUsedHint -> Maybe LocationHint
lastUsedHint (LastUsedHint vertex) = VertexHint <$> vertex

-- | Replace the remembered vertex.
rememberVertex :: VertexId -> LastUsedHint -> LastUsedHint
rememberVertex vertex _ = LastUsedHint (Just vertex)

-- | A Delaunay hierarchy for logarithmic expected random point location.
-- The first vector element is the finest sparse level; the final element is
-- the coarsest. The base triangulation is not duplicated.
--
-- The levels are nested by a single arithmetic law rather than by a stored
-- correspondence. Level @i@ holds every @branch^(i+1)@-th base vertex in base
-- order, so a level-local handle @j@ names handle @j * branch@ one level finer
-- — and at level 0 the finer level is the base mesh itself, under the very
-- same multiplication. Descent therefore never rediscovers a handle it has
-- already computed, and no level carries an index vector.
--
-- 'hierarchyBaseCount' is the base cardinality the levels were sampled from.
-- It is the hierarchy's claim about which triangulation it answers for, and
-- every maintenance entry is stated against it: a base whose cardinality is not
-- the one recorded here plus the movement the operation performs is not the
-- base this hierarchy describes, and is rebuilt for rather than patched.
data HierarchyHint = HierarchyHint
  { hierarchyBranchFactor :: {-# UNPACK #-} !Int
    -- ^ Sampling stride between adjacent hierarchy levels.
  , hierarchyBaseCount :: {-# UNPACK #-} !Int
    -- ^ Cardinality of the base mesh described by the hierarchy.
  , hierarchyLevels :: !(V.Vector (Triangulation 'Unconstrained (Point) () () ()))
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

instance Show (HierarchyHint) where
  showsPrec precedence hierarchy =
    showParen (precedence > 10) $
      showString "HierarchyHint "
        . shows (hierarchyBranchFactor hierarchy)
        . showString " "
        . shows (hierarchyBaseCount hierarchy)
        . showString " "
        . shows (hierarchyLevelCount hierarchy)
        . showString " "
        . shows (hierarchyVertexCount hierarchy)

instance Eq (HierarchyHint) where
  left == right =
    hierarchyBranchFactor left == hierarchyBranchFactor right
      && hierarchyBaseCount left == hierarchyBaseCount right
      && hierarchyLevelCount left == hierarchyLevelCount right
      && V.and (V.zipWith sameSparseMesh (hierarchyLevels left) (hierarchyLevels right))

-- Two sparse levels are the same hint when they carry the same points and the
-- same undirected edges. Half-edge index labelling records the order
-- construction happened to visit, so a level rebuilt from scratch and a level
-- extended in place are structurally equal while their arrays are not.
sameSparseMesh
  :: Triangulation 'Unconstrained (Point) () () ()
  -> Triangulation 'Unconstrained (Point) () () ()
  -> Bool
sameSparseMesh left right =
  numVertices left == numVertices right
    && numUndirectedEdges left == numUndirectedEdges right
    && meshPoints left == meshPoints right
    && meshEdges left == meshEdges right
 where
  meshPoints
    :: Triangulation mode vertex directed undirected face
    -> [Point]
  meshPoints triangulation =
    sort [vertexPoint triangulation (VertexId (fromIntegral index)) | index <- [0 .. numVertices triangulation - 1]]

  meshEdges
    :: Triangulation mode vertex directed undirected face
    -> [(Point, Point)]
  meshEdges triangulation =
    sort
      [ if from <= to then (from, to) else (to, from)
      | index <- [0 .. numUndirectedEdges triangulation - 1]
      , let (fromVertex, toVertex) = undirectedEndpoints triangulation (UndirectedEdgeId (fromIntegral index))
            from = vertexPoint triangulation fromVertex
            to = vertexPoint triangulation toVertex
      ]

-- | Default sampling stride between hierarchy levels.
defaultHierarchyBranchFactor :: Int
defaultHierarchyBranchFactor = 16

-- | Number of stored sparse levels.
hierarchyLevelCount :: HierarchyHint -> Int
hierarchyLevelCount = V.length . hierarchyLevels

-- | Total vertices retained across every sparse level.
hierarchyVertexCount :: HierarchyHint -> Int
hierarchyVertexCount =
  V.foldl' (\total level -> total + numVertices level) 0 . hierarchyLevels

-- | Build nested sparse Delaunay levels. A branch factor of 16 mirrors Spade's
-- default and gives O(log n) expected descent on uniformly distributed input.
--
-- Each level is bulk loaded from the sampled points in base order. Base
-- vertices carry pairwise distinct positions, so the load deduplicates nothing
-- and assigns local handle @j@ to sample @j@ — which is what makes the nesting
-- law on 'hierarchyLevels' an identity rather than a lookup.
buildHierarchyHint
  :: Int
  -> Triangulation mode vertex directed undirected face
  -> Either BuildError (HierarchyHint)
buildHierarchyHint requestedBranch triangulation =
  HierarchyHint branch count <$> V.mapM buildLevel levelDivisors
 where
  !branch = max 2 requestedBranch
  !count = numVertices triangulation
  levelDivisors =
    V.unfoldr
      (\candidate ->
         case candidate of
           Nothing -> Nothing
           Just divisor ->
             let !population = samplePopulation count divisor
                 next =
                   if population <= 1
                     then Nothing
                     else Just (safeMultiply divisor branch)
              in Just (divisor, next)
      )
      (if count <= 0 then Nothing else Just branch)

  buildLevel divisor =
    buildTriangulation
      <$> delaunay
        unitElementDefaults
        ( V.generate
            (samplePopulation count divisor)
            (\index ->
               vertexPoint
                 triangulation
                 (VertexId (fromIntegral (index * divisor)))
            )
        )

-- | Descend from the coarsest sparse triangulation. The handle a level returns
-- is carried to the next finer level by one multiplication, and the same
-- multiplication at level 0 names the base vertex. Nothing is relocated: the
-- coarse answer is not searched for again, it is computed.
hierarchyHint :: HierarchyHint -> QueryPoint -> Maybe LocationHint
hierarchyHint HierarchyHint{hierarchyBranchFactor, hierarchyLevels} query =
  VertexHint <$> descend (V.length hierarchyLevels - 1) Nothing
 where
  !branch = fromIntegral hierarchyBranchFactor :: Word32

  descend !levelIndex !coarse
    | levelIndex < 0 = Nothing
    | otherwise =
        case nearestNeighbor (hierarchyLevels V.! levelIndex) coarse query of
          Nothing -> Nothing
          Just (VertexId local, _) ->
            let !finer = VertexId (local * branch)
             in if levelIndex == 0 then Just finer else descend (levelIndex - 1) (Just finer)


-- | Update the nested hierarchy from an insertion's own report: the point the
-- insertion was asked for, the handle it answered, and whether it created a
-- site. Only the levels selected by the branch divisibility rule are changed.
-- Unaffected levels are structurally shared.
--
-- No triangulation is named. The hierarchy walks its own levels and nothing
-- else, so a base was only ever a lookup table for three facts — its
-- cardinality, the stored position of the new vertex, and the position of
-- vertex zero — and every one of them is in the report or already in the
-- levels, because level-local handle zero is base handle zero at every level.
-- A caller maintaining the hierarchy across a run of insertions therefore
-- never has to publish a mesh to be allowed to speak to it, which is the whole
-- cost of the arrangement this replaces: one full arena copy per step, paid
-- only to name the thing that was just edited.
--
-- An insertion that found its point already present created no site, and a
-- hierarchy valid for a triangulation is valid for that same triangulation, so
-- the answer is the argument, unexamined.
updateHierarchyAfterInsertion
  :: HierarchyHint
  -> Point
  -> VertexId
  -> InsertionDisposition
  -> Either BuildError (HierarchyHint)
updateHierarchyAfterInsertion hierarchy requested vertex disposition =
  case disposition of
    AlreadyPresent -> Right hierarchy
    Inserted
      | vertexIdIndex vertex /= baseIndex ->
          Left
            ( HierarchyInsertionHandleMismatch
                (VertexId (fromIntegral baseIndex))
                vertex
            )
      | otherwise -> do
          updated <- updateLevels branch (V.toList (hierarchyLevels hierarchy))
          topped <- ensureSingletonTop updated
          pure
            hierarchy
              { hierarchyBaseCount = baseIndex + 1
              , hierarchyLevels = V.fromList topped
              }
 where
  !branch = hierarchyBranchFactor hierarchy
  !baseIndex = hierarchyBaseCount hierarchy
  -- A level holds what the mesh stores, not what the caller wrote: storage
  -- rounds a signed zero, and a level built from the unrounded point would
  -- settle a distance tie against a different handle.
  !point = canonicalPoint requested

  -- An update is the only operation that can break the nesting law, so the law
  -- is stated here as an equation rather than trusted: a level admitted to the
  -- new base vertex must already hold exactly @baseIndex / divisor@ samples,
  -- and must receive the vertex at its end.
  updateLevels
    :: Int
    -> [Triangulation 'Unconstrained Point () () ()]
    -> Either BuildError [Triangulation 'Unconstrained Point () () ()]
  updateLevels !_ [] = Right []
  updateLevels !divisor levels@(level : rest)
    | baseIndex `rem` divisor /= 0 = Right levels
    | safeMultiply (numVertices level) divisor /= baseIndex =
        Left
          ( HierarchyLevelPopulationMismatch
              divisor
              (numVertices level)
              baseIndex
          )
    | otherwise = do
        inserted <- insert level point
        if vertexIdIndex (insertionVertex inserted) /= numVertices level
          then
            Left
              ( HierarchyInsertionHandleMismatch
                  (VertexId (fromIntegral (numVertices level)))
                  (insertionVertex inserted)
              )
          else (insertionTriangulation inserted :) <$> updateLevels (safeMultiply divisor branch) rest

  -- With no levels at all the base was empty, so the vertex just appended is
  -- vertex zero. Otherwise vertex zero is the finest level's local vertex
  -- zero, under the same law that makes the descent a multiplication.
  ensureSingletonTop [] = pure <$> singletonLevel point
  ensureSingletonTop levels@(finest : _) =
    case reverse levels of
      top : _
        | numVertices top <= 1 -> Right levels
        | otherwise ->
            (\first -> levels ++ [first])
              <$> singletonLevel (vertexPoint finest (VertexId 0))
      [] -> Right levels

-- | Repair the nested hierarchy from a removal's swap report — the slot
-- compaction freed and the position of the vertex it moved into that slot, or
-- 'Nothing' when the removal took the last vertex and compaction moved
-- nothing — rather than rebuilding because removal renumbers.
--
-- No triangulation is named, for the reason 'updateHierarchyAfterInsertion'
-- gives: the only base position this repair cannot find in its own levels is
-- the relocated vertex's, and that is what the report carries.
--
-- Swap compaction moves exactly one vertex — the former last one, into the slot
-- the removed vertex vacated — so a level's sample sequence changes in at most
-- one place, and which place is decided by the two divisibilities the level's
-- divisor gives the freed slot and the vacated last index:
--
-- * neither is sampled: the level, and every coarser level above it, is
--   untouched, because a divisor that divides neither index is divided by no
--   multiple of itself either;
-- * the vacated index is sampled and the freed slot is not: the level loses its
--   last sample and nothing else;
-- * both are sampled: the level loses its last sample and that sample's point
--   lands in the freed slot's local position — which is the level's own swap
--   removal, mirroring the base's;
-- * the freed slot is sampled and the vacated index is not: the level keeps its
--   population and substitutes the relocated position at an interior local
--   slot. No removal expresses a substitution, so that level is rebuilt — from
--   its own points and the reported one, never from a mesh.
--
-- The last case is the only one that pays a build, and it is the rarest: it
-- needs the freed slot to be sampled and the vacated index not to be.
updateHierarchyAfterRemoval
  :: HierarchyHint
  -> Maybe (VertexId, Point)
  -> Either BuildError (HierarchyHint)
updateHierarchyAfterRemoval hierarchy swap
  | baseCount <= 0 = Left (RemovalEmptyTriangulation (maybe (VertexId 0) fst swap))
  | otherwise = do
      repaired <- repairLevels branch (V.toList (hierarchyLevels hierarchy))
      pure
        hierarchy
          { hierarchyBaseCount = surviving
          , hierarchyLevels = V.fromList (levelsThroughSingleton repaired)
          }
 where
  !branch = hierarchyBranchFactor hierarchy
  !baseCount = hierarchyBaseCount hierarchy
  !surviving = baseCount - 1
  -- The freed slot is the removed vertex's own index, and it is where the
  -- former last vertex now stands. A removal that took the last vertex frees
  -- no slot and reports none, and its removed index is that last index.
  !freedSlot = maybe surviving (vertexIdIndex . fst) swap
  !vacatedIndex = surviving

  -- The nesting law is stated here as an equation for the same reason the
  -- insertion path states it: a repair is the other operation that can break
  -- it. A level the removal reaches must hold exactly the samples the
  -- pre-removal base owed it.
  repairLevels !_ [] = Right []
  repairLevels !divisor levels@(level : rest)
    | not freedSampled && not vacatedSampled = Right levels
    | numVertices level /= population =
        Left
          ( HierarchyLevelPopulationMismatch
              divisor
              (numVertices level)
              baseCount
          )
    | otherwise =
        case swap of
          -- The two divisibilities differ only when the two indices do, so a
          -- level reaching the substitution has a relocation to substitute:
          -- the guard cannot hold while compaction moved nothing.
          Just (_, relocated)
            | freedSampled && not vacatedSampled -> do
                substituted <- substituteSample level (freedSlot `quot` divisor) relocated
                (substituted :) <$> repairLevels (safeMultiply divisor branch) rest
          _ -> do
            shrunk <- removeVertex level (VertexId (fromIntegral localSample))
            (removalTriangulation shrunk :) <$> repairLevels (safeMultiply divisor branch) rest
   where
    !freedSampled = freedSlot `rem` divisor == 0
    !vacatedSampled = vacatedIndex `rem` divisor == 0
    !population = samplePopulation baseCount divisor
    !localSample
      | freedSampled = freedSlot `quot` divisor
      | otherwise = population - 1

  -- A build stops at the first level holding one sample, and removal only
  -- shrinks populations, so the shape a rebuild would answer with is this list
  -- cut after its first singleton.
  levelsThroughSingleton
    :: [Triangulation mode vertex directed undirected face]
    -> [Triangulation mode vertex directed undirected face]
  levelsThroughSingleton [] = []
  levelsThroughSingleton (level : rest)
    | numVertices level <= 0 = []
    | numVertices level <= 1 = [level]
    | otherwise = level : levelsThroughSingleton rest

-- | The coarsest level a growing hierarchy needs: one sample, the base's
-- vertex zero.
singletonLevel
  :: Point
  -> Either BuildError (Triangulation 'Unconstrained (Point) () () ())
singletonLevel origin =
  buildTriangulation <$> delaunay unitElementDefaults (V.singleton origin)

-- | The level a substitution asks for: the same samples in the same local
-- order, one slot carrying the relocated position instead of the one that
-- left. Stated over the level's own points, so no mesh is consulted.
substituteSample
  :: Triangulation 'Unconstrained (Point) () () ()
  -> Int
  -> Point
  -> Either BuildError (Triangulation 'Unconstrained (Point) () () ())
substituteSample level localSlot relocated =
  buildTriangulation
    <$> delaunay
      unitElementDefaults
      ( V.generate
          (numVertices level)
          (\index ->
            if index == localSlot
              then relocated
              else vertexPoint level (VertexId (fromIntegral index))
          )
      )

-- | Rebuild a hierarchy after an operation that may renumber vertices, such as
-- removal. The branch factor remains canonical.
rebuildHierarchyHint
  :: HierarchyHint
  -> Triangulation mode vertex directed undirected face
  -> Either BuildError (HierarchyHint)
rebuildHierarchyHint hierarchy = buildHierarchyHint (hierarchyBranchFactor hierarchy)

-- | Remove many points, each locate starting from the hierarchy's nearest
-- sample instead of the mesh boundary. One session publishes once; the
-- hierarchy is rebuilt against the surviving mesh and returned alongside it.
removeManyWithHierarchy
  :: HierarchyHint
  -> Triangulation mode vertex directed undirected face
  -> V.Vector (Point)
  -> Either
      BuildError
      ( V.Vector (Maybe (RemovalOutcome vertex))
      , Triangulation mode vertex directed undirected face
      , HierarchyHint
      )
removeManyWithHierarchy hierarchy triangulation points = do
  queryPoints <- traverse (validatePoint Nothing) points
  let guesses = fmap hierarchyGuess queryPoints
  (outcomes, surviving, _) <-
    withSession triangulation 0 (removeManyAtNear guesses points)
  repaired <- rebuildHierarchyHint hierarchy surviving
  pure (outcomes, surviving, repaired)
 where
  hierarchyGuess queryPoint =
    case hierarchyHint hierarchy queryPoint of
      Just (VertexHint vertex) -> Just vertex
      _ -> Nothing

safeMultiply :: Int -> Int -> Int
safeMultiply left right
  | left > maxBound `quot` right = maxBound
  | otherwise = left * right

-- | How many samples a divisor takes from a base of this size. Sampling takes
-- index zero and every @divisor@-th index after it; the direct
-- vector generator above states the same ceiling without constructing an
-- intermediate handle list.
samplePopulation :: Int -> Int -> Int
samplePopulation count divisor
  | count <= 0 = 0
  | otherwise = 1 + (count - 1) `quot` divisor
