{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Triangulation.Internal.InterpolationWorkspace
  ( NaturalNeighborWorkspace (..)
  , newNaturalNeighborWorkspace
  , nextFaceGeneration
  , nextOriginGeneration
  , workspaceBytes
  ) where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (ST)
import Data.Primitive.MutVar
import qualified Data.Vector.Unboxed.Mutable as MUV
import Data.Word (Word32)
import Moonlight.Triangulation.Dcel (numDirectedEdges, numFaces, numVertices)
import Moonlight.Triangulation.Internal.Representation (Triangulation)
import Moonlight.Triangulation.Internal.Types (ConstraintMode)
import Moonlight.Triangulation.Scalar (scalarByteSize)

-- | Reusable scratch space for Sibson interpolation. It borrows no mutable
-- topology: the immutable triangulation is the sole owner, while these arrays
-- are query-local marks, queues and numeric work buffers.
data NaturalNeighborWorkspace state (mode :: ConstraintMode) vertex directed undirected face = NaturalNeighborWorkspace
  { nnTriangulation :: !(Triangulation mode vertex directed undirected face)
  , nnFaceMarks :: !(MUV.MVector state Word32)
  , nnFaceSeenMarks :: !(MUV.MVector state Word32)
    -- The circumcentre plane is stamped by the same generation as the two mark
    -- planes above, and that is what makes it sound: a generation is minted
    -- once per Sibson query, and the circumcentre a face contributes is a
    -- function of the face and the query point together. Holding the plane to
    -- exactly one generation's lifetime holds the query point fixed for as long
    -- as an entry can be read.
  , nnCircumcenterMarks :: !(MUV.MVector state Word32)
  , nnFaceGeneration :: !(MutVar state Word32)
  , nnCircumcenterX :: !(MUV.MVector state Double)
  , nnCircumcenterY :: !(MUV.MVector state Double)
  , nnFaceQueue :: !(MUV.MVector state Word32)
  , nnCavityFaces :: !(MUV.MVector state Word32)
  , nnBoundaryEdges :: !(MUV.MVector state Word32)
  , nnOrderedEdges :: !(MUV.MVector state Word32)
  , nnOriginMarks :: !(MUV.MVector state Word32)
  , nnOriginGeneration :: !(MutVar state Word32)
  , nnOriginEdge :: !(MUV.MVector state Word32)
  , nnInsertionX :: !(MUV.MVector state Double)
  , nnInsertionY :: !(MUV.MVector state Double)
  , nnWeightVertex :: !(MUV.MVector state Word32)
  , nnWeightValue :: !(MUV.MVector state Double)
  }

-- | Allocate reusable interpolation storage sized to one triangulation.
newNaturalNeighborWorkspace
  :: PrimMonad m
  => Triangulation mode vertex directed undirected face
  -> m (NaturalNeighborWorkspace (PrimState m) mode vertex directed undirected face)
newNaturalNeighborWorkspace triangulation = do
  let !faceCapacity = max 1 (numFaces triangulation)
      !edgeCapacity = max 1 (numDirectedEdges triangulation)
      !vertexCapacity = max 1 (numVertices triangulation)
  faceMarks <- MUV.replicate faceCapacity 0
  faceSeenMarks <- MUV.replicate faceCapacity 0
  circumcenterMarks <- MUV.replicate faceCapacity 0
  faceGeneration <- newMutVar 0
  circumcenterX <- MUV.new faceCapacity
  circumcenterY <- MUV.new faceCapacity
  faceQueue <- MUV.new faceCapacity
  cavityFaces <- MUV.new faceCapacity
  boundaryEdges <- MUV.new edgeCapacity
  orderedEdges <- MUV.new edgeCapacity
  originMarks <- MUV.replicate vertexCapacity 0
  originGeneration <- newMutVar 0
  originEdge <- MUV.new vertexCapacity
  insertionX <- MUV.new edgeCapacity
  insertionY <- MUV.new edgeCapacity
  weightVertex <- MUV.new edgeCapacity
  weightValue <- MUV.new edgeCapacity
  pure NaturalNeighborWorkspace
    { nnTriangulation = triangulation
    , nnFaceMarks = faceMarks
    , nnFaceSeenMarks = faceSeenMarks
    , nnCircumcenterMarks = circumcenterMarks
    , nnFaceGeneration = faceGeneration
    , nnCircumcenterX = circumcenterX
    , nnCircumcenterY = circumcenterY
    , nnFaceQueue = faceQueue
    , nnCavityFaces = cavityFaces
    , nnBoundaryEdges = boundaryEdges
    , nnOrderedEdges = orderedEdges
    , nnOriginMarks = originMarks
    , nnOriginGeneration = originGeneration
    , nnOriginEdge = originEdge
    , nnInsertionX = insertionX
    , nnInsertionY = insertionY
    , nnWeightVertex = weightVertex
    , nnWeightValue = weightValue
    }

nextFaceGeneration :: NaturalNeighborWorkspace s mode vertex directed undirected face -> ST s Word32
nextFaceGeneration workspace = do
  current <- readMutVar (nnFaceGeneration workspace)
  let !next = current + 1
  if next == 0
    then do
      MUV.set (nnFaceMarks workspace) 0
      MUV.set (nnFaceSeenMarks workspace) 0
      MUV.set (nnCircumcenterMarks workspace) 0
      writeMutVar (nnFaceGeneration workspace) 1
      pure 1
    else writeMutVar (nnFaceGeneration workspace) next >> pure next

nextOriginGeneration :: NaturalNeighborWorkspace s mode vertex directed undirected face -> ST s Word32
nextOriginGeneration workspace = nextGeneration (nnOriginMarks workspace) (nnOriginGeneration workspace)

nextGeneration :: MUV.MVector s Word32 -> MutVar s Word32 -> ST s Word32
nextGeneration marks reference = do
  current <- readMutVar reference
  let !next = current + 1
  if next == 0
    then do
      MUV.set marks 0
      writeMutVar reference 1
      pure 1
    else writeMutVar reference next >> pure next

-- | Bytes owned by the reusable numeric and index planes.
workspaceBytes
  :: NaturalNeighborWorkspace state mode vertex directed undirected face
  -> Integer
workspaceBytes workspace =
  4 * toInteger wordSlots
    + toInteger scalarByteSize * toInteger scalarSlots
 where
  wordSlots =
    MUV.length (nnFaceMarks workspace)
      + MUV.length (nnFaceSeenMarks workspace)
      + MUV.length (nnCircumcenterMarks workspace)
      + MUV.length (nnFaceQueue workspace)
      + MUV.length (nnCavityFaces workspace)
      + MUV.length (nnBoundaryEdges workspace)
      + MUV.length (nnOrderedEdges workspace)
      + MUV.length (nnOriginMarks workspace)
      + MUV.length (nnOriginEdge workspace)
      + MUV.length (nnWeightVertex workspace)
  scalarSlots =
    MUV.length (nnCircumcenterX workspace)
      + MUV.length (nnCircumcenterY workspace)
      + MUV.length (nnInsertionX workspace)
      + MUV.length (nnInsertionY workspace)
      + MUV.length (nnWeightValue workspace)
