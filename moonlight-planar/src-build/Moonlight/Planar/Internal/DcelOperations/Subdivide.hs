{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Insertion into an existing element: face split and edge split.
module Moonlight.Planar.Internal.DcelOperations.Subdivide
  ( insertIntoFace
  , insertOnEdge
  , splitBoundaryEdgeWithLegalization
  ) where

import Moonlight.Planar.BuildStats (BuildMetric (FaceSplits, InteriorEdgeSplits, BoundaryEdgeSplits))

import Control.Monad (unless, void, when)
import Control.Monad.ST (ST)
import Moonlight.Planar.Internal.HandleDefs (UndirectedEdgeId (..))
import Moonlight.Planar.Internal.DcelOperations.Legalize (legalizeScratch)
import Moonlight.Planar.Internal.DcelOperations.Twin (reverseIndex)
import Moonlight.Planar.Internal.Mutable
  ( MutableDcel
  , addEdgeBlock
  , addFaceBlock
  , ensureCellCapacity
  , faceEdges
  , linkEdges
  , markConnected
  , payloadsPristine
  , readConstraint
  , readFace
  , readNext
  , readOrigin
  , readPrevious
  , resetEdgeData
  , resetFaceData
  , setConstraint
  , setCycle3
  , writeFace
  , writeFaceEdge
  , writeOrigin
  , writeVertexOut
  )
import Moonlight.Planar.Internal.OperationState
  ( Counter (..)
  , OperationState
  , addCounter
  , writeScratch
  )
import Moonlight.Planar.Internal.Probe (KnownProbe)
import Moonlight.Planar.Internal.Types (BuildError (..))

insertIntoFace :: forall p s vertex directed undirected face. KnownProbe p => MutableDcel s vertex directed undirected face -> OperationState s -> Int -> Int -> ST s (Either BuildError ())
insertIntoFace mutable operation face vertex = do
  capacity <- ensureCellCapacity mutable 3 2
  case capacity of
    Left obstruction -> pure (Left obstruction)
    Right () -> do
      insertIntoFaceWithCapacity @p mutable operation face vertex
      pure (Right ())

insertIntoFaceWithCapacity :: forall p s vertex directed undirected face. KnownProbe p => MutableDcel s vertex directed undirected face -> OperationState s -> Int -> Int -> ST s ()
insertIntoFaceWithCapacity mutable operation face vertex = do
  (eAB, eBC, eCA) <- faceEdges mutable face
  a <- readOrigin mutable eAB
  b <- readOrigin mutable eBC
  c <- readOrigin mutable eCA
  edgeBase <- addEdgeBlock mutable 3
  let !eAV = edgeBase
      !eVA = edgeBase + 1
      !eBV = edgeBase + 2
      !eVB = edgeBase + 3
      !eCV = edgeBase + 4
      !eVC = edgeBase + 5
  writeOrigin mutable eAV a
  writeOrigin mutable eVA vertex
  writeOrigin mutable eBV b
  writeOrigin mutable eVB vertex
  writeOrigin mutable eCV c
  writeOrigin mutable eVC vertex
  faceBase <- addFaceBlock mutable 2
  setCycle3 mutable face eAB eBV eVA
  setCycle3 mutable faceBase eBC eCV eVB
  setCycle3 mutable (faceBase + 1) eCA eAV eVC
  writeVertexOut mutable a eAB
  writeVertexOut mutable b eBC
  writeVertexOut mutable c eCA
  markConnected mutable vertex eVA
  -- ABC has become ABV; the other two thirds of it are fresh faces. The three
  -- boundary edges keep their endpoints and so keep their labels.
  resetFaceData mutable face
  writeScratch operation 0 eAB
  writeScratch operation 1 eBC
  writeScratch operation 2 eCA
  addCounter operation (Reported FaceSplits) 1
  legalizeScratch @p mutable operation vertex 3

insertOnEdge :: forall p s vertex directed undirected face. KnownProbe p => MutableDcel s vertex directed undirected face -> OperationState s -> Int -> Int -> ST s (Either BuildError ())
insertOnEdge mutable operation suppliedEdge vertex = do
  suppliedFace <- readFace mutable suppliedEdge
  reverseFace <- readFace mutable (reverseIndex suppliedEdge)
  if suppliedFace == 0 || reverseFace == 0
    then splitBoundaryEdge @p mutable operation suppliedEdge vertex
    else splitInteriorEdge @p mutable operation suppliedEdge vertex

splitInteriorEdge :: forall p s vertex directed undirected face. KnownProbe p => MutableDcel s vertex directed undirected face -> OperationState s -> Int -> Int -> ST s (Either BuildError ())
splitInteriorEdge mutable operation suppliedEdge vertex = do
  capacity <- ensureCellCapacity mutable 3 2
  case capacity of
    Left obstruction -> pure (Left obstruction)
    Right () -> do
      splitInteriorEdgeWithCapacity @p mutable operation suppliedEdge vertex
      pure (Right ())

splitInteriorEdgeWithCapacity :: forall p s vertex directed undirected face. KnownProbe p => MutableDcel s vertex directed undirected face -> OperationState s -> Int -> Int -> ST s ()
splitInteriorEdgeWithCapacity mutable operation suppliedEdge vertex = do
  protected <- readConstraint mutable suppliedEdge
  suppliedFace <- readFace mutable suppliedEdge
  let !edge = if suppliedFace == 0 then reverseIndex suppliedEdge else suppliedEdge
      !reverseEdgeEdge = reverseIndex edge
  leftFace <- readFace mutable edge
  rightFace <- readFace mutable reverseEdgeEdge
  eBC <- readNext mutable edge
  eCA <- readPrevious mutable edge
  eAD <- readNext mutable reverseEdgeEdge
  eDB <- readPrevious mutable reverseEdgeEdge
  a <- readOrigin mutable edge
  b <- readOrigin mutable reverseEdgeEdge
  c <- readOrigin mutable eCA
  d <- readOrigin mutable eDB
  writeOrigin mutable reverseEdgeEdge vertex
  edgeBase <- addEdgeBlock mutable 3
  let !eVB = edgeBase
      !eBV = edgeBase + 1
      !eVC = edgeBase + 2
      !eCV = edgeBase + 3
      !eVD = edgeBase + 4
      !eDV = edgeBase + 5
  writeOrigin mutable eVB vertex
  writeOrigin mutable eBV b
  writeOrigin mutable eVC vertex
  writeOrigin mutable eCV c
  writeOrigin mutable eVD vertex
  writeOrigin mutable eDV d
  faceBase <- addFaceBlock mutable 2
  setCycle3 mutable leftFace edge eVC eCA
  setCycle3 mutable faceBase eVB eBC eCV
  setCycle3 mutable rightFace eBV eVD eDB
  setCycle3 mutable (faceBase + 1) reverseEdgeEdge eAD eDV
  writeVertexOut mutable a edge
  writeVertexOut mutable b eBC
  writeVertexOut mutable c eCA
  writeVertexOut mutable d eDB
  markConnected mutable vertex reverseEdgeEdge
  -- AB became AV, and both incident triangles lost a corner to the new vertex.
  unless (payloadsPristine mutable) $ do
    resetEdgeData mutable (edge `quot` 2)
    resetFaceData mutable leftFace
    resetFaceData mutable rightFace
  when protected $ do
    _ <- setConstraint mutable eVB
    pure ()
  writeScratch operation 0 eCA
  writeScratch operation 1 eBC
  writeScratch operation 2 eDB
  writeScratch operation 3 eAD
  addCounter operation (Reported InteriorEdgeSplits) 1
  legalizeScratch @p mutable operation vertex 4

splitBoundaryEdge :: forall p s vertex directed undirected face. KnownProbe p => MutableDcel s vertex directed undirected face -> OperationState s -> Int -> Int -> ST s (Either BuildError ())
splitBoundaryEdge mutable operation suppliedEdge vertex = do
  outcome <-
    splitBoundaryEdgeWithLegalization
      (\target targetOperation insertedVertex _ _ ->
         legalizeScratch @p target targetOperation insertedVertex 2
      )
      mutable
      operation
      suppliedEdge
      vertex
  pure (void outcome)

-- | Split one hull edge and return its two replacement outer edges in the
-- original outer-cycle order.  Topology and constraint inheritance have one
-- owner; callers choose only the lawful legalization boundary.  Ordinary
-- insertion drains the new vertex's scratch star, while a source-preserving
-- seam can pin its immutable face section.
splitBoundaryEdgeWithLegalization
  :: ( MutableDcel s vertex directed undirected face
       -> OperationState s
       -> Int
       -> Int
       -> Int
       -> ST s ()
     )
  -> MutableDcel s vertex directed undirected face
  -> OperationState s
  -> Int
  -> Int
  -> ST s (Either BuildError (Int, Int))
splitBoundaryEdgeWithLegalization legalize mutable operation suppliedEdge vertex = do
  suppliedFace <- readFace mutable suppliedEdge
  reverseFace <- readFace mutable (reverseIndex suppliedEdge)
  if suppliedFace /= 0 && reverseFace /= 0
    then
      pure
        ( Left
            ( BoundarySplitRequiresBoundaryEdge
                (UndirectedEdgeId (fromIntegral (suppliedEdge `quot` 2)))
            )
        )
    else do
      capacity <- ensureCellCapacity mutable 2 1
      case capacity of
        Left obstruction -> pure (Left obstruction)
        Right () ->
          Right
            <$> splitBoundaryEdgeWithCapacity
              legalize
              mutable
              operation
              suppliedEdge
              vertex

splitBoundaryEdgeWithCapacity
  :: ( MutableDcel s vertex directed undirected face
       -> OperationState s
       -> Int
       -> Int
       -> Int
       -> ST s ()
     )
  -> MutableDcel s vertex directed undirected face
  -> OperationState s
  -> Int
  -> Int
  -> ST s (Int, Int)
splitBoundaryEdgeWithCapacity legalize mutable operation suppliedEdge vertex = do
  protected <- readConstraint mutable suppliedEdge
  suppliedFace <- readFace mutable suppliedEdge
  let !edge = if suppliedFace == 0 then reverseIndex suppliedEdge else suppliedEdge
      !outerEdge = reverseIndex edge
  innerFace <- readFace mutable edge
  eBC <- readNext mutable edge
  eCA <- readPrevious mutable edge
  a <- readOrigin mutable edge
  b <- readOrigin mutable outerEdge
  c <- readOrigin mutable eCA
  oldOuterPrevious <- readPrevious mutable outerEdge
  oldOuterNext <- readNext mutable outerEdge
  writeOrigin mutable outerEdge vertex
  edgeBase <- addEdgeBlock mutable 2
  let !eVB = edgeBase
      !eBV = edgeBase + 1
      !eVC = edgeBase + 2
      !eCV = edgeBase + 3
  writeOrigin mutable eVB vertex
  writeOrigin mutable eBV b
  writeOrigin mutable eVC vertex
  writeOrigin mutable eCV c
  newFace <- addFaceBlock mutable 1
  setCycle3 mutable innerFace edge eVC eCA
  setCycle3 mutable newFace eVB eBC eCV
  writeFace mutable outerEdge 0
  writeFace mutable eBV 0
  linkEdges mutable oldOuterPrevious eBV
  linkEdges mutable eBV outerEdge
  linkEdges mutable outerEdge oldOuterNext
  writeFaceEdge mutable 0 outerEdge
  writeVertexOut mutable a edge
  writeVertexOut mutable b eBC
  writeVertexOut mutable c eCA
  markConnected mutable vertex eVB
  -- AB became AV and the one interior triangle lost a corner. The outer face
  -- is not an element and keeps nothing to lose.
  unless (payloadsPristine mutable) $ do
    resetEdgeData mutable (edge `quot` 2)
    resetFaceData mutable innerFace
  when protected $ do
    _ <- setConstraint mutable eVB
    pure ()
  writeScratch operation 0 eCA
  writeScratch operation 1 eBC
  addCounter operation (Reported BoundaryEdgeSplits) 1
  legalize mutable operation vertex eCA eBC
  pure (eBV, outerEdge)
