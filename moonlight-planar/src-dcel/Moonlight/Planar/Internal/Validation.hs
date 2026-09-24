{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-expose-all-unfoldings #-}

-- | Discharge: the invariants the constructors guarantee, checkable on a value
-- built by any route.
module Moonlight.Planar.Internal.Validation
  ( canonicalAdmission
  , coordinatesAscend
  , validateTopology
  , ClosureStats (..)
  , topologyClosureStats
  , topologyClosureSelectionStats
  , validateTopologyClosure
  , validateTopologyClosureWithStats
  , validateDelaunay
  , validateTriangulation
  , triangulationIsValid
  , faceArea
  , faceMinimumAngleDegrees
  ) where

import Control.Monad (foldM)
import Control.Monad.ST (ST, runST)
import Data.Bits (countLeadingZeros, finiteBitSize, shiftL, shiftR, (.&.))
import Data.List (nub)
import qualified Data.IntSet as IntSet
import qualified Data.Vector.Unboxed.Mutable as MUV
import Moonlight.Planar.Internal.BoxedPaged (boxedPagedLength)
import Moonlight.Planar.Internal.BoundaryCycle (orderedPair)
import Moonlight.Planar.Internal.Paged (Paged, pagedFoldl', pagedLength, pagedUnsafeIndex)
import Moonlight.Planar.Dcel
import Moonlight.Planar.Internal.HandleDefs
import Moonlight.Planar.Internal.Incidence (nativePlanarIncidence, validatePlanarIncidence, validateIncidenceEdgeLinks)
import Moonlight.Planar.Handles.Iterators.FixedIterators (allFaces, undirectedEdges)
import Moonlight.Planar.Internal.PackedIndex (noIndex, packIndex, unpackIndex)
import Moonlight.Planar.Internal.Predicates
import Moonlight.Planar.Math
  ( squaredDistance
  , triangleArea
  )
import Moonlight.Planar.Internal.Representation
import Moonlight.Planar.Internal.Types

-- | Native cardinality/triangle/geometry laws around the single finite
-- incidence admission boundary. Unsafe geometry follows successful incidence.
validateTopology :: Triangulation mode vertex directed undirected face -> [InvariantViolation]
validateTopology triangulation =
  structuralViolations ++ orientationViolations
 where
  verticesCount = numVertices triangulation
  halfCount = numDirectedEdges triangulation
  edgeCount = numUndirectedEdges triangulation
  facesCount = numFaces triangulation

  -- Geometry descends only after the finite DCEL has glued structurally.
  -- Reading triangle coordinates through malformed links would turn a typed
  -- validation failure into an indexing crash.
  structuralViolations =
    cardinalityViolations
      ++ incidenceViolations
      ++ faceViolations
      ++ eulerViolations

  incidenceViolations
    | not (null cardinalityViolations) = []
    | otherwise = either (pure . IncidenceViolation) (const [])
        (validatePlanarIncidence (nativePlanarIncidence triangulation))

  orientationViolations
    | not (null structuralViolations) = []
    | otherwise =
        [ InnerFaceNotCounterClockwise face
        | face <- allFaces triangulation
        , face /= outerFace
        , Just (first, second, third) <- [innerFaceVertices triangulation face]
        , orient2d
            (vertexPoint triangulation first)
            (vertexPoint triangulation second)
            (vertexPoint triangulation third)
            /= GT
        ]

  cardinalityViolations =
    [ CoordinatePlaneLengthMismatch pointXCount pointYCount
    | pointXCount /= pointYCount
    ]
      ++ [VertexOutgoingLengthMismatch vertexOutCount verticesCount | vertexOutCount /= verticesCount]
      ++ [VertexPayloadLengthMismatch vertexPayloadCount verticesCount | vertexPayloadCount /= verticesCount]
      ++ [TopologyArenaLengthMismatch topologyLength (4 * halfCount) | not halfArraysEqual]
      ++ [DirectedPayloadLengthMismatch directedPayloadCount halfCount | directedPayloadCount /= halfCount]
      ++ [UndirectedPayloadLengthMismatch undirectedPayloadCount edgeCount | undirectedPayloadCount /= edgeCount]
      ++ [DirectedEdgeCountOdd halfCount | odd halfCount]
      ++ [ConstraintLengthMismatch constraintLength edgeCount | constraintLength /= edgeCount]
      ++ [ NonCanonicalConstraintFlag (UndirectedEdgeId (fromIntegral index)) flag
         | index <- [0 .. pagedLength (triConstraint triangulation) - 1]
         , let flag = pagedUnsafeIndex (triConstraint triangulation) index
         , flag /= 0 && flag /= 1
         ]
      ++ [CachedConstraintCountMismatch (triConstraintCount triangulation) actualConstraintCount | triConstraintCount triangulation /= actualConstraintCount]
      ++ [ CachedConstraintIndexMismatch
         | constraintLength == edgeCount
         , triConstraintEdges triangulation /= indexedConstraintEdges
         ]
      ++ [MissingOuterFace | facesCount == 0]
      ++ [FacePayloadLengthMismatch facePayloadCount facesCount | facePayloadCount /= facesCount]

  pointXCount = pagedLength (triPointX triangulation)
  pointYCount = pagedLength (triPointY triangulation)
  vertexOutCount = pagedLength (triVertexOut triangulation)
  vertexPayloadCount = boxedPagedLength (triVertexData triangulation)
  topologyLength = pagedLength (triHalfTopology triangulation)
  directedPayloadCount = boxedPagedLength (triDirectedData triangulation)
  undirectedPayloadCount = boxedPagedLength (triUndirectedData triangulation)
  constraintLength = pagedLength (triConstraint triangulation)
  facePayloadCount = boxedPagedLength (triFaceData triangulation)
  actualConstraintCount = pagedFoldl' (\count flag -> if flag == 1 then count + 1 else count) 0 (triConstraint triangulation)
  halfArraysEqual = topologyLength == 4 * halfCount

  indexedConstraintEdges =
    IntSet.fromAscList
      [ index
      | index <- [0 .. edgeCount - 1]
      , pagedUnsafeIndex (triConstraint triangulation) index == 1
      ]

  faceViolations
    | not (null cardinalityViolations) || not (null incidenceViolations) = []
    | otherwise =
        [ InnerFaceVertexCardinalityMismatch face (length faceVertexIds) (length (nub faceVertexIds))
        | face <- allFaces triangulation
        , face /= outerFace
        , let faceVertexIds = faceVertices triangulation face
        , length faceVertexIds /= 3 || length (nub faceVertexIds) /= 3
        ]

  eulerViolations
    | not (null cardinalityViolations) || verticesCount < 2 = []
    | numInnerFaces triangulation == 0 =
        [ CollinearEdgeCountMismatch (verticesCount - 1) edgeCount
        | edgeCount /= verticesCount - 1
        ]
    | otherwise =
        [ EulerCharacteristicMismatch eulerCharacteristic
        | eulerCharacteristic /= 2
        ]
   where
    eulerCharacteristic = verticesCount - edgeCount + facesCount

-- | Apply ranges, shared incidence link laws, triangle/representative laws and
-- orientation to a certified local closure. Whole-value incidence coverage is
-- an obligation of the admitted source, not a scan repeated for each edit. The caller
-- supplies the admitted inner faces and the interface pairs; the collar faces
-- on the other side of those pairs are included here so a protected source
-- cannot be changed behind the local transaction.  Cardinality and Euler
-- observations remain global scalar invariants and are deliberately not
-- rebuilt from the resident mesh.
validateTopologyClosure
  :: IntSet.IntSet
  -> IntSet.IntSet
  -> Triangulation mode vertex directed undirected face
  -> [InvariantViolation]
validateTopologyClosure admittedFaces interfacePairs triangulation =
  snd (validateTopologyClosureWithStats admittedFaces interfacePairs triangulation)

-- | The support a local domain spans: the admitted faces with the collar
-- faces across the interface, the directed edges those faces and interface
-- pairs carry, the vertices those edges join, and the undirected pairs
-- beneath them. A closure validation walks this selection; its statistics
-- are the selection's sizes and need no walk.
data ClosureSelection = ClosureSelection
  { selectionFaces :: !IntSet.IntSet
  , selectionEdges :: !IntSet.IntSet
  , selectionVertices :: !IntSet.IntSet
  , selectionPairs :: !IntSet.IntSet
  }

topologyClosureSelection
  :: IntSet.IntSet
  -> IntSet.IntSet
  -> Triangulation mode vertex directed undirected face
  -> ClosureSelection
topologyClosureSelection admittedFaces interfacePairs triangulation =
  ClosureSelection
    { selectionFaces = selectedFaces
    , selectionEdges = selectedEdges
    , selectionVertices = selectedVertices
    , selectionPairs = selectedPairs
    }
 where
  verticesCount = numVertices triangulation
  halfCount = numDirectedEdges triangulation

  collarFaces =
    IntSet.fromList
      [ rawFace
      | rawPair <- IntSet.toAscList interfacePairs
      , rawPair >= 0
      , rawPair < numUndirectedEdges triangulation
      , let edge = UndirectedEdgeId (fromIntegral rawPair)
      , let (forward, backward) = directedPair edge
      , rawFace <- fmap faceIdIndex [incidentFace triangulation forward, incidentFace triangulation backward]
      , rawFace > 0
      ]
  selectedFaces = IntSet.union admittedFaces collarFaces

  selectedFaceEdges =
    IntSet.fromList
      [ rawEdge
      | rawFace <- IntSet.toAscList selectedFaces
      , edge <- faceEdgesBounded triangulation (FaceId (fromIntegral rawFace))
      , rawEdge <- [directedEdgeIdIndex edge, directedEdgeIdIndex (reverseEdge edge)]
      ]
  selectedInterfaceEdges =
    IntSet.fromList
      [ rawEdge
      | rawPair <- IntSet.toAscList interfacePairs
      , rawPair >= 0
      , rawPair <= (maxBound - 1) `quot` 2
      , rawEdge <- [2 * rawPair, 2 * rawPair + 1]
      ]
  selectedEdges = IntSet.union selectedFaceEdges selectedInterfaceEdges
  selectedVertices =
    IntSet.fromList
      [ rawVertex
      | rawEdge <- IntSet.toAscList selectedEdges
      , rawEdge >= 0
      , rawEdge < halfCount
      , directedEdgeIdIndex (reverseEdge (DirectedEdgeId (fromIntegral rawEdge))) < halfCount
      , rawVertex <-
          [ vertexIdIndex (origin triangulation (DirectedEdgeId (fromIntegral rawEdge)))
          , vertexIdIndex (destination triangulation (DirectedEdgeId (fromIntegral rawEdge)))
          ]
      , rawVertex < verticesCount
      ]
  selectedPairs = IntSet.fromList [rawEdge `quot` 2 | rawEdge <- IntSet.toAscList selectedEdges]

closureSelectionStats :: IntSet.IntSet -> ClosureSelection -> ClosureStats
closureSelectionStats interfacePairs selection =
  ClosureStats
    { closureFaces = IntSet.size (selectionFaces selection)
    , closureDirectedEdges = IntSet.size (selectionEdges selection)
    , closureVertices = IntSet.size (selectionVertices selection)
    , closureInterfacePairs = IntSet.size interfacePairs
    , closureConstraintPairs = IntSet.size (selectionPairs selection)
    }

-- | The sizes of the closure a local domain spans, read off the materialized
-- selection: what 'validateTopologyClosureWithStats' reports beside its
-- violations. The oracle 'topologyClosureStats' is held to.
topologyClosureSelectionStats
  :: IntSet.IntSet
  -> IntSet.IntSet
  -> Triangulation mode vertex directed undirected face
  -> ClosureStats
topologyClosureSelectionStats admittedFaces interfacePairs triangulation =
  closureSelectionStats interfacePairs (topologyClosureSelection admittedFaces interfacePairs triangulation)

-- | The same sizes counted from the domain's own faces and interface pairs,
-- for a caller whose value is already admitted: the initial and final
-- permitted faces are taken separately so their union is never built, every
-- directed edge is attributed to the one selected face whose cycle carries it
-- (its reverse to that face only when the face across is unselected), the
-- pairs are half the edges because the selection is closed under reversal,
-- and the vertices are the distinct origins of those edges, deduplicated in
-- a table sized by the closure rather than by the mesh. Equal to
-- 'topologyClosureSelectionStats' on the union wherever each cycle edge's
-- incident face is the face whose cycle it lies on, which every admitted
-- triangulation satisfies.
topologyClosureStats
  :: IntSet.IntSet
  -> IntSet.IntSet
  -> IntSet.IntSet
  -> Triangulation mode vertex directed undirected face
  -> ClosureStats
topologyClosureStats initialFaces finalFaces interfacePairs triangulation = runST $ do
  seen <- MUV.replicate capacity (-1)
  (!cycleEdges, !cycleVertices) <-
    foldM (visitFace seen) (0, 0) selectedFaceList
  (!edges, !verticesSeen) <-
    foldM (visitInterfacePair seen) (cycleEdges, cycleVertices) (IntSet.toAscList interfacePairs)
  pure
    ClosureStats
      { closureFaces = selectedFaceCount
      , closureDirectedEdges = edges
      , closureVertices = verticesSeen
      , closureInterfacePairs = IntSet.size interfacePairs
      , closureConstraintPairs = edges `quot` 2
      }
 where
  verticesCount = numVertices triangulation
  halfCount = numDirectedEdges triangulation
  undirectedCount = numUndirectedEdges triangulation

  collarFaces =
    IntSet.fromList
      [ rawFace
      | rawPair <- IntSet.toAscList interfacePairs
      , rawPair >= 0
      , rawPair < undirectedCount
      , let (forward, backward) = directedPair (UndirectedEdgeId (fromIntegral rawPair))
      , rawFace <- fmap faceIdIndex [incidentFace triangulation forward, incidentFace triangulation backward]
      , rawFace > 0
      ]
  admitted rawFace = IntSet.member rawFace initialFaces || IntSet.member rawFace finalFaces
  selected rawFace = admitted rawFace || IntSet.member rawFace collarFaces
  novelFinal = filter (\rawFace -> not (IntSet.member rawFace initialFaces)) (IntSet.toAscList finalFaces)
  novelCollar = filter (not . admitted) (IntSet.toAscList collarFaces)
  selectedFaceList = IntSet.toAscList initialFaces ++ novelFinal ++ novelCollar
  selectedFaceCount = IntSet.size initialFaces + length novelFinal + length novelCollar

  -- Every vertex of the closure is the origin of a selected edge and there
  -- are at most three per selected face plus two per interface pair, so a
  -- half-full table of that size always has a vacant slot.
  vertexBound = 3 * (IntSet.size initialFaces + IntSet.size finalFaces + IntSet.size collarFaces) + 2 * IntSet.size interfacePairs
  capacityBits = finiteBitSize (0 :: Int) - countLeadingZeros (max 8 (2 * vertexBound))
  capacity = 1 `shiftL` capacityBits
  slotOf :: Int -> Int
  slotOf rawVertex =
    fromIntegral ((fromIntegral rawVertex * (0x9E3779B97F4A7C15 :: Word)) `shiftR` (finiteBitSize (0 :: Word) - capacityBits))
      .&. (capacity - 1)

  visitFace :: MUV.MVector s Int -> (Int, Int) -> Int -> ST s (Int, Int)
  visitFace seen (!edges, !seenVertices) rawFace =
    case adjacentEdge triangulation (FaceId (fromIntegral rawFace)) of
      Nothing -> pure (edges, seenVertices)
      Just start
        | directedEdgeIdIndex start >= halfCount -> pure (edges, seenVertices)
        | otherwise -> do
            let second = next triangulation start
            first <- visitCycleEdge seen (edges, seenVertices) start
            if directedEdgeIdIndex second >= halfCount
              then pure first
              else do
                let third = next triangulation second
                secondVisited <- visitCycleEdge seen first second
                if directedEdgeIdIndex third >= halfCount
                  then pure secondVisited
                  else visitCycleEdge seen secondVisited third

  -- A cycle edge counts for its face; its reverse counts here only when the
  -- face across is outside the selection, otherwise that face's cycle counts it.
  visitCycleEdge :: MUV.MVector s Int -> (Int, Int) -> DirectedEdgeId -> ST s (Int, Int)
  visitCycleEdge seen (!edges, !seenVertices) edge = do
    let reversed = reverseEdge edge
    seenVertices' <- visitOrigin seen seenVertices edge
    if selected (faceIdIndex (incidentFace triangulation reversed))
      then pure (edges + 1, seenVertices')
      else do
        seenVertices'' <- visitOrigin seen seenVertices' reversed
        pure (edges + 2, seenVertices'')

  -- An interface edge is already counted through a selected face on either
  -- side; only a pair with neither side selected adds its two edges here.
  visitInterfacePair :: MUV.MVector s Int -> (Int, Int) -> Int -> ST s (Int, Int)
  visitInterfacePair seen (!edges, !seenVertices) rawPair
    | rawPair < 0 || rawPair > (maxBound - 1) `quot` 2 = pure (edges, seenVertices)
    | otherwise =
        foldM
          (\(!edgesSoFar, !seenSoFar) rawEdge ->
             if rawEdge < halfCount
               && ( selected (faceIdIndex (incidentFace triangulation (DirectedEdgeId (fromIntegral rawEdge))))
                      || selected (faceIdIndex (incidentFace triangulation (DirectedEdgeId (fromIntegral (rawEdge + 1 - 2 * (rawEdge `rem` 2))))))
                  )
               then pure (edgesSoFar, seenSoFar)
               else
                 if rawEdge < halfCount
                   then do
                     seenSoFar' <- visitOrigin seen seenSoFar (DirectedEdgeId (fromIntegral rawEdge))
                     pure (edgesSoFar + 1, seenSoFar')
                   else pure (edgesSoFar + 1, seenSoFar))
          (edges, seenVertices)
          [2 * rawPair, 2 * rawPair + 1]

  visitOrigin :: forall s. MUV.MVector s Int -> Int -> DirectedEdgeId -> ST s Int
  visitOrigin seen seenVertices edge
    | rawVertex >= verticesCount = pure seenVertices
    | otherwise = insertVertex (slotOf rawVertex)
   where
    !rawVertex = vertexIdIndex (origin triangulation edge)
    insertVertex :: Int -> ST s Int
    insertVertex !slot = do
      occupant <- MUV.unsafeRead seen slot
      if occupant < 0
        then seenVertices + 1 <$ MUV.unsafeWrite seen slot rawVertex
        else
          if occupant == rawVertex
            then pure seenVertices
            else insertVertex ((slot + 1) .&. (capacity - 1))

-- | A bounded face's cycle edges, at most three, stopping at the first
-- handle beyond the arena.
faceEdgesBounded
  :: Triangulation mode vertex directed undirected face
  -> FaceId
  -> [DirectedEdgeId]
faceEdgesBounded triangulation face =
  case face of
    FaceId raw
      | toInteger raw <= 0 || toInteger raw >= toInteger (numFaces triangulation) -> []
    _ -> adjacentEdges
 where
  halfCount = numDirectedEdges triangulation
  adjacentEdges = case adjacentEdge triangulation face of
    Nothing -> []
    Just start
      | directedEdgeIdIndex start >= halfCount -> []
      | otherwise ->
          let second = next triangulation start
           in if directedEdgeIdIndex second >= halfCount
                then [start]
                else
                  let third = next triangulation second
                   in if directedEdgeIdIndex third >= halfCount
                        then [start, second]
                        else [start, second, third]

validateTopologyClosureWithStats
  :: IntSet.IntSet
  -> IntSet.IntSet
  -> Triangulation mode vertex directed undirected face
  -> (ClosureStats, [InvariantViolation])
validateTopologyClosureWithStats admittedFaces interfacePairs triangulation =
  ( closureSelectionStats interfacePairs selection
  , rangeViolations ++ edgeViolations ++ faceViolations ++ vertexViolations ++ orientationViolations ++ constraintViolations
  )
 where
  verticesCount = numVertices triangulation
  halfCount = numDirectedEdges triangulation
  facesCount = numFaces triangulation

  selection = topologyClosureSelection admittedFaces interfacePairs triangulation
  selectedFaces = selectionFaces selection
  selectedEdges = selectionEdges selection
  selectedVertices = selectionVertices selection
  selectedPairs = selectionPairs selection

  rangeViolations =
    originViolations
      ++ nextViolations
      ++ previousViolations
      ++ faceRangeViolations
      ++ [ FaceAdjacentOutOfRange face edge halfCount
         | rawFace <- IntSet.toAscList selectedFaces
         , rawFace > 0
         , rawFace < facesCount
         , let face = FaceId (fromIntegral rawFace)
         , Just edge <- [adjacentEdge triangulation face]
         , directedEdgeIdIndex edge >= halfCount
         ]

  -- The selected edges descend once, from the largest handle, so each family
  -- is consed into ascending order as four separate walks would report it.
  (originViolations, nextViolations, previousViolations, faceRangeViolations) =
    selectedEdgeRange (IntSet.toDescList selectedEdges) [] [] [] []
   where
    selectedEdgeRange remaining origins nexts previouses faces =
      case remaining of
        [] -> (origins, nexts, previouses, faces)
        rawEdge : rest
          | rawEdge < 0 || rawEdge >= halfCount ->
              selectedEdgeRange rest origins nexts previouses faces
          | otherwise ->
              let !edge = DirectedEdgeId (fromIntegral rawEdge)
                  !vertex = origin triangulation edge
                  !nextEdge = next triangulation edge
                  !previousEdge = previous triangulation edge
                  !face = incidentFace triangulation edge
               in selectedEdgeRange
                    rest
                    ( if vertexIdIndex vertex >= verticesCount
                        then EdgeOriginOutOfRange edge vertex verticesCount : origins
                        else origins
                    )
                    ( if directedEdgeIdIndex nextEdge >= halfCount
                        then EdgeNextOutOfRange edge nextEdge halfCount : nexts
                        else nexts
                    )
                    ( if directedEdgeIdIndex previousEdge >= halfCount
                        then EdgePreviousOutOfRange edge previousEdge halfCount : previouses
                        else previouses
                    )
                    ( if faceIdIndex face >= facesCount
                        then EdgeFaceOutOfRange edge face facesCount : faces
                        else faces
                    )

  edgeViolations =
    concatMap validateEdge (IntSet.toAscList selectedEdges)

  validateEdge rawEdge
    | rawEdge < 0 || rawEdge >= halfCount = []
    | otherwise =
        let edge = DirectedEdgeId (fromIntegral rawEdge)
            nextEdge = next triangulation edge
            previousEdge = previous triangulation edge
            twinEdge = reverseEdge edge
            nextValid = directedEdgeIdIndex nextEdge < halfCount
            previousValid = directedEdgeIdIndex previousEdge < halfCount
            nextNextValid = nextValid && directedEdgeIdIndex (next triangulation nextEdge) < halfCount
            innerCycle =
              if incidentFace triangulation edge /= outerFace && nextValid && previousValid && nextNextValid
                then [InnerFaceNotTriangularAtEdge edge | next triangulation (next triangulation nextEdge) /= edge]
                else []
            linkViolations
              | nextValid && previousValid && directedEdgeIdIndex twinEdge < halfCount =
                  either (pure . IncidenceViolation) (const [])
                    (validateIncidenceEdgeLinks (nativePlanarIncidence triangulation) edge)
              | otherwise = []
         in linkViolations
              ++ [EdgeSelfLinkedNext edge | nextEdge == edge && halfCount > 2]
              ++ [EdgeSelfLinkedPrevious edge | previousEdge == edge && halfCount > 2]
              ++ innerCycle

  faceViolations = concatMap validateFace (IntSet.toAscList selectedFaces)

  validateFace rawFace
    | rawFace <= 0 || rawFace >= facesCount = []
    | otherwise =
      let face = FaceId (fromIntegral rawFace)
       in case adjacentEdge triangulation face of
          Nothing -> [FaceMissingAdjacentEdge face]
          Just edge
            | directedEdgeIdIndex edge >= halfCount -> []
            | otherwise ->
                let representedFace = incidentFace triangulation edge
                    (faceEdges, faceVertices') = triangleEdgesAndVertices face
                 in [FaceRepresentativeMismatch face edge representedFace | representedFace /= face]
                      ++ [ InnerFaceVertexCardinalityMismatch face (length faceVertices') (length (nub faceVertices'))
                         | rawFace > 0
                         , length faceVertices' /= 3 || length (nub faceVertices') /= 3
                         ]
                      ++ [InnerFaceNotTriangularAtEdge edge
                         | rawFace > 0
                         , not (triangleClosed faceEdges)
                         ]

  triangleEdgesAndVertices face =
    let edges = faceEdgesBounded triangulation face
        vertices' = fmap (vertexIdIndex . origin triangulation) edges
     in (edges, vertices')

  triangleClosed edges =
    case edges of
      [first, _, third] -> next triangulation third == first
      _ -> False

  vertexViolations = concatMap validateVertex (IntSet.toAscList selectedVertices)

  validateVertex
    :: Int
    -> [InvariantViolation]
  validateVertex rawVertex =
    let vertex = VertexId (fromIntegral rawVertex)
     in case vertexOutEdge triangulation vertex of
          Nothing
            | verticesCount > 1 -> [ConnectedVertexMissingOutgoing vertex]
            | otherwise -> []
          Just edge
            | directedEdgeIdIndex edge >= halfCount ->
                [VertexOutgoingOutOfRange vertex edge halfCount]
            | otherwise ->
                [ VertexOutgoingOriginMismatch vertex edge actualOrigin
                | let actualOrigin = origin triangulation edge
                , actualOrigin /= vertex
                ]

  orientationViolations =
    [ InnerFaceNotCounterClockwise face
    | rawFace <- IntSet.toAscList selectedFaces
    , rawFace > 0
    , let face = FaceId (fromIntegral rawFace)
    , let (_, vertices') = triangleEdgesAndVertices face
    , all (< verticesCount) vertices'
    , [first, second, third] <- [fmap (VertexId . fromIntegral) vertices']
    , orient2d (vertexPoint triangulation first) (vertexPoint triangulation second) (vertexPoint triangulation third) /= GT
    ]

  constraintViolations =
    [ NonCanonicalConstraintFlag edge flag
    | rawPair <- IntSet.toAscList selectedPairs
    , rawPair >= 0
    , let edge = UndirectedEdgeId (fromIntegral rawPair)
    , rawPair < numUndirectedEdges triangulation
    , let flag = pagedUnsafeIndex (triConstraint triangulation) rawPair
    , flag /= 0 && flag /= 1
    ]
      ++ [ CachedConstraintIndexMismatch
         | rawPair <- IntSet.toAscList selectedPairs
         , rawPair >= 0
         , rawPair < numUndirectedEdges triangulation
         , let flag = pagedUnsafeIndex (triConstraint triangulation) rawPair
         , (flag == 1) /= IntSet.member rawPair (triConstraintEdges triangulation)
         ]

-- | Every edge whose circumcircle is not empty.
validateDelaunay :: Triangulation mode vertex directed undirected face -> [InvariantViolation]
validateDelaunay triangulation = concatMap validateEdge (undirectedEdges triangulation)
 where
  validateEdge edge
    | isConstraintEdge triangulation edge = []
    | isBoundaryEdge triangulation edge = []
    | otherwise =
        let directed = normalizedDirected edge
            twin = reverseEdge directed
         in case (innerFaceDirectedEdges triangulation (incidentFace triangulation directed), innerFaceDirectedEdges triangulation (incidentFace triangulation twin)) of
              (Just _, Just _) ->
                let a = vertexPoint triangulation (origin triangulation directed)
                    b = vertexPoint triangulation (destination triangulation directed)
                    c = vertexPoint triangulation (origin triangulation (previous triangulation directed))
                    d = vertexPoint triangulation (origin triangulation (previous triangulation twin))
                    convex = orient2d c d b == GT && orient2d d c a == GT
                    circle = inCircle a b c d
                    illegal = convex && (circle == GT || (circle == EQ && orderedPair c d < orderedPair a b))
                 in [LocallyIllegalDelaunayEdge edge | illegal]
              _ -> [DelaunayIncidentFaceNotTriangular edge]

-- | Topology first; the Delaunay property only if the topology holds.
validateTriangulation
  :: Triangulation mode vertex directed undirected face
  -> [InvariantViolation]
validateTriangulation triangulation =
  let topology = validateTopology triangulation
   in if null topology
        then validateDelaunay triangulation
        else topology

-- | Whether 'validateTriangulation' is empty.
triangulationIsValid
  :: Triangulation mode vertex directed undirected face
  -> Bool
triangulationIsValid = null . validateTriangulation

-- | Signed area, or 'Nothing' where the face is not a triangle.
faceArea :: Triangulation mode vertex directed undirected face -> FaceId -> Maybe Double
faceArea triangulation face = do
  (v0, v1, v2) <- innerFaceVertices triangulation face
  pure (triangleArea (vertexPoint triangulation v0) (vertexPoint triangulation v1) (vertexPoint triangulation v2))

-- | Smallest interior angle, in degrees.
faceMinimumAngleDegrees :: Triangulation mode vertex directed undirected face -> FaceId -> Maybe Double
faceMinimumAngleDegrees triangulation face = do
  (v0, v1, v2) <- innerFaceVertices triangulation face
  let p0 = vertexPoint triangulation v0
      p1 = vertexPoint triangulation v1
      p2 = vertexPoint triangulation v2
      a = sqrt (squaredDistance p1 p2)
      b = sqrt (squaredDistance p2 p0)
      c = sqrt (squaredDistance p0 p1)
  if min a (min b c) <= 0
    then Nothing
    else Just (minimum [angle b c a, angle c a b, angle a b c])
 where
  angle left right opposite = acos (clamp ((left * left + right * right - opposite * opposite) / (2 * left * right))) * 180 / pi
  clamp = max (-1) . min 1

-- | Decide, in one linear pass and without allocating, whether a
-- triangulation already has the resident numbering published by the build
-- component's canonicalizer:
-- sites in strict lexicographic order; undirected edges in strict
-- lexicographic order of their endpoint pair with the even half leaving the
-- lower endpoint; every vertex and face anchored at its least half-edge;
-- inner faces in order of those anchors, edgeless faces trailing. The arenas
-- are the only evidence read; no carried claim is consulted. Where
-- canonical rebuilding would be the identity this mints the same witness for the cost
-- of the scan rather than the copy.
canonicalAdmission :: Triangulation mode vertex directed undirected face -> CanonicalAdmission
canonicalAdmission source
  | coordinatesAscend vertexTotal coordinateX coordinateY
      && edgesRanked 1
      && halvesLeaveLower 0
      && anchorsOwned 0
      && anchorsLeast 0
      && innerFacesRanked 2 =
      CanonicalKnown
  | otherwise = CanonicalUnknown
 where
  !vertexTotal = numVertices source
  !edgeTotal = numUndirectedEdges source
  !faceTotal = numFaces source
  !directedTotal = 2 * edgeTotal
  !coordinateX = triPointX source
  !coordinateY = triPointY source
  !topologyArena = triHalfTopology source
  !vertexOutArena = triVertexOut source
  !faceEdgeArena = triFaceEdge source
  topology slot = fromIntegral (topologyArena `pagedUnsafeIndex` slot) :: Int
  originOf directed = topology (4 * directed)
  faceOf directed = topology (4 * directed + 3)
  vertexOut vertex = vertexOutArena `pagedUnsafeIndex` vertex
  faceEdge face = faceEdgeArena `pagedUnsafeIndex` face
  edgeLow edge = min (originOf (2 * edge)) (originOf (2 * edge + 1))
  edgeHigh edge = max (originOf (2 * edge)) (originOf (2 * edge + 1))

  edgesRanked !edge
    | edge >= edgeTotal = True
    | otherwise =
        let !previousLow = edgeLow (edge - 1)
            !currentLow = edgeLow edge
         in ( previousLow < currentLow
                || (previousLow == currentLow && edgeHigh (edge - 1) < edgeHigh edge)
            )
              && edgesRanked (edge + 1)

  halvesLeaveLower !edge
    | edge >= edgeTotal = True
    | otherwise = originOf (2 * edge) <= originOf (2 * edge + 1) && halvesLeaveLower (edge + 1)

  -- Each anchor names a half-edge that really leaves its vertex or bounds its
  -- face; together with 'anchorsLeast' that makes it the least such one.
  anchorsOwned !vertex
    | vertex < vertexTotal =
        let !anchor = vertexOut vertex
         in (anchor == noIndex || (unpackIndex anchor < directedTotal && originOf (unpackIndex anchor) == vertex))
              && anchorsOwned (vertex + 1)
    | vertex - vertexTotal < faceTotal =
        let !face = vertex - vertexTotal
            !anchor = faceEdge face
         in (anchor == noIndex || (unpackIndex anchor < directedTotal && faceOf (unpackIndex anchor) == face))
              && anchorsOwned (vertex + 1)
    | otherwise = True

  anchorsLeast !directed
    | directed >= directedTotal = True
    | otherwise =
        let !packed = packIndex directed
         in vertexOut (originOf directed) <= packed
              && faceEdge (faceOf directed) <= packed
              && anchorsLeast (directed + 1)

  innerFacesRanked !face
    | face >= faceTotal = True
    | otherwise =
        let !previousAnchor = faceEdge (face - 1)
            !currentAnchor = faceEdge face
         in (previousAnchor < currentAnchor || (previousAnchor == noIndex && currentAnchor == noIndex))
              && innerFacesRanked (face + 1)


-- | Whether the stored sites are already in strict lexicographic order, in
-- which case ranking them is the identity and both permutations are free.
--
-- Strict rather than non-strict: a triangulation stores each site once, so
-- equal adjacent coordinates would mean a mesh this function has no ordering
-- for, and it is the sort's business to say so rather than this predicate's.
coordinatesAscend :: Int -> Paged Double -> Paged Double -> Bool
coordinatesAscend total x y = go 1
 where
  go !index
    | index >= total = True
    | otherwise =
        let !previousX = x `pagedUnsafeIndex` (index - 1)
            !currentX = x `pagedUnsafeIndex` index
         in case compare previousX currentX of
              LT -> go (index + 1)
              GT -> False
              EQ ->
                y `pagedUnsafeIndex` (index - 1) < y `pagedUnsafeIndex` index
                  && go (index + 1)
