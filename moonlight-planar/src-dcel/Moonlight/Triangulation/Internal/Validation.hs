-- | Discharge: the invariants the constructors guarantee, checkable on a value
-- built by any route.
module Moonlight.Triangulation.Internal.Validation
  ( validateTopology
  , ValidationClosureStats (..)
  , validateTopologyClosure
  , validateTopologyClosureWithStats
  , validateDelaunay
  , validateTriangulation
  , triangulationIsValid
  , faceArea
  , faceMinimumAngleDegrees
  ) where

import Data.List (nub)
import qualified Data.IntSet as IntSet
import Moonlight.Triangulation.Internal.BoxedPaged (boxedPagedLength)
import Moonlight.Triangulation.Internal.BoundaryCycle (orderedPair)
import Moonlight.Triangulation.Internal.Paged (pagedFoldl', pagedLength, pagedUnsafeIndex)
import Moonlight.Triangulation.Dcel
import Moonlight.Triangulation.Internal.HandleDefs
import Moonlight.Triangulation.Handles.Iterators.FixedIterators (allFaces, directedEdges, undirectedEdges, vertices)
import Moonlight.Triangulation.Internal.PackedIndex (noIndex, unpackIndex)
import Moonlight.Triangulation.Internal.Predicates
import Moonlight.Triangulation.Math
  ( squaredDistance
  , triangleArea
  )
import Moonlight.Triangulation.Internal.Representation
import Moonlight.Triangulation.Internal.Types

-- | Every structural invariant violated, not the first.
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
      ++ rangeViolations
      ++ edgeViolations
      ++ faceViolations
      ++ vertexViolations
      ++ eulerViolations

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

  rangeViolations =
    [ EdgeOriginOutOfRange (DirectedEdgeId (fromIntegral index)) (VertexId value) verticesCount
    | index <- [0 .. halfCount - 1]
    , let value = pagedUnsafeIndex (triHalfTopology triangulation) (4 * index)
    , unpackIndex value >= verticesCount
    ]
      ++ [ EdgeNextOutOfRange (DirectedEdgeId (fromIntegral index)) (DirectedEdgeId value) halfCount
         | index <- [0 .. halfCount - 1]
         , let value = pagedUnsafeIndex (triHalfTopology triangulation) (4 * index + 1)
         , unpackIndex value >= halfCount
         ]
      ++ [ EdgePreviousOutOfRange (DirectedEdgeId (fromIntegral index)) (DirectedEdgeId value) halfCount
         | index <- [0 .. halfCount - 1]
         , let value = pagedUnsafeIndex (triHalfTopology triangulation) (4 * index + 2)
         , unpackIndex value >= halfCount
         ]
      ++ [ EdgeFaceOutOfRange (DirectedEdgeId (fromIntegral index)) (FaceId value) facesCount
         | index <- [0 .. halfCount - 1]
         , let value = pagedUnsafeIndex (triHalfTopology triangulation) (4 * index + 3)
         , unpackIndex value >= facesCount
         ]
      ++ [ VertexOutgoingOutOfRange (VertexId (fromIntegral index)) (DirectedEdgeId value) halfCount
         | index <- [0 .. pagedLength (triVertexOut triangulation) - 1]
         , let value = pagedUnsafeIndex (triVertexOut triangulation) index
         , value /= noIndex
         , unpackIndex value >= halfCount
         ]
      ++ [ FaceAdjacentOutOfRange (FaceId (fromIntegral index)) (DirectedEdgeId value) halfCount
         | index <- [0 .. facesCount - 1]
         , let value = pagedUnsafeIndex (triFaceEdge triangulation) index
         , value /= noIndex
         , unpackIndex value >= halfCount
         ]

  edgeViolations
    | not halfArraysEqual || odd halfCount || not (null rangeViolations) = []
    | otherwise = concatMap validateEdge (directedEdges triangulation)

  validateEdge edge =
    let index = directedEdgeIdIndex edge
        nextEdge = next triangulation edge
        previousEdge = previous triangulation edge
        twinEdge = reverseEdge edge
        local =
          [ EdgeNextPreviousMismatch edge nextEdge
          | validEdge nextEdge && previous triangulation nextEdge /= edge
          ]
            ++ [ EdgePreviousNextMismatch edge previousEdge
               | validEdge previousEdge && next triangulation previousEdge /= edge
               ]
            ++ [EdgeDoubleReversalMismatch edge | reverseEdge twinEdge /= edge]
            ++ [EdgeSelfLinkedNext edge | nextEdge == edge && halfCount > 2]
            ++ [EdgeSelfLinkedPrevious edge | previousEdge == edge && halfCount > 2]
        innerCycle =
          if incidentFace triangulation edge /= outerFace && validEdge nextEdge && validEdge previousEdge
            then
              [ InnerFaceNotTriangularAtEdge edge
              | next triangulation (next triangulation nextEdge) /= edge
              ]
            else []
     in if index < halfCount then local ++ innerCycle else []

  validEdge edge = directedEdgeIdIndex edge < halfCount

  faceViolations
    | not (null rangeViolations) = []
    | otherwise = concatMap validateFace (allFaces triangulation)
  validateFace face@(FaceId _) =
    case adjacentEdge triangulation face of
      Nothing
        | face == outerFace && halfCount == 0 -> []
        | otherwise -> [FaceMissingAdjacentEdge face]
      Just edge ->
        [ FaceRepresentativeMismatch face edge representedFace
        | let representedFace = incidentFace triangulation edge
        , representedFace /= face
        ]
          ++ [ InnerFaceVertexCardinalityMismatch face (length faceVertexIds) (length (nub faceVertexIds))
             | face /= outerFace
             , let faceVertexIds = faceVertices triangulation face
             , length faceVertexIds /= 3 || length (nub faceVertexIds) /= 3
             ]

  vertexViolations
    | vertexOutCount /= verticesCount || not (null rangeViolations) = []
    | otherwise = concatMap validateVertex (vertices triangulation)
  validateVertex vertex = case vertexOutEdge triangulation vertex of
    Nothing
      | verticesCount <= 1 -> []
      | otherwise -> [ConnectedVertexMissingOutgoing vertex]
    Just edge ->
      [ VertexOutgoingOriginMismatch vertex edge actualOrigin
      | let actualOrigin = origin triangulation edge
      , actualOrigin /= vertex
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

-- | Apply the same DCEL edge, face, vertex, and orientation laws as
-- 'validateTopology', but only to a certified local closure.  The caller
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

validateTopologyClosureWithStats
  :: IntSet.IntSet
  -> IntSet.IntSet
  -> Triangulation mode vertex directed undirected face
  -> (ValidationClosureStats, [InvariantViolation])
validateTopologyClosureWithStats admittedFaces interfacePairs triangulation =
  ( ValidationClosureStats
      { validationClosureFaces = IntSet.size selectedFaces
      , validationClosureDirectedEdges = IntSet.size selectedEdges
      , validationClosureVertices = IntSet.size selectedVertices
      , validationClosureInterfacePairs = IntSet.size interfacePairs
      , validationClosureConstraintPairs = IntSet.size selectedPairs
      }
  , rangeViolations ++ edgeViolations ++ faceViolations ++ vertexViolations ++ orientationViolations ++ constraintViolations
  )
 where
  verticesCount = numVertices triangulation
  halfCount = numDirectedEdges triangulation
  facesCount = numFaces triangulation

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
      , edge <- faceEdgesBounded (FaceId (fromIntegral rawFace))
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

  rangeViolations =
    [ EdgeOriginOutOfRange edge vertex verticesCount
    | rawEdge <- IntSet.toAscList selectedEdges
    , rawEdge >= 0
    , let edge = DirectedEdgeId (fromIntegral rawEdge)
    , rawEdge < halfCount
    , let vertex = origin triangulation edge
    , vertexIdIndex vertex >= verticesCount
    ]
      ++ [ EdgeNextOutOfRange edge nextEdge halfCount
         | rawEdge <- IntSet.toAscList selectedEdges
         , rawEdge >= 0
         , rawEdge < halfCount
         , let edge = DirectedEdgeId (fromIntegral rawEdge)
         , let nextEdge = next triangulation edge
         , directedEdgeIdIndex nextEdge >= halfCount
         ]
      ++ [ EdgePreviousOutOfRange edge previousEdge halfCount
         | rawEdge <- IntSet.toAscList selectedEdges
         , rawEdge >= 0
         , rawEdge < halfCount
         , let edge = DirectedEdgeId (fromIntegral rawEdge)
         , let previousEdge = previous triangulation edge
         , directedEdgeIdIndex previousEdge >= halfCount
         ]
      ++ [ EdgeFaceOutOfRange edge face facesCount
         | rawEdge <- IntSet.toAscList selectedEdges
         , rawEdge >= 0
         , rawEdge < halfCount
         , let edge = DirectedEdgeId (fromIntegral rawEdge)
         , let face = incidentFace triangulation edge
         , faceIdIndex face >= facesCount
         ]
      ++ [ FaceAdjacentOutOfRange face edge halfCount
         | rawFace <- IntSet.toAscList selectedFaces
         , rawFace > 0
         , rawFace < facesCount
         , let face = FaceId (fromIntegral rawFace)
         , Just edge <- [adjacentEdge triangulation face]
         , directedEdgeIdIndex edge >= halfCount
         ]

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
         in [ EdgeNextPreviousMismatch edge nextEdge
            | nextValid && previous triangulation nextEdge /= edge
            ]
              ++ [ EdgePreviousNextMismatch edge previousEdge
                 | previousValid && next triangulation previousEdge /= edge
                 ]
              ++ [EdgeDoubleReversalMismatch edge | reverseEdge twinEdge /= edge]
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
    let edges = faceEdgesBounded face
        vertices' = fmap (vertexIdIndex . origin triangulation) edges
     in (edges, vertices')

  triangleClosed edges =
    case edges of
      [first, _, third] -> next triangulation third == first
      _ -> False

  vertexViolations = concatMap validateVertex (IntSet.toAscList selectedVertices)

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

  selectedPairs = IntSet.fromList [rawEdge `quot` 2 | rawEdge <- IntSet.toAscList selectedEdges]

  faceEdgesBounded face =
    case face of
      FaceId raw
        | toInteger raw <= 0 || toInteger raw >= toInteger facesCount -> []
      _ -> adjacentEdges
   where
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
