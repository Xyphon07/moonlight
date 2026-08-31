{-# LANGUAGE DataKinds #-}

-- | The Voronoi dual gate and the dual sweep lane.
module Moonlight.Triangulation.Bench.SpadeCompare.Voronoi where

import Data.List (sort)
import System.FilePath ((</>))
import Moonlight.Triangulation
import Moonlight.Triangulation.Dcel
import Moonlight.Triangulation.Voronoi
import Moonlight.Triangulation.Bench.SpadeCompare.Support

-- | The dual, pinned by its combinatorics and by nothing else.
--
-- Every verb the dual offers resolves a primal element, so its answers are
-- exact over a mesh the bulk-load gate already pins bit for bit. Voronoi vertex
-- POSITIONS are the exception and are deliberately absent from every artifact
-- here: a position is a circumcentre, and the two implementations do not form
-- one the same way. spade takes the reciprocal of the determinant and
-- multiplies by it; this side first rescales both difference vectors by their
-- largest component and only then divides. Each rounds where the other does
-- not, so the positions cannot be bit-identical, and a gate over them would be
-- grading the arithmetic rather than the dual. What IS determined about a
-- position is whether it exists at all, and the vertex artifact pins that.
--
-- Direction vectors are pinned in full: each component is one subtraction of
-- two stored coordinates on both sides, so there is nothing left to round.
writeVoronoiGate :: FilePath -> Int -> IO ()
writeVoronoiGate directory count = do
  triangulation <- delaunayOf count
  let prefix = directory </> ("voronoi-" <> show count)
  writeFile
    (prefix <> "-dual.txt")
    (unlines (sort (map (dualEdgeRecord triangulation) (directedVoronoiEdges triangulation))))
  writeFile (prefix <> "-cells.txt") (unlines (voronoiCellLines triangulation))
  writeFile (prefix <> "-vertices.txt") (unlines (voronoiVertexLines triangulation))

-- | One directed dual edge: the primal edge it names, the site of the dual face
-- to its left, the dual vertex at each end, the destinations its dual @next@
-- and @prev@ reach, and its direction vector. @next@ and @prev@ rotate about
-- the primal origin, so naming them by destination alone loses nothing.
dualEdgeRecord
  :: DelaunayTriangulation Point
  -> DirectedVoronoiEdgeId
  -> String
dualEdgeRecord triangulation edge =
  concat
    [ coordHex (site (origin triangulation primal))
    , coordHex (site (destination triangulation primal))
    , coordHex (site (voronoiFaceSite (voronoiIncidentFace triangulation edge)))
    , dualVertexName triangulation primal (voronoiFrom triangulation edge)
    , dualVertexName triangulation primal (voronoiTo triangulation edge)
    , coordHex (site (destination triangulation (asDelaunayDirectedEdge (voronoiNext triangulation edge))))
    , coordHex (site (destination triangulation (asDelaunayDirectedEdge (voronoiPrevious triangulation edge))))
    , coordHex (voronoiDirectionVector triangulation edge)
    ]
 where
  primal = asDelaunayDirectedEdge edge
  site = vertexPoint triangulation

-- | An inner dual vertex named by the one corner of its dual face that is not
-- an endpoint of the edge the question was asked about — the record already
-- carries the other two, so the apex completes the face. An outer dual vertex
-- is the edge itself and needs no name.
--
-- The @X@ tag cannot be reached: an inner dual vertex names an inner face by
-- construction, and 'innerFaceVertices' refuses only the outer one. spade
-- cannot emit it at all, its dual vertex carrying a face handle already typed
-- inner, so an @X@ appearing here is a disagreement rather than a crash.
dualVertexName
  :: DelaunayTriangulation Point
  -> DirectedEdgeId
  -> VoronoiVertexId
  -> String
dualVertexName _ _ (OuterVoronoiVertex _) = "O"
dualVertexName triangulation primal (InnerVoronoiVertex face) =
  case innerFaceVertices triangulation face of
    Nothing -> "X"
    Just (a, b, c)
      | isApex a -> named a
      | isApex b -> named b
      | otherwise -> named c
 where
  from = origin triangulation primal
  to = destination triangulation primal
  isApex vertex = vertex /= from && vertex /= to
  named vertex = "I" <> coordHex (vertexPoint triangulation vertex)

-- | The dual faces, one line each, plus the three enumerator lengths. The
-- undirected dual carries no artifact of its own: it is the primal undirected
-- edge set relabelled, which every existing edge gate already pins, so its
-- length is the only claim left to make about it.
voronoiCellLines :: DelaunayTriangulation Point -> [String]
voronoiCellLines triangulation =
  [ "faces " <> show (length (voronoiFaces triangulation))
  , "directed " <> show (length (directedVoronoiEdges triangulation))
  , "undirected " <> show (length (undirectedVoronoiEdges triangulation))
  ]
    <> sort
      [ coordHex (vertexPoint triangulation (voronoiFaceSite face)) <> concat (rotateToSmallest ring)
      | face <- voronoiFaces triangulation
      , let ring =
              [ coordHex
                  ( vertexPoint
                      triangulation
                      (destination triangulation (asDelaunayDirectedEdge edge))
                  )
              | edge <- voronoiFaceAdjacentEdges triangulation face
              ]
      ]

-- | The inner dual vertices, keyed by their dual face's three corners. Each
-- line carries whether the vertex has a position — the only exactly determined
-- thing about a circumcentre — and the outgoing triple the dual reports.
voronoiVertexLines :: DelaunayTriangulation Point -> [String]
voronoiVertexLines triangulation =
  sort
    [ concat (sort (map coordHex [site a, site b, site c]))
        <> existence dual
        <> concat (rotateToSmallest (outgoing dual))
    | face <- innerFaces triangulation
    , Just (a, b, c) <- [innerFaceVertices triangulation face]
    , let dual = InnerVoronoiVertex face
    ]
 where
  site = vertexPoint triangulation

  existence dual = case voronoiVertexPosition triangulation dual of
    Nothing -> "N"
    Just _ -> "J"

  outgoing dual = case voronoiVertexOutgoingEdges triangulation dual of
    Nothing -> []
    Just edges ->
      [ coordHex (site (origin triangulation primal)) <> coordHex (site (destination triangulation primal))
      | edge <- edges
      , let primal = asDelaunayDirectedEdge edge
      ]

-- | The whole dual, walked the way spade's own documentation walks it: every
-- dual face, its adjacent dual edges, and the dual vertex each of those runs
-- to. The position of an inner dual vertex is summed rather than merely
-- reached, because a 'Maybe' scrutinised only to its outer constructor can wrap
-- a thunk and the lane would then be timing the allocation of a circumcentre
-- instead of the circumcentre. Unbounded cells are counted, so a dual that
-- stopped classifying hull edges as outer moves the answer rather than the cost.
voronoiSweep :: DelaunayTriangulation Point -> (Double, Int)
voronoiSweep triangulation = foldl' overCell (0, 0) (voronoiFaces triangulation)
 where
  overCell accumulator face =
    foldl' overEdge accumulator (voronoiFaceAdjacentEdges triangulation face)

  overEdge (!total, !unbounded) edge =
    case voronoiVertexPosition triangulation (voronoiTo triangulation edge) of
      Nothing -> (total, unbounded + 1)
      Just (Point x y) -> (total + x + y, unbounded)
