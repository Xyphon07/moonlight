{-# LANGUAGE DataKinds #-}

-- | Corridor-walk lanes, their inputs, and their gates.
module Moonlight.Triangulation.Bench.SpadeCompare.Intersection where

import Data.List (sortOn)
import Data.Word (Word64)
import System.FilePath ((</>))
import qualified Data.Vector as V
import Moonlight.Triangulation
import Moonlight.Triangulation.IntersectionIterator
import Moonlight.Triangulation.Bench.SpadeCompare.Support
import Moonlight.Triangulation.Bench.SpadeCompare.Input (randomPoints)

-- | Query endpoints pulled inward. The generator fills [-1, 1]^2, so a point
-- with both coordinates inside [-0.5, 0.5] is strictly inside the hull of a
-- few thousand of them and locating it lands on a face. A chord that starts
-- OUTSIDE the hull makes endpoint choice and local hull entry observable, so
-- those chords remain the separate "intersection-outside" cliff lane.
interiorQueryPoints :: Word64 -> Int -> [Point]
interiorQueryPoints seed count =
  [Point (0.5 * x) (0.5 * y) | Point x y <- randomPoints seed count]

-- | Query origins pushed away from the dense centre so the corpus repeatedly
-- exercises outside-hull entry rather than only face-to-face corridors.
exteriorQueryPoints :: Word64 -> Int -> [Point]
exteriorQueryPoints seed count =
  [Point (4 * x) (4 * y) | Point x y <- randomPoints seed count]

intersectionChords :: Int -> [(Point, Point)]
intersectionChords queryCount =
  zip
    (interiorQueryPoints 0x3141592653589793 queryCount)
    (interiorQueryPoints 0x2718281828459045 queryCount)

outsideChords :: Int -> [(Point, Point)]
outsideChords queryCount =
  zip
    (exteriorQueryPoints 0x243f6a8885a308d3 queryCount)
    (interiorQueryPoints 0x2718281828459045 queryCount)

-- | One line per query: the line's own endpoints, then its intersection
-- sequence in the order the walk reports it. Every branch of the walk turns on
-- an orientation predicate over a mesh that already agrees bit for bit, so the
-- whole sequence is exactly determined and crosses implementations — order
-- included, which is why nothing here is sorted.
writeIntersectionGate :: FilePath -> Int -> Int -> IO ()
writeIntersectionGate directory pointCount queryCount = do
  triangulation <- delaunayOf pointCount
  chords <- requireQueryChords (intersectionChords queryCount)
  writeFile
    (directory </> ("intersection-" <> show pointCount <> "-" <> show queryCount <> "-chords.txt"))
    (unlines (map (intersectionLine triangulation) chords))

writeIntersectionOutsideGate :: FilePath -> Int -> Int -> IO ()
writeIntersectionOutsideGate directory pointCount queryCount = do
  triangulation <- delaunayOf pointCount
  chords <- requireQueryChords (outsideChords queryCount)
  writeFile
    ( directory
        </> ("intersection-outside-" <> show pointCount <> "-" <> show queryCount <> ".txt")
    )
    (unlines (map (intersectionLine triangulation) chords))

-- | Lines whose endpoints are mesh vertices, which is the only way to reach the
-- vertex and overlap branches of the walk at all: over generated doubles no
-- chord lands exactly on a site, so a gate built from chords alone exercises
-- one branch of three. The first family joins vertices the constraint
-- generator's index pairs select, which starts and ends the walk on a vertex;
-- the second runs along an existing edge, which is the only shape that produces
-- an overlap. Both name their endpoints by coordinate, the second from a list
-- both sides sort by the same encoding, because vertex handles do not cross.
writeIntersectionVertexGate :: FilePath -> Int -> IO ()
writeIntersectionVertexGate directory pointCount = do
  triangulation <- delaunayOf pointCount
  let sites = V.fromList (randomPoints 0x9e3779b97f4a7c15 pointCount)
      joins =
        [ (sites V.! fromIndex, sites V.! toIndex)
        | (fromIndex, toIndex) <- constraintPairs pointCount intersectionVertexLines
        ]
      alongEdges = take intersectionVertexLines (sortedEdgeEndpoints triangulation)
  queryLines <- requireQueryChords (joins <> alongEdges)
  writeFile
    (directory </> ("intersection-vertexlines-" <> show pointCount <> ".txt"))
    (unlines (map (intersectionLine triangulation) queryLines))

intersectionVertexLines :: Int
intersectionVertexLines = 100

sortedEdgeEndpoints
  :: DelaunayTriangulation Point
  -> [(Point, Point)]
sortedEdgeEndpoints triangulation =
  map snd
    ( sortOn
        fst
        [ (coordHex first <> coordHex second, (first, second))
        | edge <- undirectedEdges triangulation
        , let (from, to) = undirectedEndpoints triangulation edge
              p = vertexPoint triangulation from
              q = vertexPoint triangulation to
              (first, second) = if pointKey p <= pointKey q then (p, q) else (q, p)
        ]
    )
 where
  pointKey (Point x y) = (x, y)

intersectionLine
  :: DelaunayTriangulation Point
  -> (QueryPoint, QueryPoint)
  -> String
intersectionLine triangulation (from, to) =
  coordHex (queryPointValue from)
    <> coordHex (queryPointValue to)
    <> concatMap (encodeIntersection triangulation) (lineIntersections triangulation from to)

encodeIntersection
  :: DelaunayTriangulation Point
  -> Intersection
  -> String
encodeIntersection triangulation event = case event of
  VertexIntersection vertex -> "V" <> coordHex (vertexPoint triangulation vertex)
  EdgeIntersection edge -> "E" <> directedHex edge
  EdgeOverlap edge -> "O" <> directedHex edge
 where
  directedHex edge =
    coordHex (vertexPoint triangulation (origin triangulation edge))
      <> coordHex (vertexPoint triangulation (destination triangulation edge))

-- | The corridor walk, counted. 'lineIntersections' materializes its answer, so
-- forcing the spine forces every step that produced it; spade's iterator is
-- lazy and is consumed by the same count.
intersectionWalk
  :: DelaunayTriangulation Point
  -> [(QueryPoint, QueryPoint)]
  -> Int
intersectionWalk triangulation =
  foldl' (\total (from, to) -> total + length (lineIntersections triangulation from to)) 0
