{-# LANGUAGE DataKinds #-}

-- | Input generation, canonical encoding, and the shared build helpers.
module Moonlight.Triangulation.Bench.SpadeCompare.Support where

import Data.List (sort)
import Data.Word (Word64)
import GHC.Float (castDoubleToWord64)
import Numeric (showHex)
import qualified Data.Vector as V
import Moonlight.Triangulation
import Moonlight.Triangulation.Bench.SpadeCompare.Input (randomPoints)
import Moonlight.Triangulation.HintGenerator

-- | The same generated points flattened onto a sliver a picometre thick. No
-- coordinate becomes equal to another and no four points become cocircular, so
-- the input stays in general position and the load stays on the ordinary
-- face-building path; what collapses is the angular spread the bulk loaders
-- index their hulls by. That collapse is the whole of what this input
-- measures, and it does not reach the face-less location path — that one needs
-- exact collinearity, which 'exactlyCollinearPoints' supplies and gates.
nearCollinearPoints :: Word64 -> Int -> [Point]
nearCollinearPoints seed count =
  [Point x (y * 1.0e-12) | Point x y <- randomPoints seed count]

-- | Points on one exact line: @(i, 0)@, every coordinate an integer that a
-- 'Double' holds exactly. No three are in general position, so 'orient2d'
-- returns an exact zero on both sides and no face is ever built.
--
-- Uniqueness does not need general position, only one valid answer. A
-- collinear set admits exactly one triangulation — the chain of consecutive
-- segments, @n - 1@ of them and no triangle — so the canonical edge set is
-- fully determined and gates bit for bit against the strict side. Both
-- implementations state that same count as their own degenerate Euler
-- invariant, which is what makes this a shared answer rather than a
-- coincidence.
exactlyCollinearPoints :: Int -> [Point]
exactlyCollinearPoints count = [Point (fromIntegral index) 0 | index <- [0 .. count - 1]]

hex64 :: Double -> String
hex64 value =
  let raw = showHex (castDoubleToWord64 value) ""
   in replicate (16 - length raw) '0' <> raw

-- | Canonical edge set: each undirected edge as its two endpoint coordinates in
-- IEEE-754 bit patterns, endpoint-ordered then globally sorted. Formatting
-- cannot introduce a spurious disagreement and rounding cannot hide a real one.
canonicalEdges :: Triangulation mode vertex directed undirected face -> [String]
canonicalEdges triangulation =
  sort
    [ encodeCanonicalEdge first second
    | edge <- undirectedEdges triangulation
    , let (from, to) = undirectedEndpoints triangulation edge
          p = vertexPoint triangulation from
          q = vertexPoint triangulation to
          (first, second) = if key p <= key q then (p, q) else (q, p)
    ]
 where
  key (Point x y) = (x, y)

canonicalConstraintEdges
  :: ConstrainedDelaunayTriangulation vertex
  -> [String]
canonicalConstraintEdges triangulation =
  sort
    [ encodeCanonicalEdge first second
    | edge <- constraintEdges triangulation
    , let (from, to) = undirectedEndpoints triangulation edge
          p = vertexPoint triangulation from
          q = vertexPoint triangulation to
          (first, second) =
            if pointKey p <= pointKey q
              then (p, q)
              else (q, p)
    ]
 where
  pointKey (Point x y) = (x, y)

encodeCanonicalEdge :: Point -> Point -> String
encodeCanonicalEdge (Point ax ay) (Point bx by) =
  concatMap hex64 [ax, ay, bx, by]

coordHex :: Point -> String
coordHex (Point x y) = hex64 x <> hex64 y

-- | A cycle, rotated to begin at its smallest encoded element.
--
-- Both implementations keep one entry edge per vertex and per face — whichever
-- their builder wrote last — so the two walk the same cycle from different
-- places. Rotating pins the order, which the mesh determines, and declines to
-- pin the starting point, which it does not. The rotation compares the encoded
-- strings rather than the coordinates so that both sides order by the same
-- comparison: the bit pattern of a negative double does not sort in numeric
-- order, so a Haskell 'Ord' on 'Point' and a Rust @partial_cmp@ on a tuple
-- would each have to be argued to agree where two byte strings simply do.
rotateToSmallest :: [String] -> [String]
rotateToSmallest [] = []
rotateToSmallest ring =
  let !smallest = minimum ring
      (before, after) = break (== smallest) ring
   in after <> before

require :: Show failure => Either failure value -> IO value
require (Left failure) = fail (show failure)
require (Right value) = pure value

requireQueryPoints
  :: Traversable container
  => container Point
  -> IO (container QueryPoint)
requireQueryPoints = require . traverse mkQueryPoint

requireQueryChords
  :: Traversable container
  => container (Point, Point)
  -> IO (container (QueryPoint, QueryPoint))
requireQueryChords =
  require
    . traverse
      (\(from, to) -> (,) <$> mkQueryPoint from <*> mkQueryPoint to)

delaunayOf :: Int -> IO (DelaunayTriangulation Point)
delaunayOf count =
  buildTriangulation <$> require (delaunay unitElementDefaults (V.fromList (randomPoints 0x9e3779b97f4a7c15 count)))

-- | One line per query: the coordinates of the nearest vertex the hierarchy
-- routed the search to, or @none@.
nearestAnswer
  :: DelaunayTriangulation Point
  -> HierarchyHint
  -> QueryPoint
  -> String
nearestAnswer triangulation hierarchy query =
  let hint = case hierarchyHint hierarchy query of
        Just (VertexHint vertex) -> Just vertex
        _ -> Nothing
   in case nearestNeighbor triangulation hint query of
        Nothing -> "none"
        Just (vertex, _) ->
          let Point x y = vertexPoint triangulation vertex
           in hex64 x <> hex64 y

pointHex :: Point -> String
pointHex (Point x y) = hex64 x <> "," <> hex64 y

constraintPairs :: Int -> Int -> [(Int, Int)]
constraintPairs pointCount constraintCount =
  take constraintCount
    [ (a, b)
    | index <- [0 ..]
    , let a = index `mod` pointCount
          b = (index * 6151 + pointCount `quot` 2) `mod` pointCount
    , a /= b
    ]

-- | A closed square with no dangling constraint. The interior segment used by
-- the package's own benchmark is outside spade's documented "closed shape"
-- contract and makes it refuse the whole domain — not a workload the two can be
-- timed on.
refinementInput :: (V.Vector Point, V.Vector (Int, Int))
refinementInput =
  ( V.fromList [Point 0 0, Point 64 0, Point 64 64, Point 0 64]
  , V.fromList [(0, 1), (1, 2), (2, 3), (3, 0)]
  )
