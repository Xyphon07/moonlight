{-# LANGUAGE DataKinds #-}

-- | The primal DCEL traversal gate and the walk lane.
module Moonlight.Triangulation.Bench.SpadeCompare.Dcel where

import Data.List (sort)
import Moonlight.Triangulation.Handles.Iterators.FixedIterators (allFaces, directedEdges)
import Moonlight.Triangulation.Handles.Iterators.HullIterator (hullEdges)
import System.FilePath ((</>))
import Moonlight.Triangulation
import Moonlight.Triangulation.Dcel
import Moonlight.Triangulation.Bench.SpadeCompare.Support

-- | Every iterator over the mesh that spade also offers. The hull cycle and
-- each vertex's counterclockwise link are cycles with no determined starting
-- point and are rotated; the inner faces are a set and are sorted. The
-- enumerator lengths are stated because they are the only cross-comparable
-- claim the fixed and dynamic iterators make: their elements are handles, and
-- handles do not cross implementations.
writeDcelWalkGate :: FilePath -> Int -> IO ()
writeDcelWalkGate directory count = do
  triangulation <- delaunayOf count
  writeFile
    (directory </> ("dcel-hull-" <> show count <> ".txt"))
    (unlines (hullGateLines triangulation))
  writeFile
    (directory </> ("dcel-circular-" <> show count <> ".txt"))
    (unlines (circularGateLines triangulation))
  writeFile
    (directory </> ("dcel-faces-" <> show count <> ".txt"))
    (unlines (innerFaceGateLines triangulation))

hullGateLines :: DelaunayTriangulation Point -> [String]
hullGateLines triangulation =
  ("hull-size " <> show (length ring)) : rotateToSmallest ring
 where
  ring =
    [ coordHex (vertexPoint triangulation (origin triangulation edge))
        <> coordHex (vertexPoint triangulation (destination triangulation edge))
    | edge <- hullEdges triangulation
    ]

circularGateLines :: DelaunayTriangulation Point -> [String]
circularGateLines triangulation =
  [ "vertices " <> show (length (vertices triangulation))
  , "directed-edges " <> show (length (directedEdges triangulation))
  , "undirected-edges " <> show (length (undirectedEdges triangulation))
  , "all-faces " <> show (length (allFaces triangulation))
  , "inner-faces " <> show (length (innerFaces triangulation))
  ]
    <> sort
      [ coordHex (vertexPoint triangulation vertex) <> concat (rotateToSmallest ring)
      | vertex <- vertices triangulation
      , let ring =
              [ coordHex (vertexPoint triangulation (destination triangulation edge))
              | edge <- vertexOutgoingEdges triangulation vertex
              ]
      ]

innerFaceGateLines :: DelaunayTriangulation Point -> [String]
innerFaceGateLines triangulation =
  sort
    [ concat (sort (map (coordHex . vertexPoint triangulation) [a, b, c]))
    | face <- innerFaces triangulation
    , Just (a, b, c) <- [innerFaceVertices triangulation face]
    ]

-- | Every traversal the DCEL offers, in one pass: the hull cycle, each vertex's
-- counterclockwise link, and each inner face's vertex triple. The coordinates
-- are summed so that no step can be satisfied by a thunk.
dcelWalk :: DelaunayTriangulation Point -> (Double, Int)
dcelWalk triangulation = overFaces (overVertices (overHull (0, 0)))
 where
  overHull accumulator =
    foldl' (\acc edge -> charge acc (origin triangulation edge)) accumulator (hullEdges triangulation)

  overVertices accumulator = foldl' overLink accumulator (vertices triangulation)

  overLink accumulator vertex =
    foldl'
      (\acc edge -> charge acc (destination triangulation edge))
      accumulator
      (vertexOutgoingEdges triangulation vertex)

  overFaces accumulator = foldl' overCorners accumulator (innerFaces triangulation)

  overCorners accumulator face =
    case innerFaceVertices triangulation face of
      Nothing -> accumulator
      Just (a, b, c) -> foldl' charge accumulator [a, b, c]

  charge (!total, !count) vertex =
    let Point x y = vertexPoint triangulation vertex
     in (total + x + y, count + 1)
