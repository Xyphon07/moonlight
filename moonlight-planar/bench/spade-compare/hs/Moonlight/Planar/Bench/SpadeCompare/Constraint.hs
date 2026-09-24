{-# LANGUAGE DataKinds #-}

-- | Constraint recovery and splitting lanes and their gates.
module Moonlight.Planar.Bench.SpadeCompare.Constraint where

import Data.Primitive.PrimArray (indexPrimArray, sizeofPrimArray)
import System.FilePath ((</>))
import qualified Data.Vector as V
import Moonlight.Planar.BulkLoad (delaunay)
import Moonlight.Planar.Cdt
import Moonlight.Planar.Dcel (previous)
import Moonlight.Planar.Handles.HandleDefs (VertexId)
import Moonlight.Planar.Handles.Iterators.FixedIterators (vertices)
import Moonlight.Planar.SetAlgebra (intersection)
import Moonlight.Planar.Types (ConstrainedDelaunayTriangulation, buildInputVertices, buildTriangulation, unitElementDefaults)
import Moonlight.Planar.Point (Point (..))
import Moonlight.Planar.Internal.HandleDefs
import Moonlight.Planar.Bench.SpadeCompare.Support
import Moonlight.Planar.Bench.SpadeCompare.Input (randomPoints)

-- | Disjoint split cells, stacked in y: the points, the vertical constraints
-- the build starts from, and the crossing requests. Band @i@ carries a
-- constraint from (16, 4i) to (16, 4i + 2) and a crossing request from
-- (0, 4i + a) to (32, 4i + b), with a and b jittered inside the band, so every
-- request crosses exactly one constraint and no request reaches another band.
--
-- The geometry is chosen so the split point is not merely close on the two
-- sides but identical. spade solves the two line equations by Cramer's rule and
-- this side walks the parametric form; here every coefficient either vanishes
-- or is a power of two, both reduce to one rounding of @u + (v - u) / 2@, and
-- neither can round it differently. A generic crossing would leave the two
-- formulas free to disagree in the last bit and there would be nothing to gate.
splitBands :: Int -> (V.Vector Point, V.Vector (Int, Int), [(Int, Int)])
splitBands bandCount =
  ( V.fromList (concatMap bandPoints (zip [0 ..] jitter))
  , V.fromList [(4 * index, 4 * index + 1) | index <- [0 .. bandCount - 1]]
  , [(4 * index + 2, 4 * index + 3) | index <- [0 .. bandCount - 1]]
  )
 where
  jitter = randomPoints 0x51ed270b7c1fd1a3 bandCount

  bandPoints (index, Point offsetX offsetY) =
    let base = 4 * fromIntegral (index :: Int)
     in [ Point 16 base
        , Point 16 (base + 2)
        , Point 0 (base + 1 + 0.5 * offsetX)
        , Point 32 (base + 1 + 0.5 * offsetY)
        ]

-- | The same constraint program the batch recovery runs, one request at a time
-- through the persistent singleton entry point. A request whose corridor is
-- blocked by an existing constraint is refused and the previous triangulation
-- stands, which is what the strict side's 'can_add_constraint' guard arranges.
addConstraintsIncrementally
  :: ConstrainedDelaunayTriangulation Point
  -> [(Point, Point)]
  -> (ConstrainedDelaunayTriangulation Point, [Int])
addConstraintsIncrementally base requests =
  case foldl' step (base, []) (zip [0 ..] requests) of
    (triangulation, accepted) -> (triangulation, reverse accepted)
 where
  step (triangulation, accepted) (index, (from, to)) =
    case addConstraintEdge triangulation from to of
      Left (ConstraintIntersection _) -> (triangulation, accepted)
      Left failure -> error (show failure)
      Right result -> (constraintRecoveryTriangulation result, index : accepted)

-- | Splitting requests, one per band, carried by the batch verb: one
-- transaction for every corridor, published once -- the shape of the strict
-- side's fold over one mutable CDT, which publishes nothing between splits.
-- Every crossed constraint is split at the intersection rather than refused,
-- so every request is accepted. Splitting appends vertices and never removes
-- one, so the handles resolved before the first request stay valid through
-- the last.
splitConstraints
  :: ConstrainedDelaunayTriangulation Point
  -> [(VertexId, VertexId)]
  -> ConstrainedDelaunayTriangulation Point
splitConstraints base requests =
  case addConstraintsAndSplit id base (V.fromList requests) of
    Left failure -> error (show failure)
    Right result -> constraintRecoveryTriangulation result

-- | The singleton constraint program's accepted request indices and final
-- constrained edge set. A blocked request must be refused at the same index on
-- both sides, or the two are not running the same program.
writeConstraintIncrementalGate :: FilePath -> Int -> Int -> IO ()
writeConstraintIncrementalGate directory pointCount constraintCount = do
  let points = V.fromList (randomPoints 0x94d049bb133111eb pointCount)
      requests =
        [ (points V.! fromIndex, points V.! toIndex)
        | (fromIndex, toIndex) <- constraintPairs pointCount constraintCount
        ]
  base <- fromDelaunay . buildTriangulation <$> require (delaunay unitElementDefaults points)
  let (recovered, accepted) = addConstraintsIncrementally base requests
      prefix =
        directory
          </> ("constraint-incremental-" <> show pointCount <> "-" <> show constraintCount)
  writeFile (prefix <> "-accepted.txt") (unlines (map show accepted))
  writeFile (prefix <> "-constraints.txt") (unlines (canonicalConstraintEdges recovered))

-- | Both the split vertices and the constrained edges they carve. The edge set
-- names every vertex by coordinate, so a split point that landed one ulp away
-- on one side shows up here rather than hiding behind a matching edge count.
writeConstraintSplitGate :: FilePath -> Int -> IO ()
writeConstraintSplitGate directory bandCount = do
  (base, handles) <- splitBandCdt bandCount
  let split = splitConstraints base handles
      prefix = directory </> ("constraint-split-" <> show bandCount)
  writeFile (prefix <> "-edges.txt") (unlines (canonicalEdges split))
  writeFile (prefix <> "-constraints.txt") (unlines (canonicalConstraintEdges split))

-- | The banded CDT and the crossing requests named by handle. Handle resolution
-- goes through the build's own input mapping rather than a locate, which is the
-- constraint gate's convention; the strict side has no such mapping and locates
-- instead. Either way both name the vertex standing at the same coordinate.
splitBandCdt
  :: Int
  -> IO (ConstrainedDelaunayTriangulation Point, [(VertexId, VertexId)])
splitBandCdt bandCount = do
  let (points, constraints, crossings) = splitBands bandCount
  built <- require (constrainedDelaunay unitElementDefaults points constraints)
  let mapping = buildInputVertices built
      len = sizeofPrimArray mapping
      resolve index
        | index >= 0 && index < len = VertexId (indexPrimArray mapping index)
        | otherwise = error ("split band endpoint is out of range: " <> show index)
  pure
    ( buildTriangulation built
    , [(resolve fromIndex, resolve toIndex) | (fromIndex, toIndex) <- crossings]
    )

writeConstraintGate :: FilePath -> IO ()
writeConstraintGate directory = do
  let points = V.fromList (randomPoints 0x94d049bb133111eb 8000)
      requestIndices = V.fromList (constraintPairs 8000 800)
  built <- require (delaunay unitElementDefaults points)
  let mapping = buildInputVertices built
      len = sizeofPrimArray mapping
  requests <-
    V.mapM
      (\(fromIndex, toIndex) ->
        let mFrom = if fromIndex >= 0 && fromIndex < len then Just (VertexId (indexPrimArray mapping fromIndex)) else Nothing
            mTo = if toIndex >= 0 && toIndex < len then Just (VertexId (indexPrimArray mapping toIndex)) else Nothing
        in case (mFrom, mTo) of
          (Just from, Just to) -> pure (from, to)
          _ -> fail "constraint gate endpoint is out of range"
      )
      requestIndices
  batch <-
    require
      ( recoverConstraints
          (fromDelaunay (buildTriangulation built))
          requests
      )
  let accepted =
        V.ifoldr
          (\index outcome indices ->
            case outcome of
              ConstraintAccepted _ _ -> index : indices
              ConstraintRejected _ -> indices
          )
          []
          (constraintBatchOutcomes batch)
  writeFile
    (directory </> "cdt-accepted-8000-800.txt")
    (unlines (map show accepted))
  writeFile
    (directory </> "cdt-constraints-8000-800.txt")
    (unlines (canonicalConstraintEdges (constraintBatchTriangulation batch)))
