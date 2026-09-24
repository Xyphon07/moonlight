{-# LANGUAGE DataKinds #-}

-- | Sibson interpolation lanes, gates, and diagnostics.
module Moonlight.Planar.Bench.SpadeCompare.Interpolation where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad.ST (stToIO)
import GHC.Exts (RealWorld)
import Data.Foldable (traverse_)
import Data.List (sort, sortOn)
import System.Exit (exitWith, ExitCode (ExitFailure))
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import qualified Data.Vector as V
import Moonlight.Planar.BulkLoad (delaunay, empty)
import Moonlight.Planar.Dcel
import Moonlight.Planar.Handles.HandleDefs (asUndirected, reverseEdge)
import Moonlight.Planar.Handles.Iterators.FixedIterators (vertices)
import Moonlight.Planar.Math (circumcenter, inCircle)
import Moonlight.Planar.Scalar (isFinite)
import Moonlight.Planar.PointLocation
import Moonlight.Planar.Types
import Moonlight.Planar.Point (Point (..), QueryPoint, queryPointValue)
import Moonlight.Planar.Telemetry (LocationStats (..), locatePointWithHint)
import Moonlight.Planar.Interpolation
import Moonlight.Planar.Internal.Predicates qualified as Predicates
import Moonlight.Planar.Bench.SpadeCompare.Support
import Moonlight.Planar.Bench.SpadeCompare.Input (randomPoints)

-- | The Sibson query reports its own cavity size, so the distribution of
-- (cavity faces, neighbours returned) says which step degenerates: a cavity of
-- one face means 'discoverCavity' never expanded, while a larger cavity paired
-- with three neighbours means the cell or the normalization was rejected.
reportInterpolationAudit :: Int -> Int -> IO ()
reportInterpolationAudit pointCount queryCount = do
  triangulation <-
    buildTriangulation
      <$> require (delaunay unitElementDefaults (V.fromList (randomPoints 0x0123456789abcdef pointCount)))
  workspace <- stToIO (newNaturalNeighborWorkspace triangulation)
  queries <- requireQueryPoints (randomPoints 0x2718281828459045 queryCount)
  pairs <-
    traverse
      ( \query -> do
          (count, _, stats) <-
            stToIO (foldNaturalNeighborWeights (\total _ _ -> total + (1 :: Int)) 0 workspace Nothing query)
          pure ((interpolationCavityFaces stats, count), interpolationUsedFallback stats)
      )
      queries
  let tally =
        [ (representative, 1 + length rest)
        | representative : rest <- groupSorted (sort (map fst pairs))
        ]
      fallbacks = length (filter snd pairs)
  putStrLn ("queries " <> show (length pairs))
  putStrLn ("fallbacks " <> show fallbacks)
  traverse_
    (\((cavity, neighbors), n) -> putStrLn ("cavityFaces=" <> show cavity <> " neighbors=" <> show neighbors <> " count=" <> show n))
    tally
 where
  groupSorted [] = []
  groupSorted (x : xs) = let (same, rest) = span (== x) xs in (x : same) : groupSorted rest

-- | Isolates the cavity flood of the Sibson pipeline: per query, the BFS
-- over 'inCircle' and nothing else. The margin tally counts tests whose
-- determinant is inside the filtered predicate's error bound — the exact
-- dyadic path, which allocates Integer arithmetic per call.
reportInterpolationCavityAudit :: Int -> Int -> IO ()
reportInterpolationCavityAudit pointCount queryCount = do
  triangulation <-
    buildTriangulation
      <$> require (delaunay unitElementDefaults (V.fromList (randomPoints 0x0123456789abcdef pointCount)))
  queries <- requireQueryPoints (V.fromList (randomPoints 0x2718281828459045 queryCount))
  (faces, tests, margins) <-
    V.foldM'
      ( \(faceAcc, testAcc, marginAcc) query ->
          case locatePoint triangulation query of
            InFace start -> do
              let (!cavitySize, !testCount, !marginCount) = flood triangulation (queryPointValue query) start
              pure (faceAcc + cavitySize, testAcc + testCount, marginAcc + marginCount)
            _ -> pure (faceAcc, testAcc, marginAcc)
      )
      (0 :: Int, 0 :: Int, 0 :: Int)
      queries
  putStrLn ("queries " <> show queryCount)
  putStrLn ("cavity-faces " <> show faces)
  putStrLn ("incircle-tests " <> show tests)
  putStrLn ("exact-path-margins " <> show margins)
 where
  flood triangulation query start = go [start] [] 0 0
   where
    go [] seen tests margins = (length seen, tests, margins)
    go (face : rest) seen tests margins
      | face `elem` seen = go rest seen tests margins
      | otherwise =
          case innerFaceVertices triangulation face of
            Nothing -> go rest seen tests margins
            Just (a, b, c) ->
              let !pa = vertexPoint triangulation a
                  !pb = vertexPoint triangulation b
                  !pc = vertexPoint triangulation c
                  !verdict = Predicates.inCircle pa pb pc query
                  !margins' = if insideMargin pa pb pc query then margins + 1 else margins
               in if verdict == GT
                    then go (adjacent triangulation face ++ rest) (face : seen) (tests + 1) margins'
                    else go rest seen (tests + 1) margins'

  adjacent triangulation face =
    case innerFaceDirectedEdges triangulation face of
      Nothing -> []
      Just (e0, e1, e2) ->
        [ opposite
        | edge <- [e0, e1, e2]
        , let opposite = incidentFace triangulation (reverseEdge edge)
        , opposite /= outerFace
        ]

  -- The branch condition of the filtered Double inCircle, recomputed over
  -- public coordinates: inside the error bound means the exact dyadic path.
  insideMargin (Point ax ay) (Point bx by) (Point cx cy) (Point dx dy) =
    abs determinant <= 1.1102230246251577e-15 * permanent
   where
    !adx = ax - dx
    !ady = ay - dy
    !bdx = bx - dx
    !bdy = by - dy
    !cdx = cx - dx
    !cdy = cy - dy
    !abdet = adx * bdy - bdx * ady
    !bcdet = bdx * cdy - cdx * bdy
    !cadet = cdx * ady - adx * cdy
    !alift = adx * adx + ady * ady
    !blift = bdx * bdx + bdy * bdy
    !clift = cdx * cdx + cdy * cdy
    !determinant = alift * bcdet + blift * cadet + clift * abdet
    !permanent =
      (abs (bdx * cdy) + abs (cdx * bdy)) * alift
        + (abs (cdx * ady) + abs (adx * cdy)) * blift
        + (abs (adx * bdy) + abs (bdx * ady)) * clift

-- | Isolates the locate half of the interpolation lane: same triangulation,
-- same queries, but only 'locatePointWithHint' runs — no cavity, no Sibson.
-- With the allocation of this in hand against the full lane's, the owner of
-- the per-query cost is named by subtraction rather than by guesswork.
reportInterpolationLocateAudit :: Int -> Int -> IO ()
reportInterpolationLocateAudit pointCount queryCount = do
  triangulation <-
    buildTriangulation
      <$> require (delaunay unitElementDefaults (V.fromList (randomPoints 0x0123456789abcdef pointCount)))
  queries <- requireQueryPoints (V.fromList (randomPoints 0x2718281828459045 queryCount))
  (steps, fallbacks) <-
    V.foldM'
      ( \(stepAcc, fallbackAcc) query -> do
          let (!location, !stats) = locatePointWithHint triangulation Nothing query
          _ <- evaluate (force (show location))
          pure (stepAcc + locationWalkSteps stats, fallbackAcc + (if locationUsedFallback stats then 1 else 0))
      )
      (0 :: Int, 0 :: Int)
      queries
  putStrLn ("queries " <> show queryCount)
  putStrLn ("walk-steps " <> show steps)
  putStrLn ("fallbacks " <> show fallbacks)

-- | The Sibson pipeline re-implemented over public API only, stage by stage,
-- mirroring the library's 'sibsonQuery': cavity flood, boundary collection,
-- loop ordering, insertion cell, stolen-area walks, normalization. The stage
-- sets are order-independent, so where the replica succeeds while the library
-- falls back the workspace layer is convicted, and where it fails the stage's
-- own algorithm is. With no index it tallies the first 1000 gate queries by
-- failing stage; with an index it dumps one query's full geometry.
reportInterpolationContext :: Int -> Maybe Int -> IO ()
reportInterpolationContext pointCount maybeIndex = do
  triangulation <-
    buildTriangulation
      <$> require (delaunay unitElementDefaults (V.fromList (randomPoints 0x0123456789abcdef pointCount)))
  queries <- requireQueryPoints (V.fromList (randomPoints 0x2718281828459045 1000))
  case maybeIndex of
    Just index -> case queries V.!? index of
      Nothing -> do
        hPutStrLn stderr "interp-context: query index out of range"
        exitWith (ExitFailure 2)
      Just query -> do
        let (_, detail) = replicateQuery triangulation query
        putStrLn ("query " <> pointHex (queryPointValue query))
        traverse_ putStrLn detail
    Nothing -> do
      let reports = map (fst . replicateQuery triangulation) (V.toList queries)
          tally =
            [ (representative, 1 + length rest)
            | representative : rest <- groupByHead (sort reports)
            ]
      putStrLn ("queries " <> show (length reports))
      traverse_ (\(stage, count) -> putStrLn (stage <> " count=" <> show count)) tally
 where
  groupByHead [] = []
  groupByHead (x : xs) = let (same, rest) = span (== x) xs in (x : same) : groupByHead rest

  replicateQuery triangulation queryPoint =
    let query = queryPointValue queryPoint
     in case locatePoint triangulation queryPoint of
      OnVertex _ -> ("special-on-vertex", ["on-vertex"])
      OnEdge edge
        | isBoundaryEdge triangulation (asUndirected edge) -> ("special-boundary-edge", ["boundary-edge"])
        | otherwise ->
            let !left = incidentFace triangulation edge
                !right = incidentFace triangulation (reverseEdge edge)
                !start = if left /= outerFace then left else right
             in sibson triangulation query start
      InFace face -> sibson triangulation query face
      OutsideConvexHull _ -> ("special-outside-hull", ["outside-hull"])
      EmptyTriangulation -> ("special-empty", ["empty"])

  sibson triangulation query startFace =
    let cavity = flood [startFace] []
        boundary =
          [ edge
          | face <- cavity
          , Just (e0, e1, e2) <- [innerFaceDirectedEdges triangulation face]
          , edge <- [e0, e1, e2]
          , let opposite = incidentFace triangulation (reverseEdge edge)
          , opposite == outerFace || not (opposite `elem` cavity)
          ]
        header =
          [ "cavity " <> show (length cavity)
          , "boundary " <> show (length boundary)
          ]
     in case orderLoop boundary of
          Left reason -> ("fail-order", header ++ ["order-failure " <> reason])
          Right ordered
            | length ordered < 3 -> ("fail-order-small", header ++ ["ordered " <> show (length ordered)])
            | otherwise ->
                case insertionCell ordered of
                  Left reason -> ("fail-cell", header ++ ["ordered " <> show (length ordered), "cell-failure " <> reason])
                  Right cell ->
                    let walks = stolenAreas ordered cell
                     in case sequence walks of
                          Left reason -> ("fail-stolen", header ++ ["ordered " <> show (length ordered), "stolen-failure " <> reason])
                          Right areas ->
                            let !total = sum areas
                                !minimumWeight = minimum areas
                                !tolerance = max 1.0e-12 (128 * 2.220446049250313e-16)
                                !clampedTotal = sum (map (max 0) areas)
                                verdict
                                  | any (not . isFinite) areas || total == 0 || minimumWeight < negate tolerance =
                                      Left ("normalize total=" <> show total <> " min=" <> show minimumWeight)
                                  | clampedTotal <= 0 || not (isFinite clampedTotal) =
                                      Left ("clamped-total " <> show clampedTotal)
                                  | otherwise = Right ()
                             in case verdict of
                                  Left reason ->
                                    ( "fail-normalize"
                                    , header
                                        ++ ["ordered " <> show (length ordered)]
                                        ++ ["area " <> show area | area <- areas]
                                        ++ ["normalize-failure " <> reason]
                                    )
                                  Right () ->
                                    ( "sibson-success"
                                    , header
                                        ++ ["ordered " <> show (length ordered)]
                                        ++ zipWith areaLine ordered areas
                                        ++ ["total " <> show total <> " min " <> show minimumWeight]
                                    )
   where
    areaLine edge area =
      "  vertex "
        <> pointHex (vertexPoint triangulation (origin triangulation edge))
        <> " area "
        <> show area

    contains face =
      case innerFaceVertices triangulation face of
        Nothing -> False
        Just (a, b, c) ->
          Predicates.inCircle
            (vertexPoint triangulation a)
            (vertexPoint triangulation b)
            (vertexPoint triangulation c)
            query
            /= LT

    adjacent face =
      case innerFaceDirectedEdges triangulation face of
        Nothing -> []
        Just (e0, e1, e2) ->
          [ opposite
          | edge <- [e0, e1, e2]
          , let opposite = incidentFace triangulation (reverseEdge edge)
          , opposite /= outerFace
          ]

    flood [] seen = seen
    flood (face : rest) seen
      | face `elem` seen = flood rest seen
      | not (contains face) = flood rest seen
      | otherwise = flood (adjacent face ++ rest) (face : seen)

    orderLoop [] = Left "empty-boundary"
    orderLoop boundary@(first : _) = chain first first [] 0
     where
      !count = length boundary
      origins = [(origin triangulation edge, edge) | edge <- boundary]
      chain start current acc index
        | index >= count =
            if current == start then Right (reverse acc) else Left "no-closure"
        | otherwise =
            case lookup (destination triangulation current) origins of
              Nothing ->
                Left
                  ( "broken-chain "
                      <> pointHex (vertexPoint triangulation (destination triangulation current))
                  )
              Just nextEdge
                | nextEdge == start && index + 1 == count -> Right (reverse (current : acc))
                | nextEdge == start -> Left "premature-closure"
                | otherwise -> chain start nextEdge (current : acc) (index + 1)

    relative point = case point of Point x y -> Point (x - qx) (y - qy)
    (qx, qy) = case query of Point x y -> (x, y)

    insertionCell ordered = traverse cellCenter ordered
     where
      cellCenter edge =
        let !from = vertexPoint triangulation (origin triangulation edge)
            !to = vertexPoint triangulation (destination triangulation edge)
         in case circumcenter (relative to) (relative from) (Point 0 0) of
              Nothing ->
                Left
                  ( "edge "
                      <> pointHex from
                      <> " "
                      <> pointHex to
                  )
              Just center -> Right center

    stolenAreas ordered cell =
      let !count = length ordered
          pairs = zip ordered cell
          predecessors = drop (count - 1) pairs ++ take (count - 1) pairs
       in zipWith walk pairs predecessors
     where
      walk (stopEdge, first) (initialEdge, initialPoint) = go initialEdge initialPoint 0 initialPositive initialNegative
       where
        !target = reverseEdge stopEdge
        !limit = numDirectedEdges triangulation + 1
        !initialPositive = px first * py initialPoint
        !initialNegative = py first * px initialPoint
        !vertex = vertexPoint triangulation (origin triangulation stopEdge)
        go lastEdge lastPoint steps positive negative
          | steps >= limit = Left "step-limit"
          | incidentFace triangulation lastEdge == outerFace =
              Left ("outer-face vertex=" <> pointHex vertex <> " steps=" <> show steps)
          | otherwise =
              case innerFaceVertices triangulation (incidentFace triangulation lastEdge) of
                Nothing -> Left "no-face-vertices"
                Just (a, b, c) ->
                  case circumcenter
                    (relative (vertexPoint triangulation a))
                    (relative (vertexPoint triangulation b))
                    (relative (vertexPoint triangulation c)) of
                    Nothing ->
                      Left ("circumcenter vertex=" <> pointHex vertex <> " steps=" <> show steps)
                    Just current ->
                      let !positive' = positive + px lastPoint * py current
                          !negative' = negative + py lastPoint * px current
                          !nextEdge = reverseEdge (next triangulation lastEdge)
                       in if nextEdge == target
                            then Right ((negative' + py current * px first) - (positive' + px current * py first))
                            else go nextEdge current (steps + 1) positive' negative'
      px (Point x _) = x
      py (Point _ y) = y

-- | The interpolation gate, two artifacts per query. The neighbour set is
-- exact-predicate determined over triangulations that already agree
-- bit-for-bit, so it crosses implementations. The weights cannot: each
-- neighbour's stolen area uses the same operation sequence on both sides,
-- but the normalization total accumulates those areas in each side's own
-- neighbour order, and floating-point addition is not associative. The
-- weights are pinned per side in the driver instead.
writeInterpolationGate :: FilePath -> Int -> Int -> IO ()
writeInterpolationGate directory pointCount queryCount = do
  triangulation <- buildTriangulation <$> require (delaunay unitElementDefaults (V.fromList (randomPoints 0x0123456789abcdef pointCount)))
  workspace <- stToIO (newNaturalNeighborWorkspace triangulation)
  queries <- requireQueryPoints (V.fromList (randomPoints 0x2718281828459045 queryCount))
  (neighborLines, weightLines) <-
    V.foldM'
      ( \(neighborAcc, weightAcc) query -> do
          (entries, _, _) <- stToIO (foldNaturalNeighborWeights (\acc vertex weight -> (vertex, weight) : acc) [] workspace Nothing query)
          let sortedEntries =
                sortOn
                  (\(x, y, _) -> (x, y))
                  [ case vertexPoint triangulation vertex of
                      Point x y -> (x, y, weight)
                  | (vertex, weight) <- entries
                  ]
              queryHex = case queryPointValue query of Point x y -> hex64 x <> hex64 y
          pure
            ( queryHex <> concatMap (\(x, y, _) -> hex64 x <> hex64 y) sortedEntries : neighborAcc
            , queryHex <> concatMap (\(x, y, weight) -> hex64 x <> hex64 y <> hex64 weight) sortedEntries : weightAcc
            )
      )
      ([], [])
      queries
  writeFile
    (directory </> ("interpolation-" <> show pointCount <> "-" <> show queryCount <> "-neighbors.txt"))
    (unlines (reverse neighborLines))
  writeFile
    (directory </> ("interpolation-" <> show pointCount <> "-" <> show queryCount <> "-weights.txt"))
    (unlines (reverse weightLines))

-- | The sum of one query's Sibson weights through the reusable workspace —
-- the allocation-free path, matching what the strict side's buffer reuse
-- does inside its measured region.
interpolationWeightSum
  :: NaturalNeighborWorkspace RealWorld 'Unconstrained Point () () ()
  -> V.Vector QueryPoint
  -> IO Double
interpolationWeightSum workspace =
  V.foldM'
    ( \total query -> do
        (weightSum, _, _) <- stToIO (foldNaturalNeighborWeights (\acc _ weight -> acc + weight) 0 workspace Nothing query)
        pure (total + weightSum)
    )
    0
