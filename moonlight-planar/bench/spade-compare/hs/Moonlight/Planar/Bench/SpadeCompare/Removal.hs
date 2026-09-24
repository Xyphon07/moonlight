{-# LANGUAGE DataKinds #-}

-- | Removal lanes, their gates, and the removal diagnostics.
module Moonlight.Planar.Bench.SpadeCompare.Removal where

import Control.Monad (when)
import Data.Foldable (traverse_)
import Data.List (sortOn)
import System.Exit (exitWith, ExitCode (ExitFailure))
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import qualified Data.Vector as V
import Moonlight.Planar.BulkLoad (delaunay, empty)
import Moonlight.Planar.Dcel
import Moonlight.Planar.Handles.HandleDefs (VertexId, reverseEdge)
import Moonlight.Planar.Handles.Iterators.FixedIterators (innerFaces, undirectedEdges, vertices)
import Moonlight.Planar.Point (mkQueryPoint)
import Moonlight.Planar.PointLocation
import Moonlight.Planar.Removal
import Moonlight.Planar.SetAlgebra (difference)
import Moonlight.Planar.Types
import Moonlight.Planar.Point (Point (..))
import Moonlight.Planar.Telemetry (removeManyAt, withSession)
import Moonlight.Planar.Internal.HandleDefs
import Moonlight.Planar.Internal.Predicates qualified as Predicates
import Moonlight.Planar.Session
import Moonlight.Planar.Bench.SpadeCompare.Support
import Moonlight.Planar.Bench.SpadeCompare.Input (randomPoints)

-- | The removal gate: a deterministic prefix of the input, removed by
-- coordinate, and the surviving canonical edge set. Delaunay triangulations
-- of point sets in general position are unique, so the survivor is forced
-- and both implementations must agree bit-for-bit.
writeRemovalGate :: FilePath -> Int -> Int -> IO ()
writeRemovalGate directory pointCount removalCount = do
  let points = randomPoints 0x9e3779b97f4a7c15 pointCount
  triangulation <- buildTriangulation <$> require (delaunay unitElementDefaults (V.fromList points))
  surviving <- removeAllByCoordinate triangulation (V.fromList (take removalCount points))
  writeFile
    (directory </> ("removal-" <> show pointCount <> "-" <> show removalCount <> "-edges.txt"))
    (unlines (canonicalEdges surviving))

-- | Remove vertices by coordinate inside one session: one O(n) dense thaw and
-- publication for the whole program, then O(degree) topology work per removal.
-- The snapshot 'locateAndRemove' fold pays that O(n) boundary per removal.
removeAllByCoordinate
  :: DelaunayTriangulation Point
  -> V.Vector Point
  -> IO (DelaunayTriangulation Point)
removeAllByCoordinate triangulation removals =
  case withSession triangulation 0 (removeManyAt removals) of
    Left refusal -> fail (show refusal)
    Right (outcomes, surviving, _) -> do
      V.imapM_ checkRemoved outcomes
      pure surviving
 where
  checkRemoved index outcome = case outcome of
    Nothing ->
      error
        ( "removal target is not a vertex: index="
            <> show index
            <> " point="
            <> ( case removals V.! index of
                  Point x y -> hex64 x <> "," <> hex64 y
               )
        )
    Just _ -> pure ()

-- | Removal by coordinate through the persistent entry point: a thaw and a
-- publication per removal, against the session fold's one of each for the whole
-- program. Same removals, same order, same survivor — only the price differs.
removeAllPersistently
  :: DelaunayTriangulation Point
  -> V.Vector Point
  -> IO (DelaunayTriangulation Point)
removeAllPersistently =
  V.foldM'
    (\current point -> do
      outcome <- require (locateAndRemove current point)
      case outcome of
        Nothing -> fail ("removal target is not a vertex: " <> pointHex point)
        Just result -> pure (removalTriangulation result)
    )

writePersistentRemovalGate :: FilePath -> Int -> Int -> IO ()
writePersistentRemovalGate directory pointCount removalCount = do
  let points = randomPoints 0x9e3779b97f4a7c15 pointCount
  base <- buildTriangulation <$> require (delaunay unitElementDefaults (V.fromList points))
  surviving <- removeAllPersistently base (V.fromList (take removalCount points))
  writeFile
    ( directory
        </> ("persistent-removal-" <> show pointCount <> "-" <> show removalCount <> "-edges.txt")
    )
    (unlines (canonicalEdges surviving))

-- | Removal audited against the one oracle that needs no second implementation:
-- the Delaunay triangulation of a point set in general position is unique, so
-- removing a prefix must leave exactly the triangulation a fresh bulk load of
-- the survivors produces. Reports the first removal at which that identity
-- breaks, so a divergence is dated rather than merely observed.
--
-- Every removal also reports the handle swap-compaction relocated into the
-- freed slot, and that handle is the caller's only re-anchor after a removal
-- invalidates the rest. An out-of-range or misnamed handle is invisible to the
-- edge-set oracle, so it is checked here against the vertex the mesh moved.
reportRemovalAudit :: Int -> Int -> IO ()
reportRemovalAudit pointCount removalCount = do
  let allPoints = V.fromList (randomPoints 0x9e3779b97f4a7c15 pointCount)
  base <- buildTriangulation <$> require (delaunay unitElementDefaults allPoints)
  let step current index
        | index >= removalCount = putStrLn ("clean-through " <> show removalCount)
        | otherwise =
            case allPoints V.!? index of
              Nothing -> putStrLn ("input-exhausted-at-removal " <> show index)
              Just point -> do
                outcome <- require (locateAndRemove current point)
                case outcome of
                  Nothing -> putStrLn ("located-nothing-at-removal " <> show index)
                  Just result -> do
                    let nextTriangulation = removalTriangulation result
                        movedPoint =
                          vertexPoint current (VertexId (fromIntegral (numVertices current - 1)))
                    case removalOutcomeSwap (removalOutcome result) of
                      Nothing
                        | vertexPoint current (VertexId (fromIntegral (numVertices current - 1))) /= point ->
                            putStrLn ("swap-handle-missing-at-removal " <> show index)
                        | otherwise -> pure ()
                      Just (swappedIn@(VertexId rawSwappedIn), _)
                        | toInteger rawSwappedIn >= toInteger (numVertices nextTriangulation) ->
                            putStrLn
                              ( "swap-handle-out-of-range-at-removal "
                                  <> show index
                                  <> " handle="
                                  <> show rawSwappedIn
                                  <> " vertices="
                                  <> show (numVertices nextTriangulation)
                              )
                        | vertexPoint nextTriangulation swappedIn /= movedPoint ->
                            putStrLn
                              ( "swap-handle-misnamed-at-removal "
                                  <> show index
                                  <> " handle="
                                  <> show rawSwappedIn
                              )
                        | otherwise -> pure ()
                    fresh <-
                      buildTriangulation
                        <$> require (delaunay unitElementDefaults (V.drop (index + 1) allPoints))
                    let removalEdges = canonicalEdges nextTriangulation
                        freshEdges = canonicalEdges fresh
                    if removalEdges == freshEdges
                      then step nextTriangulation (index + 1)
                      else do
                        let (onlyRemoval, onlyFresh) = sortedDifference removalEdges freshEdges
                        putStrLn ("diverged-at-removal " <> show index)
                        putStrLn
                          ( "removed-point "
                              <> case point of
                                Point x y -> hex64 x <> "," <> hex64 y
                          )
                        putStrLn ("removal-vertices " <> show (numVertices nextTriangulation))
                        putStrLn ("fresh-vertices " <> show (numVertices fresh))
                        putStrLn ("removal-edges " <> show (length removalEdges))
                        putStrLn ("fresh-edges " <> show (length freshEdges))
                        putStrLn ("only-in-removal " <> show (length onlyRemoval))
                        putStrLn ("only-in-fresh " <> show (length onlyFresh))
                        let (invertedRemoval, illegalRemoval) = meshHealth nextTriangulation
                            (invertedFresh, illegalFresh) = meshHealth fresh
                        putStrLn ("removal-inverted-faces " <> show invertedRemoval)
                        putStrLn ("removal-illegal-half-edges " <> show illegalRemoval)
                        putStrLn ("fresh-inverted-faces " <> show invertedFresh)
                        putStrLn ("fresh-illegal-half-edges " <> show illegalFresh)
                        traverse_ (putStrLn . ("  removal-only " <>)) (take 8 onlyRemoval)
                        traverse_ (putStrLn . ("  fresh-only " <>)) (take 8 onlyFresh)
  step base 0

-- | The geometry around one removal, reconstructed with public API only. For
-- an interior removal the cavity border is the cyclic link of the removed
-- vertex, and nothing but a drain flip can destroy one of those non-star
-- edges: cleanup removes only the star, and swap-remove touches only the
-- removed vertex. The reported hull predicate guards that interpretation.
-- The fan origin 'remeshRing' would pick is also public: 'collectOutgoing'
-- starts at the stored out-edge, so the origin is the destination of that
-- edge.
reportRemovalContext :: Int -> Int -> IO ()
reportRemovalContext pointCount target = do
  let allPoints = V.fromList (randomPoints 0x9e3779b97f4a7c15 pointCount)
  case allPoints V.!? target of
    Nothing -> do
      hPutStrLn stderr "removal-context: target index out of range"
      exitWith (ExitFailure 2)
    Just removedPoint -> do
      base <- buildTriangulation <$> require (delaunay unitElementDefaults allPoints)
      before <- removeAllByCoordinate base (V.take target allPoints)
      after <- removeAllByCoordinate before (V.singleton removedPoint)
      fresh <- buildTriangulation <$> require (delaunay unitElementDefaults (V.drop (target + 1) allPoints))
      lab <- buildTriangulation <$> require (delaunay unitElementDefaults (V.drop target allPoints))
      removedQuery <- require (mkQueryPoint removedPoint)
      putStrLn ("lab-clean " <> show (canonicalEdges before == canonicalEdges lab))
      case locatePoint before removedQuery of
        OnVertex removedVertex -> do
          let ring = ringOf before removedVertex
              ringPoints = map snd ring
              ringMember point = any (== point) ringPoints
              rOnHull = any (\(edge, _) -> incidentFace before edge == outerFace) ring
              afterPairs = canonicalEdgePairs after
              freshPairs = canonicalEdgePairs fresh
              borderPairs = cyclicPairs ringPoints
              destroyedBorder = [pair | pair <- borderPairs, not (any (== pair) afterPairs)]
              removalOnly = [pair | pair <- afterPairs, not (any (== pair) freshPairs)]
              freshOnly = [pair | pair <- freshPairs, not (any (== pair) afterPairs)]
          putStrLn ("r-on-hull " <> show rOnHull)
          putStrLn ("ring-size " <> show (length ringPoints))
          traverse_
            (\(index, (_, point)) -> putStrLn ("  ring " <> show index <> " " <> pointHex point))
            (zip [(0 :: Int) ..] ring)
          putStrLn ("border-edges-before " <> show (length borderPairs))
          putStrLn ("border-edges-destroyed " <> show (length destroyedBorder))
          traverse_
            (\(p, q) -> putStrLn ("  destroyed " <> pointHex p <> " " <> pointHex q))
            destroyedBorder
          case ringPoints of
            [] -> putStrLn "fan-unavailable empty-ring"
            fanOriginPoint : ringTail ->
              if rOnHull
                then putStrLn "fan-unavailable hull-removal"
                else do
                  let fanOrientations =
                        zipWith3
                          (\index p q -> (index, p, q, Predicates.orient2d fanOriginPoint p q))
                          [(1 :: Int) ..]
                          ringTail
                          (drop 1 ringTail)
                      fanCandidates =
                        zipWith3
                          ( \(index, priorPoint) current following ->
                              ( index
                              , current
                              , Predicates.orient2d priorPoint following fanOriginPoint == GT
                                  && Predicates.orient2d following priorPoint current == GT
                              , Predicates.inCircle current fanOriginPoint priorPoint following
                              )
                          )
                          (zip [(2 :: Int) ..] ringTail)
                          (drop 1 ringTail)
                          (drop 2 ringTail)
                  putStrLn ("fan-origin " <> pointHex fanOriginPoint)
                  putStrLn
                    ( "fan-inverted "
                        <> show (length [() | (_, _, _, verdict) <- fanOrientations, verdict == LT])
                        <> " of "
                        <> show (length fanOrientations)
                    )
                  traverse_
                    ( \(index, p, q, verdict) ->
                        when (verdict == LT) $
                          putStrLn
                            ( "  fan-inverted-at "
                                <> show index
                                <> " "
                                <> pointHex fanOriginPoint
                                <> " "
                                <> pointHex p
                                <> " "
                                <> pointHex q
                            )
                    )
                    fanOrientations
                  traverse_
                    ( \(index, current, convex, circle) ->
                        putStrLn
                          ( "  fan-candidate "
                              <> show index
                              <> " "
                              <> pointHex current
                              <> "->"
                              <> pointHex fanOriginPoint
                              <> " convex="
                              <> show convex
                              <> " incircle="
                              <> show circle
                          )
                    )
                    fanCandidates
          putStrLn ("only-in-removal " <> show (length removalOnly))
          traverse_
            ( \(p, q) ->
                putStrLn
                  ( "  removal-only "
                      <> pointHex p
                      <> " "
                      <> pointHex q
                      <> " ring="
                      <> show (ringMember p, ringMember q)
                  )
            )
            removalOnly
          putStrLn ("only-in-fresh " <> show (length freshOnly))
          traverse_
            ( \(p, q) ->
                putStrLn
                  ( "  fresh-only "
                      <> pointHex p
                      <> " "
                      <> pointHex q
                      <> " ring="
                      <> show (ringMember p, ringMember q)
                  )
            )
            freshOnly
          let inverted = invertedFaces after
          putStrLn ("inverted-faces " <> show (length inverted))
          traverse_
            ( \(p, q, r) ->
                putStrLn
                  ( "  inverted-face "
                      <> pointHex p
                      <> " "
                      <> pointHex q
                      <> " "
                      <> pointHex r
                      <> " ring="
                      <> show (ringMember p, ringMember q, ringMember r)
                  )
            )
            inverted
        other -> putStrLn ("locate-unexpected " <> show other)
 where
  ringOf triangulation vertex =
    case vertexOutEdge triangulation vertex of
      Nothing -> []
      Just start -> walk start start []
   where
    walk start current acc =
      let ringVertex = destination triangulation current
          nextEdge = counterClockwise triangulation current
          acc' = (current, vertexPoint triangulation ringVertex) : acc
       in if nextEdge == start
            then reverse acc'
            else walk start nextEdge acc'

  cyclicPairs (first : second : rest) =
    zipWith canonicalPair (first : second : rest) ((second : rest) ++ [first])
  cyclicPairs _ = []

  canonicalEdgePairs triangulation =
    sortOn
      (\(p, q) -> (pointKey p, pointKey q))
      [ canonicalPair p q
      | edge <- undirectedEdges triangulation
      , let (from, to) = undirectedEndpoints triangulation edge
            p = vertexPoint triangulation from
            q = vertexPoint triangulation to
      ]

  canonicalPair p q =
    if pointKey p <= pointKey q then (p, q) else (q, p)

  pointKey (Point x y) = (x, y)

  invertedFaces triangulation =
    [ (vertexPoint triangulation a, vertexPoint triangulation b, vertexPoint triangulation c)
    | face <- innerFaces triangulation
    , Just (a, b, c) <- [innerFaceVertices triangulation face]
    , Predicates.orient2d
        (vertexPoint triangulation a)
        (vertexPoint triangulation b)
        (vertexPoint triangulation c)
        == LT
    ]

-- | Two independent health measures, which together decide what a divergence
-- means. Delaunay's theorem says a /valid/ triangulation with no locally
-- illegal edge is globally Delaunay — so a mesh that differs from the fresh
-- build while carrying no illegal edge cannot be a valid triangulation, and
-- one carrying illegal edges was simply left under-legalized.
meshHealth
  :: Triangulation mode vertex directed undirected face
  -> (Int, Int)
meshHealth triangulation =
  ( length [() | face <- innerFaces triangulation, not (isCounterClockwise face)]
  , length
      [ ()
      | face <- innerFaces triangulation
      , edge <- faceEdgeList face
      , edgeIsIllegal edge
      ]
  )
 where
  faceEdgeList face = case innerFaceDirectedEdges triangulation face of
    Nothing -> []
    Just (e0, e1, e2) -> [e0, e1, e2]

  isCounterClockwise face = case innerFaceVertices triangulation face of
    Nothing -> True
    Just (a, b, c) ->
      Predicates.orient2d
        (vertexPoint triangulation a)
        (vertexPoint triangulation b)
        (vertexPoint triangulation c)
        == GT

  edgeIsIllegal edge =
    let twin = reverseEdge edge
     in incidentFace triangulation twin /= outerFace
          && Predicates.inCircle
            (vertexPoint triangulation (origin triangulation edge))
            (vertexPoint triangulation (destination triangulation edge))
            (vertexPoint triangulation (destination triangulation (next triangulation edge)))
            (vertexPoint triangulation (destination triangulation (next triangulation twin)))
            == GT

-- | The Delaunay triangulation of the survivors, built from scratch. Diffed
-- against a removal result it decides whether that result is the Delaunay
-- triangulation it is required to be.
writeFreshSuffix :: Int -> Int -> FilePath -> IO ()
writeFreshSuffix pointCount dropped path = do
  triangulation <-
    buildTriangulation
      <$> require
        ( delaunay
            unitElementDefaults
            (V.drop dropped (V.fromList (randomPoints 0x9e3779b97f4a7c15 pointCount)))
        )
  writeFile path (unlines (canonicalEdges triangulation))

-- | Symmetric difference of two ascending lists, by merge.
sortedDifference :: Ord a => [a] -> [a] -> ([a], [a])
sortedDifference [] right = ([], right)
sortedDifference left [] = (left, [])
sortedDifference left@(l : ls) right@(r : rs)
  | l == r = sortedDifference ls rs
  | l < r = let (onlyLeft, onlyRight) = sortedDifference ls right in (l : onlyLeft, onlyRight)
  | otherwise = let (onlyLeft, onlyRight) = sortedDifference left rs in (onlyLeft, r : onlyRight)
