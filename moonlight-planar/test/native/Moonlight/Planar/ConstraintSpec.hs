{-# LANGUAGE BangPatterns #-}

-- | Constraint recovery, batch agreement, and refusal-witness laws.
module Moonlight.Planar.ConstraintSpec
  ( tests
  ) where

import Control.Monad ( foldM, forM_, unless, when )
import Data.Primitive.PrimArray ( indexPrimArray, sizeofPrimArray )
import Moonlight.Planar.Cdt ( ConstrainedDelaunayTriangulation, recoverConstraints,
  constrainedDelaunayMaximal, fromDelaunay, canAddConstraint, constraintEdges,
  getConflictingEdgesBetweenVertices, addConstraintEdge, addConstraintAndSplit,
  addConstraintsAndSplit, CdtBuildResult(cdtRejectedConstraints, cdtAcceptedBuild),
  CdtError(ConstraintIntersection, CdtBuildError, ConstraintBatchCardinalityMismatch),
  ConstraintBatchResult(constraintBatchTriangulation, constraintBatchStats,
  constraintBatchOutcomes), ConstraintBatchStats(constraintBatchRejected, constraintBatchRequests,
  constraintBatchAccepted), ConstraintOutcome(..),
  ConstraintRecoveryResult(constraintRecoveryTriangulation) )
import Moonlight.Planar.Dcel ( isConstraintEdge, numConstraints, numVertices, undirectedEndpoints,
  vertexPoint )
import Moonlight.Planar.Internal.HandleDefs ( DirectedEdgeId, UndirectedEdgeId, VertexId(VertexId),
  asUndirected )
import Moonlight.Planar.MeshFixtures ( requirePointBuild )
import Moonlight.Planar.Point (Point(Point))
import Moonlight.Planar.Types (unitElementDefaults, BuildError(InvalidCoordinate), BuildResult(buildTriangulation, buildInputVertices))
import Moonlight.Planar.Scalar (CoordinateError(CoordinateNaN))
import Support ( assertEqual, assertValid, requireRight, randomPoints )
import qualified Data.Set as Set
import qualified Data.Vector as V


tests :: IO ()
tests =
  sequence_
    [ testConstrainedDelaunay
    , testSingletonRefusalWitness
    ]

testSingletonRefusalWitness :: IO ()
testSingletonRefusalWitness = do
  base <-
    requirePointBuild
      "refusal witness base"
      [Point 0 0, Point 6 0, Point 2 (-2), Point 2 2, Point 4 (-3), Point 4 3]
  cdt <-
    foldM
      (\resident (from, to) ->
         constraintRecoveryTriangulation
           <$> requireRight "refusal witness constraint" (addConstraintEdge resident from to))
      (fromDelaunay (buildTriangulation base))
      [(Point 2 (-2), Point 2 2), (Point 4 (-3), Point 4 3)]
  assertEqual "refusal witness fixture holds two constraints" 2 (numConstraints cdt)
  let constraintAt :: Double -> IO UndirectedEdgeId
      constraintAt abscissa =
        case [ edge
             | edge <- constraintEdges cdt
             , let (from, to) = undirectedEndpoints cdt edge
                   Point fromX _ = vertexPoint cdt from
                   Point toX _ = vertexPoint cdt to
             , fromX == abscissa && toX == abscissa
             ] of
          [edge] -> pure edge
          edges -> fail ("constraint at x = " <> show abscissa <> " is not unique: " <> show edges)
      vertexAt :: Point -> IO VertexId
      vertexAt point =
        case [ vertex
             | index <- [0 .. numVertices cdt - 1]
             , let vertex = VertexId (fromIntegral index)
             , vertexPoint cdt vertex == point
             ] of
          [vertex] -> pure vertex
          matchingVertices -> fail ("vertex at " <> show point <> " is not unique: " <> show matchingVertices)
      refusalWitness :: String -> Point -> Point -> IO UndirectedEdgeId
      refusalWitness label from to =
        case addConstraintEdge cdt from to of
          Left (ConstraintIntersection edge) -> pure edge
          Left other -> fail (label <> " was refused for another reason: " <> show other)
          Right _ -> fail (label <> " was admitted across two resident constraints")
  nearSource <- constraintAt 2
  nearTarget <- constraintAt 4
  when (nearSource == nearTarget) $
    fail "the two constraints share an edge; the fixture proves nothing"
  forward <- refusalWitness "source-to-target request" (Point 0 0) (Point 6 0)
  backward <- refusalWitness "target-to-source request" (Point 6 0) (Point 0 0)
  assertEqual
    "the request from (0,0) witnesses the x = 2 constraint, first along its corridor"
    nearSource
    forward
  assertEqual
    "the request from (6,0) witnesses the x = 4 constraint, first along its corridor"
    nearTarget
    backward
  source <- vertexAt (Point 0 0)
  target <- vertexAt (Point 6 0)
  assertEqual "canAddConstraint refuses the source-to-target request" False (canAddConstraint cdt source target)
  assertEqual "canAddConstraint refuses the target-to-source request" False (canAddConstraint cdt target source)

testConstrainedDelaunay :: IO ()
testConstrainedDelaunay = do
  base <- requirePointBuild "CDT base" [Point 0 0, Point 4 0, Point 4 4, Point 0 4, Point 1 1, Point 3 3, Point 1 3, Point 3 1]
  let cdt0 = fromDelaunay (buildTriangulation base)
      invalidPoint = Point (0 / 0) 0
  case addConstraintEdge cdt0 invalidPoint (Point 4 0) of
    Left (CdtBuildError (InvalidCoordinate Nothing _ CoordinateNaN)) -> pure ()
    outcome -> fail ("invalid first constraint endpoint was admitted: " <> either show (const "success") outcome)
  case addConstraintEdge cdt0 (Point 0 0) invalidPoint of
    Left (CdtBuildError (InvalidCoordinate Nothing _ CoordinateNaN)) -> pure ()
    outcome -> fail ("invalid second constraint endpoint was admitted: " <> either show (const "success") outcome)
  assertEqual
    "invalid endpoint refusal leaves the immutable source unpublished"
    (8, 0)
    (numVertices cdt0, numConstraints cdt0)
  partlyResident <-
    requireRight
      "one resident one new constraint endpoint"
      (addConstraintEdge cdt0 (Point 0 0) (Point 2 2))
  let partlyResidentCdt = constraintRecoveryTriangulation partlyResident
  assertEqual
    "one resident endpoint reserves only the missing site"
    (numVertices cdt0 + 1)
    (numVertices partlyResidentCdt)
  assertValid "one resident one new constraint endpoint" partlyResidentCdt
  diagonalBatch <-
    requireRight
      "constraint recovery"
      (recoverConstraints cdt0 (V.singleton (VertexId 0, VertexId 2)))
  (diagonalPath, diagonalAdded) <-
    requireAcceptedConstraint "constraint recovery" diagonalBatch
  let cdt1 = constraintBatchTriangulation diagonalBatch
  when (V.null diagonalPath) $ fail "constraint recovery returned an empty path"
  assertEqual "constraint count" diagonalAdded (numConstraints cdt1)
  assertValid "constraint recovery" cdt1
  conflictBatch <-
    requireRight
      "atomic conflict"
      (recoverConstraints cdt1 (V.singleton (VertexId 1, VertexId 3)))
  case V.toList (constraintBatchOutcomes conflictBatch) of
    [ConstraintRejected _] -> pure ()
    outcomes ->
      fail
        ( "crossing constraint was not rejected atomically: "
            <> show outcomes
        )
  assertEqual
    "crossing rejection preserves topology"
    cdt1
    (constraintBatchTriangulation conflictBatch)
  let constraintProgram =
        V.fromList
          [ (VertexId 0, VertexId 2)
          , (VertexId 1, VertexId 3)
          , (VertexId 4, VertexId 6)
          ]
  wholeProgram <-
    requireRight
      "whole constraint program"
      (recoverConstraints cdt0 constraintProgram)
  (singletonTriangulation, singletonOutcomes) <-
    requireRight
      "singleton constraint program"
      (V.foldM' replayConstraintRequest (cdt0, []) constraintProgram)
  assertEqual
    "batch outcomes equal singleton descent"
    (V.toList (constraintBatchOutcomes wholeProgram))
    (reverse singletonOutcomes)
  assertEqual
    "batch topology equals singleton descent"
    singletonTriangulation
    (constraintBatchTriangulation wholeProgram)
  assertBatchStats "whole constraint program" wholeProgram
  forM_ [0 .. 7 :: Int] $ \batchIndex -> do
    randomizedBuild <-
      requirePointBuild
        ("random constraint batch " <> show batchIndex)
        (randomPoints (0x6a09_e667_f3bc_c909 + fromIntegral batchIndex) 40)
    let randomizedBase = fromDelaunay (buildTriangulation randomizedBuild)
        mapping = buildInputVertices randomizedBuild
        handles = V.generate (sizeofPrimArray mapping) (VertexId . indexPrimArray mapping)
        requests = V.take 18 (V.zip handles (V.reverse handles))
    randomizedBatch <-
      requireRight
        ("random whole constraint batch " <> show batchIndex)
        (recoverConstraints randomizedBase requests)
    (randomizedSingleton, randomizedOutcomes) <-
      requireRight
        ("random singleton constraint batch " <> show batchIndex)
        (V.foldM' replayConstraintRequest (randomizedBase, []) requests)
    assertEqual
      ("random batch outcomes equal singleton descent " <> show batchIndex)
      (V.toList (constraintBatchOutcomes randomizedBatch))
      (reverse randomizedOutcomes)
    assertEqual
      ("random batch topology equals singleton descent " <> show batchIndex)
      randomizedSingleton
      (constraintBatchTriangulation randomizedBatch)
    assertBatchStats
      ("random constraint batch " <> show batchIndex)
      randomizedBatch
    assertValid
      ("random constraint batch " <> show batchIndex)
      (constraintBatchTriangulation randomizedBatch)
  -- Batch admission returns the first typed obstruction. Callers that need a
  -- complete diagnosis ask the explicit corridor query and alone pay for it.
  diagnosisBatch <-
    requireRight
      "conflict diagnosis"
      (recoverConstraints cdt1 (V.singleton (VertexId 6, VertexId 7)))
  case V.toList (constraintBatchOutcomes diagnosisBatch) of
    [ConstraintRejected blocking] -> do
      let diagnosed =
            fmap
              asUndirected
              (getConflictingEdgesBetweenVertices cdt1 (VertexId 6) (VertexId 7))
      case diagnosed of
        [] -> fail "explicit conflict diagnosis named no edge"
        firstBlocking : _ ->
          assertEqual "batch rejection is the first corridor obstruction" firstBlocking blocking
      assertEqual
        "explicit conflict diagnosis is deduplicated"
        (length diagnosed)
        (length (Set.fromList diagnosed))
      forM_ diagnosed $ \edge ->
        unless (isConstraintEdge cdt1 edge) $
          fail ("explicit conflict diagnosis named a non-constraint edge: " <> show edge)
    outcomes ->
      fail
        ( "a constraint crossing the diagonal was not rejected: "
            <> show outcomes
        )

  split <- requireRight "constraint split" (addConstraintAndSplit id cdt1 (VertexId 1) (VertexId 3))
  let splitCdt = constraintRecoveryTriangulation split
  unless (numVertices splitCdt > numVertices cdt1) $ fail "constraint split did not insert an intersection vertex"
  assertValid "constraint split" splitCdt

  -- The batch splitter must agree with singleton descent, including across a
  -- suspension: every vertical below is constrained only inside the batch
  -- itself, so no census against the base can reserve for its crossings and
  -- the chunk's vertex reservation exhausts mid-batch. The driver publishes,
  -- re-reserves against the published mesh, and resumes; the final mesh must
  -- not know any of that happened.
  let bandColumns = [0 .. 7 :: Int]
      bandVertices =
        V.fromList
          ( Point 0 5
              : Point 90 5
              : concat
                  [ [Point x 10, Point x 0]
                  | column <- bandColumns
                  , let x = 10 * fromIntegral column + 5
                  ]
          )
  bandBuild <-
    requireRight
      "split band base"
      (constrainedDelaunayMaximal unitElementDefaults bandVertices V.empty)
  let bandAcceptedBuild = cdtAcceptedBuild bandBuild
      bandMapping = buildInputVertices bandAcceptedBuild
      bandHandle input = VertexId (indexPrimArray bandMapping input)
      bandBase = buildTriangulation bandAcceptedBuild
      bandRequests =
        V.fromList
          ( (bandHandle 0, bandHandle 1)
              : [ (bandHandle (2 * column + 2), bandHandle (2 * column + 3))
                | column <- bandColumns
                ]
          )
      replaySplit
        :: ConstrainedDelaunayTriangulation (Point)
        -> (VertexId, VertexId)
        -> Either (CdtError) (ConstrainedDelaunayTriangulation (Point))
      replaySplit triangulation request =
        constraintRecoveryTriangulation
          <$> uncurry (addConstraintAndSplit id triangulation) request
  bandBatch <-
    requireRight
      "split batch"
      (addConstraintsAndSplit id bandBase bandRequests)
  bandDescent <-
    requireRight
      "split singleton descent"
      (V.foldM' replaySplit bandBase bandRequests)
  assertEqual
    "split batch topology equals singleton descent"
    bandDescent
    (constraintRecoveryTriangulation bandBatch)
  assertEqual
    "split batch added one vertex per crossing"
    (numVertices bandBase + length bandColumns)
    (numVertices (constraintRecoveryTriangulation bandBatch))
  assertValid "split batch" (constraintRecoveryTriangulation bandBatch)

  -- The same law with the reservation exhausting inside one corridor rather
  -- than between corridors: the closing horizontal crosses two constraints
  -- the census can see and fifteen it cannot, so the corridor suspends with
  -- its cursor mid-walk and the resumed transaction continues from the last
  -- split vertex, not from the corridor's start.
  let laceColumns = [0 .. 16 :: Int]
      laceVertices =
        V.fromList
          ( Point 0 5
              : Point 90 5
              : concat
                  [ [Point x 10, Point x 0]
                  | column <- laceColumns
                  , let x = 5 * fromIntegral column + 5
                  ]
          )
      laceBuiltIn = V.fromList [(2, 3), (34, 35)]
  laceBuild <-
    requireRight
      "split lace base"
      (constrainedDelaunayMaximal unitElementDefaults laceVertices laceBuiltIn)
  let laceAcceptedBuild = cdtAcceptedBuild laceBuild
      laceMapping = buildInputVertices laceAcceptedBuild
      laceHandle input = VertexId (indexPrimArray laceMapping input)
      laceBase = buildTriangulation laceAcceptedBuild
      laceRequests =
        V.fromList
          ( [ (laceHandle (2 * column + 2), laceHandle (2 * column + 3))
            | column <- [1 .. 15]
            ]
              <> [(laceHandle 0, laceHandle 1)]
          )
  laceBatch <-
    requireRight
      "split lace batch"
      (addConstraintsAndSplit id laceBase laceRequests)
  laceDescent <-
    requireRight
      "split lace singleton descent"
      (V.foldM' replaySplit laceBase laceRequests)
  assertEqual
    "split lace batch topology equals singleton descent"
    laceDescent
    (constraintRecoveryTriangulation laceBatch)
  assertEqual
    "split lace batch added one vertex per crossing"
    (numVertices laceBase + length laceColumns)
    (numVertices (constraintRecoveryTriangulation laceBatch))
  assertValid "split lace batch" (constraintRecoveryTriangulation laceBatch)

  let verticesInput :: V.Vector (Point)
      verticesInput = V.fromList [Point 0 0, Point 4 0, Point 4 4, Point 0 4, Point 0 0]
      constraintsInput = V.fromList [(0, 2), (1, 3), (4, 1)]
  bulk <- requireRight "stable CDT bulk load" (constrainedDelaunayMaximal unitElementDefaults verticesInput constraintsInput)
  let bulkAcceptedBuild = cdtAcceptedBuild bulk
      bulkMapping = buildInputVertices bulkAcceptedBuild
  unless (sizeofPrimArray bulkMapping > 4) $ fail "buildInputVertices out of bounds"
  assertEqual "stable duplicate reroute" (VertexId (indexPrimArray bulkMapping 0)) (VertexId (indexPrimArray bulkMapping 4))
  assertEqual "conflict reporting" 1 (V.length (cdtRejectedConstraints bulk))
  assertValid "stable CDT bulk load" (buildTriangulation bulkAcceptedBuild)

requireAcceptedConstraint
  :: String
  -> ConstraintBatchResult vertex directed undirected face
  -> IO (V.Vector DirectedEdgeId, Int)
requireAcceptedConstraint label batch =
  case V.toList (constraintBatchOutcomes batch) of
    [ConstraintAccepted path added] -> pure (path, added)
    outcomes ->
      fail
        ( label
            <> ": expected one accepted constraint, got "
            <> show outcomes
        )

assertBatchStats
  :: String
  -> ConstraintBatchResult vertex directed undirected face
  -> IO ()
assertBatchStats label batch = do
  let stats = constraintBatchStats batch
      (accepted, rejected) =
        V.foldl'
          (\(!acceptedCount, !rejectedCount) outcome ->
            case outcome of
              ConstraintAccepted _ _ -> (acceptedCount + 1, rejectedCount)
              ConstraintRejected _ -> (acceptedCount, rejectedCount + 1)
          )
          (0, 0)
          (constraintBatchOutcomes batch)
  assertEqual
    (label <> " request count")
    (V.length (constraintBatchOutcomes batch))
    (constraintBatchRequests stats)
  assertEqual
    (label <> " accepted count")
    accepted
    (constraintBatchAccepted stats)
  assertEqual
    (label <> " rejected count")
    rejected
    (constraintBatchRejected stats)

replayConstraintRequest
  :: (ConstrainedDelaunayTriangulation (Point), [ConstraintOutcome])
  -> (VertexId, VertexId)
  -> Either
      (CdtError)
      (ConstrainedDelaunayTriangulation (Point), [ConstraintOutcome])
replayConstraintRequest (current, outcomes) request = do
  singleton <- recoverConstraints current (V.singleton request)
  case V.toList (constraintBatchOutcomes singleton) of
    [outcome] ->
      Right
        ( constraintBatchTriangulation singleton
        , outcome : outcomes
        )
    cardinality ->
      Left
        ( ConstraintBatchCardinalityMismatch
            1
            (length cardinality)
        )
