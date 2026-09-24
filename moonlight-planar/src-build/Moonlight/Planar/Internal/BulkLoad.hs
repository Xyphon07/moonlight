{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -O3 -fllvm -optlo-O3 -optlc-O3 #-}

-- | Generation. @delaunay@ builds a mesh from a whole site set by circle sweep;
-- the insertion verbs extend an existing mesh one site at a time.
module Moonlight.Planar.Internal.BulkLoad
  ( empty
  , clear
  , delaunay
  , delaunayGeometry
  , DuplicatePayloadPolicy (..)
  , delaunayFromCoordinates
  , delaunayFromAdmittedDistinctAscending
  , insert
  , insertAt
  , insertMany
  ) where

import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import qualified Data.IntSet as IntSet
import Data.Primitive.PrimArray
  ( MutablePrimArray
  , newPrimArray
  , readPrimArray
  , unsafeFreezePrimArray
  , writePrimArray
  )
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MUV
import Data.Word (Word32)
import Moonlight.Planar.Dcel (numInnerFaces, numVertices)
import Moonlight.Planar.Internal.HandleDefs
  ( VertexId (..)
  , directedEdgeIdIndex
  , faceIdIndex
  , vertexIdIndex
  )
import Moonlight.Planar.Internal.BoxedPaged (BoxedFill (..), boxedFromVector, boxedUpdate, emptyBoxedPaged)
import Moonlight.Planar.Internal.Capacity (ensureCapacity)
import Moonlight.Planar.Insertion (insertExistingVertexAtLocation, insertVertexAtPoint)
import Moonlight.Planar.Internal.Location (MutableLocation (..))
import Moonlight.Planar.Internal.Mutable
import Moonlight.Planar.Internal.OperationState
  ( Counter (..)
  , OperationState
  , addCounter
  , finalizeBuildStats
  , newOperationState
  , setCounter
  )
import Moonlight.Planar.Internal.CircleSweep
  ( RadiallyOrderedArena
  , circleSweepInsert
  , radiallyOrderArena
  , radiallyOrderGeometry
  )
import Moonlight.Planar.Internal.PointIndex
  ( MutablePointIndex
  , emptyPointIndex
  , newMutablePointIndex
  , resolveMutablePoint
  , seedMutablePointIndex
  )
import Moonlight.Planar.Scalar (canonicalScalarZero)
import Moonlight.Planar.Internal.Types (validatePoint)
import Moonlight.Planar.PointLocation (locatePointWithHint)
import Moonlight.Planar.Internal.Probe (Probe (..))
import Moonlight.Planar.Internal.Representation (CanonicalAdmission (..), Triangulation (..), authoringElementDefaults)
import Moonlight.Planar.Internal.PackedIndex (noIndex, unpackIndex)
import Moonlight.Planar.Internal.Paged (TransactionShape (DenseTransaction, LocalTransaction), emptyPaged, fromVector)
import Moonlight.Planar.Internal.Transaction (runTransaction)
import Moonlight.Planar.Types
import Moonlight.Planar.BuildStats (BuildStats, tabulateBuildStats, updateBuildStats, buildStat, BuildMetric (InputPoints, UniquePoints, ExistingPoints, DuplicatePoints, SpatialSeedPoints, LocationWalkSteps, LocationFallbacks, LocationMaxWalk))
import Moonlight.Planar.Point (Point (..), QueryPoint, queryPointValue, HasPosition (..))

-- | The vertexless triangulation: the outer face and nothing else. This is the
-- canonical origin of the type — bulk loading, incremental insertion and
-- refinement all agree with growing this value.
empty
  :: ElementDefaults directed undirected face
  -> Triangulation mode vertex directed undirected face
empty ElementDefaults{defaultDirectedEdgeData, defaultUndirectedEdgeData, defaultFaceData} =
  Triangulation
    { triPointX = emptyPaged
    , triPointY = emptyPaged
    , triPointIndex = emptyPointIndex
    , triVertexOut = emptyPaged
    , triVertexData = emptyBoxedPaged NoFill
    , triHalfTopology = emptyPaged
    , triDirectedData = emptyBoxedPaged (Fill defaultDirectedEdgeData)
    , triUndirectedData = emptyBoxedPaged (Fill defaultUndirectedEdgeData)
    , triFaceEdge = fromVector noIndex (U.singleton noIndex)
    , triFaceData = boxedFromVector (Fill defaultFaceData) (V.singleton defaultFaceData)
    , triConstraint = emptyPaged
    , triConstraintCount = 0
    , triConstraintEdges = IntSet.empty
    , triSeamFrontier = Nothing
    , triCanonical = CanonicalUnknown
    }

-- | Discard every vertex while retaining the element defaults the
-- triangulation was built with.
clear
  :: Triangulation mode vertex directed undirected face
  -> Triangulation mode vertex directed undirected face
clear = empty . authoringElementDefaults

-- | How a canonical bulk source combines payloads whose exact coordinates
-- coincide. Geometry identity is settled independently by the point index.
data DuplicatePayloadPolicy vertex
  = KeepFirstPayload
  | CombineDuplicatePayload !(vertex -> vertex -> vertex)

-- | The two lawful ingress sections for the one bulk construction engine.
-- Raw callers need coordinate admission and duplicate classification; an exact
-- SiteSet has already established canonical, distinct, lexicographically
-- ascending 'QueryPoint's. The latter remains package-internal through the
-- private build component, rather than becoming a second constructor.
data BulkInput vertex
  = RawPossiblyDuplicate
      !(Int -> Point)
      !(DuplicatePayloadPolicy vertex)
  | AdmittedDistinctAscending !(Int -> QueryPoint)

-- | The local identity verdict for one input position. A fresh verdict carries
-- the canonical coordinates already admitted to the mutable DCEL, so ingress
-- never rereads its own writes merely to accumulate the sweep centre.
data PositionClaim
  = ResidentPosition !Int
  | FreshPosition !Int !Double !Double

-- | Build a finite Delaunay DCEL while preserving the first input payload at
-- every duplicate position. The returned mapping relates every input slot to
-- the canonical stored vertex.
delaunay
  :: forall vertex directed undirected face
   . HasPosition vertex
  => ElementDefaults directed undirected face
  -> V.Vector vertex
  -> Either BuildError (BuildResult 'Unconstrained vertex directed undirected face)
delaunay defaults input =
  buildDelaunayFromSource
    defaults
    (V.length input)
    (RawPossiblyDuplicate (position . (input V.!)) KeepFirstPayload)
    (input V.!)

-- | Build the geometry-only Delaunay triangulation of a coordinate vector.
-- Exact duplicate positions collapse to one site. Use 'delaunay' or
-- 'delaunayFromCoordinates' when vertex annotations or the input-to-vertex
-- mapping are part of the result.
delaunayGeometry
  :: V.Vector Point
  -> Either BuildError (DelaunayTriangulation ())
delaunayGeometry coordinates
  | inputCount == 0 = Right (empty unitElementDefaults)
  | otherwise = do
    V.iforM_ coordinates (\index point -> () <$ validatePoint (Just index) point)
    ensureCapacity inputCount
    let !canonicalArena =
          U.generate inputCount $ \index ->
            case coordinates V.! index of
              Point rawX rawY ->
                ( 0
                , canonicalScalarZero rawX
                , canonicalScalarZero rawY
                , fromIntegral index
                )
        (!inputSumX, !inputSumY) =
          U.foldl'
            (\(!sumX, !sumY) (_, x, y, _) -> (sumX + x, sumY + y))
            (0, 0)
            canonicalArena
    runST $ do
      inputArena <- U.unsafeThaw canonicalArena
      let !inputScale = recip (fromIntegral inputCount)
          !inputCenterX = inputSumX * inputScale
          !inputCenterY = inputSumY * inputScale
          (distanceArena, pointXMutable, pointYMutable, inputOrder) =
            MUV.unzip4 inputArena
      assignGeometryRadialDistances
        distanceArena
        pointXMutable
        pointYMutable
        inputCenterX
        inputCenterY
      distances <- U.unsafeFreeze distanceArena
      pointXs <- U.unsafeFreeze pointXMutable
      pointYs <- U.unsafeFreeze pointYMutable
      orderedInputArena <-
        radiallyOrderGeometry distances pointXs pointYs inputOrder
      defaultedOutcome <-
        newMutableDcelWithVertexDefault
          ()
          unitElementDefaults
          (planarDcelCapacity inputCount)
      case defaultedOutcome of
        Left failure -> pure (Left failure)
        Right defaultedMutable -> do
          let !mutable = defaultedVertexDcel defaultedMutable
              !dense = defaultedDenseVertexDcel defaultedMutable
          operation <- newOperationState (halfEdgeCapacity mutable)
          unique <- appendSortedGeometry defaultedMutable pointXs pointYs inputOrder
          orderedArena <-
            if unique == inputCount
              then pure orderedInputArena
              else do
                let !scale = recip (fromIntegral unique)
                    !uniqueOrder = MUV.unsafeSlice 0 unique inputOrder
                (sumX, sumY) <- sumGeometryVertices dense uniqueOrder
                uniqueArena <-
                  fillRadialArena
                    mutable
                    (sumX * scale)
                    (sumY * scale)
                    (MUV.unsafeRead uniqueOrder)
                    unique
                radiallyOrderArena uniqueArena
          inserted <- circleSweepInsert mutable operation orderedArena
          case inserted of
            Left failure -> pure (Left failure)
            Right _ -> freezeTriangulation mutable
 where
  !inputCount = V.length coordinates

  assignGeometryRadialDistances
    :: forall s
     . MUV.MVector s Double
    -> MUV.MVector s Double
    -> MUV.MVector s Double
    -> Double
    -> Double
    -> ST s ()
  assignGeometryRadialDistances distances pointXs pointYs centerX centerY =
    MUV.imapM_
      (\index _ -> do
        x <- MUV.unsafeRead pointXs index
        y <- MUV.unsafeRead pointYs index
        let !deltaX = centerX - x
            !deltaY = centerY - y
        MUV.unsafeWrite distances index (deltaX * deltaX + deltaY * deltaY)
      )
      distances

  appendSortedGeometry
    :: forall s
     . DefaultedVertexDcel s () () () ()
    -> U.Vector Double
    -> U.Vector Double
    -> MUV.MVector s Word32
    -> ST s Int
  appendSortedGeometry defaultedMutable pointXs pointYs order
    | inputCount == 0 = pure 0
    | otherwise = do
        firstInput <- MUV.unsafeRead order 0
        let !x = U.unsafeIndex pointXs (fromIntegral firstInput)
            !y = U.unsafeIndex pointYs (fromIntegral firstInput)
        denseWriteFreshDefaultPoint defaultedMutable 0 x y
        MUV.unsafeWrite order 0 0
        (_, _, unique) <-
          MUV.foldM'
            (\(!previousX, !previousY, !uniqueCount) nextInput ->
               let !nextX = U.unsafeIndex pointXs (fromIntegral nextInput)
                   !nextY = U.unsafeIndex pointYs (fromIntegral nextInput)
                in if nextX == previousX && nextY == previousY
                     then pure (previousX, previousY, uniqueCount)
                     else do
                       denseWriteFreshDefaultPoint defaultedMutable uniqueCount nextX nextY
                       MUV.unsafeWrite order uniqueCount (fromIntegral uniqueCount)
                       pure (nextX, nextY, uniqueCount + 1)
            )
            (x, y, 1)
            (MUV.unsafeSlice 1 (inputCount - 1) order)
        unique <$ denseCommitFreshDefaultPoints defaultedMutable unique

  sumGeometryVertices
    :: forall s
     . DenseMutableDcel s () () () ()
    -> MUV.MVector s Word32
    -> ST s (Double, Double)
  sumGeometryVertices dense vertices =
    MUV.foldM'
      (\(!sumX, !sumY) raw -> do
         x <- denseReadPointX dense (unpackIndex raw)
         y <- denseReadPointY dense (unpackIndex raw)
         pure (sumX + x, sumY + y)
      )
      (0, 0)
      vertices

-- | Canonical construction from separate geometry and annotation sources.
-- The coordinate vector remains the only geometry in ingress; payloads never
-- acquire a fabricated 'HasPosition' instance merely to reach the loader.
delaunayFromCoordinates
  :: forall vertex directed undirected face
   . ElementDefaults directed undirected face
  -> V.Vector (Point)
  -> V.Vector vertex
  -> DuplicatePayloadPolicy vertex
  -> Either BuildError (BuildResult 'Unconstrained vertex directed undirected face)
delaunayFromCoordinates defaults coordinates payloads duplicatePolicy
  | coordinateCount /= payloadCount =
      Left (CoordinatePayloadCountMismatch coordinateCount payloadCount)
  | otherwise =
      buildDelaunayFromSource
        defaults
        coordinateCount
        (RawPossiblyDuplicate (coordinates V.!) duplicatePolicy)
        (payloads V.!)
 where
  !coordinateCount = V.length coordinates
  !payloadCount = V.length payloads

-- | Construction from an already-admitted exact site section. The caller
-- supplies Map-ascending, distinct 'QueryPoint's; this skips only raw
-- admission, duplicate classification, and unique-site recentering. It still
-- derives the radial execution order and runs the one circle sweep.
delaunayFromAdmittedDistinctAscending
  :: forall vertex directed undirected face
   . ElementDefaults directed undirected face
  -> V.Vector (QueryPoint, vertex)
  -> Either BuildError (BuildResult 'Unconstrained vertex directed undirected face)
delaunayFromAdmittedDistinctAscending defaults input =
  buildDelaunayFromSource
    defaults
    (V.length input)
    (AdmittedDistinctAscending (\index -> fst (input V.! index)))
    (\index -> snd (input V.! index))
{-# INLINE delaunayFromAdmittedDistinctAscending #-}

buildDelaunayFromSource
  :: forall vertex directed undirected face
   . ElementDefaults directed undirected face
  -> Int
  -> BulkInput vertex
  -> (Int -> vertex)
  -> Either BuildError (BuildResult 'Unconstrained vertex directed undirected face)
buildDelaunayFromSource defaults inputCount input payloadAtInput = do
  ensureCapacity inputCount
  runST $ do
    inputArena <- MUV.new inputCount
    admitted <- initializeInputArena inputArena
    case admitted of
      Left failure -> pure (Left failure)
      Right (inputSumX, inputSumY) -> do
        mutable <- newMutableDcel defaults (planarDcelCapacity inputCount)
        operation <- newOperationState (halfEdgeCapacity mutable)
        (mapping, unique, orderedArena) <-
          case input of
            RawPossiblyDuplicate pointAtInput duplicatePolicy ->
              prepareRawInput
                inputArena
                mutable
                pointAtInput
                duplicatePolicy
                inputSumX
                inputSumY
            AdmittedDistinctAscending _ ->
              prepareAdmittedDistinctInput inputArena mutable inputSumX inputSumY
        setCounter operation (Reported InputPoints) inputCount
        setCounter operation (Reported UniquePoints) unique
        setCounter operation (Reported DuplicatePoints) (inputCount - unique)
        inserted <-
          if unique == 0
            then pure (Right 0)
            else circleSweepInsert mutable operation orderedArena
        case inserted of
          Left failure -> pure (Left failure)
          Right seedCount -> do
            setCounter operation (Reported SpatialSeedPoints) seedCount
            frozenOutcome <- freezeTriangulation mutable
            case frozenOutcome of
              Left failure -> pure (Left failure)
              Right frozen -> do
                mapped <- unsafeFreezePrimArray mapping
                stats <- finalizeBuildStats operation
                pure
                  ( Right
                      BuildResult
                        { buildTriangulation = frozen
                        , buildInputVertices = mapped
                        , buildStats = stats
                        }
                  )
 where
  -- Raw sources validate and canonicalize at this one ingress. Admitted
  -- sources merely project their retained QueryPoint evidence into the sweep
  -- arena; neither route reconstructs coordinate identity downstream.
  initializeInputArena
    :: forall s
     . MUV.MVector s (Double, Double, Double, Word32)
    -> ST s (Either BuildError (Double, Double))
  initializeInputArena arena =
    case input of
      RawPossiblyDuplicate pointAtInput _ ->
        initializeRawInputArena arena pointAtInput 0 0 0
      AdmittedDistinctAscending pointAtInput ->
        initializeAdmittedInputArena arena pointAtInput

  initializeRawInputArena
    :: forall s
     . MUV.MVector s (Double, Double, Double, Word32)
    -> (Int -> Point)
    -> Int
    -> Double
    -> Double
    -> ST s (Either BuildError (Double, Double))
  initializeRawInputArena arena pointAtInput !index !sumX !sumY
    | index >= inputCount = pure (Right (sumX, sumY))
    | otherwise =
        case validatePoint (Just index) (pointAtInput index) of
          Left failure -> pure (Left failure)
          Right admitted ->
            case queryPointValue admitted of
              Point x y -> do
                MUV.unsafeWrite arena index (0, x, y, fromIntegral index)
                initializeRawInputArena
                  arena
                  pointAtInput
                  (index + 1)
                  (sumX + x)
                  (sumY + y)

  initializeAdmittedInputArena
    :: forall s
     . MUV.MVector s (Double, Double, Double, Word32)
    -> (Int -> QueryPoint)
    -> ST s (Either BuildError (Double, Double))
  initializeAdmittedInputArena arena pointAtInput =
    Right
      <$> MUV.ifoldM'
        (\(!sumX, !sumY) index _ ->
           case queryPointValue (pointAtInput index) of
             Point x y -> do
               MUV.unsafeWrite arena index (0, x, y, fromIntegral index)
               pure (sumX + x, sumY + y)
        )
        (0, 0)
        arena

  prepareRawInput
    :: forall s
     . MUV.MVector s (Double, Double, Double, Word32)
    -> MutableDcel s vertex directed undirected face
    -> (Int -> Point)
    -> DuplicatePayloadPolicy vertex
    -> Double
    -> Double
    -> ST s (MutablePrimArray s Word32, Int, RadiallyOrderedArena s)
  prepareRawInput inputArena mutable pointAtInput duplicatePolicy inputSumX inputSumY = do
    let !inputScale = if inputCount == 0 then 0 else recip (fromIntegral inputCount)
        !inputCenterX = inputSumX * inputScale
        !inputCenterY = inputSumY * inputScale
    assignRadialDistances inputArena inputCenterX inputCenterY 0
    orderedInputArena <- radiallyOrderArena inputArena
    mapping <- newPrimArray inputCount
    unique <- classifySortedInputs mapping inputArena
    (sumX, sumY) <-
      appendClassifiedInputs mutable mapping pointAtInput duplicatePolicy 0 0 0
    orderedArena <-
      if unique == inputCount
        then pure orderedInputArena
        else do
          let !scale = recip (fromIntegral unique)
              !uniqueArena = MUV.unsafeSlice 0 unique inputArena
          rewriteCompactedArena
            mapping
            (sumX * scale)
            (sumY * scale)
            uniqueArena
            0
          radiallyOrderArena uniqueArena
    pure (mapping, unique, orderedArena)

  prepareAdmittedDistinctInput
    :: forall s
     . MUV.MVector s (Double, Double, Double, Word32)
    -> MutableDcel s vertex directed undirected face
    -> Double
    -> Double
    -> ST s (MutablePrimArray s Word32, Int, RadiallyOrderedArena s)
  prepareAdmittedDistinctInput inputArena mutable inputSumX inputSumY = do
    mapping <- newPrimArray inputCount
    appendAdmittedDistinctInputs mutable mapping inputArena
    let !inputScale = if inputCount == 0 then 0 else recip (fromIntegral inputCount)
        !inputCenterX = inputSumX * inputScale
        !inputCenterY = inputSumY * inputScale
    assignRadialDistances inputArena inputCenterX inputCenterY 0
    orderedArena <- radiallyOrderArena inputArena
    pure (mapping, inputCount, orderedArena)

  -- An admitted SiteSet is already distinct. Sequential append therefore
  -- transports the identity class map directly into authoritative vertex ids,
  -- without reopening duplicate policy or recentering a compacted arena.
  appendAdmittedDistinctInputs
    :: forall s
     . MutableDcel s vertex directed undirected face
    -> MutablePrimArray s Word32
    -> MUV.MVector s (Double, Double, Double, Word32)
    -> ST s ()
  appendAdmittedDistinctInputs mutable mapping arena =
    MUV.imapM_
      (\index (_, x, y, _) -> do
         vertex <- appendVertexCoordinates mutable x y (payloadAtInput index)
         writePrimArray mapping index (fromIntegral vertex)
         MUV.unsafeWrite arena index (0, x, y, fromIntegral vertex)
      )
      arena

  -- The all-input centre seeds the first radial ordering. Duplicate
  -- equivalence classes become adjacent in that total order; after descent,
  -- the compacted section is recentered over unique sites and ordered once
  -- more only when required.
  assignRadialDistances
    :: forall s
     . MUV.MVector s (Double, Double, Double, Word32)
    -> Double
    -> Double
    -> Int
    -> ST s ()
  assignRadialDistances arena centerX centerY !index
    | index >= inputCount = pure ()
    | otherwise = do
        (_, x, y, inputOrdinal) <- MUV.unsafeRead arena index
        let !deltaX = centerX - x
            !deltaY = centerY - y
        MUV.unsafeWrite arena index (deltaX * deltaX + deltaY * deltaY, x, y, inputOrdinal)
        assignRadialDistances arena centerX centerY (index + 1)

  -- Classify the sorted local sections by exact canonical position. The
  -- mapping first names each class by its earliest input slot. Compacting the
  -- unique radial representatives in place cannot overwrite an unread slot.
  classifySortedInputs
    :: forall s
     . MutablePrimArray s Word32
    -> MUV.MVector s (Double, Double, Double, Word32)
    -> ST s Int
  classifySortedInputs mapping arena
    | inputCount == 0 = pure 0
    | otherwise = do
        first@(_, firstX, firstY, firstInput) <- MUV.unsafeRead arena 0
        writePrimArray mapping (fromIntegral firstInput) firstInput
        classifyFrom first firstX firstY firstInput 1 1
   where
    classifyFrom
      :: (Double, Double, Double, Word32)
      -> Double
      -> Double
      -> Word32
      -> Int
      -> Int
      -> ST s Int
    classifyFrom !_ !previousX !previousY !classInput !readIndex !uniqueCount
      | readIndex >= inputCount = pure uniqueCount
      | otherwise = do
          record@(_, x, y, inputOrdinal) <- MUV.unsafeRead arena readIndex
          if x == previousX && y == previousY
            then do
              writePrimArray mapping (fromIntegral inputOrdinal) classInput
              classifyFrom record previousX previousY classInput (readIndex + 1) uniqueCount
            else do
              writePrimArray mapping (fromIntegral inputOrdinal) inputOrdinal
              if uniqueCount == readIndex
                then pure ()
                else MUV.unsafeWrite arena uniqueCount record
              classifyFrom record x y inputOrdinal (readIndex + 1) (uniqueCount + 1)

  -- Materialize in original order, preserving the established handle
  -- assignment and duplicate-payload law. The equivalence mapping for a
  -- duplicate always points backward to an already materialized class owner.
  appendClassifiedInputs
    :: forall s
     . MutableDcel s vertex directed undirected face
    -> MutablePrimArray s Word32
    -> (Int -> Point)
    -> DuplicatePayloadPolicy vertex
    -> Int
    -> Double
    -> Double
    -> ST s (Double, Double)
  appendClassifiedInputs mutable mapping pointAtInput duplicatePolicy !index !sumX !sumY
    | index >= inputCount = pure (sumX, sumY)
    | otherwise = do
        classInput <- readPrimArray mapping index
        let !vertexData = payloadAtInput index
        if fromIntegral classInput == index
          then case pointAtInput index of
            Point x y -> do
              let !canonicalX = canonicalScalarZero x
                  !canonicalY = canonicalScalarZero y
              vertex <- appendVertexCoordinates mutable canonicalX canonicalY vertexData
              writePrimArray mapping index (fromIntegral vertex)
              appendClassifiedInputs
                mutable
                mapping
                pointAtInput
                duplicatePolicy
                (index + 1)
                (sumX + canonicalX)
                (sumY + canonicalY)
          else do
            resident <- fromIntegral <$> readPrimArray mapping (fromIntegral classInput)
            writePrimArray mapping index (fromIntegral resident)
            case duplicatePolicy of
              KeepFirstPayload -> pure ()
              CombineDuplicatePayload combine -> do
                residentData <- vertexDataAt mutable resident
                writeVertexData mutable resident (combine residentData vertexData)
            appendClassifiedInputs
              mutable
              mapping
              pointAtInput
              duplicatePolicy
              (index + 1)
              sumX
              sumY

  -- Once duplicates have shortened the vertex arena, translate each compacted
  -- representative from its input-class name to its authoritative vertex and
  -- restate its radial key around the exact unique-site centre.
  rewriteCompactedArena
    :: forall s
     . MutablePrimArray s Word32
    -> Double
    -> Double
    -> MUV.MVector s (Double, Double, Double, Word32)
    -> Int
    -> ST s ()
  rewriteCompactedArena mapping centerX centerY arena !index
    | index >= MUV.length arena = pure ()
    | otherwise = do
        (_, x, y, classInput) <- MUV.unsafeRead arena index
        vertex <- readPrimArray mapping (fromIntegral classInput)
        let !deltaX = centerX - x
            !deltaY = centerY - y
        MUV.unsafeWrite arena index (deltaX * deltaX + deltaY * deltaY, x, y, vertex)
        rewriteCompactedArena mapping centerX centerY arena (index + 1)

-- | Insert or replace a vertex payload. A payload at an existing position is
-- overwritten without changing topology.
--
-- The published mesh is independent of the one passed in. A singleton below
-- ten thousand resident sites copies densely; larger bases publish through
-- copy-on-write pages. A caller inserting a sequence wants one
-- 'Moonlight.Planar.Session.withSession' over
-- 'Moonlight.Planar.Session.insertVertex' instead — see 'insertAt'.
insert
  :: HasPosition vertex
  => Triangulation mode vertex directed undirected face
  -> vertex
  -> Either BuildError (InsertionResult mode vertex directed undirected face)
insert triangulation vertexData = insertAt triangulation (position vertexData) vertexData

-- | Insert at a stated point. 'insert' is this with the point read out of the
-- payload, which is what a caller holding only a payload wants; a caller that
-- computed the point — a constraint split, a Steiner refinement — wants to say
-- so rather than build a payload and hope the round trip through 'HasPosition'
-- returns what it started with.
--
-- This is one shaped transaction over a single insertion. Replacing a fold of
-- it with one session is sound because the two agree on every mesh and differ
-- only in how many intermediate meshes they publish. A fold publishes @k@
-- meshes and pays a thaw for each, so it runs in Θ(n·k); the session pays one
-- thaw and runs in O(k·log n) expected.
insertAt
  :: Triangulation mode vertex directed undirected face
  -> Point
  -> vertex
  -> Either BuildError (InsertionResult mode vertex directed undirected face)
insertAt triangulation rawPoint vertexData = do
  queryPoint <- validatePoint Nothing rawPoint
  case locatePointWithHint triangulation Nothing queryPoint of
    (OnVertex resident, walked) ->
      Right (replaceResidentPayload triangulation resident walked vertexData)
    (located, walked) -> do
      let transactionShape =
            if numVertices triangulation < 10_000
              then DenseTransaction
              else LocalTransaction
      ((vertex, disposition), frozen, stats) <-
        runTransaction
          id
          transactionShape
          triangulation
          1
          (\mutable operation -> do
             addCounter operation (Reported InputPoints) 1
             inserted <-
               case located of
                 -- A frozen degenerate-line location points at an arbitrary visible
                 -- segment, while the line extension interpreter requires a terminal
                 -- edge. The frozen section carries no terminal witness, so retain
                 -- the existing mutable line locator for this one non-lawful case.
                 OutsideConvexHull (Just _)
                   | numInnerFaces triangulation == 0 ->
                       insertVertexAtPoint @'ProbeOff mutable operation Nothing (queryPointValue queryPoint) vertexData
                 _ -> do
                   capacityOutcome <- ensurePointCapacity mutable 1
                   case capacityOutcome of
                     Left failure -> pure (Left failure)
                     Right () -> do
                       vertex <- appendVertex mutable (queryPointValue queryPoint) vertexData
                       let thawedLocation =
                             case located of
                               EmptyTriangulation -> MutableEmpty
                               OnEdge edge -> MutableOnEdge (directedEdgeIdIndex edge)
                               InFace face -> MutableInFace (faceIdIndex face)
                               -- The frozen locator emits no edge only for a
                               -- singleton mesh. Its mutable interpreter ignores
                               -- this sentinel while constructing the second vertex.
                               OutsideConvexHull Nothing -> MutableOutsideHull 0
                               OutsideConvexHull (Just edge) -> MutableOutsideHull (directedEdgeIdIndex edge)
                       ((vertex, Inserted) <$) <$> insertExistingVertexAtLocation @'ProbeOff mutable operation vertex thawedLocation
             case inserted of
               Left failure -> pure (Left failure)
               Right (vertex, disposition) -> do
                 case disposition of
                   Inserted -> addCounter operation (Reported UniquePoints) 1
                   AlreadyPresent -> do
                     writeVertexData mutable vertex vertexData
                     addCounter operation (Reported ExistingPoints) 1
                     addCounter operation (Reported DuplicatePoints) 1
                 pure (Right (vertex, disposition))
          )
      pure
        InsertionResult
          { insertionTriangulation = frozen
          , insertionVertex = VertexId (fromIntegral vertex)
          , insertionDisposition = disposition
          , insertionStats = withFrozenLocationStats walked stats
          }

-- | Publish a payload replacement without opening a transaction.
--
-- A position already resident changes exactly one thing: the payload slot the
-- vertex already occupies. No coordinate, no half-edge, no constraint flag and
-- no face record differs, so the five unboxed planes are the ones the argument
-- already holds rather than copies taken out of it — which is what a thaw costs
-- and what this exists to refuse. They are immutable values; nothing reached
-- from here is a mutable buffer, and 'boxedUpdate' materializes a fresh page
-- for the one it rewrites, leaving the argument's own directory intact.
--
-- The location counters are the frozen walk's, not a thawed walk's. They
-- describe the walk that actually ran.
replaceResidentPayload
  :: Triangulation mode vertex directed undirected face
  -> VertexId
  -> LocationStats
  -> vertex
  -> InsertionResult mode vertex directed undirected face
replaceResidentPayload triangulation resident walked vertexData =
  InsertionResult
    { insertionTriangulation =
        triangulation
          { triVertexData =
              boxedUpdate (vertexIdIndex resident) vertexData (triVertexData triangulation)
          }
    , insertionVertex = resident
    , insertionDisposition = AlreadyPresent
    , insertionStats =
        withFrozenLocationStats
          walked
          (tabulateBuildStats (\metric -> case metric of
            InputPoints -> 1
            ExistingPoints -> 1
            DuplicatePoints -> 1
            _ -> 0))
    }

-- | Add the frozen locator's observation to the local topology interpreter's
-- operation-owned counters. Direct frozen-site insertion contributes no mutable
-- walk; the degenerate fallback contributes its real mutable walk rather than
-- having it erased from the published result.
withFrozenLocationStats :: LocationStats -> BuildStats -> BuildStats
withFrozenLocationStats walked stats = updateBuildStats
  [ (LocationWalkSteps, locationWalkSteps walked + buildStat LocationWalkSteps stats)
  , (LocationMaxWalk, max (locationWalkSteps walked) (buildStat LocationMaxWalk stats))
  , (LocationFallbacks, (if locationUsedFallback walked then 1 else 0) + buildStat LocationFallbacks stats)
  ] stats

-- | Apply a batch in one page transaction. Duplicate positions are processed
-- in input order, so their last payload wins exactly as repeated 'insert'
-- calls would, while topology is inserted only once per new position.
insertMany
  :: forall mode vertex directed undirected face
   . HasPosition vertex
  => Triangulation mode vertex directed undirected face
  -> V.Vector vertex
  -> Either BuildError (BuildResult mode vertex directed undirected face)
insertMany triangulation input = do
  validateVertices input
  (mapped, frozen, stats) <-
    runTransaction id DenseTransaction triangulation (V.length input) $ \mutable operation -> do
      table <- newMutablePointIndex (pointCapacity mutable)
      seeded <- seedPointTable mutable table
      case seeded of
        Left failure -> pure (Left failure)
        Right () -> do
          mapping <- newPrimArray (V.length input)
          freshBuffer <- MUV.new (V.length input)
          filled <- fill mutable operation table mapping freshBuffer 0 0 0 0
          case filled of
            Left failure -> pure (Left failure)
            Right (sumX, sumY, freshCount) -> do
              let !existingCount = V.length input - freshCount
              setCounter operation (Reported InputPoints) (V.length input)
              setCounter operation (Reported UniquePoints) freshCount
              setCounter operation (Reported ExistingPoints) existingCount
              setCounter operation (Reported DuplicatePoints) existingCount
              inserted <-
                if freshCount == 0
                  then pure (Right 0)
                  else do
                    let !scale = recip (fromIntegral freshCount)
                    arena <-
                      fillRadialArena
                        mutable
                        (sumX * scale)
                        (sumY * scale)
                        (MUV.unsafeRead freshBuffer)
                        freshCount
                    orderedArena <- radiallyOrderArena arena
                    circleSweepInsert mutable operation orderedArena
              case inserted of
                Left failure -> pure (Left failure)
                Right seedCount -> do
                  setCounter operation (Reported SpatialSeedPoints) seedCount
                  Right <$> unsafeFreezePrimArray mapping
  pure
    BuildResult
      { buildTriangulation = frozen
      , buildInputVertices = mapped
      , buildStats = stats
      }
 where
  fill
    :: forall s
     . MutableDcel s vertex directed undirected face
    -> OperationState s
    -> MutablePointIndex s
    -> MutablePrimArray s Word32
    -> MUV.MVector s Word32
    -> Int
    -> Double
    -> Double
    -> Int
    -> ST s (Either BuildError (Double, Double, Int))
  fill mutable operation table mapping freshBuffer !index !sumX !sumY !freshCount
    | index >= V.length input = pure (Right (sumX, sumY, freshCount))
    | otherwise = do
        let !vertexData = input V.! index
        claimed <- claimPosition mutable table (position vertexData) vertexData
        case claimed of
          Left failure -> pure (Left failure)
          Right (FreshPosition vertex canonicalX canonicalY) -> do
            writePrimArray mapping index (fromIntegral vertex)
            MUV.unsafeWrite freshBuffer freshCount (fromIntegral vertex)
            fill
              mutable
              operation
              table
              mapping
              freshBuffer
              (index + 1)
              (sumX + canonicalX)
              (sumY + canonicalY)
              (freshCount + 1)
          Right (ResidentPosition vertex) -> do
            writePrimArray mapping index (fromIntegral vertex)
            writeVertexData mutable vertex vertexData
            fill mutable operation table mapping freshBuffer (index + 1) sumX sumY freshCount

-- | One packed radial record per swept vertex — the derived sort fields and
-- the vertex handle, nothing else — filled straight from the coordinate
-- arenas and consumed in place by the sweep. The squared distance is stated
-- against the ingress-accumulated centre, in the widened comparison format.
fillRadialArena
  :: MutableDcel s vertex directed undirected face
  -> Double
  -> Double
  -> (Int -> ST s Word32)
  -> Int
  -> ST s (MUV.MVector s (Double, Double, Double, Word32))
fillRadialArena mutable centerX centerY lookupId count = do
  arena <- MUV.new count
  forM_ [0 .. count - 1] $ \index -> do
    raw <- lookupId index
    x <- readPointX mutable (unpackIndex raw)
    y <- readPointY mutable (unpackIndex raw)
    let !wideX = x
        !wideY = y
        !deltaX = centerX - wideX
        !deltaY = centerY - wideY
    MUV.unsafeWrite arena index (deltaX * deltaX + deltaY * deltaY, wideX, wideY, raw)
  pure arena

-- | Claim a position for the vertex the arena would append next, or answer the
-- vertex already holding it. The claim is written before the append, so the two
-- must stay adjacent: nothing may consume a vertex slot in between.
claimPosition
  :: MutableDcel s vertex directed undirected face
  -> MutablePointIndex s
  -> Point
  -> vertex
  -> ST s (Either BuildError PositionClaim)
claimPosition mutable table rawPoint vertexData =
  case rawPoint of
    Point x y -> do
      let !canonicalX = canonicalScalarZero x
          !canonicalY = canonicalScalarZero y
      slot <- nextVertexSlot mutable
      let !candidate = nextVertexSlotIndex slot
      owner <-
        resolveMutablePoint
          table
          (readPointX mutable)
          (readPointY mutable)
          canonicalX
          canonicalY
          candidate
      case owner of
        Left failure -> pure (Left failure)
        Right (Just existing) -> pure (Right (ResidentPosition existing))
        Right Nothing -> do
          vertex <- appendVertexCoordinatesAtSlot mutable slot canonicalX canonicalY vertexData
          pure (Right (FreshPosition vertex canonicalX canonicalY))
-- | Enter the positions a batch inherits from the triangulation it extends, so
-- that an input repeating one of them maps to the vertex already there.
seedPointTable
  :: MutableDcel s vertex directed undirected face
  -> MutablePointIndex s
  -> ST s (Either BuildError ())
seedPointTable mutable table = do
  existing <- pointCount mutable
  seedMutablePointIndex
    table
    existing
    (readPointX mutable)
    (readPointY mutable)

validateVertices :: HasPosition vertex => V.Vector vertex -> Either BuildError ()
validateVertices vertices =
  V.iforM_ vertices (\index vertexData -> validatePoint (Just index) (position vertexData))
