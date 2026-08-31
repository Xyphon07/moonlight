{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Moonlight.LinAlg.Pure.Sparse.Structured
  ( GraphEdge (..),
    TridiagonalRejection (..),
    symmetricTridiagonalFromCSR,
    diagonalCSR,
    tridiagonalCSR,
    pathLaplacianCSR,
    graphLaplacianCSR,
  )
where

import Control.Monad (foldM)
import Control.Monad.ST (ST, runST)
import Data.Kind (Type)
import Data.List (sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Ord (comparing)
import Data.Vector qualified as Box
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as MU
import Moonlight.Core (AdditiveGroup (..), AdditiveMonoid (..), MoonlightError (..), fieldValueValid)
import Moonlight.LinAlg.Pure.Sparse.Assembly
  ( orderedCSRFromEntries,
  )
import Moonlight.LinAlg.Pure.Sparse.Types
  ( SparseCSR,
    CSRExecutionPlan (..),
    csrFromCanonicalVectorsUnchecked,
    csrFromCanonicalVectorsWithPlanUnchecked,
    csrCols,
    csrColumnIndicesVector,
    csrRows,
    csrRowOffsetsVector,
    csrValuesVector,
    validateCSR,
  )
import Moonlight.LinAlg.Pure.Structured.Tridiagonal
  ( SymmetricTridiagonal,
    mkSymmetricTridiagonalVectors,
    pathLaplacianBands,
  )
import Prelude

type TridiagonalRejection :: Type
data TridiagonalRejection
  = TridiagonalNonSquare !Int !Int
  | TridiagonalOutOfBandEntry !Int !Int
  | TridiagonalAsymmetricOffDiagonal
  deriving stock (Eq, Show)

symmetricTridiagonalFromCSR ::
  SparseCSR Double ->
  Either
    MoonlightError
    (Either TridiagonalRejection SymmetricTridiagonal)
symmetricTridiagonalFromCSR csrValue = do
  validateCSR csrValue
  if csrRows csrValue /= csrCols csrValue
    then
      Right
        ( Left
            ( TridiagonalNonSquare
                (csrRows csrValue)
                (csrCols csrValue)
            )
        )
    else do
      let !values = csrValuesVector csrValue
      if U.any (not . fieldValueValid) values
        then
          Left
            ( InvariantViolation
                "symmetric tridiagonal classification requires finite CSR entries"
            )
        else
          case classifyTridiagonalStorage csrValue of
            Left rejection -> Right (Left rejection)
            Right (diagonalEntries, lowerEntries, upperEntries) ->
              if symmetricOffDiagonalEntries lowerEntries upperEntries
                then do
                  let !matrixSize = U.length diagonalEntries
                      !offDiagonalEntries =
                        U.generate
                          (max 0 (matrixSize - 1))
                          (U.unsafeIndex upperEntries)
                  Right
                    <$> mkSymmetricTridiagonalVectors
                      diagonalEntries
                      offDiagonalEntries
                else Right (Left TridiagonalAsymmetricOffDiagonal)

classifyTridiagonalStorage ::
  SparseCSR Double ->
  Either
    TridiagonalRejection
    (U.Vector Double, U.Vector Double, U.Vector Double)
classifyTridiagonalStorage csrValue =
  runST $ do
    diagonalEntries <- MU.replicate matrixSize 0.0
    lowerEntries <- MU.replicate matrixSize 0.0
    upperEntries <- MU.replicate matrixSize 0.0
    classification <-
      classifyRows
        diagonalEntries
        lowerEntries
        upperEntries
        0
    case classification of
      Left rejection -> pure (Left rejection)
      Right () -> do
        frozenDiagonal <- U.unsafeFreeze diagonalEntries
        frozenLower <- U.unsafeFreeze lowerEntries
        frozenUpper <- U.unsafeFreeze upperEntries
        pure
          ( Right
              (frozenDiagonal, frozenLower, frozenUpper)
          )
  where
    !matrixSize = csrRows csrValue
    !rowOffsets = csrRowOffsetsVector csrValue
    !columnIndices = csrColumnIndicesVector csrValue
    !values = csrValuesVector csrValue

    classifyRows ::
      MU.MVector s Double ->
      MU.MVector s Double ->
      MU.MVector s Double ->
      Int ->
      ST s (Either TridiagonalRejection ())
    classifyRows diagonalEntries lowerEntries upperEntries !rowIndex
      | rowIndex >= matrixSize = pure (Right ())
      | otherwise = do
          let !startIndex = rowOffsets `U.unsafeIndex` rowIndex
              !stopIndex = rowOffsets `U.unsafeIndex` (rowIndex + 1)
          rowClassification <-
            classifyRowEntries
              diagonalEntries
              lowerEntries
              upperEntries
              rowIndex
              startIndex
              stopIndex
          case rowClassification of
            Left rejection -> pure (Left rejection)
            Right () ->
              classifyRows
                diagonalEntries
                lowerEntries
                upperEntries
                (rowIndex + 1)

    classifyRowEntries ::
      MU.MVector s Double ->
      MU.MVector s Double ->
      MU.MVector s Double ->
      Int ->
      Int ->
      Int ->
      ST s (Either TridiagonalRejection ())
    classifyRowEntries
      diagonalEntries
      lowerEntries
      upperEntries
      !rowIndex
      !entryIndex
      !stopIndex
        | entryIndex >= stopIndex = pure (Right ())
        | otherwise = do
            let !columnIndex = columnIndices `U.unsafeIndex` entryIndex
                !entryValue = values `U.unsafeIndex` entryIndex
                !offset = columnIndex - rowIndex
            case offset of
              -1 -> do
                addMutableEntry lowerEntries rowIndex entryValue
                continue
              0 -> do
                addMutableEntry diagonalEntries rowIndex entryValue
                continue
              1 -> do
                addMutableEntry upperEntries rowIndex entryValue
                continue
              _ ->
                pure
                  ( Left
                      ( TridiagonalOutOfBandEntry
                          rowIndex
                          columnIndex
                      )
                  )
      where
        continue =
          classifyRowEntries
            diagonalEntries
            lowerEntries
            upperEntries
            rowIndex
            (entryIndex + 1)
            stopIndex

addMutableEntry ::
  MU.MVector s Double ->
  Int ->
  Double ->
  ST s ()
addMutableEntry targetVector !entryIndex !entryValue = do
  currentValue <- MU.unsafeRead targetVector entryIndex
  MU.unsafeWrite
    targetVector
    entryIndex
    (currentValue + entryValue)
{-# INLINE addMutableEntry #-}

symmetricOffDiagonalEntries ::
  U.Vector Double ->
  U.Vector Double ->
  Bool
symmetricOffDiagonalEntries lowerEntries upperEntries =
  go 0
  where
    !entryCount = max 0 (U.length lowerEntries - 1)

    go !entryIndex
      | entryIndex >= entryCount = True
      | upperEntries `U.unsafeIndex` entryIndex
          == lowerEntries `U.unsafeIndex` (entryIndex + 1) =
          go (entryIndex + 1)
      | otherwise = False
{-# INLINE symmetricOffDiagonalEntries #-}

type GraphEdge :: Type -> Type
data GraphEdge vertex = GraphEdge
  { graphEdgeLeft :: !vertex,
    graphEdgeRight :: !vertex,
    graphEdgeWeight :: !Double
  }
  deriving stock (Eq, Ord, Show)

type IndexedGraphEdge :: Type
data IndexedGraphEdge = IndexedGraphEdge
  { indexedGraphEdgeLeft :: !Int,
    indexedGraphEdgeRight :: !Int,
    indexedGraphEdgeWeight :: !Double
  }
  deriving stock (Eq, Show)

type IndexedGraphEdgeRows :: Type
data IndexedGraphEdgeRows = IndexedGraphEdgeRows
  { indexedGraphEdgeOffsets :: !(U.Vector Int),
    indexedGraphEdgeRights :: !(U.Vector Int),
    indexedGraphEdgeWeights :: !(U.Vector Double)
  }

type CollectedGraphEdges :: Type
data CollectedGraphEdges = CollectedGraphEdges
  { collectedGraphEdgeCount :: !Int,
    collectedGraphEdgesAreSorted :: !Bool
  }

type VertexIndex :: Type -> Type
data VertexIndex vertex
  = AscendingVertexIndex !(Box.Vector vertex)
  | MapVertexIndex !(Map vertex Int)

diagonalCSR ::
  (Eq a, AdditiveGroup a, U.Unbox a) =>
  [a] ->
  Either MoonlightError (SparseCSR a)
diagonalCSR diagonalEntries =
  let dimension = length diagonalEntries
   in orderedCSRFromEntries
        dimension
        dimension
        (nonZeroDiagonalEntries diagonalEntries)

tridiagonalCSR ::
  (Eq a, AdditiveGroup a, U.Unbox a) =>
  [a] ->
  [a] ->
  Either MoonlightError (SparseCSR a)
tridiagonalCSR diagonalEntries offDiagonalEntries
  | actualOffDiagonalCount /= expectedOffDiagonalCount =
      Left
        ( InvariantViolation
            ( "symmetric tridiagonal CSR off-diagonal length mismatch: expected "
                <> show expectedOffDiagonalCount
                <> " but received "
                <> show actualOffDiagonalCount
            )
        )
  | otherwise =
      orderedCSRFromEntries
        dimension
        dimension
        (tridiagonalEntries diagonalEntries offDiagonalEntries)
  where
    dimension = length diagonalEntries
    expectedOffDiagonalCount = max 0 (dimension - 1)
    actualOffDiagonalCount = length offDiagonalEntries

pathLaplacianCSR :: Int -> Either MoonlightError (SparseCSR Double)
pathLaplacianCSR dimension = do
  (diagonalEntries, offDiagonalEntries) <- pathLaplacianBands dimension
  tridiagonalCSR diagonalEntries offDiagonalEntries

graphLaplacianCSR ::
  (Ord vertex, Show vertex) =>
  [vertex] ->
  [GraphEdge vertex] ->
  Either MoonlightError (SparseCSR Double)
graphLaplacianCSR vertexOrder graphEdges = do
  vertexIndices <- buildVertexIndices vertexOrder
  let dimension = length vertexOrder
  matrixValue <-
    case vertexIndices of
      AscendingVertexIndex vertices ->
        case pathGraphLaplacianCSRFromAscendingEdges vertices graphEdges of
          Just pathValue -> pure pathValue
          Nothing -> graphLaplacianCSRFromGenericEdges vertexIndices dimension graphEdges
      MapVertexIndex _ ->
        graphLaplacianCSRFromGenericEdges vertexIndices dimension graphEdges
  if U.all fieldValueValid (csrValuesVector matrixValue)
    then Right matrixValue
    else Left (InvariantViolation "graph Laplacian accumulation overflowed to a non-finite matrix entry")
{-# INLINE graphLaplacianCSR #-}

graphLaplacianCSRFromGenericEdges ::
  (Ord vertex, Show vertex) =>
  VertexIndex vertex ->
  Int ->
  [GraphEdge vertex] ->
  Either MoonlightError (SparseCSR Double)
graphLaplacianCSRFromGenericEdges vertexIndices dimension graphEdges = do
  combinedEdges <-
    indexedGraphEdgesByLeft
      vertexIndices
      dimension
      graphEdges
  pure
    ( case pathGraphLaplacianCSRFromIndexedEdges dimension combinedEdges of
        Just pathValue -> pathValue
        Nothing -> graphLaplacianCSRFromIndexedEdges dimension combinedEdges
    )

pathGraphLaplacianCSRFromAscendingEdges ::
  forall vertex.
  Ord vertex =>
  Box.Vector vertex ->
  [GraphEdge vertex] ->
  Maybe (SparseCSR Double)
pathGraphLaplacianCSRFromAscendingEdges vertices graphEdges
  | dimension <= 1 =
      if null graphEdges
        then Just (emptySquareCSR dimension)
        else Nothing
  | otherwise =
      runST $ do
        edgeWeights <- MU.replicate (dimension - 1) 0.0
        collectionResult <- collectAscendingPathEdges edgeWeights 0 graphEdges
        case collectionResult of
          Nothing -> pure Nothing
          Just () -> do
            frozenWeights <- U.unsafeFreeze edgeWeights
            pure
              ( if U.any (== 0.0) frozenWeights
                  then Nothing
                  else Just (pathGraphLaplacianCSRFromWeights dimension frozenWeights)
              )
  where
    !dimension = Box.length vertices

    collectAscendingPathEdges ::
      MU.MVector s Double ->
      Int ->
      [GraphEdge vertex] ->
      ST s (Maybe ())
    collectAscendingPathEdges _ !pathIndex []
      | pathIndex <= dimension - 1 = pure (Just ())
      | otherwise = pure Nothing
    collectAscendingPathEdges edgeWeights !pathIndex edges@(edgeValue : remainingEdges)
      | pathIndex >= dimension - 1 = pure Nothing
      | ascendingPathEdgeMatches pathIndex edgeValue =
          let !weightValue = graphEdgeWeight edgeValue
           in if not (fieldValueValid weightValue) || weightValue < 0.0
                then pure Nothing
                else
                  if weightValue == 0.0
                    then collectAscendingPathEdges edgeWeights pathIndex remainingEdges
                    else do
                      addMutableEntry edgeWeights pathIndex weightValue
                      collectAscendingPathEdges edgeWeights pathIndex remainingEdges
      | otherwise =
          collectAscendingPathEdges edgeWeights (pathIndex + 1) edges

    ascendingPathEdgeMatches :: Int -> GraphEdge vertex -> Bool
    ascendingPathEdgeMatches !pathIndex edgeValue =
      let !leftVertex = graphEdgeLeft edgeValue
          !rightVertex = graphEdgeRight edgeValue
          !expectedLeft = vertices `Box.unsafeIndex` pathIndex
          !expectedRight = vertices `Box.unsafeIndex` (pathIndex + 1)
       in (leftVertex == expectedLeft && rightVertex == expectedRight)
            || (leftVertex == expectedRight && rightVertex == expectedLeft)

nonZeroDiagonalEntries ::
  (Eq a, AdditiveGroup a) =>
  [a] ->
  [(Int, Int, a)]
nonZeroDiagonalEntries entries =
  mapMaybe
    ( \(entryIndex, entryValue) ->
        nonZeroEntry
          entryIndex
          entryIndex
          entryValue
    )
    (zip [0 ..] entries)

tridiagonalEntries ::
  (Eq a, AdditiveGroup a) =>
  [a] ->
  [a] ->
  [(Int, Int, a)]
tridiagonalEntries diagonalEntries offDiagonalEntries =
  concat
    ( zipWith
        tridiagonalRowEntries
        [0 ..]
        (zip3 lowerEntries diagonalEntries upperEntries)
    )
  where
    lowerEntries = Nothing : (Just <$> offDiagonalEntries)
    upperEntries = (Just <$> offDiagonalEntries) <> [Nothing]

tridiagonalRowEntries ::
  (Eq a, AdditiveGroup a) =>
  Int ->
  (Maybe a, a, Maybe a) ->
  [(Int, Int, a)]
tridiagonalRowEntries rowIndex (lowerValue, diagonalValue, upperValue) =
  catMaybes
    [ lowerValue >>= nonZeroEntry rowIndex (rowIndex - 1),
      nonZeroEntry rowIndex rowIndex diagonalValue,
      upperValue >>= nonZeroEntry rowIndex (rowIndex + 1)
    ]

nonZeroEntry ::
  (Eq a, AdditiveGroup a) =>
  Int ->
  Int ->
  a ->
  Maybe (Int, Int, a)
nonZeroEntry rowIndex columnIndex entryValue =
  if entryValue == zero
    then Nothing
    else Just (rowIndex, columnIndex, entryValue)

buildVertexIndices ::
  (Ord vertex, Show vertex) =>
  [vertex] ->
  Either MoonlightError (VertexIndex vertex)
buildVertexIndices vertexOrder
  | isStrictlyAscending vertexOrder =
      Right (AscendingVertexIndex (Box.fromList vertexOrder))
  | otherwise =
      MapVertexIndex <$> foldMIndexed insertVertex Map.empty vertexOrder

isStrictlyAscending :: Ord vertex => [vertex] -> Bool
isStrictlyAscending [] = True
isStrictlyAscending (vertexValue : remainingVertices) =
  go vertexValue remainingVertices
  where
    go :: Ord vertex => vertex -> [vertex] -> Bool
    go _ [] = True
    go previousVertex (currentVertex : rest)
      | previousVertex < currentVertex = go currentVertex rest
      | otherwise = False

insertVertex ::
  (Ord vertex, Show vertex) =>
  Int ->
  Map vertex Int ->
  vertex ->
  Either MoonlightError (Map vertex Int)
insertVertex vertexIndex vertexIndices vertexValue =
  case Map.lookup vertexValue vertexIndices of
    Just originalIndex ->
      Left
        ( InvariantViolation
            ( "graph Laplacian vertex order contains duplicate vertex "
                <> show vertexValue
                <> " at indices "
                <> show originalIndex
                <> " and "
                <> show vertexIndex
            )
        )
    Nothing ->
      Right (Map.insert vertexValue vertexIndex vertexIndices)

foldMIndexed ::
  Monad monadValue =>
  (Int -> state -> item -> monadValue state) ->
  state ->
  [item] ->
  monadValue state
foldMIndexed step initialState items =
  snd
    <$> foldM
      ( \(itemIndex, stateValue) itemValue ->
          (\nextState -> (itemIndex + 1, nextState))
            <$> step itemIndex stateValue itemValue
      )
      (0, initialState)
      items

canonicalGraphEdge ::
  (Ord vertex, Show vertex) =>
  VertexIndex vertex ->
  GraphEdge vertex ->
  Either MoonlightError (Maybe IndexedGraphEdge)
canonicalGraphEdge vertexIndices edgeValue
  | not (fieldValueValid weightValue) =
      Left
        ( InvariantViolation
            ( "graph Laplacian edge weight must be finite, received "
                <> show weightValue
            )
        )
  | weightValue < 0.0 =
      Left
        ( InvariantViolation
            ( "graph Laplacian edge weight must be non-negative, received "
                <> show weightValue
            )
        )
  | leftVertex == rightVertex =
      Left
        ( InvariantViolation
            ( "graph Laplacian does not admit self-loop at vertex "
                <> show leftVertex
            )
        )
  | otherwise = do
      leftIndex <- requireVertexIndex "left" leftVertex vertexIndices
      rightIndex <- requireVertexIndex "right" rightVertex vertexIndices
      if weightValue == 0.0
        then Right Nothing
        else
          Right
            ( Just
                IndexedGraphEdge
                  { indexedGraphEdgeLeft = min leftIndex rightIndex,
                    indexedGraphEdgeRight = max leftIndex rightIndex,
                    indexedGraphEdgeWeight = weightValue
                  }
            )
  where
    leftVertex = graphEdgeLeft edgeValue
    rightVertex = graphEdgeRight edgeValue
    weightValue = graphEdgeWeight edgeValue

requireVertexIndex ::
  (Ord vertex, Show vertex) =>
  String ->
  vertex ->
  VertexIndex vertex ->
  Either MoonlightError Int
requireVertexIndex endpointRole vertexValue vertexIndices =
  case lookupVertexIndex vertexValue vertexIndices of
    Nothing ->
      Left
        ( InvariantViolation
            ( "graph Laplacian "
                <> endpointRole
                <> " endpoint is absent from the explicit vertex order: "
                <> show vertexValue
            )
        )
    Just vertexIndex -> Right vertexIndex

lookupVertexIndex :: Ord vertex => vertex -> VertexIndex vertex -> Maybe Int
lookupVertexIndex vertexValue vertexIndex =
  case vertexIndex of
    AscendingVertexIndex vertices ->
      lookupAscendingVertex vertices vertexValue
    MapVertexIndex vertexIndices ->
      Map.lookup vertexValue vertexIndices

lookupAscendingVertex :: Ord vertex => Box.Vector vertex -> vertex -> Maybe Int
lookupAscendingVertex vertices vertexValue =
  go 0 (Box.length vertices - 1)
  where
    go !lowerBound !upperBound
      | lowerBound > upperBound = Nothing
      | otherwise =
          let !midpoint = lowerBound + ((upperBound - lowerBound) `div` 2)
              !midpointVertex = vertices `Box.unsafeIndex` midpoint
           in case compare vertexValue midpointVertex of
                LT -> go lowerBound (midpoint - 1)
                EQ -> Just midpoint
                GT -> go (midpoint + 1) upperBound

indexedGraphEdgesByLeft ::
  (Ord vertex, Show vertex) =>
  VertexIndex vertex ->
  Int ->
  [GraphEdge vertex] ->
  Either MoonlightError IndexedGraphEdgeRows
indexedGraphEdgesByLeft vertexIndices dimension graphEdges =
  runST $ do
    let !edgeCapacity = length graphEdges
    leftCounts <- MU.replicate dimension 0
    collectedLefts <- MU.unsafeNew edgeCapacity
    collectedRights <- MU.unsafeNew edgeCapacity
    collectedWeights <- MU.unsafeNew edgeCapacity
    collectionResult <-
      collectIndexedGraphEdges
        vertexIndices
        leftCounts
        collectedLefts
        collectedRights
        collectedWeights
        0
        (-1)
        (-1)
        True
        graphEdges
    case collectionResult of
      Left err -> pure (Left err)
      Right collectionValue
        | collectedGraphEdgesAreSorted collectionValue ->
            Right
              <$> compactSortedCollectedGraphEdges
                dimension
                collectedLefts
                collectedRights
                collectedWeights
                (collectedGraphEdgeCount collectionValue)
        | otherwise -> do
            let !collectedCount = collectedGraphEdgeCount collectionValue
            leftOffsets <- MU.replicate (dimension + 1) 0
            prefixMutableIntCountsWithStarts dimension leftCounts leftOffsets
            scatteredRights <- MU.unsafeNew collectedCount
            scatteredWeights <- MU.unsafeNew collectedCount
            scatterCollectedGraphEdges
              leftCounts
              collectedLefts
              collectedRights
              collectedWeights
              scatteredRights
              scatteredWeights
              0
              collectedCount
            rawOffsets <- U.unsafeFreeze leftOffsets
            frozenRawRights <- U.unsafeFreeze scatteredRights
            frozenRawWeights <- U.unsafeFreeze scatteredWeights
            Right <$> compactGraphEdgeRows dimension rawOffsets frozenRawRights frozenRawWeights collectedCount
{-# INLINE indexedGraphEdgesByLeft #-}

collectIndexedGraphEdges ::
  (Ord vertex, Show vertex) =>
  VertexIndex vertex ->
  MU.MVector s Int ->
  MU.MVector s Int ->
  MU.MVector s Int ->
  MU.MVector s Double ->
  Int ->
  Int ->
  Int ->
  Bool ->
  [GraphEdge vertex] ->
  ST s (Either MoonlightError CollectedGraphEdges)
collectIndexedGraphEdges _ _ _ _ _ !collectedCount _ _ !edgesAreSorted [] =
  pure
    ( Right
        CollectedGraphEdges
          { collectedGraphEdgeCount = collectedCount,
            collectedGraphEdgesAreSorted = edgesAreSorted
          }
    )
collectIndexedGraphEdges vertexIndices leftCounts collectedLefts collectedRights collectedWeights !collectedCount !previousLeft !previousRight !edgesAreSorted (edgeValue : remainingEdges) =
  case canonicalGraphEdge vertexIndices edgeValue of
    Left err -> pure (Left err)
    Right Nothing ->
      collectIndexedGraphEdges
        vertexIndices
        leftCounts
        collectedLefts
        collectedRights
        collectedWeights
        collectedCount
        previousLeft
        previousRight
        edgesAreSorted
        remainingEdges
    Right (Just indexedEdge) -> do
      let !leftIndex = indexedGraphEdgeLeft indexedEdge
          !rightIndex = indexedGraphEdgeRight indexedEdge
          !nextEdgesAreSorted =
            edgesAreSorted
              && ( previousLeft < 0
                    || previousLeft < leftIndex
                    || (previousLeft == leftIndex && previousRight <= rightIndex)
                 )
      incrementMutableInt leftCounts leftIndex
      MU.unsafeWrite collectedLefts collectedCount leftIndex
      MU.unsafeWrite collectedRights collectedCount rightIndex
      MU.unsafeWrite collectedWeights collectedCount (indexedGraphEdgeWeight indexedEdge)
      collectIndexedGraphEdges
        vertexIndices
        leftCounts
        collectedLefts
        collectedRights
        collectedWeights
        (collectedCount + 1)
        leftIndex
        rightIndex
        nextEdgesAreSorted
        remainingEdges

compactSortedCollectedGraphEdges ::
  forall s.
  Int ->
  MU.MVector s Int ->
  MU.MVector s Int ->
  MU.MVector s Double ->
  Int ->
  ST s IndexedGraphEdgeRows
compactSortedCollectedGraphEdges dimension collectedLefts collectedRights collectedWeights collectedCount = do
  compactCounts <- MU.replicate dimension 0
  compactRights <- MU.unsafeNew collectedCount
  compactWeights <- MU.unsafeNew collectedCount
  compactCount <-
    combineSortedCollectedGraphEdges
      compactCounts
      compactRights
      compactWeights
      0
      collectedCount
      0
  compactOffsets <- MU.replicate (dimension + 1) 0
  prefixMutableIntCountsWithStarts dimension compactCounts compactOffsets
  frozenOffsets <- U.unsafeFreeze compactOffsets
  frozenRights <- U.unsafeFreeze compactRights
  frozenWeights <- U.unsafeFreeze compactWeights
  pure
    IndexedGraphEdgeRows
      { indexedGraphEdgeOffsets = frozenOffsets,
        indexedGraphEdgeRights = U.slice 0 compactCount frozenRights,
        indexedGraphEdgeWeights = U.slice 0 compactCount frozenWeights
      }
  where
    combineSortedCollectedGraphEdges ::
      MU.MVector s Int ->
      MU.MVector s Int ->
      MU.MVector s Double ->
      Int ->
      Int ->
      Int ->
      ST s Int
    combineSortedCollectedGraphEdges compactCounts compactRights compactWeights !entryIndex !entryStop !compactIndex
      | entryIndex >= entryStop = pure compactIndex
      | otherwise = do
          leftIndex <- MU.unsafeRead collectedLefts entryIndex
          rightIndex <- MU.unsafeRead collectedRights entryIndex
          weightValue <- MU.unsafeRead collectedWeights entryIndex
          combineSortedCollectedGraphEdge
            compactCounts
            compactRights
            compactWeights
            leftIndex
            rightIndex
            weightValue
            (entryIndex + 1)
            entryStop
            compactIndex

    combineSortedCollectedGraphEdge ::
      MU.MVector s Int ->
      MU.MVector s Int ->
      MU.MVector s Double ->
      Int ->
      Int ->
      Double ->
      Int ->
      Int ->
      Int ->
      ST s Int
    combineSortedCollectedGraphEdge compactCounts compactRights compactWeights !leftIndex !rightIndex !weightValue !entryIndex !entryStop !compactIndex
      | entryIndex >= entryStop =
          writeSortedCollectedGraphEdge compactCounts compactRights compactWeights leftIndex rightIndex weightValue entryIndex entryStop compactIndex
      | otherwise = do
          nextLeft <- MU.unsafeRead collectedLefts entryIndex
          nextRight <- MU.unsafeRead collectedRights entryIndex
          if nextLeft == leftIndex && nextRight == rightIndex
            then do
              nextWeight <- MU.unsafeRead collectedWeights entryIndex
              combineSortedCollectedGraphEdge
                compactCounts
                compactRights
                compactWeights
                leftIndex
                rightIndex
                (nextWeight + weightValue)
                (entryIndex + 1)
                entryStop
                compactIndex
            else
              writeSortedCollectedGraphEdge compactCounts compactRights compactWeights leftIndex rightIndex weightValue entryIndex entryStop compactIndex

    writeSortedCollectedGraphEdge ::
      MU.MVector s Int ->
      MU.MVector s Int ->
      MU.MVector s Double ->
      Int ->
      Int ->
      Double ->
      Int ->
      Int ->
      Int ->
      ST s Int
    writeSortedCollectedGraphEdge compactCounts compactRights compactWeights !leftIndex !rightIndex !weightValue !nextEntryIndex !entryStop !compactIndex
      | weightValue == 0.0 =
          combineSortedCollectedGraphEdges compactCounts compactRights compactWeights nextEntryIndex entryStop compactIndex
      | otherwise = do
          incrementMutableInt compactCounts leftIndex
          MU.unsafeWrite compactRights compactIndex rightIndex
          MU.unsafeWrite compactWeights compactIndex weightValue
          combineSortedCollectedGraphEdges compactCounts compactRights compactWeights nextEntryIndex entryStop (compactIndex + 1)

scatterCollectedGraphEdges ::
  MU.MVector s Int ->
  MU.MVector s Int ->
  MU.MVector s Int ->
  MU.MVector s Double ->
  MU.MVector s Int ->
  MU.MVector s Double ->
  Int ->
  Int ->
  ST s ()
scatterCollectedGraphEdges nextOffsets collectedLefts collectedRights collectedWeights scatteredRights scatteredWeights !entryIndex !entryStop
  | entryIndex >= entryStop = pure ()
  | otherwise = do
      leftIndex <- MU.unsafeRead collectedLefts entryIndex
      targetIndex <- MU.unsafeRead nextOffsets leftIndex
      rightIndex <- MU.unsafeRead collectedRights entryIndex
      weightValue <- MU.unsafeRead collectedWeights entryIndex
      MU.unsafeWrite scatteredRights targetIndex rightIndex
      MU.unsafeWrite scatteredWeights targetIndex weightValue
      MU.unsafeWrite nextOffsets leftIndex (targetIndex + 1)
      scatterCollectedGraphEdges
        nextOffsets
        collectedLefts
        collectedRights
        collectedWeights
        scatteredRights
        scatteredWeights
        (entryIndex + 1)
        entryStop

compactGraphEdgeRows ::
  Int ->
  U.Vector Int ->
  U.Vector Int ->
  U.Vector Double ->
  Int ->
  ST s IndexedGraphEdgeRows
compactGraphEdgeRows dimension rawOffsets rawRights rawWeights edgeCount = do
  compactOffsets <- MU.replicate (dimension + 1) 0
  compactRights <- MU.unsafeNew edgeCount
  compactWeights <- MU.unsafeNew edgeCount
  finalCount <- compactLeftRows compactOffsets compactRights compactWeights 0 0
  frozenOffsets <- U.unsafeFreeze compactOffsets
  frozenRights <- U.unsafeFreeze compactRights
  frozenWeights <- U.unsafeFreeze compactWeights
  pure
    IndexedGraphEdgeRows
      { indexedGraphEdgeOffsets = frozenOffsets,
        indexedGraphEdgeRights = U.slice 0 finalCount frozenRights,
        indexedGraphEdgeWeights = U.slice 0 finalCount frozenWeights
      }
  where
    compactLeftRows ::
      MU.MVector s Int ->
      MU.MVector s Int ->
      MU.MVector s Double ->
      Int ->
      Int ->
      ST s Int
    compactLeftRows compactOffsets compactRights compactWeights !leftIndex !compactCount
      | leftIndex >= dimension = do
          MU.unsafeWrite compactOffsets dimension compactCount
          pure compactCount
      | otherwise = do
          MU.unsafeWrite compactOffsets leftIndex compactCount
          let !entryStart = rawOffsets `U.unsafeIndex` leftIndex
              !entryStop = rawOffsets `U.unsafeIndex` (leftIndex + 1)
              !orderedPairs =
                sortBy
                  (comparing (\(rightIndex, weightValue) -> (rightIndex, weightValue)))
                  (collectGraphEdgePairs entryStart entryStop [])
          compactStop <- writeCombinedGraphPairs compactRights compactWeights compactCount orderedPairs
          compactLeftRows compactOffsets compactRights compactWeights (leftIndex + 1) compactStop

    collectGraphEdgePairs :: Int -> Int -> [(Int, Double)] -> [(Int, Double)]
    collectGraphEdgePairs !entryIndex !entryStop rowPairs
      | entryIndex >= entryStop = rowPairs
      | otherwise =
          collectGraphEdgePairs
            (entryIndex + 1)
            entryStop
            ( ( rawRights `U.unsafeIndex` entryIndex,
                rawWeights `U.unsafeIndex` entryIndex
              )
                : rowPairs
            )

    writeCombinedGraphPairs ::
      MU.MVector s Int ->
      MU.MVector s Double ->
      Int ->
      [(Int, Double)] ->
      ST s Int
    writeCombinedGraphPairs _ _ !compactIndex [] =
      pure compactIndex
    writeCombinedGraphPairs compactRights compactWeights !compactIndex ((rightIndex, weightValue) : rowPairs) =
      writeCombinedGraphPair compactRights compactWeights compactIndex rightIndex weightValue rowPairs

    writeCombinedGraphPair ::
      MU.MVector s Int ->
      MU.MVector s Double ->
      Int ->
      Int ->
      Double ->
      [(Int, Double)] ->
      ST s Int
    writeCombinedGraphPair compactRights compactWeights !compactIndex !rightIndex !weightValue [] =
      writeNonZeroGraphPair compactRights compactWeights compactIndex rightIndex weightValue []
    writeCombinedGraphPair compactRights compactWeights !compactIndex !rightIndex !weightValue ((nextRight, nextWeight) : rowPairs)
      | nextRight == rightIndex =
          writeCombinedGraphPair compactRights compactWeights compactIndex rightIndex (nextWeight + weightValue) rowPairs
      | otherwise =
          writeNonZeroGraphPair compactRights compactWeights compactIndex rightIndex weightValue ((nextRight, nextWeight) : rowPairs)

    writeNonZeroGraphPair ::
      MU.MVector s Int ->
      MU.MVector s Double ->
      Int ->
      Int ->
      Double ->
      [(Int, Double)] ->
      ST s Int
    writeNonZeroGraphPair compactRights compactWeights !compactIndex !rightIndex !weightValue rowPairs
      | weightValue == 0.0 =
          writeCombinedGraphPairs compactRights compactWeights compactIndex rowPairs
      | otherwise = do
          MU.unsafeWrite compactRights compactIndex rightIndex
          MU.unsafeWrite compactWeights compactIndex weightValue
          writeCombinedGraphPairs compactRights compactWeights (compactIndex + 1) rowPairs

graphLaplacianCSRFromIndexedEdges :: Int -> IndexedGraphEdgeRows -> SparseCSR Double
graphLaplacianCSRFromIndexedEdges dimension indexedEdges =
  runST $ do
    lowerCounts <- MU.replicate dimension 0
    upperCounts <- MU.replicate dimension 0
    degrees <- MU.replicate dimension 0.0
    accumulateGraphEdgeRows lowerCounts upperCounts degrees indexedEdges 0
    rowOffsets <- MU.replicate (dimension + 1) 0
    prefixGraphRowOffsets lowerCounts upperCounts degrees rowOffsets 0 0
    finalCount <- MU.unsafeRead rowOffsets dimension
    columnIndices <- MU.unsafeNew finalCount
    values <- MU.unsafeNew finalCount
    lowerNext <- MU.unsafeNew dimension
    upperNext <- MU.unsafeNew dimension
    initializeGraphRows lowerCounts degrees rowOffsets lowerNext upperNext columnIndices values 0
    writeGraphEdgeRows lowerNext upperNext columnIndices values indexedEdges 0
    frozenOffsets <- U.unsafeFreeze rowOffsets
    frozenColumns <- U.unsafeFreeze columnIndices
    frozenValues <- U.unsafeFreeze values
    pure
      ( csrFromCanonicalVectorsUnchecked
          dimension
          dimension
          frozenOffsets
          frozenColumns
          frozenValues
      )
{-# INLINE graphLaplacianCSRFromIndexedEdges #-}

pathGraphLaplacianCSRFromIndexedEdges :: Int -> IndexedGraphEdgeRows -> Maybe (SparseCSR Double)
pathGraphLaplacianCSRFromIndexedEdges dimension edgeRows
  | dimension <= 1 =
      if U.null edgeWeights
        then Just (emptySquareCSR dimension)
        else Nothing
  | U.length edgeWeights /= dimension - 1 =
      Nothing
  | pathEdgesMatch 0 =
      Just (pathGraphLaplacianCSRFromWeights dimension edgeWeights)
  | otherwise =
      Nothing
  where
    !edgeOffsets = indexedGraphEdgeOffsets edgeRows
    !edgeRights = indexedGraphEdgeRights edgeRows
    !edgeWeights = indexedGraphEdgeWeights edgeRows

    pathEdgesMatch !leftIndex
      | leftIndex >= dimension - 1 =
          edgeOffsets `U.unsafeIndex` dimension == U.length edgeWeights
      | otherwise =
          let !entryStart = edgeOffsets `U.unsafeIndex` leftIndex
              !entryStop = edgeOffsets `U.unsafeIndex` (leftIndex + 1)
           in entryStop - entryStart == 1
                && edgeRights `U.unsafeIndex` entryStart == leftIndex + 1
                && pathEdgesMatch (leftIndex + 1)

emptySquareCSR :: Int -> SparseCSR Double
emptySquareCSR dimension =
  csrFromCanonicalVectorsWithPlanUnchecked
    dimension
    dimension
    (U.replicate (dimension + 1) 0)
    U.empty
    U.empty
    CSRGeneral

pathGraphLaplacianCSRFromWeights :: Int -> U.Vector Double -> SparseCSR Double
pathGraphLaplacianCSRFromWeights dimension edgeWeights =
  runST $ do
    let !entryCount = 3 * dimension - 2
    rowOffsets <- MU.unsafeNew (dimension + 1)
    columnIndices <- MU.unsafeNew entryCount
    values <- MU.unsafeNew entryCount
    writePathGraphRows rowOffsets columnIndices values 0 0
    frozenOffsets <- U.unsafeFreeze rowOffsets
    frozenColumns <- U.unsafeFreeze columnIndices
    frozenValues <- U.unsafeFreeze values
    pure
      ( csrFromCanonicalVectorsWithPlanUnchecked
          dimension
          dimension
          frozenOffsets
          frozenColumns
          frozenValues
          (CSRContiguousBand 1 1)
      )
  where
    writePathGraphRows ::
      MU.MVector s Int ->
      MU.MVector s Int ->
      MU.MVector s Double ->
      Int ->
      Int ->
      ST s ()
    writePathGraphRows rowOffsets columnIndices values !rowIndex !entryIndex
      | rowIndex >= dimension = do
          MU.unsafeWrite rowOffsets dimension entryIndex
      | rowIndex == 0 = do
          let !rightWeight = edgeWeights `U.unsafeIndex` 0
          MU.unsafeWrite rowOffsets rowIndex entryIndex
          MU.unsafeWrite columnIndices entryIndex rowIndex
          MU.unsafeWrite values entryIndex rightWeight
          MU.unsafeWrite columnIndices (entryIndex + 1) (rowIndex + 1)
          MU.unsafeWrite values (entryIndex + 1) (negate rightWeight)
          writePathGraphRows rowOffsets columnIndices values (rowIndex + 1) (entryIndex + 2)
      | rowIndex == dimension - 1 = do
          let !leftWeight = edgeWeights `U.unsafeIndex` (rowIndex - 1)
          MU.unsafeWrite rowOffsets rowIndex entryIndex
          MU.unsafeWrite columnIndices entryIndex (rowIndex - 1)
          MU.unsafeWrite values entryIndex (negate leftWeight)
          MU.unsafeWrite columnIndices (entryIndex + 1) rowIndex
          MU.unsafeWrite values (entryIndex + 1) leftWeight
          writePathGraphRows rowOffsets columnIndices values (rowIndex + 1) (entryIndex + 2)
      | otherwise = do
          let !leftWeight = edgeWeights `U.unsafeIndex` (rowIndex - 1)
              !rightWeight = edgeWeights `U.unsafeIndex` rowIndex
          MU.unsafeWrite rowOffsets rowIndex entryIndex
          MU.unsafeWrite columnIndices entryIndex (rowIndex - 1)
          MU.unsafeWrite values entryIndex (negate leftWeight)
          MU.unsafeWrite columnIndices (entryIndex + 1) rowIndex
          MU.unsafeWrite values (entryIndex + 1) (leftWeight + rightWeight)
          MU.unsafeWrite columnIndices (entryIndex + 2) (rowIndex + 1)
          MU.unsafeWrite values (entryIndex + 2) (negate rightWeight)
          writePathGraphRows rowOffsets columnIndices values (rowIndex + 1) (entryIndex + 3)

accumulateGraphEdgeRows ::
  MU.MVector s Int ->
  MU.MVector s Int ->
  MU.MVector s Double ->
  IndexedGraphEdgeRows ->
  Int ->
  ST s ()
accumulateGraphEdgeRows lowerCounts upperCounts degrees edgeRows !leftIndex
  | leftIndex >= U.length edgeOffsets - 1 = pure ()
  | otherwise = do
      let !entryStart = edgeOffsets `U.unsafeIndex` leftIndex
          !entryStop = edgeOffsets `U.unsafeIndex` (leftIndex + 1)
      accumulateGraphEdgeSpan lowerCounts upperCounts degrees leftIndex entryStart entryStop
      accumulateGraphEdgeRows lowerCounts upperCounts degrees edgeRows (leftIndex + 1)
  where
    !edgeOffsets = indexedGraphEdgeOffsets edgeRows
    !edgeRights = indexedGraphEdgeRights edgeRows
    !edgeWeights = indexedGraphEdgeWeights edgeRows

    accumulateGraphEdgeSpan ::
      MU.MVector s Int ->
      MU.MVector s Int ->
      MU.MVector s Double ->
      Int ->
      Int ->
      Int ->
      ST s ()
    accumulateGraphEdgeSpan lowerCountSlots upperCountSlots degreeSlots !rowIndex !entryIndex !entryStop
      | entryIndex >= entryStop = pure ()
      | otherwise = do
          let !rightIndex = edgeRights `U.unsafeIndex` entryIndex
              !weightValue = edgeWeights `U.unsafeIndex` entryIndex
          incrementMutableInt upperCountSlots rowIndex
          incrementMutableInt lowerCountSlots rightIndex
          addMutableEntry degreeSlots rowIndex weightValue
          addMutableEntry degreeSlots rightIndex weightValue
          accumulateGraphEdgeSpan lowerCountSlots upperCountSlots degreeSlots rowIndex (entryIndex + 1) entryStop

incrementMutableInt :: MU.MVector s Int -> Int -> ST s ()
incrementMutableInt values !entryIndex = do
  currentValue <- MU.unsafeRead values entryIndex
  MU.unsafeWrite values entryIndex (currentValue + 1)
{-# INLINE incrementMutableInt #-}

prefixMutableIntCountsWithStarts ::
  Int ->
  MU.MVector s Int ->
  MU.MVector s Int ->
  ST s ()
prefixMutableIntCountsWithStarts axisCount counts offsets =
  go 0 0
  where
    go !axisIndex !runningTotal
      | axisIndex >= axisCount =
          MU.unsafeWrite offsets axisCount runningTotal
      | otherwise = do
          axisCountValue <- MU.unsafeRead counts axisIndex
          MU.unsafeWrite offsets axisIndex runningTotal
          MU.unsafeWrite counts axisIndex runningTotal
          go (axisIndex + 1) (runningTotal + axisCountValue)

prefixGraphRowOffsets ::
  MU.MVector s Int ->
  MU.MVector s Int ->
  MU.MVector s Double ->
  MU.MVector s Int ->
  Int ->
  Int ->
  ST s ()
prefixGraphRowOffsets lowerCounts upperCounts degrees rowOffsets !rowIndex !runningTotal
  | rowIndex >= MU.length degrees =
      MU.unsafeWrite rowOffsets rowIndex runningTotal
  | otherwise = do
      lowerCount <- MU.unsafeRead lowerCounts rowIndex
      upperCount <- MU.unsafeRead upperCounts rowIndex
      degreeValue <- MU.unsafeRead degrees rowIndex
      MU.unsafeWrite rowOffsets rowIndex runningTotal
      let !diagonalCount =
            if degreeValue == 0.0
              then 0
              else 1
      prefixGraphRowOffsets
        lowerCounts
        upperCounts
        degrees
        rowOffsets
        (rowIndex + 1)
        (runningTotal + lowerCount + diagonalCount + upperCount)

initializeGraphRows ::
  MU.MVector s Int ->
  MU.MVector s Double ->
  MU.MVector s Int ->
  MU.MVector s Int ->
  MU.MVector s Int ->
  MU.MVector s Int ->
  MU.MVector s Double ->
  Int ->
  ST s ()
initializeGraphRows lowerCounts degrees rowOffsets lowerNext upperNext columnIndices values !rowIndex
  | rowIndex >= MU.length degrees = pure ()
  | otherwise = do
      rowStart <- MU.unsafeRead rowOffsets rowIndex
      lowerCount <- MU.unsafeRead lowerCounts rowIndex
      degreeValue <- MU.unsafeRead degrees rowIndex
      let !diagonalIndex = rowStart + lowerCount
          !upperStart =
            if degreeValue == 0.0
              then diagonalIndex
              else diagonalIndex + 1
      MU.unsafeWrite lowerNext rowIndex rowStart
      MU.unsafeWrite upperNext rowIndex upperStart
      if degreeValue == 0.0
        then pure ()
        else do
          MU.unsafeWrite columnIndices diagonalIndex rowIndex
          MU.unsafeWrite values diagonalIndex degreeValue
      initializeGraphRows
        lowerCounts
        degrees
        rowOffsets
        lowerNext
        upperNext
        columnIndices
        values
        (rowIndex + 1)

writeGraphEdgeRows ::
  MU.MVector s Int ->
  MU.MVector s Int ->
  MU.MVector s Int ->
  MU.MVector s Double ->
  IndexedGraphEdgeRows ->
  Int ->
  ST s ()
writeGraphEdgeRows lowerNext upperNext columnIndices values edgeRows !leftIndex
  | leftIndex >= U.length edgeOffsets - 1 = pure ()
  | otherwise = do
      let !entryStart = edgeOffsets `U.unsafeIndex` leftIndex
          !entryStop = edgeOffsets `U.unsafeIndex` (leftIndex + 1)
      writeGraphEdgeSpan lowerNext upperNext columnIndices values leftIndex entryStart entryStop
      writeGraphEdgeRows lowerNext upperNext columnIndices values edgeRows (leftIndex + 1)
  where
    !edgeOffsets = indexedGraphEdgeOffsets edgeRows
    !edgeRights = indexedGraphEdgeRights edgeRows
    !edgeWeights = indexedGraphEdgeWeights edgeRows

    writeGraphEdgeSpan ::
      MU.MVector s Int ->
      MU.MVector s Int ->
      MU.MVector s Int ->
      MU.MVector s Double ->
      Int ->
      Int ->
      Int ->
      ST s ()
    writeGraphEdgeSpan lowerSlots upperSlots columnSlots valueSlots !rowIndex !entryIndex !entryStop
      | entryIndex >= entryStop = pure ()
      | otherwise = do
          let !rightIndex = edgeRights `U.unsafeIndex` entryIndex
              !weightValue = edgeWeights `U.unsafeIndex` entryIndex
          upperIndex <- MU.unsafeRead upperSlots rowIndex
          MU.unsafeWrite columnSlots upperIndex rightIndex
          MU.unsafeWrite valueSlots upperIndex (negate weightValue)
          MU.unsafeWrite upperSlots rowIndex (upperIndex + 1)
          lowerIndex <- MU.unsafeRead lowerSlots rightIndex
          MU.unsafeWrite columnSlots lowerIndex rowIndex
          MU.unsafeWrite valueSlots lowerIndex (negate weightValue)
          MU.unsafeWrite lowerSlots rightIndex (lowerIndex + 1)
          writeGraphEdgeSpan lowerSlots upperSlots columnSlots valueSlots rowIndex (entryIndex + 1) entryStop
