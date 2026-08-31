module Moonlight.Homology.Pure.Topology.Core
  ( BasisCellRef (..),
    FiltrationValue (..),
    FilteredFiniteChainComplex (..),
    CriticalKind (..),
    GraphEdge (..),
    Graph1Skeleton (..),
    graphAdjacency,
    GraphSpectralMode (..),
    cellCountAtDegree,
    dimensionsOf,
    allBasisCellRefs,
    rowsRespectWidth,
    identityMatrix,
    zeroMatrix,
    chunkColumns,
    takeRows,
    dropRows,
    dropColumns,
    allZeroMatrix,
    transposeMatrix,
    matrixColumnCount,
    alternatingSignedSum,
    enumerateFromZero,
    toRationalFromIntegral,
    mapMaybeWithLookup,
  )
where

import Data.Kind (Type)
import Data.Function ((&))
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Ratio ((%))
import qualified Data.Set as Set
import Moonlight.Homology.Boundary.Finite (FiniteChainComplex)
import Moonlight.Homology.Pure.Carrier (BasisCellRef (..))
import Moonlight.Homology.Pure.Filtration
  ( CriticalKind (..),
    FiltrationValue (..),
    enumerateFromZero,
  )
import Moonlight.Homology.Pure.Matrix.Shape (cellCountAtDegree, dimensionsOf)


-- | A finite chain complex indexed by an arbitrary ordered filtration key.
-- The key is deliberately independent of the chain coefficient: exact
-- geometric filtrations must not be rounded through 'FiltrationValue' merely
-- to reach the persistence reducer.
type FilteredFiniteChainComplex :: Type -> Type -> Type
data FilteredFiniteChainComplex filtration r = FilteredFiniteChainComplex
  { filteredBaseComplex :: FiniteChainComplex r,
    filteredCellBirths :: Map.Map BasisCellRef filtration,
    -- | Exact critical-value equivalence classes in ascending order.
    filteredCriticalValues :: ![filtration],
    -- | Exact critical value to its dense, complex-relative rank.
    filteredBirthValueRanks :: !(Map.Map filtration Int),
    -- | Dense ranks local to this filtered complex. They are a derived
    -- reduction index, never a semantic replacement for the exact birth.
    filteredCellBirthRanks :: Map.Map BasisCellRef Int,
    -- | Cells in the canonical @(birth rank, degree, basis index)@ reduction
    -- order. Construction derives this once from the exact birth quotient;
    -- reducers must not rediscover it through map lookups and a second sort.
    filteredRankedCells :: ![(BasisCellRef, filtration, Int)]
  }

type GraphEdge :: Type
data GraphEdge = GraphEdge
  { graphEdgeIndex :: Int,
    graphEdgeSource :: Int,
    graphEdgeTarget :: Int
  }
  deriving stock (Eq, Ord, Show)

type Graph1Skeleton :: Type
data Graph1Skeleton = Graph1Skeleton
  { graphVertexCount :: Int,
    graphEdges :: [GraphEdge],
    graphEdgeAdjacency :: Map.Map Int [GraphEdge]
  }
  deriving stock (Eq, Show)

graphAdjacency :: Graph1Skeleton -> Map.Map Int (Set.Set Int)
graphAdjacency skeleton =
  enumerateFromZero (graphVertexCount skeleton)
    & fmap
      ( \vertexValue ->
          ( vertexValue,
            graphEdgeAdjacency skeleton
              & Map.findWithDefault [] vertexValue
              & mapMaybe (oppositeGraphEdgeVertex vertexValue)
              & Set.fromList
          )
      )
    & Map.fromList

oppositeGraphEdgeVertex :: Int -> GraphEdge -> Maybe Int
oppositeGraphEdgeVertex vertexValue edgeValue =
  if graphEdgeSource edgeValue == vertexValue
    then Just (graphEdgeTarget edgeValue)
    else
      if graphEdgeTarget edgeValue == vertexValue
        then Just (graphEdgeSource edgeValue)
        else Nothing

type GraphSpectralMode :: Type
data GraphSpectralMode = GraphSpectralMode
  { spectralEigenvalue :: Double,
    spectralCoefficients :: [(Int, Double)],
    spectralPositiveSupport :: [Int],
    spectralNegativeSupport :: [Int],
    spectralSupportCriticality :: Double
  }
  deriving stock (Eq, Show)


allBasisCellRefs :: FiniteChainComplex r -> [BasisCellRef]
allBasisCellRefs finite =
  dimensionsOf finite
    >>= ( \degreeValue ->
            enumerateFromZero (cellCountAtDegree finite degreeValue)
              & fmap
                ( \cellIndexValue ->
                    BasisCellRef
                      { cellDegree = degreeValue,
                        cellIndex = cellIndexValue
                      }
                )
       )

rowsRespectWidth :: Int -> [[a]] -> Bool
rowsRespectWidth expectedWidth =
  all ((== expectedWidth) . length)

identityMatrix :: Num a => Int -> [[a]]
identityMatrix matrixSize =
  enumerateFromZero matrixSize
    & fmap
      ( \rowIndexValue ->
          enumerateFromZero matrixSize
            & fmap (\columnIndexValue -> if rowIndexValue == columnIndexValue then 1 else 0)
      )

zeroMatrix :: Num a => Int -> Int -> [[a]]
zeroMatrix rowCount columnCount =
  enumerateFromZero rowCount
    & fmap (\_ -> enumerateFromZero columnCount & fmap (const 0))

chunkColumns :: Int -> [a] -> [[a]]
chunkColumns columnCount values =
  if columnCount <= 0
    then []
    else
      case splitAt columnCount values of
        ([], _) -> []
        (rowValues, []) -> [rowValues]
        (rowValues, remainderValues) -> rowValues : chunkColumns columnCount remainderValues

takeRows :: Int -> [[a]] -> [[a]]
takeRows = take

dropRows :: Int -> [[a]] -> [[a]]
dropRows = drop

dropColumns :: Int -> [[a]] -> [[a]]
dropColumns columnCount = fmap (drop columnCount)

allZeroMatrix :: (Eq a, Num a) => [[a]] -> Bool
allZeroMatrix =
  all (all (== 0))

transposeMatrix :: [[a]] -> [[a]]
transposeMatrix matrixRows =
  case matrixRows of
    [] -> []
    ([] : _) -> []
    _ -> List.transpose matrixRows

matrixColumnCount :: [[a]] -> Int
matrixColumnCount matrixRows =
  case matrixRows of
    rowValue : _ -> length rowValue
    [] -> 0

alternatingSignedSum :: [Int] -> Int
alternatingSignedSum values =
  values
    & zip [0 :: Int ..]
    & foldl'
      ( \accumulator (indexValue, countValue) ->
          if even indexValue
            then accumulator + countValue
            else accumulator - countValue
      )
      0


toRationalFromIntegral :: Integral r => r -> Rational
toRationalFromIntegral coefficientValue = fromIntegral coefficientValue % 1

mapMaybeWithLookup :: Ord key => Map.Map key value -> [key] -> [value]
mapMaybeWithLookup valueMap = mapMaybe (flip Map.lookup valueMap)
