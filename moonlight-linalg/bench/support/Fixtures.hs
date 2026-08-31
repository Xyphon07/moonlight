{-# LANGUAGE DataKinds #-}

module Fixtures
  ( benchmarkSeedBlock,
    bandedDenseRows,
    bandedSpdCSR,
    denseBenchmarkRows,
    denseBenchmarkVector,
    denseOperator,
    denseSpdRows,
    diagonalBenchmarkValues,
    genericBenchmarkTridiagonal,
    gf2BenchmarkValues,
    packedSparseBenchmarkOperator,
    pathLaplacianTridiagonal,
    projectedBenchmarkDimension,
    projectedBenchmarkRows,
    projectedBlockBenchmarkCases,
    reducibleBenchmarkTridiagonal,
    sparseKrylovBenchmarkCases,
    staticsBenchmarkNetwork,
  )
where

import Types
  ( ProjectedBlockBenchmarkCase (..),
    SparseKrylovBenchmarkCase (..),
    SpectrumProfile (..),
  )
import Env (BenchmarkSelection (..))
import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import qualified Data.Vector as Box
import qualified Data.Vector.Unboxed as U
import Moonlight.LinAlg.Dense.GF2 (GF2 (..))
import Moonlight.LinAlg.Operator
  ( LinearOperator,
    OperatorSymmetry (..),
    declaredSelfAdjointVectorLinearOperator,
  )
import Moonlight.LinAlg.Pure.Structured.Tridiagonal
  ( SymmetricTridiagonal,
    mkSymmetricTridiagonal,
    pathLaplacianBands,
  )
import Moonlight.LinAlg.Sparse
  ( PackedSparseOperator,
    PackedSparseEntry,
    SparseCSR,
    canonicalCSRFromEntries,
    mkPackedSparseOperator,
    packedSparseEntry,
  )
import Moonlight.LinAlg.Statics
  ( ForceNetwork,
    NetworkDeclaration,
    Vec3 (..),
    load,
    member,
    network,
    support,
  )
import Prelude

sparseKrylovBenchmarkCases :: BenchmarkSelection -> [SparseKrylovBenchmarkCase]
sparseKrylovBenchmarkCases benchmarkSelection =
  [SparseKrylovBenchmarkCase "path-laplacian-10k" 10000 4]
    <> [SparseKrylovBenchmarkCase "path-laplacian-50k" 50000 4 | includeSparseLarge benchmarkSelection || includeSparse100k benchmarkSelection]
    <> [SparseKrylovBenchmarkCase "path-laplacian-100k" 100000 4 | includeSparse100k benchmarkSelection]

projectedBlockBenchmarkCases :: BenchmarkSelection -> [ProjectedBlockBenchmarkCase]
projectedBlockBenchmarkCases benchmarkSelection =
  [ ProjectedBlockBenchmarkCase "block-clustered-24" 4 6 6 4 ClusteredSpectrum,
    ProjectedBlockBenchmarkCase "block-separated-24" 4 6 6 4 SeparatedSpectrum
  ]
    <> if includeProjectedMedium benchmarkSelection || includeProjectedLarge benchmarkSelection
      then
        [ ProjectedBlockBenchmarkCase "block-clustered-144" 24 6 12 6 ClusteredSpectrum,
          ProjectedBlockBenchmarkCase "block-separated-144" 24 6 12 6 SeparatedSpectrum
        ]
      else []
    <> if includeProjectedLarge benchmarkSelection
      then
        [ ProjectedBlockBenchmarkCase "block-clustered-256" 32 8 16 8 ClusteredSpectrum,
          ProjectedBlockBenchmarkCase "block-separated-256" 32 8 16 8 SeparatedSpectrum
        ]
      else []

projectedBenchmarkDimension :: ProjectedBlockBenchmarkCase -> Int
projectedBenchmarkDimension benchmarkCase =
  projectedBenchmarkBlockCount benchmarkCase * projectedBenchmarkBlockSize benchmarkCase

benchmarkSeedBlock :: Int -> Int -> Box.Vector (U.Vector Double)
benchmarkSeedBlock dimension blockSize =
  Box.fromList
    [ U.fromList [seedEntry rowIndex columnIndex | rowIndex <- [0 .. dimension - 1]]
      | columnIndex <- [0 .. blockSize - 1]
    ]

seedEntry :: Int -> Int -> Double
seedEntry rowIndex columnIndex =
  let rowOffset = fromIntegral (rowIndex + 1)
      columnOffset = fromIntegral (columnIndex + 1)
      diagonalContribution = if rowIndex == columnIndex then 1.0 else 0.0
      smoothContribution = 1.0 / (rowOffset + columnOffset)
   in diagonalContribution + smoothContribution

denseOperator :: [[Double]] -> Either String (LinearOperator 'SelfAdjointOperator)
denseOperator rows =
  let rowVectors = Box.fromList (U.fromList <$> rows)
      rowCount = Box.length rowVectors
   in validateDenseOperatorRows rowCount rowVectors
        *> first show (declaredSelfAdjointVectorLinearOperator rowCount (denseOperatorApply rowVectors))

validateDenseOperatorRows :: Int -> Box.Vector (U.Vector Double) -> Either String ()
validateDenseOperatorRows dimension rowVectors =
  traverse_ validateRow rowVectors
  where
    validateRow rowVector
      | U.length rowVector /= dimension =
          Left "benchmark dense operator rows must form a square matrix"
      | U.any (not . isFiniteDouble) rowVector =
          Left "benchmark dense operator rows must contain finite entries"
      | otherwise = Right ()

denseOperatorApply :: Box.Vector (U.Vector Double) -> U.Vector Double -> Either errorValue (U.Vector Double)
denseOperatorApply rowVectors inputVector =
  Right
    ( U.generate
        (Box.length rowVectors)
        (\rowIndex -> dotDenseRow inputVector (rowVectors `Box.unsafeIndex` rowIndex))
    )

dotDenseRow :: U.Vector Double -> U.Vector Double -> Double
dotDenseRow inputVector rowVector =
  U.ifoldl'
    (\accumulator columnIndex rowEntry -> accumulator + rowEntry * (inputVector `U.unsafeIndex` columnIndex))
    0.0
    rowVector

isFiniteDouble :: Double -> Bool
isFiniteDouble value =
  not (isNaN value || isInfinite value)

projectedBenchmarkRows :: ProjectedBlockBenchmarkCase -> [[Double]]
projectedBenchmarkRows benchmarkCase =
  let dimension = projectedBenchmarkDimension benchmarkCase
      blockSize = projectedBenchmarkBlockSize benchmarkCase
      spectrumProfile = projectedBenchmarkSpectrumProfile benchmarkCase
   in [ [ projectedBenchmarkEntry spectrumProfile blockSize rowIndex columnIndex
          | columnIndex <- [0 .. dimension - 1]
        ]
        | rowIndex <- [0 .. dimension - 1]
      ]

projectedBenchmarkEntry :: SpectrumProfile -> Int -> Int -> Int -> Double
projectedBenchmarkEntry spectrumProfile blockSize rowIndex columnIndex =
  let (rowBlock, rowWithinBlock) = rowIndex `divMod` blockSize
      (columnBlock, columnWithinBlock) = columnIndex `divMod` blockSize
   in case compare rowBlock columnBlock of
        EQ -> diagonalBlockEntry spectrumProfile rowBlock rowWithinBlock columnWithinBlock
        LT ->
          if rowBlock + 1 == columnBlock
            then offDiagonalBlockEntry spectrumProfile rowBlock rowWithinBlock columnWithinBlock
            else 0.0
        GT ->
          if columnBlock + 1 == rowBlock
            then offDiagonalBlockEntry spectrumProfile columnBlock columnWithinBlock rowWithinBlock
            else 0.0

diagonalBlockEntry :: SpectrumProfile -> Int -> Int -> Int -> Double
diagonalBlockEntry spectrumProfile blockIndex rowWithinBlock columnWithinBlock =
  let separationBase =
        case spectrumProfile of
          ClusteredSpectrum -> 12.0 + 0.005 * fromIntegral blockIndex
          SeparatedSpectrum -> 2.0 + 1.5 * fromIntegral blockIndex
      localOffset = 0.02 * fromIntegral (rowWithinBlock + columnWithinBlock)
      entryWeight =
        if rowWithinBlock == columnWithinBlock
          then separationBase + 1.0 + localOffset
          else 0.04 / fromIntegral (1 + abs (rowWithinBlock - columnWithinBlock))
   in entryWeight

offDiagonalBlockEntry :: SpectrumProfile -> Int -> Int -> Int -> Double
offDiagonalBlockEntry spectrumProfile blockIndex rowWithinBlock columnWithinBlock =
  let couplingBase =
        case spectrumProfile of
          ClusteredSpectrum -> 0.06 + 0.002 * fromIntegral (blockIndex `mod` 3)
          SeparatedSpectrum -> 0.03 + 0.001 * fromIntegral (blockIndex `mod` 3)
   in couplingBase / fromIntegral (1 + abs (rowWithinBlock - columnWithinBlock))

pathLaplacianTridiagonal :: Int -> Either String SymmetricTridiagonal
pathLaplacianTridiagonal dimension =
  first show (pathLaplacianBands dimension >>= uncurry mkSymmetricTridiagonal)

genericBenchmarkTridiagonal :: Int -> Either String SymmetricTridiagonal
genericBenchmarkTridiagonal dimension =
  first show
    ( mkSymmetricTridiagonal
        (genericTridiagonalDiagonalEntry <$> [0 .. dimension - 1])
        (genericTridiagonalOffDiagonalEntry <$> [0 .. dimension - 2])
    )

genericTridiagonalDiagonalEntry :: Int -> Double
genericTridiagonalDiagonalEntry indexValue =
  2.0 + fromIntegral (indexValue `mod` 17) / 17.0

genericTridiagonalOffDiagonalEntry :: Int -> Double
genericTridiagonalOffDiagonalEntry indexValue =
  -0.35 - 0.01 * fromIntegral (indexValue `mod` 5)

reducibleBenchmarkTridiagonal :: Int -> Either String SymmetricTridiagonal
reducibleBenchmarkTridiagonal dimension =
  first show
    ( mkSymmetricTridiagonal
        (genericTridiagonalDiagonalEntry <$> [0 .. dimension - 1])
        (reducibleTridiagonalOffDiagonalEntry <$> [0 .. dimension - 2])
    )

reducibleTridiagonalOffDiagonalEntry :: Int -> Double
reducibleTridiagonalOffDiagonalEntry indexValue =
  if (indexValue + 1) `mod` 32 == 0
    then 0.0
    else genericTridiagonalOffDiagonalEntry indexValue

denseBenchmarkRows :: Int -> [[Double]]
denseBenchmarkRows dimension =
  [ [denseBenchmarkEntry rowIndex columnIndex | columnIndex <- [0 .. dimension - 1]]
    | rowIndex <- [0 .. dimension - 1]
  ]

denseBenchmarkEntry :: Int -> Int -> Double
denseBenchmarkEntry rowIndex columnIndex =
  let rowWeight = fromIntegral (rowIndex + 1)
      columnWeight = fromIntegral (columnIndex + 1)
      diagonalContribution =
        if rowIndex == columnIndex
          then 2.0 + 0.01 * rowWeight
          else 0.0
      smoothContribution = 1.0 / (rowWeight + 2.0 * columnWeight + 3.0)
   in diagonalContribution + smoothContribution

denseSpdRows :: Int -> [[Double]]
denseSpdRows dimension =
  [ [denseSpdEntry dimension rowIndex columnIndex | columnIndex <- [0 .. dimension - 1]]
    | rowIndex <- [0 .. dimension - 1]
  ]

denseSpdEntry :: Int -> Int -> Int -> Double
denseSpdEntry dimension rowIndex columnIndex =
  if rowIndex == columnIndex
    then fromIntegral dimension + 2.0 + 0.05 * fromIntegral rowIndex
    else 1.0 / fromIntegral (2 + abs (rowIndex - columnIndex))

denseBenchmarkVector :: Int -> [Double]
denseBenchmarkVector dimension =
  fmap (\indexValue -> 1.0 + fromIntegral (indexValue `mod` 7) / 7.0) [0 .. dimension - 1]

bandedDenseRows :: Int -> [[Double]]
bandedDenseRows dimension =
  [ [bandedDenseEntry dimension rowIndex columnIndex | columnIndex <- [0 .. dimension - 1]]
    | rowIndex <- [0 .. dimension - 1]
  ]

bandedDenseEntry :: Int -> Int -> Int -> Double
bandedDenseEntry dimension rowIndex columnIndex
  | rowIndex == columnIndex = 4.0 + 0.001 * fromIntegral dimension
  | abs (rowIndex - columnIndex) == 1 = -1.0
  | abs (rowIndex - columnIndex) == 2 = 0.25
  | otherwise = 0.0

bandedSpdCSR :: Int -> Either String (SparseCSR Double)
bandedSpdCSR dimension =
  case
    canonicalCSRFromEntries
      dimension
      dimension
      (bandedSpdEntries dimension)
    of
    Left err -> Left (show err)
    Right csrValue -> Right csrValue

bandedSpdEntries :: Int -> [(Int, Int, Double)]
bandedSpdEntries dimension =
  concatMap
    ( \rowIndex ->
        (\(columnIndex, value) -> (rowIndex, columnIndex, value))
          <$> bandedSpdRowEntries dimension rowIndex
    )
    [0 .. dimension - 1]

bandedSpdRowEntries :: Int -> Int -> [(Int, Double)]
bandedSpdRowEntries dimension rowIndex =
  filter
    (\(columnIndex, _) -> columnIndex >= 0 && columnIndex < dimension)
    [ (rowIndex - 2, 0.25),
      (rowIndex - 1, -1.0),
      (rowIndex, 4.0),
      (rowIndex + 1, -1.0),
      (rowIndex + 2, 0.25)
    ]

diagonalBenchmarkValues :: Int -> [Double]
diagonalBenchmarkValues dimension =
  let positiveDimension = max 1 dimension
   in fmap (\indexValue -> 2.0 + fromIntegral (indexValue `mod` positiveDimension) / fromIntegral positiveDimension) [0 .. dimension - 1]

packedSparseBenchmarkOperator :: Int -> Either String (PackedSparseOperator Double)
packedSparseBenchmarkOperator dimension =
  case mkPackedSparseOperator (fromIntegral dimension) (fromIntegral dimension) (packedSparseEntries dimension) of
    Left err -> Left (show err)
    Right operatorValue -> Right operatorValue

packedSparseEntries :: Int -> [PackedSparseEntry Double]
packedSparseEntries dimension =
  [ packedSparseEntry sourceOffset targetOffset (packedSparseCoefficient sourceOffset targetOffset)
    | targetOffset <- [0 .. dimension - 1],
      sourceOffset <- [targetOffset - 1, targetOffset, targetOffset + 1],
      sourceOffset >= 0,
      sourceOffset < dimension
  ]

packedSparseCoefficient :: Int -> Int -> Double
packedSparseCoefficient sourceOffset targetOffset =
  if sourceOffset == targetOffset
    then 2.0
    else (-0.5)

gf2BenchmarkValues :: Int -> Int -> [GF2]
gf2BenchmarkValues rowCount columnCount =
  [ if gf2BenchmarkBit rowIndex columnIndex then GF2One else GF2Zero
    | rowIndex <- [0 .. rowCount - 1],
      columnIndex <- [0 .. columnCount - 1]
  ]

gf2BenchmarkBit :: Int -> Int -> Bool
gf2BenchmarkBit rowIndex columnIndex =
  rowIndex == columnIndex
    || ((rowIndex * 17 + columnIndex * 31 + rowIndex * columnIndex) `mod` 23 == 0)

staticsBenchmarkNetwork :: Int -> Either String ForceNetwork
staticsBenchmarkNetwork spanCount =
  case network (concatMap spanDeclarations [0 .. spanCount - 1]) of
    Left err -> Left (show err)
    Right networkValue -> Right networkValue

spanDeclarations :: Int -> [NetworkDeclaration]
spanDeclarations spanIndex =
  let supportLabel = "support-" <> show spanIndex
      loadLabel = "load-" <> show spanIndex
      coordinate = fromIntegral spanIndex
   in [ support supportLabel (Vec3 coordinate 0.0 0.0),
        load loadLabel (Vec3 coordinate 1.0 0.0) (Vec3 0.0 (-10.0 - coordinate) 0.0),
        member supportLabel loadLabel
      ]
