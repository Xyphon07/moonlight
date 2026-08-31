{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module DomainAlgebra
  ( domainAlgebraBenchmarks,
    domainAlgebraOnceBenchmarks,
  )
where

import Control.DeepSeq (NFData (..))
import Data.Bifunctor (first)
import Data.Vector qualified as Boxed
import Data.Vector.Unboxed qualified as Unboxed
import Fixtures
  ( gf2BenchmarkValues,
  )
import Types
  ( BenchmarkSetup (..),
    BenchmarkWeight (..),
    OnceBenchmark,
    PreparedBenchmarkRow (..),
    eitherBenchmarkWeight,
    renderPreparedBenchmark,
    renderPreparedOnceBenchmark,
  )
import Moonlight.LinAlg.Dense
  ( Matrix,
    fromListMatrix,
    toListMatrix,
  )
import Moonlight.LinAlg.Dense.Exterior
  ( exteriorPowerMatrix,
  )
import Moonlight.LinAlg.Dense.Field
  ( rank,
  )
import Moonlight.LinAlg.Dense.GF2
  ( GF2 (..),
    GF2PackedMatrix,
    GF2SparseColumn,
    defaultGF2SparseReducerConfig,
    gf2SparseColumnRows,
    gf2PackedWords,
    mkGF2SparseColumn,
    mkGF2PackedMatrixFromRowMajor,
    rankGF2SparseColumns,
    rankGF2PackedMatrix,
  )
import Moonlight.LinAlg.Domain
  ( SmithDiagonalForm (..),
    SmithNormalForm (..),
    smithDiagonalForm,
    smithNormalForm,
  )
import Test.Tasty.Bench (Benchmark, bgroup)
import Prelude

data DomainAlgebraCase = DomainAlgebraCase
  { domainSmithMatrix :: !(Matrix 4 4 Integer),
    domainRankMatrix :: !(Matrix 8 8 Double),
    domainExteriorRows :: ![[Integer]],
    domainGF2Matrix :: !GF2PackedMatrix,
    domainGF2SparseColumns :: !(Boxed.Vector GF2SparseColumn)
  }

instance NFData DomainAlgebraCase where
  rnf benchmarkCase =
    domainSmithMatrix benchmarkCase
      `seq` domainRankMatrix benchmarkCase
      `seq` rnf (domainExteriorRows benchmarkCase)
      `seq` rnf (Unboxed.toList (gf2PackedWords (domainGF2Matrix benchmarkCase)))
      `seq` rnf (gf2SparseColumnRows <$> Boxed.toList (domainGF2SparseColumns benchmarkCase))

domainAlgebraBenchmarks :: Benchmark
domainAlgebraBenchmarks =
  bgroup
    "domain algebra, exterior powers, GF2"
    (renderPreparedBenchmark prepareDomainAlgebraCase <$> domainAlgebraRows)

domainAlgebraOnceBenchmarks :: [OnceBenchmark]
domainAlgebraOnceBenchmarks =
  renderPreparedOnceBenchmark "domain algebra, exterior powers, GF2." prepareDomainAlgebraCase <$> domainAlgebraRows

domainAlgebraRows :: [PreparedBenchmarkRow DomainAlgebraCase]
domainAlgebraRows =
  [ PurePreparedBenchmarkRow "Smith normal form 4x4 Integer" smithWeight,
    PurePreparedBenchmarkRow "Smith diagonal form 4x4 Integer" smithDiagonalWeight,
    PurePreparedBenchmarkRow "field rank 8x8 Double" fieldRankWeight,
    PurePreparedBenchmarkRow "exterior power k=2 n=8 Integer" exteriorPowerWeight,
    PurePreparedBenchmarkRow "GF2 packed rank 128x192" gf2RankWeight,
    PurePreparedBenchmarkRow "GF2 sparse-column rank 128x192" gf2SparseRankWeight
  ]

prepareDomainAlgebraCase :: BenchmarkSetup DomainAlgebraCase
prepareDomainAlgebraCase =
  BenchmarkSetup $ do
    smithMatrix <- first show (fromListMatrix @4 @4 @Integer smithEntries)
    rankMatrix <- first show (fromListMatrix @8 @8 @Double rankEntries)
    let gf2Values = gf2BenchmarkValues 128 192
    gf2Matrix <- first show (mkGF2PackedMatrixFromRowMajor 128 192 gf2Values)
    gf2SparseColumns <- gf2SparseBenchmarkColumns 128 192 gf2Values
    pure
      DomainAlgebraCase
        { domainSmithMatrix = smithMatrix,
          domainRankMatrix = rankMatrix,
          domainExteriorRows = exteriorRows,
          domainGF2Matrix = gf2Matrix,
          domainGF2SparseColumns = gf2SparseColumns
        }

smithEntries :: [Integer]
smithEntries =
  [ 6, 10, 14, 22,
    9, 15, 21, 33,
    4, 8, 12, 16,
    5, 11, 17, 23
  ]

rankEntries :: [Double]
rankEntries =
  [ rankEntry rowIndex columnIndex
    | rowIndex <- [0 .. 7],
      columnIndex <- [0 .. 7]
  ]

rankEntry :: Int -> Int -> Double
rankEntry rowIndex columnIndex =
  let diagonalContribution = if rowIndex == columnIndex then 3 else 0
      smoothContribution = fromIntegral (((rowIndex + 1) * (columnIndex + 2)) `mod` 7) / 13.0
   in diagonalContribution + smoothContribution

exteriorRows :: [[Integer]]
exteriorRows =
  [ [ exteriorEntry rowIndex columnIndex | columnIndex <- [0 .. 7] ]
    | rowIndex <- [0 .. 7]
  ]

exteriorEntry :: Int -> Int -> Integer
exteriorEntry rowIndex columnIndex
  | rowIndex == columnIndex = 2 + fromIntegral rowIndex
  | abs (rowIndex - columnIndex) == 1 = 1
  | otherwise = 0

smithWeight :: DomainAlgebraCase -> BenchmarkWeight
smithWeight benchmarkCase =
  eitherBenchmarkWeight
    "Smith normal form 4x4 Integer"
    smithChecksum
    (smithNormalForm (domainSmithMatrix benchmarkCase))

smithDiagonalWeight :: DomainAlgebraCase -> BenchmarkWeight
smithDiagonalWeight benchmarkCase =
  eitherBenchmarkWeight
    "Smith diagonal form 4x4 Integer"
    smithDiagonalChecksum
    (smithDiagonalForm (domainSmithMatrix benchmarkCase))

fieldRankWeight :: DomainAlgebraCase -> BenchmarkWeight
fieldRankWeight benchmarkCase =
  eitherBenchmarkWeight
    "field rank 8x8 Double"
    fromIntegral
    (rank (domainRankMatrix benchmarkCase))

exteriorPowerWeight :: DomainAlgebraCase -> BenchmarkWeight
exteriorPowerWeight benchmarkCase =
  eitherBenchmarkWeight
    "exterior power k=2 n=8 Integer"
    integerRowsChecksum
    (exteriorPowerMatrix 2 (domainExteriorRows benchmarkCase))

gf2RankWeight :: DomainAlgebraCase -> BenchmarkWeight
gf2RankWeight benchmarkCase =
  BenchmarkWeight (fromIntegral (rankGF2PackedMatrix (domainGF2Matrix benchmarkCase)))

gf2SparseRankWeight :: DomainAlgebraCase -> BenchmarkWeight
gf2SparseRankWeight benchmarkCase =
  eitherBenchmarkWeight
    "GF2 sparse-column rank 128x192"
    fromIntegral
    (rankGF2SparseColumns defaultGF2SparseReducerConfig 128 192 (domainGF2SparseColumns benchmarkCase))

smithChecksum :: SmithNormalForm 4 4 Integer -> Double
smithChecksum smithValue =
  integerVectorChecksum (toListMatrix (smithDiagonal smithValue))
    + integerVectorChecksum (toListMatrix (smithLeft smithValue))
    + integerVectorChecksum (toListMatrix (smithRight smithValue))

smithDiagonalChecksum :: SmithDiagonalForm 4 4 Integer -> Double
smithDiagonalChecksum smithValue =
  integerVectorChecksum (toListMatrix (smithDiagonalMatrix smithValue))

integerRowsChecksum :: [[Integer]] -> Double
integerRowsChecksum rows =
  integerVectorChecksum (concat rows)

integerVectorChecksum :: [Integer] -> Double
integerVectorChecksum values =
  fromIntegral (sum (abs <$> values))

gf2SparseBenchmarkColumns :: Int -> Int -> [GF2] -> Either String (Boxed.Vector GF2SparseColumn)
gf2SparseBenchmarkColumns rowCount columnCount values
  | length values /= rowCount * columnCount =
      Left "GF2 sparse-column benchmark fixture has malformed row-major length"
  | otherwise =
      first show
        ( Boxed.fromList
            <$> traverse
              ( \columnIndex ->
                  mkGF2SparseColumn
                    ("GF2 sparse-column benchmark column " <> show columnIndex)
                    rowCount
                    columnIndex
                    (gf2SparseBenchmarkSupport columnIndex)
              )
              [0 .. columnCount - 1]
        )
  where
    indexedValues =
      zip
        [ (rowIndex, columnIndex)
          | rowIndex <- [0 .. rowCount - 1],
            columnIndex <- [0 .. columnCount - 1]
        ]
        values

    gf2SparseBenchmarkSupport columnIndex =
      [ rowIndex
        | ((rowIndex, valueColumn), GF2One) <- indexedValues,
          valueColumn == columnIndex
      ]
