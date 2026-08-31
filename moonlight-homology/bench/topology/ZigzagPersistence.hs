module ZigzagPersistence
  ( zigzagPersistenceBenchmarks,
  )
where

import Data.List qualified as List
import Moonlight.Homology.Boundary
  ( FiniteChainComplex,
    emptyBoundaryIncidence,
    emptyBoundaryIncidenceOf,
    mkBoundaryEntry,
    mkBoundaryIncidence,
  )
import Moonlight.Homology.Boundary.Finite (degreeCardinality, mkFiniteChainComplex)
import Moonlight.Homology.Chain (HomologicalDegree (..))
import Moonlight.Homology.Persistence
  ( FiniteChainMap,
    FiniteChainZigzag,
    ZigzagArrow (..),
    ZigzagInterval (..),
    mkFiniteChainMapChecked,
    mkFiniteChainZigzag,
    rationalZigzagIntervals,
  )
import Test.Tasty.Bench (Benchmark, bench, bgroup, whnf)

zigzagPersistenceBenchmarks :: Bool -> Benchmark
zigzagPersistenceBenchmarks includeLarge =
  case traverse benchmarkCase ([("vertices-9", 9), ("vertices-17", 17)] <> [("vertices-33", 33) | includeLarge]) of
    Left fixtureFailure -> bench "invalid-zigzag-fixture" (whnf id fixtureFailure)
    Right benchmarks -> bgroup "zigzag-persistence" benchmarks

benchmarkCase :: (String, Int) -> Either String Benchmark
benchmarkCase (caseName, vertexCount) = do
  let seeds = benchmarkIntervalSeeds vertexCount
  diagram <- intervalSumDiagram vertexCount seeds
  intervals <- firstShow "zigzag preflight failed" (rationalZigzagIntervals diagram)
  let recoveredMultiplicity = sum (fmap zigzagIntervalMultiplicity intervals)
  if recoveredMultiplicity /= length seeds
    then Left ("zigzag preflight recovered " <> show recoveredMultiplicity <> " of " <> show (length seeds) <> " interval summands")
    else Right (bench caseName (whnf zigzagChecksum diagram))

zigzagChecksum :: FiniteChainZigzag Int -> Int
zigzagChecksum diagram =
  either
    (const minBound)
    ( foldl'
        ( \checksum interval ->
            checksum * 16777619
              + unHomologicalDegree (zigzagIntervalDegree interval) * 31
              + zigzagIntervalFirst interval * 17
              + zigzagIntervalLast interval * 7
              + zigzagIntervalMultiplicity interval
        )
        2166136261
    )
    (rationalZigzagIntervals diagram)

type IntervalSeed = (Int, Int, Int)

benchmarkIntervalSeeds :: Int -> [IntervalSeed]
benchmarkIntervalSeeds vertexCount =
  fmap (\seedIndex -> (seedIndex, 0, vertexCount - 1)) [0 .. 3]
    <> fmap
      ( \seedIndex ->
          let firstIndex = seedIndex `mod` vertexCount
              maximumSpan = vertexCount - firstIndex
              intervalWidth = 1 + (seedIndex * 7 `mod` maximumSpan)
           in (seedIndex + 4, firstIndex, firstIndex + intervalWidth - 1)
      )
      [0 .. 3 * vertexCount - 1]

intervalSumDiagram :: Int -> [IntervalSeed] -> Either String (FiniteChainZigzag Int)
intervalSumDiagram vertexCount seeds
  | vertexCount <= 0 = Left "zigzag benchmark requires at least one vertex"
  | otherwise = do
      let activeSeeds stageIndex =
            filter (\(_, firstIndex, lastIndex) -> firstIndex <= stageIndex && stageIndex <= lastIndex) seeds
          complexAt stageIndex = zeroComplex (length (activeSeeds stageIndex))
          coordinates sourceIndex targetIndex =
            [ (sourceCoordinate, targetCoordinate)
            | (sourceCoordinate, seed) <- zip [0 :: Int ..] (activeSeeds sourceIndex),
              Just targetCoordinate <- [List.elemIndex seed (activeSeeds targetIndex)]
            ]
          arrowAt arrowIndex =
            if even arrowIndex
              then
                ForwardArrow
                  <$> coordinateMap
                    (complexAt arrowIndex)
                    (complexAt (arrowIndex + 1))
                    (coordinates arrowIndex (arrowIndex + 1))
              else
                BackwardArrow
                  <$> coordinateMap
                    (complexAt (arrowIndex + 1))
                    (complexAt arrowIndex)
                    (coordinates (arrowIndex + 1) arrowIndex)
      arrows <- traverse arrowAt [0 .. vertexCount - 2]
      firstShow "invalid zigzag fixture" (mkFiniteChainZigzag (complexAt 0) arrows)

zeroComplex :: Int -> FiniteChainComplex Int
zeroComplex dimensionValue =
  mkFiniteChainComplex (HomologicalDegree 0) $ \degreeValue ->
    case degreeValue of
      HomologicalDegree 0 -> emptyBoundaryIncidenceOf (fromIntegral dimensionValue) 0
      _ -> emptyBoundaryIncidence

coordinateMap ::
  FiniteChainComplex Int ->
  FiniteChainComplex Int ->
  [(Int, Int)] ->
  Either String (FiniteChainMap Int)
coordinateMap sourceComplex targetComplex coordinates = do
  degreeZeroMap <-
    firstShow
      "invalid coordinate map"
      ( mkBoundaryIncidence
          (fromIntegral (sourceDimension sourceComplex))
          (fromIntegral (sourceDimension targetComplex))
          ( fmap
              (\(sourceIndex, targetIndex) -> mkBoundaryEntry (fromIntegral sourceIndex) (fromIntegral targetIndex) (1 :: Int))
              coordinates
          )
      )
  firstShow
    "invalid chain map"
    ( mkFiniteChainMapChecked sourceComplex targetComplex $ \degreeValue ->
        case degreeValue of
          HomologicalDegree 0 -> degreeZeroMap
          _ -> emptyBoundaryIncidence
    )

sourceDimension :: FiniteChainComplex Int -> Int
sourceDimension finite = degreeCardinality finite (HomologicalDegree 0)

firstShow :: Show failure => String -> Either failure value -> Either String value
firstShow contextMessage =
  either (Left . ((contextMessage <> ": ") <>) . show) Right
