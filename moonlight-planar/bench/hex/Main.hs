{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}

module Main (main) where

import BenchMeasure (requireRight, timedValue)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.List.NonEmpty (NonEmpty (..))
import Moonlight.Hex.Coordinate (HexCoord (..), HexDirection (HexNorthEast))
import Moonlight.Hex.Region

main :: IO ()
main = do
  benchmarkPackedAlgebra "hex-4096" 1024 4
  benchmarkPackedAlgebra "hex-1048576" 1024 1024
  benchmarkPackedAlgebra "hex-16777216" 4096 4096

benchmarkPackedAlgebra :: String -> Int -> Int -> IO ()
benchmarkPackedAlgebra label width height = do
  layout <- requireRight (hexLayout (HexCoord 0 0) width height)
  left <- evaluate (force (hexRegionGenerate layout leftPredicate))
  right <- evaluate (force (hexRegionGenerate layout rightPredicate))
  let cells = hexLayoutCellCount layout
      wordsVisited = hexLayoutWordCount layout
  putStrLn
    ( label
        <> "-layout: cells="
        <> show cells
        <> " words="
        <> show wordsVisited
    )
  unionRegion <- timedValue (label <> "-union") (requireRight (hexRegionUnion left right))
  intersectionRegion <- timedValue (label <> "-intersection") (requireRight (hexRegionIntersection left right))
  differenceRegion <- timedValue (label <> "-difference") (requireRight (hexRegionDifference left right))
  symmetricRegion <- timedValue (label <> "-symmetric-difference") (requireRight (hexRegionSymmetricDifference left right))
  complemented <- timedValue (label <> "-complement") (evaluate (complementHexRegion left))
  generated <- timedValue (label <> "-generate") (evaluate (hexRegionGenerate layout generatedPredicate))
  neighbourCount <-
    timedValue
      (label <> "-neighbour")
      ( evaluate
          (countRepeatedNeighbourLookups cells layout (HexCoord 1 1))
      )
  restricted <- benchmarkRestriction label layout left
  glued <- benchmarkGluing label width height
  putStrLn
    ( label
        <> "-receipt: cardinalities="
        <> show
          ( hexRegionCardinality left
          , hexRegionCardinality right
          , hexRegionCardinality unionRegion
          , hexRegionCardinality intersectionRegion
          , hexRegionCardinality differenceRegion
          , hexRegionCardinality symmetricRegion
          , hexRegionCardinality complemented
          , hexRegionCardinality generated
          , hexRegionCardinality restricted
          , hexRegionCardinality glued
          )
        <> " neighbours="
        <> show neighbourCount
    )

benchmarkRestriction :: String -> HexLayout -> HexRegion -> IO HexRegion
benchmarkRestriction label layout source = do
  let targetHeight = max 1 (hexLayoutHeight layout `quot` 2)
  target <- requireRight (hexLayout (hexLayoutOrigin layout) (hexLayoutWidth layout) targetHeight)
  timedValue (label <> "-restriction") (requireRight (restrictHexRegion target source))

benchmarkGluing :: String -> Int -> Int -> IO HexRegion
benchmarkGluing label width height = do
  let quarter = max 1 (width `quot` 4)
      localWidth = width - quarter
      membership (HexCoord q r) = (q + 3 * r) `mod` 7 <= 2
  leftLayout <- requireRight (hexLayout (HexCoord 0 0) localWidth height)
  rightLayout <- requireRight (hexLayout (HexCoord quarter 0) localWidth height)
  left <- evaluate (force (hexRegionGenerate leftLayout membership))
  right <- evaluate (force (hexRegionGenerate rightLayout membership))
  timedValue
    (label <> "-gluing")
    (requireRight (glueCompatibleHexRegions (left :| [right])))

leftPredicate :: HexCoord -> Bool
leftPredicate (HexCoord q r) = (q + r) `mod` 3 /= 0

rightPredicate :: HexCoord -> Bool
rightPredicate (HexCoord q r) = (2 * q - r) `mod` 5 <= 1

generatedPredicate :: HexCoord -> Bool
generatedPredicate (HexCoord q r) = mixedCoordinate q r `mod` 11 == 0

mixedCoordinate :: Int -> Int -> Int
mixedCoordinate left right = (left + right) * (left - right)

countRepeatedNeighbourLookups :: Int -> HexLayout -> HexCoord -> Int
countRepeatedNeighbourLookups repetitions layout coordinate = descend repetitions 0
 where
  descend :: Int -> Int -> Int
  descend !remaining !count
    | remaining <= 0 = count
    | otherwise =
        descend
          (remaining - 1)
          (maybe count (const (count + 1)) (hexNeighbourCoord layout coordinate HexNorthEast))
