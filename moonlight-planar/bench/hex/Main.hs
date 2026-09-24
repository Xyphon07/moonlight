{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}

module Main (main) where

import BenchMeasure (requireRight, timedValue)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (foldM)
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (traverse_)
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq (..), ViewL (..), (|>))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Hex.Coordinate
  ( HexCoord (..)
  , HexDirection (HexNorthEast)
  , allHexDirections
  )
import Moonlight.Hex.Planar
  ( hexRegionFullyCoveredByPlanarRegion
  , hexRegionIntersectingPlanarRegion
  )
import Moonlight.Hex.Region
import Moonlight.Hex.Topology
import Moonlight.Hex.Serialization
import Moonlight.Planar.Region
  ( planarRegionComponents
  , polygonHoleLoops
  )
import PlanarSelectionBaseline qualified as Baseline
import PlanarSelectionBench (holedRegion, runPlanarRepeated)
import System.Environment (getArgs, withArgs)

main :: IO ()
main = do
  arguments <- getArgs
  case arguments of
    ["--planar-only"] -> do
      benchmarkPlanarSelection "hex-planar-1" 1 1
      benchmarkPlanarSelection "hex-planar-85" 17 5
      benchmarkPlanarSelection "hex-planar-4096" 64 64
    "--planar-repeated" : benchmarkArguments ->
      withArgs benchmarkArguments runPlanarRepeated
    _ -> benchmarkAll

benchmarkAll :: IO ()
benchmarkAll = do
  benchmarkPackedAlgebra Nothing "hex-4096" 1024 4
  benchmarkPackedAlgebra Nothing "hex-1048576" 1024 1024
  benchmarkPackedAlgebra (Just 16) "hex-16777216" 4096 4096
  benchmarkSeparatedGluing "hex-256-separated-strips" 256 16 1024
  benchmarkReferenceTopology
  benchmarkPlanarSelection "hex-planar-4096" 64 64

-- | Both exact lattice-against-region selections over one layout: full
-- coverage and closed intersection against a region whose outer loop avoids
-- the lattice and whose holes include one lattice-aligned cell, so both the
-- generic and the boundary-touching cases are exercised. The selected
-- coordinate lists are printed in full; they are the identity receipt.
benchmarkPlanarSelection :: String -> Int -> Int -> IO ()
benchmarkPlanarSelection label width height = do
  layout <- requireRight (hexLayout (HexCoord 0 0) width height)
  region <- holedRegion >>= evaluate . force
  putStrLn
    ( label
        <> "-layout: cells="
        <> show (hexLayoutCellCount layout)
        <> " holes="
        <> show (sum (fmap (length . polygonHoleLoops) (planarRegionComponents region)))
    )
  baselineIntersecting <-
    timedValue (label <> "-baseline-intersection") (evaluate (force (Baseline.hexRegionIntersectingPlanarRegion layout region)))
  intersecting <-
    timedValue (label <> "-intersection") (evaluate (force (hexRegionIntersectingPlanarRegion layout region)))
  if baselineIntersecting == intersecting
    then pure ()
    else fail (label <> ": baseline/candidate intersection disagreement")
  putStrLn (label <> "-intersection-cells: " <> show (selectedCoordinates intersecting))
  baselineCovered <-
    timedValue (label <> "-baseline-coverage") (evaluate (force (Baseline.hexRegionFullyCoveredByPlanarRegion layout region)))
  covered <-
    timedValue (label <> "-coverage") (evaluate (force (hexRegionFullyCoveredByPlanarRegion layout region)))
  if baselineCovered == covered
    then pure ()
    else fail (label <> ": baseline/candidate coverage disagreement")
  putStrLn (label <> "-coverage-cells: " <> show (selectedCoordinates covered))
  putStrLn
    ( label
        <> "-receipt: cardinalities="
        <> show (hexRegionCardinality covered, hexRegionCardinality intersecting)
    )
 where
  selectedCoordinates = sort . foldHexRegionCoords (flip (:)) []

benchmarkPackedAlgebra :: Maybe Int -> String -> Int -> Int -> IO ()
benchmarkPackedAlgebra repetitions label width height = do
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
  encoded <- timedValue (label <> "-encode") (evaluate (encodeHexRegion left))
  let decodingBudget =
        HexDecodingBudget
          { hexDecodingMaximumInputBytes = fromIntegral (BL.length encoded)
          , hexDecodingMaximumCells = fromIntegral cells
          }
  decoded <- timedValue (label <> "-decode") (requireRight (decodeHexRegion decodingBudget encoded))
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
          , hexRegionCardinality decoded
          , hexRegionCardinality restricted
          , hexRegionCardinality glued
          )
        <> " neighbours="
        <> show neighbourCount
    )
  traverse_
    (\count -> benchmarkSteadyPackedAlgebra label count layout left right)
    repetitions

benchmarkSteadyPackedAlgebra :: String -> Int -> HexLayout -> HexRegion -> HexRegion -> IO ()
benchmarkSteadyPackedAlgebra label repetitions layout left right = do
  _ <-
    timedValue
      (label <> "-steady-union-" <> show repetitions)
      (foldM forceUnion (emptyHexRegion layout) operands)
  _ <-
    timedValue
      (label <> "-steady-generate-" <> show repetitions)
      (foldM forceGeneration (emptyHexRegion layout) [1 .. repetitions])
  (localLeft, localRight) <- prepareGluing (hexLayoutWidth layout) (hexLayoutHeight layout)
  initialGluing <- evaluate . force =<< requireRight (glueCompatibleHexRegions (localLeft :| [localRight]))
  _ <-
    timedValue
      (label <> "-steady-gluing-" <> show repetitions)
      (foldM (forceGluing localLeft localRight) initialGluing [1 .. repetitions])
  pure ()
 where
  operands = fmap (\index -> if even index then left else right) [1 .. repetitions]
  forceUnion :: HexRegion -> HexRegion -> IO HexRegion
  forceUnion accumulated operand =
    evaluate . force =<< requireRight (hexRegionUnion accumulated operand)
  forceGeneration :: HexRegion -> Int -> IO HexRegion
  forceGeneration _ salt =
    evaluate
      ( force
          ( hexRegionGenerate layout
              (\(HexCoord q r) -> (mixedCoordinate q r + salt) `mod` 11 == 0)
          )
      )
  forceGluing :: HexRegion -> HexRegion -> HexRegion -> Int -> IO HexRegion
  forceGluing localLeft localRight accumulated _ =
    evaluate . force
      =<< requireRight
        (glueCompatibleHexRegions (accumulated :| [localLeft, localRight]))

benchmarkRestriction :: String -> HexLayout -> HexRegion -> IO HexRegion
benchmarkRestriction label layout source = do
  let targetHeight = max 1 (hexLayoutHeight layout `quot` 2)
  target <- requireRight (hexLayout (hexLayoutOrigin layout) (hexLayoutWidth layout) targetHeight)
  timedValue (label <> "-restriction") (requireRight (restrictHexRegion target source))

benchmarkGluing :: String -> Int -> Int -> IO HexRegion
benchmarkGluing label width height = do
  (left, right) <- prepareGluing width height
  timedValue
    (label <> "-gluing")
    (requireRight (glueCompatibleHexRegions (left :| [right])))

prepareGluing :: Int -> Int -> IO (HexRegion, HexRegion)
prepareGluing width height = do
  let quarter = max 1 (width `quot` 4)
      localWidth = width - quarter
      membership (HexCoord q r) = (q + 3 * r) `mod` 7 <= 2
  leftLayout <- requireRight (hexLayout (HexCoord 0 0) localWidth height)
  rightLayout <- requireRight (hexLayout (HexCoord quarter 0) localWidth height)
  left <- evaluate (force (hexRegionGenerate leftLayout membership))
  right <- evaluate (force (hexRegionGenerate rightLayout membership))
  pure (left, right)

benchmarkSeparatedGluing :: String -> Int -> Int -> Int -> IO ()
benchmarkSeparatedGluing label sectionCount sectionWidth sectionHeight = do
  sections <- traverse section [0 .. sectionCount - 1]
  case sections of
    firstSection : remainingSections -> do
      glued <-
        timedValue
          (label <> "-gluing")
          (requireRight (glueCompatibleHexRegions (firstSection :| remainingSections)))
      putStrLn
        ( label
            <> "-receipt: cells="
            <> show (hexLayoutCellCount (hexRegionLayout glued))
            <> " cardinality="
            <> show (hexRegionCardinality glued)
        )
    [] -> fail (label <> ": expected a nonempty section family")
 where
  section index = do
    layout <- requireRight (hexLayout (HexCoord (index * sectionWidth) 0) sectionWidth sectionHeight)
    evaluate (force (hexRegionGenerate layout leftPredicate))

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

benchmarkReferenceTopology :: IO ()
benchmarkReferenceTopology = do
  layout <- requireRight (hexLayout (HexCoord 0 0) 256 256)
  let domainPredicate (HexCoord q r) = q `mod` 29 /= 0 || r `mod` 31 == 0
  domainRegion <-
    evaluate
      ( force
          ( hexRegionGenerate
              layout
              domainPredicate
          )
      )
  sourceRegion <-
    evaluate
      ( force
          ( hexRegionGenerate
              layout
              (\coordinate@(HexCoord q r) ->
                 domainPredicate coordinate && q `mod` 97 == 1 && r `mod` 89 == 2)
          )
      )
  let domain = Set.fromList (foldHexRegionCoords (flip (:)) [] domainRegion)
      sources = Set.fromList (foldHexRegionCoords (flip (:)) [] sourceRegion) `Set.intersection` domain
  dilated <- timedValue "hex-reference-dilation" (evaluate (referenceDilation layout domain))
  eroded <- timedValue "hex-reference-erosion" (evaluate (referenceErosion layout domain))
  componentCount <- timedValue "hex-reference-components" (evaluate (referenceComponentCount layout domain))
  distances <- timedValue "hex-reference-distances" (evaluate (referenceDistances layout domain sources))
  nativeDilated <- timedValue "hex-native-dilation" (evaluate (hexRegionDilate domainRegion))
  nativeEroded <- timedValue "hex-native-erosion" (evaluate (hexRegionErode domainRegion))
  nativeInnerFrontier <- timedValue "hex-native-inner-frontier" (evaluate (hexRegionInnerFrontier domainRegion))
  nativeOuterFrontier <- timedValue "hex-native-outer-frontier" (evaluate (hexRegionOuterFrontier domainRegion))
  nativeComponents <- timedValue "hex-native-components" (evaluate (hexRegionComponentLabels domainRegion))
  nativeDistances <-
    timedValue
      "hex-native-distances"
      (requireRight (hexRegionDistancesWithin domainRegion sourceRegion))
  putStrLn
    ( "hex-reference-receipt: cardinalities="
        <> show (Set.size domain, Set.size dilated, Set.size eroded)
        <> " components="
        <> show componentCount
        <> " reached="
        <> show (Map.size distances)
        <> " maximum-distance="
        <> show (maximumDistance distances)
    )
  putStrLn
    ( "hex-native-receipt: cardinalities="
        <> show
          ( hexRegionCardinality domainRegion
          , hexRegionCardinality nativeDilated
          , hexRegionCardinality nativeEroded
          , hexRegionCardinality nativeInnerFrontier
          , hexRegionCardinality nativeOuterFrontier
          )
        <> " components="
        <> show (hexComponentCount nativeComponents)
        <> " reached="
        <> show (hexRegionCardinality (hexDistanceReachableRegion nativeDistances))
        <> " maximum-distance="
        <> show (hexDistanceMaximum nativeDistances)
    )

referenceDilation :: HexLayout -> Set HexCoord -> Set HexCoord
referenceDilation layout selected =
  Set.foldl'
    (\expanded coordinate -> foldl' (insertNeighbour coordinate) expanded allHexDirections)
    selected
    selected
 where
  insertNeighbour coordinate expanded direction =
    maybe expanded (`Set.insert` expanded) (hexNeighbourCoord layout coordinate direction)

referenceErosion :: HexLayout -> Set HexCoord -> Set HexCoord
referenceErosion layout selected =
  Set.filter
    (\coordinate -> all (maybe True (`Set.member` selected) . hexNeighbourCoord layout coordinate) allHexDirections)
    selected

referenceComponentCount :: HexLayout -> Set HexCoord -> Int
referenceComponentCount layout = descend 0
 where
  descend :: Int -> Set HexCoord -> Int
  descend count remaining =
    case Set.minView remaining of
      Nothing -> count
      Just (seed, withoutSeed) ->
        descend (count + 1) (referenceFlood layout remaining (Seq.singleton seed) withoutSeed)

referenceFlood :: HexLayout -> Set HexCoord -> Seq HexCoord -> Set HexCoord -> Set HexCoord
referenceFlood layout domain frontier remaining =
  case Seq.viewl frontier of
    EmptyL -> remaining
    coordinate :< rest ->
      let neighbours =
            foldl'
              (\found direction ->
                 maybe found
                   (\candidate ->
                      if Set.member candidate domain && Set.member candidate remaining
                        then Set.insert candidate found
                        else found)
                   (hexNeighbourCoord layout coordinate direction))
              Set.empty
              allHexDirections
       in referenceFlood
            layout
            domain
            (foldl' (|>) rest neighbours)
            (remaining `Set.difference` neighbours)

referenceDistances :: HexLayout -> Set HexCoord -> Set HexCoord -> Map HexCoord Int
referenceDistances layout domain sources = descend initialFrontier initialDistances
 where
  initialFrontier = Seq.fromList (Set.toAscList sources)
  initialDistances = Map.fromSet (const 0) sources

  descend :: Seq HexCoord -> Map HexCoord Int -> Map HexCoord Int
  descend frontier distances =
    case Seq.viewl frontier of
      EmptyL -> distances
      coordinate :< rest ->
        let nextDistance = maybe 1 (+ 1) (Map.lookup coordinate distances)
            (nextFrontier, nextDistances) =
              foldl'
                (admitNeighbour coordinate nextDistance)
                (rest, distances)
                allHexDirections
         in descend nextFrontier nextDistances

  admitNeighbour
    :: HexCoord
    -> Int
    -> (Seq HexCoord, Map HexCoord Int)
    -> HexDirection
    -> (Seq HexCoord, Map HexCoord Int)
  admitNeighbour coordinate distance (frontier, distances) direction =
    case hexNeighbourCoord layout coordinate direction of
      Just candidate
        | Set.member candidate domain
        , Map.notMember candidate distances ->
            (frontier |> candidate, Map.insert candidate distance distances)
      _ -> (frontier, distances)

maximumDistance :: Map HexCoord Int -> Int
maximumDistance = Map.foldl' max 0
