{-# LANGUAGE LambdaCase #-}

-- | Typed receipt parsing and pure Spade-style SVG projection.
module Moonlight.Triangulation.Bench.DelaunayCompare.Picture
  ( BenchmarkReceipt
  , PictureObstruction (..)
  , parseBenchmarkReceipt
  , pictureFileName
  , renderComparisonPicture
  , renderPictureObstruction
  ) where

import Data.List (group, sort, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Moonlight.Triangulation.Bench.DelaunayCompare.Domain
import Numeric (showFFloat)
import Text.Read (readMaybe)

data ObservationKey = ObservationKey
  { observationFixture :: !FixtureSpec
  , observationImplementation :: !Implementation
  }
  deriving stock (Eq, Ord, Show)

data BenchmarkObservation = BenchmarkObservation
  { benchmarkKey :: !ObservationKey
  , benchmarkMeanPicoseconds :: !Double
  , benchmarkSpreadPicoseconds :: !Double
  }
  deriving stock (Eq, Show)

newtype BenchmarkReceipt = BenchmarkReceipt
  { receiptObservations :: Map.Map ObservationKey BenchmarkObservation
  }

data PictureObstruction
  = MissingCsvHeader
  | UnexpectedCsvHeader !String
  | MalformedCsvRow !Int !String
  | UnknownBenchmarkName !Int !String
  | InvalidBenchmarkScalar !Int !String !String
  | DuplicateBenchmarkObservations ![ObservationKey]
  | MissingBenchmarkObservations ![ObservationKey]
  deriving stock (Eq, Show)

parseBenchmarkReceipt :: String -> Either PictureObstruction BenchmarkReceipt
parseBenchmarkReceipt source =
  case lines source of
    [] -> Left MissingCsvHeader
    header : rows
      | header /= expectedCsvHeader -> Left (UnexpectedCsvHeader header)
      | otherwise -> do
          observations <- traverse (uncurry parseObservationRow) (zip [2 ..] rows)
          let keys = benchmarkKey <$> observations
              duplicateKeys =
                mapMaybe
                  ( \case
                      key : _ : _ -> Just key
                      _ -> Nothing
                  )
                  (group (sort keys))
              observationMap = Map.fromList ((\observation -> (benchmarkKey observation, observation)) <$> observations)
              missingKeys = Set.toList (expectedObservationKeys `Set.difference` Map.keysSet observationMap)
          case (duplicateKeys, missingKeys) of
            (duplicate : duplicates, _) -> Left (DuplicateBenchmarkObservations (duplicate : duplicates))
            ([], missing : remaining) -> Left (MissingBenchmarkObservations (missing : remaining))
            ([], []) -> Right (BenchmarkReceipt observationMap)

renderComparisonPicture
  :: SizeBand
  -> BenchmarkReceipt
  -> String
renderComparisonPicture sizeBand receipt =
  renderSvg sizeBand pointCounts axisMaximum panels
 where
  pointCounts = pointCountValue <$> pointCountsFor sizeBand
  panels = picturePanel receipt sizeBand <$> allPointDistributions
  allObservations = concatMap (concatMap pictureSeriesObservations . picturePanelSeries) panels
  maximumMilliseconds =
    foldr
      (max . observationUpperMilliseconds)
      0
      allObservations
  axisMaximum = niceCeiling maximumMilliseconds

pictureFileName :: SizeBand -> FilePath
pictureFileName = \case
  Small -> "moonlight-delaunay-compare-small.svg"
  Big -> "moonlight-delaunay-compare-big.svg"

renderPictureObstruction :: PictureObstruction -> String
renderPictureObstruction = \case
  MissingCsvHeader -> "benchmark CSV has no header"
  UnexpectedCsvHeader header ->
    "benchmark CSV header was " <> show header <> "; expected " <> show expectedCsvHeader
  MalformedCsvRow lineNumber row ->
    "benchmark CSV row " <> show lineNumber <> " is not a three-column tasty-bench row: " <> show row
  UnknownBenchmarkName lineNumber name ->
    "benchmark CSV row " <> show lineNumber <> " names an unknown comparison case: " <> show name
  InvalidBenchmarkScalar lineNumber field value ->
    "benchmark CSV row "
      <> show lineNumber
      <> " has invalid "
      <> field
      <> ": "
      <> show value
  DuplicateBenchmarkObservations keys ->
    "benchmark CSV repeats comparison cases: " <> show keys
  MissingBenchmarkObservations keys ->
    "benchmark CSV omits comparison cases: " <> show keys

expectedCsvHeader :: String
expectedCsvHeader = "Name,Mean (ps),2*Stdev (ps)"

expectedObservationKeys :: Set.Set ObservationKey
expectedObservationKeys =
  Set.fromList (liftA2 ObservationKey allFixtureSpecs allImplementations)

expectedBenchmarkNames :: Map.Map String ObservationKey
expectedBenchmarkNames =
  Map.fromList ((\key -> (benchmarkName key, key)) <$> Set.toList expectedObservationKeys)

benchmarkName :: ObservationKey -> String
benchmarkName key =
  "All.comparison: creation benchmark ("
    <> sizeBandLabel (fixtureSizeBand fixture)
    <> ")."
    <> implementationLabel (observationImplementation key)
    <> "."
    <> pointDistributionLabel (fixturePointDistribution fixture)
    <> "."
    <> show (pointCountValue (fixturePointCount fixture))
 where
  fixture = observationFixture key

parseObservationRow :: Int -> String -> Either PictureObstruction BenchmarkObservation
parseObservationRow lineNumber row =
  case csvTriple row of
    Nothing -> Left (MalformedCsvRow lineNumber row)
    Just (name, rawMean, rawSpread) -> do
      key <- maybe (Left (UnknownBenchmarkName lineNumber name)) Right (Map.lookup name expectedBenchmarkNames)
      meanPicoseconds <- parseNonNegativeScalar lineNumber "mean picoseconds" rawMean
      spreadPicoseconds <- parseNonNegativeScalar lineNumber "two-standard-deviation spread" rawSpread
      pure
        BenchmarkObservation
          { benchmarkKey = key
          , benchmarkMeanPicoseconds = meanPicoseconds
          , benchmarkSpreadPicoseconds = spreadPicoseconds
          }

csvTriple :: String -> Maybe (String, String, String)
csvTriple row =
  case break (== ',') row of
    (firstColumn, ',' : remaining) ->
      case break (== ',') remaining of
        (secondColumn, ',' : thirdColumn)
          | ',' `notElem` thirdColumn -> Just (firstColumn, secondColumn, thirdColumn)
        _ -> Nothing
    _ -> Nothing

parseNonNegativeScalar :: Int -> String -> String -> Either PictureObstruction Double
parseNonNegativeScalar lineNumber field rawValue =
  case readMaybe rawValue of
    Just value
      | value >= 0 && not (isInfinite value) && not (isNaN value) -> Right value
    _ -> Left (InvalidBenchmarkScalar lineNumber field rawValue)

data PictureSeries = PictureSeries
  { pictureSeriesImplementation :: !Implementation
  , pictureSeriesObservations :: ![BenchmarkObservation]
  }

data PicturePanel = PicturePanel
  { picturePanelDistribution :: !PointDistribution
  , picturePanelSeries :: ![PictureSeries]
  }

picturePanel
  :: BenchmarkReceipt
  -> SizeBand
  -> PointDistribution
  -> PicturePanel
picturePanel receipt sizeBand distribution =
  PicturePanel distribution (pictureSeries receipt sizeBand distribution <$> allImplementations)

pictureSeries
  :: BenchmarkReceipt
  -> SizeBand
  -> PointDistribution
  -> Implementation
  -> PictureSeries
pictureSeries receipt sizeBand distribution implementation =
  PictureSeries implementation observations
 where
  observations =
    sortOn (pointCountValue . fixturePointCount . observationFixture . benchmarkKey)
      [ observation
      | observation <- Map.elems (receiptObservations receipt)
      , let key = benchmarkKey observation
            fixture = observationFixture key
      , fixtureSizeBand fixture == sizeBand
      , fixturePointDistribution fixture == distribution
      , observationImplementation key == implementation
      ]

data PictureLayout = PictureLayout
  { pictureWidth :: !Double
  , pictureHeight :: !Double
  , picturePlotTop :: !Double
  , picturePlotBottom :: !Double
  , picturePanelWidth :: !Double
  , picturePanelLefts :: ![Double]
  }

pictureLayout :: PictureLayout
pictureLayout =
  PictureLayout
    { pictureWidth = 1280
    , pictureHeight = 720
    , picturePlotTop = 168
    , picturePlotBottom = 610
    , picturePanelWidth = 540
    , picturePanelLefts = [70, 670]
    }

renderSvg :: SizeBand -> [Int] -> Double -> [PicturePanel] -> String
renderSvg sizeBand pointCounts axisMaximum panels =
  unlines
    ( [ "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
      , "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 "
          <> coordinate (pictureWidth pictureLayout)
          <> " "
          <> coordinate (pictureHeight pictureLayout)
          <> "\" role=\"img\" aria-labelledby=\"title description\">"
      , "<title id=\"title\">" <> pictureTitle sizeBand <> "</title>"
      , "<desc id=\"description\">CPU construction time for Spade, cdt, delaunator, and Moonlight over the upstream Delaunay comparison fixtures. Lower lines are faster.</desc>"
      , "<defs>"
      , "  <pattern id=\"triangulation-mesh\" width=\"54\" height=\"46\" patternUnits=\"userSpaceOnUse\">"
      , "    <path d=\"M0 46L27 0L54 46ZM0 46L54 46M27 0L27 46\" fill=\"none\" stroke=\"rgb(255,140,0)\" stroke-width=\"0.7\"/>"
      , "  </pattern>"
      , "  <clipPath id=\"header-mesh-clip\"><rect x=\"955\" y=\"8\" width=\"290\" height=\"82\" rx=\"20\"/></clipPath>"
      , "</defs>"
      , "<rect width=\""
          <> coordinate (pictureWidth pictureLayout)
          <> "\" height=\""
          <> coordinate (pictureHeight pictureLayout)
          <> "\" fill=\"white\"/>"
      , "<rect x=\"955\" y=\"8\" width=\"290\" height=\"82\" fill=\"url(#triangulation-mesh)\" opacity=\"0.10\" clip-path=\"url(#header-mesh-clip)\"/>"
      , svgText 640 34 "middle" 24 "500" (pictureTitle sizeBand)
      , svgText 640 61 "middle" 13 "400" "Moonlight against Spade's construction referents · CPU time · lower is faster"
      ]
        <> renderLegend
        <> concat (zipWith (renderPanel pointCounts axisMaximum) (picturePanelLefts pictureLayout) panels)
        <> [ svgText 640 704 "middle" 11 "400" "Whiskers show tasty-bench's reported 2× standard-deviation spread."
           , "</svg>"
           ]
    )

pictureTitle :: SizeBand -> String
pictureTitle = \case
  Small -> "Moonlight Delaunay construction · small point sets"
  Big -> "Moonlight Delaunay construction · big point sets"

renderLegend :: [String]
renderLegend =
  concat
    ( zipWith
        renderLegendItem
        [105, 315, 560, 735, 940]
        allImplementations
    )

renderLegendItem :: Double -> Implementation -> [String]
renderLegendItem x implementation =
  [ "<g aria-label=\"" <> implementationLabel implementation <> "\">"
  , "  <line x1=\"" <> coordinate x <> "\" y1=\"101\" x2=\"" <> coordinate (x + 34) <> "\" y2=\"101\" stroke=\"" <> implementationColor implementation <> "\" stroke-width=\"" <> seriesStrokeWidth implementation <> "\"" <> seriesDash implementation <> "/>"
  , "  " <> renderMarker implementation (x + 17) 101
  , "  " <> svgText (x + 43) 105 "start" 12 "400" (implementationLabel implementation)
  , "</g>"
  ]

renderPanel :: [Int] -> Double -> Double -> PicturePanel -> [String]
renderPanel pointCounts axisMaximum panelLeft panel =
  [ "<g aria-label=\"" <> panelLabel <> "\">"
  , svgText (panelLeft + panelWidth / 2) 144 "middle" 15 "500" panelLabel
  ]
    <> concatMap (renderHorizontalGrid panelLeft panelWidth axisMaximum) [0 .. 5]
    <> concatMap (renderVerticalTick panelLeft panelWidth pointCounts) pointCounts
    <> [ "<rect x=\"" <> coordinate panelLeft <> "\" y=\"" <> coordinate plotTop <> "\" width=\"" <> coordinate panelWidth <> "\" height=\"" <> coordinate plotHeight <> "\" fill=\"none\" stroke=\"rgb(25,25,25)\" stroke-width=\"1\"/>"
       , svgText (panelLeft + panelWidth / 2) 664 "middle" 12 "400" "input size (points)"
       , "<text x=\"" <> coordinate (panelLeft - 55) <> "\" y=\"" <> coordinate (plotTop + plotHeight / 2) <> "\" text-anchor=\"middle\" font-family=\"Helvetica, Arial, sans-serif\" font-size=\"12\" fill=\"rgb(20,20,20)\" transform=\"rotate(-90 " <> coordinate (panelLeft - 55) <> " " <> coordinate (plotTop + plotHeight / 2) <> ")\">CPU time (ms)</text>"
       ]
    <> concatMap (renderSeries panelLeft panelWidth pointCounts axisMaximum) (picturePanelSeries panel)
    <> ["</g>"]
 where
  panelLabel = pointDistributionLabel (picturePanelDistribution panel)
  panelWidth = picturePanelWidth pictureLayout
  plotTop = picturePlotTop pictureLayout
  plotHeight = picturePlotBottom pictureLayout - plotTop

renderHorizontalGrid :: Double -> Double -> Double -> Int -> [String]
renderHorizontalGrid panelLeft panelWidth axisMaximum tickIndex =
  [ "<line x1=\"" <> coordinate panelLeft <> "\" y1=\"" <> coordinate y <> "\" x2=\"" <> coordinate (panelLeft + panelWidth) <> "\" y2=\"" <> coordinate y <> "\" stroke=\"rgb(170,170,170)\" stroke-width=\"0.7\" stroke-dasharray=\"2 4\"/>"
  , svgText (panelLeft - 10) (y + 4) "end" 11 "400" (axisLabel tickValue)
  ]
 where
  tickValue = axisMaximum * fromIntegral tickIndex / 5
  y = pictureY axisMaximum tickValue

renderVerticalTick :: Double -> Double -> [Int] -> Int -> [String]
renderVerticalTick panelLeft panelWidth pointCounts pointCount =
  [ "<line x1=\"" <> coordinate x <> "\" y1=\"610\" x2=\"" <> coordinate x <> "\" y2=\"616\" stroke=\"rgb(25,25,25)\" stroke-width=\"1\"/>"
  , svgText x 635 "middle" 11 "400" (pointCountLabel pointCount)
  ]
 where
  x = pictureX panelLeft panelWidth pointCounts pointCount

renderSeries :: Double -> Double -> [Int] -> Double -> PictureSeries -> [String]
renderSeries panelLeft panelWidth pointCounts axisMaximum series =
  [ "<g aria-label=\"" <> implementationLabel implementation <> "\">"
  , "  <polyline points=\"" <> unwords (observationPoint <$> observations) <> "\" fill=\"none\" stroke=\"" <> implementationColor implementation <> "\" stroke-width=\"" <> seriesStrokeWidth implementation <> "\" stroke-linecap=\"round\" stroke-linejoin=\"round\"" <> seriesDash implementation <> "/>"
  ]
    <> concatMap renderObservation observations
    <> ["</g>"]
 where
  implementation = pictureSeriesImplementation series
  observations = pictureSeriesObservations series
  observationPoint observation =
    coordinate (observationX observation) <> "," <> coordinate (observationY observation)
  observationX =
    pictureX panelLeft panelWidth pointCounts
      . pointCountValue
      . fixturePointCount
      . observationFixture
      . benchmarkKey
  observationY = pictureY axisMaximum . observationMeanMilliseconds
  renderObservation observation =
    [ "  <line x1=\"" <> coordinate x <> "\" y1=\"" <> coordinate upperY <> "\" x2=\"" <> coordinate x <> "\" y2=\"" <> coordinate lowerY <> "\" stroke=\"" <> implementationColor implementation <> "\" stroke-width=\"0.9\" opacity=\"0.48\"/>"
    , "  <line x1=\"" <> coordinate (x - 3) <> "\" y1=\"" <> coordinate upperY <> "\" x2=\"" <> coordinate (x + 3) <> "\" y2=\"" <> coordinate upperY <> "\" stroke=\"" <> implementationColor implementation <> "\" stroke-width=\"0.9\" opacity=\"0.48\"/>"
    , "  <line x1=\"" <> coordinate (x - 3) <> "\" y1=\"" <> coordinate lowerY <> "\" x2=\"" <> coordinate (x + 3) <> "\" y2=\"" <> coordinate lowerY <> "\" stroke=\"" <> implementationColor implementation <> "\" stroke-width=\"0.9\" opacity=\"0.48\"/>"
    , "  " <> renderMarker implementation x y
    ]
   where
    x = observationX observation
    y = observationY observation
    upperY = pictureY axisMaximum (observationUpperMilliseconds observation)
    lowerY = pictureY axisMaximum (observationLowerMilliseconds observation)

implementationColor :: Implementation -> String
implementationColor = \case
  Spade -> "rgb(0,0,139)"
  SpadeHierarchy -> "rgb(139,0,139)"
  Cdt -> "rgb(178,34,34)"
  Delaunator -> "rgb(0,139,139)"
  Moonlight -> "rgb(255,140,0)"

seriesStrokeWidth :: Implementation -> String
seriesStrokeWidth = \case
  Moonlight -> "3.2"
  _ -> "2.0"

seriesDash :: Implementation -> String
seriesDash = \case
  SpadeHierarchy -> " stroke-dasharray=\"7 4\""
  _ -> ""

renderMarker :: Implementation -> Double -> Double -> String
renderMarker implementation x y =
  case implementation of
    Spade -> circle 3.2
    SpadeHierarchy ->
      "<path d=\"M " <> coordinate x <> " " <> coordinate (y - 4.3) <> " L " <> coordinate (x - 4.1) <> " " <> coordinate (y + 3.5) <> " L " <> coordinate (x + 4.1) <> " " <> coordinate (y + 3.5) <> " Z\" fill=\"white\" stroke=\"" <> color <> "\" stroke-width=\"1.6\"/>"
    Cdt ->
      "<rect x=\"" <> coordinate (x - 3.2) <> "\" y=\"" <> coordinate (y - 3.2) <> "\" width=\"6.4\" height=\"6.4\" fill=\"white\" stroke=\"" <> color <> "\" stroke-width=\"1.6\"/>"
    Delaunator ->
      "<path d=\"M " <> coordinate x <> " " <> coordinate (y - 4.2) <> " L " <> coordinate (x + 4.2) <> " " <> coordinate y <> " L " <> coordinate x <> " " <> coordinate (y + 4.2) <> " L " <> coordinate (x - 4.2) <> " " <> coordinate y <> " Z\" fill=\"white\" stroke=\"" <> color <> "\" stroke-width=\"1.6\"/>"
    Moonlight ->
      "<path d=\"M " <> coordinate x <> " " <> coordinate (y - 5.4) <> " L " <> coordinate (x + 1.7) <> " " <> coordinate (y - 1.7) <> " L " <> coordinate (x + 5.4) <> " " <> coordinate y <> " L " <> coordinate (x + 1.7) <> " " <> coordinate (y + 1.7) <> " L " <> coordinate x <> " " <> coordinate (y + 5.4) <> " L " <> coordinate (x - 1.7) <> " " <> coordinate (y + 1.7) <> " L " <> coordinate (x - 5.4) <> " " <> coordinate y <> " L " <> coordinate (x - 1.7) <> " " <> coordinate (y - 1.7) <> " Z\" fill=\"" <> color <> "\" stroke=\"white\" stroke-width=\"1\"/>"
 where
  color = implementationColor implementation
  circle radius =
    "<circle cx=\"" <> coordinate x <> "\" cy=\"" <> coordinate y <> "\" r=\"" <> coordinate radius <> "\" fill=\"white\" stroke=\"" <> color <> "\" stroke-width=\"1.6\"/>"

pictureX :: Double -> Double -> [Int] -> Int -> Double
pictureX panelLeft panelWidth pointCounts pointCount =
  case pointCounts of
    [] -> panelLeft
    firstCount : remainingCounts ->
      let minimumCount = firstCount
          maximumCount = foldr max firstCount remainingCounts
          countRange = maximumCount - minimumCount
       in if countRange == 0
            then panelLeft + panelWidth / 2
            else
              panelLeft
                + panelWidth
                  * fromIntegral (pointCount - minimumCount)
                  / fromIntegral countRange

pictureY :: Double -> Double -> Double
pictureY axisMaximum milliseconds =
  picturePlotBottom pictureLayout
    - plotHeight * min axisMaximum (max 0 milliseconds) / axisMaximum
 where
  plotHeight = picturePlotBottom pictureLayout - picturePlotTop pictureLayout

observationMeanMilliseconds :: BenchmarkObservation -> Double
observationMeanMilliseconds = picosecondsToMilliseconds . benchmarkMeanPicoseconds

observationUpperMilliseconds :: BenchmarkObservation -> Double
observationUpperMilliseconds observation =
  picosecondsToMilliseconds
    (benchmarkMeanPicoseconds observation + benchmarkSpreadPicoseconds observation)

observationLowerMilliseconds :: BenchmarkObservation -> Double
observationLowerMilliseconds observation =
  picosecondsToMilliseconds
    (max 0 (benchmarkMeanPicoseconds observation - benchmarkSpreadPicoseconds observation))

picosecondsToMilliseconds :: Double -> Double
picosecondsToMilliseconds picoseconds = picoseconds / 1.0e9

niceCeiling :: Double -> Double
niceCeiling value
  | value <= 0 = 1
  | otherwise = niceFraction * magnitude
 where
  magnitude = 10 ** fromIntegral (floor (logBase 10 value) :: Int)
  fraction = value / magnitude
  niceFraction
    | fraction <= 1 = 1
    | fraction <= 2 = 2
    | fraction <= 5 = 5
    | otherwise = 10

axisLabel :: Double -> String
axisLabel value
  | value >= 10 = showFFloat (Just 0) value ""
  | value >= 1 = showFFloat (Just 1) value ""
  | otherwise = showFFloat (Just 2) value ""

pointCountLabel :: Int -> String
pointCountLabel pointCount
  | pointCount `mod` 1000 == 0 = show (pointCount `div` 1000) <> "k"
  | otherwise = show pointCount

coordinate :: Double -> String
coordinate value = showFFloat (Just 1) value ""

svgText :: Double -> Double -> String -> Int -> String -> String -> String
svgText x y anchor fontSize fontWeight content =
  "<text x=\""
    <> coordinate x
    <> "\" y=\""
    <> coordinate y
    <> "\" text-anchor=\""
    <> anchor
    <> "\" font-family=\"Helvetica, Arial, sans-serif\" font-size=\""
    <> show fontSize
    <> "\" font-weight=\""
    <> fontWeight
    <> "\" fill=\"rgb(20,20,20)\">"
    <> content
    <> "</text>"
