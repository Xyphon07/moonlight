{-# LANGUAGE RecordWildCards #-}

module Moonlight.Planar.Foreign.Region
  ( regionCopyF64
  , regionCounts
  , regionCreateF64
  , regionDifference
  , regionFree
  , regionIntersection
  , regionLocatePointF64
  , regionMeasure
  , regionSymmetricDifference
  , regionUnion
  ) where

import Data.Bifunctor (first)
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Foreign.C.String (withCStringLen)
import Foreign.C.Types (CChar, CDouble (..), CSize, CUInt)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peekElemOff, poke, pokeElemOff)
import qualified Data.Vector as V
import Moonlight.Planar.Exact qualified as T (ExactPoint, exactPointFromPoint, exactPointToEmbeddingCandidate)
import Moonlight.Planar.Internal.ExactRational qualified as T (exactRationalDenominator, exactRationalNumerator)
import Moonlight.Planar.Overlay qualified as T (overlayLayers, overlaySelectedRegion)
import Moonlight.Planar.Region qualified as T (ExactLoop, PlanarRegion, PolygonComponent, RegionPointLocation (..), exactLoop, exactLoopPoints, planarLayer, planarRegion, planarRegionComponents, polygonComponent, polygonHoleLoops, polygonOuterLoop, regionPointLocation)
import qualified Moonlight.Planar.Point as T (Point (..), queryPointValue)
import Moonlight.Planar.Valuation qualified as T (CertifiedInterval (..), ExactLengthMeasurement, PlanarValuations, eulerCharacteristicValue, exactAreaValue, exactLengthBounds, planarValuationsPerimeter, regionValuations, valuationArea, valuationEuler)
import Moonlight.Planar.Foreign.Boundary
  ( checkedCount
  , dereferenceHandle
  , freeHandle
  , produceHandle
  , readPoints
  , requireOutputCapacity
  , requirePointer
  , runBoundary
  )
import Moonlight.Planar.Foreign.Contract
  ( CObstruction
  , CRegion
  , RegionLocationCode (..)
  , regionLocationCodeId
  )
import Moonlight.Planar.Foreign.Obstruction
  ( AbiFailure
  , RegionCountKind (..)
  , RegionLayoutError (..)
  , nullPointerFailure
  , overlayFailure
  , pointInputFailure
  , projectionFailure
  , regionLayoutFailure
  , regionPublicationFailure
  , regionValidationFailure
  , valuationFailure
  )

readCounts
  :: String
  -> Ptr CSize
  -> Int
  -> IO (Either AbiFailure (V.Vector Int))
readCounts _ _ 0 = pure (Right V.empty)
readCounts label pointer count
  | pointer == nullPtr = pure (Left (nullPointerFailure label))
  | otherwise = do
      rawCounts <- V.generateM count (peekElemOff pointer)
      pure (V.mapM (checkedCount 1) rawCounts)

validateCounts
  :: RegionCountKind
  -> Int
  -> V.Vector Int
  -> Either RegionLayoutError ()
validateCounts kind expectedTotal counts =
  case V.findIndex (== 0) counts of
    Just index -> Left (RegionGroupEmpty kind index)
    Nothing
      | observedTotal /= toInteger expectedTotal ->
          Left (RegionCountTotalMismatch kind observedTotal (toInteger expectedTotal))
      | otherwise -> Right ()
 where
  observedTotal = V.foldl' (\total count -> total + toInteger count) 0 counts

regionCreateF64
  :: Ptr CDouble
  -> CSize
  -> Ptr CSize
  -> CSize
  -> Ptr CSize
  -> CSize
  -> Ptr (Ptr CRegion)
  -> Ptr CObstruction
  -> IO CUInt
regionCreateF64 coordinates rawPointCount loopPointCounts rawLoopCount componentLoopCounts rawComponentCount output obstructionPointer =
  runBoundary obstructionPointer $
    produceHandle output $
      readRegionF64
        coordinates
        rawPointCount
        loopPointCounts
        rawLoopCount
        componentLoopCounts
        rawComponentCount

readRegionF64
  :: Ptr CDouble
  -> CSize
  -> Ptr CSize
  -> CSize
  -> Ptr CSize
  -> CSize
  -> IO (Either AbiFailure T.PlanarRegion)
readRegionF64 coordinates rawPointCount loopPointCounts rawLoopCount componentLoopCounts rawComponentCount =
  case checkedRegionInputCounts rawPointCount rawLoopCount rawComponentCount of
    Left failure -> pure (Left failure)
    Right (pointCount, loopCount, componentCount) -> do
      points <- readPoints coordinates pointCount
      loopCounts <- readCounts "loop_point_counts" loopPointCounts loopCount
      componentCounts <- readCounts "component_loop_counts" componentLoopCounts componentCount
      pure $ do
        admittedPoints <- points
        admittedLoopCounts <- loopCounts
        admittedComponentCounts <- componentCounts
        first regionLayoutFailure (validateCounts LoopPointCounts pointCount admittedLoopCounts)
        first regionLayoutFailure (validateCounts ComponentLoopCounts loopCount admittedComponentCounts)
        buildRegion admittedPoints admittedLoopCounts admittedComponentCounts

checkedRegionInputCounts
  :: CSize
  -> CSize
  -> CSize
  -> Either AbiFailure (Int, Int, Int)
checkedRegionInputCounts rawPointCount rawLoopCount rawComponentCount = do
  pointCount <- checkedCount 2 rawPointCount
  loopCount <- checkedCount 1 rawLoopCount
  componentCount <- checkedCount 1 rawComponentCount
  pure (pointCount, loopCount, componentCount)

buildRegion
  :: V.Vector T.Point
  -> V.Vector Int
  -> V.Vector Int
  -> Either AbiFailure T.PlanarRegion
buildRegion points loopCounts componentCounts = do
  exactPoints <-
    V.imapM
      (\index point -> first (pointInputFailure index point) (T.exactPointFromPoint point))
      points
  loops <- V.imapM (buildLoop exactPoints) (adjacentOffsets loopCounts)
  components <- V.imapM (buildComponent loops) (adjacentOffsets componentCounts)
  first regionValidationFailure (T.planarRegion (V.toList components))

adjacentOffsets :: V.Vector Int -> V.Vector (Int, Int)
adjacentOffsets counts =
  let offsets = V.scanl' (+) 0 counts
   in V.zip offsets (V.drop 1 offsets)

buildLoop
  :: V.Vector T.ExactPoint
  -> Int
  -> (Int, Int)
  -> Either AbiFailure T.ExactLoop
buildLoop points loopIndex (start, end) =
  case NonEmpty.nonEmpty (V.toList (V.slice start (end - start) points)) of
    Nothing -> Left (regionLayoutFailure (RegionGroupEmpty LoopPointCounts loopIndex))
    Just submitted -> first regionValidationFailure (T.exactLoop submitted)

buildComponent
  :: V.Vector T.ExactLoop
  -> Int
  -> (Int, Int)
  -> Either AbiFailure T.PolygonComponent
buildComponent loops componentIndex (start, end) =
  case NonEmpty.nonEmpty (V.toList (V.slice start (end - start) loops)) of
    Nothing -> Left (regionLayoutFailure (RegionGroupEmpty ComponentLoopCounts componentIndex))
    Just (outer :| holes) -> first regionValidationFailure (T.polygonComponent outer holes)

regionShape
  :: T.PlanarRegion
  -> ([T.PolygonComponent], [[T.ExactLoop]], [T.ExactLoop])
regionShape region =
  let components = T.planarRegionComponents region
      componentLoops component = T.polygonOuterLoop component : T.polygonHoleLoops component
      loopsByComponent = map componentLoops components
   in (components, loopsByComponent, concat loopsByComponent)

regionCounts
  :: Ptr CRegion
  -> Ptr CSize
  -> Ptr CSize
  -> Ptr CSize
  -> Ptr CObstruction
  -> IO CUInt
regionCounts regionPointer componentCountOutput loopCountOutput pointCountOutput obstructionPointer =
  runBoundary obstructionPointer $
    case
      requirePointer "region" regionPointer
        >> requirePointer "component_count" componentCountOutput
        >> requirePointer "loop_count" loopCountOutput
        >> requirePointer "point_count" pointCountOutput
    of
      Left failure -> pure (Left failure)
      Right () -> do
        region <- dereferenceHandle regionPointer
        let (components, _, loops) = regionShape region
            pointCount = sum (map (NonEmpty.length . T.exactLoopPoints) loops)
        poke componentCountOutput (fromIntegral (length components))
        poke loopCountOutput (fromIntegral (length loops))
        poke pointCountOutput (fromIntegral pointCount)
        pure (Right ())

data RegionProjection = RegionProjection
  { projectionPoints :: !(V.Vector T.Point)
  , projectionLoopPointOffsets :: !(V.Vector CSize)
  , projectionComponentLoopOffsets :: !(V.Vector CSize)
  }

regionProjection :: T.PlanarRegion -> Either AbiFailure RegionProjection
regionProjection region = do
  let (_, loopsByComponent, loops) = regionShape region
      exactPoints = concatMap (NonEmpty.toList . T.exactLoopPoints) loops
      loopPointOffsets = scanl (+) 0 (map (NonEmpty.length . T.exactLoopPoints) loops)
      componentLoopOffsets = scanl (+) 0 (map length loopsByComponent)
  projectedPoints <-
    V.fromList
      <$> traverse
        (uncurry projectPoint)
        (zip [0 ..] exactPoints)
  pure
    RegionProjection
      { projectionPoints = projectedPoints
      , projectionLoopPointOffsets = V.fromList (map fromIntegral loopPointOffsets)
      , projectionComponentLoopOffsets = V.fromList (map fromIntegral componentLoopOffsets)
      }
 where
  projectPoint index point =
    T.queryPointValue
      <$> first (projectionFailure index) (T.exactPointToEmbeddingCandidate point)

regionCopyF64
  :: Ptr CRegion
  -> Ptr CDouble
  -> CSize
  -> Ptr CSize
  -> CSize
  -> Ptr CSize
  -> CSize
  -> Ptr CObstruction
  -> IO CUInt
regionCopyF64 regionPointer coordinates rawPointCapacity loopPointOffsets rawLoopOffsetCapacity componentLoopOffsets rawComponentOffsetCapacity obstructionPointer =
  runBoundary obstructionPointer $
    case
      (,,)
        <$> (requirePointer "region" regionPointer >> checkedCount 2 rawPointCapacity)
        <*> checkedCount 1 rawLoopOffsetCapacity
        <*> checkedCount 1 rawComponentOffsetCapacity
    of
      Left failure -> pure (Left failure)
      Right (pointCapacity, loopOffsetCapacity, componentOffsetCapacity) -> do
        region <- dereferenceHandle regionPointer
        case regionProjection region of
          Left failure -> pure (Left failure)
          Right RegionProjection {..} ->
            case
              requireOutputCapacity "coordinates" coordinates (V.length projectionPoints) pointCapacity
                >> requireOutputCapacity "loop_point_offsets" loopPointOffsets (V.length projectionLoopPointOffsets) loopOffsetCapacity
                >> requireOutputCapacity "component_loop_offsets" componentLoopOffsets (V.length projectionComponentLoopOffsets) componentOffsetCapacity
            of
              Left failure -> pure (Left failure)
              Right () -> do
                V.imapM_
                  ( \index (T.Point x y) -> do
                      pokeElemOff coordinates (index * 2) (CDouble x)
                      pokeElemOff coordinates (index * 2 + 1) (CDouble y)
                  )
                  projectionPoints
                V.imapM_ (pokeElemOff loopPointOffsets) projectionLoopPointOffsets
                V.imapM_ (pokeElemOff componentLoopOffsets) projectionComponentLoopOffsets
                pure (Right ())

regionUnion, regionIntersection, regionDifference, regionSymmetricDifference :: Ptr CRegion -> Ptr CRegion -> Ptr (Ptr CRegion) -> Ptr CObstruction -> IO CUInt
regionUnion = binaryRegionOperation (\(left, right) -> left || right)
regionIntersection = binaryRegionOperation (\(left, right) -> left && right)
regionDifference = binaryRegionOperation (\(left, right) -> left && not right)
regionSymmetricDifference = binaryRegionOperation (uncurry (/=))

binaryRegionOperation
  :: ((Bool, Bool) -> Bool)
  -> Ptr CRegion
  -> Ptr CRegion
  -> Ptr (Ptr CRegion)
  -> Ptr CObstruction
  -> IO CUInt
binaryRegionOperation selected leftPointer rightPointer output obstructionPointer =
  runBoundary obstructionPointer $ produceHandle output $ do
    case requirePointer "left region" leftPointer >> requirePointer "right region" rightPointer of
      Left failure -> pure (Left failure)
      Right () -> do
        left <- dereferenceHandle leftPointer
        right <- dereferenceHandle rightPointer
        pure (exactRegionBoolean selected left right)

exactRegionBoolean
  :: ((Bool, Bool) -> Bool)
  -> T.PlanarRegion
  -> T.PlanarRegion
  -> Either AbiFailure T.PlanarRegion
exactRegionBoolean selected left right = do
  leftLayer <- first regionValidationFailure (T.planarLayer False (Map.singleton True left))
  rightLayer <- first regionValidationFailure (T.planarLayer False (Map.singleton True right))
  overlay <- first overlayFailure (T.overlayLayers leftLayer rightLayer)
  first regionPublicationFailure (T.overlaySelectedRegion selected overlay)

regionLocatePointF64
  :: Ptr CRegion
  -> CDouble
  -> CDouble
  -> Ptr CUInt
  -> Ptr CObstruction
  -> IO CUInt
regionLocatePointF64 regionPointer (CDouble x) (CDouble y) output obstructionPointer =
  runBoundary obstructionPointer $
    case
      requirePointer "region" regionPointer
        >> requirePointer "location" output
        >> first (pointInputFailure 0 (T.Point x y)) (T.exactPointFromPoint (T.Point x y))
    of
      Left failure -> pure (Left failure)
      Right query -> do
        region <- dereferenceHandle regionPointer
        poke output (regionLocationCode (T.regionPointLocation region query))
        pure (Right ())

regionLocationCode :: T.RegionPointLocation -> CUInt
regionLocationCode location =
  fromIntegral
    ( regionLocationCodeId
        ( case location of
            T.RegionExterior -> RegionLocationExterior
            T.RegionOnBoundary -> RegionLocationBoundary
            T.RegionInterior -> RegionLocationInterior
        )
    )

regionMeasure
  :: Ptr CRegion
  -> Ptr Int64
  -> Ptr CChar
  -> CSize
  -> Ptr CSize
  -> Ptr CDouble
  -> Ptr CDouble
  -> Ptr CObstruction
  -> IO CUInt
regionMeasure regionPointer eulerOutput areaRatioOutput rawAreaCapacity areaBytesWritten perimeterLowerOutput perimeterUpperOutput obstructionPointer =
  runBoundary obstructionPointer $
    case
      requirePointer "region" regionPointer
        >> requirePointer "euler_characteristic" eulerOutput
        >> requirePointer "area_bytes_written" areaBytesWritten
        >> requirePointer "perimeter_lower" perimeterLowerOutput
        >> requirePointer "perimeter_upper" perimeterUpperOutput
        >> checkedCount 1 rawAreaCapacity
    of
      Left failure -> pure (Left failure)
      Right areaCapacity -> do
        region <- dereferenceHandle regionPointer
        case regionMeasurements region of
          Left failure -> pure (Left failure)
          Right (valuations, perimeter) -> do
            let area = T.exactAreaValue (T.valuationArea valuations)
                areaText =
                  show (T.exactRationalNumerator area)
                    <> "/"
                    <> show (T.exactRationalDenominator area)
                bounds = T.exactLengthBounds perimeter
            poke eulerOutput (fromIntegral (T.eulerCharacteristicValue (T.valuationEuler valuations)))
            poke perimeterLowerOutput (CDouble (T.intervalLower bounds))
            poke perimeterUpperOutput (CDouble (T.intervalUpper bounds))
            copyCStringOutput areaText areaRatioOutput areaCapacity areaBytesWritten

regionMeasurements
  :: T.PlanarRegion
  -> Either AbiFailure (T.PlanarValuations, T.ExactLengthMeasurement)
regionMeasurements region = do
  valuations <- first valuationFailure (T.regionValuations region)
  perimeter <- first valuationFailure (T.planarValuationsPerimeter valuations)
  pure (valuations, perimeter)

copyCStringOutput
  :: String
  -> Ptr CChar
  -> Int
  -> Ptr CSize
  -> IO (Either AbiFailure ())
copyCStringOutput value output capacity bytesWritten =
  withCStringLen value $ \(source, byteCount) -> do
    poke bytesWritten (fromIntegral byteCount)
    case requireOutputCapacity "area_ratio_utf8" output (byteCount + 1) capacity of
      Left failure -> pure (Left failure)
      Right () -> do
        copyBytes output source byteCount
        pokeElemOff output byteCount 0
        pure (Right ())

regionFree :: Ptr CRegion -> IO ()
regionFree = freeHandle
