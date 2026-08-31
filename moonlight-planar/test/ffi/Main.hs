module Main (main) where

import Control.Exception (bracket)
import Control.Monad (unless)
import Data.Int (Int64)
import Data.List (nub)
import Data.Maybe (mapMaybe)
import Data.Word (Word32)
import Foreign.C.String (peekCString)
import Foreign.C.Types (CDouble (..), CSize (..), CUInt)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray, peekArray, withArray)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable (sizeOf), peek)
import Moonlight.Triangulation.Foreign.Contract
import Moonlight.Triangulation.Foreign.Mesh
import Moonlight.Triangulation.Foreign.Morphology
import Moonlight.Triangulation.Foreign.Region

type MeshPointer = Ptr CMesh
type RegionPointer = Ptr CRegion
type Point = (Double, Double)
type Loop = [Point]
data ComponentInput = ComponentInput !Loop ![Loop]

data RegionMeasurement = RegionMeasurement
  { measuredEuler :: !Int64
  , measuredArea :: !String
  , measuredPerimeterLower :: !Double
  , measuredPerimeterUpper :: !Double
  }

main :: IO ()
main = do
  testStableWireContract
  unless (sizeOf obstructionLayoutWitness == 320) $
    fail "C obstruction layout changed"
  unless (sizeOf receiptLayoutWitness == 72) $
    fail "C Minkowski receipt layout changed"
  bracket (buildMesh [(0, 0), (2, 0), (0, 2), (2, 2)]) meshFree $ \left ->
    bracket (buildMesh [(2, 0), (4, 0), (2, 2), (4, 2)]) meshFree $ \right -> do
      requireMeshCount "left vertex count" meshVertexCount left 4
      requireMeshCount "left triangle count" meshTriangleCount left 2
      testDenseCopies left
      testImmutableBatch left
      testBatchAdmission left
      testSiteSetAlgebra left right
  testExactRegionSurface
  testTypedCoordinateRefusal
  testCoordinateCountOverflow
  testNullPointerRefusal
  testRegionLayoutRefusal
  putStrLn "ffi: ok"

testStableWireContract :: IO ()
testStableWireContract = do
  unless
    ( allDistinct (fmap abiStatusId allAbiStatuses)
        && allDistinct (fmap obstructionCodeId allObstructionCodes)
        && allDistinct (fmap coordinateErrorCodeId allCoordinateErrorCodes)
        && allDistinct (fmap regionLocationCodeId allRegionLocationCodes)
        && allDistinct (fmap minkowskiOperationCodeId allMinkowskiOperationCodes)
        && allDistinct wireSymbols
        && allDistinct (fmap abiFunctionSymbol allAbiFunctions)
        && allDistinct (mapMaybe haskellImplementationName allAbiFunctions)
        && allDistinct (fmap abiHandleSymbol allAbiHandles <> fmap abiStructSymbol allAbiStructs)
    )
    (fail "FFI contract contains duplicate identifiers")
  unless
    ( abiVersion == 2
        && fmap abiStatusId allAbiStatuses == [0, 1, 2, 3, 4, 5]
        && obstructionCodeId ObstructionInvalidCoordinate == 1
        && obstructionCodeId ObstructionNullPointer == 100
        && obstructionCodeId ObstructionCountOverflow == 101
        && obstructionCodeId ObstructionBufferTooSmall == 102
        && obstructionCodeId ObstructionRegionLayoutInvalid == 200
        && fmap regionLocationCodeId allRegionLocationCodes == [0, 1, 2]
        && fmap minkowskiOperationCodeId allMinkowskiOperationCodes == [0, 1, 2, 3]
    )
    (fail "stable FFI wire identifiers changed")
 where
  wireSymbols =
    fmap abiStatusSymbol allAbiStatuses
      <> fmap obstructionCodeSymbol allObstructionCodes
      <> fmap coordinateErrorCodeSymbol allCoordinateErrorCodes
      <> fmap regionLocationCodeSymbol allRegionLocationCodes
      <> fmap minkowskiOperationCodeSymbol allMinkowskiOperationCodes
  haskellImplementationName function =
    case abiFunctionImplementation function of
      AbiFunctionRuntime -> Nothing
      AbiFunctionHaskell _ name -> Just name

allDistinct :: Eq value => [value] -> Bool
allDistinct values = length values == length (nub values)

obstructionLayoutWitness :: CObstruction
obstructionLayoutWitness = CObstruction 0 0 0 0 0 0 0 0 0 ""

receiptLayoutWitness :: CMinkowskiReceipt
receiptLayoutWitness = CMinkowskiReceipt 0 0 0 0 0 0 0 0 0

buildMesh :: [Point] -> IO MeshPointer
buildMesh points =
  withPointArray points $ \coordinates ->
    produceHandle "delaunay" (delaunayF64 coordinates (fromIntegral (length points)))

testDenseCopies :: MeshPointer -> IO ()
testDenseCopies mesh = do
  alloca $ \written ->
    alloca $ \obstruction -> do
      status <- meshCopyVerticesF64 mesh nullPtr 0 written obstruction
      requireStatus "undersized vertex copy" AbiStatusBufferTooSmall status obstruction
      required <- peek written
      refusal <- peek obstruction
      unless (required == 4 && obstructionCode refusal == obstructionCodeId ObstructionBufferTooSmall) $
        fail "undersized vertex copy lost its required-capacity witness"
  allocaArray 8 $ \coordinates ->
    alloca $ \written ->
      alloca $ \obstruction -> do
        status <- meshCopyVerticesF64 mesh coordinates 4 written obstruction
        requireStatus "vertex copy" AbiStatusOk status obstruction
        values <- peekArray 8 coordinates
        unless (length values == 8) (fail "vertex copy wrote the wrong coordinate extent")
  allocaArray 6 $ \triangles ->
    alloca $ \written ->
      alloca $ \obstruction -> do
        status <- meshCopyTrianglesU32 mesh triangles 2 written obstruction
        requireStatus "triangle copy" AbiStatusOk status obstruction
        indices <- peekArray 6 triangles :: IO [Word32]
        unless (all (< 4) indices) (fail "triangle copy produced an out-of-range vertex")

testImmutableBatch :: MeshPointer -> IO ()
testImmutableBatch original =
  withPointArray [(1, 1), (3, 1)] $ \coordinates ->
    bracket
      (produceHandle "batch insert" (meshInsertManyF64 original coordinates 2))
      meshFree
      (\revised -> do
        requireMeshCount "original after batch" meshVertexCount original 4
        requireMeshCount "revised after batch" meshVertexCount revised 6)

testBatchAdmission :: MeshPointer -> IO ()
testBatchAdmission original = do
  testLateInvalidPoint
  testCanonicalZeros
 where
  testLateInvalidPoint =
    withPointArray [(1, 1), (3, 1), (0 / 0, 1)] $ \coordinates ->
      alloca $ \output ->
        alloca $ \obstruction -> do
          status <- meshInsertManyF64 original coordinates 3 output obstruction
          requireStatus "late invalid batch point" AbiStatusGeometryObstruction status obstruction
          refusal <- peek obstruction
          unless
            ( obstructionCode refusal == obstructionCodeId ObstructionInvalidCoordinate
                && obstructionCoordinateError refusal == coordinateErrorCodeId CoordinateErrorNaN
                && obstructionInputIndex refusal == 2
            )
            (fail "late invalid batch point lost its raw input-index obstruction")
          requireMeshCount "original after refused batch" meshVertexCount original 4
  testCanonicalZeros =
    let negativeZero = -0.0 :: Double
     in withPointArray [(negativeZero, 1), (1, negativeZero)] $ \coordinates ->
          bracket
            (produceHandle "canonical-zero batch" (meshInsertManyF64 original coordinates 2))
            meshFree
            (\revised -> do
               requireMeshCount "canonical-zero batch vertex count" meshVertexCount revised 6
               allocaArray 12 $ \copiedCoordinates ->
                 alloca $ \_written ->
                   alloca $ \obstruction -> do
                     status <- meshCopyVerticesF64 revised copiedCoordinates 6 _written obstruction
                     requireStatus "canonical-zero batch copy" AbiStatusOk status obstruction
                     copied <- map unCDouble <$> peekArray 12 copiedCoordinates
                     unless (not (any isNegativeZero copied)) $
                       fail "canonical-zero batch published a negative zero")
  unCDouble (CDouble value) = value

testSiteSetAlgebra :: MeshPointer -> MeshPointer -> IO ()
testSiteSetAlgebra left right = do
  test "site union" meshSiteUnion 6
  test "site intersection" meshSiteIntersection 2
  test "site difference" meshSiteDifference 2
  test "site symmetric difference" meshSiteSymmetricDifference 4
 where
  test label operation expected =
    bracket (produceHandle label (operation left right)) meshFree $ \result ->
      requireMeshCount label meshVertexCount result expected

testExactRegionSurface :: IO ()
testExactRegionSurface =
  bracket (buildRegion [ComponentInput leftSquare []]) regionFree $ \left ->
    bracket (buildRegion [ComponentInput rightSquare []]) regionFree $ \right -> do
      testRegionProjection left
      testRegionBooleans left right
      testRegionPointLocation left
      testRegionMorphology left right
 where
  leftSquare = [(0, 0), (2, 0), (2, 2), (0, 2)]
  rightSquare = [(1, 0), (3, 0), (3, 2), (1, 2)]

testRegionProjection :: RegionPointer -> IO ()
testRegionProjection region =
  alloca $ \componentCountOutput ->
    alloca $ \loopCountOutput ->
      alloca $ \pointCountOutput ->
        alloca $ \obstruction -> do
          status <- regionCounts region componentCountOutput loopCountOutput pointCountOutput obstruction
          requireStatus "region counts" AbiStatusOk status obstruction
          componentCount <- fromIntegral <$> peek componentCountOutput
          loopCount <- fromIntegral <$> peek loopCountOutput
          pointCount <- fromIntegral <$> peek pointCountOutput
          unless ((componentCount, loopCount, pointCount) == (1, 1, 4)) $
            fail "region counts lost component/loop/point structure"
          allocaArray (pointCount * 2) $ \coordinates ->
            allocaArray (loopCount + 1) $ \loopOffsets ->
              allocaArray (componentCount + 1) $ \componentOffsets -> do
                copyStatus <-
                  regionCopyF64
                    region
                    coordinates
                    (fromIntegral pointCount)
                    loopOffsets
                    (fromIntegral (loopCount + 1))
                    componentOffsets
                    (fromIntegral (componentCount + 1))
                    obstruction
                requireStatus "region copy" AbiStatusOk copyStatus obstruction
                copiedLoopOffsets <- peekArray (loopCount + 1) loopOffsets
                copiedComponentOffsets <- peekArray (componentCount + 1) componentOffsets
                unless (copiedLoopOffsets == [0, 4] && copiedComponentOffsets == [0, 1]) $
                  fail "region copy changed the bulk offset topology"

testRegionBooleans :: RegionPointer -> RegionPointer -> IO ()
testRegionBooleans left right = do
  test "region union" regionUnion 1 "6/1" 10
  test "region intersection" regionIntersection 1 "2/1" 6
  test "region difference" regionDifference 1 "2/1" 6
  test "region symmetric difference" regionSymmetricDifference 2 "4/1" 12
 where
  test label operation expectedEuler expectedArea expectedPerimeter =
    bracket (produceHandle label (operation left right)) regionFree $ \result -> do
      measurement <- measureRegion result
      unless (measuredEuler measurement == expectedEuler && measuredArea measurement == expectedArea) $
        fail (label <> " returned the wrong exact valuation")
      requireCertifiedContainment
        label
        expectedPerimeter
        (measuredPerimeterLower measurement)
        (measuredPerimeterUpper measurement)

testRegionPointLocation :: RegionPointer -> IO ()
testRegionPointLocation region = do
  requireRegionLocation "interior location" region (1, 1) RegionLocationInterior
  requireRegionLocation "boundary location" region (0, 1) RegionLocationBoundary
  requireRegionLocation "exterior location" region (3, 1) RegionLocationExterior

testRegionMorphology :: RegionPointer -> RegionPointer -> IO ()
testRegionMorphology left right =
  withPointArray [(-0.5, -0.5), (0.5, -0.5), (0.5, 0.5), (-0.5, 0.5)] $ \coordinates ->
    bracket
      (produceHandle "structuring element" (structuringElementCreateF64 coordinates 4))
      structuringElementFree
      (\element -> do
        test "Minkowski sum" regionMinkowskiSum left right MinkowskiOperationAddition "16/1"
        test "offset" regionOffset element left MinkowskiOperationAddition "9/1"
        test "inset" regionInset element left MinkowskiOperationErosion "1/1"
        test "open" regionOpen element left MinkowskiOperationOpening "4/1"
        test "close" regionClose element left MinkowskiOperationClosing "4/1")
 where
  test
    :: String
    -> (first -> RegionPointer -> Ptr RegionPointer -> Ptr CMinkowskiReceipt -> Ptr CObstruction -> IO CUInt)
    -> first
    -> RegionPointer
    -> MinkowskiOperationCode
    -> String
    -> IO ()
  test label operation firstInput secondInput expectedOperation expectedArea =
    bracket
      (produceRegionWithReceipt label (operation firstInput secondInput))
      (regionFree . fst)
      (\(result, receipt) -> do
        measurement <- measureRegion result
        unless (receiptOperation receipt == minkowskiOperationCodeId expectedOperation && measuredArea measurement == expectedArea) $
          fail (label <> " lost its result or morphology receipt"))

measureRegion :: RegionPointer -> IO RegionMeasurement
measureRegion region =
  alloca $ \eulerOutput ->
    allocaArray 64 $ \areaOutput ->
      alloca $ \areaBytesWritten ->
        alloca $ \perimeterLowerOutput ->
          alloca $ \perimeterUpperOutput ->
            alloca $ \obstruction -> do
              status <-
                regionMeasure
                  region
                  eulerOutput
                  areaOutput
                  64
                  areaBytesWritten
                  perimeterLowerOutput
                  perimeterUpperOutput
                  obstruction
              requireStatus "region measure" AbiStatusOk status obstruction
              measuredEuler <- peek eulerOutput
              measuredArea <- peekCString areaOutput
              CDouble measuredPerimeterLower <- peek perimeterLowerOutput
              CDouble measuredPerimeterUpper <- peek perimeterUpperOutput
              pure (RegionMeasurement measuredEuler measuredArea measuredPerimeterLower measuredPerimeterUpper)

requireCertifiedContainment :: String -> Double -> Double -> Double -> IO ()
requireCertifiedContainment label expected lower upper =
  unless (lower <= expected && expected <= upper) $
    fail (label <> " perimeter certificate excludes the exact perimeter")

requireRegionLocation :: String -> RegionPointer -> Point -> RegionLocationCode -> IO ()
requireRegionLocation label region (x, y) expected =
  alloca $ \output ->
    alloca $ \obstruction -> do
      status <- regionLocatePointF64 region (CDouble x) (CDouble y) output obstruction
      requireStatus label AbiStatusOk status obstruction
      observed <- peek output
      unless (observed == fromIntegral (regionLocationCodeId expected)) $
        fail (label <> " returned location " <> show observed)

buildRegion :: [ComponentInput] -> IO RegionPointer
buildRegion components =
  let loopsByComponent = map (\(ComponentInput outer holes) -> outer : holes) components
      loops = concat loopsByComponent
      points = concat loops
      loopPointCounts = map length loops
      componentLoopCounts = map length loopsByComponent
   in withPointArray points $ \coordinates ->
        withArray (map fromIntegral loopPointCounts) $ \loopCounts ->
          withArray (map fromIntegral componentLoopCounts) $ \componentCounts ->
            produceHandle
              "region create"
              ( regionCreateF64
                  coordinates
                  (fromIntegral (length points))
                  loopCounts
                  (fromIntegral (length loops))
                  componentCounts
                  (fromIntegral (length components))
              )

testRegionLayoutRefusal :: IO ()
testRegionLayoutRefusal =
  withPointArray [(0, 0), (1, 0), (1, 1), (0, 1)] $ \coordinates ->
    withArray [5] $ \loopCounts ->
      withArray [1] $ \componentCounts ->
        alloca $ \output ->
          alloca $ \obstruction -> do
            status <- regionCreateF64 coordinates 4 loopCounts 1 componentCounts 1 output obstruction
            requireStatus "invalid region layout" AbiStatusGeometryObstruction status obstruction
            refusal <- peek obstruction
            unless (obstructionCode refusal == obstructionCodeId ObstructionRegionLayoutInvalid) $
              fail "invalid region layout lost its typed obstruction"

testTypedCoordinateRefusal :: IO ()
testTypedCoordinateRefusal =
  withPointArray [(0, 0), (0 / 0, 1), (1, 0)] $ \coordinates ->
    alloca $ \output ->
      alloca $ \obstruction -> do
        status <- delaunayF64 coordinates 3 output obstruction
        requireStatus "invalid coordinate" AbiStatusGeometryObstruction status obstruction
        refusal <- peek obstruction
        unless
          ( obstructionCode refusal == obstructionCodeId ObstructionInvalidCoordinate
              && obstructionCoordinateError refusal == coordinateErrorCodeId CoordinateErrorNaN
              && obstructionInputIndex refusal == 1
          ) $
          fail "invalid coordinate lost its typed witness"

testNullPointerRefusal :: IO ()
testNullPointerRefusal =
  alloca $ \count ->
    alloca $ \obstruction -> do
      status <- meshVertexCount nullPtr count obstruction
      requireStatus "null mesh" AbiStatusNullPointer status obstruction
      refusal <- peek obstruction
      unless (obstructionCode refusal == obstructionCodeId ObstructionNullPointer) $
        fail "null pointer refusal lost its typed witness"

testCoordinateCountOverflow :: IO ()
testCoordinateCountOverflow =
  alloca $ \output ->
    alloca $ \obstruction -> do
      let overflowingCount = fromIntegral (maxBound `div` 2 + 1 :: Int)
      status <- delaunayF64 nullPtr overflowingCount output obstruction
      requireStatus "coordinate count overflow" AbiStatusCountOverflow status obstruction
      refusal <- peek obstruction
      unless (obstructionCode refusal == obstructionCodeId ObstructionCountOverflow) $
        fail "coordinate count overflow lost its typed witness"

requireMeshCount :: String -> (MeshPointer -> Ptr CSize -> Ptr CObstruction -> IO CUInt) -> MeshPointer -> Int -> IO ()
requireMeshCount label operation mesh expected =
  alloca $ \count ->
    alloca $ \obstruction -> do
      status <- operation mesh count obstruction
      requireStatus label AbiStatusOk status obstruction
      observed <- peek count
      unless (observed == fromIntegral expected) $
        fail (label <> " produced " <> show observed <> ", expected " <> show expected)

produceHandle :: String -> (Ptr (Ptr carrier) -> Ptr CObstruction -> IO CUInt) -> IO (Ptr carrier)
produceHandle label operation =
  alloca $ \output ->
    alloca $ \obstruction -> do
      status <- operation output obstruction
      requireStatus label AbiStatusOk status obstruction
      handle <- peek output
      unless (handle /= nullPtr) (fail (label <> " returned a null handle"))
      pure handle

produceRegionWithReceipt
  :: String
  -> (Ptr RegionPointer -> Ptr CMinkowskiReceipt -> Ptr CObstruction -> IO CUInt)
  -> IO (RegionPointer, CMinkowskiReceipt)
produceRegionWithReceipt label operation =
  alloca $ \output ->
    alloca $ \receiptOutput ->
      alloca $ \obstruction -> do
        status <- operation output receiptOutput obstruction
        requireStatus label AbiStatusOk status obstruction
        handle <- peek output
        receipt <- peek receiptOutput
        unless (handle /= nullPtr) (fail (label <> " returned a null region"))
        pure (handle, receipt)

requireStatus :: String -> AbiStatus -> CUInt -> Ptr CObstruction -> IO ()
requireStatus label expected observed obstruction
  | observed == fromIntegral (abiStatusId expected) = pure ()
  | otherwise = do
      refusal <- peek obstruction
      fail (label <> " returned status " <> show observed <> ": " <> obstructionMessage refusal)

withPointArray :: [Point] -> (Ptr CDouble -> IO result) -> IO result
withPointArray points = withArray (concatMap (\(x, y) -> [CDouble x, CDouble y]) points)
