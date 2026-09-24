module Moonlight.Planar.Foreign.Morphology
  ( regionClose
  , regionInset
  , regionMinkowskiSum
  , regionOffset
  , regionOpen
  , structuringElementCreateF64
  , structuringElementFree
  ) where

import Data.Bifunctor (first)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Word (Word32)
import Foreign.C.Types (CDouble, CSize, CUInt)
import Foreign.Ptr (Ptr)
import Foreign.Storable (poke)
import qualified Data.Vector as V
import Moonlight.Planar.Convex qualified as T (convexPolygon)
import Moonlight.Planar.Exact qualified as T (exactPointFromPoint)
import Moonlight.Planar.Minkowski qualified as T (MinkowskiError (..), MinkowskiOperation (..), MinkowskiReceipt (..), StructuringElement, closeWith, erodeBy, minkowskiSum, openWith, polygonOffset, structuringElement)
import Moonlight.Planar.Region qualified as T (PlanarRegion)
import qualified Moonlight.Planar.Point as T (Point (..))
import Moonlight.Planar.Foreign.Boundary
  ( checkedCount
  , dereferenceHandle
  , freeHandle
  , prepareHandleOutput
  , produceHandle
  , publishHandle
  , readPoints
  , requirePointer
  , runBoundary
  )
import Moonlight.Planar.Foreign.Contract
  ( CMinkowskiReceipt (..)
  , CObstruction
  , CRegion
  , CStructuringElement
  , MinkowskiOperationCode (..)
  , minkowskiOperationCodeId
  )
import Moonlight.Planar.Foreign.Obstruction
  ( AbiFailure
  , convexFailure
  , minkowskiFailure
  , pointInputFailure
  , structuringElementEmptyFailure
  )

structuringElementCreateF64
  :: Ptr CDouble
  -> CSize
  -> Ptr (Ptr CStructuringElement)
  -> Ptr CObstruction
  -> IO CUInt
structuringElementCreateF64 coordinates rawPointCount output obstructionPointer =
  runBoundary obstructionPointer $ produceHandle output $ do
    case checkedCount 2 rawPointCount of
      Left failure -> pure (Left failure)
      Right pointCount -> do
        points <- readPoints coordinates pointCount
        pure (points >>= buildStructuringElement)

buildStructuringElement
  :: V.Vector T.Point
  -> Either AbiFailure T.StructuringElement
buildStructuringElement points = do
  exactPoints <-
    V.imapM
      (\index point -> first (pointInputFailure index point) (T.exactPointFromPoint point))
      points
  submitted <-
    maybe
      (Left structuringElementEmptyFailure)
      Right
      (NonEmpty.nonEmpty (V.toList exactPoints))
  polygon <- first convexFailure (T.convexPolygon submitted)
  first minkowskiFailure (T.structuringElement polygon)

structuringElementFree :: Ptr CStructuringElement -> IO ()
structuringElementFree = freeHandle

regionMinkowskiSum
  :: Ptr CRegion
  -> Ptr CRegion
  -> Ptr (Ptr CRegion)
  -> Ptr CMinkowskiReceipt
  -> Ptr CObstruction
  -> IO CUInt
regionMinkowskiSum leftPointer rightPointer output receiptOutput obstructionPointer =
  runBoundary obstructionPointer $
    produceRegionWithReceipt output receiptOutput $ do
      case requirePointer "left region" leftPointer >> requirePointer "right region" rightPointer of
        Left failure -> pure (Left failure)
        Right () -> do
          left <- dereferenceHandle leftPointer
          right <- dereferenceHandle rightPointer
          pure (first minkowskiFailure (T.minkowskiSum left right))

regionOffset, regionInset, regionOpen, regionClose :: Ptr CStructuringElement -> Ptr CRegion -> Ptr (Ptr CRegion) -> Ptr CMinkowskiReceipt -> Ptr CObstruction -> IO CUInt
regionOffset = structuringElementOperation T.polygonOffset
regionInset = structuringElementOperation T.erodeBy
regionOpen = structuringElementOperation T.openWith
regionClose = structuringElementOperation T.closeWith

structuringElementOperation
  :: (T.StructuringElement -> T.PlanarRegion -> Either T.MinkowskiError (T.PlanarRegion, T.MinkowskiReceipt))
  -> Ptr CStructuringElement
  -> Ptr CRegion
  -> Ptr (Ptr CRegion)
  -> Ptr CMinkowskiReceipt
  -> Ptr CObstruction
  -> IO CUInt
structuringElementOperation operation elementPointer regionPointer output receiptOutput obstructionPointer =
  runBoundary obstructionPointer $
    produceRegionWithReceipt output receiptOutput $ do
      case requirePointer "structuring element" elementPointer >> requirePointer "region" regionPointer of
        Left failure -> pure (Left failure)
        Right () -> do
          element <- dereferenceHandle elementPointer
          region <- dereferenceHandle regionPointer
          pure (first minkowskiFailure (operation element region))

produceRegionWithReceipt
  :: Ptr (Ptr CRegion)
  -> Ptr CMinkowskiReceipt
  -> IO (Either AbiFailure (T.PlanarRegion, T.MinkowskiReceipt))
  -> IO (Either AbiFailure ())
produceRegionWithReceipt output receiptOutput obtain = do
  prepared <- prepareHandleOutput output
  case (prepared, requirePointer "receipt" receiptOutput) of
    (Left failure, _) -> pure (Left failure)
    (_, Left failure) -> pure (Left failure)
    (Right (), Right ()) -> do
      outcome <- obtain
      case outcome of
        Left failure -> pure (Left failure)
        Right (region, receipt) -> do
          poke receiptOutput (minkowskiReceiptProjection receipt)
          publishHandle output region
          pure (Right ())

minkowskiReceiptProjection :: T.MinkowskiReceipt -> CMinkowskiReceipt
minkowskiReceiptProjection receipt =
  CMinkowskiReceipt
    { receiptOperation = minkowskiOperationCode (T.minkowskiOperation receipt)
    , receiptInputComponents = fromIntegral (T.minkowskiInputComponents receipt)
    , receiptConvexPieces = fromIntegral (T.minkowskiConvexPieces receipt)
    , receiptGeneratedPieces = fromIntegral (T.minkowskiGeneratedPieces receipt)
    , receiptGeneratedConvolutionEdges = fromIntegral (T.minkowskiGeneratedConvolutionEdges receipt)
    , receiptOverlayPasses = fromIntegral (T.minkowskiOverlayPasses receipt)
    , receiptExactCrossings = fromIntegral (T.minkowskiExactCrossings receipt)
    , receiptOutputCells = fromIntegral (T.minkowskiOutputCells receipt)
    , receiptExactCoordinateBitGrowth = fromIntegral (T.minkowskiExactCoordinateBitGrowth receipt)
    }

minkowskiOperationCode :: T.MinkowskiOperation -> Word32
minkowskiOperationCode operation =
  minkowskiOperationCodeId
    ( case operation of
        T.MinkowskiAddition -> MinkowskiOperationAddition
        T.MinkowskiErosion -> MinkowskiOperationErosion
        T.MinkowskiOpening -> MinkowskiOperationOpening
        T.MinkowskiClosing -> MinkowskiOperationClosing
    )
