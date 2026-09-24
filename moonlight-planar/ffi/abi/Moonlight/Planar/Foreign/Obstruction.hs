module Moonlight.Planar.Foreign.Obstruction
  ( AbiFailure (..)
  , RegionCountKind (..)
  , RegionLayoutError (..)
  , bufferTooSmallFailure
  , buildFailure
  , countOverflowFailure
  , emptyObstruction
  , minkowskiFailure
  , convexFailure
  , nullPointerFailure
  , overlayFailure
  , pointInputFailure
  , projectionFailure
  , regionLayoutFailure
  , regionPublicationFailure
  , regionValidationFailure
  , runtimeFailure
  , statusOk
  , structuringElementEmptyFailure
  , valuationFailure
  ) where

import Control.Exception (SomeException, displayException)
import Data.Word (Word32, Word64)
import Foreign.C.Types (CUInt)
import Moonlight.Planar.Foreign.Contract
import Moonlight.Planar.Convex qualified as T (ConvexError (..))
import Moonlight.Planar.Handles.HandleDefs qualified as T (unDirectedEdgeId, unFaceId, unUndirectedEdgeId, unVertexId)
import Moonlight.Planar.Minkowski qualified as T (MinkowskiError (..))
import Moonlight.Planar.Overlay qualified as T (OverlayError (..))
import Moonlight.Planar.Region qualified as T (RegionPublicationError (..), RegionValidationError (..))
import qualified Moonlight.Planar.Types as T (BuildError (..))
import qualified Moonlight.Planar.Scalar as T (CoordinateError (..), NonFiniteValue (..))
import qualified Moonlight.Planar.Point as T (Point (..), PointValidationError (..))
import Moonlight.Planar.Valuation qualified as T (ValuationError (..))

data AbiFailure = AbiFailure !CUInt !CObstruction

statusCode :: AbiStatus -> CUInt
statusCode = fromIntegral . abiStatusId

statusOk :: CUInt
statusOk = statusCode AbiStatusOk

emptyObstruction :: CObstruction
emptyObstruction =
  CObstruction
    { obstructionCode = obstructionCodeId ObstructionNone
    , obstructionCoordinateError = coordinateErrorCodeId CoordinateErrorNone
    , obstructionInputIndex = maxBound
    , obstructionFirstIndex = 0
    , obstructionSecondIndex = 0
    , obstructionFirstValue = 0
    , obstructionSecondValue = 0
    , obstructionPointX = 0
    , obstructionPointY = 0
    , obstructionMessage = ""
    }

apiFailure :: AbiStatus -> ObstructionCode -> String -> AbiFailure
apiFailure status code message =
  AbiFailure
    (statusCode status)
    emptyObstruction
      { obstructionCode = obstructionCodeId code
      , obstructionMessage = message
      }

geometryFailure :: Show obstruction => ObstructionCode -> obstruction -> AbiFailure
geometryFailure code obstruction =
  apiFailure AbiStatusGeometryObstruction code (show obstruction)

nullPointerFailure :: String -> AbiFailure
nullPointerFailure label =
  apiFailure AbiStatusNullPointer ObstructionNullPointer (label <> " must not be null")

countOverflowFailure :: Word64 -> AbiFailure
countOverflowFailure count =
  AbiFailure
    (statusCode AbiStatusCountOverflow)
    emptyObstruction
      { obstructionCode = obstructionCodeId ObstructionCountOverflow
      , obstructionFirstIndex = count
      , obstructionMessage = "count exceeds the host Int range"
      }

bufferTooSmallFailure :: Int -> Int -> AbiFailure
bufferTooSmallFailure required capacity =
  AbiFailure
    (statusCode AbiStatusBufferTooSmall)
    emptyObstruction
      { obstructionCode = obstructionCodeId ObstructionBufferTooSmall
      , obstructionFirstIndex = fromIntegral required
      , obstructionSecondIndex = fromIntegral capacity
      , obstructionMessage = "output buffer is smaller than the required element count"
      }

runtimeFailure :: SomeException -> AbiFailure
runtimeFailure = apiFailure AbiStatusRuntimeFailure ObstructionRuntimeFailure . displayException

buildFailure :: T.BuildError -> AbiFailure
buildFailure obstruction =
  AbiFailure (statusCode AbiStatusGeometryObstruction) (buildErrorObstruction obstruction)

data RegionCountKind
  = LoopPointCounts
  | ComponentLoopCounts
  deriving stock (Eq, Show)

data RegionLayoutError
  = RegionGroupEmpty !RegionCountKind !Int
  | RegionCountTotalMismatch !RegionCountKind !Integer !Integer
  deriving stock (Eq, Show)

regionLayoutFailure :: RegionLayoutError -> AbiFailure
regionLayoutFailure layoutError =
  AbiFailure (statusCode AbiStatusGeometryObstruction) (layoutObstruction layoutError)

layoutObstruction :: RegionLayoutError -> CObstruction
layoutObstruction layoutError =
  ( case layoutError of
      RegionCountTotalMismatch _ actual expected ->
        base
          { obstructionFirstIndex = fromIntegral actual
          , obstructionSecondIndex = fromIntegral expected
          }
      RegionGroupEmpty _ index -> base {obstructionInputIndex = fromIntegral index}
  )
    {obstructionMessage = show layoutError}
 where
  base = emptyObstruction {obstructionCode = obstructionCodeId ObstructionRegionLayoutInvalid}

regionValidationFailure :: T.RegionValidationError -> AbiFailure
regionValidationFailure = geometryFailure ObstructionRegionValidationFailed

overlayFailure :: T.OverlayError -> AbiFailure
overlayFailure = geometryFailure ObstructionOverlayFailed

regionPublicationFailure :: T.RegionPublicationError -> AbiFailure
regionPublicationFailure = geometryFailure ObstructionRegionPublicationFailed

valuationFailure :: T.ValuationError -> AbiFailure
valuationFailure = geometryFailure ObstructionValuationFailed

minkowskiFailure :: T.MinkowskiError -> AbiFailure
minkowskiFailure = geometryFailure ObstructionMinkowskiFailed

convexFailure :: T.ConvexError -> AbiFailure
convexFailure = geometryFailure ObstructionConvexFailed

structuringElementEmptyFailure :: AbiFailure
structuringElementEmptyFailure =
  apiFailure
    AbiStatusGeometryObstruction
    ObstructionRegionLayoutInvalid
    "structuring element requires at least one point"

pointInputFailure :: Int -> T.Point -> T.PointValidationError -> AbiFailure
pointInputFailure index (T.Point x y) pointError =
  AbiFailure
    (statusCode AbiStatusGeometryObstruction)
    emptyObstruction
      { obstructionCode = obstructionCodeId ObstructionInvalidCoordinate
      , obstructionCoordinateError = coordinateErrorCode reason
      , obstructionInputIndex = fromIntegral index
      , obstructionFirstValue = invalidValue
      , obstructionPointX = x
      , obstructionPointY = y
      , obstructionMessage = show pointError
      }
 where
  (invalidValue, reason) =
    case pointError of
      T.InvalidPointX coordinateError -> (x, coordinateError)
      T.InvalidPointY coordinateError -> (y, coordinateError)

projectionFailure :: Int -> T.PointValidationError -> AbiFailure
projectionFailure index pointError =
  AbiFailure
    (statusCode AbiStatusGeometryObstruction)
    emptyObstruction
      { obstructionCode = obstructionCodeId ObstructionRegionProjectionFailed
      , obstructionCoordinateError =
          coordinateErrorCode
            ( case pointError of
                T.InvalidPointX coordinateError -> coordinateError
                T.InvalidPointY coordinateError -> coordinateError
            )
      , obstructionInputIndex = fromIntegral index
      , obstructionMessage = show pointError
      }

buildErrorObstruction :: T.BuildError -> CObstruction
buildErrorObstruction failure =
  ( case failure of
      T.InvalidCoordinate inputIndex value reason ->
        emptyObstruction
          { obstructionCode = obstructionCodeId ObstructionInvalidCoordinate
          , obstructionCoordinateError = coordinateErrorCode reason
          , obstructionInputIndex = maybe maxBound fromIntegral inputIndex
          , obstructionFirstValue = value
          }
      T.PointLocationFailed (T.Point x y) -> pointObstruction ObstructionPointLocationFailed x y
      T.LocationWalkExhausted (T.Point x y) steps ->
        (pointObstruction ObstructionLocationWalkExhausted x y) {obstructionFirstIndex = fromIntegral steps}
      T.RefinementInputTopologyInvalid _ -> codeOnly ObstructionRefinementInputTopologyInvalid
      T.FreshInsertionMatchedExistingVertex firstVertex secondVertex -> indices ObstructionFreshInsertionMatchedExistingVertex (T.unVertexId firstVertex) (T.unVertexId secondVertex)
      T.DegenerateLineEndpointMissingOutgoing vertex -> firstIndex ObstructionDegenerateLineEndpointMissingOutgoing (T.unVertexId vertex)
      T.DegenerateLineEndpointTurnMissing index -> firstIndex ObstructionDegenerateLineEndpointTurnMissing index
      T.DegenerateLineConnectedVertexMissing index -> firstIndex ObstructionDegenerateLineConnectedVertexMissing index
      T.HullStartNotVisible edge -> firstIndex ObstructionHullStartNotVisible (T.unDirectedEdgeId edge)
      T.OuterRangeDidNotTerminate firstEdge secondEdge steps ->
        (indices ObstructionOuterRangeDidNotTerminate (T.unDirectedEdgeId firstEdge) (T.unDirectedEdgeId secondEdge))
          {obstructionFirstValue = fromIntegral steps}
      T.OuterRangeContainsInnerEdge edge face -> indices ObstructionOuterRangeContainsInnerEdge (T.unDirectedEdgeId edge) (T.unFaceId face)
      T.ConstrainedEdgeFlipRefused edge -> firstIndex ObstructionConstrainedEdgeFlipRefused (T.unUndirectedEdgeId edge)
      T.RemovalVertexOutOfRange vertex count -> indices ObstructionRemovalVertexOutOfRange (T.unVertexId vertex) count
      T.RemovalEdgeOutOfRange edge count -> indices ObstructionRemovalEdgeOutOfRange (T.unUndirectedEdgeId edge) count
      T.RemovalFaceOutOfRange face count -> indices ObstructionRemovalFaceOutOfRange (T.unFaceId face) count
      T.RemovalFaceCycleDidNotTerminate face edge steps ->
        (indices ObstructionRemovalFaceCycleDidNotTerminate (T.unFaceId face) (T.unDirectedEdgeId edge))
          {obstructionFirstValue = fromIntegral steps}
      T.RemovalEmptyTriangulation vertex -> firstIndex ObstructionRemovalEmptyTriangulation (T.unVertexId vertex)
      T.RemovalTwoPointDegreeMismatch vertex degree -> indices ObstructionRemovalTwoPointDegreeMismatch (T.unVertexId vertex) degree
      T.RemovalCollinearDegreeMismatch vertex degree -> indices ObstructionRemovalCollinearDegreeMismatch (T.unVertexId vertex) degree
      T.RemovalBorderTooShort count -> firstIndex ObstructionRemovalBorderTooShort count
      T.RemovalBorderArityMismatch count -> firstIndex ObstructionRemovalBorderArityMismatch count
      T.RemovalOutgoingCycleDidNotTerminate vertex edge steps ->
        (indices ObstructionRemovalOutgoingCycleDidNotTerminate (T.unVertexId vertex) (T.unDirectedEdgeId edge))
          {obstructionFirstValue = fromIntegral steps}
      T.CircleSweepHullEmpty -> codeOnly ObstructionCircleSweepHullEmpty
      T.OuterCycleDidNotTerminate firstEdge secondEdge steps ->
        (indices ObstructionOuterCycleDidNotTerminate (T.unDirectedEdgeId firstEdge) (T.unDirectedEdgeId secondEdge))
          {obstructionFirstValue = fromIntegral steps}
      T.HierarchyLevelPopulationMismatch level expected observed ->
        (indices ObstructionHierarchyLevelPopulationMismatch expected observed) {obstructionFirstValue = fromIntegral level}
      T.HierarchyInsertionHandleMismatch expected observed -> indices ObstructionHierarchyInsertionHandleMismatch (T.unVertexId expected) (T.unVertexId observed)
      T.PointIndexCapacityExhausted count -> firstIndex ObstructionPointIndexCapacityExhausted count
      T.RefinementMinimumAngleNotFinite value -> nonFinite ObstructionRefinementMinimumAngleNotFinite value
      T.RefinementMinimumAngleOutOfRange value -> firstValue ObstructionRefinementMinimumAngleOutOfRange value
      T.RefinementMinimumAngleDerivedRatioNotFinite value -> nonFinite ObstructionRefinementMinimumAngleDerivedRatioNotFinite value
      T.RefinementMaximumAdditionalVerticesNegative value -> firstValue ObstructionRefinementMaximumAdditionalVerticesNegative (fromIntegral value)
      T.RefinementMinimumAreaNotFinite value -> nonFinite ObstructionRefinementMinimumAreaNotFinite value
      T.RefinementMinimumAreaNegative value -> firstValue ObstructionRefinementMinimumAreaNegative value
      T.RefinementMaximumAreaNotFinite value -> nonFinite ObstructionRefinementMaximumAreaNotFinite value
      T.RefinementMaximumAreaNotPositive value -> firstValue ObstructionRefinementMaximumAreaNotPositive value
      T.RefinementMaximumRadiusEdgeRatioNotFinite value -> nonFinite ObstructionRefinementMaximumRadiusEdgeRatioNotFinite value
      T.RefinementMaximumRadiusEdgeRatioNotPositive value -> firstValue ObstructionRefinementMaximumRadiusEdgeRatioNotPositive value
      T.RefinementMaximumEdgeLengthNotFinite value -> nonFinite ObstructionRefinementMaximumEdgeLengthNotFinite value
      T.RefinementMaximumEdgeLengthNotPositive value -> firstValue ObstructionRefinementMaximumEdgeLengthNotPositive value
      T.RefinementMinimumAreaExceedsMaximum minimumArea maximumArea -> values ObstructionRefinementMinimumAreaExceedsMaximum minimumArea maximumArea
      T.RefinementSeedFaceNotActive face count -> indices ObstructionRefinementSeedFaceNotActive (T.unFaceId face) count
      T.RefinementDomainTopologyChanged -> codeOnly ObstructionRefinementDomainTopologyChanged
      T.RefinementDomainRequiresConvexHullPreservation -> codeOnly ObstructionRefinementDomainRequiresConvexHullPreservation
      T.RefinementDomainRequiresConstraintPreservation -> codeOnly ObstructionRefinementDomainRequiresConstraintPreservation
      T.RefinementDomainForbidsOuterFaceExclusion -> codeOnly ObstructionRefinementDomainForbidsOuterFaceExclusion
      T.RefinementDomainWouldCrossInterface edge face -> indices ObstructionRefinementDomainWouldCrossInterface (T.unUndirectedEdgeId edge) (T.unFaceId face)
      T.RefinementDomainWouldRewriteProtectedFace face -> firstIndex ObstructionRefinementDomainWouldRewriteProtectedFace (T.unFaceId face)
      T.RefinementDomainProtectedFaceChanged face -> firstIndex ObstructionRefinementDomainProtectedFaceChanged (T.unFaceId face)
      T.RefinementDomainInterfaceOppositeFaceNotPermitted edge face ->
        indices ObstructionRefinementDomainInterfaceOppositeFaceNotPermitted (T.unUndirectedEdgeId edge) (T.unFaceId face)
      T.RefinementOversizedEdge face edge actual bound ->
        (indices ObstructionRefinementOversizedEdge (T.unFaceId face) (T.unUndirectedEdgeId edge))
          { obstructionFirstValue = actual
          , obstructionSecondValue = bound
          }
      T.CapacityExceeded count -> firstIndex ObstructionCapacityExceeded count
      T.HalfEdgeCapacityExceeded requested capacity -> indices ObstructionHalfEdgeCapacityExceeded requested capacity
      T.FaceCapacityExceeded requested capacity -> indices ObstructionFaceCapacityExceeded requested capacity
      T.PayloadStorageFailure _ -> codeOnly ObstructionPayloadStorageFailure
      T.CoordinatePayloadCountMismatch coordinates payloads -> indices ObstructionCoordinatePayloadCountMismatch coordinates payloads
      T.CircleSweepRequiresDenseStorage -> codeOnly ObstructionCircleSweepRequiresDenseStorage
      T.SeamFrontierUnavailable -> codeOnly ObstructionSeamFrontierUnavailable
      T.RefinementDomainRequiresFiniteVertexBudget -> codeOnly ObstructionRefinementDomainRequiresFiniteVertexBudget
      T.SeamSourceEdgeRequiresFlip edge -> firstIndex ObstructionSeamSourceEdgeRequiresFlip (T.unUndirectedEdgeId edge)
      T.RefinementSeamBridgeBudgetExceeded required available -> indices ObstructionRefinementSeamBridgeBudgetExceeded required available
      T.RefinementSeamBridgeMidpointCollapsed edge -> firstIndex ObstructionRefinementSeamBridgeMidpointCollapsed (T.unUndirectedEdgeId edge)
      T.BoundarySplitRequiresBoundaryEdge edge -> firstIndex ObstructionBoundarySplitRequiresBoundaryEdge (T.unUndirectedEdgeId edge)
  )
    {obstructionMessage = show failure}
 where
  codeOnly :: ObstructionCode -> CObstruction
  codeOnly code = emptyObstruction {obstructionCode = obstructionCodeId code}
  firstIndex :: Integral index => ObstructionCode -> index -> CObstruction
  firstIndex code index = (codeOnly code) {obstructionFirstIndex = fromIntegral index}
  indices :: (Integral first, Integral second) => ObstructionCode -> first -> second -> CObstruction
  indices code firstIndexValue secondIndexValue =
    (codeOnly code)
      { obstructionFirstIndex = fromIntegral firstIndexValue
      , obstructionSecondIndex = fromIntegral secondIndexValue
      }
  firstValue :: ObstructionCode -> Double -> CObstruction
  firstValue code value = (codeOnly code) {obstructionFirstValue = value}
  values :: ObstructionCode -> Double -> Double -> CObstruction
  values code firstCoordinateValue secondCoordinateValue =
    (codeOnly code)
      { obstructionFirstValue = firstCoordinateValue
      , obstructionSecondValue = secondCoordinateValue
      }
  pointObstruction :: ObstructionCode -> Double -> Double -> CObstruction
  pointObstruction code x y =
    (codeOnly code)
      { obstructionPointX = x
      , obstructionPointY = y
      }
  nonFinite :: ObstructionCode -> T.NonFiniteValue -> CObstruction
  nonFinite code value = firstIndex code (nonFiniteCode value)

coordinateErrorCode :: T.CoordinateError -> Word32
coordinateErrorCode reason =
  coordinateErrorCodeId
    ( case reason of
        T.CoordinateNaN -> CoordinateErrorNaN
        T.CoordinateInfinite -> CoordinateErrorInfinite
        T.CoordinateTooSmall -> CoordinateErrorTooSmall
        T.CoordinateTooLarge -> CoordinateErrorTooLarge
    )

nonFiniteCode :: T.NonFiniteValue -> Word32
nonFiniteCode value =
  case value of
    T.ValueNaN -> 1
    T.ValuePositiveInfinity -> 2
    T.ValueNegativeInfinity -> 3
