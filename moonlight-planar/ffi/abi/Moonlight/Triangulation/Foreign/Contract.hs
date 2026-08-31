{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Moonlight.Triangulation.Foreign.Contract
  ( AbiField (..)
  , AbiFieldType (..)
  , AbiFunction
  , AbiFunctionFamily (..)
  , AbiFunctionImplementation (..)
  , AbiFunctionName (..)
  , AbiHandle (..)
  , AbiParameter (..)
  , AbiParameterKind (..)
  , AbiResult (..)
  , AbiStatus (..)
  , AbiStruct (..)
  , CMesh
  , CMinkowskiReceipt (..)
  , CObstruction (..)
  , CRegion
  , CStructuringElement
  , CoordinateErrorCode (..)
  , MinkowskiOperationCode (..)
  , ObstructionCode (..)
  , RegionLocationCode (..)
  , abiFunction
  , abiFunctionImplementation
  , abiFunctionParameters
  , abiFunctionResult
  , abiFunctionSymbol
  , abiHandleSymbol
  , abiStatusId
  , abiStatusSymbol
  , abiStructFields
  , abiStructSymbol
  , abiVersion
  , allAbiFunctionNames
  , allAbiFunctions
  , allAbiHandles
  , allAbiStatuses
  , allAbiStructs
  , allCoordinateErrorCodes
  , allMinkowskiOperationCodes
  , allObstructionCodes
  , allRegionLocationCodes
  , coordinateErrorCodeId
  , coordinateErrorCodeSymbol
  , minkowskiOperationCodeId
  , minkowskiOperationCodeSymbol
  , obstructionCodeId
  , obstructionCodeSymbol
  , regionLocationCodeId
  , regionLocationCodeSymbol
  ) where

import Data.Word (Word32, Word64)
import Foreign.C.String (peekCString, withCStringLen)
import Foreign.C.Types (CChar)
import Foreign.Marshal.Utils (copyBytes, fillBytes)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (Storable (..))
import GHC.Generics (Generic)

abiVersion :: Word32
abiVersion = 2

data AbiStatus
  = AbiStatusOk
  | AbiStatusNullPointer
  | AbiStatusCountOverflow
  | AbiStatusBufferTooSmall
  | AbiStatusGeometryObstruction
  | AbiStatusRuntimeFailure
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

allAbiStatuses :: [AbiStatus]
allAbiStatuses = [minBound .. maxBound]

abiStatusId :: AbiStatus -> Word32
abiStatusId status =
  case status of
    AbiStatusOk -> 0
    AbiStatusNullPointer -> 1
    AbiStatusCountOverflow -> 2
    AbiStatusBufferTooSmall -> 3
    AbiStatusGeometryObstruction -> 4
    AbiStatusRuntimeFailure -> 5

abiStatusSymbol :: AbiStatus -> String
abiStatusSymbol status =
  case status of
    AbiStatusOk -> "ML_STATUS_OK"
    AbiStatusNullPointer -> "ML_STATUS_NULL_POINTER"
    AbiStatusCountOverflow -> "ML_STATUS_COUNT_OVERFLOW"
    AbiStatusBufferTooSmall -> "ML_STATUS_BUFFER_TOO_SMALL"
    AbiStatusGeometryObstruction -> "ML_STATUS_GEOMETRY_OBSTRUCTION"
    AbiStatusRuntimeFailure -> "ML_STATUS_RUNTIME_FAILURE"

data ObstructionCode
  = ObstructionNone
  | ObstructionInvalidCoordinate
  | ObstructionPointLocationFailed
  | ObstructionLocationWalkExhausted
  | ObstructionRefinementInputTopologyInvalid
  | ObstructionFreshInsertionMatchedExistingVertex
  | ObstructionDegenerateLineEndpointMissingOutgoing
  | ObstructionDegenerateLineEndpointTurnMissing
  | ObstructionDegenerateLineConnectedVertexMissing
  | ObstructionHullStartNotVisible
  | ObstructionOuterRangeDidNotTerminate
  | ObstructionOuterRangeContainsInnerEdge
  | ObstructionConstrainedEdgeFlipRefused
  | ObstructionRemovalVertexOutOfRange
  | ObstructionRemovalEdgeOutOfRange
  | ObstructionRemovalFaceOutOfRange
  | ObstructionRemovalFaceCycleDidNotTerminate
  | ObstructionRemovalEmptyTriangulation
  | ObstructionRemovalTwoPointDegreeMismatch
  | ObstructionRemovalCollinearDegreeMismatch
  | ObstructionRemovalBorderTooShort
  | ObstructionRemovalBorderArityMismatch
  | ObstructionRemovalOutgoingCycleDidNotTerminate
  | ObstructionCircleSweepHullEmpty
  | ObstructionOuterCycleDidNotTerminate
  | ObstructionHierarchyLevelPopulationMismatch
  | ObstructionHierarchyInsertionHandleMismatch
  | ObstructionPointIndexCapacityExhausted
  | ObstructionRefinementMinimumAngleNotFinite
  | ObstructionRefinementMinimumAngleOutOfRange
  | ObstructionRefinementMinimumAngleDerivedRatioNotFinite
  | ObstructionRefinementMaximumAdditionalVerticesNegative
  | ObstructionRefinementMinimumAreaNotFinite
  | ObstructionRefinementMinimumAreaNegative
  | ObstructionRefinementMaximumAreaNotFinite
  | ObstructionRefinementMaximumAreaNotPositive
  | ObstructionRefinementMaximumRadiusEdgeRatioNotFinite
  | ObstructionRefinementMaximumRadiusEdgeRatioNotPositive
  | ObstructionRefinementMinimumAreaExceedsMaximum
  | ObstructionRefinementSeedFaceNotActive
  | ObstructionRefinementDomainInterfaceEdgeNotActive
  | ObstructionRefinementDomainInterfaceMissing
  | ObstructionRefinementDomainInterfaceExtraneous
  | ObstructionRefinementDomainTopologyChanged
  | ObstructionRefinementDomainRequiresConvexHullPreservation
  | ObstructionRefinementDomainRequiresConstraintPreservation
  | ObstructionRefinementDomainForbidsOuterFaceExclusion
  | ObstructionRefinementDomainWouldCrossInterface
  | ObstructionRefinementDomainWouldRewriteProtectedFace
  | ObstructionRefinementDomainProtectedFaceChanged
  | ObstructionCapacityExceeded
  | ObstructionHalfEdgeCapacityExceeded
  | ObstructionFaceCapacityExceeded
  | ObstructionPayloadStorageFailure
  | ObstructionCoordinatePayloadCountMismatch
  | ObstructionCircleSweepRequiresDenseStorage
  | ObstructionSeamFrontierUnavailable
  | ObstructionRefinementDomainRequiresFiniteVertexBudget
  | ObstructionSeamSourceEdgeRequiresFlip
  | ObstructionSeamProtectedSourceFaceChanged
  | ObstructionRefinementMaximumEdgeLengthNotFinite
  | ObstructionRefinementMaximumEdgeLengthNotPositive
  | ObstructionRefinementOversizedEdge
  | ObstructionRefinementSeamBridgeBudgetExceeded
  | ObstructionRefinementSeamBridgeMidpointCollapsed
  | ObstructionBoundarySplitRequiresBoundaryEdge
  | ObstructionRefinementDomainInterfaceOppositeFaceNotPermitted
  | ObstructionNullPointer
  | ObstructionCountOverflow
  | ObstructionBufferTooSmall
  | ObstructionRuntimeFailure
  | ObstructionRegionLayoutInvalid
  | ObstructionRegionValidationFailed
  | ObstructionOverlayFailed
  | ObstructionRegionPublicationFailed
  | ObstructionValuationFailed
  | ObstructionMinkowskiFailed
  | ObstructionRegionProjectionFailed
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

allObstructionCodes :: [ObstructionCode]
allObstructionCodes = [minBound .. maxBound]

obstructionCodeId :: ObstructionCode -> Word32
obstructionCodeId code = fst (obstructionCodeMetadata code)

obstructionCodeSymbol :: ObstructionCode -> String
obstructionCodeSymbol code = snd (obstructionCodeMetadata code)

obstructionCodeMetadata :: ObstructionCode -> (Word32, String)
obstructionCodeMetadata code =
  case code of
    ObstructionNone -> (0, "ML_OBSTRUCTION_NONE")
    ObstructionInvalidCoordinate -> (1, "ML_OBSTRUCTION_INVALID_COORDINATE")
    ObstructionPointLocationFailed -> (2, "ML_OBSTRUCTION_POINT_LOCATION_FAILED")
    ObstructionLocationWalkExhausted -> (3, "ML_OBSTRUCTION_LOCATION_WALK_EXHAUSTED")
    ObstructionRefinementInputTopologyInvalid -> (4, "ML_OBSTRUCTION_REFINEMENT_INPUT_TOPOLOGY_INVALID")
    ObstructionFreshInsertionMatchedExistingVertex -> (5, "ML_OBSTRUCTION_FRESH_INSERTION_MATCHED_EXISTING_VERTEX")
    ObstructionDegenerateLineEndpointMissingOutgoing -> (6, "ML_OBSTRUCTION_DEGENERATE_LINE_ENDPOINT_MISSING_OUTGOING")
    ObstructionDegenerateLineEndpointTurnMissing -> (7, "ML_OBSTRUCTION_DEGENERATE_LINE_ENDPOINT_TURN_MISSING")
    ObstructionDegenerateLineConnectedVertexMissing -> (8, "ML_OBSTRUCTION_DEGENERATE_LINE_CONNECTED_VERTEX_MISSING")
    ObstructionHullStartNotVisible -> (9, "ML_OBSTRUCTION_HULL_START_NOT_VISIBLE")
    ObstructionOuterRangeDidNotTerminate -> (10, "ML_OBSTRUCTION_OUTER_RANGE_DID_NOT_TERMINATE")
    ObstructionOuterRangeContainsInnerEdge -> (11, "ML_OBSTRUCTION_OUTER_RANGE_CONTAINS_INNER_EDGE")
    ObstructionConstrainedEdgeFlipRefused -> (12, "ML_OBSTRUCTION_CONSTRAINED_EDGE_FLIP_REFUSED")
    ObstructionRemovalVertexOutOfRange -> (13, "ML_OBSTRUCTION_REMOVAL_VERTEX_OUT_OF_RANGE")
    ObstructionRemovalEdgeOutOfRange -> (14, "ML_OBSTRUCTION_REMOVAL_EDGE_OUT_OF_RANGE")
    ObstructionRemovalFaceOutOfRange -> (15, "ML_OBSTRUCTION_REMOVAL_FACE_OUT_OF_RANGE")
    ObstructionRemovalFaceCycleDidNotTerminate -> (16, "ML_OBSTRUCTION_REMOVAL_FACE_CYCLE_DID_NOT_TERMINATE")
    ObstructionRemovalEmptyTriangulation -> (17, "ML_OBSTRUCTION_REMOVAL_EMPTY_TRIANGULATION")
    ObstructionRemovalTwoPointDegreeMismatch -> (18, "ML_OBSTRUCTION_REMOVAL_TWO_POINT_DEGREE_MISMATCH")
    ObstructionRemovalCollinearDegreeMismatch -> (19, "ML_OBSTRUCTION_REMOVAL_COLLINEAR_DEGREE_MISMATCH")
    ObstructionRemovalBorderTooShort -> (20, "ML_OBSTRUCTION_REMOVAL_BORDER_TOO_SHORT")
    ObstructionRemovalBorderArityMismatch -> (21, "ML_OBSTRUCTION_REMOVAL_BORDER_ARITY_MISMATCH")
    ObstructionRemovalOutgoingCycleDidNotTerminate -> (22, "ML_OBSTRUCTION_REMOVAL_OUTGOING_CYCLE_DID_NOT_TERMINATE")
    ObstructionCircleSweepHullEmpty -> (23, "ML_OBSTRUCTION_CIRCLE_SWEEP_HULL_EMPTY")
    ObstructionOuterCycleDidNotTerminate -> (24, "ML_OBSTRUCTION_OUTER_CYCLE_DID_NOT_TERMINATE")
    ObstructionHierarchyLevelPopulationMismatch -> (25, "ML_OBSTRUCTION_HIERARCHY_LEVEL_POPULATION_MISMATCH")
    ObstructionHierarchyInsertionHandleMismatch -> (26, "ML_OBSTRUCTION_HIERARCHY_INSERTION_HANDLE_MISMATCH")
    ObstructionPointIndexCapacityExhausted -> (27, "ML_OBSTRUCTION_POINT_INDEX_CAPACITY_EXHAUSTED")
    ObstructionRefinementMinimumAngleNotFinite -> (28, "ML_OBSTRUCTION_REFINEMENT_MINIMUM_ANGLE_NOT_FINITE")
    ObstructionRefinementMinimumAngleOutOfRange -> (29, "ML_OBSTRUCTION_REFINEMENT_MINIMUM_ANGLE_OUT_OF_RANGE")
    ObstructionRefinementMinimumAngleDerivedRatioNotFinite -> (30, "ML_OBSTRUCTION_REFINEMENT_MINIMUM_ANGLE_DERIVED_RATIO_NOT_FINITE")
    ObstructionRefinementMaximumAdditionalVerticesNegative -> (31, "ML_OBSTRUCTION_REFINEMENT_MAXIMUM_ADDITIONAL_VERTICES_NEGATIVE")
    ObstructionRefinementMinimumAreaNotFinite -> (32, "ML_OBSTRUCTION_REFINEMENT_MINIMUM_AREA_NOT_FINITE")
    ObstructionRefinementMinimumAreaNegative -> (33, "ML_OBSTRUCTION_REFINEMENT_MINIMUM_AREA_NEGATIVE")
    ObstructionRefinementMaximumAreaNotFinite -> (34, "ML_OBSTRUCTION_REFINEMENT_MAXIMUM_AREA_NOT_FINITE")
    ObstructionRefinementMaximumAreaNotPositive -> (35, "ML_OBSTRUCTION_REFINEMENT_MAXIMUM_AREA_NOT_POSITIVE")
    ObstructionRefinementMaximumRadiusEdgeRatioNotFinite -> (36, "ML_OBSTRUCTION_REFINEMENT_MAXIMUM_RADIUS_EDGE_RATIO_NOT_FINITE")
    ObstructionRefinementMaximumRadiusEdgeRatioNotPositive -> (37, "ML_OBSTRUCTION_REFINEMENT_MAXIMUM_RADIUS_EDGE_RATIO_NOT_POSITIVE")
    ObstructionRefinementMinimumAreaExceedsMaximum -> (38, "ML_OBSTRUCTION_REFINEMENT_MINIMUM_AREA_EXCEEDS_MAXIMUM")
    ObstructionRefinementSeedFaceNotActive -> (39, "ML_OBSTRUCTION_REFINEMENT_SEED_FACE_NOT_ACTIVE")
    ObstructionRefinementDomainInterfaceEdgeNotActive -> (40, "ML_OBSTRUCTION_REFINEMENT_DOMAIN_INTERFACE_EDGE_NOT_ACTIVE")
    ObstructionRefinementDomainInterfaceMissing -> (41, "ML_OBSTRUCTION_REFINEMENT_DOMAIN_INTERFACE_MISSING")
    ObstructionRefinementDomainInterfaceExtraneous -> (42, "ML_OBSTRUCTION_REFINEMENT_DOMAIN_INTERFACE_EXTRANEOUS")
    ObstructionRefinementDomainTopologyChanged -> (43, "ML_OBSTRUCTION_REFINEMENT_DOMAIN_TOPOLOGY_CHANGED")
    ObstructionRefinementDomainRequiresConvexHullPreservation -> (44, "ML_OBSTRUCTION_REFINEMENT_DOMAIN_REQUIRES_CONVEX_HULL_PRESERVATION")
    ObstructionRefinementDomainRequiresConstraintPreservation -> (45, "ML_OBSTRUCTION_REFINEMENT_DOMAIN_REQUIRES_CONSTRAINT_PRESERVATION")
    ObstructionRefinementDomainForbidsOuterFaceExclusion -> (46, "ML_OBSTRUCTION_REFINEMENT_DOMAIN_FORBIDS_OUTER_FACE_EXCLUSION")
    ObstructionRefinementDomainWouldCrossInterface -> (47, "ML_OBSTRUCTION_REFINEMENT_DOMAIN_WOULD_CROSS_INTERFACE")
    ObstructionRefinementDomainWouldRewriteProtectedFace -> (48, "ML_OBSTRUCTION_REFINEMENT_DOMAIN_WOULD_REWRITE_PROTECTED_FACE")
    ObstructionRefinementDomainProtectedFaceChanged -> (49, "ML_OBSTRUCTION_REFINEMENT_DOMAIN_PROTECTED_FACE_CHANGED")
    ObstructionCapacityExceeded -> (50, "ML_OBSTRUCTION_CAPACITY_EXCEEDED")
    ObstructionHalfEdgeCapacityExceeded -> (51, "ML_OBSTRUCTION_HALF_EDGE_CAPACITY_EXCEEDED")
    ObstructionFaceCapacityExceeded -> (52, "ML_OBSTRUCTION_FACE_CAPACITY_EXCEEDED")
    ObstructionPayloadStorageFailure -> (53, "ML_OBSTRUCTION_PAYLOAD_STORAGE_FAILURE")
    ObstructionCoordinatePayloadCountMismatch -> (54, "ML_OBSTRUCTION_COORDINATE_PAYLOAD_COUNT_MISMATCH")
    ObstructionCircleSweepRequiresDenseStorage -> (55, "ML_OBSTRUCTION_CIRCLE_SWEEP_REQUIRES_DENSE_STORAGE")
    ObstructionSeamFrontierUnavailable -> (56, "ML_OBSTRUCTION_SEAM_FRONTIER_UNAVAILABLE")
    ObstructionRefinementDomainRequiresFiniteVertexBudget -> (57, "ML_OBSTRUCTION_REFINEMENT_DOMAIN_REQUIRES_FINITE_VERTEX_BUDGET")
    ObstructionSeamSourceEdgeRequiresFlip -> (58, "ML_OBSTRUCTION_SEAM_SOURCE_EDGE_REQUIRES_FLIP")
    ObstructionSeamProtectedSourceFaceChanged -> (59, "ML_OBSTRUCTION_SEAM_PROTECTED_SOURCE_FACE_CHANGED")
    ObstructionRefinementMaximumEdgeLengthNotFinite -> (60, "ML_OBSTRUCTION_REFINEMENT_MAXIMUM_EDGE_LENGTH_NOT_FINITE")
    ObstructionRefinementMaximumEdgeLengthNotPositive -> (61, "ML_OBSTRUCTION_REFINEMENT_MAXIMUM_EDGE_LENGTH_NOT_POSITIVE")
    ObstructionRefinementOversizedEdge -> (62, "ML_OBSTRUCTION_REFINEMENT_OVERSIZED_EDGE")
    ObstructionRefinementSeamBridgeBudgetExceeded -> (63, "ML_OBSTRUCTION_REFINEMENT_SEAM_BRIDGE_BUDGET_EXCEEDED")
    ObstructionRefinementSeamBridgeMidpointCollapsed -> (64, "ML_OBSTRUCTION_REFINEMENT_SEAM_BRIDGE_MIDPOINT_COLLAPSED")
    ObstructionBoundarySplitRequiresBoundaryEdge -> (65, "ML_OBSTRUCTION_BOUNDARY_SPLIT_REQUIRES_BOUNDARY_EDGE")
    ObstructionRefinementDomainInterfaceOppositeFaceNotPermitted -> (66, "ML_OBSTRUCTION_REFINEMENT_DOMAIN_INTERFACE_OPPOSITE_FACE_NOT_PERMITTED")
    ObstructionNullPointer -> (100, "ML_OBSTRUCTION_NULL_POINTER")
    ObstructionCountOverflow -> (101, "ML_OBSTRUCTION_COUNT_OVERFLOW")
    ObstructionBufferTooSmall -> (102, "ML_OBSTRUCTION_BUFFER_TOO_SMALL")
    ObstructionRuntimeFailure -> (103, "ML_OBSTRUCTION_RUNTIME_FAILURE")
    ObstructionRegionLayoutInvalid -> (200, "ML_OBSTRUCTION_REGION_LAYOUT_INVALID")
    ObstructionRegionValidationFailed -> (201, "ML_OBSTRUCTION_REGION_VALIDATION_FAILED")
    ObstructionOverlayFailed -> (202, "ML_OBSTRUCTION_OVERLAY_FAILED")
    ObstructionRegionPublicationFailed -> (203, "ML_OBSTRUCTION_REGION_PUBLICATION_FAILED")
    ObstructionValuationFailed -> (204, "ML_OBSTRUCTION_VALUATION_FAILED")
    ObstructionMinkowskiFailed -> (205, "ML_OBSTRUCTION_MINKOWSKI_FAILED")
    ObstructionRegionProjectionFailed -> (206, "ML_OBSTRUCTION_REGION_PROJECTION_FAILED")

data CoordinateErrorCode
  = CoordinateErrorNone
  | CoordinateErrorNaN
  | CoordinateErrorInfinite
  | CoordinateErrorTooSmall
  | CoordinateErrorTooLarge
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

allCoordinateErrorCodes :: [CoordinateErrorCode]
allCoordinateErrorCodes = [minBound .. maxBound]

coordinateErrorCodeId :: CoordinateErrorCode -> Word32
coordinateErrorCodeId code =
  case code of
    CoordinateErrorNone -> 0
    CoordinateErrorNaN -> 1
    CoordinateErrorInfinite -> 2
    CoordinateErrorTooSmall -> 3
    CoordinateErrorTooLarge -> 4

coordinateErrorCodeSymbol :: CoordinateErrorCode -> String
coordinateErrorCodeSymbol code =
  case code of
    CoordinateErrorNone -> "ML_COORDINATE_ERROR_NONE"
    CoordinateErrorNaN -> "ML_COORDINATE_ERROR_NAN"
    CoordinateErrorInfinite -> "ML_COORDINATE_ERROR_INFINITE"
    CoordinateErrorTooSmall -> "ML_COORDINATE_ERROR_TOO_SMALL"
    CoordinateErrorTooLarge -> "ML_COORDINATE_ERROR_TOO_LARGE"

data RegionLocationCode
  = RegionLocationExterior
  | RegionLocationBoundary
  | RegionLocationInterior
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

allRegionLocationCodes :: [RegionLocationCode]
allRegionLocationCodes = [minBound .. maxBound]

regionLocationCodeId :: RegionLocationCode -> Word32
regionLocationCodeId code =
  case code of
    RegionLocationExterior -> 0
    RegionLocationBoundary -> 1
    RegionLocationInterior -> 2

regionLocationCodeSymbol :: RegionLocationCode -> String
regionLocationCodeSymbol code =
  case code of
    RegionLocationExterior -> "ML_REGION_EXTERIOR"
    RegionLocationBoundary -> "ML_REGION_BOUNDARY"
    RegionLocationInterior -> "ML_REGION_INTERIOR"

data MinkowskiOperationCode
  = MinkowskiOperationAddition
  | MinkowskiOperationErosion
  | MinkowskiOperationOpening
  | MinkowskiOperationClosing
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

allMinkowskiOperationCodes :: [MinkowskiOperationCode]
allMinkowskiOperationCodes = [minBound .. maxBound]

minkowskiOperationCodeId :: MinkowskiOperationCode -> Word32
minkowskiOperationCodeId code =
  case code of
    MinkowskiOperationAddition -> 0
    MinkowskiOperationErosion -> 1
    MinkowskiOperationOpening -> 2
    MinkowskiOperationClosing -> 3

minkowskiOperationCodeSymbol :: MinkowskiOperationCode -> String
minkowskiOperationCodeSymbol code =
  case code of
    MinkowskiOperationAddition -> "ML_MINKOWSKI_ADDITION"
    MinkowskiOperationErosion -> "ML_MINKOWSKI_EROSION"
    MinkowskiOperationOpening -> "ML_MINKOWSKI_OPENING"
    MinkowskiOperationClosing -> "ML_MINKOWSKI_CLOSING"

data CMesh
data CRegion
data CStructuringElement

data CObstruction = CObstruction
  { obstructionCode :: !Word32
  , obstructionCoordinateError :: !Word32
  , obstructionInputIndex :: !Word64
  , obstructionFirstIndex :: !Word64
  , obstructionSecondIndex :: !Word64
  , obstructionFirstValue :: !Double
  , obstructionSecondValue :: !Double
  , obstructionPointX :: !Double
  , obstructionPointY :: !Double
  , obstructionMessage :: !String
  }
  deriving stock (Eq, Show, Generic)

instance Storable CObstruction where
  sizeOf _ = 320
  alignment _ = alignment (0 :: Word64)
  peek pointer = do
    obstructionCode <- peekByteOff pointer 0
    obstructionCoordinateError <- peekByteOff pointer 4
    obstructionInputIndex <- peekByteOff pointer 8
    obstructionFirstIndex <- peekByteOff pointer 16
    obstructionSecondIndex <- peekByteOff pointer 24
    obstructionFirstValue <- peekByteOff pointer 32
    obstructionSecondValue <- peekByteOff pointer 40
    obstructionPointX <- peekByteOff pointer 48
    obstructionPointY <- peekByteOff pointer 56
    obstructionMessage <- peekCString (castPtr pointer `plusPtr` 64)
    pure CObstruction {..}
  poke pointer CObstruction {..} = do
    pokeByteOff pointer 0 obstructionCode
    pokeByteOff pointer 4 obstructionCoordinateError
    pokeByteOff pointer 8 obstructionInputIndex
    pokeByteOff pointer 16 obstructionFirstIndex
    pokeByteOff pointer 24 obstructionSecondIndex
    pokeByteOff pointer 32 obstructionFirstValue
    pokeByteOff pointer 40 obstructionSecondValue
    pokeByteOff pointer 48 obstructionPointX
    pokeByteOff pointer 56 obstructionPointY
    let messagePointer = castPtr pointer `plusPtr` 64 :: Ptr CChar
    fillBytes messagePointer 0 256
    withCStringLen obstructionMessage $ \(source, lengthInBytes) ->
      copyBytes messagePointer source (min 255 lengthInBytes)

data CMinkowskiReceipt = CMinkowskiReceipt
  { receiptOperation :: !Word32
  , receiptInputComponents :: !Word64
  , receiptConvexPieces :: !Word64
  , receiptGeneratedPieces :: !Word64
  , receiptGeneratedConvolutionEdges :: !Word64
  , receiptOverlayPasses :: !Word64
  , receiptExactCrossings :: !Word64
  , receiptOutputCells :: !Word64
  , receiptExactCoordinateBitGrowth :: !Word64
  }
  deriving stock (Eq, Show, Generic)

instance Storable CMinkowskiReceipt where
  sizeOf _ = 72
  alignment _ = alignment (0 :: Word64)
  peek pointer = do
    receiptOperation <- peekByteOff pointer 0
    receiptInputComponents <- peekByteOff pointer 8
    receiptConvexPieces <- peekByteOff pointer 16
    receiptGeneratedPieces <- peekByteOff pointer 24
    receiptGeneratedConvolutionEdges <- peekByteOff pointer 32
    receiptOverlayPasses <- peekByteOff pointer 40
    receiptExactCrossings <- peekByteOff pointer 48
    receiptOutputCells <- peekByteOff pointer 56
    receiptExactCoordinateBitGrowth <- peekByteOff pointer 64
    pure CMinkowskiReceipt {..}
  poke pointer CMinkowskiReceipt {..} = do
    pokeByteOff pointer 0 receiptOperation
    pokeByteOff pointer 4 (0 :: Word32)
    pokeByteOff pointer 8 receiptInputComponents
    pokeByteOff pointer 16 receiptConvexPieces
    pokeByteOff pointer 24 receiptGeneratedPieces
    pokeByteOff pointer 32 receiptGeneratedConvolutionEdges
    pokeByteOff pointer 40 receiptOverlayPasses
    pokeByteOff pointer 48 receiptExactCrossings
    pokeByteOff pointer 56 receiptOutputCells
    pokeByteOff pointer 64 receiptExactCoordinateBitGrowth

data AbiFieldType
  = AbiFieldUInt32
  | AbiFieldUInt64
  | AbiFieldDouble
  | AbiFieldCharArray !Int
  deriving stock (Eq, Show, Generic)

data AbiField = AbiField
  { abiFieldName :: !String
  , abiFieldType :: !AbiFieldType
  }
  deriving stock (Eq, Show, Generic)

data AbiStruct
  = AbiStructObstruction
  | AbiStructMinkowskiReceipt
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

allAbiStructs :: [AbiStruct]
allAbiStructs = [minBound .. maxBound]

abiStructSymbol :: AbiStruct -> String
abiStructSymbol structure =
  case structure of
    AbiStructObstruction -> "ml_obstruction"
    AbiStructMinkowskiReceipt -> "ml_minkowski_receipt"

abiStructFields :: AbiStruct -> [AbiField]
abiStructFields structure =
  case structure of
    AbiStructObstruction ->
      [ AbiField "code" AbiFieldUInt32
      , AbiField "coordinate_error" AbiFieldUInt32
      , AbiField "input_index" AbiFieldUInt64
      , AbiField "first_index" AbiFieldUInt64
      , AbiField "second_index" AbiFieldUInt64
      , AbiField "first_value" AbiFieldDouble
      , AbiField "second_value" AbiFieldDouble
      , AbiField "point_x" AbiFieldDouble
      , AbiField "point_y" AbiFieldDouble
      , AbiField "message" (AbiFieldCharArray 256)
      ]
    AbiStructMinkowskiReceipt ->
      [ AbiField "operation" AbiFieldUInt32
      , AbiField "reserved" AbiFieldUInt32
      , AbiField "input_components" AbiFieldUInt64
      , AbiField "convex_pieces" AbiFieldUInt64
      , AbiField "generated_pieces" AbiFieldUInt64
      , AbiField "generated_convolution_edges" AbiFieldUInt64
      , AbiField "overlay_passes" AbiFieldUInt64
      , AbiField "exact_crossings" AbiFieldUInt64
      , AbiField "output_cells" AbiFieldUInt64
      , AbiField "exact_coordinate_bit_growth" AbiFieldUInt64
      ]

data AbiHandle
  = AbiMesh
  | AbiRegion
  | AbiStructuringElement
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

allAbiHandles :: [AbiHandle]
allAbiHandles = [minBound .. maxBound]

abiHandleSymbol :: AbiHandle -> String
abiHandleSymbol handle =
  case handle of
    AbiMesh -> "ml_mesh"
    AbiRegion -> "ml_region"
    AbiStructuringElement -> "ml_structuring_element"

data AbiParameterKind
  = AbiValueSize
  | AbiValueDouble
  | AbiHandleInput !AbiHandle
  | AbiHandleRelease !AbiHandle
  | AbiHandleOutput !AbiHandle
  | AbiBufferInputDouble
  | AbiBufferInputSize
  | AbiBufferOutputChar
  | AbiBufferOutputDouble
  | AbiBufferOutputUInt32
  | AbiBufferOutputSize
  | AbiScalarOutputRegionLocation
  | AbiScalarOutputInt64
  | AbiScalarOutputSize
  | AbiScalarOutputDouble
  | AbiStructOutput !AbiStruct
  deriving stock (Eq, Show, Generic)

data AbiParameter = AbiParameter
  { abiParameterName :: !String
  , abiParameterKind :: !AbiParameterKind
  }
  deriving stock (Eq, Show, Generic)

data AbiResult
  = AbiResultVoid
  | AbiResultUInt32
  | AbiResultStatus
  deriving stock (Eq, Show, Generic)

data AbiFunctionFamily
  = AbiFunctionMesh
  | AbiFunctionRegion
  | AbiFunctionMorphology
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

data AbiFunctionImplementation
  = AbiFunctionRuntime
  | AbiFunctionHaskell !AbiFunctionFamily !String
  deriving stock (Eq, Show, Generic)

data AbiFunctionName
  = AbiVersion
  | RuntimeInitialize
  | DelaunayF64
  | MeshInsertManyF64
  | MeshSiteUnion
  | MeshSiteIntersection
  | MeshSiteDifference
  | MeshSiteSymmetricDifference
  | MeshVertexCount
  | MeshTriangleCount
  | MeshCopyVerticesF64
  | MeshCopyTrianglesU32
  | MeshFree
  | RegionCreateF64
  | RegionCounts
  | RegionCopyF64
  | RegionUnion
  | RegionIntersection
  | RegionDifference
  | RegionSymmetricDifference
  | RegionLocatePointF64
  | RegionMeasure
  | RegionFree
  | StructuringElementCreateF64
  | StructuringElementFree
  | RegionMinkowskiSum
  | RegionOffset
  | RegionInset
  | RegionOpen
  | RegionClose
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

data AbiFunction = AbiFunction
  { abiFunctionImplementation :: !AbiFunctionImplementation
  , abiFunctionSymbol :: !String
  , abiFunctionResult :: !AbiResult
  , abiFunctionParameters :: ![AbiParameter]
  }
  deriving stock (Eq, Show, Generic)

allAbiFunctionNames :: [AbiFunctionName]
allAbiFunctionNames = [minBound .. maxBound]

allAbiFunctions :: [AbiFunction]
allAbiFunctions = fmap abiFunction allAbiFunctionNames

abiFunction :: AbiFunctionName -> AbiFunction
abiFunction name =
  case name of
    AbiVersion -> runtime "ml_abi_version" AbiResultUInt32 []
    RuntimeInitialize -> runtime "ml_runtime_initialize" AbiResultStatus []
    DelaunayF64 -> mesh "ml_delaunay_f64" "delaunayF64" AbiResultStatus [inputDoubles "coordinates", valueSize "point_count", outputHandle "result" AbiMesh, obstruction]
    MeshInsertManyF64 -> mesh "ml_mesh_insert_many_f64" "meshInsertManyF64" AbiResultStatus [inputHandle "mesh" AbiMesh, inputDoubles "coordinates", valueSize "point_count", outputHandle "result" AbiMesh, obstruction]
    MeshSiteUnion -> binaryMesh "ml_mesh_site_union" "meshSiteUnion"
    MeshSiteIntersection -> binaryMesh "ml_mesh_site_intersection" "meshSiteIntersection"
    MeshSiteDifference -> binaryMesh "ml_mesh_site_difference" "meshSiteDifference"
    MeshSiteSymmetricDifference -> binaryMesh "ml_mesh_site_symmetric_difference" "meshSiteSymmetricDifference"
    MeshVertexCount -> meshCount "ml_mesh_vertex_count" "meshVertexCount"
    MeshTriangleCount -> meshCount "ml_mesh_triangle_count" "meshTriangleCount"
    MeshCopyVerticesF64 -> mesh "ml_mesh_copy_vertices_f64" "meshCopyVerticesF64" AbiResultStatus [inputHandle "mesh" AbiMesh, AbiParameter "coordinates" AbiBufferOutputDouble, valueSize "point_capacity", AbiParameter "points_written" AbiScalarOutputSize, obstruction]
    MeshCopyTrianglesU32 -> mesh "ml_mesh_copy_triangles_u32" "meshCopyTrianglesU32" AbiResultStatus [inputHandle "mesh" AbiMesh, AbiParameter "triangles" AbiBufferOutputUInt32, valueSize "triangle_capacity", AbiParameter "triangles_written" AbiScalarOutputSize, obstruction]
    MeshFree -> mesh "ml_mesh_free" "meshFree" AbiResultVoid [releaseHandle "mesh" AbiMesh]
    RegionCreateF64 -> region "ml_region_create_f64" "regionCreateF64" AbiResultStatus [inputDoubles "coordinates", valueSize "point_count", AbiParameter "loop_point_counts" AbiBufferInputSize, valueSize "loop_count", AbiParameter "component_loop_counts" AbiBufferInputSize, valueSize "component_count", outputHandle "result" AbiRegion, obstruction]
    RegionCounts -> region "ml_region_counts" "regionCounts" AbiResultStatus [inputHandle "region" AbiRegion, AbiParameter "component_count" AbiScalarOutputSize, AbiParameter "loop_count" AbiScalarOutputSize, AbiParameter "point_count" AbiScalarOutputSize, obstruction]
    RegionCopyF64 -> region "ml_region_copy_f64" "regionCopyF64" AbiResultStatus [inputHandle "region" AbiRegion, AbiParameter "coordinates" AbiBufferOutputDouble, valueSize "point_capacity", AbiParameter "loop_point_offsets" AbiBufferOutputSize, valueSize "loop_offset_capacity", AbiParameter "component_loop_offsets" AbiBufferOutputSize, valueSize "component_offset_capacity", obstruction]
    RegionUnion -> binaryRegion "ml_region_union" "regionUnion"
    RegionIntersection -> binaryRegion "ml_region_intersection" "regionIntersection"
    RegionDifference -> binaryRegion "ml_region_difference" "regionDifference"
    RegionSymmetricDifference -> binaryRegion "ml_region_symmetric_difference" "regionSymmetricDifference"
    RegionLocatePointF64 -> region "ml_region_locate_point_f64" "regionLocatePointF64" AbiResultStatus [inputHandle "region" AbiRegion, valueDouble "x", valueDouble "y", AbiParameter "location" AbiScalarOutputRegionLocation, obstruction]
    RegionMeasure -> region "ml_region_measure" "regionMeasure" AbiResultStatus [inputHandle "region" AbiRegion, AbiParameter "euler_characteristic" AbiScalarOutputInt64, AbiParameter "area_ratio_utf8" AbiBufferOutputChar, valueSize "area_capacity", AbiParameter "area_bytes_written" AbiScalarOutputSize, AbiParameter "perimeter_lower" AbiScalarOutputDouble, AbiParameter "perimeter_upper" AbiScalarOutputDouble, obstruction]
    RegionFree -> region "ml_region_free" "regionFree" AbiResultVoid [releaseHandle "region" AbiRegion]
    StructuringElementCreateF64 -> morphology "ml_structuring_element_create_f64" "structuringElementCreateF64" AbiResultStatus [inputDoubles "coordinates", valueSize "point_count", outputHandle "result" AbiStructuringElement, obstruction]
    StructuringElementFree -> morphology "ml_structuring_element_free" "structuringElementFree" AbiResultVoid [releaseHandle "element" AbiStructuringElement]
    RegionMinkowskiSum -> morphologyOperation "ml_region_minkowski_sum" "regionMinkowskiSum" (inputHandle "left" AbiRegion) (inputHandle "right" AbiRegion)
    RegionOffset -> morphologyOperation "ml_region_offset" "regionOffset" (inputHandle "element" AbiStructuringElement) (inputHandle "region" AbiRegion)
    RegionInset -> morphologyOperation "ml_region_inset" "regionInset" (inputHandle "element" AbiStructuringElement) (inputHandle "region" AbiRegion)
    RegionOpen -> morphologyOperation "ml_region_open" "regionOpen" (inputHandle "element" AbiStructuringElement) (inputHandle "region" AbiRegion)
    RegionClose -> morphologyOperation "ml_region_close" "regionClose" (inputHandle "element" AbiStructuringElement) (inputHandle "region" AbiRegion)
 where
  runtime = AbiFunction AbiFunctionRuntime
  implemented family symbol haskellName =
    AbiFunction (AbiFunctionHaskell family haskellName) symbol
  mesh = implemented AbiFunctionMesh
  region = implemented AbiFunctionRegion
  morphology = implemented AbiFunctionMorphology
  binaryMesh symbol haskellName =
    mesh symbol haskellName AbiResultStatus [inputHandle "left" AbiMesh, inputHandle "right" AbiMesh, outputHandle "result" AbiMesh, obstruction]
  meshCount symbol haskellName =
    mesh symbol haskellName AbiResultStatus [inputHandle "mesh" AbiMesh, AbiParameter "count" AbiScalarOutputSize, obstruction]
  binaryRegion symbol haskellName =
    region symbol haskellName AbiResultStatus [inputHandle "left" AbiRegion, inputHandle "right" AbiRegion, outputHandle "result" AbiRegion, obstruction]
  morphologyOperation symbol haskellName first second =
    morphology symbol haskellName AbiResultStatus [first, second, outputHandle "result" AbiRegion, AbiParameter "receipt" (AbiStructOutput AbiStructMinkowskiReceipt), obstruction]
  valueSize parameterName = AbiParameter parameterName AbiValueSize
  valueDouble parameterName = AbiParameter parameterName AbiValueDouble
  inputHandle parameterName handle = AbiParameter parameterName (AbiHandleInput handle)
  releaseHandle parameterName handle = AbiParameter parameterName (AbiHandleRelease handle)
  outputHandle parameterName handle = AbiParameter parameterName (AbiHandleOutput handle)
  inputDoubles parameterName = AbiParameter parameterName AbiBufferInputDouble
  obstruction = AbiParameter "obstruction" (AbiStructOutput AbiStructObstruction)
