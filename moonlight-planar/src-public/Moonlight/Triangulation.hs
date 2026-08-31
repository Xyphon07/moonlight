-- | The public surface for Delaunay triangulations as values of finite
-- coordinate supports. Its exports are grouped by object, generation,
-- annotations, finite-set algebra, constraints, refinement, observations and
-- validation.
--
-- Geometry-only set laws are observed after erasing vertex annotations with
-- @mapVertices (const ())@ and applying 'canonicalize'. Successful construction
-- may retain schedule-dependent resident numbering, so structural 'Eq' compares
-- stored representations rather than silently normalizing them. Annotations may
-- change independently through 'mapVertices' and 'setVertexData'; they never
-- determine geometry.
--
-- The algebra is deliberately partial. Finite arena exhaustion is returned as
-- 'BuildError', while unrealizable constrained compositions return
-- 'ConstrainedUnionError'. These typed obstructions are why the surface exposes
-- explicit operations rather than total 'Semigroup' instances. Refinement then
-- composes over any admitted result without introducing a second mesh type.
module Moonlight.Triangulation
  ( -- * The object — a triangulation is a value of its site set
    Triangulation
  , DelaunayTriangulation
  , ConstrainedDelaunayTriangulation
  , ConstraintMode (..)
  , Point (..)
  , QueryPoint
  , queryPointValue
  , PointValidationError (..)
  , mkQueryPoint
  , HasPosition (..)
  , ElementDefaults (..)
  , unitElementDefaults
  , JoinSemilattice (..)
  , Coordinate2 (..)
  , mapCoordinate2
    -- | Fixed identifiers are compact resident indices. Their observations
    -- deliberately avoid a second bounds check and therefore require the
    -- issuing mesh; use the scoped section below or owning handles whenever
    -- mesh provenance is not already carried by the surrounding type.
  , VertexId
  , unVertexId
  , FaceId
  , unFaceId
  , DirectedEdgeId
  , unDirectedEdgeId
  , UndirectedEdgeId
  , unUndirectedEdgeId
  , reverseEdge
  , asUndirected
  , normalizedDirected
  , reversedDirected
  , directedPair
  , isNormalized

    -- * Mesh-scoped observations — zero-cost nominal ownership; identifiers
    -- from distinct scopes cannot be mixed
  , ScopedTriangulation
  , ScopedVertex
  , ScopedDirectedEdge
  , ScopedUndirectedEdge
  , ScopedFace
  , withScopedTriangulation
  , scopedVertices
  , scopedDirectedEdges
  , scopedUndirectedEdges
  , scopedFaces
  , scopedInnerFaces
  , scopedOuterFace
  , scopedVertexPoint
  , scopedVertexData
  , scopedDirectedEdgeData
  , scopedUndirectedEdgeData
  , scopedFaceData
  , scopedVertexOutEdge
  , scopedAdjacentEdge
  , scopedOrigin
  , scopedDestination
  , scopedReverseEdge
  , scopedAsUndirected
  , scopedNext
  , scopedPrevious
  , scopedIncidentFace
  , scopedIsOuterDirectedEdge
  , scopedIsBoundaryEdge
  , scopedIsConstraintEdge
  , scopedUndirectedEndpoints
  , scopedFaceDirectedEdges
  , scopedFaceVertices
  , scopedVertexOutgoingEdges

    -- * Generation and editing — persistent verbs publish one new value;
    -- sessions compose many edits behind one sealed thaw/freeze boundary
  , empty
  , clear
  , delaunay
  , delaunayGeometry
  , DuplicatePayloadPolicy (..)
  , delaunayFromCoordinates
  , BuildResult
  , buildTriangulation
  , buildInputVertices
  , buildStats
  , BuildStats (..)
  , emptyBuildStats
  , InsertionDisposition (..)
  , InsertionResult
  , insertionTriangulation
  , insertionVertex
  , insertionDisposition
  , insertionStats
  , insert
  , insertAt
  , insertMany
  , RemovalOutcome (..)
  , RemovalResult (..)
  , removeVertex
  , locateAndRemove
  , Session
  , withSession
  , insertVertex
  , insertVertexAt
  , insertVertexAtNear
  , insertVertexAtNearVertex
  , removeAt
  , removeAtNear
  , removeManyAt
  , removeManyAtNear
  , refuse
  , BuildError (..)
  , PublicationStats (..)
  , ValidationClosureStats (..)
  , CoordinateError (..)
  , NonFiniteValue (..)

    -- * The annotation functor — payloads annotate geometry, never author it;
    -- the mesh is the same mesh before and after
  , mapVertices
  , mapDirectedEdges
  , mapUndirectedEdges
  , mapFaces
  , imapUndirectedEdges
  , imapFaces
  , PayloadTraversal
  , vertexPayloads
  , directedPayloads
  , undirectedPayloads
  , facePayloads
  , overPayloads
  , foldPayloads
  , payloadList
  , geometryOnlyPublication
  , vertexData
  , directedEdgeData
  , undirectedEdgeData
  , faceData
  , setVertexData
  , setDirectedEdgeData
  , setUndirectedEdgeData
  , setFaceData

    -- * Finite-set algebra and its normal form — support order is observable,
    -- intersection can combine heterogeneous annotations, and each
    -- construction returns its obstruction rather than an instance that lies
  , canonicalize
  , SiteRelation (..)
  , siteRelation
  , union
  , unions
  , intersection
  , intersectionWith
  , difference
  , symmetricDifference

    -- * The constraint layer — 'constrainedDelaunay' is the partial map from
    -- (sites, segments), defined exactly on realizable segment sets and
    -- naming the corridor that blocked it where it is not
  , constrainedDelaunay
  , constrainedDelaunayMaximal
  , fromDelaunay
  , constraintEdges
  , isConstraintEdge
  , boundedRegionFaces
  , CdtError (..)
  , CorridorObstruction (..)
  , CanonicalSegment
  , segmentStart
  , segmentEnd
  , ConstraintConflict
  , conflictFirstSegment
  , conflictSecondSegment
  , ConstrainedUnionError (..)
  , ConstraintBatchResult
  , constraintBatchTriangulation
  , constraintBatchOutcomes
  , constraintBatchStats
  , ConstrainedExtensionResult
  , constrainedExtensionConstraintBatch
  , constrainedExtensionBuildStats
  , ConstraintRecoveryResult (..)
  , ConstraintResult
  , ConstraintOutcome (..)
  , ConstraintBatchStats (..)
  , ConstraintSplitBatchResult
  , CdtBuildResult (..)
  , ConstrainedSeamSide (..)
  , ConstrainedSeamFaceEvidence
  , constrainedSeamSourceFace
  , constrainedSeamTargetFace
  , constrainedSeamFaceFirstPoint
  , constrainedSeamFaceSecondPoint
  , constrainedSeamFaceThirdPoint
  , ConstrainedSeamResult
  , constrainedSeamResultTriangulation
  , constrainedSeamLeftFaceCount
  , constrainedSeamRightFaceEvidence
  , constrainedSeamJoinFaces
  , constrainedSeamLeftConstraintCount
  , constrainedSeamConstraintStats
  , constrainedSeamBuildStats
  , constrainedSeamPublicationStats
  , constrainedSeamCachedFrontierPointReads
  , constraintSegments
  , unionConstrainedWith
  , unionConstrained
  , joinSeparatedConstrained
  , extendConstrainedWith
  , constraintStorageBytes
  , existsConstraint
  , canAddConstraint
  , intersectsConstraint
  , getConflictingEdgesBetweenPoints
  , getConflictingEdgesBetweenVertices
  , recoverConstraints
  , addConstraintEdge
  , addConstraintEdges
  , addConstraintAndSplit
  , addConstraintsAndSplit
  , removeConstraintEdge
  , outerRegionFaces

    -- * Refinement — budget-bounded, composed after any operation above rather
    -- than configured into it; 'refinementComplete' reports sufficiency, not
    -- effort
  , refine
  , refineWithinDomain
  , validateRefinementParameters
  , RefinementParameters (..)
  , defaultRefinementParameters
  , RefinementReceipt
  , RefinementDomainResult
  , RefinementResult
  , refinementDomainResult
  , refinementDomainReceipt
  , refinedTriangulation
  , refinementStats
  , refinementVisitedJoinFaces
  , refinementVisitedProtectedFaces
  , refinementCreatedFaces
  , refinementFinalPermittedFaces
  , refinementFinalInterfaceIncidence
  , refinementTouchedEdges
  , refinementRemovedEdges
  , refinementInterfaceBoundaryReads
  , refinementAttemptedBoundaryCrossings
  , refinementPublicationStats
  , refinementValidationClosureStats
  , refinementAddedVertices
  , refinementExcludedFaces
  , refinementComplete
  , withMinimumAngle
  , radiusEdgeRatioForAngle

    -- * Observations — pure functions of the value: incidence, location,
    -- interpolation and barrier parity, none of which build a second mesh
  , numVertices
  , vertices
  , foldVertices'
  , vertexPoint
  , vertexPoints
  , numFaces
  , numInnerFaces
  , allFaces
  , innerFaces
  , foldAllFaces'
  , foldInnerFaces'
  , outerFace
  , adjacentEdge
  , faceDirectedEdges
  , faceVertices
  , innerFaceDirectedEdges
  , innerFaceVertices
  , innerFaceDirectedEdgeTriples
  , innerFaceVertexTriples
  , foldFaceDirectedEdges'
  , vertexOutEdge
  , vertexOutgoingEdges
  , foldVertexOutgoingEdges'
  , numDirectedEdges
  , directedEdges
  , foldDirectedEdges'
  , numUndirectedEdges
  , undirectedEdges
  , foldUndirectedEdges'
  , hullEdges
  , hullVertices
  , undirectedEndpoints
  , origin
  , destination
  , next
  , previous
  , clockwise
  , counterClockwise
  , incidentFace
  , isOuterDirectedEdge
  , isBoundaryEdge
  , numConstraints
  , locatePoint
  , locatePointWithHint
  , Location (..)
  , nearestNeighbor
  , NearestStats (..)
  , interpolateNearest
  , interpolateBarycentric
  , LocationHint (..)
  , LocationStats (..)
  , emptyLocationStats
  , Intersection (..)
  , lineIntersections
  , lineIntersectionsBetweenVertices
  , foldCorridorBetweenPoints
  , foldCorridorBetweenVertices
  , conflictingEdges
  , segmentIntersectsNonCollinear
  , DistanceMetric (..)
  , CircleMetric
  , CircleMetricError (..)
  , RectangleMetric
  , RectangleMetricError (..)
  , circleMetric
  , rectangleMetric
  , edgesInShape
  , verticesInShape
  , edgesInCircle
  , verticesInCircle
  , edgesInRectangle
  , verticesInRectangle
  , floodFillFaces
  , outerFaceFloodFill
  , facesAtEvenBarrierDepth

    -- ** Robust planar predicates and constructions
  , orient2d
  , sideQuery
  , inCircle
  , segmentRelation
  , segmentsProperlyCross
  , segmentsIntersect
  , squaredDistance
  , distance
  , midpoint
  , centroid
  , triangleArea
  , circumcenter
  , barycentricCoordinates

    -- ** Face regions and alpha filtration
  , FaceComponent
  , faceComponentFaces
  , BoundaryOrientation (..)
  , BoundaryLoop
  , boundaryLoopOrientation
  , boundaryLoopResidentVertices
  , boundaryLoopVertices
  , RegionBoundary
  , regionBoundaryOuterLoop
  , regionBoundaryHoleLoops
  , BoundaryObstruction (..)
  , faceComponents
  , faceComponentsBy
  , labelledRegionBoundaries
  , componentBoundaryLoops
  , componentBoundary
  , componentBoundaryForFaces
  , RadiusSquared
  , RadiusSquaredError (..)
  , mkRadiusSquared
  , radiusSquaredValue
  , AlphaBirth
  , alphaBirthNumerator
  , alphaBirthDenominator
  , alphaBirthToDouble
  , alphaBirthFromRadiusSquared
  , AlphaFiltration
  , AlphaFiltrationError (..)
  , alphaFiltration
  , alphaFiltrationCellSet
  , alphaVertexBirth
  , alphaEdgeBirth
  , alphaFaceBirth
  , alphaFiltrationCriticalBirths
  , alphaComplexAtBirth
  , alphaComplexAtRadius
  , alphaShapeContainsFace

    -- * Exact planar regions — authoritative rational geometry, labelled
    -- overlay, closed cell selections, valuations, and polygonal morphology
  , ExactRational
  , ExactArithmeticError (..)
  , exactRational
  , exactRationalNumerator
  , exactRationalDenominator
  , ExactPoint
  , exactPoint
  , exactPointCoordinates
  , exactPointCross
  , ExactVector (..)
  , exactVectorFromPoints
  , addExactVectors
  , exactCross
  , compareExactVectorAngle
  , translateExactPoint
  , ExactRay
  , exactRay
  , exactRayOrigin
  , exactRayDirection
  , ExactSegment
  , ExactGeometryError (..)
  , exactSegment
  , exactSegmentEndpoints
  , exactPointFromPoint
  , exactPointFromQueryPoint
  , exactPointToEmbeddingCandidate
  , exactOrient2d
  , exactOnClosedSegment
  , SegmentRelation (..)
  , allSegmentRelations
  , exactSegmentRelation
  , ExactIntersectionError (..)
  , exactLineIntersection
  , exactSupportingLineIntersection
  , ExactLoop
  , exactLoop
  , exactLoopPoints
  , PolygonComponent
  , polygonComponent
  , polygonOuterLoop
  , polygonHoleLoops
  , PlanarRegion
  , planarRegion
  , planarRegionComponents
  , emptyPlanarRegion
  , RegionPointLocation (..)
  , regionPointLocation
  , PlanarLayer
  , planarLayerOutsideLabel
  , planarLayerRegions
  , planarLayer
  , planarLayerLabelAt
  , RegionValidationError (..)
  , RegionPublicationError (..)
  , labelledPlanarLayer
  , ExactCellSet
  , CellSelectionError (..)
  , exactCellSet
  , closeFaceCellSet
  , exactCellSetVertexCount
  , exactCellSetEdgeCount
  , exactCellSetFaceCount
  , foldExactCellVertices
  , foldExactCellEdges
  , foldExactCellFaces
  , OverlayResult
  , OverlayReceipt (..)
  , OverlayError (..)
  , OverlaySelectionKind (..)
  , OverlaySelectionError (..)
  , CoverGap
  , coverGapRegion
  , LayerCoverageError (..)
  , overlayLayers
  , overlayAll
  , overlayEmbeddedTriangulation
  , overlayReceipt
  , overlayCells
  , overlayArrangementVertices
  , overlayArrangementEdges
  , overlayPlanarLayer
  , overlaySelectedRegion
  , overlayMass
  , overlayConfusion
  , layerCovers
  , overlayClosedUnion
  , overlayClosedIntersection
  , overlayRegularizedDifference
  , EulerCharacteristic
  , eulerCharacteristicValue
  , ExactArea
  , ExactPlanarMoments
  , exactAreaValue
  , exactPlanarMeasure
  , exactPlanarFirstX
  , exactPlanarFirstY
  , exactPlanarSecondXX
  , exactPlanarSecondXY
  , exactPlanarSecondYY
  , scaleExactPlanarMoments
  , polygonComponentMoments
  , polygonComponentArea
  , ExactLengthTerm
  , lengthCoefficient
  , squaredLength
  , ExactLengthExpression
  , exactLengthTerms
  , CertifiedInterval (..)
  , ExactLengthMeasurement
  , exactLengthExpression
  , exactLengthBounds
  , PlanarValuations
  , valuationEuler
  , valuationArea
  , valuationIntrinsic1
  , ValuationError (..)
  , cellValuations
  , regionValuations
  , planarValuationsPerimeter
  , cellSetPerimeter
  , regionPerimeter
  , ConvexPolygon
  , convexPolygon
  , convexPolygonPoints
  , StructuringElement
  , structuringElement
  , MinkowskiOperation (..)
  , MinkowskiError (..)
  , MinkowskiReceipt (..)
  , convexMinkowskiSum
  , reflectConvexPolygon
  , minkowskiSum
  , erodeBy
  , openWith
  , closeWith
  , polygonOffset
  , polygonInset
  , PowerWeight
  , PowerWeightError (..)
  , powerWeight
  , powerWeightFromExact
  , powerWeightExact
  , PowerSite
  , powerSite
  , powerSiteLabel
  , powerSitePosition
  , powerSiteExactPosition
  , powerSiteWeight
  , PowerCellDisposition (..)
  , RegularSiteDisposition (..)
  , RegularFace
  , regularFaceLabels
  , regularFaceDualPoint
  , PowerDualEdge (..)
  , RegularEdge
  , regularEdgeLabels
  , regularEdgeDual
  , RegularTriangulation
  , emptyRegularTriangulation
  , regularTriangulation
  , regularSite
  , regularSites
  , regularSiteCount
  , regularSiteDisposition
  , regularFaces
  , regularEdges
  , regularNeighbours
  , regularTriangulationReceipt
  , RegularTriangulationReceipt (..)
  , RegularSiteTransition (..)
  , RegularEditError (..)
  , RegularEditResult (..)
  , insertRegularSite
  , removeRegularSite
  , reweightRegularSites
  , BoundedPowerDiagram
  , boundedPowerDiagram
  , boundedPowerDiagramFromRegular
  , powerCellDisposition
  , powerCellDispositions
  , powerDiagramPlanarLayer
  , PowerDiagramError (..)
  , RegularTopologyError (..)
  , PowerDiagramReceipt (..)
  , powerDiagramInputSites
  , powerDiagramPeakIntermediateBitGrowth
  , powerDiagramFinalCoordinateBitGrowth
  , AffineForm (..)
  , UpperEnvelopeError (..)
  , upperEnvelope
  , PowerAlphaBirth
  , powerAlphaBirthExact
  , powerAlphaBirthNumerator
  , powerAlphaBirthDenominator
  , RegularAlphaFiltration
  , RegularAlphaError (..)
  , regularAlphaFiltration
  , regularAlphaComplex
  , regularAlphaBirths
  , regularAlphaSimplexBirth
  , regularAlphaCriticalBirths
  , regularAlphaComplexAtBirth

    -- * Discharge — the invariants the constructors guarantee, checkable on a
    -- value built by any route; every violation is a value carrying its witness
  , validateTopology
  , validateDelaunay
  , validateTriangulation
  , triangulationIsValid
  , faceArea
  , faceMinimumAngleDegrees
  , InvariantViolation (..)
  ) where

import Moonlight.Triangulation.BulkLoad
  ( DuplicatePayloadPolicy (..)
  , clear
  , delaunay
  , delaunayFromCoordinates
  , delaunayGeometry
  , empty
  , insert
  , insertAt
  , insertMany
  )
import Moonlight.Triangulation.CellSet
  ( CellSelectionError (..)
  , ExactCellSet
  , closeFaceCellSet
  , exactCellSet
  , exactCellSetEdgeCount
  , exactCellSetFaceCount
  , exactCellSetVertexCount
  , foldExactCellEdges
  , foldExactCellFaces
  , foldExactCellVertices
  )
import Moonlight.Triangulation.Dcel
  ( adjacentEdge
  , clockwise
  , counterClockwise
  , destination
  , directedEdgeData
  , faceData
  , faceDirectedEdges
  , faceVertices
  , foldFaceDirectedEdges'
  , foldVertexOutgoingEdges'
  , imapFaces
  , imapUndirectedEdges
  , innerFaceDirectedEdges
  , innerFaceDirectedEdgeTriples
  , innerFaceVertices
  , innerFaceVertexTriples
  , incidentFace
  , isBoundaryEdge
  , isConstraintEdge
  , isOuterDirectedEdge
  , next
  , numConstraints
  , numDirectedEdges
  , numFaces
  , numInnerFaces
  , numUndirectedEdges
  , numVertices
  , origin
  , outerFace
  , previous
  , setDirectedEdgeData
  , setFaceData
  , setUndirectedEdgeData
  , setVertexData
  , undirectedEdgeData
  , undirectedEndpoints
  , vertexData
  , vertexOutEdge
  , vertexOutgoingEdges
  , vertexPoint
  , vertexPoints
  )
import Moonlight.Triangulation.Alpha
  ( AlphaBirth
  , AlphaFiltration
  , AlphaFiltrationError (..)
  , alphaBirthDenominator
  , alphaBirthFromRadiusSquared
  , alphaBirthNumerator
  , alphaBirthToDouble
  , alphaComplexAtBirth
  , alphaComplexAtRadius
  , alphaEdgeBirth
  , alphaFaceBirth
  , alphaFiltration
  , alphaFiltrationCellSet
  , alphaFiltrationCriticalBirths
  , alphaShapeContainsFace
  , alphaVertexBirth
  )
import Moonlight.Triangulation.FloodFillIterator
  ( BoundaryLoop
  , BoundaryObstruction (..)
  , BoundaryOrientation (..)
  , CircleMetric
  , CircleMetricError (..)
  , DistanceMetric (..)
  , FaceComponent
  , RectangleMetric
  , RectangleMetricError (..)
  , RegionBoundary
  , boundaryLoopOrientation
  , boundaryLoopResidentVertices
  , boundaryLoopVertices
  , circleMetric
  , componentBoundary
  , componentBoundaryLoops
  , componentBoundaryForFaces
  , edgesInCircle
  , edgesInRectangle
  , edgesInShape
  , faceComponentFaces
  , faceComponents
  , faceComponentsBy
  , facesAtEvenBarrierDepth
  , floodFillFaces
  , labelledRegionBoundaries
  , outerFaceFloodFill
  , rectangleMetric
  , regionBoundaryHoleLoops
  , regionBoundaryOuterLoop
  , verticesInCircle
  , verticesInRectangle
  , verticesInShape
  )
import Moonlight.Triangulation.Exact
  ( ExactGeometryError (..)
  , ExactIntersectionError (..)
  , ExactPoint
  , ExactRay
  , ExactSegment
  , ExactVector (..)
  , SegmentRelation (..)
  , addExactVectors
  , allSegmentRelations
  , compareExactVectorAngle
  , exactCross
  , exactLineIntersection
  , exactOnClosedSegment
  , exactOrient2d
  , exactPoint
  , exactPointCoordinates
  , exactPointCross
  , exactPointFromPoint
  , exactPointFromQueryPoint
  , exactPointToEmbeddingCandidate
  , exactRay
  , exactRayDirection
  , exactRayOrigin
  , exactSegment
  , exactSegmentEndpoints
  , exactSegmentRelation
  , exactSupportingLineIntersection
  , exactVectorFromPoints
  , translateExactPoint
  )
import Moonlight.Triangulation.Handles.HandleDefs
  ( DirectedEdgeId
  , FaceId
  , UndirectedEdgeId
  , VertexId
  , asUndirected
  , directedPair
  , isNormalized
  , normalizedDirected
  , reverseEdge
  , reversedDirected
  , unDirectedEdgeId
  , unFaceId
  , unUndirectedEdgeId
  , unVertexId
  )
import Moonlight.Triangulation.Handles.Iterators.FixedIterators
  ( allFaces
  , directedEdges
  , foldAllFaces'
  , foldDirectedEdges'
  , foldInnerFaces'
  , foldUndirectedEdges'
  , foldVertices'
  , innerFaces
  , undirectedEdges
  , vertices
  )
import Moonlight.Triangulation.Handles.Iterators.HullIterator
  ( hullEdges
  , hullVertices
  )
import Moonlight.Triangulation.Handles.Scoped
  ( ScopedDirectedEdge
  , ScopedFace
  , ScopedTriangulation
  , ScopedUndirectedEdge
  , ScopedVertex
  , scopedAdjacentEdge
  , scopedAsUndirected
  , scopedDestination
  , scopedDirectedEdgeData
  , scopedDirectedEdges
  , scopedFaceData
  , scopedFaceDirectedEdges
  , scopedFaceVertices
  , scopedFaces
  , scopedIncidentFace
  , scopedInnerFaces
  , scopedIsBoundaryEdge
  , scopedIsConstraintEdge
  , scopedIsOuterDirectedEdge
  , scopedNext
  , scopedOrigin
  , scopedOuterFace
  , scopedPrevious
  , scopedReverseEdge
  , scopedUndirectedEdgeData
  , scopedUndirectedEdges
  , scopedUndirectedEndpoints
  , scopedVertexData
  , scopedVertexOutEdge
  , scopedVertexOutgoingEdges
  , scopedVertexPoint
  , scopedVertices
  , withScopedTriangulation
  )
import Moonlight.Triangulation.Canonical (canonicalize)
import Moonlight.Triangulation.Cdt
  ( CanonicalSegment
  , CdtBuildResult (..)
  , CdtError (..)
  , ConstrainedExtensionResult
  , ConstrainedSeamFaceEvidence
  , ConstrainedSeamResult
  , ConstrainedSeamSide (..)
  , ConstrainedUnionError (..)
  , ConstraintBatchStats (..)
  , ConstraintBatchResult
  , ConstraintConflict
  , ConstraintOutcome (..)
  , ConstraintRecoveryResult (..)
  , ConstraintResult
  , ConstraintSplitBatchResult
  , CorridorObstruction (..)
  , addConstraintAndSplit
  , addConstraintEdge
  , addConstraintEdges
  , addConstraintsAndSplit
  , boundedRegionFaces
  , canAddConstraint
  , constrainedDelaunay
  , constrainedDelaunayMaximal
  , constrainedExtensionBuildStats
  , constrainedExtensionConstraintBatch
  , constraintEdges
  , constraintBatchOutcomes
  , constraintBatchStats
  , constraintBatchTriangulation
  , constraintSegments
  , constraintStorageBytes
  , constrainedSeamBuildStats
  , constrainedSeamConstraintStats
  , constrainedSeamCachedFrontierPointReads
  , constrainedSeamFaceFirstPoint
  , constrainedSeamFaceSecondPoint
  , constrainedSeamFaceThirdPoint
  , constrainedSeamLeftConstraintCount
  , constrainedSeamLeftFaceCount
  , constrainedSeamJoinFaces
  , constrainedSeamPublicationStats
  , constrainedSeamResultTriangulation
  , constrainedSeamRightFaceEvidence
  , constrainedSeamSourceFace
  , constrainedSeamTargetFace
  , conflictFirstSegment
  , conflictSecondSegment
  , existsConstraint
  , extendConstrainedWith
  , fromDelaunay
  , getConflictingEdgesBetweenPoints
  , getConflictingEdgesBetweenVertices
  , intersectsConstraint
  , joinSeparatedConstrained
  , outerRegionFaces
  , recoverConstraints
  , removeConstraintEdge
  , segmentEnd
  , segmentStart
  , unionConstrained
  , unionConstrainedWith
  )
import Moonlight.Triangulation.Internal.Paged (PublicationStats (..))
import Moonlight.Triangulation.Internal.ExactRational
  ( ExactArithmeticError (..)
  , ExactRational
  , exactRational
  , exactRationalDenominator
  , exactRationalNumerator
  )
import Moonlight.Triangulation.Internal.Representation (geometryOnlyPublication)
import Moonlight.Triangulation.Interop
  ( Coordinate2 (..)
  , mapCoordinate2
  )
import Moonlight.Triangulation.IntersectionIterator
  ( Intersection (..)
  , conflictingEdges
  , foldCorridorBetweenPoints
  , foldCorridorBetweenVertices
  , lineIntersections
  , lineIntersectionsBetweenVertices
  , segmentIntersectsNonCollinear
  )
import Moonlight.Triangulation.Types
  ( BuildError (..)
  , BuildResult
  , BuildStats (..)
  , ConstrainedDelaunayTriangulation
  , ConstraintMode (..)
  , CoordinateError (..)
  , DelaunayTriangulation
  , ElementDefaults (..)
  , HasPosition (..)
  , InsertionDisposition (..)
  , InsertionResult
  , InvariantViolation (..)
  , Location (..)
  , LocationHint (..)
  , LocationStats (..)
  , NearestStats (..)
  , NonFiniteValue (..)
  , Point (..)
  , PointValidationError (..)
  , QueryPoint
  , RadiusSquared
  , RadiusSquaredError (..)
  , RefinementDomainResult
  , RefinementParameters (..)
  , RefinementReceipt
  , RefinementResult
  , SiteRelation (..)
  , Triangulation
  , ValidationClosureStats (..)
  , buildInputVertices
  , buildStats
  , buildTriangulation
  , defaultRefinementParameters
  , emptyBuildStats
  , emptyLocationStats
  , insertionDisposition
  , insertionStats
  , insertionTriangulation
  , insertionVertex
  , mkRadiusSquared
  , queryPointValue
  , radiusSquaredValue
  , refinedTriangulation
  , refinementAddedVertices
  , refinementAttemptedBoundaryCrossings
  , refinementComplete
  , refinementCreatedFaces
  , refinementDomainReceipt
  , refinementDomainResult
  , refinementExcludedFaces
  , refinementFinalInterfaceIncidence
  , refinementFinalPermittedFaces
  , refinementInterfaceBoundaryReads
  , refinementPublicationStats
  , refinementRemovedEdges
  , refinementStats
  , refinementTouchedEdges
  , refinementValidationClosureStats
  , refinementVisitedJoinFaces
  , refinementVisitedProtectedFaces
  , unitElementDefaults
  )
import Moonlight.Triangulation.Interpolation
  ( interpolateBarycentric
  , interpolateNearest
  , nearestNeighbor
  )
import Moonlight.Triangulation.Minkowski
  ( ConvexPolygon
  , MinkowskiError (..)
  , MinkowskiOperation (..)
  , MinkowskiReceipt (..)
  , StructuringElement
  , closeWith
  , convexMinkowskiSum
  , reflectConvexPolygon
  , convexPolygon
  , convexPolygonPoints
  , erodeBy
  , minkowskiSum
  , openWith
  , polygonInset
  , polygonOffset
  , structuringElement
  )
import Moonlight.Triangulation.Math
  ( barycentricCoordinates
  , centroid
  , circumcenter
  , distance
  , inCircle
  , midpoint
  , mkQueryPoint
  , orient2d
  , segmentRelation
  , segmentsIntersect
  , segmentsProperlyCross
  , sideQuery
  , squaredDistance
  , triangleArea
  )
import Moonlight.Triangulation.Overlay
  ( CoverGap
  , LayerCoverageError (..)
  , OverlayError (..)
  , OverlayReceipt (..)
  , OverlayResult
  , OverlaySelectionError (..)
  , OverlaySelectionKind (..)
  , coverGapRegion
  , layerCovers
  , overlayAll
  , overlayArrangementEdges
  , overlayArrangementVertices
  , overlayCells
  , overlayClosedIntersection
  , overlayClosedUnion
  , overlayEmbeddedTriangulation
  , overlayLayers
  , overlayMass
  , overlayConfusion
  , overlayPlanarLayer
  , overlayReceipt
  , overlayRegularizedDifference
  , overlaySelectedRegion
  )
import Moonlight.Triangulation.PowerDiagram
  ( AffineForm (..)
  , BoundedPowerDiagram
  , PowerDualEdge (..)
  , PowerCellDisposition (..)
  , PowerDiagramError (..)
  , PowerDiagramReceipt (..)
  , PowerSite
  , PowerWeight
  , PowerWeightError (..)
  , RegularEdge
  , RegularEditError (..)
  , RegularEditResult (..)
  , RegularFace
  , RegularSiteDisposition (..)
  , RegularSiteTransition (..)
  , RegularTriangulation
  , RegularTriangulationReceipt (..)
  , RegularTopologyError (..)
  , UpperEnvelopeError (..)
  , boundedPowerDiagram
  , boundedPowerDiagramFromRegular
  , emptyRegularTriangulation
  , insertRegularSite
  , powerCellDisposition
  , powerCellDispositions
  , powerDiagramPlanarLayer
  , powerDiagramInputSites
  , powerDiagramPeakIntermediateBitGrowth
  , powerDiagramFinalCoordinateBitGrowth
  , powerSite
  , powerSiteLabel
  , powerSitePosition
  , powerSiteExactPosition
  , powerSiteWeight
  , powerWeight
  , powerWeightFromExact
  , powerWeightExact
  , regularEdgeDual
  , regularEdgeLabels
  , regularEdges
  , regularFaceDualPoint
  , regularFaceLabels
  , regularFaces
  , regularNeighbours
  , regularSite
  , regularSiteCount
  , regularSiteDisposition
  , regularSites
  , regularTriangulation
  , regularTriangulationReceipt
  , removeRegularSite
  , reweightRegularSites
  , upperEnvelope
  )
import Moonlight.Triangulation.RegularAlpha
  ( PowerAlphaBirth
  , RegularAlphaError (..)
  , RegularAlphaFiltration
  , powerAlphaBirthDenominator
  , powerAlphaBirthExact
  , powerAlphaBirthNumerator
  , regularAlphaBirths
  , regularAlphaComplex
  , regularAlphaComplexAtBirth
  , regularAlphaCriticalBirths
  , regularAlphaFiltration
  , regularAlphaSimplexBirth
  )
import Moonlight.Triangulation.Payload
  ( PayloadTraversal
  , directedPayloads
  , facePayloads
  , foldPayloads
  , mapDirectedEdges
  , mapFaces
  , mapUndirectedEdges
  , mapVertices
  , overPayloads
  , payloadList
  , undirectedPayloads
  , vertexPayloads
  )
import Moonlight.Triangulation.PointLocation
  ( locatePoint
  , locatePointWithHint
  )
import Moonlight.Triangulation.Removal
  ( RemovalOutcome (..)
  , RemovalResult (..)
  , locateAndRemove
  , removeVertex
  )
import Moonlight.Triangulation.JoinSemilattice (JoinSemilattice (..))
import Moonlight.Triangulation.Refinement
  ( radiusEdgeRatioForAngle
  , refine
  , refineWithinDomain
  , validateRefinementParameters
  , withMinimumAngle
  )
import Moonlight.Triangulation.Region
  ( ExactLoop
  , PlanarLayer
  , PlanarRegion
  , PolygonComponent
  , RegionPointLocation (..)
  , RegionPublicationError (..)
  , RegionValidationError (..)
  , emptyPlanarRegion
  , exactLoop
  , exactLoopPoints
  , labelledPlanarLayer
  , planarLayer
  , planarLayerLabelAt
  , planarLayerOutsideLabel
  , planarLayerRegions
  , planarRegion
  , planarRegionComponents
  , polygonComponent
  , polygonHoleLoops
  , polygonOuterLoop
  , regionPointLocation
  )
import Moonlight.Triangulation.SetAlgebra
  ( difference
  , intersection
  , intersectionWith
  , siteRelation
  , symmetricDifference
  , union
  , unions
  )
import Moonlight.Triangulation.Session
  ( Session
  , insertVertex
  , insertVertexAt
  , insertVertexAtNear
  , insertVertexAtNearVertex
  , refuse
  , removeAt
  , removeAtNear
  , removeManyAt
  , removeManyAtNear
  , withSession
  )
import Moonlight.Triangulation.Validation
  ( faceArea
  , faceMinimumAngleDegrees
  , triangulationIsValid
  , validateDelaunay
  , validateTopology
  , validateTriangulation
  )
import Moonlight.Triangulation.Valuation
  ( CertifiedInterval (..)
  , EulerCharacteristic
  , ExactArea
  , ExactPlanarMoments
  , ExactLengthExpression
  , ExactLengthMeasurement
  , ExactLengthTerm
  , PlanarValuations
  , ValuationError (..)
  , cellSetPerimeter
  , cellValuations
  , eulerCharacteristicValue
  , exactAreaValue
  , exactPlanarFirstX
  , exactPlanarFirstY
  , exactPlanarMeasure
  , exactPlanarSecondXX
  , exactPlanarSecondXY
  , exactPlanarSecondYY
  , polygonComponentArea
  , polygonComponentMoments
  , exactLengthBounds
  , exactLengthExpression
  , exactLengthTerms
  , lengthCoefficient
  , planarValuationsPerimeter
  , regionPerimeter
  , regionValuations
  , scaleExactPlanarMoments
  , squaredLength
  , valuationArea
  , valuationEuler
  , valuationIntrinsic1
  )
