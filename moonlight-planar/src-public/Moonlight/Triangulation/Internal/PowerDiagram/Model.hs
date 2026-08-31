{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Shared opaque carriers for exact power topology. This module owns
-- representation only; construction and interpretation live in sibling modules.
module Moonlight.Triangulation.Internal.PowerDiagram.Model where

import Control.DeepSeq (NFData)
import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Vector as Vector
import GHC.Generics (Generic)
import Moonlight.Triangulation.Exact
  ( ExactAffineLine
  , ExactClipError
  , ExactHalfPlaneError
  , ExactPoint
  , ExactRay
  , ExactSegment
  , exactPointFromQueryPoint
  )
import Moonlight.Triangulation.Internal.ExactRational
  ( ExactRational
  , exactRationalFromFiniteDouble
  )
import Moonlight.Triangulation.Internal.BoundaryCycle (orderedPair)
import Moonlight.Triangulation.Internal.PowerDiagram.Generator
  ( ExactPowerGenerator (..)
  , GeneratorRegularReceipt
  , GeneratorRegularTopology
  , RegularTopologyError
  )
import Moonlight.Triangulation.Math (mkQueryPoint)
import Moonlight.Triangulation.Minkowski (ConvexPolygon, MinkowskiError)
import Moonlight.Triangulation.Internal.Overlay.Types (OverlayError)
import Moonlight.Triangulation.Types
  ( NonFiniteValue
  , Point
  , PointValidationError
  , QueryPoint
  , classifyNonFinite
  , queryPointValue
  )

-- | An admitted signed additive power offset.  Power distance is
-- @||x-p||^2-w@, so negative values are lawful and this is deliberately not a
-- squared-radius refinement.
newtype PowerWeight = PowerWeight ExactRational
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | The sole obstruction to admitting a signed binary64 power offset.
data PowerWeightError
  = PowerWeightNonFinite !NonFiniteValue
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Admit a finite binary64 power offset exactly.
powerWeight :: Double -> Either PowerWeightError PowerWeight
powerWeight value =
  case classifyNonFinite value of
    Just obstruction -> Left (PowerWeightNonFinite obstruction)
    Nothing -> Right (PowerWeight (exactRationalFromFiniteDouble value))

-- | Admit an already-exact signed power offset without a binary64 round trip.
powerWeightFromExact :: ExactRational -> PowerWeight
powerWeightFromExact = PowerWeight

-- | Exact rational value of an admitted power offset.
powerWeightExact :: PowerWeight -> ExactRational
powerWeightExact (PowerWeight value) = value

-- | One labelled, admitted weighted site.  Construction validates and
-- canonicalizes the binary64 position once.
data PowerSite label = PowerSite
  { powerSiteLabel :: !label
    -- ^ Stable identity used by edits and canonical topology.
  , powerSiteQueryPoint :: !QueryPoint
  , powerSiteWeight :: !PowerWeight
    -- ^ Signed additive power offset.
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Admit one stable-labelled site at a finite position.
powerSite
  :: label
  -> Point
  -> PowerWeight
  -> Either (PowerDiagramError label) (PowerSite label)
powerSite label point weight =
  PowerSite label <$> first (PowerSitePositionInvalid label) (mkQueryPoint point) <*> pure weight

-- | Binary64 position originally admitted for the site.
powerSitePosition :: PowerSite label -> Point
powerSitePosition = queryPointValue . powerSiteQueryPoint

-- | Exact coordinates retained by an admitted site.
powerSiteExactPosition :: PowerSite label -> ExactPoint
powerSiteExactPosition = exactPointFromQueryPoint . powerSiteQueryPoint

-- | Exactly one authoritative result for each submitted label.
data PowerCellDisposition label
  = PublishedPowerCell !ConvexPolygon
  | LowerDimensionalPowerCell !(NonEmpty ExactPoint)
  | EmptyPowerCell
  | CoincidentEquivalentTo !label
  | CoincidentDominatedBy !label
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Visibility of one submitted label in the exact regular subdivision.
data RegularSiteDisposition label
  = RegularSiteVisible
  | RegularSiteLowerDimensional
  | RegularSiteHidden
  | RegularSiteCoincidentEquivalentTo !label
  | RegularSiteCoincidentDominatedBy !label
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | One oriented regular face and its exact weighted-dual vertex.
data RegularFace label = RegularFace !label !label !label !ExactPoint
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Canonically oriented labels of the face.
regularFaceLabels :: RegularFace label -> (label, label, label)
regularFaceLabels (RegularFace firstLabel secondLabel thirdLabel _) =
  (firstLabel, secondLabel, thirdLabel)

-- | Exact power vertex dual to the face.
regularFaceDualPoint :: RegularFace label -> ExactPoint
regularFaceDualPoint (RegularFace _ _ _ dualPoint) = dualPoint

-- | Exact weighted Voronoi geometry dual to one regular edge.
data PowerDualEdge
  = BoundedPowerDual !ExactSegment
  | UnboundedPowerDual !ExactRay
  | FullLinePowerDual !ExactAffineLine
  | CollapsedPowerDual !ExactPoint
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | One unordered regular edge and its exact dual geometry.
data RegularEdge label = RegularEdge !label !label !PowerDualEdge
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Ascending endpoint labels of the unordered edge.
regularEdgeLabels :: RegularEdge label -> (label, label)
regularEdgeLabels (RegularEdge firstLabel secondLabel _) =
  (firstLabel, secondLabel)

-- | Exact power-dual carrier of the edge.
regularEdgeDual :: RegularEdge label -> PowerDualEdge
regularEdgeDual (RegularEdge _ _ dual) = dual

type RegularFaceKey label = (label, label, label)
type RegularEdgeKey label = (label, label)

data RegularEdgeSection label = RegularEdgeSection
  { sectionRegularEdge :: !(RegularEdge label)
  , sectionRegularEdgeFaces :: !(Set (RegularFaceKey label))
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data RegularSiteStar label = RegularSiteStar
  { sectionIncidentFaces :: !(Set (RegularFaceKey label))
  , sectionSiteNeighbours :: !(Set label)
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data RegularLocality label = RegularLocality
  { localityFaceSeed :: !(RegularFaceKey label)
  , localityHiddenSupport :: !(Map label (RegularFaceKey label))
  , localitySupportHidden :: !(Map (RegularFaceKey label) (Set label))
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data RegularSlopeLocation label
  = RegularSlopeInside !(RegularFaceKey label)
  | RegularSlopeOutside !(RegularEdgeKey label)

data LocalRegularEditObstruction label
  = LocalRegularSectionUnavailable
  | LocalRegularLocalityUnavailable
  | LocalRegularSlopeCoincidence !label
  | LocalRegularSlopeOutside !(RegularEdgeKey label)
  | LocalRegularSlopeLocationFailed
  | LocalRegularDegenerateFace !(RegularFaceKey label)
  | LocalRegularFaceMissing !(RegularFaceKey label)
  | LocalRegularGeneratorMissing !label
  | LocalRegularNonManifoldEdge !(RegularEdgeKey label) !Int
  | LocalRegularNonConvexEdge !(RegularEdgeKey label)
  | LocalRegularEmptyHorizon
  | LocalRegularBoundaryRemoval !label
  | LocalRegularCandidateSectionEmpty
  | LocalRegularCavityBoundaryMismatch
  | LocalRegularTopologyObstruction !(RegularTopologyError label)

data RegularConflictDescent label = RegularConflictDescent
  { conflictPendingFaces :: !(Set (RegularFaceKey label))
  , conflictVisitedFaces :: !(Set (RegularFaceKey label))
  , conflictVisibleFaces :: !(Set (RegularFaceKey label))
  , conflictObstruction :: !(Maybe (LocalRegularEditObstruction label))
  }

data RegularFacePatch label = RegularFacePatch
  { patchedRegularFaces :: !(Map (RegularFaceKey label) (RegularFace label))
  , patchedRegularEdges :: !(Map (RegularEdgeKey label) (RegularEdgeSection label))
  , patchedRegularStars :: !(Map label (RegularSiteStar label))
  , patchedRegularLabels :: !(Set label)
  , patchedRegularTouchedEdges :: !(Set (RegularEdgeKey label))
  }

data RegularSection label = RegularSection
  { sectionGenerators :: !(Map label (ExactPowerGenerator label))
  , sectionSlopeRepresentatives :: !(Map (ExactRational, ExactRational) label)
  , sectionCoincidentDispositions :: !(Map label (CoincidentGeneratorDisposition label))
  , sectionRegularDispositions :: !(Map label (RegularSiteDisposition label))
  , sectionRegularFaces :: !(Map (RegularFaceKey label) (RegularFace label))
  , sectionRegularEdges :: !(Map (RegularEdgeKey label) (RegularEdgeSection label))
  , sectionRegularStars :: !(Map label (RegularSiteStar label))
  , sectionRegularReceipt :: !GeneratorRegularReceipt
  , sectionRegularLocality :: !(Maybe (RegularLocality label))
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Exact regular subdivision. Every weighted site remains resident in this
-- opaque value; one normalized section owns all derived topology and incidence.
data RegularTriangulation label = RegularTriangulation
  { storedRegularSites :: !(Map label (PowerSite label))
  , storedRegularSection :: !(Maybe (RegularSection label))
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

-- | Semantic equality is equality of the complete weighted-site section.
-- Topology and visibility are sealed derived caches of that section.
instance Eq label => Eq (RegularTriangulation label) where
  left == right = storedRegularSites left == storedRegularSites right

instance Show label => Show (RegularTriangulation label) where
  showsPrec precedence triangulation =
    showParen (precedence > 10)
      ( showString "RegularTriangulation "
          . shows (storedRegularSites triangulation)
      )

-- | Strict cardinality receipt for one normalized regular section.
data RegularTriangulationReceipt = RegularTriangulationReceipt
  { regularTriangulationInputSites :: !Int
  , regularTriangulationRepresentativeSites :: !Int
  , regularTriangulationVisibleSites :: !Int
  , regularTriangulationLowerDimensionalSites :: !Int
  , regularTriangulationHiddenSites :: !Int
  , regularTriangulationCoincidentSites :: !Int
  , regularTriangulationFaces :: !Int
  , regularTriangulationEdges :: !Int
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | One exact change in the exhaustive visibility section. Appearance and
-- disappearance are explicit rather than encoded with sentinel dispositions.
data RegularSiteTransition label
  = RegularSiteAppeared !label !(RegularSiteDisposition label)
  | RegularSiteDisappeared !label !(RegularSiteDisposition label)
  | RegularSiteTransitioned
      !label
      !(RegularSiteDisposition label)
      !(RegularSiteDisposition label)
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | A regular edit either conflicts with an existing stable identity, names
-- absent patch identities, or carries the exact normalization obstruction.
data RegularEditError label
  = RegularEditSiteConflict
      !label
      !(PowerSite label)
      !(PowerSite label)
  | RegularEditUnknownSites !(NonEmpty label)
  | RegularEditTopologyFailed !(RegularTopologyError label)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | One common publication for every pure regular-site edit. The changed-site
-- support is empty precisely for an idempotent edit.
data RegularEditResult label = RegularEditResult
  { regularEditTriangulation :: !(RegularTriangulation label)
  , regularEditChangedSites :: !(Set label)
  , regularEditTransitions :: !(Vector.Vector (RegularSiteTransition label))
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Total labelled result, opaque so callers cannot omit a submitted label.
newtype BoundedPowerDiagram label =
  BoundedPowerDiagram (Map label (PowerCellDisposition label))
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Typed admission, topology, and clipping obstructions.
data PowerDiagramError label
  = PowerSitePositionInvalid !label !PointValidationError
  | DuplicatePowerSiteLabel !label
  | PowerDomainInvalid !ExactHalfPlaneError
  | PowerBisectorInvalid !label !label !ExactHalfPlaneError
  | PowerRegularTopologyFailed !(RegularTopologyError label)
  | PowerRegularDispositionMissing !label
  | PowerRegularGeneratorMissing !label
  | PowerCellClipFailed !label !ExactClipError
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Exact construction counts and rational-width evidence for a bounded diagram.
data PowerDiagramReceipt = PowerDiagramReceipt
  { powerDiagramDomainVertices :: !Int
  , powerDiagramSubmittedSiteConstraints :: !Int
  , powerDiagramActiveBoundaries :: !Int
  , powerDiagramBoundaryCompatibilityChecks :: !Int
  , powerDiagramExactIntersections :: !Int
  , powerDiagramPublishedCells :: !Int
  , powerDiagramLowerDimensionalCells :: !Int
  , powerDiagramEmptyCells :: !Int
  , powerDiagramCoincidentEquivalentCells :: !Int
  , powerDiagramCoincidentDominatedCells :: !Int
  , powerDiagramRegularFaces :: !Int
  , powerDiagramRegularEdges :: !Int
  , powerDiagramOracleCells :: !Int
  , powerDiagramMaximumCellConstraints :: !Int
  , powerDiagramMaximumInputBits :: !Int
  , powerDiagramMaximumAffineCoefficientBits :: !Int
  , powerDiagramPeakIntermediateCoordinateBits :: !Int
  , powerDiagramFinalCoordinateBits :: !Int
  , powerDiagramFinalDenominatorBits :: !Int
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Submitted sites, derived from the exhaustive disposition partition.
powerDiagramInputSites :: PowerDiagramReceipt -> Int
powerDiagramInputSites receipt =
  powerDiagramPublishedCells receipt
    + powerDiagramLowerDimensionalCells receipt
    + powerDiagramEmptyCells receipt
    + powerDiagramCoincidentEquivalentCells receipt
    + powerDiagramCoincidentDominatedCells receipt
{-# INLINE powerDiagramInputSites #-}

-- | Peak exact-coordinate width beyond the widest admitted input.
powerDiagramPeakIntermediateBitGrowth :: PowerDiagramReceipt -> Int
powerDiagramPeakIntermediateBitGrowth receipt =
  max
    0
    ( powerDiagramPeakIntermediateCoordinateBits receipt
        - powerDiagramMaximumInputBits receipt
    )
{-# INLINE powerDiagramPeakIntermediateBitGrowth #-}

-- | Published coordinate width beyond the widest admitted input.
powerDiagramFinalCoordinateBitGrowth :: PowerDiagramReceipt -> Int
powerDiagramFinalCoordinateBitGrowth receipt =
  max
    0
    ( powerDiagramFinalCoordinateBits receipt
        - powerDiagramMaximumInputBits receipt
    )
{-# INLINE powerDiagramFinalCoordinateBitGrowth #-}

-- | One exact affine form @c0 + cx*x + cy*y@.
data AffineForm = AffineForm
  { affineFormConstant :: !ExactRational
  , affineFormXCoefficient :: !ExactRational
  , affineFormYCoefficient :: !ExactRational
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Typed obstructions from exact affine argmax decomposition.  Power-cell
-- construction remains the canonical geometric owner; window restriction is
-- the only additional boundary.
data UpperEnvelopeError label
  = UpperEnvelopeEmptyForms
  | UpperEnvelopeWindowHullFailed !MinkowskiError
  | UpperEnvelopePowerConstructionFailed !(PowerDiagramError label)
  | UpperEnvelopeWindowOverlayFailed !(OverlayError (Maybe label) Bool)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)


data CoincidentGeneratorDisposition label
  = CoincidentGeneratorEquivalentTo !label
  | CoincidentGeneratorDominatedBy !label
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data ResolvedGeneratorSection label = ResolvedGeneratorSection
  { resolvedCoincidentDispositions :: !(Map label (CoincidentGeneratorDisposition label))
  , resolvedRegularTopology :: !(GeneratorRegularTopology label)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

foldRegularFaceBoundary
  :: (label -> label -> label -> result -> result)
  -> result
  -> RegularFace label
  -> result
foldRegularFaceBoundary step initial face =
  let (firstLabel, secondLabel, thirdLabel) = regularFaceLabels face
   in step firstLabel secondLabel thirdLabel
        ( step secondLabel thirdLabel firstLabel
            (step thirdLabel firstLabel secondLabel initial)
        )
{-# INLINE foldRegularFaceBoundary #-}

regularFaceDirectedEdgeKeys :: RegularFace label -> [RegularEdgeKey label]
regularFaceDirectedEdgeKeys =
  foldRegularFaceBoundary
    (\fromLabel toLabel _ -> ((fromLabel, toLabel) :))
    []

regularFaceCanonicalEdgeKeys
  :: Ord label
  => RegularFace label
  -> [RegularEdgeKey label]
regularFaceCanonicalEdgeKeys =
  foldRegularFaceBoundary
    (\fromLabel toLabel _ -> (orderedPair fromLabel toLabel :))
    []

regularFaceLabelSet :: Ord label => RegularFace label -> Set label
regularFaceLabelSet =
  foldRegularFaceBoundary (\label _ _ -> Set.insert label) Set.empty

requireFaceEdgeWitness
  :: Ord label
  => RegularEdgeKey label
  -> RegularFace label
  -> Either (LocalRegularEditObstruction label) (label, label, label)
requireFaceEdgeWitness edgeKey face =
  maybe
    (Left (LocalRegularFaceMissing (regularFaceKey face)))
    Right
    ( foldRegularFaceBoundary
        (\fromLabel toLabel oppositeLabel later ->
            if orderedPair fromLabel toLabel == edgeKey
              then Just (fromLabel, toLabel, oppositeLabel)
              else later)
        Nothing
        face
    )

regularFaceKey :: Ord label => RegularFace label -> RegularFaceKey label
regularFaceKey face =
  let (firstLabel, secondLabel, thirdLabel) = regularFaceLabels face
   in sortedRegularFaceLabels firstLabel secondLabel thirdLabel

sortedRegularFaceLabels
  :: Ord label
  => label
  -> label
  -> label
  -> RegularFaceKey label
sortedRegularFaceLabels firstLabel secondLabel thirdLabel =
  let (firstLow, firstHigh) = orderedPair firstLabel secondLabel
      (secondLow, finalHigh) = orderedPair firstHigh thirdLabel
      (finalLow, finalMiddle) = orderedPair firstLow secondLow
   in (finalLow, finalMiddle, finalHigh)

regularEdgeKey :: Ord label => RegularEdge label -> RegularEdgeKey label
regularEdgeKey = uncurry orderedPair . regularEdgeLabels

regularFaceEdgeIncidence
  :: Ord label
  => RegularFace label
  -> Map (RegularEdgeKey label) (Set (RegularFaceKey label))
regularFaceEdgeIncidence face =
  let incidentFace = Set.singleton (regularFaceKey face)
   in foldRegularFaceBoundary
        (\fromLabel toLabel _ ->
            Map.insert (orderedPair fromLabel toLabel) incidentFace)
        Map.empty
        face

emptyRegularSiteStar :: RegularSiteStar label
emptyRegularSiteStar = RegularSiteStar Set.empty Set.empty

requireFaceFromMap
  :: Ord label
  => Map (RegularFaceKey label) (RegularFace label)
  -> RegularFaceKey label
  -> Either (LocalRegularEditObstruction label) (RegularFace label)
requireFaceFromMap faces faceKey =
  maybe (Left (LocalRegularFaceMissing faceKey)) Right (Map.lookup faceKey faces)

requireGeneratorFromMap
  :: Ord label
  => Map label (ExactPowerGenerator label)
  -> label
  -> Either (LocalRegularEditObstruction label) (ExactPowerGenerator label)
requireGeneratorFromMap generators label =
  maybe (Left (LocalRegularGeneratorMissing label)) Right (Map.lookup label generators)

regularFaceNeighbours
  :: Ord label
  => RegularSection label
  -> RegularFaceKey label
  -> Either
      (LocalRegularEditObstruction label)
      (Set (RegularFaceKey label))
regularFaceNeighbours section faceKey = do
  face <- requireFaceFromMap (sectionRegularFaces section) faceKey
  incident <- traverse requireIncidence (regularFaceCanonicalEdgeKeys face)
  pure (Set.delete faceKey (Set.unions incident))
 where
  requireIncidence edgeKey =
    maybe
      (Left (LocalRegularNonManifoldEdge edgeKey 0))
      (Right . sectionRegularEdgeFaces)
      (Map.lookup edgeKey (sectionRegularEdges section))

replaceRegularSiteSection
  :: Map label (PowerSite label)
  -> RegularSection label
  -> RegularTriangulation label
  -> RegularTriangulation label
replaceRegularSiteSection sites section triangulation =
  triangulation
    { storedRegularSites = sites
    , storedRegularSection = Just section
    }

regularDispositionSection
  :: RegularTriangulation label
  -> Map label (RegularSiteDisposition label)
regularDispositionSection =
  maybe Map.empty sectionRegularDispositions . storedRegularSection

exactGeneratorSlope
  :: ExactPowerGenerator label
  -> (ExactRational, ExactRational)
exactGeneratorSlope generator =
  ( exactPowerGeneratorXCoefficient generator
  , exactPowerGeneratorYCoefficient generator
  )
