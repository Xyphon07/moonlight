{-# LANGUAGE BangPatterns #-}
-- | Bounded clipping and planar publication of exact regular sections.
module Moonlight.Triangulation.Internal.PowerDiagram.Projection
  ( boundedPowerDiagram
  , boundedPowerDiagramFromRegular
  , powerCellDisposition
  , powerCellDispositions
  , powerDiagramPlanarLayer
  , upperEnvelope
  ) where

import Data.Bifunctor (first)
import qualified Data.Foldable as Foldable
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Vector as Vector
import Moonlight.Triangulation.Exact
  ( ExactClipDisposition (..)
  , ExactClipReceipt (..)
  , ExactAffineLine
  , ExactClosedHalfPlane
  , ExactRetainedPolygon
  , exactClipRetainedPolygon
  , exactClosedHalfPlane
  , exactRetainedPolygon
  , oppositeExactAffineLine
  )
import Moonlight.Triangulation.Internal.PowerDiagram.Generator
  ( ExactPowerGenerator (..)
  , GeneratorRegularEdge (..)
  , GeneratorRegularReceipt (..)
  , GeneratorRegularTopology (..)
  , RegularGeneratorDisposition (..)
  , exactGeneratorAxis
  )
import Moonlight.Triangulation.Internal.PowerDiagram.Model
import Moonlight.Triangulation.Internal.PowerDiagram.Section
import Moonlight.Triangulation.Internal.Minkowski.Convex
  ( convexHullPolygon
  , convexPolygonComponent
  , convexPolygonFromRetained
  )
import Moonlight.Triangulation.Internal.Overlay.Types
  ( OverlayCell (..)
  , OverlayCellGeometry (..)
  , OverlayResult (..)
  )
import Moonlight.Triangulation.Internal.Region.Publication
  ( planarLayerFromAdmittedComponents
  )
import Moonlight.Triangulation.Minkowski
  ( ConvexPolygon
  , convexPolygonPoints
  )
import Moonlight.Triangulation.Overlay
  ( overlayLayers )
import Moonlight.Triangulation.Region
  ( PlanarLayer
  , PolygonComponent
  , exactLoopPoints
  , polygonOuterLoop
  )

-- | Construct and clip a normalized regular subdivision to one convex domain.
boundedPowerDiagram
  :: Ord label
  => ConvexPolygon
  -> NonEmpty (PowerSite label)
  -> Either (PowerDiagramError label) (BoundedPowerDiagram label, PowerDiagramReceipt)
boundedPowerDiagram domain submitted = do
  sortedSites <- validateAndSortSites submitted
  let preparedGenerators = fmap prepareExactPowerGenerator sortedSites
      generators = fmap fst preparedGenerators
      siteInputBits =
        Foldable.foldl' (\bits preparation -> max bits (snd preparation)) 0 preparedGenerators
  (dispositions, clipReceipt, regularReceipt, maximumCellConstraints) <-
    exactGeneratorDispositionsWith (<>) mempty domain generators
  let receipt =
        aggregateReceipt
          domain
          dispositions
          siteInputBits
          clipReceipt
          regularReceipt
          maximumCellConstraints
  pure (BoundedPowerDiagram dispositions, receipt)

-- | Clip an already normalized regular value without rebuilding its lifted
-- hull. The empty regular value yields an empty labelled diagram.
boundedPowerDiagramFromRegular
  :: Ord label
  => ConvexPolygon
  -> RegularTriangulation label
  -> Either (PowerDiagramError label) (BoundedPowerDiagram label, PowerDiagramReceipt)
boundedPowerDiagramFromRegular domain triangulation =
  case storedRegularSection triangulation of
    Nothing ->
      Right
        ( BoundedPowerDiagram Map.empty
        , aggregateReceipt
            domain
            Map.empty
            0
            mempty
            emptyGeneratorRegularReceipt
            0
        )
    Just section -> do
      (dispositions, clipReceipt, generatorReceipt, maximumCellConstraints) <-
        exactRegularSectionDispositionsWith (<>) mempty domain section
      let receipt =
            aggregateReceipt
              domain
              dispositions
              (maximumPowerSiteInputBits (storedRegularSites triangulation))
              clipReceipt
              generatorReceipt
              maximumCellConstraints
      pure (BoundedPowerDiagram dispositions, receipt)

-- | Full-dimensional labelled argmax regions for affine forms
-- @c0 + cx*x + cy*y@ inside an admitted polygonal window. The result is a
-- planar projection: lower-dimensional and empty winners intentionally have
-- no region. Use 'regularTriangulation' on corresponding weighted sites when
-- those dispositions or exact unbounded duals are required. Identical forms
-- choose the least label, independent of map construction order.
upperEnvelope
  :: Ord label
  => PolygonComponent
  -> Map label AffineForm
  -> Either (UpperEnvelopeError label) (PlanarLayer (Maybe label))
upperEnvelope window forms = do
  generators <- affineFormGenerators forms
  domain <-
    first UpperEnvelopeWindowHullFailed
      (convexHullPolygon (exactLoopPoints (polygonOuterLoop window)))
  (dispositions, _, _, _) <-
    first UpperEnvelopePowerConstructionFailed
      (exactGeneratorDispositionsWith discardClipReceipt () domain generators)
  let envelopeLayer = affineDispositionLayer dispositions
  if convexPolygonComponent domain == window
    then Right envelopeLayer
    else restrictEnvelopeToWindow window envelopeLayer

-- | Look up the exhaustive bounded-cell disposition for one submitted label.
powerCellDisposition
  :: Ord label
  => label
  -> BoundedPowerDiagram label
  -> Maybe (PowerCellDisposition label)
powerCellDisposition label (BoundedPowerDiagram dispositions) =
  Map.lookup label dispositions

-- | Enumerate every submitted label and bounded-cell disposition in order.
powerCellDispositions
  :: BoundedPowerDiagram label
  -> [(label, PowerCellDisposition label)]
powerCellDispositions (BoundedPowerDiagram dispositions) = Map.toAscList dispositions

-- | Publish the full-dimensional cell layer with a canonical exterior.
powerDiagramPlanarLayer
  :: Ord label
  => BoundedPowerDiagram label
  -> PlanarLayer (Maybe label)
powerDiagramPlanarLayer (BoundedPowerDiagram dispositions) =
  affineDispositionLayer dispositions

affineDispositionLayer
  :: Ord label
  => Map label (PowerCellDisposition label)
  -> PlanarLayer (Maybe label)
affineDispositionLayer = publishedPowerLayer Nothing Just

publishedPowerLayer
  :: Ord publishedLabel
  => publishedLabel
  -> (label -> publishedLabel)
  -> Map label (PowerCellDisposition label)
  -> PlanarLayer publishedLabel
publishedPowerLayer outside publishLabel dispositions =
  planarLayerFromAdmittedComponents
    outside
    [ (publishLabel label, convexPolygonComponent polygon)
    | (label, PublishedPowerCell polygon) <- Map.toAscList dispositions
    ]

restrictEnvelopeToWindow
  :: Ord label
  => PolygonComponent
  -> PlanarLayer (Maybe label)
  -> Either (UpperEnvelopeError label) (PlanarLayer (Maybe label))
restrictEnvelopeToWindow window envelopeLayer = do
  let windowLayer = planarLayerFromAdmittedComponents False [(True, window)]
  clipped <-
    first UpperEnvelopeWindowOverlayFailed
      (overlayLayers envelopeLayer windowLayer)
  pure
    ( planarLayerFromAdmittedComponents
        Nothing
        [ (Just label, component)
        | cell <- Vector.toList (overlayResultCells clipped)
        , overlayCellRight cell
        , Just label <- [overlayCellLeft cell]
        , BoundedOverlayCell component <- [overlayCellGeometry cell]
        ]
    )


affineFormGenerators
  :: Map label AffineForm
  -> Either (UpperEnvelopeError label) (NonEmpty (ExactPowerGenerator label))
affineFormGenerators forms =
  case Map.minViewWithKey forms of
    Nothing -> Left UpperEnvelopeEmptyForms
    Just ((firstLabel, firstForm), remaining) ->
      Right
        ( affineFormGenerator firstLabel firstForm
            :| fmap (uncurry affineFormGenerator) (Map.toAscList remaining)
        )

affineFormGenerator
  :: label
  -> AffineForm
  -> ExactPowerGenerator label
affineFormGenerator label form =
  ExactPowerGenerator
    { exactPowerGeneratorLabel = label
    , exactPowerGeneratorXCoefficient = affineFormXCoefficient form
    , exactPowerGeneratorYCoefficient = affineFormYCoefficient form
    , exactPowerGeneratorConstant = affineFormConstant form
    }

exactGeneratorDispositionsWith
  :: Ord label
  => (summary -> ExactClipReceipt -> summary)
  -> summary
  -> ConvexPolygon
  -> NonEmpty (ExactPowerGenerator label)
  -> Either
      (PowerDiagramError label)
      ( Map label (PowerCellDisposition label)
      , summary
      , GeneratorRegularReceipt
      , Int
      )
exactGeneratorDispositionsWith summarizeReceipt initialSummary domain generators = do
  section <- first PowerRegularTopologyFailed (resolvedGeneratorSection generators)
  exactResolvedGeneratorDispositionsWith
    summarizeReceipt
    initialSummary
    domain
    section

exactResolvedGeneratorDispositionsWith
  :: Ord label
  => (summary -> ExactClipReceipt -> summary)
  -> summary
  -> ConvexPolygon
  -> ResolvedGeneratorSection label
  -> Either
      (PowerDiagramError label)
      ( Map label (PowerCellDisposition label)
      , summary
      , GeneratorRegularReceipt
      , Int
      )
exactResolvedGeneratorDispositionsWith summarizeReceipt initialSummary domain section = do
  retainedDomain <-
    first PowerDomainInvalid (exactRetainedPolygon (convexPolygonPoints domain))
  let topology = resolvedRegularTopology section
  constraints <- prepareRegularConstraintSection topology
  clipped <-
    traverse
      (clipPowerCell retainedDomain section constraints)
      (generatorRegularDispositions topology)
  let (dispositions, summary, maximumCellConstraints) =
        summarizeClippedPowerCells
          summarizeReceipt
          initialSummary
          (resolvedCoincidentDispositions section)
          (NonEmpty.toList clipped)
  pure
    ( dispositions
    , summary
    , generatorRegularReceipt topology
    , maximumCellConstraints
    )

exactRegularSectionDispositionsWith
  :: Ord label
  => (summary -> ExactClipReceipt -> summary)
  -> summary
  -> ConvexPolygon
  -> RegularSection label
  -> Either
      (PowerDiagramError label)
      ( Map label (PowerCellDisposition label)
      , summary
      , GeneratorRegularReceipt
      , Int
      )
exactRegularSectionDispositionsWith summarizeReceipt initialSummary domain section = do
  retainedDomain <-
    first PowerDomainInvalid (exactRetainedPolygon (convexPolygonPoints domain))
  constraints <- preparePublishedRegularConstraints section
  representativeDispositions <-
    traverse
      (\(label, generator) -> do
         disposition <-
           maybe
             (Left (PowerRegularDispositionMissing label))
             Right
             (Map.lookup label (sectionRegularDispositions section))
         clipPublishedPowerCell
           retainedDomain
           section
           constraints
           generator
           disposition)
      (Map.toAscList (sectionGenerators section))
  let (dispositions, summary, maximumCellConstraints) =
        summarizeClippedPowerCells
          summarizeReceipt
          initialSummary
          (sectionCoincidentDispositions section)
          representativeDispositions
  pure
    ( dispositions
    , summary
    , sectionRegularReceipt section
    , maximumCellConstraints
    )

summarizeClippedPowerCells
  :: Ord label
  => (summary -> ExactClipReceipt -> summary)
  -> summary
  -> Map label (CoincidentGeneratorDisposition label)
  -> [(label, PowerCellDisposition label, ExactClipReceipt, Int)]
  -> (Map label (PowerCellDisposition label), summary, Int)
summarizeClippedPowerCells summarizeReceipt initialSummary coincident clipped =
  let (publishedAssociations, summary, maximumCellConstraints) =
        Foldable.foldl'
          (\(associations, accumulatedSummary, peak) (label, disposition, cellReceipt, cellAxes) ->
             let !combinedSummary = summarizeReceipt accumulatedSummary cellReceipt
              in ( (label, disposition) : associations
                 , combinedSummary
                 , max peak cellAxes
                 ))
          ([], initialSummary, 0)
          clipped
   in ( fmap coincidentPowerDisposition coincident
          <> Map.fromList publishedAssociations
      , summary
      , maximumCellConstraints
      )

preparePublishedRegularConstraints
  :: Ord label
  => RegularSection label
  -> Either (PowerDiagramError label) (Map label [ExactClosedHalfPlane])
preparePublishedRegularConstraints section =
  Foldable.foldlM prepare Map.empty (Map.elems (sectionRegularEdges section))
 where
  prepare constraints edgeSection = do
    let edge = sectionRegularEdge edgeSection
        (firstLabel, secondLabel) = regularEdgeLabels edge
    firstGenerator <- requireSectionGenerator section firstLabel
    secondGenerator <- requireSectionGenerator section secondLabel
    insertRegularConstraintEdge constraints firstGenerator secondGenerator

clipPublishedPowerCell
  :: Ord label
  => ExactRetainedPolygon
  -> RegularSection label
  -> Map label [ExactClosedHalfPlane]
  -> ExactPowerGenerator label
  -> RegularSiteDisposition label
  -> Either
      (PowerDiagramError label)
      (label, PowerCellDisposition label, ExactClipReceipt, Int)
clipPublishedPowerCell retainedDomain section constraints ownerGenerator disposition =
  let ownerLabel = exactPowerGeneratorLabel ownerGenerator
      competitors =
        filter
          ((/= ownerLabel) . exactPowerGeneratorLabel)
          (Map.elems (sectionGenerators section))
      clipAs =
        clipRegularGeneratorCell
          retainedDomain
          ownerGenerator
          (Map.findWithDefault [] ownerLabel constraints)
          competitors
   in case disposition of
        RegularSiteHidden -> clipAs RegularGeneratorHidden
        RegularSiteVisible -> clipAs RegularGeneratorVisible
        RegularSiteLowerDimensional -> clipAs RegularGeneratorLowerDimensional
        RegularSiteCoincidentEquivalentTo _ ->
          Left (PowerRegularDispositionMissing ownerLabel)
        RegularSiteCoincidentDominatedBy _ ->
          Left (PowerRegularDispositionMissing ownerLabel)

requireSectionGenerator
  :: Ord label
  => RegularSection label
  -> label
  -> Either (PowerDiagramError label) (ExactPowerGenerator label)
requireSectionGenerator section label =
  maybe
    (Left (PowerRegularGeneratorMissing label))
    Right
    (Map.lookup label (sectionGenerators section))

discardClipReceipt :: () -> ExactClipReceipt -> ()
discardClipReceipt _ _ = ()

prepareRadicalAxis
  :: ExactPowerGenerator label
  -> ExactPowerGenerator label
  -> Either (PowerDiagramError label) ExactAffineLine
prepareRadicalAxis firstGenerator secondGenerator =
  first
    ( PowerBisectorInvalid
        (exactPowerGeneratorLabel firstGenerator)
        (exactPowerGeneratorLabel secondGenerator)
    )
    (exactGeneratorAxis firstGenerator secondGenerator)

prepareRegularConstraintSection
  :: Ord label
  => GeneratorRegularTopology label
  -> Either (PowerDiagramError label) (Map label [ExactClosedHalfPlane])
prepareRegularConstraintSection =
  Foldable.foldlM prepareRegularConstraintEdge Map.empty . generatorRegularEdges

prepareRegularConstraintEdge
  :: Ord label
  => Map label [ExactClosedHalfPlane]
  -> GeneratorRegularEdge label
  -> Either (PowerDiagramError label) (Map label [ExactClosedHalfPlane])
prepareRegularConstraintEdge constraints edge = do
  let firstGenerator = generatorRegularEdgeFirst edge
      secondGenerator = generatorRegularEdgeSecond edge
  insertRegularConstraintEdge constraints firstGenerator secondGenerator

insertRegularConstraintEdge
  :: Ord label
  => Map label [ExactClosedHalfPlane]
  -> ExactPowerGenerator label
  -> ExactPowerGenerator label
  -> Either (PowerDiagramError label) (Map label [ExactClosedHalfPlane])
insertRegularConstraintEdge constraints firstGenerator secondGenerator = do
  let firstLabel = exactPowerGeneratorLabel firstGenerator
      secondLabel = exactPowerGeneratorLabel secondGenerator
  axis <- prepareRadicalAxis firstGenerator secondGenerator
  pure
    ( Map.insertWith (<>) secondLabel [exactClosedHalfPlane (oppositeExactAffineLine axis)]
        (Map.insertWith (<>) firstLabel [exactClosedHalfPlane axis] constraints)
    )

clipPowerCell
  :: Ord label
  => ExactRetainedPolygon
  -> ResolvedGeneratorSection label
  -> Map label [ExactClosedHalfPlane]
  -> (ExactPowerGenerator label, RegularGeneratorDisposition)
  -> Either
      (PowerDiagramError label)
      (label, PowerCellDisposition label, ExactClipReceipt, Int)
clipPowerCell retainedDomain section constraints (ownerGenerator, disposition) =
  let ownerLabel = exactPowerGeneratorLabel ownerGenerator
      topology = resolvedRegularTopology section
      competitors =
        [ generator
        | (generator, _) <-
            NonEmpty.toList (generatorRegularDispositions topology)
        , exactPowerGeneratorLabel generator /= ownerLabel
        ]
   in clipRegularGeneratorCell
        retainedDomain
        ownerGenerator
        (Map.findWithDefault [] ownerLabel constraints)
        competitors
        disposition

clipRegularGeneratorCell
  :: ExactRetainedPolygon
  -> ExactPowerGenerator label
  -> [ExactClosedHalfPlane]
  -> [ExactPowerGenerator label]
  -> RegularGeneratorDisposition
  -> Either
      (PowerDiagramError label)
      (label, PowerCellDisposition label, ExactClipReceipt, Int)
clipRegularGeneratorCell retainedDomain ownerGenerator activeConstraints competitors disposition =
  case disposition of
    RegularGeneratorHidden ->
      pure (exactPowerGeneratorLabel ownerGenerator, EmptyPowerCell, mempty, 0)
    RegularGeneratorVisible ->
      finishPowerCell retainedDomain ownerGenerator activeConstraints
    RegularGeneratorLowerDimensional -> do
      halfPlanes <- traverse (preparedDirectHalfPlane ownerGenerator) competitors
      finishPowerCell retainedDomain ownerGenerator halfPlanes

preparedDirectHalfPlane
  :: ExactPowerGenerator label
  -> ExactPowerGenerator label
  -> Either (PowerDiagramError label) ExactClosedHalfPlane
preparedDirectHalfPlane owner competitor =
  exactClosedHalfPlane <$> prepareRadicalAxis owner competitor

finishPowerCell
  :: ExactRetainedPolygon
  -> ExactPowerGenerator label
  -> [ExactClosedHalfPlane]
  -> Either
      (PowerDiagramError label)
      (label, PowerCellDisposition label, ExactClipReceipt, Int)
finishPowerCell retainedDomain ownerGenerator halfPlanes = do
  (exactDisposition, receipt) <-
    first (PowerCellClipFailed (exactPowerGeneratorLabel ownerGenerator))
      (exactClipRetainedPolygon retainedDomain halfPlanes)
  pure
    ( exactPowerGeneratorLabel ownerGenerator
    , case exactDisposition of
        ExactClipFullDimensional retained ->
          PublishedPowerCell (convexPolygonFromRetained retained)
        ExactClipLowerDimensional points -> LowerDimensionalPowerCell points
        ExactClipEmpty -> EmptyPowerCell
    , receipt
    , length halfPlanes
    )

aggregateReceipt
  :: ConvexPolygon
  -> Map label (PowerCellDisposition label)
  -> Int
  -> ExactClipReceipt
  -> GeneratorRegularReceipt
  -> Int
  -> PowerDiagramReceipt
aggregateReceipt domain dispositions siteInputBits clipReceipt regularReceipt maximumCellConstraints =
  let inputBits = max siteInputBits (exactClipInputCoordinateBits clipReceipt)
      peakBits = exactClipPeakIntermediateCoordinateBits clipReceipt
      finalBits = exactClipFinalCoordinateBits clipReceipt
      dispositionCounts = countPowerDispositions dispositions
   in PowerDiagramReceipt
        { powerDiagramDomainVertices = NonEmpty.length (convexPolygonPoints domain)
        , powerDiagramSubmittedSiteConstraints = exactClipSubmittedHalfPlanes clipReceipt
        , powerDiagramActiveBoundaries = exactClipActiveBoundaries clipReceipt
        , powerDiagramBoundaryCompatibilityChecks = exactClipBoundaryCompatibilityChecks clipReceipt
        , powerDiagramExactIntersections = exactClipExactIntersections clipReceipt
        , powerDiagramPublishedCells = countedPublishedCells dispositionCounts
        , powerDiagramLowerDimensionalCells = countedLowerDimensionalCells dispositionCounts
        , powerDiagramEmptyCells = countedEmptyCells dispositionCounts
        , powerDiagramCoincidentEquivalentCells = countedCoincidentEquivalentCells dispositionCounts
        , powerDiagramCoincidentDominatedCells = countedCoincidentDominatedCells dispositionCounts
        , powerDiagramRegularFaces = generatorRegularFaceCount regularReceipt
        , powerDiagramRegularEdges = generatorRegularEdgeCount regularReceipt
        , powerDiagramOracleCells = generatorRegularLowerDimensionalSites regularReceipt
        , powerDiagramMaximumCellConstraints = maximumCellConstraints
        , powerDiagramMaximumInputBits = inputBits
        , powerDiagramMaximumAffineCoefficientBits = exactClipMaximumAffineCoefficientBits clipReceipt
        , powerDiagramPeakIntermediateCoordinateBits = peakBits
        , powerDiagramFinalCoordinateBits = finalBits
        , powerDiagramFinalDenominatorBits = exactClipFinalDenominatorBits clipReceipt
        }

data PowerDispositionCounts = PowerDispositionCounts
  { countedPublishedCells :: !Int
  , countedLowerDimensionalCells :: !Int
  , countedEmptyCells :: !Int
  , countedCoincidentEquivalentCells :: !Int
  , countedCoincidentDominatedCells :: !Int
  }

countPowerDispositions
  :: Map label (PowerCellDisposition label)
  -> PowerDispositionCounts
countPowerDispositions =
  Map.foldl'
    (\counts disposition -> case disposition of
        PublishedPowerCell _ ->
          counts {countedPublishedCells = countedPublishedCells counts + 1}
        LowerDimensionalPowerCell _ ->
          counts {countedLowerDimensionalCells = countedLowerDimensionalCells counts + 1}
        EmptyPowerCell ->
          counts {countedEmptyCells = countedEmptyCells counts + 1}
        CoincidentEquivalentTo _ ->
          counts {countedCoincidentEquivalentCells = countedCoincidentEquivalentCells counts + 1}
        CoincidentDominatedBy _ ->
          counts {countedCoincidentDominatedCells = countedCoincidentDominatedCells counts + 1})
    PowerDispositionCounts
      { countedPublishedCells = 0
      , countedLowerDimensionalCells = 0
      , countedEmptyCells = 0
      , countedCoincidentEquivalentCells = 0
      , countedCoincidentDominatedCells = 0
      }
