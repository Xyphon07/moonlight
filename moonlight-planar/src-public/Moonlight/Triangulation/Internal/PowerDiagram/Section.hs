-- | Batch normalization and publication of one exact regular section.
module Moonlight.Triangulation.Internal.PowerDiagram.Section where

import Data.Bifunctor (first)
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Ord (comparing)
import qualified Data.Set as Set
import Data.Set (Set)
import Moonlight.Triangulation.Exact
  ( ExactPoint
  , exactPointBitWidth
  , exactPointCoordinates
  , exactPointFromQueryPoint
  )
import Moonlight.Triangulation.Internal.BoundaryCycle (consecutivePairs)
import Moonlight.Triangulation.Internal.ExactRational (exactRationalBitWidth)
import Moonlight.Triangulation.Internal.PowerDiagram.Generator
import Moonlight.Triangulation.Internal.PowerDiagram.Hull (regularGeneratorTopology)
import Moonlight.Triangulation.Internal.PowerDiagram.Locality (buildRegularLocality)
import Moonlight.Triangulation.Internal.PowerDiagram.Model

regularTriangulation
  :: Ord label
  => NonEmpty (PowerSite label)
  -> Either
      (PowerDiagramError label)
      (RegularTriangulation label, RegularTriangulationReceipt)
regularTriangulation submitted = do
  sortedSites <- validateAndSortSites submitted
  triangulation <-
    first PowerRegularTopologyFailed
      ( constructRegularTriangulation
          (Map.fromDistinctAscList (fmap (\site -> (powerSiteLabel site, site)) (NonEmpty.toList sortedSites)))
      )
  pure (triangulation, regularTriangulationReceipt triangulation)

-- | The empty all-site section. It is the identity for insertion and the
-- result of removing the final site.
emptyRegularTriangulation :: RegularTriangulation label
emptyRegularTriangulation =
  RegularTriangulation
    { storedRegularSites = Map.empty
    , storedRegularSection = Nothing
    }

-- | Look up one canonical site by its stable label.
regularSite
  :: Ord label
  => label
  -> RegularTriangulation label
  -> Maybe (PowerSite label)
regularSite label = Map.lookup label . storedRegularSites

-- | Enumerate every canonical site in ascending label order, including hidden
-- and coincident sites.
regularSites :: RegularTriangulation label -> [PowerSite label]
regularSites = Map.elems . storedRegularSites

-- | Number of canonical sites, independent of their visibility.
regularSiteCount :: RegularTriangulation label -> Int
regularSiteCount = Map.size . storedRegularSites

-- | Observe one site's exhaustive derived visibility disposition.
regularSiteDisposition
  :: Ord label
  => label
  -> RegularTriangulation label
  -> Maybe (RegularSiteDisposition label)
regularSiteDisposition label triangulation =
  storedRegularSection triangulation
    >>= Map.lookup label . sectionRegularDispositions

-- | Enumerate canonical visible faces in ascending key order.
regularFaces :: RegularTriangulation label -> [RegularFace label]
regularFaces =
  maybe [] (Map.elems . sectionRegularFaces) . storedRegularSection

-- | Enumerate canonical regular edges in ascending endpoint order.
regularEdges :: RegularTriangulation label -> [RegularEdge label]
regularEdges =
  maybe [] (fmap sectionRegularEdge . Map.elems . sectionRegularEdges)
    . storedRegularSection

-- | Exact neighbours of one visible site; nonvisible and absent sites have none.
regularNeighbours
  :: Ord label
  => label
  -> RegularTriangulation label
  -> Set label
regularNeighbours label =
  maybe
    Set.empty
    ( maybe Set.empty sectionSiteNeighbours
        . Map.lookup label
        . sectionRegularStars
    )
    . storedRegularSection

-- | Construction statistics derived from the sealed normalized section.
regularTriangulationReceipt
  :: RegularTriangulation label
  -> RegularTriangulationReceipt
regularTriangulationReceipt triangulation =
  case storedRegularSection triangulation of
    Nothing -> emptyRegularTriangulationReceipt
    Just section ->
      regularReceiptFromSection (regularSiteCount triangulation) section

validateAndSortSites
  :: Ord label
  => NonEmpty (PowerSite label)
  -> Either (PowerDiagramError label) (NonEmpty (PowerSite label))
validateAndSortSites submitted =
  let sorted = NonEmpty.sortBy (comparing powerSiteLabel) submitted
   in case List.find (uncurry sameLabel) (consecutivePairs (NonEmpty.toList sorted)) of
        Just (duplicate, _) -> Left (DuplicatePowerSiteLabel (powerSiteLabel duplicate))
        Nothing -> Right sorted
 where
  sameLabel :: Eq label => PowerSite label -> PowerSite label -> Bool
  sameLabel left right = powerSiteLabel left == powerSiteLabel right

maximumPowerSiteInputBits :: Map label (PowerSite label) -> Int
maximumPowerSiteInputBits =
  Map.foldl' (\bits site -> max bits (powerSiteInputBitWidth site)) 0

constructRegularTriangulation
  :: Ord label
  => Map label (PowerSite label)
  -> Either (RegularTopologyError label) (RegularTriangulation label)
constructRegularTriangulation sites =
  case NonEmpty.nonEmpty (Map.elems sites) of
    Nothing -> Right emptyRegularTriangulation
    Just sortedSites -> do
      let generators = fmap (fst . prepareExactPowerGenerator) sortedSites
      section <- resolvedGeneratorSection generators
      pure (publishRegularSection sites section)

prepareExactPowerGenerator :: PowerSite label -> (ExactPowerGenerator label, Int)
prepareExactPowerGenerator site =
  let point = exactPointFromQueryPoint (powerSiteQueryPoint site)
      (coordinateX, coordinateY) = exactPointCoordinates point
      weight = powerWeightExact (powerSiteWeight site)
   in ( ExactPowerGenerator
          { exactPowerGeneratorLabel = powerSiteLabel site
          , exactPowerGeneratorXCoefficient = 2 * coordinateX
          , exactPowerGeneratorYCoefficient = 2 * coordinateY
          , exactPowerGeneratorConstant =
              weight - coordinateX * coordinateX - coordinateY * coordinateY
          }
      , max (exactPointBitWidth point) (exactRationalBitWidth weight)
      )

powerSiteInputBitWidth :: PowerSite label -> Int
powerSiteInputBitWidth site =
  max
    (exactPointBitWidth (exactPointFromQueryPoint (powerSiteQueryPoint site)))
    (exactRationalBitWidth (powerWeightExact (powerSiteWeight site)))

groupGeneratorsBySlope
  :: NonEmpty (ExactPowerGenerator label)
  -> NonEmpty (NonEmpty (ExactPowerGenerator label))
groupGeneratorsBySlope (initial :| remaining) =
  let initialSlope = exactGeneratorSlope initial
      (sameInitialSlope, otherGenerators) =
        List.partition ((== initialSlope) . exactGeneratorSlope) remaining
      otherGroups =
        Map.fromListWith
          (<>)
          [ (exactGeneratorSlope generator, generator :| [])
          | generator <- otherGenerators
          ]
   in (initial :| sameInitialSlope) :| Map.elems otherGroups

resolveCoincidentGeneratorGroup
  :: Ord label
  => NonEmpty (ExactPowerGenerator label)
  -> (ExactPowerGenerator label, [(label, CoincidentGeneratorDisposition label)])
resolveCoincidentGeneratorGroup generators@(initial :| remaining) =
  let representative = List.foldl' chooseCoincidentRepresentative initial remaining
      representativeLabel = exactPowerGeneratorLabel representative
   in ( representative
      , [ ( exactPowerGeneratorLabel generator
          , classifyCoincidentGenerator representative generator
          )
        | generator <- NonEmpty.toList generators
        , exactPowerGeneratorLabel generator /= representativeLabel
        ]
      )

chooseCoincidentRepresentative
  :: Ord label
  => ExactPowerGenerator label
  -> ExactPowerGenerator label
  -> ExactPowerGenerator label
chooseCoincidentRepresentative selected candidate =
  case
    compareCoincidentPriority
      (exactPowerGeneratorConstant candidate, exactPowerGeneratorLabel candidate)
      (exactPowerGeneratorConstant selected, exactPowerGeneratorLabel selected) of
    GT -> candidate
    _ -> selected

compareCoincidentPriority
  :: (Ord label, Ord value)
  => (value, label)
  -> (value, label)
  -> Ordering
compareCoincidentPriority (candidateValue, candidateLabel) (selectedValue, selectedLabel) =
  compare candidateValue selectedValue <> compare selectedLabel candidateLabel

classifyCoincidentGenerator
  :: ExactPowerGenerator label
  -> ExactPowerGenerator label
  -> CoincidentGeneratorDisposition label
classifyCoincidentGenerator representative candidate =
  classifyCoincidentValue
    (exactPowerGeneratorLabel representative)
    (exactPowerGeneratorConstant representative)
    (exactPowerGeneratorConstant candidate)

classifyCoincidentValue
  :: Eq value
  => label
  -> value
  -> value
  -> CoincidentGeneratorDisposition label
classifyCoincidentValue representativeLabel representativeValue candidateValue
  | candidateValue == representativeValue =
      CoincidentGeneratorEquivalentTo representativeLabel
  | otherwise = CoincidentGeneratorDominatedBy representativeLabel


coincidentPowerDisposition
  :: CoincidentGeneratorDisposition label
  -> PowerCellDisposition label
coincidentPowerDisposition disposition =
  case disposition of
    CoincidentGeneratorEquivalentTo label -> CoincidentEquivalentTo label
    CoincidentGeneratorDominatedBy label -> CoincidentDominatedBy label

coincidentRegularDisposition
  :: CoincidentGeneratorDisposition label
  -> RegularSiteDisposition label
coincidentRegularDisposition disposition =
  case disposition of
    CoincidentGeneratorEquivalentTo label -> RegularSiteCoincidentEquivalentTo label
    CoincidentGeneratorDominatedBy label -> RegularSiteCoincidentDominatedBy label


resolvedGeneratorSection
  :: Ord label
  => NonEmpty (ExactPowerGenerator label)
  -> Either (RegularTopologyError label) (ResolvedGeneratorSection label)
resolvedGeneratorSection generators = do
  let resolvedGroups = fmap resolveCoincidentGeneratorGroup (groupGeneratorsBySlope generators)
      representatives = fmap fst resolvedGroups
      coincidentDispositions =
        Map.fromList (concatMap snd (NonEmpty.toList resolvedGroups))
  topology <- regularGeneratorTopology (DistinctSlopeGenerators representatives)
  pure
    ResolvedGeneratorSection
      { resolvedCoincidentDispositions = coincidentDispositions
      , resolvedRegularTopology = topology
      }

publishRegularSection
  :: Ord label
  => Map label (PowerSite label)
  -> ResolvedGeneratorSection label
  -> RegularTriangulation label
publishRegularSection sites resolvedSection =
  let topology = resolvedRegularTopology resolvedSection
      coincident = resolvedCoincidentDispositions resolvedSection
      representativeDispositions = publishRepresentativeDispositions topology
      dispositions =
        representativeDispositions <> fmap coincidentRegularDisposition coincident
      generators =
        Map.fromList
          [ (exactPowerGeneratorLabel generator, generator)
          | (generator, _) <- NonEmpty.toList (generatorRegularDispositions topology)
          ]
      faces = fmap publishGeneratorFace (generatorRegularFaces topology)
      faceSection = Map.fromList (fmap (\face -> (regularFaceKey face, face)) faces)
      edgeFaceIncidence =
        List.foldl'
          (Map.unionWith Set.union)
          Map.empty
          (fmap regularFaceEdgeIncidence faces)
      edges = fmap publishGeneratorEdge (generatorRegularEdges topology)
      edgeSection =
        Map.fromList
          [ ( key
            , RegularEdgeSection
                edge
                (Map.findWithDefault Set.empty key edgeFaceIncidence)
            )
          | edge <- edges
          , let key = regularEdgeKey edge
          ]
      stars = regularSiteStars dispositions faceSection edgeSection
      baseSection =
        RegularSection
          { sectionGenerators = generators
          , sectionSlopeRepresentatives =
              Map.fromList
                [ (exactGeneratorSlope generator, label)
                | (label, generator) <- Map.toAscList generators
                ]
          , sectionCoincidentDispositions = coincident
          , sectionRegularDispositions = dispositions
          , sectionRegularFaces = faceSection
          , sectionRegularEdges = edgeSection
          , sectionRegularStars = stars
          , sectionRegularReceipt = generatorRegularReceipt topology
          , sectionRegularLocality = Nothing
          }
      section =
        baseSection
          { sectionRegularLocality = buildRegularLocality baseSection
          }
   in RegularTriangulation
        { storedRegularSites = sites
        , storedRegularSection = Just section
        }

regularReceiptFromSection
  :: Int
  -> RegularSection label
  -> RegularTriangulationReceipt
regularReceiptFromSection inputSites section =
  let generatorReceipt = sectionRegularReceipt section
      coincidentCount = Map.size (sectionCoincidentDispositions section)
   in RegularTriangulationReceipt
        { regularTriangulationInputSites = inputSites
        , regularTriangulationRepresentativeSites =
            generatorRegularInputSites generatorReceipt
        , regularTriangulationVisibleSites =
            generatorRegularVisibleSites generatorReceipt
        , regularTriangulationLowerDimensionalSites =
            generatorRegularLowerDimensionalSites generatorReceipt
        , regularTriangulationHiddenSites =
            generatorRegularHiddenSites generatorReceipt
        , regularTriangulationCoincidentSites = coincidentCount
        , regularTriangulationFaces = generatorRegularFaceCount generatorReceipt
        , regularTriangulationEdges = generatorRegularEdgeCount generatorReceipt
        }

emptyRegularTriangulationReceipt :: RegularTriangulationReceipt
emptyRegularTriangulationReceipt =
  RegularTriangulationReceipt
    { regularTriangulationInputSites = 0
    , regularTriangulationRepresentativeSites = 0
    , regularTriangulationVisibleSites = 0
    , regularTriangulationLowerDimensionalSites = 0
    , regularTriangulationHiddenSites = 0
    , regularTriangulationCoincidentSites = 0
    , regularTriangulationFaces = 0
    , regularTriangulationEdges = 0
    }

emptyGeneratorRegularReceipt :: GeneratorRegularReceipt
emptyGeneratorRegularReceipt =
  GeneratorRegularReceipt
    { generatorRegularInputSites = 0
    , generatorRegularVisibleSites = 0
    , generatorRegularLowerDimensionalSites = 0
    , generatorRegularHiddenSites = 0
    , generatorRegularFaceCount = 0
    , generatorRegularEdgeCount = 0
    }

publishGeneratorDisposition
  :: RegularGeneratorDisposition
  -> RegularSiteDisposition label
publishGeneratorDisposition disposition =
  case disposition of
    RegularGeneratorVisible -> RegularSiteVisible
    RegularGeneratorLowerDimensional -> RegularSiteLowerDimensional
    RegularGeneratorHidden -> RegularSiteHidden

publishGeneratorFace
  :: Ord label
  => GeneratorRegularFace label
  -> RegularFace label
publishGeneratorFace face =
  canonicalRegularFace
    (generatorRegularFaceFirst face)
    (generatorRegularFaceSecond face)
    (generatorRegularFaceThird face)
    (generatorRegularFaceDualPoint face)

canonicalRegularFace
  :: Ord label
  => label
  -> label
  -> label
  -> ExactPoint
  -> RegularFace label
canonicalRegularFace firstLabel secondLabel thirdLabel dualPoint
  | firstLabel <= secondLabel && firstLabel <= thirdLabel =
      RegularFace firstLabel secondLabel thirdLabel dualPoint
  | secondLabel <= thirdLabel =
      RegularFace secondLabel thirdLabel firstLabel dualPoint
  | otherwise =
      RegularFace thirdLabel firstLabel secondLabel dualPoint

publishGeneratorEdge :: GeneratorRegularEdge label -> RegularEdge label
publishGeneratorEdge edge =
  RegularEdge
    (exactPowerGeneratorLabel (generatorRegularEdgeFirst edge))
    (exactPowerGeneratorLabel (generatorRegularEdgeSecond edge))
    (publishGeneratorDualGeometry (generatorRegularEdgeDual edge))

publishGeneratorDualGeometry :: GeneratorDualGeometry -> PowerDualEdge
publishGeneratorDualGeometry dual =
  case dual of
    GeneratorDualSegment segment -> BoundedPowerDual segment
    GeneratorDualRay ray -> UnboundedPowerDual ray
    GeneratorDualLine line -> FullLinePowerDual line
    GeneratorDualCollapsed point -> CollapsedPowerDual point

regularSiteStars
  :: Ord label
  => Map label disposition
  -> Map (RegularFaceKey label) (RegularFace label)
  -> Map (RegularEdgeKey label) (RegularEdgeSection label)
  -> Map label (RegularSiteStar label)
regularSiteStars dispositions faces =
  Map.foldl'
    insertRegularEdgeStar
    (Map.foldlWithKey' insertRegularFaceStar (emptyRegularStars dispositions) faces)

emptyRegularStars
  :: Map label disposition
  -> Map label (RegularSiteStar label)
emptyRegularStars = Map.map (const emptyRegularSiteStar)

publishRepresentativeDispositions
  :: Ord label
  => GeneratorRegularTopology label
  -> Map label (RegularSiteDisposition label)
publishRepresentativeDispositions topology =
  Map.fromList
    [ ( exactPowerGeneratorLabel generator
      , publishGeneratorDisposition disposition
      )
    | (generator, disposition) <-
        NonEmpty.toList (generatorRegularDispositions topology)
    ]

insertRegularFaceStar
  :: Ord label
  => Map label (RegularSiteStar label)
  -> RegularFaceKey label
  -> RegularFace label
  -> Map label (RegularSiteStar label)
insertRegularFaceStar stars faceKey face =
  let (firstLabel, secondLabel, thirdLabel) = regularFaceLabels face
   in Foldable.foldl'
        (\current label -> Map.adjust (addIncidentFace faceKey) label current)
        stars
        [firstLabel, secondLabel, thirdLabel]

insertRegularEdgeStar
  :: Ord label
  => Map label (RegularSiteStar label)
  -> RegularEdgeSection label
  -> Map label (RegularSiteStar label)
insertRegularEdgeStar stars edgeSection =
  let (firstLabel, secondLabel) = regularEdgeLabels (sectionRegularEdge edgeSection)
   in Map.adjust (addNeighbour secondLabel) firstLabel
        (Map.adjust (addNeighbour firstLabel) secondLabel stars)

addIncidentFace
  :: Ord label
  => RegularFaceKey label
  -> RegularSiteStar label
  -> RegularSiteStar label
addIncidentFace faceKey star =
  star{sectionIncidentFaces = Set.insert faceKey (sectionIncidentFaces star)}

addNeighbour :: Ord label => label -> RegularSiteStar label -> RegularSiteStar label
addNeighbour label star =
  star{sectionSiteNeighbours = Set.insert label (sectionSiteNeighbours star)}
