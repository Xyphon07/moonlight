-- | Pure topology-changing regular edits by exact conflict-cavity descent and gluing.
module Moonlight.Planar.Internal.PowerDiagram.Edit where

import Control.Monad (when)
import Data.Bifunctor (first)
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Vector as Vector
import Moonlight.Planar.Exact
  ( exactOrient2d
  , exactPoint
  )
import Moonlight.Planar.Internal.BoundaryCycle (orderedPair)
import Moonlight.Planar.Internal.ExactRational (ExactRational)
import Moonlight.Planar.Internal.PowerDiagram.Generator
import Moonlight.Planar.Internal.PowerDiagram.Hull (regularGeneratorTopology)
import Moonlight.Planar.Internal.PowerDiagram.Locality
import Moonlight.Planar.Internal.PowerDiagram.Model
import Moonlight.Planar.Internal.PowerDiagram.Section

-- | Insert one stable-labelled site. Repeating the identical site is
-- idempotent; reusing its label for different geometry is a typed conflict.
insertRegularSite
  :: Ord label
  => PowerSite label
  -> RegularTriangulation label
  -> Either
      (RegularEditError label)
      (RegularEditResult label)
insertRegularSite site triangulation =
  let label = powerSiteLabel site
   in case Map.lookup label (storedRegularSites triangulation) of
        Nothing ->
          withRegularEditFallback
            triangulation
            (Set.singleton label)
            ( normalizeRegularEdit
                triangulation
                (Map.insert label site (storedRegularSites triangulation))
                (Set.singleton label)
            )
            (insertRegularSiteLocally site triangulation)
        Just resident
          | resident == site ->
              Right (unchangedRegularEdit triangulation)
          | otherwise -> Left (RegularEditSiteConflict label resident site)

-- | Remove one labelled site. Absence is an idempotent successful result.
removeRegularSite
  :: Ord label
  => label
  -> RegularTriangulation label
  -> Either
      (RegularEditError label)
      (RegularEditResult label)
removeRegularSite label triangulation =
  case Map.lookup label (storedRegularSites triangulation) of
    Nothing ->
      Right (unchangedRegularEdit triangulation)
    Just _ ->
      case removeTopologyPreservingSite label triangulation of
        Just (edited, transitions) ->
          Right
            ( RegularEditResult
                edited
                (Set.singleton label)
                transitions
            )
        Nothing ->
          withRegularEditFallback
            triangulation
            (Set.singleton label)
            ( normalizeRegularEdit
                triangulation
                (Map.delete label (storedRegularSites triangulation))
                (Set.singleton label)
            )
            (removeRegularSiteLocally label triangulation)

-- | Replace many weights in one normalization. Every label must already
-- belong to the triangulation; an empty or unchanged patch is idempotent.
reweightRegularSites
  :: Ord label
  => Map label PowerWeight
  -> RegularTriangulation label
  -> Either
      (RegularEditError label)
      (RegularEditResult label)
reweightRegularSites requested triangulation =
  case NonEmpty.nonEmpty unknownLabels of
    Just unknown -> Left (RegularEditUnknownSites unknown)
    Nothing
      | Map.null changedSites ->
          Right (unchangedRegularEdit triangulation)
      | otherwise ->
          case reweightTopologyPreservingSites changedLabels changedSites reweightedSites triangulation of
            Just (edited, transitions) ->
              Right
                ( RegularEditResult
                    edited
                    changedLabels
                    transitions
                )
            Nothing ->
              case commonRegularWeightShift requested sites of
                Just shift ->
                  Right
                    ( translateRegularWeights
                        shift
                        reweightedSites
                        changedLabels
                        triangulation
                    )
                Nothing ->
                  case Map.elems changedSites of
                    [revised] ->
                      withRegularEditFallback
                        triangulation
                        changedLabels
                        normalizeGlobally
                        (reweightRegularSiteLocally revised triangulation)
                    _ -> normalizeGlobally
 where
  sites = storedRegularSites triangulation
  unknownLabels = Map.keys (Map.difference requested sites)
  changedSites =
    Map.mapMaybeWithKey
      (\label weight ->
          case Map.lookup label sites of
            Just site
              | powerSiteWeight site /= weight ->
                  Just site{powerSiteWeight = weight}
            _ -> Nothing
      )
      requested
  reweightedSites = Map.union changedSites sites
  changedLabels = Map.keysSet changedSites
  normalizeGlobally =
    normalizeRegularEdit triangulation reweightedSites changedLabels

withRegularEditFallback
  :: Ord label
  => RegularTriangulation label
  -> Set label
  -> Either (RegularEditError label) (RegularEditResult label)
  -> Either obstruction (RegularTriangulation label, Set label)
  -> Either (RegularEditError label) (RegularEditResult label)
withRegularEditFallback before changed fallback =
  either (const fallback) (Right . publishLocalRegularEdit before changed)
{-# INLINE withRegularEditFallback #-}

publishLocalRegularEdit
  :: Ord label
  => RegularTriangulation label
  -> Set label
  -> (RegularTriangulation label, Set label)
  -> RegularEditResult label
publishLocalRegularEdit before changed (edited, transitionSupport) =
  RegularEditResult
    edited
    changed
    ( dispositionTransitionsOn
        transitionSupport
        (regularDispositionSection before)
        (regularDispositionSection edited)
    )
{-# INLINE publishLocalRegularEdit #-}

insertRegularSiteLocally
  :: Ord label
  => PowerSite label
  -> RegularTriangulation label
  -> Either
      (LocalRegularEditObstruction label)
      (RegularTriangulation label, Set label)
insertRegularSiteLocally site triangulation = do
  section <-
    maybe (Left LocalRegularSectionUnavailable) Right
      (storedRegularSection triangulation)
  let generator = prepareExactPowerGenerator site
      slope = exactGeneratorSlope generator
  case Map.lookup slope (sectionSlopeRepresentatives section) of
    Just representative ->
      insertCoincidentRegularSite site generator representative section triangulation
    Nothing ->
      insertDistinctRegularSite site generator section triangulation

insertCoincidentRegularSite
  :: Ord label
  => PowerSite label
  -> ExactPowerGenerator label
  -> label
  -> RegularSection label
  -> RegularTriangulation label
  -> Either
      (LocalRegularEditObstruction label)
      (RegularTriangulation label, Set label)
insertCoincidentRegularSite site generator representativeLabel section triangulation = do
  representative <-
    requireGeneratorFromMap (sectionGenerators section) representativeLabel
  let label = powerSiteLabel site
  case
    compareCoincidentPriority
      (exactPowerGeneratorConstant generator, label)
      (exactPowerGeneratorConstant representative, representativeLabel) of
    LT ->
      let coincident = classifyCoincidentGenerator representative generator
          disposition = coincidentRegularDisposition coincident
          editedSection =
            section
              { sectionCoincidentDispositions =
                  Map.insert label coincident (sectionCoincidentDispositions section)
              , sectionRegularDispositions =
                  Map.insert label disposition (sectionRegularDispositions section)
              , sectionRegularStars =
                  Map.insert label emptyRegularSiteStar (sectionRegularStars section)
              }
       in Right
            ( replaceRegularSiteSection
                (Map.insert label site (storedRegularSites triangulation))
                editedSection
                triangulation
            , Set.singleton label
            )
    _ -> Left (LocalRegularSlopeCoincidence representativeLabel)

insertDistinctRegularSite
  :: Ord label
  => PowerSite label
  -> ExactPowerGenerator label
  -> RegularSection label
  -> RegularTriangulation label
  -> Either
      (LocalRegularEditObstruction label)
      (RegularTriangulation label, Set label)
insertDistinctRegularSite site generator section triangulation = do
  locality <-
    maybe (Left LocalRegularLocalityUnavailable) Right
      (sectionRegularLocality section)
  location <-
    maybe (Left LocalRegularSlopeLocationFailed) Right
      (locateRegularSlope (localityFaceSeed locality) section generator)
  case location of
    RegularSlopeOutside edgeKey -> Left (LocalRegularSlopeOutside edgeKey)
    RegularSlopeInside faceKey -> do
      volumeSign <- regularFaceSign section faceKey generator
      case volumeSign of
        LT -> insertHiddenRegularSite site generator faceKey locality section triangulation
        EQ -> Left (LocalRegularDegenerateFace faceKey)
        GT -> insertVisibleRegularSite site generator faceKey locality section triangulation

insertHiddenRegularSite
  :: Ord label
  => PowerSite label
  -> ExactPowerGenerator label
  -> RegularFaceKey label
  -> RegularLocality label
  -> RegularSection label
  -> RegularTriangulation label
  -> Either
      (LocalRegularEditObstruction label)
      (RegularTriangulation label, Set label)
insertHiddenRegularSite site generator supportFace locality section triangulation =
  let label = powerSiteLabel site
      receipt = sectionRegularReceipt section
      editedLocality = attachRegularHidden label supportFace locality
      editedSection =
        section
          { sectionGenerators = Map.insert label generator (sectionGenerators section)
          , sectionSlopeRepresentatives =
              Map.insert (exactGeneratorSlope generator) label (sectionSlopeRepresentatives section)
          , sectionRegularDispositions =
              Map.insert label RegularSiteHidden (sectionRegularDispositions section)
          , sectionRegularStars =
              Map.insert label emptyRegularSiteStar (sectionRegularStars section)
          , sectionRegularReceipt =
              receipt
                { generatorRegularInputSites = generatorRegularInputSites receipt + 1
                , generatorRegularHiddenSites = generatorRegularHiddenSites receipt + 1
                }
          , sectionRegularLocality = Just editedLocality
          }
   in Right
        ( replaceRegularSiteSection
            (Map.insert label site (storedRegularSites triangulation))
            editedSection
            triangulation
        , Set.singleton label
        )

insertVisibleRegularSite
  :: Ord label
  => PowerSite label
  -> ExactPowerGenerator label
  -> RegularFaceKey label
  -> RegularLocality label
  -> RegularSection label
  -> RegularTriangulation label
  -> Either
      (LocalRegularEditObstruction label)
      (RegularTriangulation label, Set label)
insertVisibleRegularSite site generator seedFace locality section triangulation = do
  conflictFaces <- regularConflictCavity section generator seedFace
  horizon <- regularConflictHorizon section conflictFaces
  when (Map.null horizon) (Left LocalRegularEmptyHorizon)
  newFaces <- traverse (regularHorizonFace section generator) (Map.elems horizon)
  let label = powerSiteLabel site
      generators = Map.insert label generator (sectionGenerators section)
  facePatch <-
    applyRegularFacePatch
      generators
      conflictFaces
      newFaces
      section
  certifyRegularFacePatch generators facePatch
  let changedTopologyLabels = Set.insert label (patchedRegularLabels facePatch)
      provisionalDispositions =
        Set.foldl'
          (publishPatchedRegularDisposition (patchedRegularStars facePatch))
          (Map.insert label RegularSiteVisible (sectionRegularDispositions section))
          changedTopologyLabels
      newlyHidden =
        Set.filter
          (becameRegularHidden (sectionRegularDispositions section) provisionalDispositions)
          changedTopologyLabels
      hiddenOnConflict =
        Set.unions
          [ Map.findWithDefault Set.empty faceKey (localitySupportHidden locality)
          | faceKey <- Set.toAscList conflictFaces
          ]
      hiddenToAttach = Set.union newlyHidden hiddenOnConflict
      receipt = sectionRegularReceipt section
      provisionalSection =
        section
          { sectionGenerators = generators
          , sectionSlopeRepresentatives =
              Map.insert (exactGeneratorSlope generator) label (sectionSlopeRepresentatives section)
          , sectionRegularDispositions = provisionalDispositions
          , sectionRegularFaces = patchedRegularFaces facePatch
          , sectionRegularEdges = patchedRegularEdges facePatch
          , sectionRegularStars = patchedRegularStars facePatch
          , sectionRegularReceipt =
              receipt
                { generatorRegularInputSites = generatorRegularInputSites receipt + 1
                , generatorRegularVisibleSites =
                    generatorRegularVisibleSites receipt + 1 - Set.size newlyHidden
                , generatorRegularHiddenSites =
                    generatorRegularHiddenSites receipt + Set.size newlyHidden
                , generatorRegularFaceCount = Map.size (patchedRegularFaces facePatch)
                , generatorRegularEdgeCount = Map.size (patchedRegularEdges facePatch)
                }
          , sectionRegularLocality = Nothing
          }
  editedLocality <-
    patchRegularLocality locality conflictFaces hiddenToAttach provisionalSection
  let editedSection = provisionalSection{sectionRegularLocality = Just editedLocality}
  pure
    ( replaceRegularSiteSection
        (Map.insert label site (storedRegularSites triangulation))
        editedSection
        triangulation
    , Set.insert label newlyHidden
    )


regularFaceSign
  :: Ord label
  => RegularSection label
  -> RegularFaceKey label
  -> ExactPowerGenerator label
  -> Either (LocalRegularEditObstruction label) Ordering
regularFaceSign section faceKey candidate = do
  face <- requireFaceFromMap (sectionRegularFaces section) faceKey
  (firstGenerator, secondGenerator, thirdGenerator) <-
    regularFaceGenerators (sectionGenerators section) face
  pure
    ( exactGeneratorLiftedSign
        firstGenerator
        secondGenerator
        thirdGenerator
        candidate
    )

regularConflictCavity
  :: Ord label
  => RegularSection label
  -> ExactPowerGenerator label
  -> RegularFaceKey label
  -> Either
      (LocalRegularEditObstruction label)
      (Set (RegularFaceKey label))
regularConflictCavity section candidate seed =
  let descended =
        descendRegularConflict
          section
          candidate
          RegularConflictDescent
            { conflictPendingFaces = Set.singleton seed
            , conflictVisitedFaces = Set.empty
            , conflictVisibleFaces = Set.empty
            , conflictObstruction = Nothing
            }
   in case conflictObstruction descended of
        Just obstruction -> Left obstruction
        Nothing
          | Set.null (conflictVisibleFaces descended) ->
              Left (LocalRegularDegenerateFace seed)
          | otherwise -> Right (conflictVisibleFaces descended)

-- | Every step retires one pending face into the visited set and enqueues only
-- unvisited neighbours, so the descent halts within one step per section face.
descendRegularConflict
  :: Ord label
  => RegularSection label
  -> ExactPowerGenerator label
  -> RegularConflictDescent label
  -> RegularConflictDescent label
descendRegularConflict section candidate descent =
  case Set.minView (conflictPendingFaces descent) of
    Nothing -> descent
    Just (faceKey, remaining) ->
      case regularFaceSign section faceKey candidate of
        Left obstruction ->
          descent
            { conflictPendingFaces = remaining
            , conflictObstruction = Just obstruction
            }
        Right volumeSign ->
          let visited = Set.insert faceKey (conflictVisitedFaces descent)
           in case volumeSign of
                LT ->
                  descendRegularConflict
                    section
                    candidate
                    descent
                      { conflictPendingFaces = remaining
                      , conflictVisitedFaces = visited
                      }
                EQ ->
                  descent
                    { conflictPendingFaces = remaining
                    , conflictVisitedFaces = visited
                    , conflictObstruction = Just (LocalRegularDegenerateFace faceKey)
                    }
                GT ->
                  case regularFaceNeighbours section faceKey of
                    Left obstruction ->
                      descent
                        { conflictPendingFaces = remaining
                        , conflictVisitedFaces = visited
                        , conflictObstruction = Just obstruction
                        }
                    Right neighbours ->
                      descendRegularConflict
                        section
                        candidate
                        descent
                          { conflictPendingFaces =
                              Set.union remaining (Set.difference neighbours visited)
                          , conflictVisitedFaces = visited
                          , conflictVisibleFaces =
                              Set.insert faceKey (conflictVisibleFaces descent)
                          }


regularConflictHorizon
  :: Ord label
  => RegularSection label
  -> Set (RegularFaceKey label)
  -> Either
      (LocalRegularEditObstruction label)
      (Map (RegularEdgeKey label) (label, label))
regularConflictHorizon section conflictFaces = do
  boundarySections <-
    traverse
      (regularConflictFaceHorizon section conflictFaces)
      (Set.toAscList conflictFaces)
  pure (Map.unions boundarySections)

regularConflictFaceHorizon
  :: Ord label
  => RegularSection label
  -> Set (RegularFaceKey label)
  -> RegularFaceKey label
  -> Either
      (LocalRegularEditObstruction label)
      (Map (RegularEdgeKey label) (label, label))
regularConflictFaceHorizon section conflictFaces faceKey = do
  face <- requireFaceFromMap (sectionRegularFaces section) faceKey
  associations <-
    traverse
      (classifyConflictEdge section conflictFaces)
      (regularFaceDirectedEdgeKeys face)
  pure (Map.fromList (concat associations))

classifyConflictEdge
  :: Ord label
  => RegularSection label
  -> Set (RegularFaceKey label)
  -> (label, label)
  -> Either
      (LocalRegularEditObstruction label)
      [(RegularEdgeKey label, (label, label))]
classifyConflictEdge section conflictFaces directed@(fromLabel, toLabel) = do
  let edgeKey = orderedPair fromLabel toLabel
  incident <- requireRegularEdgeIncidence section edgeKey
  if Set.size incident > 2
    then Left (LocalRegularNonManifoldEdge edgeKey (Set.size incident))
    else
      if Set.size (Set.intersection incident conflictFaces) == 1
        then Right [(edgeKey, directed)]
        else Right []

regularHorizonFace
  :: Ord label
  => RegularSection label
  -> ExactPowerGenerator label
  -> (label, label)
  -> Either (LocalRegularEditObstruction label) (RegularFace label)
regularHorizonFace section candidate (fromLabel, toLabel) = do
  fromGenerator <- requireGeneratorFromMap (sectionGenerators section) fromLabel
  toGenerator <- requireGeneratorFromMap (sectionGenerators section) toLabel
  let candidateLabel = exactPowerGeneratorLabel candidate
      faceKey = sortedRegularFaceLabels fromLabel toLabel candidateLabel
      fromPoint = uncurry exactPoint (exactGeneratorSlope fromGenerator)
      toPoint = uncurry exactPoint (exactGeneratorSlope toGenerator)
      candidatePoint = uncurry exactPoint (exactGeneratorSlope candidate)
  case exactOrient2d fromPoint toPoint candidatePoint of
    GT -> do
      dual <-
        first LocalRegularTopologyObstruction
          (exactGeneratorFaceDual fromGenerator toGenerator candidate)
      pure (canonicalRegularFace fromLabel toLabel candidateLabel dual)
    _ -> Left (LocalRegularDegenerateFace faceKey)

applyRegularFacePatch
  :: Ord label
  => Map label (ExactPowerGenerator label)
  -> Set (RegularFaceKey label)
  -> [RegularFace label]
  -> RegularSection label
  -> Either (LocalRegularEditObstruction label) (RegularFacePatch label)
applyRegularFacePatch generators removedFaceKeys newFaces section = do
  removedFaces <-
    traverse
      (requireFaceFromMap (sectionRegularFaces section))
      (Set.toAscList removedFaceKeys)
  let newFaceSection =
        Map.fromList [(regularFaceKey face, face) | face <- newFaces]
      faces =
        Map.union newFaceSection
          (Map.withoutKeys (sectionRegularFaces section) removedFaceKeys)
      touchedEdges =
        Set.unions
          [ Set.unions (fmap (Set.fromList . regularFaceCanonicalEdgeKeys) removedFaces)
          , Set.unions (fmap (Set.fromList . regularFaceCanonicalEdgeKeys) newFaces)
          ]
      newIncidence =
        List.foldl'
          (Map.unionWith Set.union)
          Map.empty
          (fmap regularFaceEdgeIncidence newFaces)
      affectedLabels =
        Set.unions
          [ Set.unions (fmap regularFaceLabelSet removedFaces)
          , Set.unions (fmap regularFaceLabelSet newFaces)
          ]
  edges <-
    Foldable.foldlM
      (patchRegularEdge generators faces removedFaceKeys newIncidence)
      (sectionRegularEdges section)
      touchedEdges
  let stars =
        patchRegularStars
          affectedLabels
          removedFaceKeys
          newFaceSection
          touchedEdges
          edges
          section
  pure
    RegularFacePatch
      { patchedRegularFaces = faces
      , patchedRegularEdges = edges
      , patchedRegularStars = stars
      , patchedRegularLabels = affectedLabels
      , patchedRegularTouchedEdges = touchedEdges
      }

patchRegularEdge
  :: Ord label
  => Map label (ExactPowerGenerator label)
  -> Map (RegularFaceKey label) (RegularFace label)
  -> Set (RegularFaceKey label)
  -> Map (RegularEdgeKey label) (Set (RegularFaceKey label))
  -> Map (RegularEdgeKey label) (RegularEdgeSection label)
  -> RegularEdgeKey label
  -> Either
      (LocalRegularEditObstruction label)
      (Map (RegularEdgeKey label) (RegularEdgeSection label))
patchRegularEdge generators faces removedFaceKeys newIncidence edges edgeKey =
  let oldIncidence =
        maybe Set.empty sectionRegularEdgeFaces (Map.lookup edgeKey edges)
      incidence =
        Set.union
          (Set.difference oldIncidence removedFaceKeys)
          (Map.findWithDefault Set.empty edgeKey newIncidence)
   in if Set.null incidence
        then Right (Map.delete edgeKey edges)
        else do
          edgeSection <- publishRegularEdgeSection generators faces edgeKey incidence
          pure (Map.insert edgeKey edgeSection edges)

publishRegularEdgeSection
  :: Ord label
  => Map label (ExactPowerGenerator label)
  -> Map (RegularFaceKey label) (RegularFace label)
  -> RegularEdgeKey label
  -> Set (RegularFaceKey label)
  -> Either (LocalRegularEditObstruction label) (RegularEdgeSection label)
publishRegularEdgeSection generators faces edgeKey@(firstLabel, secondLabel) incidence =
  case Set.toAscList incidence of
    [faceKey] -> do
      face <- requireFaceFromMap faces faceKey
      (fromLabel, toLabel, oppositeLabel) <-
        requireFaceEdgeWitness edgeKey face
      fromGenerator <- requireGeneratorFromMap generators fromLabel
      toGenerator <- requireGeneratorFromMap generators toLabel
      oppositeGenerator <- requireGeneratorFromMap generators oppositeLabel
      ray <-
        first LocalRegularTopologyObstruction
          ( exactGeneratorBoundaryDualRay
              (regularFaceDualPoint face)
              fromGenerator
              toGenerator
              oppositeGenerator
          )
      pure
        ( RegularEdgeSection
            (RegularEdge firstLabel secondLabel (UnboundedPowerDual ray))
            incidence
        )
    [firstFaceKey, secondFaceKey] -> do
      firstFace <- requireFaceFromMap faces firstFaceKey
      secondFace <- requireFaceFromMap faces secondFaceKey
      let firstDual = regularFaceDualPoint firstFace
          secondDual = regularFaceDualPoint secondFace
      dual <-
        first LocalRegularTopologyObstruction
          ( exactGeneratorDualBetween
              firstLabel
              secondLabel
              firstDual
              secondDual
          )
      pure
        ( RegularEdgeSection
            (RegularEdge firstLabel secondLabel dual)
            incidence
        )
    incidentFaces ->
      Left (LocalRegularNonManifoldEdge edgeKey (length incidentFaces))

patchRegularStars
  :: Ord label
  => Set label
  -> Set (RegularFaceKey label)
  -> Map (RegularFaceKey label) (RegularFace label)
  -> Set (RegularEdgeKey label)
  -> Map (RegularEdgeKey label) (RegularEdgeSection label)
  -> RegularSection label
  -> Map label (RegularSiteStar label)
patchRegularStars affectedLabels removedFaceKeys newFaceSection touchedEdges edges section =
  Set.foldl' patchStar (sectionRegularStars section) affectedLabels
 where
  affectedSupport = Map.fromSet (const ()) affectedLabels
  touchedStars =
    regularSiteStars
      affectedSupport
      Map.empty
      ( Map.restrictKeys
          (Map.union edges (sectionRegularEdges section))
          touchedEdges
      )
  addedStars =
    regularSiteStars
      affectedSupport
      newFaceSection
      (Map.restrictKeys edges touchedEdges)
  patchStar stars label =
    let oldStar = Map.findWithDefault emptyRegularSiteStar label stars
        touchedStar = Map.findWithDefault emptyRegularSiteStar label touchedStars
        addedStar = Map.findWithDefault emptyRegularSiteStar label addedStars
        incidentFaces =
          Set.union
            (Set.difference (sectionIncidentFaces oldStar) removedFaceKeys)
            (sectionIncidentFaces addedStar)
        retainedNeighbours =
          Set.difference
            (sectionSiteNeighbours oldStar)
            (sectionSiteNeighbours touchedStar)
     in Map.insert
          label
          ( RegularSiteStar
              incidentFaces
              (Set.union retainedNeighbours (sectionSiteNeighbours addedStar))
          )
          stars

certifyRegularFacePatch
  :: Ord label
  => Map label (ExactPowerGenerator label)
  -> RegularFacePatch label
  -> Either (LocalRegularEditObstruction label) ()
certifyRegularFacePatch generators patch =
  Foldable.traverse_
    (\edgeKey ->
       case Map.lookup edgeKey (patchedRegularEdges patch) of
         Nothing -> Right ()
         Just edgeSection ->
           certifyRegularEdgeConvexity
             generators
             (patchedRegularFaces patch)
             (edgeKey, edgeSection))
    (patchedRegularTouchedEdges patch)

certifyRegularEdgeConvexity
  :: Ord label
  => Map label (ExactPowerGenerator label)
  -> Map (RegularFaceKey label) (RegularFace label)
  -> (RegularEdgeKey label, RegularEdgeSection label)
  -> Either (LocalRegularEditObstruction label) ()
certifyRegularEdgeConvexity generators faces (edgeKey, edgeSection) =
  case Set.toAscList (sectionRegularEdgeFaces edgeSection) of
    [_] -> Right ()
    [firstFaceKey, secondFaceKey] -> do
      firstFace <- requireFaceFromMap faces firstFaceKey
      secondFace <- requireFaceFromMap faces secondFaceKey
      (_, _, secondOpposite) <- requireFaceEdgeWitness edgeKey secondFace
      secondOppositeGenerator <- requireGeneratorFromMap generators secondOpposite
      (firstGenerator, secondGenerator, thirdGenerator) <-
        regularFaceGenerators generators firstFace
      if exactGeneratorLiftedSign
          firstGenerator
          secondGenerator
          thirdGenerator
          secondOppositeGenerator
          == LT
        then Right ()
        else Left (LocalRegularNonConvexEdge edgeKey)
    incident -> Left (LocalRegularNonManifoldEdge edgeKey (length incident))

publishPatchedRegularDisposition
  :: Ord label
  => Map label (RegularSiteStar label)
  -> Map label (RegularSiteDisposition label)
  -> label
  -> Map label (RegularSiteDisposition label)
publishPatchedRegularDisposition stars dispositions label =
  case Map.lookup label dispositions of
    Just RegularSiteVisible
      | maybe True (Set.null . sectionIncidentFaces) (Map.lookup label stars) ->
          Map.insert label RegularSiteHidden dispositions
    _ -> dispositions

becameRegularHidden
  :: Ord label
  => Map label (RegularSiteDisposition label)
  -> Map label (RegularSiteDisposition label)
  -> label
  -> Bool
becameRegularHidden before after label =
  Map.lookup label before == Just RegularSiteVisible
    && Map.lookup label after == Just RegularSiteHidden

requireRegularEdgeIncidence
  :: Ord label
  => RegularSection label
  -> RegularEdgeKey label
  -> Either
      (LocalRegularEditObstruction label)
      (Set (RegularFaceKey label))
requireRegularEdgeIncidence section edgeKey =
  maybe
    (Left (LocalRegularNonManifoldEdge edgeKey 0))
    (Right . sectionRegularEdgeFaces)
    (Map.lookup edgeKey (sectionRegularEdges section))

regularFaceGenerators
  :: Ord label
  => Map label (ExactPowerGenerator label)
  -> RegularFace label
  -> Either
      (LocalRegularEditObstruction label)
      ( ExactPowerGenerator label
      , ExactPowerGenerator label
      , ExactPowerGenerator label
      )
regularFaceGenerators generators face = do
  let (firstLabel, secondLabel, thirdLabel) = regularFaceLabels face
  (,,)
    <$> requireGeneratorFromMap generators firstLabel
    <*> requireGeneratorFromMap generators secondLabel
    <*> requireGeneratorFromMap generators thirdLabel

removeRegularSiteLocally
  :: Ord label
  => label
  -> RegularTriangulation label
  -> Either
      (LocalRegularEditObstruction label)
      (RegularTriangulation label, Set label)
removeRegularSiteLocally label triangulation = do
  section <-
    maybe (Left LocalRegularSectionUnavailable) Right
      (storedRegularSection triangulation)
  locality <-
    maybe (Left LocalRegularLocalityUnavailable) Right
      (sectionRegularLocality section)
  disposition <-
    maybe (Left (LocalRegularGeneratorMissing label)) Right
      (Map.lookup label (sectionRegularDispositions section))
  when
    (disposition /= RegularSiteVisible || hasCoincidentAliases label section)
    (Left (LocalRegularBoundaryRemoval label))
  star <-
    maybe (Left (LocalRegularGeneratorMissing label)) Right
      (Map.lookup label (sectionRegularStars section))
  let removedFaces = sectionIncidentFaces star
  when (Set.null removedFaces) (Left (LocalRegularBoundaryRemoval label))
  Foldable.traverse_
    (requireInteriorRemovalEdge section label)
    (sectionSiteNeighbours star)
  horizon <- regularConflictHorizon section removedFaces
  when (Map.null horizon) (Left LocalRegularEmptyHorizon)
  let hiddenCandidates =
        Set.unions
          [ Map.findWithDefault Set.empty faceKey (localitySupportHidden locality)
          | faceKey <- Set.toAscList removedFaces
          ]
      horizonLabels =
        Set.fromList
          [ endpoint
          | (firstLabel, secondLabel) <- Map.keys horizon
          , endpoint <- [firstLabel, secondLabel]
          ]
      candidateLabels = Set.union horizonLabels hiddenCandidates
      generators = Map.delete label (sectionGenerators section)
  candidateGenerators <-
    traverse
      (requireGeneratorFromMap generators)
      (Set.toAscList candidateLabels)
  nonEmptyCandidates <-
    maybe (Left LocalRegularCandidateSectionEmpty) Right
      (NonEmpty.nonEmpty candidateGenerators)
  candidateTopology <-
    first LocalRegularTopologyObstruction
      (regularGeneratorTopology (DistinctSlopeGenerators nonEmptyCandidates))
  when
    ( any
        ((== RegularGeneratorLowerDimensional) . snd)
        (NonEmpty.toList (generatorRegularDispositions candidateTopology))
    )
    (Left LocalRegularCavityBoundaryMismatch)
  removedFaceValues <-
    traverse
      (requireFaceFromMap (sectionRegularFaces section))
      (Set.toAscList removedFaces)
  let proposedFaces = fmap publishGeneratorFace (generatorRegularFaces candidateTopology)
      newFaces =
        filter
          (regularFaceCentroidInside generators removedFaceValues)
          proposedFaces
      proposedBoundary = regularFaceSectionBoundary newFaces
  when
    (proposedBoundary /= Map.keysSet horizon)
    (Left LocalRegularCavityBoundaryMismatch)
  facePatch <- applyRegularFacePatch generators removedFaces newFaces section
  certifyRegularFacePatch generators facePatch
  let candidateDispositions =
        Set.foldl'
          (publishRemovalCandidateDisposition (patchedRegularStars facePatch))
          (Map.delete label (sectionRegularDispositions section))
          candidateLabels
      exposed =
        Set.filter
          (becameRegularVisible (sectionRegularDispositions section) candidateDispositions)
          candidateLabels
      stillHidden =
        Set.filter
          (\candidate -> Map.lookup candidate candidateDispositions == Just RegularSiteHidden)
          hiddenCandidates
      receipt = sectionRegularReceipt section
      provisionalSection =
        section
          { sectionGenerators = generators
          , sectionSlopeRepresentatives =
              Map.filter (/= label) (sectionSlopeRepresentatives section)
          , sectionRegularDispositions = candidateDispositions
          , sectionRegularFaces = patchedRegularFaces facePatch
          , sectionRegularEdges = patchedRegularEdges facePatch
          , sectionRegularStars = Map.delete label (patchedRegularStars facePatch)
          , sectionRegularReceipt =
              receipt
                { generatorRegularInputSites = generatorRegularInputSites receipt - 1
                , generatorRegularVisibleSites =
                    generatorRegularVisibleSites receipt - 1 + Set.size exposed
                , generatorRegularHiddenSites =
                    generatorRegularHiddenSites receipt - Set.size exposed
                , generatorRegularFaceCount = Map.size (patchedRegularFaces facePatch)
                , generatorRegularEdgeCount = Map.size (patchedRegularEdges facePatch)
                }
          , sectionRegularLocality = Nothing
          }
  editedLocality <-
    patchRegularLocality locality removedFaces stillHidden provisionalSection
  let editedSection = provisionalSection{sectionRegularLocality = Just editedLocality}
  pure
    ( replaceRegularSiteSection
        (Map.delete label (storedRegularSites triangulation))
        editedSection
        triangulation
    , Set.insert label exposed
    )

requireInteriorRemovalEdge
  :: Ord label
  => RegularSection label
  -> label
  -> label
  -> Either (LocalRegularEditObstruction label) ()
requireInteriorRemovalEdge section label neighbour = do
  let edgeKey = orderedPair label neighbour
  incidence <- requireRegularEdgeIncidence section edgeKey
  if Set.size incidence == 2
    then Right ()
    else Left (LocalRegularBoundaryRemoval label)

regularFaceCentroidInside
  :: Ord label
  => Map label (ExactPowerGenerator label)
  -> [RegularFace label]
  -> RegularFace label
  -> Bool
regularFaceCentroidInside generators domainFaces candidate =
  case regularFaceSlopeSum generators candidate of
    Left _ -> False
    Right slopeSum -> any (regularSlopeSumInsideFace generators slopeSum) domainFaces

regularFaceSlopeSum
  :: Ord label
  => Map label (ExactPowerGenerator label)
  -> RegularFace label
  -> Either
      (LocalRegularEditObstruction label)
      (ExactRational, ExactRational)
regularFaceSlopeSum generators face = do
  (firstGenerator, secondGenerator, thirdGenerator) <-
    regularFaceGenerators generators face
  let (firstX, firstY) = exactGeneratorSlope firstGenerator
      (secondX, secondY) = exactGeneratorSlope secondGenerator
      (thirdX, thirdY) = exactGeneratorSlope thirdGenerator
  pure (firstX + secondX + thirdX, firstY + secondY + thirdY)

regularSlopeSumInsideFace
  :: Ord label
  => Map label (ExactPowerGenerator label)
  -> (ExactRational, ExactRational)
  -> RegularFace label
  -> Bool
regularSlopeSumInsideFace generators slopeSum =
  not
    . any (generatorEdgeExcludesScaled 3 slopeSum generators)
    . regularFaceDirectedEdgeKeys

regularFaceSectionBoundary
  :: Ord label
  => [RegularFace label]
  -> Set (RegularEdgeKey label)
regularFaceSectionBoundary =
  Map.keysSet
    . Map.filter ((== 1) . Set.size)
    . List.foldl'
        (Map.unionWith Set.union)
        Map.empty
    . fmap regularFaceEdgeIncidence

publishRemovalCandidateDisposition
  :: Ord label
  => Map label (RegularSiteStar label)
  -> Map label (RegularSiteDisposition label)
  -> label
  -> Map label (RegularSiteDisposition label)
publishRemovalCandidateDisposition stars dispositions label =
  Map.insert
    label
    ( if maybe True (Set.null . sectionIncidentFaces) (Map.lookup label stars)
        then RegularSiteHidden
        else RegularSiteVisible
    )
    dispositions

becameRegularVisible
  :: Ord label
  => Map label (RegularSiteDisposition label)
  -> Map label (RegularSiteDisposition label)
  -> label
  -> Bool
becameRegularVisible before after label =
  Map.lookup label before == Just RegularSiteHidden
    && Map.lookup label after == Just RegularSiteVisible

reweightRegularSiteLocally
  :: Ord label
  => PowerSite label
  -> RegularTriangulation label
  -> Either
      (LocalRegularEditObstruction label)
      (RegularTriangulation label, Set label)
reweightRegularSiteLocally revised triangulation = do
  let label = powerSiteLabel revised
  (removed, removalSupport) <- removeAnyRegularSiteLocally label triangulation
  (inserted, insertionSupport) <- insertRegularSiteLocally revised removed
  pure (inserted, Set.union removalSupport insertionSupport)

commonRegularWeightShift
  :: Ord label
  => Map label PowerWeight
  -> Map label (PowerSite label)
  -> Maybe ExactRational
commonRegularWeightShift requested sites
  | Map.size requested /= Map.size sites = Nothing
  | otherwise = do
      (firstLabel, firstWeight) <- Map.lookupMin requested
      firstSite <- Map.lookup firstLabel sites
      let shift =
            powerWeightExact firstWeight
              - powerWeightExact (powerSiteWeight firstSite)
      if Map.isSubmapOfBy (powerSiteHasWeightShift shift) requested sites
        then Just shift
        else Nothing

powerSiteHasWeightShift
  :: ExactRational
  -> PowerWeight
  -> PowerSite label
  -> Bool
powerSiteHasWeightShift shift weight site =
  powerWeightExact weight
    - powerWeightExact (powerSiteWeight site)
    == shift

translateRegularWeights
  :: ExactRational
  -> Map label (PowerSite label)
  -> Set label
  -> RegularTriangulation label
  -> RegularEditResult label
translateRegularWeights shift sites changedLabels triangulation =
  let shiftedSection =
        fmap
          (\section ->
             section
               { sectionGenerators =
                   fmap
                     (translateExactPowerGenerator shift)
                     (sectionGenerators section)
               })
          (storedRegularSection triangulation)
   in RegularEditResult
        { regularEditTriangulation =
            triangulation
              { storedRegularSites = sites
              , storedRegularSection = shiftedSection
              }
        , regularEditChangedSites = changedLabels
        , regularEditTransitions = Vector.empty
        }

translateExactPowerGenerator
  :: ExactRational
  -> ExactPowerGenerator label
  -> ExactPowerGenerator label
translateExactPowerGenerator shift generator =
  generator
    { exactPowerGeneratorConstant =
        exactPowerGeneratorConstant generator + shift
    }

removeAnyRegularSiteLocally
  :: Ord label
  => label
  -> RegularTriangulation label
  -> Either
      (LocalRegularEditObstruction label)
      (RegularTriangulation label, Set label)
removeAnyRegularSiteLocally label triangulation =
  case removeTopologyPreservingSite label triangulation of
    Just (edited, _) -> Right (edited, Set.singleton label)
    Nothing -> removeRegularSiteLocally label triangulation

-- Exact topology-preserving edits descend through the existing regular
-- section. A failed classification is not an error; it selects the canonical
-- global normalization below.

classifyCoincidentSubordinate
  :: Ord label
  => PowerSite label
  -> PowerSite label
  -> Maybe (CoincidentGeneratorDisposition label)
classifyCoincidentSubordinate representative candidate =
  if powerSiteQueryPoint candidate /= powerSiteQueryPoint representative
    then Nothing
    else
      case
        compareCoincidentPriority
          (powerSiteWeight candidate, powerSiteLabel candidate)
          (powerSiteWeight representative, powerSiteLabel representative) of
        LT ->
          Just
            ( classifyCoincidentValue
                (powerSiteLabel representative)
                (powerSiteWeight representative)
                (powerSiteWeight candidate)
            )
        _ -> Nothing

removeTopologyPreservingSite
  :: Ord label
  => label
  -> RegularTriangulation label
  -> Maybe
      ( RegularTriangulation label
      , Vector.Vector (RegularSiteTransition label)
      )
removeTopologyPreservingSite label triangulation = do
  section <- storedRegularSection triangulation
  disposition <- Map.lookup label (sectionRegularDispositions section)
  editedSection <-
    case disposition of
      RegularSiteCoincidentEquivalentTo _ -> removeCoincident section
      RegularSiteCoincidentDominatedBy _ -> removeCoincident section
      RegularSiteHidden
        | not (hasCoincidentAliases label section) -> do
            generator <- Map.lookup label (sectionGenerators section)
            let receipt = sectionRegularReceipt section
            pure
              section
                { sectionGenerators = Map.delete label (sectionGenerators section)
                , sectionSlopeRepresentatives =
                    Map.delete (exactGeneratorSlope generator) (sectionSlopeRepresentatives section)
                , sectionRegularDispositions =
                    Map.delete label (sectionRegularDispositions section)
                , sectionRegularStars = Map.delete label (sectionRegularStars section)
                , sectionRegularReceipt =
                    receipt
                      { generatorRegularInputSites = generatorRegularInputSites receipt - 1
                      , generatorRegularHiddenSites = generatorRegularHiddenSites receipt - 1
                      }
                , sectionRegularLocality = fmap (detachRegularHidden label) (sectionRegularLocality section)
                }
      _ -> Nothing
  let edited =
        replaceRegularSiteSection
          (Map.delete label (storedRegularSites triangulation))
          editedSection
          triangulation
  pure
    ( edited
    , Vector.singleton (RegularSiteDisappeared label disposition)
    )
 where
  removeCoincident section = do
    _ <- Map.lookup label (sectionCoincidentDispositions section)
    pure
      section
        { sectionCoincidentDispositions =
            Map.delete label (sectionCoincidentDispositions section)
        , sectionRegularDispositions =
            Map.delete label (sectionRegularDispositions section)
        , sectionRegularStars = Map.delete label (sectionRegularStars section)
        }

data RegularReweightDescent label = RegularReweightDescent
  { descendedCoincidentDispositions :: !(Map label (CoincidentGeneratorDisposition label))
  , descendedGenerators :: !(Map label (ExactPowerGenerator label))
  , descendedRegularDispositions :: !(Map label (RegularSiteDisposition label))
  , descendedTransitions :: ![RegularSiteTransition label]
  }

reweightTopologyPreservingSites
  :: Ord label
  => Set label
  -> Map label (PowerSite label)
  -> Map label (PowerSite label)
  -> RegularTriangulation label
  -> Maybe
      ( RegularTriangulation label
      , Vector.Vector (RegularSiteTransition label)
      )
reweightTopologyPreservingSites changedLabels changedSites reweightedSites triangulation = do
  section <- storedRegularSection triangulation
  let coincidentRepresentatives =
        Map.foldl'
          (flip (Set.insert . coincidentRepresentativeLabel))
          Set.empty
          (sectionCoincidentDispositions section)
  descent <-
    Foldable.foldlM
      ( descendTopologyPreservingReweight
          changedLabels
          reweightedSites
          coincidentRepresentatives
          triangulation
      )
      RegularReweightDescent
        { descendedCoincidentDispositions = sectionCoincidentDispositions section
        , descendedGenerators = sectionGenerators section
        , descendedRegularDispositions = sectionRegularDispositions section
        , descendedTransitions = []
        }
      changedSites
  let editedSection =
        section
          { sectionCoincidentDispositions = descendedCoincidentDispositions descent
          , sectionGenerators = descendedGenerators descent
          , sectionRegularDispositions = descendedRegularDispositions descent
          }
      edited =
        replaceRegularSiteSection
          reweightedSites
          editedSection
          triangulation
  pure (edited, Vector.fromList (reverse (descendedTransitions descent)))

descendTopologyPreservingReweight
  :: Ord label
  => Set label
  -> Map label (PowerSite label)
  -> Set label
  -> RegularTriangulation label
  -> RegularReweightDescent label
  -> PowerSite label
  -> Maybe (RegularReweightDescent label)
descendTopologyPreservingReweight changedLabels reweightedSites coincidentRepresentatives triangulation descent revised = do
  resident <- Map.lookup label (storedRegularSites triangulation)
  disposition <-
    storedRegularSection triangulation
      >>= Map.lookup label . sectionRegularDispositions
  case regularCoincidentRepresentative disposition of
    Just representative
      | Set.notMember representative changedLabels -> do
          representativeSite <- Map.lookup representative reweightedSites
          coincident <- classifyCoincidentSubordinate representativeSite revised
          let after = coincidentRegularDisposition coincident
          pure
            descent
              { descendedCoincidentDispositions =
                  Map.insert label coincident (descendedCoincidentDispositions descent)
              , descendedRegularDispositions =
                  Map.insert label after (descendedRegularDispositions descent)
              , descendedTransitions =
                  if disposition == after
                    then descendedTransitions descent
                    else RegularSiteTransitioned label disposition after : descendedTransitions descent
              }
    _
      | disposition == RegularSiteHidden
          && powerSiteWeight revised < powerSiteWeight resident
          && Set.notMember label coincidentRepresentatives ->
          let generator = prepareExactPowerGenerator revised
           in Just
                descent
                  { descendedGenerators =
                      Map.insert label generator (descendedGenerators descent)
                  }
      | otherwise -> Nothing
 where
  label = powerSiteLabel revised

hasCoincidentAliases
  :: Eq label
  => label
  -> RegularSection label
  -> Bool
hasCoincidentAliases label =
  any ((== label) . coincidentRepresentativeLabel)
    . Map.elems
    . sectionCoincidentDispositions

regularCoincidentRepresentative :: RegularSiteDisposition label -> Maybe label
regularCoincidentRepresentative disposition =
  case disposition of
    RegularSiteCoincidentEquivalentTo label -> Just label
    RegularSiteCoincidentDominatedBy label -> Just label
    _ -> Nothing

coincidentRepresentativeLabel :: CoincidentGeneratorDisposition label -> label
coincidentRepresentativeLabel disposition =
  case disposition of
    CoincidentGeneratorEquivalentTo label -> label
    CoincidentGeneratorDominatedBy label -> label

normalizeRegularEdit
  :: Ord label
  => RegularTriangulation label
  -> Map label (PowerSite label)
  -> Set label
  -> Either
      (RegularEditError label)
      (RegularEditResult label)
normalizeRegularEdit previous sites changedSites = do
  triangulation <-
    first RegularEditTopologyFailed
      (constructRegularTriangulation sites)
  pure
    ( RegularEditResult
        triangulation
        changedSites
        ( dispositionTransitions
            (regularDispositionSection previous)
            (regularDispositionSection triangulation)
        )
    )

unchangedRegularEdit
  :: RegularTriangulation label
  -> RegularEditResult label
unchangedRegularEdit triangulation =
  RegularEditResult triangulation Set.empty Vector.empty


dispositionTransitions
  :: Ord label
  => Map label (RegularSiteDisposition label)
  -> Map label (RegularSiteDisposition label)
  -> Vector.Vector (RegularSiteTransition label)
dispositionTransitions previous current =
  Vector.fromList
    ( Map.elems
        ( Map.mergeWithKey
            changedDispositionTransition
            (Map.mapWithKey RegularSiteDisappeared)
            (Map.mapWithKey RegularSiteAppeared)
            previous
            current
        )
    )

dispositionTransitionsOn
  :: Ord label
  => Set label
  -> Map label (RegularSiteDisposition label)
  -> Map label (RegularSiteDisposition label)
  -> Vector.Vector (RegularSiteTransition label)
dispositionTransitionsOn support previous current =
  dispositionTransitions
    (Map.restrictKeys previous support)
    (Map.restrictKeys current support)

changedDispositionTransition
  :: Eq label
  => label
  -> RegularSiteDisposition label
  -> RegularSiteDisposition label
  -> Maybe (RegularSiteTransition label)
changedDispositionTransition label before after
  | before == after = Nothing
  | otherwise = Just (RegularSiteTransitioned label before after)
