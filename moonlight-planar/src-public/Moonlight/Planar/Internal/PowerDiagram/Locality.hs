{-# LANGUAGE BangPatterns #-}

-- | The discardable hidden-support index and exact face walk for one regular section.
module Moonlight.Planar.Internal.PowerDiagram.Locality where

import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Moonlight.Planar.Exact
  ( ExactPoint
  , exactOrient2d
  , exactPoint
  )
import Moonlight.Planar.Internal.BoundaryCycle (orderedPair)
import Moonlight.Planar.Internal.ExactRational (ExactRational)
import Moonlight.Planar.Internal.PowerDiagram.Generator
  ( ExactPowerGenerator
  )
import Moonlight.Planar.Internal.PowerDiagram.Model

patchRegularLocality
  :: Ord label
  => RegularLocality label
  -> Set (RegularFaceKey label)
  -> Set label
  -> RegularSection label
  -> Either (LocalRegularEditObstruction label) (RegularLocality label)
patchRegularLocality locality removedFaces hiddenToAttach section = do
  seed <-
    case Map.lookupMin (sectionRegularFaces section) of
      Nothing -> Left LocalRegularLocalityUnavailable
      Just (minimumFace, _) ->
        Right
          ( if Map.member (localityFaceSeed locality) (sectionRegularFaces section)
              then localityFaceSeed locality
              else minimumFace
          )
  let detachedForFaces =
        Set.unions
          [ Map.findWithDefault Set.empty faceKey (localitySupportHidden locality)
          | faceKey <- Set.toAscList removedFaces
          ]
      detachedLabels = Set.union detachedForFaces hiddenToAttach
      detached = Set.foldl' (flip detachRegularHidden) locality detachedLabels
  attachments <-
    traverse
      (locateHiddenSupport seed section)
      (Set.toAscList hiddenToAttach)
  pure
    ( Foldable.foldl'
        (\current (label, faceKey) -> attachRegularHidden label faceKey current)
        detached{localityFaceSeed = seed}
        attachments
    )

locateHiddenSupport
  :: Ord label
  => RegularFaceKey label
  -> RegularSection label
  -> label
  -> Either (LocalRegularEditObstruction label) (label, RegularFaceKey label)
locateHiddenSupport seed section label = do
  generator <- requireGeneratorFromMap (sectionGenerators section) label
  location <-
    maybe (Left LocalRegularSlopeLocationFailed) Right
      (locateRegularSlope seed section generator)
  case location of
    RegularSlopeInside faceKey -> Right (label, faceKey)
    RegularSlopeOutside edgeKey -> Left (LocalRegularSlopeOutside edgeKey)

attachRegularHidden
  :: Ord label
  => label
  -> RegularFaceKey label
  -> RegularLocality label
  -> RegularLocality label
attachRegularHidden label faceKey locality =
  locality
    { localityHiddenSupport = Map.insert label faceKey (localityHiddenSupport locality)
    , localitySupportHidden =
        Map.insertWith Set.union faceKey (Set.singleton label) (localitySupportHidden locality)
    }

detachRegularHidden
  :: Ord label
  => label
  -> RegularLocality label
  -> RegularLocality label
detachRegularHidden label locality =
  case Map.lookup label (localityHiddenSupport locality) of
    Nothing -> locality
    Just faceKey ->
      locality
        { localityHiddenSupport = Map.delete label (localityHiddenSupport locality)
        , localitySupportHidden =
            Map.update
              (\labels ->
                 let retained = Set.delete label labels
                  in if Set.null retained then Nothing else Just retained)
              faceKey
              (localitySupportHidden locality)
        }

buildRegularLocality
  :: Ord label
  => RegularSection label
  -> Maybe (RegularLocality label)
buildRegularLocality section = do
  (seed, _) <- Map.lookupMin (sectionRegularFaces section)
  Foldable.foldlM
    (attachHiddenGenerator section)
    RegularLocality
      { localityFaceSeed = seed
      , localityHiddenSupport = Map.empty
      , localitySupportHidden = Map.empty
      }
    [ label
    | (_, label) <- Map.toAscList (sectionSlopeRepresentatives section)
    , Map.lookup label (sectionRegularDispositions section)
        == Just RegularSiteHidden
    ]

attachHiddenGenerator
  :: Ord label
  => RegularSection label
  -> RegularLocality label
  -> label
  -> Maybe (RegularLocality label)
attachHiddenGenerator section locality label = do
  generator <- Map.lookup label (sectionGenerators section)
  location <- locateRegularSlope (localityFaceSeed locality) section generator
  case location of
    RegularSlopeInside faceKey ->
      Just
        (attachRegularHidden label faceKey locality)
          { localityFaceSeed = faceKey
          }
    RegularSlopeOutside _ -> Nothing

locateRegularSlope
  :: Ord label
  => RegularFaceKey label
  -> RegularSection label
  -> ExactPowerGenerator label
  -> Maybe (RegularSlopeLocation label)
locateRegularSlope seed section generator =
  descendRegularSlope
    (exactPoint queryX queryY)
    section
    (Map.size (sectionRegularFaces section) + 1)
    seed
 where
  (queryX, queryY) = exactGeneratorSlope generator

descendRegularSlope
  :: Ord label
  => ExactPoint
  -> RegularSection label
  -> Int
  -> RegularFaceKey label
  -> Maybe (RegularSlopeLocation label)
descendRegularSlope query section !remaining faceKey
  | remaining <= 0 = Nothing
  | otherwise = do
      face <- Map.lookup faceKey (sectionRegularFaces section)
      case firstExteriorFaceEdge query section face of
        Nothing -> Just (RegularSlopeInside faceKey)
        Just edgeKey -> do
          edgeSection <- Map.lookup edgeKey (sectionRegularEdges section)
          case Set.lookupMin (Set.delete faceKey (sectionRegularEdgeFaces edgeSection)) of
            Just adjacent ->
              descendRegularSlope query section (remaining - 1) adjacent
            Nothing -> Just (RegularSlopeOutside edgeKey)

firstExteriorFaceEdge
  :: Ord label
  => ExactPoint
  -> RegularSection label
  -> RegularFace label
  -> Maybe (RegularEdgeKey label)
firstExteriorFaceEdge query section face =
  fmap
    (uncurry orderedPair)
    ( List.find
        (faceEdgeExcludes query section)
        (regularFaceDirectedEdgeKeys face)
    )

faceEdgeExcludes
  :: Ord label
  => ExactPoint
  -> RegularSection label
  -> RegularEdgeKey label
  -> Bool
faceEdgeExcludes query section (fromLabel, toLabel) =
  generatorEdgeExcludes
    query
    (sectionGenerators section)
    (fromLabel, toLabel)

generatorEdgeExcludes
  :: Ord label
  => ExactPoint
  -> Map label (ExactPowerGenerator label)
  -> RegularEdgeKey label
  -> Bool
generatorEdgeExcludes query generators (fromLabel, toLabel) =
  case (Map.lookup fromLabel generators, Map.lookup toLabel generators) of
    (Just fromGenerator, Just toGenerator) ->
      exactOrient2d
        (uncurry exactPoint (exactGeneratorSlope fromGenerator))
        (uncurry exactPoint (exactGeneratorSlope toGenerator))
        query
        == LT
    _ -> False

generatorEdgeExcludesScaled
  :: Ord label
  => ExactRational
  -> (ExactRational, ExactRational)
  -> Map label (ExactPowerGenerator label)
  -> RegularEdgeKey label
  -> Bool
generatorEdgeExcludesScaled scale (queryX, queryY) generators (fromLabel, toLabel) =
  case (Map.lookup fromLabel generators, Map.lookup toLabel generators) of
    (Just fromGenerator, Just toGenerator) ->
      let (fromX, fromY) = exactGeneratorSlope fromGenerator
          (toX, toY) = exactGeneratorSlope toGenerator
       in (toX - fromX) * (queryY - scale * fromY)
            - (toY - fromY) * (queryX - scale * fromX)
            < 0
    _ -> False
