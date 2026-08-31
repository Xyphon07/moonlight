{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | The sole construction boundary for nonidentity epoch deltas.
module Moonlight.Delta.Epoch.Internal.Construction
  ( epochDelta,
    identityDelta,
  )
where

import Data.Maybe (fromMaybe)
import Moonlight.Core (OrdMap (..), OrdSet (..))
import Moonlight.Delta.Epoch.Internal.Types
  ( DeltaViolation (..),
    Endpoint (..),
    EpochDelta (..),
    EpochKeyed,
    survivingTransportImage,
  )
import Prelude
  ( Either (..),
    Ord ((>=)),
    fmap,
    fst,
    not,
    otherwise,
    (/=),
  )

epochDelta ::
  EpochKeyed keyMap observed =>
  Endpoint observed ->
  Endpoint observed ->
  keyMap ->
  observed ->
  observed ->
  Either (DeltaViolation (SetKey observed)) (EpochDelta keyMap observed)
epochDelta sourceEndpoint targetEndpoint transportProposal retiredProposal changedProposal
  | sourceVersion >= targetVersion =
      Left (VersionDidNotAdvance sourceVersion targetVersion)
  | otherwise =
      case domainEscapes of
        badKey : _ ->
          Left (TransportDomainEscapesSource badKey)
        [] ->
          case imageEscapes of
            (badKey, badImage) : _ ->
              Left (TransportImageEscapesTarget badKey badImage)
            [] ->
              case retiredEscapes of
                badKey : _ ->
                  Left (RetiredKeyOutsideSource badKey)
                [] ->
                  case transportsRetired of
                    badKey : _ ->
                      Left (TransportDefinedForRetiredSource badKey)
                    [] ->
                      case survivingEscapes of
                        badKey : _ ->
                          Left (SurvivingKeyOutsideTarget badKey)
                        [] ->
                          case changedEscapes of
                            badKey : _ ->
                              Left (ChangedKeyOutsideSource badKey)
                            [] ->
                              Right
                                EpochDelta
                                  { sourceEndpoint = sourceEndpoint,
                                    targetEndpoint = targetEndpoint,
                                    transportOverride = strippedTransport,
                                    retiredSourceKeys = retiredProposal,
                                    dirtyTargetKeys = targetDirtyKeys
                                  }
  where
    sourceVersion = endpointVersion sourceEndpoint
    targetVersion = endpointVersion targetEndpoint
    sourceKeySet = endpointKeys sourceEndpoint
    targetKeySet = endpointKeys targetEndpoint
    proposalEntries = toAscListMap transportProposal
    strippedEntries =
      [ (sourceKey, targetKey)
        | (sourceKey, targetKey) <- proposalEntries,
          sourceKey /= targetKey
      ]
    strippedTransport = fromListMap strippedEntries
    domainEscapes =
      [ sourceKey
        | (sourceKey, _) <- proposalEntries,
          not (memberSet sourceKey sourceKeySet)
      ]
    imageEscapes =
      [ (sourceKey, targetKey)
        | (sourceKey, targetKey) <- proposalEntries,
          not (memberSet targetKey targetKeySet)
      ]
    retiredEscapes =
      toAscListSet (differenceSet retiredProposal sourceKeySet)
    transportsRetired =
      [ sourceKey
        | (sourceKey, _) <- proposalEntries,
          memberSet sourceKey retiredProposal
      ]
    survivingEscapes =
      toAscListSet (differenceSet identitySurvivingKeys targetKeySet)
    identitySurvivingKeys =
      differenceSet
        (differenceSet sourceKeySet retiredProposal)
        (fromListSet (fmap fst strippedEntries))
    targetFor sourceKey =
      fromMaybe sourceKey (lookupMap sourceKey strippedTransport)
    changedEscapes =
      toAscListSet (differenceSet changedProposal sourceKeySet)
    survivingImage =
      survivingTransportImage sourceKeySet strippedTransport retiredProposal
    freshTargetKeys =
      differenceSet targetKeySet survivingImage
    transportedChangedKeys =
      fromListSet
        [ targetFor sourceKey
          | sourceKey <- toAscListSet changedProposal,
            not (memberSet sourceKey retiredProposal)
        ]
    targetDirtyKeys =
      unionSet transportedChangedKeys freshTargetKeys

identityDelta ::
  EpochKeyed keyMap observed =>
  Endpoint observed ->
  EpochDelta keyMap observed
identityDelta endpoint =
  EpochDelta
    { sourceEndpoint = endpoint,
      targetEndpoint = endpoint,
      transportOverride = emptyMap,
      retiredSourceKeys = emptySet,
      dirtyTargetKeys = emptySet
    }
