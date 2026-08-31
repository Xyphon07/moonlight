{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Total composition of boundary-compatible partial epoch transports.
module Moonlight.Delta.Epoch.Internal.Compose
  ( composeDelta,
  )
where

import Moonlight.Core (OrdMap (..), OrdSet (..))
import Moonlight.Delta.Epoch.Internal.Types
  ( ComposeError (..),
    EpochDelta (..),
    EpochKeyed,
    sourceKeys,
    sourceVersion,
    targetKeys,
    targetVersion,
    transportKeyTotal,
  )
import Prelude
  ( Either (..),
    Eq,
    Maybe (..),
    otherwise,
    (>>=),
    (/=),
  )

-- | Compose a newer delta over an older delta.
composeDelta ::
  (EpochKeyed keyMap observed, Eq observed) =>
  EpochDelta keyMap observed ->
  EpochDelta keyMap observed ->
  Either (ComposeError (SetKey observed)) (EpochDelta keyMap observed)
composeDelta newer older
  | targetVersion older /= sourceVersion newer =
      Left (ComposeVersionMismatch (targetVersion older) (sourceVersion newer))
  | targetKeys older /= sourceKeys newer =
      Left ComposeUniverseMismatch
  | otherwise =
      Right
        EpochDelta
          { sourceEndpoint = sourceEndpoint older,
            targetEndpoint = targetEndpoint newer,
            transportOverride = compositeOverrides,
            retiredSourceKeys = compositeRetired,
            dirtyTargetKeys = compositeDirty
          }
  where
    sourceKeyList = toAscListSet (sourceKeys older)

    compositeTarget sourceKey =
      transportKeyTotal older sourceKey
        >>= transportKeyTotal newer

    compositeRetired =
      fromListSet
        [ sourceKey
          | sourceKey <- sourceKeyList,
            Nothing <- [compositeTarget sourceKey]
        ]

    compositeOverrides =
      fromListMap
        [ (sourceKey, targetKey)
          | sourceKey <- sourceKeyList,
            Just targetKey <- [compositeTarget sourceKey],
            sourceKey /= targetKey
        ]

    transportedOlderDirty =
      fromListSet
        [ targetKey
          | dirtyKey <- toAscListSet (dirtyTargetKeys older),
            Just targetKey <- [transportKeyTotal newer dirtyKey]
        ]

    compositeDirty =
      unionSet transportedOlderDirty (dirtyTargetKeys newer)
