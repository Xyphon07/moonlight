{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Query descent and view gluing for partial epoch transports.
module Moonlight.Delta.Epoch.Internal.Transport
  ( transportKeys,
    transportView,
  )
where

import Moonlight.Core (OrdMap (..), OrdSet (..))
import Moonlight.Delta.Epoch.Internal.Types
  ( EpochDelta,
    EpochKeyed,
    Transport (..),
    ViewTransportError (..),
    retiredKeys,
    sourceKeys,
    sourceVersion,
    targetVersion,
    transportKeyTotal,
  )
import Moonlight.Delta.Epoch.Internal.View
  ( ContextView (..),
    viewWithSupport,
    viewWithVersion,
  )
import Prelude (Either (..), Eq ((/=)), Maybe (..), fmap, otherwise, snd)

transportKeys ::
  EpochKeyed keyMap observed =>
  EpochDelta keyMap observed ->
  observed ->
  Transport keyMap observed
transportKeys deltaValue queryKeys =
  Transport
    { transportedKeys =
        fromListMap
          [ (sourceKey, targetKey)
            | sourceKey <- toAscListSet survivingKeys,
              Just targetKey <- [transportKeyTotal deltaValue sourceKey]
          ],
      transportRetiredKeys = queriedRetiredKeys,
      transportUnknownKeys = unknownKeys
    }
  where
    knownKeys = intersectionSet queryKeys (sourceKeys deltaValue)
    unknownKeys = differenceSet queryKeys (sourceKeys deltaValue)
    queriedRetiredKeys = intersectionSet knownKeys (retiredKeys deltaValue)
    survivingKeys = differenceSet knownKeys queriedRetiredKeys
{-# INLINABLE transportKeys #-}

transportView ::
  EpochKeyed keyMap observed =>
  EpochDelta keyMap observed ->
  ContextView observed section ->
  Either (ViewTransportError (SetKey observed)) (ContextView observed section)
transportView deltaValue contextView
  | cvVersion contextView /= sourceVersion deltaValue =
      Left (ViewSourceVersionMismatch (sourceVersion deltaValue) (cvVersion contextView))
  | otherwise =
      case toAscListSet (transportUnknownKeys transportResult) of
        unknownKey : _ ->
          Left (ViewObservedKeyUnknown unknownKey)
        [] ->
          Right
            ( viewWithVersion
                (targetVersion deltaValue)
                ( viewWithSupport
                    (fromListSet (fmap snd (toAscListMap (transportedKeys transportResult))))
                    contextView
                )
            )
  where
    transportResult =
      transportKeys deltaValue (cvObservedKeys contextView)
{-# INLINABLE transportView #-}
