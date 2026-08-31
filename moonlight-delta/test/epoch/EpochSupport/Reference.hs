module EpochSupport.Reference
  ( EpochReference (..),
    referenceFromInput,
    referenceFromDelta,
    referenceTransportKeys,
    referenceChangedKeys,
    referenceRetiredKeys,
    referenceTransportView,
    referenceCompose,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Maybe (catMaybes)
import Moonlight.Delta.Epoch
import EpochSupport.Types (EpochInput (..))

-- | A deliberately denormalized sequential oracle. Every source key owns an
-- explicit 'Maybe' target, so composition is ordinary Kleisli composition of
-- partial functions rather than a copy of the production sparse formula.
data EpochReference = EpochReference
  { erSourceVersion :: !Version,
    erTargetVersion :: !Version,
    erSourceKeys :: !IntSet,
    erTargetKeys :: !IntSet,
    erMovement :: !(IntMap (Maybe Int)),
    erDirtyTargetKeys :: !IntSet
  }
  deriving stock (Eq, Show)

-- | Lower an accepted constructor input into the deliberately denormalized
-- oracle without consulting any production transport observer.
referenceFromInput :: EpochInput (IntMap Int) IntSet -> EpochReference
referenceFromInput input =
  EpochReference
    { erSourceVersion = endpointVersion (eiSource input),
      erTargetVersion = endpointVersion (eiTarget input),
      erSourceKeys = sourceKeySet,
      erTargetKeys = targetKeySet,
      erMovement = movement,
      erDirtyTargetKeys = IntSet.union transportedChanged freshTargetKeys
    }
  where
    sourceKeySet = endpointKeys (eiSource input)
    targetKeySet = endpointKeys (eiTarget input)
    movement =
      IntMap.fromAscList
        [ ( sourceKey,
            if IntSet.member sourceKey (eiRetired input)
              then Nothing
              else Just (IntMap.findWithDefault sourceKey sourceKey (eiTransport input))
          )
          | sourceKey <- IntSet.toAscList sourceKeySet
        ]
    freshTargetKeys =
      IntSet.difference targetKeySet (IntSet.fromList (catMaybes (IntMap.elems movement)))
    transportedChanged =
      IntSet.fromList
        [ targetKey
          | sourceKey <- IntSet.toAscList (eiChanged input),
            Just (Just targetKey) <- [IntMap.lookup sourceKey movement]
        ]

referenceFromDelta :: EpochDelta (IntMap Int) IntSet -> EpochReference
referenceFromDelta deltaValue =
  EpochReference
    { erSourceVersion = sourceVersion deltaValue,
      erTargetVersion = targetVersion deltaValue,
      erSourceKeys = sourceKeys deltaValue,
      erTargetKeys = targetKeys deltaValue,
      erMovement =
        IntMap.fromList
          ( fmap (fmap Just) (IntMap.toAscList (transportedKeys transportResult))
              <> fmap (,Nothing) (IntSet.toAscList (transportRetiredKeys transportResult))
          ),
      erDirtyTargetKeys = changedKeysAcrossEpoch deltaValue
    }
  where
    transportResult =
      transportKeys deltaValue (sourceKeys deltaValue)

referenceTransportKeys :: EpochReference -> IntSet -> Transport (IntMap Int) IntSet
referenceTransportKeys reference queryKeys =
  Transport
    { transportedKeys =
        IntMap.fromList
          [ (sourceKey, targetKey)
            | sourceKey <- IntSet.toAscList knownKeys,
              Just (Just targetKey) <- [IntMap.lookup sourceKey (erMovement reference)]
          ],
      transportRetiredKeys =
        IntSet.fromList
          [ sourceKey
            | sourceKey <- IntSet.toAscList knownKeys,
              Just Nothing <- [IntMap.lookup sourceKey (erMovement reference)]
          ],
      transportUnknownKeys =
        IntSet.difference queryKeys (erSourceKeys reference)
    }
  where
    knownKeys =
      IntSet.intersection queryKeys (erSourceKeys reference)

referenceChangedKeys :: EpochReference -> IntSet
referenceChangedKeys =
  erDirtyTargetKeys

referenceRetiredKeys :: EpochReference -> IntSet
referenceRetiredKeys reference =
  IntSet.fromList
    [ sourceKey
      | (sourceKey, Nothing) <- IntMap.toAscList (erMovement reference)
    ]

referenceTransportView ::
  EpochReference ->
  ContextView IntSet section ->
  Either (ViewTransportError Int) (ContextView IntSet section)
referenceTransportView reference contextView
  | cvVersion contextView /= erSourceVersion reference =
      Left (ViewSourceVersionMismatch (erSourceVersion reference) (cvVersion contextView))
  | otherwise =
      case IntSet.toAscList (transportUnknownKeys transportResult) of
        unknownKey : _ ->
          Left (ViewObservedKeyUnknown unknownKey)
        [] ->
          Right
            ( viewWithVersion
                (erTargetVersion reference)
                (viewWithSupport (IntSet.fromList (IntMap.elems (transportedKeys transportResult))) contextView)
            )
  where
    transportResult =
      referenceTransportKeys reference (cvObservedKeys contextView)

referenceCompose ::
  EpochReference ->
  EpochReference ->
  Either (ComposeError Int) EpochReference
referenceCompose newer older
  | erTargetVersion older /= erSourceVersion newer =
      Left (ComposeVersionMismatch (erTargetVersion older) (erSourceVersion newer))
  | erTargetKeys older /= erSourceKeys newer =
      Left ComposeUniverseMismatch
  | otherwise =
      Right
        EpochReference
          { erSourceVersion = erSourceVersion older,
            erTargetVersion = erTargetVersion newer,
            erSourceKeys = erSourceKeys older,
            erTargetKeys = erTargetKeys newer,
            erMovement = IntMap.map composeMovement (erMovement older),
            erDirtyTargetKeys =
              IntSet.union
                (transportDirtyThroughReference newer (erDirtyTargetKeys older))
                (erDirtyTargetKeys newer)
          }
  where
    composeMovement Nothing =
      Nothing
    composeMovement (Just intermediateKey) =
      IntMap.findWithDefault Nothing intermediateKey (erMovement newer)

transportDirtyThroughReference :: EpochReference -> IntSet -> IntSet
transportDirtyThroughReference reference dirtyKeys =
  IntSet.fromList
    [ targetKey
      | dirtyKey <- IntSet.toAscList dirtyKeys,
        Just (Just targetKey) <- [IntMap.lookup dirtyKey (erMovement reference)]
    ]
