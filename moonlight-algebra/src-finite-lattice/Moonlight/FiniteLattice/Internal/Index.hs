{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE RoleAnnotations #-}

module Moonlight.FiniteLattice.Internal.Index
  ( ContextIndex (..),
    contextIndexFromUniverse,
    contextIndexValueForKey,
    lookupCompileKey,
    contextLatticeFromPlan,
    encodeRelationKeyPairs,
    reflexiveKeyPairs,
    decodeIndexKeys,
    firstDuplicate,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Moonlight.FiniteLattice.Internal.Key
  ( ContextKey (..),
    ContextKeySet,
    contextKeySetToAscList,
  )
import Moonlight.FiniteLattice.Internal.Invariant
  ( boxedIndexInvariant,
  )
import Moonlight.FiniteLattice.Internal.Plan (ContextPlan)
import Moonlight.FiniteLattice.Internal.Types
  ( ContextLattice (..),
    ContextLatticeCompileError (..),
  )

data ContextIndex c = ContextIndex
  { ciContextsByKey :: !(Vector.Vector c),
    ciKeyByContext :: !(Map c ContextKey),
    ciSize :: !Int
  }

type role ContextIndex nominal

contextIndexFromUniverse :: Set c -> Either (ContextLatticeCompileError c) (ContextIndex c)
contextIndexFromUniverse universe =
  case contexts of
    [] -> Left ContextLatticeEmptyUniverse
    _ : _ ->
      Right
        ContextIndex
          { ciContextsByKey = Vector.fromList contexts,
            ciKeyByContext = Map.fromDistinctAscList keyEntries,
            ciSize = Set.size universe
          }
  where
    contexts = Set.toAscList universe
    keyEntries =
      zipWith
        (\contextValue keyOrdinal -> (contextValue, ContextKey keyOrdinal))
        contexts
        [0 ..]

contextIndexValueForKey :: ContextIndex c -> ContextKey -> c
contextIndexValueForKey index (ContextKey keyOrdinal) =
  contextIndexValueAtOrdinal index keyOrdinal
{-# INLINE contextIndexValueForKey #-}

lookupCompileKey ::
  Ord c =>
  ContextIndex c ->
  c ->
  ContextLatticeCompileError c ->
  Either (ContextLatticeCompileError c) ContextKey
lookupCompileKey index contextValue missingError =
  maybe
    (Left missingError)
    Right
    (Map.lookup contextValue (ciKeyByContext index))

contextLatticeFromPlan ::
  c ->
  c ->
  ContextKey ->
  ContextKey ->
  ContextIndex c ->
  ContextPlan ->
  ContextLattice c
contextLatticeFromPlan topValue bottomValue topKey bottomKey index plan =
  ContextLattice
    { clTop = topValue,
      clBottom = bottomValue,
      clTopKey = topKey,
      clBottomKey = bottomKey,
      clContextsByKey = ciContextsByKey index,
      clKeyByContext = ciKeyByContext index,
      clPlan = plan,
      clSize = ciSize index
    }

encodeRelationKeyPairs ::
  Ord c =>
  ContextIndex c ->
  Set (c, c) ->
  Either (ContextLatticeCompileError c) [(ContextKey, ContextKey)]
encodeRelationKeyPairs index relation =
  traverse encodePair (Set.toAscList relation)
  where
    encodePair pair@(lowerContext, upperContext) = do
      lowerKey <-
        maybe
          (Left (ContextLatticeUnknownRelationEndpoint pair))
          Right
          (Map.lookup lowerContext (ciKeyByContext index))
      upperKey <-
        maybe
          (Left (ContextLatticeUnknownRelationEndpoint pair))
          Right
          (Map.lookup upperContext (ciKeyByContext index))
      pure (lowerKey, upperKey)

reflexiveKeyPairs :: ContextIndex c -> [(ContextKey, ContextKey)]
reflexiveKeyPairs index =
  [ (ContextKey keyOrdinal, ContextKey keyOrdinal)
  | keyOrdinal <- [0 .. ciSize index - 1]
  ]

decodeIndexKeys :: Ord c => ContextIndex c -> ContextKeySet -> Set c
decodeIndexKeys index =
  Set.fromAscList
    . fmap (contextIndexValueForKey index . ContextKey)
    . contextKeySetToAscList

firstDuplicate :: Ord c => [c] -> Maybe c
firstDuplicate =
  go Set.empty
  where
    go :: Ord c => Set c -> [c] -> Maybe c
    go _ [] = Nothing
    go seen (contextValue : rest)
      | Set.member contextValue seen = Just contextValue
      | otherwise = go (Set.insert contextValue seen) rest

contextIndexValueAtOrdinal :: ContextIndex c -> Int -> c
contextIndexValueAtOrdinal index =
  boxedIndexInvariant (ciContextsByKey index)
{-# INLINE contextIndexValueAtOrdinal #-}
