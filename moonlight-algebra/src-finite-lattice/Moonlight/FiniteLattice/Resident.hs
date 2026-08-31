{-# LANGUAGE GHC2024 #-}

-- | Resident context keys: brand-parameterised handles that tie compiled-lattice
-- keys to the plan that issued them. The brand's nominal role is the safety
-- guarantee (see @negative-finite-lattice/@ for the must-not-compile fixture).
module Moonlight.FiniteLattice.Resident
  ( ResidentContext,
    ResidentContextKey,
    residentContextKeyOrdinal,
    ResidentContextKeySet,
    residentContextKeySetNull,
    residentContextKeySetCardinality,
    residentContextKeySetFoldr,
    residentContextKeySetToAscList,
    residentContextKeySetMember,
    ResidentContextElement,
    residentContextElementKey,
    residentContextElementValue,
    withResidentContext,
    residentContextSize,
    residentContextKeys,
    residentContextElements,
    residentContextKeyFromOrdinal,
    checkResidentContext,
    residentContextElementForKey,
    residentContextUpperKeys,
    residentContextLowerKeys,
    residentContextKeyLeq,
    residentJoinKey,
    residentMeetKey,
    residentJoinMeetKeys,
    residentJoin,
    residentMeet,
  )
where

import Data.Map.Strict qualified as Map
import Moonlight.FiniteLattice.Internal.Key
  ( contextKeySetCardinality,
    contextKeySetFoldr,
    contextKeySetMember,
    contextKeySetNull,
  )
import Moonlight.FiniteLattice.Internal.Plan
  ( contextPlanJoinKey,
    contextPlanJoinMeetKeys,
    contextPlanLeq,
    contextPlanLowerKeys,
    contextPlanMeetKey,
    contextPlanUpperKeys,
  )
import Moonlight.FiniteLattice.Internal.Types
  ( ContextLattice (..),
    ContextLatticeLookupError (..),
    ResidentContext (..),
    ResidentContextElement (..),
    ResidentContextKey (..),
    ResidentContextKeySet (..),
    contextKeyFromResidentKey,
    residentContextElementForKey,
    residentKeyFromContextKey,
  )

withResidentContext ::
  ContextLattice c ->
  (forall s. ResidentContext s c -> result) ->
  result
withResidentContext lattice continuation =
  continuation (ResidentContext lattice)
{-# INLINE withResidentContext #-}

residentContextSize :: ResidentContext s c -> Int
residentContextSize (ResidentContext lattice) =
  clSize lattice
{-# INLINE residentContextSize #-}

residentContextKeys :: ResidentContext s c -> [ResidentContextKey s]
residentContextKeys (ResidentContext lattice) =
  fmap ResidentContextKey [0 .. clSize lattice - 1]
{-# INLINE residentContextKeys #-}

residentContextElements :: ResidentContext s c -> [ResidentContextElement s c]
residentContextElements context =
  residentContextElementForKey context <$> residentContextKeys context

residentContextKeyFromOrdinal ::
  ResidentContext s c ->
  Int ->
  Maybe (ResidentContextKey s)
residentContextKeyFromOrdinal (ResidentContext lattice) keyOrdinal
  | keyOrdinal >= 0 && keyOrdinal < clSize lattice =
      Just (ResidentContextKey keyOrdinal)
  | otherwise = Nothing

checkResidentContext ::
  Ord c =>
  ResidentContext s c ->
  c ->
  Either (ContextLatticeLookupError c) (ResidentContextElement s c)
checkResidentContext context@(ResidentContext lattice) contextValue =
  case Map.lookup contextValue (clKeyByContext lattice) of
    Nothing -> Left (ContextLatticeUnknownContext contextValue)
    Just contextKey ->
      Right
        ( residentContextElementForKey
            context
            (residentKeyFromContextKey contextKey)
        )

residentContextKeySetNull :: ResidentContextKeySet s -> Bool
residentContextKeySetNull (ResidentContextKeySet keySet) =
  contextKeySetNull keySet
{-# INLINE residentContextKeySetNull #-}

residentContextKeySetCardinality :: ResidentContextKeySet s -> Int
residentContextKeySetCardinality (ResidentContextKeySet keySet) =
  contextKeySetCardinality keySet
{-# INLINE residentContextKeySetCardinality #-}

residentContextKeySetFoldr ::
  (ResidentContextKey s -> result -> result) ->
  result ->
  ResidentContextKeySet s ->
  result
residentContextKeySetFoldr step initial (ResidentContextKeySet keySet) =
  contextKeySetFoldr
    (\keyOrdinal rest -> step (ResidentContextKey keyOrdinal) rest)
    initial
    keySet

residentContextKeySetToAscList ::
  ResidentContextKeySet s ->
  [ResidentContextKey s]
residentContextKeySetToAscList =
  residentContextKeySetFoldr (:) []

residentContextKeySetMember ::
  ResidentContextKey s ->
  ResidentContextKeySet s ->
  Bool
residentContextKeySetMember (ResidentContextKey keyOrdinal) (ResidentContextKeySet keySet) =
  contextKeySetMember keyOrdinal keySet

residentContextUpperKeys ::
  ResidentContext s c ->
  ResidentContextKey s ->
  ResidentContextKeySet s
residentContextUpperKeys (ResidentContext lattice) residentKey =
  ResidentContextKeySet
    ( contextPlanUpperKeys
        (clPlan lattice)
        (contextKeyFromResidentKey residentKey)
    )

residentContextLowerKeys ::
  ResidentContext s c ->
  ResidentContextKey s ->
  ResidentContextKeySet s
residentContextLowerKeys (ResidentContext lattice) residentKey =
  ResidentContextKeySet
    ( contextPlanLowerKeys
        (clPlan lattice)
        (contextKeyFromResidentKey residentKey)
    )

residentContextKeyLeq ::
  ResidentContext s c ->
  ResidentContextKey s ->
  ResidentContextKey s ->
  Bool
residentContextKeyLeq (ResidentContext lattice) leftKey rightKey =
  contextPlanLeq
    (clPlan lattice)
    (contextKeyFromResidentKey leftKey)
    (contextKeyFromResidentKey rightKey)
{-# INLINE residentContextKeyLeq #-}

residentJoinKey ::
  ResidentContext s c ->
  ResidentContextKey s ->
  ResidentContextKey s ->
  ResidentContextKey s
residentJoinKey (ResidentContext lattice) leftKey rightKey =
  residentKeyFromContextKey
    ( contextPlanJoinKey
        (clPlan lattice)
        (contextKeyFromResidentKey leftKey)
        (contextKeyFromResidentKey rightKey)
    )
{-# INLINE residentJoinKey #-}

residentMeetKey ::
  ResidentContext s c ->
  ResidentContextKey s ->
  ResidentContextKey s ->
  ResidentContextKey s
residentMeetKey (ResidentContext lattice) leftKey rightKey =
  residentKeyFromContextKey
    ( contextPlanMeetKey
        (clPlan lattice)
        (contextKeyFromResidentKey leftKey)
        (contextKeyFromResidentKey rightKey)
    )
{-# INLINE residentMeetKey #-}

residentJoinMeetKeys ::
  ResidentContext s c ->
  ResidentContextKey s ->
  ResidentContextKey s ->
  (ResidentContextKey s, ResidentContextKey s)
residentJoinMeetKeys (ResidentContext lattice) leftKey rightKey =
  let (joinKey, meetKey) =
        contextPlanJoinMeetKeys
        (clPlan lattice)
        (contextKeyFromResidentKey leftKey)
        (contextKeyFromResidentKey rightKey)
   in (residentKeyFromContextKey joinKey, residentKeyFromContextKey meetKey)
{-# INLINE residentJoinMeetKeys #-}

residentJoin ::
  ResidentContext s c ->
  ResidentContextElement s c ->
  ResidentContextElement s c ->
  ResidentContextElement s c
residentJoin context leftElement rightElement =
  residentContextElementForKey
    context
    ( residentJoinKey
      context
      (residentContextElementKey leftElement)
      (residentContextElementKey rightElement)
    )

residentMeet ::
  ResidentContext s c ->
  ResidentContextElement s c ->
  ResidentContextElement s c ->
  ResidentContextElement s c
residentMeet context leftElement rightElement =
  residentContextElementForKey
    context
    ( residentMeetKey
      context
      (residentContextElementKey leftElement)
      (residentContextElementKey rightElement)
    )
