-- | Generator supports over a finite 'ContextLattice': semantic value-boundary
-- supports plus resident antichain kernels over branded lattice keys.
module Moonlight.FiniteLattice.Support
  ( SupportBasis,
    supportBasis,
    supportBasisWithOrder,
    emptySupport,
    supportGenerators,
    principalSupport,
    supportContains,
    supportReachableContexts,
    supportReachableLatticeContexts,
    normalizeSupport,
    supportUnion,
    supportMeet,
    ResidentSupport,
    residentSupportFromElements,
    residentSupportFromKeys,
    residentSupportKeys,
    residentSupportWithClosure,
    residentSupportContainsKey,
    residentSupportContainsElement,
    residentSupportReachableElements,
    residentSupportUnion,
    residentSupportMeet,
  )
where

import Data.Kind (Type)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.FiniteLattice.Internal.Key
  ( ContextKey (..),
    ContextKeySet,
    contextKeySetChunkCount,
    contextKeySetFilter,
    contextKeySetFromKeys,
    contextKeySetImage2,
    contextKeySetIntersects,
    contextKeySetIntersectsExcept,
    contextKeySetUnion,
    contextKeySetUnionImages,
  )
import Moonlight.FiniteLattice.Internal.Plan
  ( contextPlanJoinKey,
    contextPlanLowerKeys,
    contextPlanUpperKeys,
  )
import Moonlight.FiniteLattice.Internal.Types
  ( ContextLattice (..),
    ResidentContext (..),
    ResidentContextKeySet (..),
    contextKeyFromResidentKey,
  )
import Moonlight.FiniteLattice.Core
  ( ContextLatticeLookupError (..),
  )
import Moonlight.FiniteLattice.Resident
  ( ResidentContextElement,
    ResidentContextKey,
    checkResidentContext,
    residentContextElementForKey,
    residentContextElementKey,
    residentContextElementValue,
    residentContextElements,
    residentContextKeyOrdinal,
    residentContextKeySetMember,
    residentContextKeySetToAscList,
    withResidentContext,
  )

type SupportBasis :: Type -> Type
newtype SupportBasis c = SupportBasis
  { unSupportBasis :: Set c
  }
  deriving stock (Eq, Ord, Show)

type ResidentSupport :: Type -> Type -> Type
data ResidentSupport s c = ResidentSupport
  { rsMinimalKeys :: !(ResidentContextKeySet s),
    rsClosure :: !(Maybe (ResidentContextKeySet s))
  }
  deriving stock (Eq, Show)

supportBasis :: Ord c => ContextLattice c -> [c] -> Either (ContextLatticeLookupError c) (SupportBasis c)
supportBasis contextLatticeValue =
  normalizeSupport contextLatticeValue . SupportBasis . Set.fromList

supportBasisWithOrder :: Ord c => (c -> c -> Bool) -> [c] -> SupportBasis c
supportBasisWithOrder leqValue contexts =
  SupportBasis (Set.filter isMinimal candidates)
  where
    candidates =
      Set.fromList contexts
    isMinimal candidateValue =
      not
        ( any
            (\otherValue -> otherValue /= candidateValue && leqValue otherValue candidateValue)
            (Set.toAscList candidates)
        )

emptySupport :: SupportBasis c
emptySupport =
  SupportBasis Set.empty

supportGenerators :: SupportBasis c -> [c]
supportGenerators =
  Set.toAscList . unSupportBasis

principalSupport :: c -> SupportBasis c
principalSupport =
  SupportBasis . Set.singleton

supportContains :: Ord c => ContextLattice c -> SupportBasis c -> c -> Either (ContextLatticeLookupError c) Bool
supportContains contextLatticeValue supportValue contextValue =
  withResidentSupport contextLatticeValue supportValue $ \residentContext support ->
    residentSupportContainsElement residentContext support
      <$> checkResidentContext residentContext contextValue

supportReachableContexts :: Ord c => ContextLattice c -> [c] -> SupportBasis c -> Either (ContextLatticeLookupError c) [c]
supportReachableContexts contextLatticeValue candidateContexts supportValue =
  withResidentSupport contextLatticeValue supportValue $ \residentContext support ->
    let cachedSupport = residentSupportWithClosure residentContext support
     in catMaybes
          <$>
          traverse
            (reachableCandidate residentContext cachedSupport)
            candidateContexts

supportReachableLatticeContexts :: Ord c => ContextLattice c -> SupportBasis c -> Either (ContextLatticeLookupError c) [c]
supportReachableLatticeContexts contextLatticeValue supportValue =
  withResidentSupport contextLatticeValue supportValue $ \residentContext support ->
    pure
      ( residentContextElementValue
          <$> residentSupportReachableElements residentContext support
      )

normalizeSupport :: Ord c => ContextLattice c -> SupportBasis c -> Either (ContextLatticeLookupError c) (SupportBasis c)
normalizeSupport contextLatticeValue supportValue =
  withResidentSupport contextLatticeValue supportValue $ \residentContext support ->
    pure (supportFromResidentSupport residentContext support)

supportUnion :: Ord c => ContextLattice c -> SupportBasis c -> SupportBasis c -> Either (ContextLatticeLookupError c) (SupportBasis c)
supportUnion contextLatticeValue leftSupport rightSupport =
  withResidentSupports contextLatticeValue leftSupport rightSupport $ \residentContext residentLeft residentRight ->
    pure
      ( supportFromResidentSupport
          residentContext
          (residentSupportUnion residentContext residentLeft residentRight)
      )

supportMeet :: Ord c => ContextLattice c -> SupportBasis c -> SupportBasis c -> Either (ContextLatticeLookupError c) (SupportBasis c)
supportMeet contextLatticeValue leftSupport rightSupport =
  withResidentSupports contextLatticeValue leftSupport rightSupport $ \residentContext residentLeft residentRight -> do
    pure
      ( supportFromResidentSupport
          residentContext
          (residentSupportMeet residentContext residentLeft residentRight)
      )

residentSupportFromElements :: Ord c => ResidentContext s c -> [c] -> Either (ContextLatticeLookupError c) (ResidentSupport s c)
residentSupportFromElements contextValue contextValues =
  residentSupportFromKeys contextValue
    . fmap residentContextElementKey
    <$> traverse (checkResidentContext contextValue) contextValues

residentSupportFromKeys :: ResidentContext s c -> [ResidentContextKey s] -> ResidentSupport s c
residentSupportFromKeys (ResidentContext lattice) keys =
  ResidentSupport
    { rsMinimalKeys =
        ResidentContextKeySet
          ( minimalResidentKeySet
              lattice
              ( contextKeySetFromKeys
                  (contextKeySetChunkCount (clSize lattice))
                  (fmap residentContextKeyOrdinal keys)
              )
          ),
      rsClosure = Nothing
    }

residentSupportKeys :: ResidentContext s c -> ResidentSupport s c -> [ResidentContextKey s]
residentSupportKeys _ supportValue =
  residentContextKeySetToAscList (rsMinimalKeys supportValue)

residentSupportWithClosure :: ResidentContext s c -> ResidentSupport s c -> ResidentSupport s c
residentSupportWithClosure contextValue supportValue =
  case rsClosure supportValue of
    Just _ ->
      supportValue
    Nothing ->
      supportValue
        { rsClosure = Just (residentSupportClosure contextValue supportValue)
        }

residentSupportContainsKey :: ResidentContext s c -> ResidentSupport s c -> ResidentContextKey s -> Bool
residentSupportContainsKey (ResidentContext lattice) supportValue candidateKey =
  case rsClosure supportValue of
    Just closure ->
      residentContextKeySetMember candidateKey closure
    Nothing ->
      case rsMinimalKeys supportValue of
        ResidentContextKeySet minimalKeys ->
          contextKeySetIntersects
            minimalKeys
            ( contextPlanLowerKeys
                (clPlan lattice)
                (contextKeyFromResidentKey candidateKey)
            )

residentSupportContainsElement :: ResidentContext s c -> ResidentSupport s c -> ResidentContextElement s c -> Bool
residentSupportContainsElement contextValue supportValue =
  residentSupportContainsKey contextValue supportValue . residentContextElementKey

residentSupportReachableElements :: ResidentContext s c -> ResidentSupport s c -> [ResidentContextElement s c]
residentSupportReachableElements contextValue supportValue =
  let closure =
        case rsClosure supportValue of
          Just cachedClosure -> cachedClosure
          Nothing -> residentSupportClosure contextValue supportValue
   in [ contextElement
      | contextElement <- residentContextElements contextValue,
        residentContextKeySetMember (residentContextElementKey contextElement) closure
      ]

residentSupportUnion :: ResidentContext s c -> ResidentSupport s c -> ResidentSupport s c -> ResidentSupport s c
residentSupportUnion (ResidentContext lattice) leftSupport rightSupport =
  case (rsMinimalKeys leftSupport, rsMinimalKeys rightSupport) of
    (ResidentContextKeySet leftKeys, ResidentContextKeySet rightKeys) ->
      ResidentSupport
        { rsMinimalKeys =
            ResidentContextKeySet
              (minimalResidentKeySet lattice (contextKeySetUnion leftKeys rightKeys)),
          rsClosure = Nothing
        }

residentSupportMeet :: ResidentContext s c -> ResidentSupport s c -> ResidentSupport s c -> ResidentSupport s c
residentSupportMeet (ResidentContext lattice) leftSupport rightSupport =
  case (rsMinimalKeys leftSupport, rsMinimalKeys rightSupport) of
    (ResidentContextKeySet leftKeys, ResidentContextKeySet rightKeys) ->
      let chunkCount =
            contextKeySetChunkCount (clSize lattice)
          pairwiseJoins =
            contextKeySetImage2
              chunkCount
              ( \leftOrdinal rightOrdinal ->
                  contextKeyOrdinal
                    ( contextPlanJoinKey
                        (clPlan lattice)
                        (ContextKey leftOrdinal)
                        (ContextKey rightOrdinal)
                    )
              )
              leftKeys
              rightKeys
       in ResidentSupport
          { rsMinimalKeys =
              ResidentContextKeySet
                (minimalResidentKeySet lattice pairwiseJoins),
            rsClosure = Nothing
          }

withResidentSupport ::
  Ord c =>
  ContextLattice c ->
  SupportBasis c ->
  (forall s. ResidentContext s c -> ResidentSupport s c -> Either (ContextLatticeLookupError c) result) ->
  Either (ContextLatticeLookupError c) result
withResidentSupport contextLatticeValue supportValue continuation =
  withResidentContext contextLatticeValue $ \residentContext -> do
    residentSupport <-
      residentSupportFromElements
        residentContext
        (supportGenerators supportValue)
    continuation residentContext residentSupport

withResidentSupports ::
  Ord c =>
  ContextLattice c ->
  SupportBasis c ->
  SupportBasis c ->
  (forall s. ResidentContext s c -> ResidentSupport s c -> ResidentSupport s c -> Either (ContextLatticeLookupError c) result) ->
  Either (ContextLatticeLookupError c) result
withResidentSupports contextLatticeValue leftSupport rightSupport continuation =
  withResidentContext contextLatticeValue $ \residentContext -> do
    residentLeft <-
      residentSupportFromElements
        residentContext
        (supportGenerators leftSupport)
    residentRight <-
      residentSupportFromElements
        residentContext
        (supportGenerators rightSupport)
    continuation residentContext residentLeft residentRight

supportFromResidentSupport ::
  Ord c =>
  ResidentContext s c ->
  ResidentSupport s c ->
  SupportBasis c
supportFromResidentSupport residentContext supportValue =
  SupportBasis
    ( Set.fromList
        ( residentContextElementValue
            . residentContextElementForKey residentContext
            <$> residentSupportKeys residentContext supportValue
        )
    )

reachableCandidate ::
  Ord c =>
  ResidentContext s c ->
  ResidentSupport s c ->
  c ->
  Either (ContextLatticeLookupError c) (Maybe c)
reachableCandidate residentContext supportValue contextValue = do
  candidateElement <- checkResidentContext residentContext contextValue
  pure
    ( if residentSupportContainsElement residentContext supportValue candidateElement
        then Just contextValue
        else Nothing
    )

minimalResidentKeySet :: ContextLattice c -> ContextKeySet -> ContextKeySet
minimalResidentKeySet lattice candidates =
  contextKeySetFilter isMinimal candidates
  where
    isMinimal candidateOrdinal =
      not
        ( contextKeySetIntersectsExcept
            candidateOrdinal
            candidates
            (contextPlanLowerKeys (clPlan lattice) (ContextKey candidateOrdinal))
        )

residentSupportClosure :: ResidentContext s c -> ResidentSupport s c -> ResidentContextKeySet s
residentSupportClosure (ResidentContext lattice) supportValue =
  case rsMinimalKeys supportValue of
    ResidentContextKeySet minimalKeys ->
      ResidentContextKeySet
        ( contextKeySetUnionImages
            (contextKeySetChunkCount (clSize lattice))
            ( \generatorOrdinal ->
                contextPlanUpperKeys
                  (clPlan lattice)
                  (ContextKey generatorOrdinal)
            )
            minimalKeys
        )
