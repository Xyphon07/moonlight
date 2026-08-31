{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE RoleAnnotations #-}

-- | Heyting implication over compiled finite context lattices: the relative
-- pseudocomplement as a precompiled query, with resident-key variants.
module Moonlight.FiniteLattice.Heyting
  ( ContextHeyting,
    contextHeytingLattice,
    ContextHeytingCompileError (..),
    compileContextHeyting,
    impliesContext,
    ResidentHeytingContext,
    residentHeytingBaseContext,
    withResidentHeytingContext,
    residentImpliesKey,
    residentImplies,
  )
where

import Data.Bits
  ( (.&.),
    (.|.),
    complement,
  )
import Data.Foldable (asum)
import Data.Kind (Type)
import Moonlight.FiniteLattice.Internal.Distributive
  ( ContextDistributivePlan,
    distributivePlanFromDenseComponents,
    distributiveResidualKey,
  )
import Moonlight.FiniteLattice.Internal.Key
  ( ContextKey (..),
    ContextKeySet,
    contextKeySetAll,
    contextKeySetChunkCount,
    contextKeySetDifference,
    contextKeySetFind,
    contextKeySetFoldr,
    contextKeySetUnionImages,
  )
import Moonlight.FiniteLattice.Internal.Plan
  ( ContextBooleanPlan (..),
    ContextBoundedFanPlan (..),
    ContextDenseTablePlan (..),
    ContextMaskPlan (..),
    ContextPlan (..),
    ContextTotalOrderPlan (..),
    booleanKeyForMask,
    booleanMaskForKey,
    contextPlanJoinKey,
    contextPlanLeq,
    contextPlanLowerKeys,
    contextPlanMeetKey,
    contextPlanUpperKeys,
    totalOrderKeyRank,
  )
import Moonlight.FiniteLattice.Internal.Types
  ( ContextLattice (..),
    ContextLatticeLookupError (..),
    ResidentContext (..),
    ResidentContextElement (..),
    ResidentContextKey,
    contextKeyForMaybe,
    contextKeyFromResidentKey,
    contextValueForKey,
    residentContextElementForKey,
    residentKeyFromContextKey,
  )

type ContextHeyting :: Type -> Type
data ContextHeyting c = ContextHeyting !(ContextLattice c) !ContextResidualPlan

type role ContextHeyting nominal

contextHeytingLattice :: ContextHeyting c -> ContextLattice c
contextHeytingLattice (ContextHeyting lattice _) =
  lattice
{-# INLINE contextHeytingLattice #-}

type ContextResidualPlan :: Type
data ContextResidualPlan
  = LazyDenseResidualPlan !ContextKeySet !Int
  | BirkhoffResidualPlan !ContextDistributivePlan
  | OrdinalTotalOrderResidualPlan !Int
  | TotalOrderResidualPlan !ContextTotalOrderPlan
  | BooleanResidualPlan !ContextBooleanPlan

type ContextHeytingCompileError :: Type -> Type
data ContextHeytingCompileError c
  -- | Antecedent, consequent, and the join of every candidate
  -- @x@ satisfying @antecedent ∧ x <= consequent@. The join is not itself a
  -- candidate, which proves that no greatest candidate exists.
  = ContextResidualDoesNotExist !c !c !c
  deriving stock (Eq, Ord, Show, Read)

type role ContextHeytingCompileError nominal

compileContextHeyting ::
  ContextLattice c ->
  Either (ContextHeytingCompileError c) (ContextHeyting c)
compileContextHeyting lattice =
  ContextHeyting lattice <$> compileResidualPlan lattice

compileResidualPlan ::
  ContextLattice c ->
  Either (ContextHeytingCompileError c) ContextResidualPlan
compileResidualPlan lattice =
  case clPlan lattice of
    OrdinalTotalOrderPlan size ->
      Right (OrdinalTotalOrderResidualPlan size)
    TotalOrderPlan plan ->
      Right (TotalOrderResidualPlan plan)
    MaskPlan (BooleanPlan plan) ->
      Right (BooleanResidualPlan plan)
    MaskPlan (DistributivePlan distributivePlan) ->
      Right (BirkhoffResidualPlan distributivePlan)
    MaskPlan (DenseRowsPlan _) ->
      validateDenseResidual lattice
    OrdinalBoundedFanPlan size ->
      rejectNonHeytingOrdinalFan lattice size
    BoundedFanPlan plan ->
      rejectNonHeytingFan lattice plan
    DensePlan tablePlan ->
      maybe
        (validateDenseResidual lattice)
        (Right . BirkhoffResidualPlan)
        ( distributivePlanFromDenseComponents
            (cdtpSize tablePlan)
            (clTopKey lattice)
            (clBottomKey lattice)
            (cdtpUpperRows tablePlan)
            (cdtpLowerRows tablePlan)
            (cdtpJoinTable tablePlan)
            (cdtpMeetTable tablePlan)
        )

-- In a fan with at least three atoms, choose an atom a and b = bottom. Every
-- other atom is a candidate for a => b; the join of two such atoms is top, but
-- a ∧ top = a is not <= bottom. Therefore the residual does not exist.
rejectNonHeytingFan ::
  ContextLattice c ->
  ContextBoundedFanPlan ->
  Either (ContextHeytingCompileError c) ContextResidualPlan
rejectNonHeytingFan lattice plan =
  case contextKeySetFind (const True) (cbfAtomKeys plan) of
    Nothing -> validateDenseResidual lattice
    Just atomOrdinal ->
      Left
        ( ContextResidualDoesNotExist
            (contextValueForKey lattice (ContextKey atomOrdinal))
            (contextValueForKey lattice (cbfBottomKey plan))
            (contextValueForKey lattice (cbfTopKey plan))
        )

rejectNonHeytingOrdinalFan ::
  ContextLattice c ->
  Int ->
  Either (ContextHeytingCompileError c) ContextResidualPlan
rejectNonHeytingOrdinalFan lattice size =
  Left
    ( ContextResidualDoesNotExist
        (contextValueForKey lattice (ContextKey 1))
        (contextValueForKey lattice (ContextKey 0))
        (contextValueForKey lattice (ContextKey (size - 1)))
    )

validateDenseResidual ::
  ContextLattice c ->
  Either (ContextHeytingCompileError c) ContextResidualPlan
validateDenseResidual lattice =
  case asum (residualObstruction <$> residualPairs) of
    Just obstruction -> Left obstruction
    Nothing -> Right (LazyDenseResidualPlan allKeys chunkCount)
  where
    size = clSize lattice
    chunkCount = contextKeySetChunkCount size
    allKeys = contextKeySetAll size
    plan = clPlan lattice
    residualPairs =
      [ (ContextKey antecedentOrdinal, ContextKey consequentOrdinal)
      | antecedentOrdinal <- [0 .. size - 1],
        consequentOrdinal <- [0 .. size - 1]
      ]

    residualObstruction (antecedentKey, consequentKey) =
      let candidateJoin =
            residualCandidateJoin allKeys chunkCount lattice antecedentKey consequentKey
          candidateMeet = contextPlanMeetKey plan antecedentKey candidateJoin
       in if contextPlanLeq plan candidateMeet consequentKey
            then Nothing
            else
              Just
                ( ContextResidualDoesNotExist
                    (contextValueForKey lattice antecedentKey)
                    (contextValueForKey lattice consequentKey)
                    (contextValueForKey lattice candidateJoin)
                )

-- C = {x | a ∧ x <= b}, r = join C. Bottom ∈ C, so C is nonempty; the residual
-- exists iff r ∈ C, where it is then the greatest member. Candidates avoid
-- recomputing meets: a ∧ x <= b iff lower(a) ∩ lower(x) ⊆ lower(b) iff x is
-- above no y ∈ lower(a) \\ lower(b). So C is all keys minus the upward closure
-- of those forbidden lower keys.
residualCandidateJoin ::
  ContextKeySet ->
  Int ->
  ContextLattice c ->
  ContextKey ->
  ContextKey ->
  ContextKey
residualCandidateJoin allKeys chunkCount lattice antecedentKey consequentKey =
  contextKeySetFoldr joinCandidate (clBottomKey lattice) candidateKeys
  where
    plan = clPlan lattice
    candidateKeys =
      residualCandidateKeys allKeys chunkCount lattice antecedentKey consequentKey

    joinCandidate !candidateOrdinal !candidateJoin =
      contextPlanJoinKey plan candidateJoin (ContextKey candidateOrdinal)

residualCandidateKeys ::
  ContextKeySet ->
  Int ->
  ContextLattice c ->
  ContextKey ->
  ContextKey ->
  ContextKeySet
residualCandidateKeys allKeys chunkCount lattice antecedentKey consequentKey =
  contextKeySetDifference allKeys rejectedKeys
  where
    plan = clPlan lattice
    forbiddenLowerKeys =
      contextKeySetDifference
        (contextPlanLowerKeys plan antecedentKey)
        (contextPlanLowerKeys plan consequentKey)
    rejectedKeys =
      contextKeySetUnionImages
        chunkCount
        (contextPlanUpperKeys plan . ContextKey)
        forbiddenLowerKeys

impliesContext ::
  Ord c =>
  ContextHeyting c ->
  c ->
  c ->
  Either (ContextLatticeLookupError c) c
impliesContext (ContextHeyting lattice residualPlan) antecedent consequent = do
  antecedentKey <- lookupContextKey lattice antecedent
  consequentKey <- lookupContextKey lattice consequent
  pure
    ( contextValueForKey
        lattice
        (residualKey lattice residualPlan antecedentKey consequentKey)
    )

lookupContextKey ::
  Ord c =>
  ContextLattice c ->
  c ->
  Either (ContextLatticeLookupError c) ContextKey
lookupContextKey lattice contextValue =
  maybe
    (Left (ContextLatticeUnknownContext contextValue))
    Right
    (contextKeyForMaybe lattice contextValue)

type ResidentHeytingContext :: Type -> Type -> Type
data ResidentHeytingContext s c = ResidentHeytingContext
  { residentHeytingBaseContext :: !(ResidentContext s c),
    residentHeytingResidualPlan :: !ContextResidualPlan
  }

type role ResidentHeytingContext nominal nominal

withResidentHeytingContext ::
  ContextHeyting c ->
  (forall s. ResidentHeytingContext s c -> result) ->
  result
withResidentHeytingContext (ContextHeyting lattice residualPlan) continuation =
  continuation
    ResidentHeytingContext
      { residentHeytingBaseContext = ResidentContext lattice,
        residentHeytingResidualPlan = residualPlan
      }

residentImpliesKey ::
  ResidentHeytingContext s c ->
  ResidentContextKey s ->
  ResidentContextKey s ->
  ResidentContextKey s
residentImpliesKey context antecedentKey consequentKey =
  case residentHeytingBaseContext context of
    ResidentContext lattice ->
      residentKeyFromContextKey
        ( residualKey
            lattice
            (residentHeytingResidualPlan context)
            (contextKeyFromResidentKey antecedentKey)
            (contextKeyFromResidentKey consequentKey)
        )
{-# INLINE residentImpliesKey #-}

residentImplies ::
  ResidentHeytingContext s c ->
  ResidentContextElement s c ->
  ResidentContextElement s c ->
  ResidentContextElement s c
residentImplies context antecedent consequent =
  residentContextElementForKey
    (residentHeytingBaseContext context)
    ( residentImpliesKey
        context
        (residentContextElementKey antecedent)
        (residentContextElementKey consequent)
    )

residualKey ::
  ContextLattice c ->
  ContextResidualPlan ->
  ContextKey ->
  ContextKey ->
  ContextKey
residualKey lattice residualPlan antecedentKey consequentKey =
  case residualPlan of
    LazyDenseResidualPlan allKeys chunkCount ->
      residualCandidateJoin allKeys chunkCount lattice antecedentKey consequentKey
    BirkhoffResidualPlan plan ->
      distributiveResidualKey plan antecedentKey consequentKey
    OrdinalTotalOrderResidualPlan size ->
      if contextKeyOrdinal antecedentKey <= contextKeyOrdinal consequentKey
            then ContextKey (size - 1)
            else consequentKey
    TotalOrderResidualPlan plan ->
      if
            totalOrderKeyRank plan antecedentKey
              <= totalOrderKeyRank plan consequentKey
            then ctoTopKey plan
            else consequentKey
    BooleanResidualPlan plan ->
      booleanKeyForMask
            plan
            ( (complement (booleanMaskForKey plan antecedentKey) .&. cboFullMask plan)
                .|. booleanMaskForKey plan consequentKey
            )
{-# INLINE residualKey #-}
