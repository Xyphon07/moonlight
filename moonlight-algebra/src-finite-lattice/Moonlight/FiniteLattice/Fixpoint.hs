{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE RoleAnnotations #-}

-- | Compiled monotone endomaps over a 'ContextLattice' and their least and
-- greatest fixed points.
module Moonlight.FiniteLattice.Fixpoint
  ( ResidentMonotoneMap,
    ContextMonotoneMapError (..),
    compileResidentMonotoneMap,
    compileResidentMonotoneKeyMap,
    leastResidentFixpoint,
    greatestResidentFixpoint,
    leastContextFixpoint,
    greatestContextFixpoint,
  )
where

import Data.Kind (Type)
import Data.Vector.Unboxed qualified as UVector
import Moonlight.FiniteLattice.Internal.Invariant
  ( unboxedIndexInvariant,
  )
import Moonlight.FiniteLattice.Internal.Key
  ( ContextKey (..),
    contextKeySetFind,
  )
import Moonlight.FiniteLattice.Internal.Plan
  ( contextPlanLeq,
    contextPlanMonotonicityTargets,
  )
import Moonlight.FiniteLattice.Internal.Types
  ( ContextLattice (..),
    ResidentContext (..),
    ResidentContextElement,
    ResidentContextKey (..),
    contextKeyForMaybe,
    contextValueForKey,
    residentContextElementForKey,
    residentContextElementValue,
    residentKeyFromContextKey,
  )
import Moonlight.FiniteLattice.Resident
  ( withResidentContext,
  )

type ResidentMonotoneMap :: Type -> Type
newtype ResidentMonotoneMap s = ResidentMonotoneMap
  { residentMonotoneMapImage :: UVector.Vector Int
  }

type role ResidentMonotoneMap nominal

type ContextMonotoneMapError :: Type -> Type
data ContextMonotoneMapError c
  = ContextEndomapOutsideUniverse !c !c
  | ContextEndomapNotMonotone !c !c !c !c
  deriving stock (Eq, Ord, Show, Read)

type role ContextMonotoneMapError nominal

compileResidentMonotoneMap ::
  Ord c =>
  ResidentContext s c ->
  (c -> c) ->
  Either (ContextMonotoneMapError c) (ResidentMonotoneMap s)
compileResidentMonotoneMap (ResidentContext lattice) step = do
  image <-
    UVector.generateM
      (clSize lattice)
      (compileImageAt lattice step)
  validateMonotonicity lattice image
  pure (ResidentMonotoneMap image)

-- | Like 'compileResidentMonotoneMap', but closure is structural — branded keys
-- cannot leave the context — so only monotonicity is checked.
compileResidentMonotoneKeyMap ::
  ResidentContext s c ->
  (ResidentContextKey s -> ResidentContextKey s) ->
  Either (ContextMonotoneMapError c) (ResidentMonotoneMap s)
compileResidentMonotoneKeyMap (ResidentContext lattice) step =
  let image =
        UVector.generate
          (clSize lattice)
          ( \keyOrdinal ->
              residentContextKeyOrdinal
                (step (residentKeyFromContextKey (ContextKey keyOrdinal)))
          )
   in validateMonotonicity lattice image *> pure (ResidentMonotoneMap image)

compileImageAt ::
  Ord c =>
  ContextLattice c ->
  (c -> c) ->
  Int ->
  Either (ContextMonotoneMapError c) Int
compileImageAt lattice step keyOrdinal =
  let input = contextValueForKey lattice (ContextKey keyOrdinal)
      output = step input
   in case contextKeyForMaybe lattice output of
        Nothing ->
          Left (ContextEndomapOutsideUniverse input output)
        Just outputKey -> Right (contextKeyOrdinal outputKey)

validateMonotonicity ::
  ContextLattice c ->
  UVector.Vector Int ->
  Either (ContextMonotoneMapError c) ()
validateMonotonicity lattice image =
  checkLower 0
  where
    checkLower !lowerOrdinal
      | lowerOrdinal >= clSize lattice = Right ()
      | otherwise = do
          let targets =
                contextPlanMonotonicityTargets (clPlan lattice) (ContextKey lowerOrdinal)
          case contextKeySetFind (violates lowerOrdinal) targets of
            Nothing -> checkLower (lowerOrdinal + 1)
            Just upperOrdinal ->
              let lowerKey = ContextKey lowerOrdinal
                  upperKey = ContextKey upperOrdinal
                  lowerImageKey = imageKey image lowerKey
                  upperImageKey = imageKey image upperKey
               in Left
                    ( ContextEndomapNotMonotone
                        (contextValueForKey lattice lowerKey)
                        (contextValueForKey lattice upperKey)
                        (contextValueForKey lattice lowerImageKey)
                        (contextValueForKey lattice upperImageKey)
                    )

    violates lowerOrdinal upperOrdinal =
      not
        ( contextPlanLeq
            (clPlan lattice)
            (imageKey image (ContextKey lowerOrdinal))
            (imageKey image (ContextKey upperOrdinal))
        )

-- | Least fixed point of a compiled monotone endomap.
--
-- The orbit from bottom is ascending (bottom <= x1; and @xn <= x(n+1)@ gives
-- @f xn <= f x(n+1)@ by monotonicity), so finiteness forces stabilization;
-- induction gives @xn <= p@ for every fixed point @p@, so it is the least.
leastResidentFixpoint ::
  ResidentContext s c ->
  ResidentMonotoneMap s ->
  ResidentContextElement s c
leastResidentFixpoint context@(ResidentContext lattice) monotoneMap =
  residentContextElementForKey
    context
    (residentKeyFromContextKey (iterateToFixpoint monotoneMap (clBottomKey lattice)))

-- | Greatest fixed point; the order-dual proof starts from top.
greatestResidentFixpoint ::
  ResidentContext s c ->
  ResidentMonotoneMap s ->
  ResidentContextElement s c
greatestResidentFixpoint context@(ResidentContext lattice) monotoneMap =
  residentContextElementForKey
    context
    (residentKeyFromContextKey (iterateToFixpoint monotoneMap (clTopKey lattice)))

leastContextFixpoint ::
  Ord c =>
  ContextLattice c ->
  (c -> c) ->
  Either (ContextMonotoneMapError c) c
leastContextFixpoint lattice step =
  withResidentContext lattice $ \context -> do
    monotoneMap <- compileResidentMonotoneMap context step
    pure
      (residentContextElementValue (leastResidentFixpoint context monotoneMap))

greatestContextFixpoint ::
  Ord c =>
  ContextLattice c ->
  (c -> c) ->
  Either (ContextMonotoneMapError c) c
greatestContextFixpoint lattice step =
  withResidentContext lattice $ \context -> do
    monotoneMap <- compileResidentMonotoneMap context step
    pure
      (residentContextElementValue (greatestResidentFixpoint context monotoneMap))

iterateToFixpoint :: ResidentMonotoneMap s -> ContextKey -> ContextKey
iterateToFixpoint monotoneMap =
  go
  where
    go !currentKey =
      let nextKey = imageKey (residentMonotoneMapImage monotoneMap) currentKey
       in if nextKey == currentKey
            then currentKey
            else go nextKey

imageKey :: UVector.Vector Int -> ContextKey -> ContextKey
imageKey image (ContextKey keyOrdinal) =
  ContextKey (unboxedIndexInvariant image keyOrdinal)
{-# INLINE imageKey #-}
