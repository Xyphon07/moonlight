-- | Invertible-morphism structure of a finite category: the core groupoid and
-- per-object automorphism groupoids, with their forgetful maps to the base category.
module Moonlight.Category.Pure.Invertibility
  ( InvertibilityIndex,
    CoreGroupoid,
    CoreGroupoidObject,
    CoreGroupoidMorphism,
    AutomorphismGroupoid,
    AutomorphismGroupoidObject,
    AutomorphismGroupoidMorphism,
    forgetCoreGroupoidObject,
    forgetCoreGroupoidMorphism,
    forgetAutomorphismGroupoidObject,
    forgetAutomorphismGroupoidMorphism,
    invertibilityIndex,
    coreGroupoid,
    coreGroupoidFromIndex,
    coreGroupoidObjects,
    coreGroupoidMorphisms,
    coreGroupoidMorphismsBetween,
    automorphismGroupoid,
    automorphismGroupoidFromIndex,
    automorphismGroupoidObjects,
    automorphismGroupAt,
  )
where

import Data.Function ((&))
import Data.Kind (Type)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Category.Pure.Category (Category (..), composeMor)
import Moonlight.Category.Pure.FinCat (FinCat)
import Moonlight.Category.Pure.FiniteComposable (FiniteComposableCategory (..))

type InvertibilityIndex :: Type -> Type
data InvertibilityIndex c = InvertibilityIndex
  { invertibilityByEndpoints :: Map (Ob c, Ob c) (Set (Mor c)),
    invertibilityAutomorphisms :: Map (Ob c) (Set (Mor c))
  }

type CoreGroupoid :: Type -> Type
data CoreGroupoid c = CoreGroupoid
  { coreGroupoidBaseCategory :: c,
    coreGroupoidObjectSet :: Set (Ob c),
    coreGroupoidHomSets :: Map (Ob c, Ob c) (Set (Mor c))
  }

type CoreGroupoidObject :: Type -> Type
newtype CoreGroupoidObject c = CoreGroupoidObject
  { forgetCoreGroupoidObject :: Ob c
  }

type CoreGroupoidMorphism :: Type -> Type
newtype CoreGroupoidMorphism c = CoreGroupoidMorphism
  { forgetCoreGroupoidMorphism :: Mor c
  }

type CoreGroupoidTwoMorphism :: Type -> Type
data CoreGroupoidTwoMorphism c

type CoreGroupoidCompositor :: Type -> Type
newtype CoreGroupoidCompositor c = CoreGroupoidCompositor (Compositor c)

type AutomorphismGroupoid :: Type -> Type
data AutomorphismGroupoid c = AutomorphismGroupoid
  { automorphismGroupoidBaseCategory :: c,
    automorphismGroupoidObjectSet :: Set (Ob c),
    automorphismGroupoidHomSets :: Map (Ob c) (Set (Mor c))
  }

type AutomorphismGroupoidObject :: Type -> Type
newtype AutomorphismGroupoidObject c = AutomorphismGroupoidObject
  { forgetAutomorphismGroupoidObject :: Ob c
  }

type AutomorphismGroupoidMorphism :: Type -> Type
newtype AutomorphismGroupoidMorphism c = AutomorphismGroupoidMorphism
  { forgetAutomorphismGroupoidMorphism :: Mor c
  }

type AutomorphismGroupoidTwoMorphism :: Type -> Type
data AutomorphismGroupoidTwoMorphism c

type AutomorphismGroupoidCompositor :: Type -> Type
newtype AutomorphismGroupoidCompositor c = AutomorphismGroupoidCompositor (Compositor c)

instance Eq (Ob c) => Eq (CoreGroupoidObject c) where
  CoreGroupoidObject left == CoreGroupoidObject right = left == right

instance Ord (Ob c) => Ord (CoreGroupoidObject c) where
  compare (CoreGroupoidObject left) (CoreGroupoidObject right) = compare left right

instance Show (Ob c) => Show (CoreGroupoidObject c) where
  show (CoreGroupoidObject objectValue) = show objectValue

instance Eq (Mor c) => Eq (CoreGroupoidMorphism c) where
  CoreGroupoidMorphism left == CoreGroupoidMorphism right = left == right

instance Ord (Mor c) => Ord (CoreGroupoidMorphism c) where
  compare (CoreGroupoidMorphism left) (CoreGroupoidMorphism right) = compare left right

instance Show (Mor c) => Show (CoreGroupoidMorphism c) where
  show (CoreGroupoidMorphism morphismValue) = show morphismValue

instance Eq (Ob c) => Eq (AutomorphismGroupoidObject c) where
  AutomorphismGroupoidObject left == AutomorphismGroupoidObject right = left == right

instance Ord (Ob c) => Ord (AutomorphismGroupoidObject c) where
  compare (AutomorphismGroupoidObject left) (AutomorphismGroupoidObject right) = compare left right

instance Show (Ob c) => Show (AutomorphismGroupoidObject c) where
  show (AutomorphismGroupoidObject objectValue) = show objectValue

instance Eq (Mor c) => Eq (AutomorphismGroupoidMorphism c) where
  AutomorphismGroupoidMorphism left == AutomorphismGroupoidMorphism right = left == right

instance Ord (Mor c) => Ord (AutomorphismGroupoidMorphism c) where
  compare (AutomorphismGroupoidMorphism left) (AutomorphismGroupoidMorphism right) = compare left right

instance Show (Mor c) => Show (AutomorphismGroupoidMorphism c) where
  show (AutomorphismGroupoidMorphism morphismValue) = show morphismValue

instance Category c => Category (CoreGroupoid c) where
  type Ob (CoreGroupoid c) = CoreGroupoidObject c
  type Mor (CoreGroupoid c) = CoreGroupoidMorphism c
  type TwoMor (CoreGroupoid c) = CoreGroupoidTwoMorphism c
  type Compositor (CoreGroupoid c) = CoreGroupoidCompositor c
  type CategoryError (CoreGroupoid c) = CategoryError c

  identity coreGroupoidValue (CoreGroupoidObject objectValue) =
    CoreGroupoidMorphism <$> identity (coreGroupoidBaseCategory coreGroupoidValue) objectValue

  compose coreGroupoidValue (CoreGroupoidMorphism left) (CoreGroupoidMorphism right) =
    compose (coreGroupoidBaseCategory coreGroupoidValue) left right
      & fmap (\(morphismValue, compositorValue) -> (CoreGroupoidMorphism morphismValue, CoreGroupoidCompositor compositorValue))

  source coreGroupoidValue (CoreGroupoidMorphism morphismValue) =
    CoreGroupoidObject <$> source (coreGroupoidBaseCategory coreGroupoidValue) morphismValue

  target coreGroupoidValue (CoreGroupoidMorphism morphismValue) =
    CoreGroupoidObject <$> target (coreGroupoidBaseCategory coreGroupoidValue) morphismValue

instance (Category c, Eq (Ob c), Ord (Ob c), Ord (Mor c)) => FiniteComposableCategory (CoreGroupoid c) where
  enumerateObjects =
    coreGroupoidObjects

  enumerateMorphisms =
    coreGroupoidMorphisms

  enumerateMorphismsFrom coreGroupoidValue sourceObject =
    coreGroupoidValue
      & coreGroupoidObjectSet
      & Set.toAscList
      & foldMap (coreGroupoidMorphismsBetween coreGroupoidValue sourceObject . CoreGroupoidObject)

instance Category c => Category (AutomorphismGroupoid c) where
  type Ob (AutomorphismGroupoid c) = AutomorphismGroupoidObject c
  type Mor (AutomorphismGroupoid c) = AutomorphismGroupoidMorphism c
  type TwoMor (AutomorphismGroupoid c) = AutomorphismGroupoidTwoMorphism c
  type Compositor (AutomorphismGroupoid c) = AutomorphismGroupoidCompositor c
  type CategoryError (AutomorphismGroupoid c) = CategoryError c

  identity automorphismGroupoidValue (AutomorphismGroupoidObject objectValue) =
    AutomorphismGroupoidMorphism <$> identity (automorphismGroupoidBaseCategory automorphismGroupoidValue) objectValue

  compose automorphismGroupoidValue (AutomorphismGroupoidMorphism left) (AutomorphismGroupoidMorphism right) =
    compose (automorphismGroupoidBaseCategory automorphismGroupoidValue) left right
      & fmap (\(morphismValue, compositorValue) -> (AutomorphismGroupoidMorphism morphismValue, AutomorphismGroupoidCompositor compositorValue))

  source automorphismGroupoidValue (AutomorphismGroupoidMorphism morphismValue) =
    AutomorphismGroupoidObject <$> source (automorphismGroupoidBaseCategory automorphismGroupoidValue) morphismValue

  target automorphismGroupoidValue (AutomorphismGroupoidMorphism morphismValue) =
    AutomorphismGroupoidObject <$> target (automorphismGroupoidBaseCategory automorphismGroupoidValue) morphismValue

instance (Category c, Eq (Ob c), Ord (Ob c), Ord (Mor c)) => FiniteComposableCategory (AutomorphismGroupoid c) where
  enumerateObjects =
    automorphismGroupoidObjects

  enumerateMorphisms automorphismGroupoidValue =
    automorphismGroupoidValue
      & automorphismGroupoidObjectSet
      & Set.toAscList
      & foldMap (automorphismGroupAt automorphismGroupoidValue . AutomorphismGroupoidObject)

  enumerateMorphismsFrom =
    automorphismGroupAt

isInversePairWithIdentities :: (Category c, Eq (Mor c)) => c -> Mor c -> Mor c -> Mor c -> Mor c -> Bool
isInversePairWithIdentities categoryValue targetIdentity sourceIdentity left right =
  case
    ( composeMor categoryValue left right,
      composeMor categoryValue right left
    )
    of
      (Right leftThenRight, Right rightThenLeft) ->
        leftThenRight == targetIdentity && rightThenLeft == sourceIdentity
      _ ->
        False
{-# INLINE isInversePairWithIdentities #-}

endpointMorphismIndex ::
  (FiniteComposableCategory c, Ord (Ob c), Ord (Mor c)) =>
  c ->
  Map (Ob c, Ob c) (Set (Mor c))
endpointMorphismIndex categoryValue =
  enumerateMorphisms categoryValue
    & List.foldl' insertMorphism Map.empty
  where
    insertMorphism accumulated morphism =
      case (source categoryValue morphism, target categoryValue morphism) of
        (Right sourceObject, Right targetObject) ->
          Map.insertWith Set.union (sourceObject, targetObject) (Set.singleton morphism) accumulated
        _ ->
          accumulated

invertibleBucket ::
  (FiniteComposableCategory c, Ord (Ob c), Ord (Mor c)) =>
  c ->
  Map (Ob c, Ob c) (Set (Mor c)) ->
  (Ob c, Ob c) ->
  Set (Mor c) ->
  Set (Mor c)
invertibleBucket categoryValue endpointIndex (sourceObject, targetObject) morphismsInBucket =
  let inverseCandidates =
        Map.findWithDefault Set.empty (targetObject, sourceObject) endpointIndex
   in case (identity categoryValue targetObject, identity categoryValue sourceObject) of
        (Right targetIdentity, Right sourceIdentity) ->
          morphismsInBucket
            & Set.filter
              ( \morphism ->
                  inverseCandidates
                    & any (isInversePairWithIdentities categoryValue targetIdentity sourceIdentity morphism)
              )
        _ ->
          Set.empty

invertibleEndpointIndex ::
  (FiniteComposableCategory c, Ord (Ob c), Ord (Mor c)) =>
  c ->
  Map (Ob c, Ob c) (Set (Mor c)) ->
  Map (Ob c, Ob c) (Set (Mor c))
invertibleEndpointIndex categoryValue endpointIndex =
  endpointIndex
    & Map.foldlWithKey'
      ( \accumulated endpoints morphismsInBucket ->
          let invertibles = invertibleBucket categoryValue endpointIndex endpoints morphismsInBucket
           in if Set.null invertibles
                then accumulated
                else Map.insert endpoints invertibles accumulated
      )
      Map.empty

automorphismIndex ::
  (Ord (Ob c), Ord (Mor c)) =>
  Map (Ob c, Ob c) (Set (Mor c)) ->
  Map (Ob c) (Set (Mor c))
automorphismIndex invertibleIndex =
  invertibleIndex
    & Map.foldlWithKey'
      ( \accumulated (sourceObject, targetObject) invertibles ->
          if sourceObject == targetObject
            then Map.insertWith Set.union sourceObject invertibles accumulated
            else accumulated
      )
      Map.empty

objectsFromIndex :: Ord (Ob c) => InvertibilityIndex c -> Set (Ob c)
objectsFromIndex invertibilityIndexValue =
  let endpointObjects =
        invertibilityByEndpoints invertibilityIndexValue
          & Map.foldlWithKey'
            (\accumulated (sourceObject, targetObject) _ -> Set.insert sourceObject (Set.insert targetObject accumulated))
            Set.empty
   in Set.union endpointObjects (Map.keysSet (invertibilityAutomorphisms invertibilityIndexValue))

invertibilityIndex ::
  (FiniteComposableCategory c, Ord (Ob c), Ord (Mor c)) =>
  c ->
  InvertibilityIndex c
invertibilityIndex categoryValue =
  let endpointIndex = endpointMorphismIndex categoryValue
      invertibleIndex = invertibleEndpointIndex categoryValue endpointIndex
   in InvertibilityIndex
        { invertibilityByEndpoints = invertibleIndex,
          invertibilityAutomorphisms = automorphismIndex invertibleIndex
        }
{-# SPECIALIZE invertibilityIndex :: FinCat -> InvertibilityIndex FinCat #-}

coreGroupoidFromIndex :: Ord (Ob c) => c -> InvertibilityIndex c -> CoreGroupoid c
coreGroupoidFromIndex categoryValue invertibilityIndexValue =
  CoreGroupoid
    { coreGroupoidBaseCategory = categoryValue,
      coreGroupoidObjectSet = objectsFromIndex invertibilityIndexValue,
      coreGroupoidHomSets = invertibilityByEndpoints invertibilityIndexValue
    }

coreGroupoid ::
  (FiniteComposableCategory c, Ord (Ob c), Ord (Mor c)) =>
  c ->
  CoreGroupoid c
coreGroupoid categoryValue =
  coreGroupoidFromIndex categoryValue (invertibilityIndex categoryValue)
{-# SPECIALIZE coreGroupoid :: FinCat -> CoreGroupoid FinCat #-}

coreGroupoidObjects :: CoreGroupoid c -> [CoreGroupoidObject c]
coreGroupoidObjects coreGroupoidValue =
  coreGroupoidObjectSet coreGroupoidValue
    & Set.toAscList
    & fmap CoreGroupoidObject

coreGroupoidMorphisms :: CoreGroupoid c -> [CoreGroupoidMorphism c]
coreGroupoidMorphisms coreGroupoidValue =
  coreGroupoidHomSets coreGroupoidValue
    & Map.elems
    & foldMap Set.toAscList
    & fmap CoreGroupoidMorphism

coreGroupoidMorphismsBetween ::
  Ord (Ob c) =>
  CoreGroupoid c ->
  CoreGroupoidObject c ->
  CoreGroupoidObject c ->
  [CoreGroupoidMorphism c]
coreGroupoidMorphismsBetween coreGroupoidValue (CoreGroupoidObject sourceObject) (CoreGroupoidObject targetObject) =
  Map.findWithDefault Set.empty (sourceObject, targetObject) (coreGroupoidHomSets coreGroupoidValue)
    & Set.toAscList
    & fmap CoreGroupoidMorphism

automorphismGroupoidFromIndex :: Ord (Ob c) => c -> InvertibilityIndex c -> AutomorphismGroupoid c
automorphismGroupoidFromIndex categoryValue invertibilityIndexValue =
  AutomorphismGroupoid
    { automorphismGroupoidBaseCategory = categoryValue,
      automorphismGroupoidObjectSet = objectsFromIndex invertibilityIndexValue,
      automorphismGroupoidHomSets = invertibilityAutomorphisms invertibilityIndexValue
    }

automorphismGroupoid ::
  (FiniteComposableCategory c, Ord (Ob c), Ord (Mor c)) =>
  c ->
  AutomorphismGroupoid c
automorphismGroupoid categoryValue =
  automorphismGroupoidFromIndex categoryValue (invertibilityIndex categoryValue)
{-# SPECIALIZE automorphismGroupoid :: FinCat -> AutomorphismGroupoid FinCat #-}

automorphismGroupoidObjects :: AutomorphismGroupoid c -> [AutomorphismGroupoidObject c]
automorphismGroupoidObjects automorphismGroupoidValue =
  automorphismGroupoidObjectSet automorphismGroupoidValue
    & Set.toAscList
    & fmap AutomorphismGroupoidObject

automorphismGroupAt ::
  Ord (Ob c) =>
  AutomorphismGroupoid c ->
  AutomorphismGroupoidObject c ->
  [AutomorphismGroupoidMorphism c]
automorphismGroupAt automorphismGroupoidValue (AutomorphismGroupoidObject baseObject) =
  Map.findWithDefault Set.empty baseObject (automorphismGroupoidHomSets automorphismGroupoidValue)
    & Set.toAscList
    & fmap AutomorphismGroupoidMorphism
