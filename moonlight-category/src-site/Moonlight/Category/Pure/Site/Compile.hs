{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RoleAnnotations #-}

-- | Compilation of a thin site presentation to a runtime-validated finite category,
-- with kernel-relative object and morphism lookup.
module Moonlight.Category.Pure.Site.Compile
  ( ThinSiteValidation (..),
    ThinSiteKernel,
    thinSiteKernelManifest,
    thinSiteKernelCodomain,
    ThinSiteLookupError (..),
    ThinSiteObjectValueError (..),
    ThinSitePresentation (..),
    thinSitePresentation,
    thinPresentationToFinCat,
    thinSiteImportKernel,
    thinSiteKernel,
    thinSiteFinObject,
    thinSiteObjectValue,
    thinSiteFinMorphism,
    thinSiteFinMorphismByEndpoints,
  )
where

import Data.Kind (Type)
import Data.Bifunctor (first)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import Moonlight.Category.Pure.Category (Category (identity))
import Moonlight.Category.Pure.FinCat
  ( FinCat,
    FinCatHandle,
    FinCatError,
    FinCatValidationError,
    FinMor,
    FinMorphismId (..),
    FinObjectId (..),
    FinObj,
    mkFinCat,
    mkFinMorphism,
    mkFinObject,
    denseThinEndpointMorphismsFromCategory,
    finCatExplicitCompositionMapView,
    finCatHandle,
    finCatMorphismIdByEndpoints,
    finObjCategoryHandle,
    finObjId,
    trustedDenseThinFinCatFromReachabilityRows,
  )
import Moonlight.Category.Pure.Site.Core (SiteFinCatError (..), SiteManifest, SiteViolation)
import Moonlight.Category.Pure.Site.Manifest
  ( ValidatedSiteManifest,
    validateSiteImportManifest,
    validateSiteManifestDetailed,
    validatedSiteObjectVector,
    validatedSiteReachabilityRows,
  )

type ThinSitePresentation :: Type -> Type
data ThinSitePresentation obj = ThinSitePresentation
  { thinPresentationObjectIds :: Map obj FinObjectId,
    thinPresentationPairIds :: Map (obj, obj) FinMorphismId,
    thinPresentationObjects :: Set FinObjectId,
    thinPresentationMorphisms :: Map (FinObjectId, FinObjectId) [FinMorphismId],
    thinPresentationComposition :: Map (FinMorphismId, FinMorphismId) FinMorphismId
  }

-- | The validation obligation discharged before compiling a 'ThinSiteKernel'.
--
-- An import kernel validates only the import graph. A site kernel additionally
-- proves the cover axioms required by path and quotient construction.
type ThinSiteValidation :: Type
data ThinSiteValidation
  = ImportsValidated
  | SiteValidated
  deriving stock (Eq, Show)

-- | A finite import category together with the exact manifest-local
-- correspondence between semantic objects and its opaque finite objects.
--
-- 'FinObjectId' values produced here are representation tokens relative to this
-- kernel, not semantic object names or a stable ordering contract. Use
-- 'thinSiteFinObject' and 'thinSiteObjectValue' rather than reconstructing the
-- ascending-set enumeration.
--
-- The validation index is nominal: an import-only kernel cannot be coerced into
-- the full-site evidence required by path and quotient construction.
type ThinSiteKernel :: ThinSiteValidation -> Type -> Type
type role ThinSiteKernel nominal nominal
data ThinSiteKernel validation obj = ThinSiteKernel
  { thinSiteKernelManifest :: SiteManifest obj,
    thinSiteKernelCodomain :: FinCat,
    thinSiteKernelObjectIds :: Map obj FinObjectId,
    thinSiteKernelObjectValues :: Vector.Vector obj
  }
  deriving stock (Eq, Show)

type ThinSiteLookupError :: Type -> Type
data ThinSiteLookupError obj
  = ThinSiteUnknownObject obj
  | ThinSiteCodomainObjectMissing FinObjectId
  | ThinSiteUnknownMorphismPair obj obj
  | ThinSiteCodomainMorphismMissing FinMorphismId
  | ThinSiteCodomainMorphismInvalid FinCatError
  deriving stock (Eq, Show)

-- | Obstructions specific to inverting a finite object through a site kernel.
--
-- These are intentionally distinct from 'ThinSiteLookupError': callers that
-- only construct finite objects or morphisms do not acquire impossible inverse
-- lookup cases in their error algebra.
type ThinSiteObjectValueError :: Type
data ThinSiteObjectValueError
  = ThinSiteForeignCodomainObject FinCatHandle FinCatHandle FinObjectId
  | ThinSiteUnmappedCodomainObject FinObjectId
  deriving stock (Eq, Show)

-- | Builds the explicit presentation, including the materialized composition
-- table via 'finCatExplicitCompositionMapView' — an output-bound @Θ(n³)@ witness
-- for a linear site on @n@ objects, dominating the @Θ(n²/w)@ dense validation
-- that precedes it. The record fields are lazy, so the cubic table is only paid
-- when 'thinPresentationComposition' is forced. Callers that need composition
-- queries rather than the explicit witness should use a 'ThinSiteKernel', which
-- stays on the dense handle and answers composition in
-- @O(1)@ without materializing.
thinSitePresentation :: ThinSiteKernel validation obj -> ThinSitePresentation obj
thinSitePresentation kernel =
  let objectIds = thinSiteKernelObjectIds kernel
      codomain = thinSiteKernelCodomain kernel
      objectSet = thinSiteFinObjectSet objectIds
      endpointPairIds = denseThinEndpointMorphismsFromCategory codomain
   in ThinSitePresentation
        { thinPresentationObjectIds = objectIds,
          thinPresentationPairIds = thinSitePairIdsFromEndpoints objectIds endpointPairIds,
          thinPresentationObjects = objectSet,
          thinPresentationMorphisms = thinSiteMorphismMap endpointPairIds,
          thinPresentationComposition = finCatExplicitCompositionMapView codomain
        }

thinSitePairIdsFromEndpoints :: Map obj FinObjectId -> Map (FinObjectId, FinObjectId) FinMorphismId -> Map (obj, obj) FinMorphismId
thinSitePairIdsFromEndpoints objectIds endpointPairIds =
  objectIds
    & Map.toAscList
    >>= ( \(sourceObject, sourceId) ->
            objectIds
              & Map.toAscList
              >>= ( \(targetObject, targetId) ->
                      case Map.lookup (sourceId, targetId) endpointPairIds of
                        Nothing -> []
                        Just morphismId -> [((sourceObject, targetObject), morphismId)]
                  )
        )
    & Map.fromDistinctAscList

thinSiteObjectIds :: Ord obj => [obj] -> Map obj FinObjectId
thinSiteObjectIds objects =
  objects
    & zip [0 ..]
    & fmap (\(idx, obj) -> (obj, FinObjectId idx))
    & Map.fromList

thinSiteFinObjectSet :: Map obj FinObjectId -> Set FinObjectId
thinSiteFinObjectSet objectIds =
  objectIds
    & Map.elems
    & Set.fromList

thinSiteMorphismMap :: Map (FinObjectId, FinObjectId) FinMorphismId -> Map (FinObjectId, FinObjectId) [FinMorphismId]
thinSiteMorphismMap =
  fmap (: [])

thinPresentationToFinCat ::
  ThinSitePresentation obj ->
  Either (NonEmpty FinCatValidationError) FinCat
thinPresentationToFinCat presentation =
  mkFinCat
    (thinPresentationObjects presentation)
    (thinPresentationMorphisms presentation)
    (thinPresentationComposition presentation)

-- | Compile the import category while deliberately leaving cover validation
-- outside the obligation. The resulting kernel cannot construct a
-- 'SitePathCategory' or a 'SitePathQuotient'.
thinSiteImportKernel :: Ord obj => SiteManifest obj -> Either (SiteFinCatError obj) (ThinSiteKernel 'ImportsValidated obj)
thinSiteImportKernel =
  compileThinSiteKernel validateSiteImportManifest

-- | Compile a full site kernel after validating both imports and cover axioms.
thinSiteKernel :: Ord obj => SiteManifest obj -> Either (SiteFinCatError obj) (ThinSiteKernel 'SiteValidated obj)
thinSiteKernel =
  compileThinSiteKernel validateSiteManifestDetailed

compileThinSiteKernel ::
  Ord obj =>
  (SiteManifest obj -> Either (NonEmpty (SiteViolation obj)) (ValidatedSiteManifest obj)) ->
  SiteManifest obj ->
  Either (SiteFinCatError obj) (ThinSiteKernel validation obj)
compileThinSiteKernel validateManifest manifest =
  case validateManifest manifest of
    Left errors -> Left (SiteManifestInvalid errors)
    Right validatedManifest ->
      Right (thinSiteKernelFromValidatedManifest manifest validatedManifest)

thinSiteKernelFromValidatedManifest ::
  Ord obj =>
  SiteManifest obj ->
  ValidatedSiteManifest obj ->
  ThinSiteKernel validation obj
thinSiteKernelFromValidatedManifest manifest validatedManifest =
  let objectValues = validatedSiteObjectVector validatedManifest
      objectIds = thinSiteObjectIds (Vector.toList objectValues)
      codomain =
        trustedDenseThinFinCatFromReachabilityRows
          (thinSiteFinObjectSet objectIds)
          (validatedSiteReachabilityRows validatedManifest)
   in ThinSiteKernel
        { thinSiteKernelManifest = manifest,
          thinSiteKernelCodomain = codomain,
          thinSiteKernelObjectIds = objectIds,
          thinSiteKernelObjectValues = objectValues
        }

thinSiteFinObject :: Ord obj => ThinSiteKernel validation obj -> obj -> Either (ThinSiteLookupError obj) FinObj
thinSiteFinObject kernel objectValue =
  case Map.lookup objectValue (thinSiteKernelObjectIds kernel) of
    Nothing ->
      Left (ThinSiteUnknownObject objectValue)
    Just objectId ->
      case mkFinObject (thinSiteKernelCodomain kernel) objectId of
        Left _ ->
          Left (ThinSiteCodomainObjectMissing objectId)
        Right finObject ->
          Right finObject

-- | Recover the semantic manifest object for an object from this kernel's
-- codomain. The category-handle check rejects objects from a different finite
-- category; a correctly handled object without an entry is reported as an
-- explicit unmapped-codomain obstruction rather than silently reusing its
-- numeric identifier.
thinSiteObjectValue :: ThinSiteKernel validation obj -> FinObj -> Either ThinSiteObjectValueError obj
thinSiteObjectValue kernel finObject
  | finObjCategoryHandle finObject /= expectedHandle =
      Left
        ( ThinSiteForeignCodomainObject
            expectedHandle
            (finObjCategoryHandle finObject)
            (finObjId finObject)
        )
  | otherwise =
      case finObjId finObject of
        objectId@(FinObjectId objectIndex) ->
          case thinSiteKernelObjectValues kernel Vector.!? objectIndex of
            Nothing -> Left (ThinSiteUnmappedCodomainObject objectId)
            Just objectValue -> Right objectValue
  where
    expectedHandle = finCatHandle (thinSiteKernelCodomain kernel)

thinSiteFinMorphism :: Ord obj => ThinSiteKernel validation obj -> NonEmpty obj -> Either (ThinSiteLookupError obj) FinMor
thinSiteFinMorphism kernel nodes =
  thinSiteFinMorphismByEndpoints
    kernel
    (NonEmpty.head nodes)
    (NonEmpty.last nodes)

thinSiteFinMorphismByEndpoints ::
  Ord obj =>
  ThinSiteKernel validation obj ->
  obj ->
  obj ->
  Either (ThinSiteLookupError obj) FinMor
thinSiteFinMorphismByEndpoints kernel sourceValue targetValue =
  if sourceValue == targetValue
    then do
      sourceObject <- thinSiteFinObject kernel sourceValue
      first ThinSiteCodomainMorphismInvalid (identity (thinSiteKernelCodomain kernel) sourceObject)
    else
      case thinSiteMorphismIdByEndpoints kernel sourceValue targetValue of
        Nothing ->
          Left (ThinSiteUnknownMorphismPair sourceValue targetValue)
        Just morId ->
          case mkFinMorphism (thinSiteKernelCodomain kernel) morId of
            Left _ ->
              Left (ThinSiteCodomainMorphismMissing morId)
            Right finMorphism ->
              Right finMorphism

thinSiteMorphismIdByEndpoints :: Ord obj => ThinSiteKernel validation obj -> obj -> obj -> Maybe FinMorphismId
thinSiteMorphismIdByEndpoints kernel sourceValue targetValue = do
  sourceId <- Map.lookup sourceValue (thinSiteKernelObjectIds kernel)
  targetId <- Map.lookup targetValue (thinSiteKernelObjectIds kernel)
  finCatMorphismIdByEndpoints (thinSiteKernelCodomain kernel) sourceId targetId
