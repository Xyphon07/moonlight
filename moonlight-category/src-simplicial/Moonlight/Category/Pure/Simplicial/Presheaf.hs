-- | Simplicial sets as presheaves on the simplex category: object and morphism
-- maps with identity and composition law checks.
module Moonlight.Category.Pure.Simplicial.Presheaf
  ( SimplicialPresheaf,
    presheafUpperBound,
    presheafObjectMap,
    presheafMorphismMap,
    generatedAsPresheaf,
    applyPresheaf,
    presheafIdentityLaw,
    presheafCompositionLaw,
  )
where

import Control.Monad ((<=<), foldM)
import Data.Function ((&))
import Data.Kind (Type)
import Numeric.Natural (Natural)
import Moonlight.Category.Pure.Simplicial.Delta
  ( DeltaMorphism,
    composeDeltaMorphism,
    deltaIdentity,
    injectionMissingIndices,
    normalCodomainDimension,
    normalDomainDimension,
    normalInjection,
    normalSurjection,
    normalizeDeltaMorphism,
    surjectionDegeneracyIndices,
  )
import Moonlight.Category.Pure.Simplicial.Set
  ( GeneratedSSet,
    applyGeneratedDegeneracyAtDimension,
    applyGeneratedFaceAtDimension,
    generatedSimplicesAtDimension,
    generationBound,
  )

type SimplicialPresheaf :: Type -> Type
data SimplicialPresheaf simplex = SimplicialPresheaf
  { presheafUpperBound :: Natural,
    presheafObjectMap :: Natural -> [simplex],
    presheafMorphismMap :: DeltaMorphism -> simplex -> Maybe simplex
  }

applyPresheaf :: SimplicialPresheaf simplex -> DeltaMorphism -> simplex -> Maybe simplex
applyPresheaf = presheafMorphismMap

stepFaces :: (Natural -> Natural -> simplex -> Maybe simplex) -> Natural -> [Natural] -> simplex -> Maybe (Natural, simplex)
stepFaces applyFace startDimension faceIndices simplexValue =
  foldM
    ( \(dimensionValue, currentSimplex) faceIndex -> do
        nextSimplex <- applyFace dimensionValue faceIndex currentSimplex
        pure (dimensionValue - 1, nextSimplex)
    )
    (startDimension, simplexValue)
    (reverse faceIndices)

stepDegeneracies :: (Natural -> Natural -> simplex -> Maybe simplex) -> Natural -> [Natural] -> simplex -> Maybe (Natural, simplex)
stepDegeneracies applyDegeneracy startDimension degeneracyIndices simplexValue =
  foldM
    ( \(dimensionValue, currentSimplex) degeneracyIndex -> do
        nextSimplex <- applyDegeneracy dimensionValue degeneracyIndex currentSimplex
        pure (dimensionValue + 1, nextSimplex)
    )
    (startDimension, simplexValue)
    (reverse degeneracyIndices)

applyWithGenerators ::
  (Natural -> Natural -> simplex -> Maybe simplex) ->
  (Natural -> Natural -> simplex -> Maybe simplex) ->
  DeltaMorphism ->
  simplex ->
  Maybe simplex
applyWithGenerators applyFace applyDegeneracy morphism simplexValue =
  do
    normalForm <- normalizeDeltaMorphism morphism
    let missingFaces =
          injectionMissingIndices
            (normalCodomainDimension normalForm)
            (normalInjection normalForm)
        degeneracyIndices = surjectionDegeneracyIndices (normalSurjection normalForm)
    (middleDimension, afterFaces) <-
      stepFaces applyFace (normalCodomainDimension normalForm) missingFaces simplexValue
    (resultDimension, afterDegeneracies) <-
      stepDegeneracies applyDegeneracy middleDimension degeneracyIndices afterFaces
    if resultDimension == normalDomainDimension normalForm
      then Just afterDegeneracies
      else Nothing

generatedAsPresheaf :: GeneratedSSet simplex -> SimplicialPresheaf simplex
generatedAsPresheaf generatedSet =
  SimplicialPresheaf
    { presheafUpperBound = generationBound generatedSet,
      presheafObjectMap = generatedSimplicesAtDimension generatedSet,
      presheafMorphismMap =
        applyWithGenerators
          (applyGeneratedFaceAtDimension generatedSet)
          (applyGeneratedDegeneracyAtDimension generatedSet)
    }

presheafIdentityLaw :: Eq simplex => SimplicialPresheaf simplex -> Natural -> Bool
presheafIdentityLaw presheaf dimensionValue =
  presheafObjectMap presheaf dimensionValue
    & all
      (\simplexValue -> applyPresheaf presheaf (deltaIdentity dimensionValue) simplexValue == Just simplexValue)

presheafCompositionLaw ::
  Eq simplex =>
  SimplicialPresheaf simplex ->
  DeltaMorphism ->
  DeltaMorphism ->
  simplex ->
  Bool
presheafCompositionLaw presheaf outer inner simplexValue =
  let leftAction =
        composeDeltaMorphism outer inner
          >>= (\composed -> applyPresheaf presheaf composed simplexValue)
      rightAction =
        (applyPresheaf presheaf inner <=< applyPresheaf presheaf outer) simplexValue
   in leftAction == rightAction
