-- | Executable checks for path-quotient uniqueness, faithfulness, and
-- interpreter coherence.
module Moonlight.Category.Effect.PathQuotientHarness
  ( quotientUniquenessPerEndpoint,
    pathThinCodomainFaithful,
    quotientInterpreterCoherence,
  )
where

import Data.Function ((&))
import Moonlight.Category.Effect.Laws.Generators (allPairs)
import Moonlight.Category.Effect.SitePathEnumeration
  ( allEqual,
    sitePathMorphisms,
    sitePathObjects,
  )
import Moonlight.Category.Pure.Site
  ( SitePathCategory,
    pathThinCodomainMorphism,
    pathThinCodomainObject,
    quotientMapMorphism,
    quotientMapObject,
    quotientPathThinMorphism,
    quotientPathThinObject,
    sitePathMorphismsBetween,
    sitePathQuotient,
  )

quotientUniquenessPerEndpoint :: forall obj. Ord obj => SitePathCategory obj -> obj -> obj -> Bool
quotientUniquenessPerEndpoint category sourceValue targetValue =
  sitePathMorphismsBetween category sourceValue targetValue
    & fmap quotientPathThinMorphism
    & allEqual

pathThinCodomainFaithful :: forall obj. Ord obj => SitePathCategory obj -> Bool
pathThinCodomainFaithful category =
  let mappedMorphisms =
        sitePathMorphisms category
          & fmap quotientPathThinMorphism
   in all
        ( \pairValue ->
            let leftValue = fst pairValue
                rightValue = snd pairValue
                mappedLeft = pathThinCodomainMorphism leftValue
                mappedRight = pathThinCodomainMorphism rightValue
             in mappedLeft /= mappedRight || leftValue == rightValue
        )
        (allPairs mappedMorphisms)

quotientInterpreterCoherence :: forall obj. Ord obj => SitePathCategory obj -> Bool
quotientInterpreterCoherence category =
  let quotient = sitePathQuotient category
      objectCoherence =
        sitePathObjects category
          & all
            ( \objectValue ->
                let interpreted =
                      pathThinCodomainObject (quotientPathThinObject objectValue)
                 in case quotientMapObject quotient objectValue of
                      Left _ -> False
                      Right expected -> interpreted == expected
            )
      morphismCoherence =
        sitePathMorphisms category
          & all
            ( \morphismValue ->
                let interpreted =
                      pathThinCodomainMorphism (quotientPathThinMorphism morphismValue)
                 in case quotientMapMorphism quotient morphismValue of
                      Left _ -> False
                      Right expected -> interpreted == expected
            )
   in objectCoherence && morphismCoherence
