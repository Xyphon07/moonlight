{-# LANGUAGE AllowAmbiguousTypes #-}

-- | The pooled law-harness surface: category and site law records with their
-- constructors, re-exported from the per-domain harness modules.
module Moonlight.Category.Effect.Harness
  ( CategoryLaws (..),
    SiteLaws (..),
    mkCategoryLaws,
    mkSiteLaws,
    adhesiveWitnessMonicSound,
    pushoutComplementSquareCommutes,
    pushoutComplementUniversal,
    pbpoPullbackSquareCommutes,
    pbpoPushoutSquareCommutes,
    pbpoComplementUniversal,
    galoisAdjoint,
    galoisDeflation,
    galoisInflation,
    galoisRetraction,
    ordinalGaloisMonotone,
    productProjection1,
    productProjection2,
    coproductInjection1,
    coproductInjection2,
    pullbackCommutative,
    pushoutCommutative,
    equalizerCommutative,
    coequalizerCommutative,
    horizontalBoundary,
    verticalBoundary,
    interchange,
  )
where

import Moonlight.Category.Effect.Harness.Adhesive
  ( adhesiveWitnessMonicSound,
    pbpoComplementUniversal,
    pbpoPullbackSquareCommutes,
    pbpoPushoutSquareCommutes,
    pushoutComplementSquareCommutes,
    pushoutComplementUniversal,
  )
import Moonlight.Category.Effect.Harness.Algebra
  ( galoisAdjoint,
    galoisDeflation,
    galoisInflation,
    galoisRetraction,
    ordinalGaloisMonotone,
  )
import Moonlight.Category.Effect.Harness.Category
  ( mkCategoryLaws,
  )
import Moonlight.Category.Effect.Harness.Core
  ( CategoryLaws (..),
    SiteLaws (..),
  )
import Moonlight.Category.Effect.Harness.Higher
  ( horizontalBoundary,
    interchange,
    verticalBoundary,
  )
import Moonlight.Category.Effect.Harness.Limits
  ( coequalizerCommutative,
    coproductInjection1,
    coproductInjection2,
    equalizerCommutative,
    productProjection1,
    productProjection2,
    pullbackCommutative,
    pushoutCommutative,
  )
import Moonlight.Category.Effect.Harness.Site (mkSiteLaws)
