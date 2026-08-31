-- | Stable finite-category values for executable laws, tests, and benchmarks.
module Moonlight.Category.Effect.Fixture.FinCat
  ( sampleFinCat,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Moonlight.Category.Pure.FinCat
  ( FinCat,
    FinGeneratorId (..),
    FinMorphismId (..),
    FinObjectId (..),
    trustedThinFinCatFromTransitiveEndpoints,
  )

-- | The thin chain @0 -> 1 -> 2@ with its composite.
sampleFinCat :: FinCat
sampleFinCat =
  trustedThinFinCatFromTransitiveEndpoints
    (Set.fromList (FinObjectId <$> [0, 1, 2]))
    ( Map.fromList
        [ ((FinObjectId 0, FinObjectId 1), FinGeneratorMorphismId (FinGeneratorId 10)),
          ((FinObjectId 1, FinObjectId 2), FinGeneratorMorphismId (FinGeneratorId 11)),
          ((FinObjectId 0, FinObjectId 2), FinGeneratorMorphismId (FinGeneratorId 12))
        ]
    )
