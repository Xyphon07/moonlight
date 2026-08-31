-- | Thin morphisms over a relation: at most one morphism between any two objects,
-- with relation-checked construction, identity and composition.
module Moonlight.Category.Pure.Thin
  ( ThinMorphism,
    thinMorphismSource,
    thinMorphismTarget,
    mkThinMorphismBy,
    identityThinMorphism,
    composeThinMorphismBy,
  )
where

import Data.Kind (Type)

type ThinMorphism :: Type -> Type
-- | The unique candidate morphism between two objects of a thin category.
data ThinMorphism obj = ThinMorphism
  { thinMorphismSource :: obj,
    thinMorphismTarget :: obj
  }
  deriving stock (Eq, Ord, Show)

-- | Admit a morphism exactly when the supplied relation holds.
mkThinMorphismBy :: (obj -> obj -> Bool) -> obj -> obj -> Maybe (ThinMorphism obj)
mkThinMorphismBy relation sourceValue targetValue =
  if relation sourceValue targetValue
    then Just (ThinMorphism sourceValue targetValue)
    else Nothing

-- | Construct the reflexive identity witness.
identityThinMorphism :: obj -> ThinMorphism obj
identityThinMorphism objectValue =
  ThinMorphism objectValue objectValue

-- | Compose relation-valid morphisms with matching middle endpoints.
composeThinMorphismBy :: Eq obj => (obj -> obj -> Bool) -> ThinMorphism obj -> ThinMorphism obj -> Maybe (ThinMorphism obj)
composeThinMorphismBy relation leftMorphism rightMorphism
  | not (relation (thinMorphismSource leftMorphism) (thinMorphismTarget leftMorphism)) = Nothing
  | not (relation (thinMorphismSource rightMorphism) (thinMorphismTarget rightMorphism)) = Nothing
  | thinMorphismTarget rightMorphism == thinMorphismSource leftMorphism =
      mkThinMorphismBy
        relation
        (thinMorphismSource rightMorphism)
        (thinMorphismTarget leftMorphism)
  | otherwise = Nothing
