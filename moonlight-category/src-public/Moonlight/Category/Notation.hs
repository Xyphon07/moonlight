-- | An opt-in ergonomic notation for working with 'FinCat' morphisms that reads as
-- mathematics while staying zero-cost. Every binding here is a trusted, total view
-- over already-validated data: because the 'FinMor' constructor is unexported, every
-- morphism in hand was produced by a checked path, so 'dom'/'cod' need not re-validate
-- and compile to plain record reads.
--
-- Construction of finite categories lives in "Moonlight.Category.Presentation".
-- This module begins only after a 'FinCat' has been compiled and validated.
--
-- This module is deliberately /not/ re-exported by "Moonlight.Category": the scoped
-- operators below are introduced only where you ask for them, leaving the rest of the
-- public facade operator-averse.
--
-- == Scoped operators
--
-- Composition and reachability need the category, so pin it once with a @let@ and the
-- mathematics reads on the page:
--
-- > import Moonlight.Category.Notation
-- >
-- > example category f g h =
-- >   let (∘) = composeIn category   -- g ∘ f  ≡  g after f
-- >       (≤) = reachableIn category
-- >    in (h ∘ g ∘ f, dom f, cod h, 0 ≤ (2 :: FinObjectId))
module Moonlight.Category.Notation
  ( dom,
    cod,
    domObj,
    codObj,
    idOf,
    hom,
    composeIn,
    reachableIn,
  )
where

import Data.Maybe (isJust)
import Moonlight.Category.Pure.Category (composeMor)
import Moonlight.Category.Pure.FinCat
  ( FinCat,
    FinCatError,
    FinMor,
    FinObj,
    FinObjectId,
    finCatHomMorphism,
    finObjectIdentityMor,
    finCatMorphismIdByEndpoints,
    finMorCodObject,
    finMorDomObject,
    finMorSourceId,
    finMorTargetId,
  )

-- | The source object identifier of a morphism. O(1), total.
dom :: FinMor -> FinObjectId
dom = finMorSourceId
{-# INLINE dom #-}

-- | The target object identifier of a morphism. O(1), total.
cod :: FinMor -> FinObjectId
cod = finMorTargetId
{-# INLINE cod #-}

-- | The source object of a morphism. O(1), total.
domObj :: FinMor -> FinObj
domObj = finMorDomObject
{-# INLINE domObj #-}

-- | The target object of a morphism. O(1), total.
codObj :: FinMor -> FinObj
codObj = finMorCodObject
{-# INLINE codObj #-}

-- | The identity morphism at an already validated object.
idOf :: FinObj -> FinMor
idOf = finObjectIdentityMor
{-# INLINE idOf #-}

-- | The unique morphism between two endpoints, when one exists.
hom :: FinCat -> FinObjectId -> FinObjectId -> Maybe FinMor
hom = finCatHomMorphism
{-# INLINE hom #-}

-- | Composition pinned to a category: @composeIn cat g f@ is @g ∘ f@ (f then g), and
-- is 'Left' exactly when the endpoints do not meet. Bind to @(∘)@ at the use site.
composeIn :: FinCat -> FinMor -> FinMor -> Either FinCatError FinMor
composeIn = composeMor
{-# INLINE composeIn #-}

-- | Whether the second object is reachable from the first (O(1) on the dense form).
-- Bind to @(≤)@ at the use site for a preorder reading.
reachableIn :: FinCat -> FinObjectId -> FinObjectId -> Bool
reachableIn category sourceId targetId =
  isJust (finCatMorphismIdByEndpoints category sourceId targetId)
{-# INLINE reachableIn #-}
