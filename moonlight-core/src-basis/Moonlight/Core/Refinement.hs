{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Predicate-indexed refinement: a 'Refined' value carries proof it passed its 'RefinementPredicate'. Checked doors live here; the unsafe one lives in "Moonlight.Core.Unsound".
module Moonlight.Core.Refinement
  ( Refined,
    RefinementPredicate (..),
    refineMaybe,
    refineEither,
    refinedValue,
    withRefined,
  )
where

import Moonlight.Core.Aggregate (note)
import Moonlight.Internal.Unsound (Refined (..))
import Data.Proxy (Proxy (..))
import Prelude (Bool, Either, Maybe (..))

class RefinementPredicate tag value where
  refinementPredicate :: Proxy tag -> value -> Bool

refineMaybe :: forall tag value. RefinementPredicate tag value => value -> Maybe (Refined tag value)
refineMaybe candidate =
  if refinementPredicate (Proxy @tag) candidate
    then Just (Refined candidate)
    else Nothing

refineEither :: RefinementPredicate tag value => errorValue -> value -> Either errorValue (Refined tag value)
refineEither errorValue candidate =
  note errorValue (refineMaybe candidate)

refinedValue :: Refined tag value -> value
refinedValue (Refined value) = value

withRefined :: Refined tag value -> (value -> result) -> result
withRefined refinedCandidate use =
  use (refinedValue refinedCandidate)
