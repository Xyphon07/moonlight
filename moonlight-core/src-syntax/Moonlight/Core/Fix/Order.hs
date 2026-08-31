-- | 'OrderedFix', a @newtype@ giving 'Eq' and 'Ord' for fixed-point terms over
-- any 'Language' functor by structural comparison.
module Moonlight.Core.Fix.Order
  ( OrderedFix (..),
  )
where

import Data.Fix (Fix (..))
import Data.Kind (Type)
import Moonlight.Core.Language (Language)
import Prelude (Eq ((==)), Functor (fmap), Ord (compare), Ordering (EQ))

type OrderedFix :: (Type -> Type) -> Type
newtype OrderedFix f = OrderedFix (Fix f)

instance Language f => Eq (OrderedFix f) where
  OrderedFix leftTerm == OrderedFix rightTerm =
    compare (OrderedFix leftTerm) (OrderedFix rightTerm) == EQ

instance Language f => Ord (OrderedFix f) where
  compare (OrderedFix (Fix leftNode)) (OrderedFix (Fix rightNode)) =
    compare (fmap OrderedFix leftNode) (fmap OrderedFix rightNode)
