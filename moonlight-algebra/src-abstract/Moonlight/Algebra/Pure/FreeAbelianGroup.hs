-- | The free abelian group on a set of generators, as finite formal sums backed
-- by 'Moonlight.Algebra.Pure.SparseVec.SparseVec'.
--
-- Laws: the universal abelian group on its generators — addition is associative
-- and commutative, the empty sum is the identity, every element has an inverse.
module Moonlight.Algebra.Pure.FreeAbelianGroup
  ( FreeAbelianGroup,
    fromTerms,
    toTerms,
    singleton,
    normalizeFreeAbelianGroup,
  )
where

import Data.Coerce (coerce)
import Data.Kind (Type)
import Moonlight.Algebra.Pure.Module (FreeModule (..), Module (..))
import Moonlight.Algebra.Pure.SparseVec (SparseVec)
import qualified Moonlight.Algebra.Pure.SparseVec as SparseVec
import Moonlight.Core
  ( AdditiveGroup (..),
    AdditiveMonoid,
    IsoNorm (..),
    isoNormalize,
  )

type FreeAbelianGroup :: Type -> Type
newtype FreeAbelianGroup g = FreeAbelianGroup (SparseVec Integer g)
  deriving stock (Eq, Show)
  deriving newtype
    ( AdditiveMonoid,
      AdditiveGroup,
      Module Integer,
      FreeModule Integer
    )

fromTerms :: Ord g => [(g, Integer)] -> FreeAbelianGroup g
fromTerms = coerce SparseVec.fromEntries

toTerms :: FreeAbelianGroup g -> [(g, Integer)]
toTerms = coerce SparseVec.toEntries

singleton :: Ord g => g -> Integer -> FreeAbelianGroup g
singleton basisElement weight = fromTerms [(basisElement, weight)]

normalizeFreeAbelianGroup :: Ord g => FreeAbelianGroup g -> FreeAbelianGroup g
normalizeFreeAbelianGroup = isoNormalize

instance Ord g => IsoNorm (FreeAbelianGroup g) [(g, Integer)] where
  isoFrom = fromTerms
  isoTo = toTerms
