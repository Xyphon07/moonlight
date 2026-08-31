-- | Finite power sets ('PowerSet') as a 'BooleanAlgebra': join is union, meet is
-- intersection.
--
-- Laws: a distributive (Boolean) lattice — union and intersection are
-- idempotent, commutative, associative, absorptive, distributive, complemented.
module Moonlight.Algebra.Pure.PowerSet
  ( PowerSet,
    fromList,
    toPowerSetList,
    normalizePowerSet,
    member,
  )
where

import Data.Kind (Type)
import qualified Data.Set as Set
import Moonlight.Algebra.Pure.Lattice
  ( BooleanAlgebra (..),
    BoundedJoinSemilattice (..),
    BoundedMeetSemilattice (..),
    DistributiveLattice,
    HeytingAlgebra (..),
    JoinSemilattice (..),
    Lattice,
    MeetSemilattice (..),
    OrderedLattice,
  )
import Moonlight.Core
  ( FiniteUniverse,
    IsoNorm (..),
    PartialOrder (..),
    finiteUniverseSet,
    isoNormalize,
  )

type PowerSet :: Type -> Type
newtype PowerSet a = PowerSet (Set.Set a)
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype
    ( JoinSemilattice,
      BoundedJoinSemilattice,
      MeetSemilattice,
      PartialOrder,
      Lattice,
      OrderedLattice,
      DistributiveLattice
    )

fromList :: Ord a => [a] -> PowerSet a
fromList = PowerSet . Set.fromList

toPowerSetList :: PowerSet a -> [a]
toPowerSetList (PowerSet elements) = Set.toAscList elements

normalizePowerSet :: Ord a => PowerSet a -> PowerSet a
normalizePowerSet = isoNormalize

member :: Ord a => a -> PowerSet a -> Bool
member value (PowerSet elements) = Set.member value elements

universeSet :: (Ord a, FiniteUniverse a) => Set.Set a
universeSet =
  finiteUniverseSet

instance Ord a => IsoNorm (PowerSet a) [a] where
  isoFrom = fromList
  isoTo = toPowerSetList

instance (Ord a, FiniteUniverse a) => BoundedMeetSemilattice (PowerSet a) where
  top = PowerSet universeSet

instance (Ord a, FiniteUniverse a) => HeytingAlgebra (PowerSet a) where
  implies left = join (complement left)

instance (Ord a, FiniteUniverse a) => BooleanAlgebra (PowerSet a) where
  complement (PowerSet elements) =
    PowerSet (Set.difference universeSet elements)
