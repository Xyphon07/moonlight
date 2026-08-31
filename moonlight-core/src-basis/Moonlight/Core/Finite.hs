-- | The 'FiniteUniverse' class enumerating a type's finitely many inhabitants,
-- with list/set projections and a 'Bounded'/'Enum' default.
module Moonlight.Core.Finite
  ( FiniteUniverse (..),
    finiteUniverseList,
    finiteUniverseSet,
    boundedEnumUniverse,
  )
where

import Data.List.NonEmpty
  ( NonEmpty (..),
  )
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Prelude
  ( Bounded (..),
    Enum,
    Ord,
    drop,
    enumFromTo,
  )

-- | Law: 'finiteUniverse' enumerates every inhabitant of @value@ exactly once.
-- Total finite structures may rely on any @value@ appearing in this list.
class FiniteUniverse value where
  finiteUniverse :: NonEmpty value

finiteUniverseList :: FiniteUniverse value => [value]
finiteUniverseList =
  case finiteUniverse of
    first :| rest ->
      first : rest

finiteUniverseSet :: (FiniteUniverse value, Ord value) => Set value
finiteUniverseSet =
  Set.fromList finiteUniverseList

boundedEnumUniverse :: forall value. (Bounded value, Enum value) => NonEmpty value
boundedEnumUniverse =
  minBound :| drop 1 (enumFromTo minBound maxBound)
