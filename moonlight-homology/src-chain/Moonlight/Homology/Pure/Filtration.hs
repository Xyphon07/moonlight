module Moonlight.Homology.Pure.Filtration
  ( FiltrationValue (..),
    CriticalKind (..),
    enumerateFromZero,
  )
where

import Data.Kind (Type)

type FiltrationValue :: Type
newtype FiltrationValue = FiltrationValue
  { unFiltrationValue :: Double
  }
  deriving stock (Eq, Ord, Show, Read)

type CriticalKind :: Type
data CriticalKind
  = Basin
  | Peak
  | Merge
  | Split
  | Pass
  | Isolated
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

enumerateFromZero :: Int -> [Int]
enumerateFromZero upperExclusive =
  if upperExclusive <= 0
    then []
    else [0 .. upperExclusive - 1]
