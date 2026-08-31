module Moonlight.Homology.Pure.Group
  ( HomologyGroup (..),
  )
where

import Data.Kind (Type)

type HomologyGroup :: Type -> Type
data HomologyGroup r = HomologyGroup
  { freeRank :: Int,
    torsionInvariants :: [r]
  }
  deriving stock (Eq, Show)
