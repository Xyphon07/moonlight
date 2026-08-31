module Moonlight.Homology.Pure.Degree
  ( HomologicalDegree (..),
    incrementDegree,
    decrementDegree,
  )
where

import Data.Kind (Type)

type HomologicalDegree :: Type
newtype HomologicalDegree = HomologicalDegree
  { unHomologicalDegree :: Int
  }
  deriving stock (Eq, Ord, Show, Read)

incrementDegree :: HomologicalDegree -> HomologicalDegree
incrementDegree (HomologicalDegree degreeValue) =
  HomologicalDegree (degreeValue + 1)

decrementDegree :: HomologicalDegree -> HomologicalDegree
decrementDegree (HomologicalDegree degreeValue) =
  HomologicalDegree (degreeValue - 1)
