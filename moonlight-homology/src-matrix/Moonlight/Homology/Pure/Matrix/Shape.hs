module Moonlight.Homology.Pure.Matrix.Shape
  ( cellCountAtDegree,
    dimensionsOf,
  )
where

import Data.Function ((&))
import Moonlight.Homology.Boundary.Finite
  ( FiniteChainComplex,
    degreeCardinality,
    maxHomologicalDegree,
  )
import Moonlight.Homology.Pure.Degree (HomologicalDegree (..))
import Moonlight.Homology.Pure.Filtration (enumerateFromZero)

cellCountAtDegree :: FiniteChainComplex r -> HomologicalDegree -> Int
cellCountAtDegree = degreeCardinality

dimensionsOf :: FiniteChainComplex r -> [HomologicalDegree]
dimensionsOf finite =
  enumerateFromZero (unHomologicalDegree (maxHomologicalDegree finite) + 1)
    & fmap HomologicalDegree
