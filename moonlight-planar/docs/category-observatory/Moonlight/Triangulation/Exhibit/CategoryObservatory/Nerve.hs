module Moonlight.Triangulation.Exhibit.CategoryObservatory.Nerve
  ( projectNerve,
    validateNerveCount,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Moonlight.Category (chainVertices)
import Moonlight.Category.Simplicial
  ( GeneratedSSet,
    NerveSimplex,
    TruncatedNormalizedSSet,
    generatedSimplicesAtDimension,
    nerveSimplexChain,
    normalizedNerve,
    simplicesAtDimension,
    unnormalizedNerve,
  )
import Moonlight.Homology.Pure.Topology.CellCategory (ComplexCat)
import Moonlight.Homology.Pure.Topology.CellComplex (CellComplex2D (..))
import Moonlight.Triangulation.CellComplex (DCELComplex)
import Moonlight.Triangulation.Exhibit.CategoryObservatory.Category (incidenceFlagCount)
import Moonlight.Triangulation.Exhibit.CategoryObservatory.Identifier (objectIdentifier)
import Moonlight.Triangulation.Exhibit.CategoryObservatory.Model
  ( ExhibitError (..),
    NerveProjection (..),
    NerveSimplexCount (..),
    NerveTwoSimplex (..),
  )
import Numeric.Natural (Natural)

projectNerve :: ComplexCat DCELComplex -> Either ExhibitError NerveProjection
projectNerve categoryValue =
  let normalizedNerveValue = normalizedNerve categoryValue nerveObservationBound
      unnormalizedNerveValue = unnormalizedNerve categoryValue nerveObservationBound
   in NerveProjection
        <$> traverse
          projectTwoSimplex
          (zip [0 ..] (simplicesAtDimension normalizedNerveValue nerveObservationBound))
        <*> pure
          ( fmap
              (projectNerveSimplexCount normalizedNerveValue unnormalizedNerveValue)
              nerveObservationDimensions
          )

validateNerveCount :: DCELComplex -> NerveProjection -> Either ExhibitError ()
validateNerveCount complexValue nerveProjection =
  let expectedCount = sum (fmap (incidenceFlagCount complexValue) (faces complexValue))
      observedCount = length (nerveTwoSimplices nerveProjection)
   in if observedCount == expectedCount
        then Right ()
        else Left (ExhibitNerveCountMismatch expectedCount observedCount)

projectNerveSimplexCount ::
  TruncatedNormalizedSSet (NerveSimplex (ComplexCat DCELComplex)) ->
  GeneratedSSet (NerveSimplex (ComplexCat DCELComplex)) ->
  Natural ->
  NerveSimplexCount
projectNerveSimplexCount normalizedNerveValue unnormalizedNerveValue dimensionValue =
  NerveSimplexCount
    { nerveDimension = dimensionValue,
      normalizedSimplexCount = length (simplicesAtDimension normalizedNerveValue dimensionValue),
      unnormalizedSimplexCount = length (generatedSimplicesAtDimension unnormalizedNerveValue dimensionValue)
    }

projectTwoSimplex ::
  (Int, NerveSimplex (ComplexCat DCELComplex)) ->
  Either ExhibitError NerveTwoSimplex
projectTwoSimplex (simplexIndex, simplexValue) =
  case chainVertices (nerveSimplexChain simplexValue) of
    faceObject :| [edgeObject, vertexObject] ->
      Right
        NerveTwoSimplex
          { nerveSimplexIdentifier = "simplex-" <> show simplexIndex,
            nerveSimplexObjectIdentifiers =
              fmap objectIdentifier [faceObject, edgeObject, vertexObject]
          }
    _ -> Left (ExhibitMalformedTwoSimplex simplexIndex)

nerveObservationBound :: Natural
nerveObservationBound = 2

nerveObservationDimensions :: [Natural]
nerveObservationDimensions = [0, 1, 2]
