
-- | The standard simplicial spaces: simplices, their boundaries, and horns,
-- as generated simplicial sets.
module Moonlight.Category.Pure.Simplicial.Spaces
  ( standardSimplexGenerated,
    standardSimplex,
    boundarySimplexGenerated,
    boundarySimplex,
    hornSimplexGenerated,
    hornSimplex,
  )
where

import Data.Function ((&))
import Data.List (genericSplitAt)
import Data.Map.Strict qualified as Map
import Numeric.Natural (Natural)
import Moonlight.Category.Pure.Simplicial.Delta
  ( allDeltaMorphisms,
    deltaMapValues,
  )
import Moonlight.Category.Pure.Simplicial.Set
  ( GeneratedSSet,
    TruncatedNormalizedSSet,
    generatedSimplicesAtDimension,
  )
import Moonlight.Category.Pure.Simplicial.Set.Internal
  ( GeneratedSSet (generatedDegeneracyMap, generatedFaceMap),
    trustedGeneratedSSet,
    trustedTruncatedNormalizedSSet,
  )
import Moonlight.Category.Pure.Simplicial.TypeLevel (finValue)

duplicateAt :: Natural -> [a] -> Maybe [a]
duplicateAt targetIndex values =
  case genericSplitAt targetIndex values of
    (_, []) -> Nothing
    (prefix, x : suffix) -> Just (prefix <> [x, x] <> suffix)

removeAt :: Natural -> [a] -> Maybe [a]
removeAt targetIndex values =
  case genericSplitAt targetIndex values of
    (_, []) -> Nothing
    (prefix, _ : suffix) -> Just (prefix <> suffix)


simplexRows :: Natural -> Natural -> [[Natural]]
simplexRows simplexDimension domainDimension =
  deltaMapValues <$> allDeltaMorphisms domainDimension simplexDimension

availableVertexCount :: Natural -> Natural -> Natural
availableVertexCount lowerBound upperBound =
  if lowerBound > upperBound
    then 0
    else upperBound - lowerBound + 1

strictlyIncreasingRows :: Natural -> Natural -> Natural -> [[Natural]]
strictlyIncreasingRows lowerBound upperBound rowLength
  | rowLength == 0 = [[]]
  | rowLength > availableVertexCount lowerBound upperBound = []
  | otherwise =
      [lowerBound .. upperBound]
        & concatMap
          ( \headValue ->
              strictlyIncreasingRows (headValue + 1) upperBound (rowLength - 1)
                & map (headValue :)
          )

nondegenerateSimplexRows :: Natural -> Natural -> [[Natural]]
nondegenerateSimplexRows simplexDimension domainDimension =
  strictlyIncreasingRows 0 simplexDimension (domainDimension + 1)

nonemptyNondegenerateRows :: Natural -> Natural -> ([Natural] -> Bool) -> Map.Map Natural [[Natural]]
nonemptyNondegenerateRows simplexDimension truncationBound rowPredicate =
  [0 .. truncationBound]
    & fmap
      ( \dimensionValue ->
          ( dimensionValue,
            nondegenerateSimplexRows simplexDimension dimensionValue
              & filter rowPredicate
          )
      )
    & filter (not . null . snd)
    & Map.fromAscList

omitsVertex :: Natural -> [Natural] -> Bool
omitsVertex vertexValue simplexValue =
  vertexValue `notElem` simplexValue

belongsToBoundary :: Natural -> [Natural] -> Bool
belongsToBoundary simplexDimension simplexValue =
  any (`omitsVertex` simplexValue) [0 .. simplexDimension]

belongsToHorn :: Natural -> Natural -> [Natural] -> Bool
belongsToHorn simplexDimension missingFaceIndex simplexValue =
  [0 .. simplexDimension]
    & any
      (\vertexValue -> vertexValue /= missingFaceIndex && omitsVertex vertexValue simplexValue)

standardSimplexGenerated :: Natural -> Natural -> GeneratedSSet [Natural]
standardSimplexGenerated simplexDimension truncationBound =
  trustedGeneratedSSet
    truncationBound
    (simplexRows simplexDimension)
    (\_ faceIndex simplexValue -> removeAt (finValue faceIndex) simplexValue)
    (\_ degeneracyIndex simplexValue -> duplicateAt (finValue degeneracyIndex) simplexValue)

standardSimplex :: Natural -> Natural -> TruncatedNormalizedSSet [Natural]
standardSimplex simplexDimension truncationBound =
  trustedTruncatedNormalizedSSet
    truncationBound
    (nonemptyNondegenerateRows simplexDimension truncationBound (const True))
    (\_ faceIndex simplexValue -> removeAt (finValue faceIndex) simplexValue)
    (\_ degeneracyIndex simplexValue -> duplicateAt (finValue degeneracyIndex) simplexValue)

boundarySimplexGenerated :: Natural -> Natural -> GeneratedSSet [Natural]
boundarySimplexGenerated simplexDimension truncationBound =
  let baseSet = standardSimplexGenerated simplexDimension truncationBound
   in trustedGeneratedSSet
        truncationBound
        (\dimensionValue' -> filter (belongsToBoundary simplexDimension) (generatedSimplicesAtDimension baseSet dimensionValue'))
        (generatedFaceMap baseSet)
        (generatedDegeneracyMap baseSet)

boundarySimplex :: Natural -> Natural -> TruncatedNormalizedSSet [Natural]
boundarySimplex simplexDimension truncationBound =
  trustedTruncatedNormalizedSSet
    truncationBound
    (nonemptyNondegenerateRows simplexDimension truncationBound (belongsToBoundary simplexDimension))
    (\_ faceIndex simplexValue -> removeAt (finValue faceIndex) simplexValue)
    (\_ degeneracyIndex simplexValue -> duplicateAt (finValue degeneracyIndex) simplexValue)

hornSimplexGenerated :: Natural -> Natural -> Natural -> Maybe (GeneratedSSet [Natural])
hornSimplexGenerated simplexDimension missingFaceIndex truncationBound
  | simplexDimension == 0 = Nothing
  | missingFaceIndex > simplexDimension = Nothing
  | otherwise =
      let baseSet = standardSimplexGenerated simplexDimension truncationBound
       in Just
            ( trustedGeneratedSSet
                truncationBound
                (\dimensionValue' -> filter (belongsToHorn simplexDimension missingFaceIndex) (generatedSimplicesAtDimension baseSet dimensionValue'))
                (generatedFaceMap baseSet)
                (generatedDegeneracyMap baseSet)
            )

hornSimplex :: Natural -> Natural -> Natural -> Maybe (TruncatedNormalizedSSet [Natural])
hornSimplex simplexDimension missingFaceIndex truncationBound =
  if simplexDimension == 0 || missingFaceIndex > simplexDimension
    then Nothing
    else
      Just
        ( trustedTruncatedNormalizedSSet
            truncationBound
            (nonemptyNondegenerateRows simplexDimension truncationBound (belongsToHorn simplexDimension missingFaceIndex))
            (\_ faceIndex simplexValue -> removeAt (finValue faceIndex) simplexValue)
            (\_ degeneracyIndex simplexValue -> duplicateAt (finValue degeneracyIndex) simplexValue)
        )
