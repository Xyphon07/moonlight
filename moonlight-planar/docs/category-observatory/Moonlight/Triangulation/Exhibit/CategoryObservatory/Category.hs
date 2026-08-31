module Moonlight.Triangulation.Exhibit.CategoryObservatory.Category
  ( incidenceFlagCount,
    projectCategory,
    validateCategoryEnumeration,
  )
where

import Data.Bifunctor (first)
import Data.Containers.ListUtils (nubOrd)
import Moonlight.Category
  ( Category (..),
    FiniteComposableCategory (..),
  )
import Moonlight.Homology.Pure.Topology.CellCategory
  ( ComplexCat,
    ComplexMor (..),
    ComplexOb,
  )
import Moonlight.Homology.Pure.Topology.CellComplex
  ( CellComplex2D (..),
    CellTypes (..),
    OrientedEdge (..),
  )
import Moonlight.Triangulation.CellComplex (DCELComplex)
import Moonlight.Triangulation.Exhibit.CategoryObservatory.Geometry (positionForObject)
import Moonlight.Triangulation.Exhibit.CategoryObservatory.Identifier
  ( morphismKind,
    objectIdentifier,
    objectKind,
    objectLabel,
  )
import Moonlight.Triangulation.Exhibit.CategoryObservatory.Model
  ( CategoryMorphism (..),
    CategoryObject (..),
    CategoryProjection (..),
    ExhibitError (..),
    MorphismEndpoint (..),
    MorphismInventory (..),
  )
import Moonlight.Triangulation.Types (DelaunayTriangulation)

projectCategory ::
  DelaunayTriangulation () ->
  ComplexCat DCELComplex ->
  Either ExhibitError CategoryProjection
projectCategory triangulation categoryValue =
  CategoryProjection
    <$> traverse
      (projectCategoryObject triangulation)
      (enumerateObjects categoryValue)
    <*> traverse
      (projectCategoryMorphism categoryValue)
      (zip [0 ..] (enumerateMorphisms categoryValue))

projectCategoryObject ::
  DelaunayTriangulation () ->
  ComplexOb DCELComplex ->
  Either ExhibitError CategoryObject
projectCategoryObject triangulation objectValue =
  CategoryObject
    (objectIdentifier objectValue)
    (objectKind objectValue)
    (objectLabel objectValue)
    <$> positionForObject triangulation objectValue

projectCategoryMorphism ::
  ComplexCat DCELComplex ->
  (Int, ComplexMor DCELComplex) ->
  Either ExhibitError CategoryMorphism
projectCategoryMorphism categoryValue (morphismIndex, morphismValue) =
  CategoryMorphism
    ("morphism-" <> show morphismIndex)
    (morphismKind morphismValue)
    <$> projectedEndpoint MorphismSource source
    <*> projectedEndpoint MorphismTarget target
  where
    projectedEndpoint endpoint endpointProjection =
      objectIdentifier
        <$> first
          (const (ExhibitMorphismEndpointUnavailable endpoint morphismIndex))
          (endpointProjection categoryValue morphismValue)

validateCategoryEnumeration ::
  DCELComplex ->
  ComplexCat DCELComplex ->
  Either ExhibitError ()
validateCategoryEnumeration complexValue categoryValue =
  let morphisms = enumerateMorphisms categoryValue
      expectedInventory =
        MorphismInventory
          { identityMorphismCount =
              length (vertices complexValue)
                + length (edges complexValue)
                + length (faces complexValue),
            faceEdgeMorphismCount =
              sum (fmap (length . faceBoundary complexValue) (faces complexValue)),
            edgeVertexMorphismCount =
              sum (fmap (edgeEndpointCount complexValue) (edges complexValue)),
            faceVertexMorphismCount =
              sum (fmap (incidenceFlagCount complexValue) (faces complexValue))
          }
      observedInventory =
        foldMap morphismInventory morphisms
   in if observedInventory == expectedInventory
        then Right ()
        else Left (ExhibitCategoryEnumerationMismatch expectedInventory observedInventory)

edgeEndpointCount :: DCELComplex -> Edge DCELComplex -> Int
edgeEndpointCount complexValue edgeValue =
  let (sourceVertex, targetVertex) = edgeBoundary complexValue edgeValue
   in length (nubOrd [sourceVertex, targetVertex])

incidenceFlagCount :: DCELComplex -> Face DCELComplex -> Int
incidenceFlagCount complexValue faceValue =
  sum
    ( fmap
        (edgeEndpointCount complexValue . orientedEdge)
        (faceBoundary complexValue faceValue)
    )

morphismInventory :: ComplexMor DCELComplex -> MorphismInventory
morphismInventory morphismValue =
  case morphismValue of
    IdentityMor _ -> MorphismInventory 1 0 0 0
    FaceToEdge {} -> MorphismInventory 0 1 0 0
    EdgeToVertex _ _ -> MorphismInventory 0 0 1 0
    FaceToVertex {} -> MorphismInventory 0 0 0 1
