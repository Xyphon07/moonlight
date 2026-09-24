module Moonlight.Planar.Exhibit.CategoryObservatory
  ( Exhibit,
    ExhibitError,
    buildExhibit,
    renderExhibitError,
  )
where

import Data.Bifunctor (first)
import Data.Vector qualified as Vector
import Moonlight.Homology.Pure.Topology.CellCategory (complexCategory)
import Moonlight.Homology.Pure.Topology.CellComplex (CellComplex2D (..))
import Moonlight.Planar.BulkLoad (delaunayGeometry)
import Moonlight.Planar.CellComplex
  ( DCELComplex,
    fromExactCellSet,
  )
import Moonlight.Planar.CellSet
  ( ExactCellSet,
    closeFaceCellSet,
    exactCellSetEdgeCount,
    exactCellSetFaceCount,
    exactCellSetVertexCount,
  )
import Moonlight.Planar.Exhibit.CategoryObservatory.Category
  ( projectCategory,
    validateCategoryEnumeration,
  )
import Moonlight.Planar.Exhibit.CategoryObservatory.Geometry (projectMesh)
import Moonlight.Planar.Exhibit.CategoryObservatory.Model
  ( CellInventory (..),
    Exhibit (..),
    ExhibitError (..),
    renderExhibitError,
  )
import Moonlight.Planar.Exhibit.CategoryObservatory.Nerve
  ( projectNerve,
    validateNerveCount,
  )
import Moonlight.Planar.Handles.Iterators.FixedIterators (innerFaces)
import Moonlight.Planar.Point (Point (..))

buildExhibit :: Either ExhibitError Exhibit
buildExhibit = do
  triangulation <- first ExhibitTriangulationBuildFailed (delaunayGeometry exhibitPoints)
  selectedCells <-
    first ExhibitCellSelectionFailed
      (closeFaceCellSet triangulation (innerFaces triangulation))
  complexValue <- first ExhibitCellComplexFailed (fromExactCellSet selectedCells)
  let categoryValue = complexCategory complexValue
  validateInventory selectedCells complexValue
  validateCategoryEnumeration complexValue categoryValue
  categoryProjection <- projectCategory triangulation categoryValue
  nerveProjection <- projectNerve categoryValue
  validateNerveCount complexValue nerveProjection
  meshProjection <- projectMesh triangulation complexValue
  pure
    Exhibit
      { schemaVersion = 2,
        exhibitTitle = "Triangulation, incidence category, and flag nerve",
        mesh = meshProjection,
        category = categoryProjection,
        nerve = nerveProjection
      }

validateInventory :: ExactCellSet -> DCELComplex -> Either ExhibitError ()
validateInventory cellSet complexValue =
  let expectedInventory =
        CellInventory
          { cellVertexCount = exactCellSetVertexCount cellSet,
            cellEdgeCount = exactCellSetEdgeCount cellSet,
            cellFaceCount = exactCellSetFaceCount cellSet
          }
      observedInventory =
        CellInventory
          { cellVertexCount = length (vertices complexValue),
            cellEdgeCount = length (edges complexValue),
            cellFaceCount = length (faces complexValue)
          }
   in if observedInventory == expectedInventory
        then Right ()
        else Left (ExhibitInventoryMismatch expectedInventory observedInventory)

exhibitPoints :: Vector.Vector Point
exhibitPoints =
  Vector.fromList
    [ Point 80 72,
      Point 222 48,
      Point 362 92,
      Point 52 208,
      Point 176 166,
      Point 310 190,
      Point 414 232,
      Point 112 330,
      Point 258 314,
      Point 380 354
    ]
