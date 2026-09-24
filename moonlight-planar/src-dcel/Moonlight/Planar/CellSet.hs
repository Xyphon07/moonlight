-- | Finite downward-closed selections of relatively open vertices, edges, and
-- bounded faces. A selection retains shared geometry-free incidence and exact
-- coordinates for selected vertices behind an opaque carrier.
module Moonlight.Planar.CellSet
  ( ExactCellSet
  , CellSelectionError (..)
  , exactCellSet
  , closeFaceCellSet
  , exactCellSetVertexCount
  , exactCellSetEdgeCount
  , exactCellSetFaceCount
  , foldExactCellVertices
  , foldExactCellEdges
  , foldExactCellFaces
  ) where

import Moonlight.Planar.Internal.CellSet
