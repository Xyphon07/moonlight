-- | Finite downward-closed selections of relatively open vertices, edges, and
-- bounded faces. A selection retains its resident DCEL and exact coordinate
-- projection behind an opaque carrier.
module Moonlight.Triangulation.CellSet
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

import Moonlight.Triangulation.Internal.CellSet

