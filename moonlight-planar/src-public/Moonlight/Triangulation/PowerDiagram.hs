-- | Exact regular topology and power cells. The complete labelled weighted-site
-- section is authoritative; visibility, incidence, and bounded planar layers
-- are derived views.
module Moonlight.Triangulation.PowerDiagram
  ( PowerWeight
  , PowerWeightError (..)
  , powerWeight
  , powerWeightFromExact
  , powerWeightExact
  , PowerSite
  , powerSite
  , powerSiteLabel
  , powerSitePosition
  , powerSiteExactPosition
  , powerSiteWeight
  , PowerCellDisposition (..)
  , RegularSiteDisposition (..)
  , RegularFace
  , regularFaceLabels
  , regularFaceDualPoint
  , PowerDualEdge (..)
  , RegularEdge
  , regularEdgeLabels
  , regularEdgeDual
  , RegularTriangulation
  , emptyRegularTriangulation
  , regularTriangulation
  , regularSite
  , regularSites
  , regularSiteCount
  , regularSiteDisposition
  , regularFaces
  , regularEdges
  , regularNeighbours
  , regularTriangulationReceipt
  , RegularTriangulationReceipt (..)
  , RegularSiteTransition (..)
  , RegularEditError (..)
  , RegularEditResult (..)
  , insertRegularSite
  , removeRegularSite
  , reweightRegularSites
  , BoundedPowerDiagram
  , boundedPowerDiagram
  , boundedPowerDiagramFromRegular
  , powerCellDisposition
  , powerCellDispositions
  , powerDiagramPlanarLayer
  , PowerDiagramError (..)
  , RegularTopologyError (..)
  , PowerDiagramReceipt (..)
  , powerDiagramInputSites
  , powerDiagramPeakIntermediateBitGrowth
  , powerDiagramFinalCoordinateBitGrowth
  , AffineForm (..)
  , UpperEnvelopeError (..)
  , upperEnvelope
  ) where

import Moonlight.Triangulation.Internal.PowerDiagram.Edit
import Moonlight.Triangulation.Internal.PowerDiagram.Generator (RegularTopologyError (..))
import Moonlight.Triangulation.Internal.PowerDiagram.Model
import Moonlight.Triangulation.Internal.PowerDiagram.Projection
import Moonlight.Triangulation.Internal.PowerDiagram.Section
