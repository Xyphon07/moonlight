-- | Exact regular topology and power cells. The complete labelled weighted-site
-- section is authoritative; visibility, incidence, and bounded planar layers
-- are derived views.
module Moonlight.Planar.PowerDiagram
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
  , PreparedPowerMassSection
  , PowerMassPolynomial (..)
  , PowerMassBoundary (..)
  , PowerMassObligation (..)
  , PowerMassCertificate
  , powerMassCertificateObligation
  , powerMassCertificateConstant
  , powerMassCertificateRate
  , PowerMassBound
  , powerMassBoundParameter
  , powerMassBoundCertificates
  , PowerMassInterval
  , powerMassIntervalLower
  , powerMassIntervalUpper
  , PowerMassError (..)
  , preparePowerMassSection
  , powerMassSectionWindow
  , powerMassSectionSites
  , powerMassSectionDirection
  , powerMassSectionInterval
  , powerMassSectionCertificates
  , powerMassSectionPolynomials
  , evaluatePowerMasses
  ) where

import Moonlight.Planar.Internal.PowerDiagram.Edit
import Moonlight.Planar.Internal.PowerDiagram.Generator
  ( PowerDualEdge (..)
  , RegularTopologyError (..)
  )
import Moonlight.Planar.Internal.PowerDiagram.Model
import Moonlight.Planar.Internal.PowerDiagram.Mass
import Moonlight.Planar.Internal.PowerDiagram.Projection
import Moonlight.Planar.Internal.PowerDiagram.Section
