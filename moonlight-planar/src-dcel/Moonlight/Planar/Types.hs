-- | The package vocabulary: sites and validated queries, the mode-indexed
-- triangulation, what construction returns, and the closed failure types.
module Moonlight.Planar.Types
  ( SiteRelation (..)
  , ElementDefaults (..)
  , unitElementDefaults
  , ConstraintMode (..)
  , KnownConstraintMode (..)
  , Triangulation
  , DelaunayTriangulation
  , ConstrainedDelaunayTriangulation
  , BuildResult (..)
  , InsertionDisposition (..)
  , InsertionResult (..)
  , BuildError (..)
  , Location (..)
  , LocationHint (..)
  , LocationStats (..)
  , emptyLocationStats
  , NearestStats (..)
  , RefinementParameters
  , defaultRefinementParameters
  , ClosureStats (..)
  , RefinementReceipt (..)
  , RefinementDomainResult (..)
  , RefinementResult (..)
  , InvariantViolation (..)
  , PlanarIncidenceError (..)
  ) where

import Moonlight.Planar.Internal.Representation
import Moonlight.Planar.Internal.Types
