-- | The package vocabulary: sites and validated queries, the mode-indexed
-- triangulation, what construction returns, and the closed failure types.
module Moonlight.Triangulation.Types
  ( Point (..)
  , SiteRelation (..)
  , QueryPoint
  , queryPointValue
  , PointValidationError (..)
  , HasPosition (..)
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
  , BuildStats (..)
  , emptyBuildStats
  , CoordinateError (..)
  , NonFiniteValue (..)
  , classifyNonFinite
  , RadiusSquared
  , RadiusSquaredError (..)
  , mkRadiusSquared
  , radiusSquaredValue
  , BuildError (..)
  , Location (..)
  , LocationHint (..)
  , LocationStats (..)
  , emptyLocationStats
  , NearestStats (..)
  , RefinementParameters (..)
  , defaultRefinementParameters
  , ValidationClosureStats (..)
  , RefinementReceipt (..)
  , RefinementDomainResult (..)
  , RefinementResult (..)
  , InvariantViolation (..)
  ) where

import Moonlight.Triangulation.Internal.Representation
import Moonlight.Triangulation.Internal.Types
