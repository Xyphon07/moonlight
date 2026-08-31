-- | Whole-support construction and persistent incremental insertion.
module Moonlight.Triangulation.BulkLoad
  ( empty
  , clear
  , delaunay
  , delaunayGeometry
  , DuplicatePayloadPolicy (..)
  , delaunayFromCoordinates
  , insert
  , insertAt
  , insertMany
  ) where

import Moonlight.Triangulation.Internal.BulkLoad
