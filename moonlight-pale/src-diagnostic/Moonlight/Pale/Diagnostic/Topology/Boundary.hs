{-| Typed boundary-incidence shape obstructions. -}
module Moonlight.Pale.Diagnostic.Topology.Boundary
  ( BoundaryIncidenceShapeError (..),
  )
where

import Data.Kind (Type)
import Prelude (Eq, Int, Read, Show)

type BoundaryIncidenceShapeError :: Type
data BoundaryIncidenceShapeError
  = BoundaryIncidenceShapeMismatch Int Int Int Int
  | BoundaryIncidenceBlockShapeMismatch Int Int Int Int
  | BoundaryIncidenceEntryOutOfBounds Int Int Int Int
  | BoundaryIncidenceBasisLookupFailure Int Int
  deriving stock (Eq, Show, Read)
