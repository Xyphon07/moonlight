{-| Coboundary-construction obstructions and nilpotence evidence. -}
module Moonlight.Pale.Diagnostic.Topology.Cohomology
  ( CoboundaryConstructionError (..),
    CoboundaryNilpotenceEvidence (..),
    evidenceNilpotent,
  )
where

import Data.Kind (Type)
import Moonlight.Pale.Diagnostic.Topology.Boundary (BoundaryIncidenceShapeError)
import Prelude (Bool (False, True), Eq, Int, Read, Show, String)

type CoboundaryConstructionError :: Type
data CoboundaryConstructionError
  = CoboundaryBoundaryShapeError BoundaryIncidenceShapeError
  | CoboundaryMiddleBasisCardinalityMismatch Int Int
  | CoboundaryMiddleBasisCellMismatch Int
  | CoboundaryOperatorBuildError String
  deriving stock (Eq, Show, Read)

type CoboundaryNilpotenceEvidence :: Type
data CoboundaryNilpotenceEvidence
  = SingleContextNilpotent
  | SingleContextNonNilpotent
  | MultiContextNilpotent
  | MultiContextNonNilpotent
  | CoboundaryConstructionFailed CoboundaryConstructionError
  deriving stock (Eq, Show, Read)

evidenceNilpotent :: CoboundaryNilpotenceEvidence -> Bool
evidenceNilpotent evidenceValue =
  case evidenceValue of
    SingleContextNilpotent -> True
    SingleContextNonNilpotent -> False
    MultiContextNilpotent -> True
    MultiContextNonNilpotent -> False
    CoboundaryConstructionFailed _ -> False
