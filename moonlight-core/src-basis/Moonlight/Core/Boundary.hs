{-# LANGUAGE TypeFamilies #-}

-- | The 'BoundaryOps' class: overlap, restriction, compatibility and subsumption
-- of region boundaries via an associated overlap type.
module Moonlight.Core.Boundary
  ( BoundaryOps (..),
  )
where

import Data.Kind (Constraint, Type)
import Prelude (Bool, Either)

type BoundaryOps :: Type -> Constraint
class BoundaryOps boundary where
  type BoundaryOverlap boundary :: Type

  overlapBetweenBoundary :: boundary -> boundary -> BoundaryOverlap boundary
  restrictBoundaryRaw :: BoundaryOverlap boundary -> boundary -> boundary
  compatibleBoundaryRaw :: boundary -> boundary -> Either boundary boundary
  subsumesBoundaryRaw :: boundary -> boundary -> Bool
