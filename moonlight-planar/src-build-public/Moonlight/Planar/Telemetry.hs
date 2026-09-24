-- | The operational layer beside the equational surface: the statistics the
-- engine reports about its own work, and the verbs that name a session, a
-- hint, or a query workspace. Nothing here is a function of the value alone,
-- which is why none of it sits on the equational surface; every name is a
-- re-export of its sublibrary owner, never a wrapper.
module Moonlight.Planar.Telemetry
  ( -- * Construction statistics — a triangulation is not a history of its
    -- construction; the history is read here, beside the value, and two
    -- results that differ only here are the same value
    buildStats
  , insertionStats
  , PublicationStats (..)
  , ClosureStats (..)

    -- * Constraint statistics — what recovery and seam joins cost; the
    -- constrained value they produced is observed on the surface
  , ConstraintBatchStats (..)
  , constraintBatchStats
  , constrainedExtensionBuildStats
  , constrainedSeamConstraintStats
  , constrainedSeamBuildStats
  , constrainedSeamPublicationStats

    -- * Refinement statistics — the receipt's work counters; every mesh
    -- projection of the receipt stays on the surface
  , refinementStats
  , refinementPublicationStats
  , refinementClosureStats

    -- * Location and query work — a hint preserves geometric support, not
    -- necessarily the orientation of an edge witness. The same hint retains
    -- the same oriented-witness semantics as the uninstrumented descent.
  , LocationHint (..)
  , LocationStats (..)
  , emptyLocationStats
  , locatePointWithHint
  , NearestStats (..)
  , nearestNeighbor

    -- * Sessions — many edits behind one sealed thaw/freeze boundary; the
    -- published value equals the fold of the persistent surface verbs
  , Session
  , withSession
  , insertVertex
  , insertVertexAt
  , insertVertexAtNear
  , insertVertexAtNearVertex
  , removeAt
  , removeAtNear
  , removeManyAt
  , removeManyAtNear
  , refuse

    -- * Incremental bulk insertion — the incremental route to the value
    -- 'Moonlight.Planar.delaunay' publishes; exposed for its cost,
    -- not for a different answer
  , insertMany

    -- * Hint-seeded interpolation — the same samples the surface's own
    -- weights would select, reached from a caller-supplied seed
  , interpolateNearest
  , interpolateBarycentric
  ) where

import Moonlight.Planar.BulkLoad (insertMany)
import Moonlight.Planar.Cdt
  ( ConstraintBatchStats (..)
  , constrainedExtensionBuildStats
  , constrainedSeamBuildStats
  , constrainedSeamConstraintStats
  , constrainedSeamPublicationStats
  , constraintBatchStats
  )
import Moonlight.Planar.Internal.Paged (PublicationStats (..))
import Moonlight.Planar.Interpolation
  ( interpolateBarycentric
  , interpolateNearest
  , nearestNeighbor
  )
import Moonlight.Planar.PointLocation (locatePointWithHint)
import Moonlight.Planar.Session
  ( Session
  , insertVertex
  , insertVertexAt
  , insertVertexAtNear
  , insertVertexAtNearVertex
  , refuse
  , removeAt
  , removeAtNear
  , removeManyAt
  , removeManyAtNear
  , withSession
  )
import Moonlight.Planar.Types
  ( LocationHint (..)
  , LocationStats (..)
  , NearestStats (..)
  , ClosureStats (..)
  , buildStats
  , emptyLocationStats
  , insertionStats
  , refinementPublicationStats
  , refinementStats
  , refinementClosureStats
  )
