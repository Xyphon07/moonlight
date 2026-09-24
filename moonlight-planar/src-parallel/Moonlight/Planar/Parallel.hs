{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}

-- | Bounded concurrent interpretation of the canonical join tournament. This
-- layer owns effects only: pairing and pair-schedule semantics remain in the
-- pure build planner.
module Moonlight.Planar.Parallel
  ( unionsConcurrently
  ) where

import Control.Concurrent.Async (concurrently)
import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Data.List.NonEmpty (NonEmpty)
import Moonlight.Planar.Internal.Join
  ( joinNormalForm )
import Moonlight.Planar.Internal.Representation (Triangulation)
import Moonlight.Planar.Internal.Tournament
  ( TournamentPlan (..)
  , planTournament
  )
import Moonlight.Planar.Internal.Types (BuildError, ConstraintMode (Unconstrained))
import Moonlight.Algebra.Pure.Join (JoinSemilattice)

-- | Execute the same deterministic tournament as sequential @unions@, with
-- no more than the requested number of leaf computations live at once. A
-- non-positive request means one worker; requesting more workers than leaves
-- is harmless. Each child is fully evaluated before its parent becomes
-- runnable, so thunks do not smuggle unbounded work across the boundary.
unionsConcurrently
  :: (Eq annotation, JoinSemilattice annotation, NFData annotation)
  => Int
  -> NonEmpty (Triangulation 'Unconstrained annotation () () ())
  -> IO (Either BuildError (Triangulation 'Unconstrained annotation () () ()))
unionsConcurrently requestedWorkers operands =
  executeTournamentConcurrently
    (max 1 requestedWorkers)
    (planTournament operands)

executeTournamentConcurrently
  :: (Eq annotation, JoinSemilattice annotation, NFData annotation)
  => Int
  -> TournamentPlan (Triangulation 'Unconstrained annotation () () ())
  -> IO (Either BuildError (Triangulation 'Unconstrained annotation () () ()))
executeTournamentConcurrently workers tournament =
  case tournament of
    TournamentLeaf mesh -> Right <$> evaluate (force mesh)
    TournamentNode left right
      | workers <= 1 ->
            publishUnion
            =<< ((,)
                  <$> executeTournamentConcurrently 1 left
                  <*> executeTournamentConcurrently 1 right
                )
      | otherwise -> do
          let !leftLeaves = tournamentLeaves left
              !rightLeaves = tournamentLeaves right
              !available = min workers (leftLeaves + rightLeaves)
              !leftWorkers =
                max 1 (min leftLeaves (available * leftLeaves `quot` (leftLeaves + rightLeaves)))
              !rightWorkers = max 1 (available - leftWorkers)
          concurrently
            (executeTournamentConcurrently leftWorkers left)
            (executeTournamentConcurrently rightWorkers right)
            >>= publishUnion

publishUnion
  :: (Eq annotation, JoinSemilattice annotation, NFData annotation)
  => ( Either BuildError (Triangulation 'Unconstrained annotation () () ())
     , Either BuildError (Triangulation 'Unconstrained annotation () () ())
     )
  -> IO (Either BuildError (Triangulation 'Unconstrained annotation () () ()))
publishUnion (Left failure, _) = pure (Left failure)
publishUnion (Right _, Left failure) = pure (Left failure)
publishUnion (Right leftMesh, Right rightMesh) =
  evaluate (force (joinNormalForm leftMesh rightMesh))

tournamentLeaves :: TournamentPlan mesh -> Int
tournamentLeaves tournament =
  case tournament of
    TournamentLeaf _ -> 1
    TournamentNode left right -> tournamentLeaves left + tournamentLeaves right
{-# INLINE tournamentLeaves #-}
