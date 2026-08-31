-- | The mutable solver arena: value cells plus pending-delta accumulators,
-- with seeding, delta bookkeeping, direct evaluation, and result extraction.
module Moonlight.Core.Fixpoint.Internal.Solver.Arena
  ( Arena (..),
    new,
    seed,
    seedDeltaM,
    evaluate,
    toResult,
    takePendingDelta,
    mergePendingDelta,
  )
where

import Control.Applicative.Free (runAp)
import Control.Monad.ST (ST)
import Data.Foldable (traverse_)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Vector qualified as Vector
import Data.Vector.Mutable qualified as MVector
import Moonlight.Core.Fixpoint.Internal.Solver.Types
  ( DeltaDomain (..),
    EquationId (..),
    EquationRead (..),
    Evaluation (..),
    Result (..),
    Snapshot (..),
  )
import Prelude

data Arena state value delta = Arena
  { values :: !(MVector.MVector state value),
    pending :: !(MVector.MVector state delta)
  }

new :: DeltaDomain value delta -> Snapshot value delta -> ST state (Arena state value delta)
new domain snapshot = do
  values <- Vector.thaw (snapshotValues snapshot)
  pending <- MVector.replicate (Vector.length (snapshotValues snapshot)) (deltaEmpty domain)
  pure
    Arena
      { values = values,
        pending = pending
      }

seed :: DeltaDomain value delta -> Arena state value delta -> IntMap delta -> ST state ()
seed domain arena =
  traverse_ (uncurry (seedDeltaM domain arena)) . IntMap.toAscList

seedDeltaM :: DeltaDomain value delta -> Arena state value delta -> Int -> delta -> ST state Bool
seedDeltaM domain arena key deltaValue
  | deltaNull domain deltaValue = pure False
  | otherwise = do
      oldValue <- MVector.read (values arena) key
      let candidateValue = deltaApply domain deltaValue oldValue
          effectiveDelta = deltaBetween domain oldValue candidateValue
      if deltaNull domain effectiveDelta
        then pure False
        else do
          MVector.write (values arena) key candidateValue
          mergePendingDelta domain arena key effectiveDelta
          pure True

evaluate :: Arena state value delta -> Evaluation value result -> ST state result
evaluate arena (Evaluation program) =
  runAp (interpretRead arena) program

interpretRead :: Arena state value delta -> EquationRead value result -> ST state result
interpretRead arena (ReadEquationValue (EquationId key))
  = MVector.read (values arena) key

toResult :: Arena state value delta -> ST state (Result value delta)
toResult arena = do
  values <- Vector.freeze (values arena)
  pure
    Result
      { resultValues = values,
        resultSnapshot = Snapshot values
      }

takePendingDelta :: DeltaDomain value delta -> Arena state value delta -> Int -> ST state delta
takePendingDelta domain arena key = do
  deltaValue <- MVector.read (pending arena) key
  MVector.write (pending arena) key (deltaEmpty domain)
  pure deltaValue

mergePendingDelta :: DeltaDomain value delta -> Arena state value delta -> Int -> delta -> ST state ()
mergePendingDelta domain arena key deltaValue = do
  oldPending <- MVector.read (pending arena) key
  MVector.write (pending arena) key (deltaMerge domain oldPending deltaValue)
