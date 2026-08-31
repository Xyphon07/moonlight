module Kernels
  ( leqSweepWeight,
    residentLeqSweepWeight,
    residentJoinMeetKeySweepWeight,
    joinMeetSweepWeight,
    implicationSweepWeight,
    implicationKeySweepWeight,
    fixpointPairWeight,
  )
where

import Data.Bifunctor
  ( first,
  )
import Fixtures
  ( Shape,
    joinSeedStep,
    keys,
    meetSeedStep,
  )
import Moonlight.FiniteLattice.Core
  ( ContextLattice,
    joinContext,
    meetContext,
  )
import Moonlight.FiniteLattice.Fixpoint
  ( greatestContextFixpoint,
    leastContextFixpoint,
  )
import Moonlight.FiniteLattice.Heyting
  ( ContextHeyting,
    compileContextHeyting,
    impliesContext,
    residentHeytingBaseContext,
    residentImpliesKey,
    withResidentHeytingContext,
  )
import Moonlight.FiniteLattice.Resident
  ( ResidentContext,
    ResidentContextKey,
    residentContextElementKey,
    residentContextElements,
    residentContextKeyLeq,
    residentContextKeyOrdinal,
    residentContextKeys,
    residentJoinMeetKeys,
    withResidentContext,
  )

leqSweepWeight :: Int -> ContextLattice Int -> Int
leqSweepWeight _size lattice =
  withResidentContext lattice $ \contextValue ->
    residentLeqSweepWeight contextValue (residentContextKeys contextValue)

residentLeqSweepWeight ::
  ResidentContext s Int ->
  [ResidentContextKey s] ->
  Int
residentLeqSweepWeight contextValue contextKeys =
  length
    [ ()
    | leftKey <- contextKeys,
      rightKey <- contextKeys,
      residentContextKeyLeq contextValue leftKey rightKey
    ]

residentJoinMeetKeySweepWeight ::
  ResidentContext s Int ->
  [ResidentContextKey s] ->
  Either String Int
residentJoinMeetKeySweepWeight contextValue contextKeys =
  Right $
    sum
      [ let (joinKey, meetKey) = residentJoinMeetKeys contextValue leftKey rightKey
         in residentContextKeyOrdinal joinKey + residentContextKeyOrdinal meetKey
      | leftKey <- contextKeys,
        rightKey <- contextKeys
      ]

joinMeetSweepWeight :: Int -> ContextLattice Int -> Either String Int
joinMeetSweepWeight size lattice =
  fmap sum
    ( traverse
        joinMeetPairWeight
        [ (leftValue, rightValue)
        | leftValue <- keys size,
          rightValue <- keys size
        ]
    )
  where
    joinMeetPairWeight (leftValue, rightValue) = do
      joined <- first show (joinContext lattice leftValue rightValue)
      met <- first show (meetContext lattice leftValue rightValue)
      pure (joined + met)

implicationSweepWeight :: Int -> ContextLattice Int -> Either String Int
implicationSweepWeight size lattice = do
  heyting <- first show (compileContextHeyting lattice)
  fmap sum
    ( traverse
        (implicationPairWeight heyting)
        [ (leftValue, rightValue)
        | leftValue <- keys size,
          rightValue <- keys size
        ]
    )
  where
    implicationPairWeight :: ContextHeyting Int -> (Int, Int) -> Either String Int
    implicationPairWeight heyting (leftValue, rightValue) =
      first show (impliesContext heyting leftValue rightValue)

implicationKeySweepWeight :: ContextLattice Int -> Either String Int
implicationKeySweepWeight lattice = do
  heyting <- first show (compileContextHeyting lattice)
  pure $
    withResidentHeytingContext heyting $ \contextValue ->
      let baseContext = residentHeytingBaseContext contextValue
          contextKeys =
            residentContextElementKey <$> residentContextElements baseContext
       in sum
            [ residentContextKeyOrdinal (residentImpliesKey contextValue leftKey rightKey)
            | leftKey <- contextKeys,
              rightKey <- contextKeys
            ]

fixpointPairWeight :: Shape -> Int -> ContextLattice Int -> Either String (Int, Int)
fixpointPairWeight shape size lattice =
  (,)
    <$> first show (leastContextFixpoint lattice (joinSeedStep shape size))
    <*> first show (greatestContextFixpoint lattice (meetSeedStep shape size))
