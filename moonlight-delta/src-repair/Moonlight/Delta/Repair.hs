{-# LANGUAGE DerivingStrategies #-}

{-|
Bounded obstruction repair. A repair @Kernel@ checks obstructions, turns
repairable ones into corrections, and applies those corrections under explicit
fuel. The terminal cases are @ResultConverged@, @ResultStuck@, and
@ResultBudgetExhausted@.

> import Data.List.NonEmpty (NonEmpty ((:|)))
> import Moonlight.Delta.Repair qualified as Repair
>
> data TooSmall = TooSmall Int
> data Increment = Increment
>
> incrementKernel :: Int -> Repair.Kernel Int TooSmall Increment
> incrementKernel target =
>   Repair.Kernel
>     { Repair.check = \state ->
>         if state >= target
>           then Repair.StepConverged state
>           else Repair.StepObstructed state (TooSmall target :| [])
>     , Repair.residuate = \_ -> Just Increment
>     , Repair.applyKernelCorrection = \state Increment -> state + 1
>     }
>
> repaired :: Repair.Result Int TooSmall
> repaired = Repair.boundedRepair (incrementKernel 2) (Repair.Config 4) 0

The result is @ResultConverged 2 2@. Compose kernels with @sequenceRepair@,
@productRepair@, or @focusRepair@.
-}
module Moonlight.Delta.Repair
  ( Kernel (..),
    Step (..),
    Config (..),
    Result (..),
    Trace (..),
    Round,
    Correction (..),
    boundedRepair,
    boundedRepairTraced,
    sequenceRepair,
    productRepair,
    focusRepair,
    identityRepair,
    mapRepair,
    corrections,
    obstructions,
    applied,
    appliedCorrections,
    irreducible,
    roundsUsed,
    isConverged,
    resultState,
  )
where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (maybeToList)
import Numeric.Natural (Natural)
import Prelude

type Kernel :: Type -> Type -> Type -> Type
data Kernel state obstruction correction = Kernel
  { check :: state -> Step state obstruction,
    residuate :: obstruction -> Maybe correction,
    applyKernelCorrection :: state -> correction -> state
  }

type Step :: Type -> Type -> Type
data Step state obstruction
  = StepConverged state
  | StepObstructed state (NonEmpty obstruction)
  deriving stock (Eq, Show)

type Config :: Type
data Config = Config
  { maxRounds :: Natural
  }
  deriving stock (Eq, Ord, Show)

-- | A repair run report, not a certificate. Every judgment it carries is
-- relative to the caller-supplied 'Kernel': 'ResultConverged'
-- means the kernel's own check accepted the state, so the constructors stay
-- public — fabricating a result asserts nothing that supplying a vacuous
-- kernel could not already assert. Truthfulness relative to a given kernel is
-- the post-condition of 'boundedRepair', never an invariant of this type.
-- 'ResultBudgetExhausted' retains both the terminal obstruction set and its
-- ordered irreducible subset; budget exhaustion describes the run, not every
-- obstruction in that run.
type Result :: Type -> Type -> Type
data Result state obstruction
  = ResultConverged state Natural
  | ResultStuck state (NonEmpty obstruction) Natural
  | ResultBudgetExhausted state (NonEmpty obstruction) [obstruction] Natural
  deriving stock (Eq, Show)

-- | A round-by-round account of a repair run, in chronological order. Like
-- 'Result' it is a description relative to the kernel that produced it;
-- consumers may construct traces directly (an empty trace is the natural
-- seed for accumulation) because the type never promises the run happened.
type Trace :: Type -> Type -> Type
newtype Trace obstruction correction = Trace
  { rounds :: [Round obstruction correction]
  }
  deriving stock (Eq, Show)

type Round :: Type -> Type -> Type
newtype Round obstruction correction = Round
  { roundCorrections :: NonEmpty (Correction obstruction correction)
  }
  deriving stock (Eq, Show)

type Correction :: Type -> Type -> Type
data Correction obstruction correction
  = Applied obstruction correction
  | Irreducible obstruction
  deriving stock (Eq, Show)

type StepOutput :: Type -> Type -> Type -> Type
data StepOutput state obstruction correction
  = StepFinished !(Result state obstruction) !(Maybe (Round obstruction correction))
  | StepAdvanced !Natural !state !(Round obstruction correction)

roundsUsed :: Result state obstruction -> Natural
roundsUsed result =
  case result of
    ResultConverged _ rounds -> rounds
    ResultStuck _ _ rounds -> rounds
    ResultBudgetExhausted _ _ _ rounds -> rounds

isConverged :: Result state obstruction -> Bool
isConverged result =
  case result of
    ResultConverged _ _ -> True
    _ -> False

resultState :: Result state obstruction -> state
resultState result =
  case result of
    ResultConverged state _ -> state
    ResultStuck state _ _ -> state
    ResultBudgetExhausted state _ _ _ -> state

boundedRepair ::
  Kernel state obstruction correction ->
  Config ->
  state ->
  Result state obstruction
boundedRepair kernel config initialState =
  go 0 initialState
  where
    go currentRound state =
      case stepOutput kernel config currentRound state of
        StepFinished result _ ->
          result
        StepAdvanced nextRound nextState _ ->
          go nextRound nextState

boundedRepairTraced ::
  Kernel state obstruction correction ->
  Config ->
  state ->
  (Result state obstruction, Trace obstruction correction)
boundedRepairTraced kernel config initialState =
  go 0 initialState []
  where
    go currentRound state rounds =
      case stepOutput kernel config currentRound state of
        StepFinished result maybeRound ->
          (result, Trace (reverse (maybe rounds (: rounds) maybeRound)))
        StepAdvanced nextRound nextState roundValue ->
          go nextRound nextState (roundValue : rounds)

stepOutput ::
  Kernel state obstruction correction ->
  Config ->
  Natural ->
  state ->
  StepOutput state obstruction correction
stepOutput kernel config currentRound state
  | currentRound >= maxRounds config =
      case check kernel state of
        StepConverged convergedState ->
          StepFinished (ResultConverged convergedState currentRound) Nothing
        StepObstructed obstructedState obstructionValues ->
          let terminalIrreducibleObstructions =
                irreducible (roundFor kernel obstructionValues)
           in StepFinished
                ( ResultBudgetExhausted
                    obstructedState
                    obstructionValues
                    terminalIrreducibleObstructions
                    currentRound
                )
                Nothing
  | otherwise =
      case check kernel state of
        StepConverged convergedState ->
          StepFinished (ResultConverged convergedState currentRound) Nothing
        StepObstructed obstructedState obstructionValues ->
          obstructedStepOutput kernel currentRound obstructedState obstructionValues

obstructedStepOutput ::
  Kernel state obstruction correction ->
  Natural ->
  state ->
  NonEmpty obstruction ->
  StepOutput state obstruction correction
obstructedStepOutput kernel currentRound obstructedState obstructionValues =
  case roundDeltas roundValue of
    [] ->
      StepFinished (ResultStuck obstructedState obstructionValues currentRound) (Just roundValue)
    deltas ->
      StepAdvanced
        (currentRound + 1)
        (foldl' (applyKernelCorrection kernel) obstructedState deltas)
        roundValue
  where
    roundValue =
      roundFor kernel obstructionValues

roundFor ::
  Kernel state obstruction correction ->
  NonEmpty obstruction ->
  Round obstruction correction
roundFor kernel obstructionValues =
  Round correctionsValue
  where
    correctionsValue =
      fmap (correctionFor kernel) obstructionValues

corrections :: Round obstruction correction -> NonEmpty (Correction obstruction correction)
corrections =
  roundCorrections

obstructions :: Round obstruction correction -> NonEmpty obstruction
obstructions =
  fmap correctionObstruction . roundCorrections

applied :: Round obstruction correction -> Natural
applied =
  fromIntegral . length . roundDeltas

appliedCorrections :: Round obstruction correction -> [correction]
appliedCorrections =
  roundDeltas

irreducible :: Round obstruction correction -> [obstruction]
irreducible =
  roundIrreducibleFromCorrections . roundCorrections

correctionObstruction :: Correction obstruction correction -> obstruction
correctionObstruction correction =
  case correction of
    Applied obstruction _ -> obstruction
    Irreducible obstruction -> obstruction

correctionFor ::
  Kernel state obstruction correction ->
  obstruction ->
  Correction obstruction correction
correctionFor kernel obstruction =
  case residuate kernel obstruction of
    Nothing -> Irreducible obstruction
    Just correction -> Applied obstruction correction

roundDeltas :: Round obstruction correction -> [correction]
roundDeltas =
  roundDeltasFromCorrections . roundCorrections

roundDeltasFromCorrections :: NonEmpty (Correction obstruction correction) -> [correction]
roundDeltasFromCorrections =
  concatMap (maybeToList . correctionDelta) . NonEmpty.toList

correctionDelta :: Correction obstruction correction -> Maybe correction
correctionDelta correction =
  case correction of
    Applied _ delta -> Just delta
    Irreducible _ -> Nothing

roundIrreducibleFromCorrections :: NonEmpty (Correction obstruction correction) -> [obstruction]
roundIrreducibleFromCorrections =
  concatMap (maybeToList . correctionIrreducible) . NonEmpty.toList

correctionIrreducible :: Correction obstruction correction -> Maybe obstruction
correctionIrreducible correction =
  case correction of
    Applied _ _ -> Nothing
    Irreducible obstruction -> Just obstruction

sequenceRepair ::
  Kernel state obstruction1 correction1 ->
  Kernel state obstruction2 correction2 ->
  Kernel state (Either obstruction1 obstruction2) (Either correction1 correction2)
sequenceRepair kernel1 kernel2 =
  Kernel
    { check = \state ->
        case check kernel1 state of
          StepObstructed obstructedState obstructionValues ->
            StepObstructed obstructedState (fmap Left obstructionValues)
          StepConverged convergedState ->
            case check kernel2 convergedState of
              StepConverged finalState ->
                StepConverged finalState
              StepObstructed obstructedState obstructionValues ->
                StepObstructed obstructedState (fmap Right obstructionValues),
      residuate =
        either
          (fmap Left . residuate kernel1)
          (fmap Right . residuate kernel2),
      applyKernelCorrection = \state eitherDelta ->
        case eitherDelta of
          Left delta1 -> applyKernelCorrection kernel1 state delta1
          Right delta2 -> applyKernelCorrection kernel2 state delta2
    }

productRepair ::
  Kernel state obstruction1 correction1 ->
  Kernel state obstruction2 correction2 ->
  Kernel state (Either obstruction1 obstruction2) (Either correction1 correction2)
productRepair kernel1 kernel2 =
  Kernel
    { check = \state ->
        let leftStep = check kernel1 state
            leftState = stepState leftStep
            rightStep = check kernel2 leftState
            rightState = stepState rightStep
         in case (leftStep, rightStep) of
              (StepConverged _, StepConverged _) ->
                StepConverged rightState
              (StepObstructed _ obs1, StepConverged _) ->
                StepObstructed rightState (fmap Left obs1)
              (StepConverged _, StepObstructed _ obs2) ->
                StepObstructed rightState (fmap Right obs2)
              (StepObstructed _ obs1, StepObstructed _ obs2) ->
                StepObstructed rightState (fmap Left obs1 <> fmap Right obs2),
      residuate =
        either
          (fmap Left . residuate kernel1)
          (fmap Right . residuate kernel2),
      applyKernelCorrection = \state eitherDelta ->
        case eitherDelta of
          Left delta1 -> applyKernelCorrection kernel1 state delta1
          Right delta2 -> applyKernelCorrection kernel2 state delta2
    }

focusRepair ::
  (state -> focus) ->
  (state -> focus -> state) ->
  Kernel focus obstruction correction ->
  Kernel state obstruction correction
focusRepair extract embed innerKernel =
  Kernel
    { check = \state ->
        case check innerKernel (extract state) of
          StepConverged innerConvergedValue -> StepConverged (embed state innerConvergedValue)
          StepObstructed innerObstructedValue obstructionValues -> StepObstructed (embed state innerObstructedValue) obstructionValues,
      residuate = residuate innerKernel,
      applyKernelCorrection = \state delta ->
        embed state (applyKernelCorrection innerKernel (extract state) delta)
    }

identityRepair :: Kernel state obstruction correction
identityRepair =
  Kernel
    { check = StepConverged,
      residuate = const Nothing,
      applyKernelCorrection = const
    }

mapRepair ::
  (obstruction1 -> obstruction2) ->
  (obstruction2 -> obstruction1) ->
  (correction1 -> correction2) ->
  (correction2 -> correction1) ->
  Kernel state obstruction1 correction1 ->
  Kernel state obstruction2 correction2
mapRepair forwardObs backwardObs forwardDelta backwardDelta innerKernel =
  Kernel
    { check = \state ->
        case check innerKernel state of
          StepConverged convergedState ->
            StepConverged convergedState
          StepObstructed obstructedState obstructionValues ->
            StepObstructed obstructedState (fmap forwardObs obstructionValues),
      residuate =
        fmap forwardDelta . residuate innerKernel . backwardObs,
      applyKernelCorrection = \state delta ->
        applyKernelCorrection innerKernel state (backwardDelta delta)
    }

stepState :: Step state obstruction -> state
stepState step =
  case step of
    StepConverged state -> state
    StepObstructed state _ -> state
