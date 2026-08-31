-- | One deterministic balanced binary descent shared by every pure
-- associative schedule in the package.  Leaves retain their source values;
-- interpreters own only their binary operation.
module Moonlight.Triangulation.Internal.Tournament
  ( TournamentPlan (..)
  , planTournament
  , interpretTournament
  ) where

import Data.List.NonEmpty (NonEmpty (..))

data TournamentPlan value
  = TournamentLeaf !value
  | TournamentNode !(TournamentPlan value) !(TournamentPlan value)

planTournament :: NonEmpty value -> TournamentPlan value
planTournament = descendTournament . fmap TournamentLeaf

interpretTournament
  :: (value -> value -> Either failure value)
  -> TournamentPlan value
  -> Either failure value
interpretTournament combine tournament =
  case tournament of
    TournamentLeaf value -> Right value
    TournamentNode left right -> do
      leftValue <- interpretTournament combine left
      rightValue <- interpretTournament combine right
      combine leftValue rightValue

descendTournament :: NonEmpty (TournamentPlan value) -> TournamentPlan value
descendTournament (single :| []) = single
descendTournament plans = descendTournament (pairTournamentRound plans)

pairTournamentRound
  :: NonEmpty (TournamentPlan value)
  -> NonEmpty (TournamentPlan value)
pairTournamentRound (left :| right : rest) =
  TournamentNode left right :| pairTournamentTail rest
pairTournamentRound (single :| []) = single :| []

pairTournamentTail :: [TournamentPlan value] -> [TournamentPlan value]
pairTournamentTail (left : right : rest) =
  TournamentNode left right : pairTournamentTail rest
pairTournamentTail rest = rest
