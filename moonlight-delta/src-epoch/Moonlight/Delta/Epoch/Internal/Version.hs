-- | The unbounded epoch version algebra.
module Moonlight.Delta.Epoch.Internal.Version
  ( Version,
    initialVersion,
    nextVersion,
    versionKey,
    versionFromKey,
  )
where

import Data.Kind (Type)
import Moonlight.Core (PartialOrder (..), totalOrderLeq)
import Prelude (Eq, Integer, Num ((+)), Ord, Show)

type Version :: Type
newtype Version = Version Integer
  deriving stock (Eq, Ord, Show)

initialVersion :: Version
initialVersion =
  Version 0

nextVersion :: Version -> Version
nextVersion (Version versionValue) =
  Version (versionValue + 1)

versionKey :: Version -> Integer
versionKey (Version versionValue) =
  versionValue

versionFromKey :: Integer -> Version
versionFromKey =
  Version

instance PartialOrder Version where
  leq =
    totalOrderLeq
