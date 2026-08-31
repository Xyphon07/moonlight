{-| Connected-component and Betti-number profiles of a diagnostic nerve. -}
module Moonlight.Pale.Diagnostic.Topology.Homotopy
  ( NerveHomotopyProfile (..),
  )
where

import Data.Kind (Type)
import Prelude (Eq, Int, Read, Show)

type NerveHomotopyProfile :: Type
data NerveHomotopyProfile = NerveHomotopyProfile
  { nhpConnectedComponents :: Int,
    nhpBettiVector :: [Int]
  }
  deriving stock (Eq, Show, Read)
