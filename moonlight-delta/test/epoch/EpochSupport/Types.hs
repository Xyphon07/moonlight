{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module EpochSupport.Types where

import Data.Map.Strict (Map)
import Data.Set (Set)
import Moonlight.Delta.Epoch

newtype GenericKey = GenericKey Int
  deriving stock (Eq, Ord, Show)

type GenericMap = Map GenericKey GenericKey

type GenericSet = Set GenericKey

data EpochInput keyMap observed = EpochInput
  { eiSource :: !(Endpoint observed),
    eiTarget :: !(Endpoint observed),
    eiTransport :: !keyMap,
    eiRetired :: !observed,
    eiChanged :: !observed
  }
  deriving stock (Eq, Show)

data EpochDeltaCase keyMap observed = EpochDeltaCase
  { edcInput :: !(EpochInput keyMap observed),
    edcDelta :: !(EpochDelta keyMap observed)
  }
  deriving stock (Eq, Show)

data EpochPairCase keyMap observed = EpochPairCase
  { epcFirst :: !(EpochDelta keyMap observed),
    epcSecond :: !(EpochDelta keyMap observed)
  }
  deriving stock (Eq, Show)

data EpochChainCase keyMap observed = EpochChainCase
  { eccFirst :: !(EpochDelta keyMap observed),
    eccSecond :: !(EpochDelta keyMap observed),
    eccThird :: !(EpochDelta keyMap observed)
  }
  deriving stock (Eq, Show)
