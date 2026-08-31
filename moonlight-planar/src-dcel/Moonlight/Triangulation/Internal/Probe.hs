{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}

-- | Compile-time instrumentation switch. The bulk board is the measuring
-- instrument, so per-candidate probe counters must not exist on its path at
-- all: gating behind a flag the compiler can eliminate is the only honest way
-- to have both one sweep implementation and an undistorted measurement. The
-- flag is a type, not a value: at 'ProbeOff the counter type is @()@, so no
-- arithmetic on it can survive in the worker — erasure is a property of the
-- kind, not of the optimizer's mood. The instrumented entry instantiates the
-- same code at 'ProbeOn and pays the cold counter writes itself.
module Moonlight.Triangulation.Internal.Probe
  ( Probe (..)
  , ProbeCounter
  , KnownProbe (..)
  ) where

import Control.Monad.ST (ST)
import Data.Kind (Type)
import Moonlight.Triangulation.Internal.OperationState
  ( Counter
  , OperationState
  , addCounter
  )

-- | The instrumentation switch, promoted to a kind by @DataKinds@.
data Probe = ProbeOff | ProbeOn

-- | The counter a probe site threads. At 'ProbeOff there is nothing to
-- thread; at 'ProbeOn it is a strict @Int@.
type family ProbeCounter (probe :: Probe) = (counter :: Type) | counter -> probe where
  ProbeCounter 'ProbeOff = ()
  ProbeCounter 'ProbeOn = Int

class KnownProbe probe where
  probeZero :: ProbeCounter probe
  probeBump :: ProbeCounter probe -> ProbeCounter probe
  -- | Charge one finished counter to the operation's diagnostic cell. At
  -- 'ProbeOff this is @pure ()@ and disappears with the dictionary; at
  -- 'ProbeOn it is one cold vector write per drain or probe site, on the
  -- instrumented entry that asked for it.
  probeCharge :: OperationState s -> Counter -> ProbeCounter probe -> ST s ()

instance KnownProbe 'ProbeOff where
  probeZero = ()
  probeBump = id
  probeCharge _ _ _ = pure ()
  {-# INLINE probeZero #-}
  {-# INLINE probeBump #-}
  {-# INLINE probeCharge #-}

instance KnownProbe 'ProbeOn where
  probeZero = 0
  probeBump counter = counter + 1
  probeCharge operation counter value = addCounter operation counter value
  {-# INLINE probeZero #-}
  {-# INLINE probeBump #-}
  {-# INLINE probeCharge #-}
