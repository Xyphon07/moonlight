{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}

-- | A value stamped with its event time; 'retime' replaces the stamp and nothing else.
module Moonlight.Delta.Time
  ( Timed (..),
    retime,
  )
where

import Data.Kind (Type)
import Prelude (Eq, Functor, Ord, Read, Show)

type Timed :: Type -> Type -> Type
data Timed time value = Timed
  { timedAt :: !time,
    timedValue :: !value
  }
  deriving stock (Eq, Ord, Show, Read, Functor)

retime :: time -> Timed time value -> Timed time value
retime eventTime timed =
  timed {timedAt = eventTime}
