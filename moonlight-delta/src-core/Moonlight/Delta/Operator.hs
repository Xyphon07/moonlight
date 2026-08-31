{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Stateful operators over 'Timed' streams: each step fails typed or yields the next state plus emissions; 'opFlush' ends the stream.
module Moonlight.Delta.Operator
  ( OpResult (..),
    Operator (..),
    noOutput,
    emitOnly,
    retimeOpResult,
  )
where

import Moonlight.Delta.Time
  ( Timed (..),
    retime,
  )
import Prelude (Either, Eq, Functor, Show, fmap)

data OpResult time st out = OpResult
  { orState :: !st,
    orEmit :: ![Timed time out]
  }
  deriving stock (Eq, Show, Functor)

data Operator time st input output err = Operator
  { opStep :: st -> Timed time input -> Either err (OpResult time st output),
    opFlush :: st -> Either err (OpResult time st output)
  }

noOutput :: st -> OpResult time st out
noOutput stateValue =
  OpResult
    { orState = stateValue,
      orEmit = []
    }

emitOnly :: st -> [Timed time out] -> OpResult time st out
emitOnly stateValue emitted =
  OpResult
    { orState = stateValue,
      orEmit = emitted
    }

retimeOpResult ::
  time ->
  OpResult time st out ->
  OpResult time st out
retimeOpResult eventTime result =
  result {orEmit = fmap (retime eventTime) (orEmit result)}
