-- | A simple FIFO 'Queue' backed by 'Data.Sequence.Seq', with the usual
-- enqueue/dequeue operations.
module Moonlight.Core.Queue
  ( Queue,
    emptyQueue,
    enqueue,
    enqueueAll,
    dequeue,
    queueFromList,
    queueToList,
    queueNull,
  )
where

import Data.Bool (Bool)
import Data.Eq (Eq)
import Data.Foldable qualified as Foldable
import Data.Kind (Type)
import Data.Maybe (Maybe (..))
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq
import Prelude (Show, flip, (.))

type Queue :: Type -> Type
newtype Queue a = Queue (Seq a)
  deriving stock (Eq, Show)

emptyQueue :: Queue a
emptyQueue = Queue Seq.empty

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue s) = Queue (s |> x)

enqueueAll :: Foldable.Foldable t => t a -> Queue a -> Queue a
enqueueAll xs q = Foldable.foldl' (flip enqueue) q xs

dequeue :: Queue a -> Maybe (a, Queue a)
dequeue (Queue s) = case Seq.viewl s of
  Seq.EmptyL -> Nothing
  x Seq.:< rest -> Just (x, Queue rest)

queueFromList :: [a] -> Queue a
queueFromList = Queue . Seq.fromList

queueToList :: Queue a -> [a]
queueToList (Queue s) = Foldable.toList s

queueNull :: Queue a -> Bool
queueNull (Queue s) = Seq.null s
