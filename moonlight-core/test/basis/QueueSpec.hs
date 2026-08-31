{-# LANGUAGE DerivingStrategies #-}

module QueueSpec (tests) where

import Moonlight.Core (IsLawName (..), constructorLawName)
import Moonlight.Core
  ( dequeue,
    emptyQueue,
    enqueue,
    enqueueAll,
    queueFromList,
    queueNull,
    queueToList,
  )
import LawProperty (lawProperty)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, (===), (.&&.))

data QueueLaw
  = QueueProjectionRoundTrip
  | QueueEnqueuePreservesTailOrder
  | QueueEnqueueAllPreservesInputOrder
  | QueueDequeuePreservesFifoOrder
  | QueueNullCoherentWithProjection
  | QueueDequeueSizeCoherent
  deriving stock (Bounded, Enum, Eq, Ord, Show)

instance IsLawName QueueLaw where
  lawNameText =
    constructorLawName . show

tests :: TestTree
tests =
  testGroup
    "Queue"
    [ lawProperty QueueProjectionRoundTrip propProjectionRoundTrip,
      lawProperty QueueEnqueuePreservesTailOrder propEnqueuePreservesTailOrder,
      lawProperty QueueEnqueueAllPreservesInputOrder propEnqueueAllPreservesInputOrder,
      lawProperty QueueDequeuePreservesFifoOrder propDequeuePreservesFifoOrder,
      lawProperty QueueNullCoherentWithProjection propNullCoherentWithProjection,
      lawProperty QueueDequeueSizeCoherent propDequeueSizeCoherent
    ]

propProjectionRoundTrip :: [Int] -> Property
propProjectionRoundTrip values =
  queueToList (queueFromList values) === values

propEnqueuePreservesTailOrder :: Int -> [Int] -> Property
propEnqueuePreservesTailOrder value values =
  queueToList (enqueue value (queueFromList values)) === values <> [value]

propEnqueueAllPreservesInputOrder :: [Int] -> [Int] -> Property
propEnqueueAllPreservesInputOrder appendedValues initialValues =
  queueToList (enqueueAll appendedValues (queueFromList initialValues))
    === initialValues <> appendedValues

propDequeuePreservesFifoOrder :: [Int] -> Property
propDequeuePreservesFifoOrder values =
  case values of
    [] ->
      dequeue (queueFromList values) === Nothing
    first : rest ->
      dequeue (queueFromList values) === Just (first, queueFromList rest)

propNullCoherentWithProjection :: [Int] -> Property
propNullCoherentWithProjection values =
  queueNull queue === null values
    .&&. queueNull emptyQueue === True
  where
    queue =
      queueFromList values

propDequeueSizeCoherent :: [Int] -> Property
propDequeueSizeCoherent values =
  case dequeue (queueFromList values) of
    Nothing ->
      length values === 0
    Just (_, restQueue) ->
      length (queueToList restQueue) === length values - 1
