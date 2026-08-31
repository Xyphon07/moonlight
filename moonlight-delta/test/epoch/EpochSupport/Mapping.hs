module EpochSupport.Mapping where

import Data.IntSet (IntSet)
import Moonlight.Delta.Epoch
import EpochSupport.Types

mapProjectionInt ::
  (Int -> Int) ->
  ContextProjectionDelta IntSet ->
  ContextProjectionDelta IntSet
mapProjectionInt =
  mapContextProjectionDelta

mapProjectionGeneric ::
  (GenericKey -> GenericKey) ->
  ContextProjectionDelta GenericSet ->
  ContextProjectionDelta GenericSet
mapProjectionGeneric =
  mapContextProjectionDelta

mapViewInt ::
  (Int -> Int) ->
  ContextView IntSet section ->
  ContextView IntSet section
mapViewInt =
  mapContextViewKeys

mapViewGeneric ::
  (GenericKey -> GenericKey) ->
  ContextView GenericSet section ->
  ContextView GenericSet section
mapViewGeneric =
  mapContextViewKeys

identityInt :: Int -> Int
identityInt value =
  value

doubleInt :: Int -> Int
doubleInt value =
  value * 2

incrementInt :: Int -> Int
incrementInt value =
  value + 1

identityGenericKey :: GenericKey -> GenericKey
identityGenericKey key =
  key

genericDouble :: GenericKey -> GenericKey
genericDouble (GenericKey value) =
  GenericKey (value * 2)

genericIncrement :: GenericKey -> GenericKey
genericIncrement (GenericKey value) =
  GenericKey (value + 1)
