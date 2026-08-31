{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Delta.Patch.Internal.Cell
  ( assertAbsent,
    insert,
    delete,
    replace,
    matchCell,
    cellBefore,
    cellAfter,
    cellBeforeEndpoint,
    cellAfterEndpoint,
    endpointToMaybe,
    cellFromEndpointPair,
    cellFromEndpoints,
    mapCell,
    traverseCell,
  )
where

import Prelude
import Moonlight.Delta.Patch.Internal.Types

assertAbsent :: CellPatch value
assertAbsent =
  AssertAbsent
{-# INLINE assertAbsent #-}

insert :: value -> CellPatch value
insert =
  Insert
{-# INLINE insert #-}

delete :: value -> CellPatch value
delete =
  Delete
{-# INLINE delete #-}

replace :: value -> value -> CellPatch value
replace =
  Replace
{-# INLINE replace #-}

matchCell ::
  result ->
  (value -> result) ->
  (value -> result) ->
  (value -> value -> result) ->
  CellPatch value ->
  result
matchCell onAssertAbsent onInsert onDelete onReplace cell =
  case cell of
    AssertAbsent ->
      onAssertAbsent
    Insert value ->
      onInsert value
    Delete value ->
      onDelete value
    Replace before after ->
      onReplace before after
{-# INLINE matchCell #-}

cellBefore :: CellPatch value -> Maybe value
cellBefore =
  matchCell Nothing (const Nothing) Just (\before _after -> Just before)
{-# INLINE cellBefore #-}

cellAfter :: CellPatch value -> Maybe value
cellAfter =
  matchCell Nothing Just (const Nothing) (\_before after -> Just after)
{-# INLINE cellAfter #-}

cellBeforeEndpoint :: CellPatch value -> Endpoint value
cellBeforeEndpoint cell =
  case cell of
    AssertAbsent ->
      EndpointAbsent
    Insert _after ->
      EndpointAbsent
    Delete before ->
      EndpointPresent before
    Replace before _after ->
      EndpointPresent before
{-# INLINE cellBeforeEndpoint #-}

cellAfterEndpoint :: CellPatch value -> Endpoint value
cellAfterEndpoint cell =
  case cell of
    AssertAbsent ->
      EndpointAbsent
    Insert after ->
      EndpointPresent after
    Delete _before ->
      EndpointAbsent
    Replace _before after ->
      EndpointPresent after
{-# INLINE cellAfterEndpoint #-}

endpointToMaybe :: Endpoint value -> Maybe value
endpointToMaybe endpoint =
  case endpoint of
    EndpointAbsent ->
      Nothing
    EndpointPresent value ->
      Just value
{-# INLINE endpointToMaybe #-}

cellFromEndpointPair ::
  Endpoint value ->
  Endpoint value ->
  CellPatch value
cellFromEndpointPair before after =
  case (before, after) of
    (EndpointAbsent, EndpointAbsent) ->
      AssertAbsent
    (EndpointAbsent, EndpointPresent value) ->
      Insert value
    (EndpointPresent value, EndpointAbsent) ->
      Delete value
    (EndpointPresent beforeValue, EndpointPresent afterValue) ->
      Replace beforeValue afterValue
{-# INLINE cellFromEndpointPair #-}

cellFromEndpoints ::
  Maybe value ->
  Maybe value ->
  CellPatch value
cellFromEndpoints before after =
  case (before, after) of
    (Nothing, Nothing) ->
      AssertAbsent
    (Nothing, Just value) ->
      Insert value
    (Just value, Nothing) ->
      Delete value
    (Just beforeValue, Just afterValue) ->
      Replace beforeValue afterValue
{-# INLINE cellFromEndpoints #-}

mapCell ::
  (value -> value') ->
  CellPatch value ->
  CellPatch value'
mapCell project =
  matchCell
    AssertAbsent
    (Insert . project)
    (Delete . project)
    (\before after -> Replace (project before) (project after))
{-# INLINE mapCell #-}

traverseCell ::
  Applicative effect =>
  (value -> effect value') ->
  CellPatch value ->
  effect (CellPatch value')
traverseCell project cell =
  case cell of
    AssertAbsent ->
      pure AssertAbsent
    Insert value ->
      Insert <$> project value
    Delete value ->
      Delete <$> project value
    Replace before after ->
      Replace <$> project before <*> project after
{-# INLINE traverseCell #-}
