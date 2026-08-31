-- | Stable deduplication and duplicate detection over lists, keyed by an
-- 'Ord'-projection.
module Moonlight.Core.Dedup
  ( dedupStableOn,
    duplicatesOrd,
    firstDuplicate,
    duplicateValuesOn,
  )
where

import Data.Map.Strict qualified as Map
import Data.Maybe (Maybe (Just, Nothing), catMaybes, listToMaybe)
import Data.Set qualified as Set
import Data.Traversable (mapAccumL)
import Prelude (Ord, fmap, id, snd, (.))

dedupStableOn :: Ord key => (value -> key) -> [value] -> [value]
dedupStableOn keyOf =
  catMaybes
    . snd
    . mapAccumL observe Set.empty
  where
    observe seen value =
      let valueKey = keyOf value
       in if Set.member valueKey seen
            then (seen, Nothing)
            else (Set.insert valueKey seen, Just value)

duplicatesOrd :: Ord value => [value] -> [value]
duplicatesOrd =
  Set.toAscList
    . Set.fromList
    . fmap snd
    . duplicateValuesOn id

firstDuplicate :: Ord value => [value] -> Maybe value
firstDuplicate =
  fmap snd . listToMaybe . duplicateValuesOn id

duplicateValuesOn :: Ord key => (value -> key) -> [value] -> [(value, value)]
duplicateValuesOn keyOf =
  catMaybes
    . snd
    . mapAccumL observe Map.empty
  where
    observe firstByKey value =
      let valueKey = keyOf value
       in case Map.lookup valueKey firstByKey of
            Nothing ->
              (Map.insert valueKey value firstByKey, Nothing)
            Just firstValue ->
              (firstByKey, Just (firstValue, value))
