-- | Exact axis-aligned candidate bounds for admitted region geometry. Bounds
-- prune impossible overlap obligations; exact predicates remain authoritative.
module Moonlight.Planar.Internal.Region.Bounds
  ( exactLoopBounds
  , componentBounds
  , regionBounds
  , overlappingPairs
  , overlappingOptionalPairs
  , overlappingPairsBetween
  , overlappingPredecessors
  ) where

import Data.List (sortOn, tails)
import qualified Data.List as List
import qualified Data.IntMap.Strict as IntMap
import Moonlight.Planar.Exact
  ( ExactBounds
  , exactPointsBounds
  , boundsUnion
  , boundsOverlap
  , boundsMinimumX
  , boundsMaximumX
  )
import Moonlight.Planar.Internal.Region.Types
  ( ExactLoop (..)
  , PlanarRegion (..)
  , PolygonComponent (..)
  , polygonOuterLoop
  )

exactLoopBounds :: ExactLoop -> ExactBounds
exactLoopBounds (ExactLoop points) = exactPointsBounds points

componentBounds :: PolygonComponent -> ExactBounds
componentBounds = exactLoopBounds . polygonOuterLoop

regionBounds :: PlanarRegion -> Maybe ExactBounds
regionBounds (PlanarRegion components) =
  case components of
    [] -> Nothing
    firstComponent : remaining ->
      Just
        ( List.foldl'
            (\bounds component -> boundsUnion bounds (componentBounds component))
            (componentBounds firstComponent)
            remaining
        )

overlappingPairs :: (value -> ExactBounds) -> [value] -> [(value, value)]
overlappingPairs boundsOf =
  overlappingIndexedValuePairs
    . zipWith (\index value -> (index, boundsOf value, value)) [0 ..]

overlappingOptionalPairs
  :: (value -> Maybe ExactBounds)
  -> [value]
  -> [(value, value)]
overlappingOptionalPairs boundsOf =
  overlappingIndexedValuePairs
    . concatMap
      (\(index, value) ->
         case boundsOf value of
           Nothing -> []
           Just bounds -> [(index, bounds, value)])
    . zip [0 ..]

overlappingPairsBetween
  :: (value -> ExactBounds)
  -> [value]
  -> [value]
  -> [(value, value)]
overlappingPairsBetween boundsOf leftValues rightValues =
  [ pair
  | (firstBounds, firstValue) : remaining <- tails ordered
  , (secondBounds, secondValue) <-
      takeWhile
        (\(bounds, _) -> boundsMinimumX bounds <= boundsMaximumX firstBounds)
        remaining
  , pair <- crossPair firstValue secondValue
  , boundsOverlap firstBounds secondBounds
  ]
 where
  ordered =
    sortOn
      (boundsMinimumX . fst)
      ( map
          (\value -> (either boundsOf boundsOf value, value))
          (map Left leftValues <> map Right rightValues)
      )
  crossPair :: Either value value -> Either value value -> [(value, value)]
  crossPair (Left leftValue) (Right rightValue) = [(leftValue, rightValue)]
  crossPair (Right rightValue) (Left leftValue) = [(leftValue, rightValue)]
  crossPair _ _ = []

-- | Each value together with only the earlier input values whose exact bounds
-- overlap it. This is the local cover used when a fold glues one component at
-- a time and needs the complete overlap section accumulated so far.
overlappingPredecessors
  :: (value -> ExactBounds)
  -> [value]
  -> [(value, [value])]
overlappingPredecessors boundsOf values =
  [ ( value
    , map snd
        ( sortOn fst
            (IntMap.findWithDefault [] index predecessorsByIndex)
        )
    )
  | (index, value) <- zip [0 ..] values
  ]
 where
  indexed = zipWith (\index value -> (index, boundsOf value, value)) [0 ..] values
  predecessorsByIndex =
    IntMap.fromListWith (<>)
      [ (rightIndex, [(leftIndex, leftValue)])
      | (leftIndex, rightIndex, leftValue, _) <- overlappingIndexedEntries indexed
      ]

-- The authoring path retains its nested result pair because the flattened
-- four-field candidate increased allocation on the registered 1,024-component
-- workload. The predecessor path below needs the indices after selection and
-- therefore carries the indexed specialization separately.
overlappingIndexedValuePairs
  :: [(Int, ExactBounds, value)]
  -> [(value, value)]
overlappingIndexedValuePairs indexed =
  map (\(_, _, pair) -> pair)
    ( sortOn (\(leftIndex, rightIndex, _) -> (leftIndex, rightIndex))
        [ if leftIndex <= rightIndex
            then (leftIndex, rightIndex, (leftValue, rightValue))
            else (rightIndex, leftIndex, (rightValue, leftValue))
        | (leftIndex, leftBounds, leftValue) : remaining <- tails ordered
        , (rightIndex, rightBounds, rightValue) <-
            takeWhile
              (\(_, bounds, _) -> boundsMinimumX bounds <= boundsMaximumX leftBounds)
              remaining
        , boundsOverlap leftBounds rightBounds
        ]
    )
 where
  ordered = sortOn (\(_, bounds, _) -> boundsMinimumX bounds) indexed

overlappingIndexedEntries
  :: [(Int, ExactBounds, value)]
  -> [(Int, Int, value, value)]
overlappingIndexedEntries indexed =
  sortOn
    (\(leftIndex, rightIndex, _, _) -> (leftIndex, rightIndex))
    [ if leftIndex <= rightIndex
        then (leftIndex, rightIndex, leftValue, rightValue)
        else (rightIndex, leftIndex, rightValue, leftValue)
    | (leftIndex, leftBounds, leftValue) : remaining <- tails ordered
    , (rightIndex, rightBounds, rightValue) <-
        takeWhile
          (\(_, bounds, _) -> boundsMinimumX bounds <= boundsMaximumX leftBounds)
          remaining
    , boundsOverlap leftBounds rightBounds
    ]
 where
  ordered = sortOn (\(_, bounds, _) -> boundsMinimumX bounds) indexed
