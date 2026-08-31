{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE RankNTypes #-}

module Moonlight.FiniteLattice.Internal.Topological
  ( SuccessorFold,
    topologicalOrder,
  )
where

import Data.Foldable qualified as Foldable
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet

-- | A non-allocating fold over the successors of a key.
type SuccessorFold =
  forall result.
  Int ->
  (Int -> result -> result) ->
  result ->
  result

-- | Deterministic Kahn topological sorting over dense integer keys.
topologicalOrder :: Int -> SuccessorFold -> Maybe [Int]
topologicalOrder size successors
  | size < 0 = Nothing
  | otherwise = consume initialInDegrees initialReady [] size
  where
    sources =
      [0 .. size - 1]

    successorList source =
      successors source (:) []

    validNonSelfTarget source target =
      target /= source && target >= 0 && target < size

    initialInDegrees :: IntMap.IntMap Int
    initialInDegrees =
      Foldable.foldl'
        ( \inDegrees source ->
            Foldable.foldl'
              (\counts target -> IntMap.insertWith (+) target 1 counts)
              inDegrees
              (filter (validNonSelfTarget source) (successorList source))
        )
        IntMap.empty
        sources

    initialReady =
      IntSet.fromAscList
        [ source
        | source <- sources,
          IntMap.findWithDefault 0 source initialInDegrees == 0
        ]

    consume inDegrees ready reverseOrder remaining
      | remaining == 0 = Just (reverse reverseOrder)
      | otherwise =
          case IntSet.minView ready of
            Nothing -> Nothing
            Just (source, readyWithoutSource) ->
              let (nextInDegrees, nextReady) =
                    releaseTargets
                      source
                      inDegrees
                      readyWithoutSource
                      (successorList source)
               in consume
                    nextInDegrees
                    nextReady
                    (source : reverseOrder)
                    (remaining - 1)

    releaseTargets source inDegrees ready =
      Foldable.foldl'
        ( \(currentInDegrees, currentReady) target ->
            if validNonSelfTarget source target
              then
                let nextDegree =
                      IntMap.findWithDefault 0 target currentInDegrees - 1
                    nextInDegrees =
                      IntMap.insert target nextDegree currentInDegrees
                 in ( nextInDegrees,
                      if nextDegree == 0
                        then IntSet.insert target currentReady
                        else currentReady
                    )
              else (currentInDegrees, currentReady)
        )
        (inDegrees, ready)
