{-# LANGUAGE GHC2024 #-}

module Moonlight.FiniteLattice.Cover
  ( strictOrderPairs,
    coverPairs,
    upperCovers,
    lowerCovers,
    residentUpperCoverKeys,
    residentLowerCoverKeys,
  )
where

import Moonlight.FiniteLattice.Internal.Key
  ( ContextKey (..),
    contextKeySetToAscList,
  )
import Moonlight.FiniteLattice.Internal.Plan
  ( contextPlanLowerCoverKeys,
    contextPlanUpperCoverKeys,
    contextPlanUpperKeys,
  )
import Moonlight.FiniteLattice.Internal.Types
  ( ContextLattice (..),
    ContextLatticeLookupError (..),
    ResidentContext (..),
    ResidentContextKey,
    ResidentContextKeySet (..),
    contextKeyForMaybe,
    contextKeyFromResidentKey,
    contextValueForKey,
  )

-- | Every strict comparable pair, oriented @(lower, upper)@.
strictOrderPairs :: ContextLattice c -> [(c, c)]
strictOrderPairs lattice =
  [ ( contextValueForKey lattice lowerKey,
      contextValueForKey lattice (ContextKey upperOrdinal)
    )
  | lowerOrdinal <- [0 .. clSize lattice - 1],
    let lowerKey = ContextKey lowerOrdinal,
    upperOrdinal <-
      contextKeySetToAscList
        (contextPlanUpperKeys (clPlan lattice) lowerKey),
    upperOrdinal /= lowerOrdinal
  ]

-- | Hasse edges, oriented @(lower, upper)@.
coverPairs :: ContextLattice c -> [(c, c)]
coverPairs lattice =
  [ ( contextValueForKey lattice lowerKey,
      contextValueForKey lattice (ContextKey upperOrdinal)
    )
  | lowerOrdinal <- [0 .. clSize lattice - 1],
    let lowerKey = ContextKey lowerOrdinal,
    upperOrdinal <-
      contextKeySetToAscList
        (contextPlanUpperCoverKeys (clPlan lattice) lowerKey)
  ]

upperCovers ::
  Ord c =>
  ContextLattice c ->
  c ->
  Either (ContextLatticeLookupError c) [c]
upperCovers lattice lower = do
  lowerKey <- lookupContextKey lattice lower
  let upperKeySet = contextPlanUpperCoverKeys (clPlan lattice) lowerKey
  pure
    [ contextValueForKey lattice (ContextKey upperOrdinal)
    | upperOrdinal <-
        contextKeySetToAscList
          upperKeySet
    ]

lowerCovers ::
  Ord c =>
  ContextLattice c ->
  c ->
  Either (ContextLatticeLookupError c) [c]
lowerCovers lattice upper = do
  upperKey <- lookupContextKey lattice upper
  let lowerKeySet = contextPlanLowerCoverKeys (clPlan lattice) upperKey
  pure
    [ contextValueForKey lattice (ContextKey lowerOrdinal)
    | lowerOrdinal <-
        contextKeySetToAscList
          lowerKeySet
    ]

residentUpperCoverKeys ::
  ResidentContext s c ->
  ResidentContextKey s ->
  ResidentContextKeySet s
residentUpperCoverKeys (ResidentContext lattice) key =
  ResidentContextKeySet
    ( contextPlanUpperCoverKeys
        (clPlan lattice)
        (contextKeyFromResidentKey key)
    )

residentLowerCoverKeys ::
  ResidentContext s c ->
  ResidentContextKey s ->
  ResidentContextKeySet s
residentLowerCoverKeys (ResidentContext lattice) key =
  ResidentContextKeySet
    ( contextPlanLowerCoverKeys
        (clPlan lattice)
        (contextKeyFromResidentKey key)
    )

lookupContextKey ::
  Ord c =>
  ContextLattice c ->
  c ->
  Either (ContextLatticeLookupError c) ContextKey
lookupContextKey lattice contextValue =
  maybe
    (Left (ContextLatticeUnknownContext contextValue))
    Right
    (contextKeyForMaybe lattice contextValue)
