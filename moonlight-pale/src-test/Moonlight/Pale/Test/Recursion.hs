{-| Recursion identity and interpreter-coherence predicates. -}
module Moonlight.Pale.Test.Recursion
  ( cataAfterAnaIdentity,
    interpreterCoherence,
  )
where

cataAfterAnaIdentity :: Eq seed => (seed -> recursive) -> (recursive -> seed) -> seed -> Bool
cataAfterAnaIdentity anamorphism catamorphism seed =
  catamorphism (anamorphism seed) == seed

interpreterCoherence :: Eq value => (seed -> recursive) -> (recursive -> value) -> (seed -> value) -> seed -> Bool
interpreterCoherence anamorphism interpretation seedInterpreter seed =
  seedInterpreter seed == interpretation (anamorphism seed)
