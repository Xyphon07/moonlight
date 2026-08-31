-- | Payloads that can be glued coordinate-wise when triangulation site sets
-- overlap. The laws are the contract: implementations must be commutative,
-- associative, and idempotent.
module Moonlight.Triangulation.JoinSemilattice
  ( JoinSemilattice (..)
  ) where

-- | A join-semilattice carried by vertex annotations.
--
-- @
-- joinAnnotations left right == joinAnnotations right left
-- joinAnnotations (joinAnnotations a b) c == joinAnnotations a (joinAnnotations b c)
-- joinAnnotations value value == value
-- @
class Eq annotation => JoinSemilattice annotation where
  joinAnnotations :: annotation -> annotation -> annotation

instance JoinSemilattice () where
  joinAnnotations _ _ = ()
  {-# INLINE joinAnnotations #-}
