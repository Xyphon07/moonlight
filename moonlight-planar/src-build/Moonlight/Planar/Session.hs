-- | One sealed editing transaction: thaw once, compose edits, publish once.
module Moonlight.Planar.Session
  ( RemovalOutcome (..)
  , Session
  , withSession
  , insertVertex
  , insertVertexAt
  , insertVertexAtNear
  , insertVertexAtNearVertex
  , removeAt
  , removeAtNear
  , removeManyAt
  , removeManyAtNear
  , refuse
  ) where

import Moonlight.Planar.Internal.Session
