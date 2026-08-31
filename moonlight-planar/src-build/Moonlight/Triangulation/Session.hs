-- | One sealed editing transaction: thaw once, compose edits, publish once.
module Moonlight.Triangulation.Session
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

import Moonlight.Triangulation.Internal.Session
