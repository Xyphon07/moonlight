{-# LANGUAGE RankNTypes #-}

-- | The payload parameters' functorial and traversable structure: relabeling a
-- payload touches no geometry, so every law here holds for the reason that a
-- triangulation's points and its annotations are separate things.
module Moonlight.Triangulation.Payload
  ( PayloadTraversal
  , vertexPayloads
  , directedPayloads
  , undirectedPayloads
  , facePayloads
  , mapVertices
  , mapDirectedEdges
  , mapUndirectedEdges
  , mapFaces
  , overPayloads
  , foldPayloads
  , payloadList
  ) where

import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Monoid (Endo (..))
import Moonlight.Triangulation.Internal.Representation
  ( PayloadTraversal
  , directedPayloads
  , facePayloads
  , mapDirectedEdges
  , mapFaces
  , mapUndirectedEdges
  , mapVertices
  , undirectedPayloads
  , vertexPayloads
  )

-- | Relabel every payload a traversal reaches.
--
-- The 'mapVertices' family is this at each parameter and cheaper: a pure map
-- leaves an unmaterialized page unmaterialized, where a traversal must visit
-- every slot the page would have reported and so materializes it. Reach for
-- this one when the traversal is chosen at runtime, and for the named map when
-- the parameter is known where you stand.
overPayloads
  :: PayloadTraversal source target payload payload'
  -> (payload -> payload')
  -> source
  -> target
overPayloads traversal relabel = runIdentity . traversal (Identity . relabel)
{-# INLINE overPayloads #-}

-- | Summarize every payload a traversal reaches.
foldPayloads
  :: Monoid summary
  => PayloadTraversal source source payload payload
  -> (payload -> summary)
  -> source
  -> summary
foldPayloads traversal measure = getConst . traversal (Const . measure)
{-# INLINE foldPayloads #-}

-- | Every payload a traversal reaches, in visit order. Accumulated through
-- t'Endo' so the list is built by a right fold rather than by repeated append.
payloadList :: PayloadTraversal source source payload payload -> source -> [payload]
payloadList traversal source = appEndo (foldPayloads traversal (Endo . (:)) source) []
{-# INLINE payloadList #-}
