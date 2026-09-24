{-# LANGUAGE DataKinds #-}

-- | Storage-shape fixtures shared only by serialization tests and measurements.
-- The persistent insertion is real public construction; flattening is an
-- independent test projection of the same logical value, never a wire owner.
module Moonlight.Planar.SerializationFixtures
  ( SerializationFixture
  , serializationFixtures
  , serializationGridFixtures
  ) where

import qualified Data.Vector as V
import Moonlight.Planar.Internal.BulkLoad (delaunay, insertAt)
import Moonlight.Planar.Internal.BoxedPaged
  ( BoxedPaged, boxedDefaulted, boxedFill, boxedFromVector, boxedPagedLength
  , boxedToVector, boxedUpdate
  )
import Moonlight.Planar.Internal.Paged (fromLocalVector, fromVector, pagedLength, toVector)
import Moonlight.Planar.Internal.Representation
  ( BuildResult (buildTriangulation), InsertionResult (insertionTriangulation)
  , Triangulation (..), mapVertices
  )
import Moonlight.Planar.Internal.Types (BuildError, ConstraintMode (Unconstrained), ElementDefaults (..))
import Moonlight.Planar.Point (Point (..))

type SerializationFixture = Triangulation 'Unconstrained Int Int Int Int

serializationFixtures :: Int -> Either BuildError [(String, SerializationFixture)]
serializationFixtures count =
  serializationFixturesForPoints
    (V.generate count (\index -> Point (fromIntegral index) 0))
    (Point (fromIntegral count) 0)

serializationGridFixtures :: Int -> Either BuildError [(String, SerializationFixture)]
serializationGridFixtures count =
  serializationFixturesForPoints
    (V.generate count (\index -> let (row, column) = index `quotRem` 128 in Point (fromIntegral column) (fromIntegral row)))
    (Point (-1) (-1))

serializationFixturesForPoints
  :: V.Vector Point
  -> Point
  -> Either BuildError [(String, SerializationFixture)]
serializationFixturesForPoints points arrival = do
  built <- delaunay (ElementDefaults 11 13 17) points
  let initial :: SerializationFixture
      initial = mapVertices (const 3) (buildTriangulation built)
      defaulted :: SerializationFixture
      defaulted =
        initial
          { triVertexData = boxedDefaulted 3 (pagedLength (triPointX initial))
          , triDirectedData = boxedDefaulted 11 (pagedLength (triHalfTopology initial) `quot` 4)
          , triUndirectedData = boxedDefaulted 13 (pagedLength (triConstraint initial))
          , triFaceData = boxedDefaulted 17 (pagedLength (triFaceEdge initial))
          }
      partiallyMaterialized :: SerializationFixture
      partiallyMaterialized =
        defaulted
          { triVertexData = writeBoundaryPayloads (triVertexData defaulted)
          , triDirectedData = writeBoundaryPayloads (triDirectedData defaulted)
          , triUndirectedData = writeBoundaryPayloads (triUndirectedData defaulted)
          , triFaceData = writeBoundaryPayloads (triFaceData defaulted)
          }
      materialized :: SerializationFixture
      materialized =
        defaulted
          { triVertexData = materialize (triVertexData defaulted)
          , triDirectedData = materialize (triDirectedData defaulted)
          , triUndirectedData = materialize (triUndirectedData defaulted)
          , triFaceData = materialize (triFaceData defaulted)
          }
  shared <- insertionTriangulation <$> insertAt partiallyMaterialized arrival 23
  shortTail <- insertionTriangulation <$> insertAt materialized arrival 23
  pure
    [ ("flat-dense", flatten materialized)
    , ("flat-vertex-dense", flatten initial)
    , ("flat-defaulted", flatten defaulted)
    , ("flat-partial", flatten partiallyMaterialized)
    , ("persistent-partial", shared)
    , ("persistent-materialized-tail", shortTail)
    ]
 where
  materialize :: BoxedPaged requirement Int -> BoxedPaged requirement Int
  materialize payloads = boxedFromVector (boxedFill payloads) (boxedToVector payloads)

  writeBoundaryPayloads :: BoxedPaged requirement Int -> BoxedPaged requirement Int
  writeBoundaryPayloads payloads
    | boxedPagedLength payloads == 0 = payloads
    | otherwise = boxedUpdate 0 101 (boxedUpdate (boxedPagedLength payloads - 1) 103 payloads)

  flatten :: SerializationFixture -> SerializationFixture
  flatten mesh =
    mesh
      { triPointX = fromLocalVector 0 (toVector (triPointX mesh))
      , triPointY = fromLocalVector 0 (toVector (triPointY mesh))
      , triVertexOut = fromLocalVector maxBound (toVector (triVertexOut mesh))
      , triHalfTopology = fromVector maxBound (toVector (triHalfTopology mesh))
      , triFaceEdge = fromLocalVector maxBound (toVector (triFaceEdge mesh))
      , triConstraint = fromVector 0 (toVector (triConstraint mesh))
      }
