module Main (main) where

import Data.Bifunctor (first)
import qualified Data.Vector as Vector
import Moonlight.Triangulation.BulkLoad (delaunayGeometry)
import Moonlight.Triangulation.Dcel (numVertices)
import Moonlight.Triangulation.FloodFillIterator
  ( RectangleMetricError
  , verticesInRectangle
  )
import Moonlight.Triangulation.Handles.Iterators.DynamicIterators
  ( vertexHandles
  )
import Moonlight.Triangulation.Handles.Scoped
  ( scopedVertexPoint
  , scopedVertices
  , withScopedTriangulation
  )
import Moonlight.Triangulation.PointLocation (locatePoint)
import Moonlight.Triangulation.Session
  ( insertVertexAtNearVertex
  , removeAt
  , withSession
  )
import Moonlight.Triangulation.Types
  ( BuildError
  , InsertionDisposition (Inserted)
  , Location (OnVertex)
  , Point (..)
  , PointValidationError
  )
import Moonlight.Triangulation.Math (mkQueryPoint, orient2d)
import Moonlight.Triangulation.Validation (validateTriangulation)
import System.Exit (die)

data PublicComponentFailure
  = PublicBuildFailure !BuildError
  | PublicQueryFailure !PointValidationError
  | PublicRectangleFailure !RectangleMetricError
  | PublicSeedVertexMissing
  deriving stock (Show)

data PublicComponentSummary = PublicComponentSummary
  { residentVertices :: !Int
  , owningVertexHandles :: !Int
  , scopedVertexPoints :: !Int
  , rectangleVertices :: !Int
  , locatedResidentVertex :: !Bool
  , admittedPredicateCorrect :: !Bool
  , sessionInsertedVertex :: !Bool
  , sessionRemovedVertex :: !Bool
  , finalMeshValid :: !Bool
  }
  deriving stock (Eq, Show)

main :: IO ()
main =
  either
    (die . ("public component workflow refused: " <>) . show)
    assertExpected
    publicComponentWorkflow

publicComponentWorkflow :: Either PublicComponentFailure PublicComponentSummary
publicComponentWorkflow = do
  square <-
    first PublicBuildFailure
      ( delaunayGeometry
          ( Vector.fromList
              [ Point (-1) (-1)
              , Point 1 (-1)
              , Point 1 1
              , Point (-1) 1
              ]
          )
      )
  query <- first PublicQueryFailure (mkQueryPoint (Point (-1) (-1)))
  seed <- case locatePoint square query of
    OnVertex vertex -> Right vertex
    _ -> Left PublicSeedVertexMissing
  ((disposition, removed), restored, _) <-
    first PublicBuildFailure
      ( withSession square 1 $ do
          (_, inserted) <- insertVertexAtNearVertex seed (Point 0 0) ()
          removal <- removeAt (Point 0 0)
          pure (inserted, removal)
      )
  queryRight <- first PublicQueryFailure (mkQueryPoint (Point 1 (-1)))
  queryTop <- first PublicQueryFailure (mkQueryPoint (Point (-1) 1))
  inside <-
    first PublicRectangleFailure
      (verticesInRectangle restored (Point (-2) (-2)) (Point 2 2))
  pure
    PublicComponentSummary
      { residentVertices = numVertices restored
      , owningVertexHandles = length (vertexHandles restored)
      , scopedVertexPoints =
          withScopedTriangulation restored $ \scoped ->
            length (fmap (scopedVertexPoint scoped) (scopedVertices scoped))
      , rectangleVertices = length inside
      , locatedResidentVertex = case locatePoint restored query of
          OnVertex _ -> True
          _ -> False
      , admittedPredicateCorrect = orient2d query queryRight queryTop == GT
      , sessionInsertedVertex = disposition == Inserted
      , sessionRemovedVertex = maybe False (const True) removed
      , finalMeshValid = null (validateTriangulation restored)
      }

assertExpected :: PublicComponentSummary -> IO ()
assertExpected actual
  | actual == expected = putStrLn "public components: ok"
  | otherwise =
      die
        ( "public component summary mismatch; expected "
            <> show expected
            <> ", got "
            <> show actual
        )
 where
  expected =
    PublicComponentSummary
      { residentVertices = 4
      , owningVertexHandles = 4
      , scopedVertexPoints = 4
      , rectangleVertices = 4
      , locatedResidentVertex = True
      , admittedPredicateCorrect = True
      , sessionInsertedVertex = True
      , sessionRemovedVertex = True
      , finalMeshValid = True
      }
