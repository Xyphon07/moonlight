module Main (main) where

import Data.Bifunctor (first)
import qualified Data.Vector as Vector
import qualified Moonlight.Planar.AdmissionSpec as AdmissionSpec
import Moonlight.Planar.BulkLoad (delaunayGeometry)
import Moonlight.Planar.Dcel (numVertices)
import Moonlight.Planar.FloodFillIterator
  ( RectangleMetricError
  , verticesInRectangle
  )
import Moonlight.Planar.Handles.Iterators.DynamicIterators
  ( vertexHandles
  )
import Moonlight.Planar.Handles.Scoped
  ( scopedVertexPoint
  , scopedVertices
  , withScopedTriangulation
  )
import Moonlight.Planar.HintGenerator (buildHierarchyHint, hierarchyHint)
import Moonlight.Planar.PointLocation (locatePoint)
import Moonlight.Planar.Refinement
  ( refinementAdditionalVertexBudget
  , refinementExcludesOuterFaces
  , refinementMaximumArea
  , refinementMinimumArea
  , refinementPreservesConstraints
  , validateRefinementParameters
  , withAdditionalVertexBudget
  , withConstraintPreservation
  , withMaximumArea
  , withMinimumArea
  , withOuterFaceExclusion
  )
import Moonlight.Planar.Session
  ( insertVertexAtNearVertex
  , removeAt
  , withSession
  )
import Moonlight.Planar.Types (BuildError, defaultRefinementParameters, InsertionDisposition (Inserted), Location (OnVertex))
import Moonlight.Planar.Point (Point (..), PointValidationError)
import qualified Moonlight.Planar.Telemetry as Telemetry
import Moonlight.Planar.Point (mkQueryPoint)
import Moonlight.Planar.Math (orient2d)
import Moonlight.Planar.Validation (validateTriangulation)
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
  , hierarchyTelemetryNearestCorrect :: !Bool
  , sessionInsertedVertex :: !Bool
  , sessionRemovedVertex :: !Bool
  , finalMeshValid :: !Bool
  }
  deriving stock (Eq, Show)

main :: IO ()
main = do
  AdmissionSpec.tests
  assertRefinementParameterAPI
  either
    (die . ("public component workflow refused: " <>) . show)
    assertExpected
    publicComponentWorkflow

assertRefinementParameterAPI :: IO ()
assertRefinementParameterAPI =
  case do
    withAdditionalVertexBudget 32 defaultRefinementParameters
      >>= withMaximumArea 4
      >>= withMinimumArea 1
      >>= pure . withConstraintPreservation True
      >>= pure . withOuterFaceExclusion True
  of
    Left obstruction -> die ("checked refinement authoring refused: " <> show obstruction)
    Right parameters -> do
      case validateRefinementParameters parameters of
        Left obstruction -> die ("admitted refinement parameters failed validation: " <> show obstruction)
        Right () -> pure ()
      if refinementAdditionalVertexBudget parameters == Just 32
          && refinementMinimumArea parameters == Just 1
          && refinementMaximumArea parameters == Just 4
          && refinementPreservesConstraints parameters
          && refinementExcludesOuterFaces parameters
        then pure ()
        else die "checked refinement observations disagreed with their setters"
      case withMaximumArea 2 defaultRefinementParameters >>= withMinimumArea 3 of
        Left _ -> pure ()
        Right _ -> die "minimum area greater than maximum area was admitted"

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
  hierarchy <- first PublicBuildFailure (buildHierarchyHint 2 square)
  let hintedVertex = case hierarchyHint hierarchy query of
        Just (Telemetry.VertexHint vertex) -> Just vertex
        _ -> Nothing
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
      , hierarchyTelemetryNearestCorrect =
          hintedVertex /= Nothing
            && fmap fst (Telemetry.nearestNeighbor square hintedVertex query) == Just seed
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
      , hierarchyTelemetryNearestCorrect = True
      , sessionInsertedVertex = True
      , sessionRemovedVertex = True
      , finalMeshValid = True
      }
