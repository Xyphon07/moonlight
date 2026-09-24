module Moonlight.Planar.Exhibit.Geometry
  ( ShapeError (..)
  , Triangle (..)
  , triangulateShape
  , solidObj
  ) where

import Data.Bifunctor (first)
import Data.Function (on)
import Data.List (groupBy, sortOn)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Vector as Vector
import Moonlight.Planar.Cdt (CdtError, boundedRegionFaces, constrainedDelaunay)
import Moonlight.Planar.Dcel (innerFaceVertices, vertexPoint)
import Moonlight.Planar.Handles.HandleDefs (FaceId)
import Moonlight.Planar.Point (Point (..))
import Moonlight.Planar.Types (buildTriangulation, unitElementDefaults)
import Text.Printf (printf)

data ShapeError = MeshRejected !CdtError | MissingTriangle !FaceId
  deriving stock (Show)

data Triangle = Triangle !Point !Point !Point

-- Concave outlines and holes share the native constrained-domain builder.
triangulateShape :: NonEmpty (NonEmpty Point) -> [Point] -> Either ShapeError [Triangle]
triangulateShape loops interior = do
  let boundaries = NonEmpty.toList loops
      offsets = scanl (+) 0 (NonEmpty.length <$> boundaries)
      constraints = concat (zipWith loopConstraints offsets boundaries)
      points = concatMap NonEmpty.toList boundaries <> interior
  result <- first MeshRejected $
    constrainedDelaunay unitElementDefaults (Vector.fromList points) (Vector.fromList constraints)
  let mesh = buildTriangulation result
  traverse
    (\face -> case innerFaceVertices mesh face of
      Nothing -> Left (MissingTriangle face)
      Just (a, b, c) -> Right (Triangle (vertexPoint mesh a) (vertexPoint mesh b) (vertexPoint mesh c)))
    (boundedRegionFaces mesh)

loopConstraints :: Int -> NonEmpty Point -> [(Int, Int)]
loopConstraints offset points =
  let indices = [offset .. offset + NonEmpty.length points - 1]
   in zip indices (drop 1 indices <> take 1 indices)

-- OBJ is a transport view. Coincident face vertices are welded on import;
-- only edges incident to one selected triangle receive a side wall.
solidObj :: String -> (Point -> Double) -> [Triangle] -> String
solidObj name halfThickness triangles =
  let oriented = orient <$> triangles
      boundary = concat . filter ((== 1) . length)
        . groupBy ((==) `on` edgeKey) . sortOn edgeKey
        $ concatMap triangleEdges oriented
   in "o " <> name <> "\n"
      <> foldMap surfaces oriented
      <> foldMap wall boundary
 where
  orient :: Triangle -> Triangle
  orient triangle@(Triangle a@(Point ax ay) b@(Point bx by) c@(Point cx cy))
    | (bx - ax) * (cy - ay) - (by - ay) * (cx - ax) >= 0 = triangle
    | otherwise = Triangle a c b

  triangleEdges :: Triangle -> [(Point, Point)]
  triangleEdges (Triangle a b c) = [(a, b), (b, c), (c, a)]

  edgeKey :: (Point, Point) -> ((Double, Double), (Double, Double))
  edgeKey (Point ax ay, Point bx by) =
    (min (ax, ay) (bx, by), max (ax, ay) (bx, by))

  surfaces :: Triangle -> String
  surfaces (Triangle a b c) = face (-1) [c, b, a] <> face 1 [a, b, c]

  face :: Double -> [Point] -> String
  face side points = foldMap (vertex side) points <> "f -3 -2 -1\n"

  wall :: (Point, Point) -> String
  wall (a, b) = vertex (-1) a <> vertex (-1) b <> vertex 1 b <> vertex 1 a
    <> "f -4 -3 -2 -1\n"

  vertex :: Double -> Point -> String
  vertex side point@(Point x y) = printf "v %.9f %.9f %.9f\n"
    (x / 750) (side * halfThickness point) (negate y / 750)
