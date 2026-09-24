-- | Convex admission and publication laws at the exact-geometry owner.
module Moonlight.Planar.ConvexSpec (tests) where

import Data.Foldable (traverse_)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Moonlight.Planar.Convex
  ( ConvexError (..)
  , convexHullPolygon
  , convexPolygon
  , convexPolygonCentroid
  , convexPolygonFromLoop
  , convexPolygonFromRetained
  , convexPolygonPoints
  , convexPolygonRegion
  , reflectConvexPolygon
  )
import Moonlight.Planar.Exact (exactOrient2d, exactPoint, exactRetainedPolygon)
import Moonlight.Planar.Region
  ( RegionPointLocation (..)
  , exactLoop
  , regionPointLocation
  )
import Support (assertEqual, requireRight)

tests :: IO ()
tests = sequence_
  [ testConvexAdmission
  , testConvexHull
  , testRetainedPublication
  , testConvexReflection
  , putStrLn "convex: ok"
  ]

testConvexAdmission :: IO ()
testConvexAdmission = do
  let points = exactPoint 0 0 :| [exactPoint 3 0, exactPoint 3 2, exactPoint 0 2]
  polygon <- requireRight "convex rectangle" (convexPolygon points)
  rotated <- requireRight "rotated rectangle"
    (convexPolygon (exactPoint 3 2 :| [exactPoint 0 2, exactPoint 0 0, exactPoint 3 0]))
  assertEqual "convex admission canonicalizes rotation" polygon rotated
  loop <- requireRight "admitted rectangle loop" (exactLoop points)
  assertEqual "admitted loop strengthens without readmission" (Just polygon) (convexPolygonFromLoop loop)
  centroid <- requireRight "convex interior witness" (convexPolygonCentroid polygon)
  assertEqual "convex witness lies inside" RegionInterior (regionPointLocation (convexPolygonRegion polygon) centroid)
  case convexPolygon (exactPoint 0 0 :| [exactPoint 0 2, exactPoint 3 2, exactPoint 3 0]) of
    Left (ConvexNonConvexTurn _ LT) -> pure ()
    outcome -> fail ("clockwise convex cycle was not refused: " <> show outcome)
  let top = exactPoint 0 3
      lowerLeft = exactPoint (-2) (-3)
      right = exactPoint 3 1
      left = exactPoint (-3) 1
      lowerRight = exactPoint 2 (-3)
      star = top :| [lowerLeft, right, left, lowerRight]
  assertEqual "star has strictly positive local turns" (replicate 5 GT)
    [ exactOrient2d top lowerLeft right
    , exactOrient2d lowerLeft right left
    , exactOrient2d right left lowerRight
    , exactOrient2d left lowerRight top
    , exactOrient2d lowerRight top lowerLeft
    ]
  case convexPolygon star of
    Left (ConvexInvalidLoop _) -> pure ()
    outcome -> fail ("local turns must not admit a self-intersecting star: " <> show outcome)

testConvexHull :: IO ()
testConvexHull = do
  let points = exactPoint 0 0 :| [exactPoint 2 0, exactPoint 2 2, exactPoint 0 2]
  expected <- requireRight "canonical square hull" (convexPolygon points)
  traverse_
    (\permutation ->
       case NonEmpty.nonEmpty permutation of
         Nothing -> fail "a nonempty permutation became empty"
         Just submitted -> do
           hull <- requireRight "permuted convex hull" (convexHullPolygon submitted)
           assertEqual "convex hull is permutation invariant" expected hull)
    (List.permutations (NonEmpty.toList points))
  hull <- requireRight "hull discards interior points and repeated vertices"
    (convexHullPolygon (points <> (exactPoint 1 1 :| [exactPoint 0 0])))
  assertEqual "convex hull keeps only extreme points" expected hull
  let collinear = exactPoint 0 0 :| [exactPoint 1 0, exactPoint 2 0]
  assertEqual "degenerate hull is a convex error"
    (Left (ConvexHullDegenerate (NonEmpty.toList collinear)))
    (convexHullPolygon collinear)

testRetainedPublication :: IO ()
testRetainedPublication = do
  let rotated = exactPoint 2 2 :| [exactPoint 0 2, exactPoint 0 0, exactPoint 2 0]
  retained <- requireRight "retained convex polygon" (exactRetainedPolygon rotated)
  checked <- requireRight "checked retained polygon" (convexPolygon rotated)
  assertEqual "trusted retained publication agrees with checked admission"
    checked (convexPolygonFromRetained retained)

testConvexReflection :: IO ()
testConvexReflection = do
  triangle <- requireRight "asymmetric convex triangle"
    (convexPolygon (exactPoint 0 0 :| [exactPoint 3 0, exactPoint 1 2]))
  let reflected = reflectConvexPolygon triangle
  readmitted <- requireRight "reflection preserves counter-clockwise orientation"
    (convexPolygon (convexPolygonPoints reflected))
  assertEqual "reflected carrier remains admissible" reflected readmitted
  assertEqual "convex reflection is involutive" triangle (reflectConvexPolygon reflected)
