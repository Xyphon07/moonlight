-- | Closed alpha thresholds and resident component-boundary laws.
module Moonlight.Planar.AlphaSpec
  ( tests
  ) where

import Data.Foldable ( traverse_ )
import Moonlight.Planar.Alpha ( alphaShapeContainsFace )
import Moonlight.Planar.BoundaryFixtures ( requireComponentBoundary, assertLoopWinding,
  assertBoundaryShape )
import Moonlight.Planar.Dcel ( outerFace )
import Moonlight.Planar.FloodFillIterator ( RegionBoundary(regionBoundaryHoleLoops,
  regionBoundaryOuterLoop) )
import Moonlight.Planar.Handles.Iterators.FixedIterators ( innerFaces )
import Moonlight.Planar.MeshFixtures ( pointMeshOf )
import Moonlight.Planar.Point (Point(Point))
import Moonlight.Planar.Scalar (mkRadiusSquared, NonFiniteValue(ValueNegativeInfinity,
  ValueNaN, ValuePositiveInfinity), RadiusSquaredError(NonFiniteRadiusSquared,
  NegativeRadiusSquared))
import Support ( assertEqual, requireRight )


tests :: IO ()
tests =
  sequence_
    [ testAlphaFaceFiltration
    ]

testAlphaFaceFiltration :: IO ()
testAlphaFaceFiltration = do
  triangle <-
    pointMeshOf
      "alpha right triangle"
      [Point 0 0, Point 2 0, Point 0 2]
  face <- case innerFaces triangle of
    [singleFace] -> pure singleFace
    faces -> fail ("alpha fixture faces: " <> show faces)
  exactThreshold <- requireRight "exact alpha threshold" (mkRadiusSquared 2)
  lowerThreshold <- requireRight "lower alpha threshold" (mkRadiusSquared 1.999)
  zeroThreshold <- requireRight "zero alpha threshold" (mkRadiusSquared 0)
  assertEqual "zero alpha threshold excludes face" False
    (alphaShapeContainsFace zeroThreshold triangle face)
  assertEqual "closed alpha threshold includes equality" True (alphaShapeContainsFace exactThreshold triangle face)
  assertEqual "lower alpha threshold excludes face" False (alphaShapeContainsFace lowerThreshold triangle face)
  assertEqual "outer face is absent from alpha filtration" False (alphaShapeContainsFace exactThreshold triangle outerFace)

  let roundedPoints =
        ( Point (-20) (-20)
        , Point (-19) (-13)
        , Point (-2) (-19)
        )
      (roundedFirst, roundedSecond, roundedThird) = roundedPoints
      roundedRadiusSquared = 84.5
  roundedTriangle <-
    pointMeshOf
      "rounded alpha equality"
      [roundedFirst, roundedSecond, roundedThird]
  roundedFace <- case innerFaces roundedTriangle of
    [singleFace] -> pure singleFace
    faces -> fail ("rounded alpha fixture faces: " <> show faces)
  roundedThreshold <-
    requireRight
      "rounded exact alpha threshold"
      (mkRadiusSquared roundedRadiusSquared)
  roundedLowerThreshold <-
    requireRight
      "rounded lower alpha threshold"
      (mkRadiusSquared (roundedRadiusSquared - encodeFloat 1 (-46)))
  assertEqual
    "closed alpha equality is exact"
    True
    (alphaShapeContainsFace roundedThreshold roundedTriangle roundedFace)
  assertEqual
    "one binary64 step below exact alpha equality is excluded"
    False
    (alphaShapeContainsFace roundedLowerThreshold roundedTriangle roundedFace)

  alphaBoundary <-
    requireComponentBoundary
      "alpha"
      True
      triangle
      (alphaShapeContainsFace exactThreshold triangle)
  assertBoundaryShape "alpha component" triangle 3 [] alphaBoundary

  annulus <-
    pointMeshOf
      "alpha annulus"
      (ringPoints 4 32 <> ringPoints 2 16 <> ringPoints 3 24)
  annulusThreshold <-
    requireRight "alpha annulus threshold" (mkRadiusSquared 0.4)
  annulusBoundary <-
    requireComponentBoundary
      "alpha annulus"
      True
      annulus
      (alphaShapeContainsFace annulusThreshold annulus)
  assertEqual
    "alpha annulus hole count"
    1
    (length (regionBoundaryHoleLoops annulusBoundary))
  assertLoopWinding
    "alpha annulus outer winding"
    GT
    annulus
    (regionBoundaryOuterLoop annulusBoundary)
  traverse_
    (assertLoopWinding "alpha annulus hole winding" LT annulus)
    (regionBoundaryHoleLoops annulusBoundary)
  traverse_
    (\(label, value, failure) ->
       assertEqual label (Left failure) (mkRadiusSquared value))
    [ ("negative alpha threshold refusal", -1, NegativeRadiusSquared (-1))
    , ("NaN alpha threshold refusal", 0 / 0, NonFiniteRadiusSquared ValueNaN)
    , ("positive-infinite alpha threshold refusal", 1 / 0, NonFiniteRadiusSquared ValuePositiveInfinity)
    , ("negative-infinite alpha threshold refusal", (-1) / 0, NonFiniteRadiusSquared ValueNegativeInfinity)
    ]

ringPoints :: Double -> Int -> [Point]
ringPoints radius count =
  [ Point (radius * cos angle) (radius * sin angle)
  | index <- [0 .. count - 1]
  , let angle = 2 * pi * fromIntegral index / fromIntegral count
  ]
