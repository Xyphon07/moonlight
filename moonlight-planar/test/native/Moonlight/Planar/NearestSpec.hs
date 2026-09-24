-- | Exact nearest-site selection against an independent rational argmin.
module Moonlight.Planar.NearestSpec
  ( tests
  ) where

import Control.Monad ( when )
import Data.Foldable ( traverse_ )
import Data.Primitive.PrimArray ( indexPrimArray, primArrayFromList, sizeofPrimArray )
import Moonlight.Planar.BulkLoad ( delaunay )
import Moonlight.Planar.Cdt ( constrainedDelaunay, fromDelaunay )
import Moonlight.Planar.Dcel ( destination, innerFaceVertexTriples, isConstraintEdge,
  numConstraints, numVertices, undirectedEndpoints, vertexOutgoingEdges, vertexPoint )
import Moonlight.Planar.Handles.Iterators.FixedIterators ( undirectedEdges, vertices )
import Moonlight.Planar.Internal.HandleDefs ( VertexId(VertexId) )
import Moonlight.Planar.Interpolation ( nearestNeighbor )
import Moonlight.Planar.Point (canonicalPoint)
import Moonlight.Planar.Math (squaredDistance)
import Moonlight.Planar.MeshFixtures ( requirePointBuild, pointKey, edgeKeys )
import Moonlight.Planar.Scalar ( maximumAllowedCoordinate, minimumAllowedCoordinate )
import Moonlight.Planar.Point (QueryPoint, Point(Point), queryPointValue)
import Moonlight.Planar.Types (unitElementDefaults, NearestStats(nearestDistanceTests, nearestWalkSteps), BuildResult(buildTriangulation,
  buildInputVertices), Triangulation)
import Support ( assertEqual, assertValid, requireQueryPoint, requireRight, requireJust )
import Moonlight.Planar.Internal.Predicates qualified as Admitted
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Vector as V


tests :: IO ()
tests =
  sequence_
    [ testWideDistanceNearestNeighbor
    , testExactNearestNeighbor
    ]

testWideDistanceNearestNeighbor :: IO ()
testWideDistanceNearestNeighbor = do
  let largePoints = V.fromList
        [ Point 0 0
        , Point 1.0e20 0
        , Point 2.0e20 0
        , Point 1.0e20 1.0e19
        ] :: V.Vector (Point)
  largeBuild <- requireRight "wide-distance build" (delaunay unitElementDefaults largePoints)
  let largeTriangulation = buildTriangulation largeBuild
  assertValid "wide-distance triangulation" largeTriangulation
  query <- requireQueryPoint "wide-distance query" (Point 1.05e20 0)
  case nearestNeighbor largeTriangulation Nothing query of
    Nothing -> fail "wide-distance nearest-neighbor returned Nothing"
    Just (nearest, _) ->
      assertEqual "wide-distance nearest-neighbor" (Point 1.0e20 0) (vertexPoint largeTriangulation nearest)
  assertNearestMatchesExactArgmin "wide-distance" largeTriangulation query

testExactNearestNeighbor :: IO ()
testExactNearestNeighbor = do
  traverse_ checkConstrainedPermutation (List.permutations strictSites)
  traverse_ checkCocircularPermutation (List.permutations squareSites)
  tieBuild <- requirePointBuild "nearest handle trap" [squareD, squareB, squareA, squareC]
  assertNearestInputMapping "nearest handle trap" [squareD, squareB, squareA, squareC] tieBuild
  assertEqual "nearest handle trap retains D=0 B=1 A=2 C=3"
    (primArrayFromList [0, 1, 2, 3]) (buildInputVertices tieBuild)
  let tieMesh = buildTriangulation tieBuild
      tieSeed = VertexId (indexPrimArray (buildInputVertices tieBuild) 1)
  assertEqual "nearest handle trap retained AC" True (Set.member (pointKey squareA, pointKey squareC) (edgeKeys tieMesh))
  assertEqual "nearest handle trap B sees only larger handles"
    (Set.fromList [VertexId 2, VertexId 3])
    (Set.fromList (fmap (destination tieMesh) (vertexOutgoingEdges tieMesh tieSeed)))
  centerQuery <- requireQueryPoint "nearest handle trap center" (Point 1 1)
  assertNearestMatchesExactArgmin "nearest handle trap" tieMesh centerQuery
  traverse_ checkRoundedPermutation (List.permutations [roundedFarther, roundedNearer])
  traverse_ checkDomainFixture domainFixtures
  traverse_ checkIntegerRing
    [ integerRing
    , reverse integerRing
    , drop 3 integerRing <> take 3 integerRing
    ]
  hullFrom <- requireJust "nearest hull constraint from input" (List.elemIndex strictA strictSites)
  hullTo <- requireJust "nearest hull constraint to input" (List.elemIndex strictB strictSites)
  hullBuild <- requireRight "nearest hull constraint build" $
    constrainedDelaunay unitElementDefaults (V.fromList strictSites) (V.singleton (hullFrom, hullTo))
  let hullMesh = buildTriangulation hullBuild
  assertEqual "nearest hull constraint admitted" 1 (numConstraints hullMesh)
  strictQuery <- requireQueryPoint "nearest hull constraint query" (Point 17 12)
  assertNearestMatchesExactArgmin "nearest hull constraint" hullMesh strictQuery
 where
  strictA = Point 0 0
  strictB = Point 32 0
  strictC = Point 32 32
  strictD = Point 0 8
  strictSites = [strictA, strictB, strictC, strictD]
  squareA = Point 0 0
  squareB = Point 2 0
  squareC = Point 2 2
  squareD = Point 0 2
  squareSites = [squareA, squareB, squareC, squareD]
  roundedFarther = Point 1 (encodeFloat 1 (-27))
  roundedNearer = Point 1 0

  checkConstrainedPermutation points = do
    fromInput <- requireJust "nearest constrained from input" (List.elemIndex strictA points)
    toInput <- requireJust "nearest constrained to input" (List.elemIndex strictC points)
    built <- requireRight "nearest constrained trap build" $
      constrainedDelaunay unitElementDefaults (V.fromList points) (V.singleton (fromInput, toInput))
    assertNearestInputMapping "nearest constrained trap" points built
    let mesh = buildTriangulation built
        fromVertex = VertexId (indexPrimArray (buildInputVertices built) fromInput)
        toVertex = VertexId (indexPrimArray (buildInputVertices built) toInput)
        retainedConstraint =
          any
            (\edge ->
              isConstraintEdge mesh edge
                && uncurry (\from to -> Set.fromList [from, to] == Set.fromList [fromVertex, toVertex])
                  (undirectedEndpoints mesh edge)
            )
            (undirectedEdges mesh)
        triangleSupports =
          Set.fromList
            (fmap (\(a, b, c) -> Set.fromList (fmap (vertexPoint mesh) [a, b, c]))
              (V.toList (innerFaceVertexTriples mesh)))
    assertValid "nearest constrained trap" mesh
    assertEqual "nearest constrained trap admitted AC" (1, True) (numConstraints mesh, retainedConstraint)
    assertEqual "nearest constrained trap retained exactly ABC and ACD"
      (Set.fromList [Set.fromList [strictA, strictB, strictC], Set.fromList [strictA, strictC, strictD]])
      triangleSupports
    query <- requireQueryPoint "nearest constrained trap query" (Point 17 12)
    assertEqual "nearest constrained trap exact distances"
      [433, 369, 625, 305] (fmap (rationalSquaredDistance (queryPointValue query)) strictSites)
    seedInput <- requireJust "nearest constrained B input" (List.elemIndex strictB points)
    let seed = VertexId (indexPrimArray (buildInputVertices built) seedInput)
    assertEqual "nearest constrained B has no closer neighbour" True $
      all
        ((> rationalSquaredDistance (queryPointValue query) strictB)
          . rationalSquaredDistance (queryPointValue query)
          . vertexPoint mesh
          . destination mesh)
        (vertexOutgoingEdges mesh seed)
    case nearestNeighbor mesh (Just seed) query of
      Nothing -> fail "nearest constrained trap returned Nothing"
      Just (nearest, _) -> assertEqual "nearest constrained trap returns D" strictD (vertexPoint mesh nearest)
    assertNearestMatchesExactArgmin "nearest constrained trap" mesh query

  checkCocircularPermutation points = do
    built <- requirePointBuild "nearest cocircular square" points
    assertNearestInputMapping "nearest cocircular square" points built
    let mesh = buildTriangulation built
    assertValid "nearest cocircular square" mesh
    assertEqual "nearest cocircular square retains canonical AC" True (Set.member (pointKey squareA, pointKey squareC) (edgeKeys mesh))
    assertEqual "nearest cocircular square excludes BD" False (Set.member (pointKey squareD, pointKey squareB) (edgeKeys mesh))
    query <- requireQueryPoint "nearest cocircular square center" (Point 1 1)
    assertNearestMatchesExactArgmin "nearest cocircular square" mesh query

  checkRoundedPermutation points = do
    built <- requirePointBuild "nearest rounded trap" points
    assertNearestInputMapping "nearest rounded trap" points built
    query <- requireQueryPoint "nearest rounded trap query" (Point 0 0)
    _ <- traverse (requireQueryPoint "nearest rounded trap admitted site") points
    assertEqual "nearest rounded trap really rounds to a tie"
      (squaredDistance (queryPointValue query) roundedFarther)
      (squaredDistance (queryPointValue query) roundedNearer)
    assertEqual "nearest rounded trap exact gap" (toRational (encodeFloat 1 (-54) :: Double))
      (rationalSquaredDistance (queryPointValue query) roundedFarther
        - rationalSquaredDistance (queryPointValue query) roundedNearer)
    let mesh = buildTriangulation built
    fartherInput <- requireJust "nearest rounded farther input" (List.elemIndex roundedFarther points)
    let fartherVertex = VertexId (indexPrimArray (buildInputVertices built) fartherInput)
    case nearestNeighbor mesh (Just fartherVertex) query of
      Nothing -> fail "nearest rounded trap returned Nothing"
      Just (nearest, _) -> assertEqual "nearest rounded trap returns genuinely nearer site" roundedNearer (vertexPoint mesh nearest)
    assertNearestMatchesExactArgmin "nearest rounded trap" mesh query

  checkDomainFixture (label, points, rawQuery) = do
    built <- requirePointBuild label points
    assertNearestInputMapping label points built
    query <- requireQueryPoint (label <> " query") rawQuery
    assertNearestMatchesExactArgmin label (buildTriangulation built) query
    assertNearestMatchesExactArgmin (label <> " promoted without constraints")
      (fromDelaunay (buildTriangulation built)) query

  domainFixtures =
    [ ("nearest empty", [], Point 0 0)
    , ("nearest singleton", [Point 3 4], Point (-1) 2)
    , ("nearest two-site tie", [Point (-1) 0, Point 1 0], Point 0 0)
    , ("nearest collinear tie", fmap (\x -> Point x 0) [2, -2, 1, -1, 0], Point 0.5 1)
    , ("nearest signed-zero admission", [Point (-0.0) 0, Point 0 (-0.0), Point 1 0], Point (-0.0) (-0.0))
    , ("nearest one-dyadic-unit separation",
        [Point 1 (encodeFloat 1 (-27) + encodeFloat 1 (-79)), roundedFarther], Point 0 0)
    , ("nearest admitted maximum exponent", [Point large 0, Point (-large) 0, Point 0 large], Point 0 0)
    , ("nearest admitted minimum exponent", [Point small 0, Point (-small) 0, Point 0 small], Point 0 0)
    , ("nearest mixed admitted exponents", [Point large small, Point large 0], Point 0 0)
    ]
  large = maximumAllowedCoordinate
  small = minimumAllowedCoordinate
  integerRing =
    [ Point 5 0, Point 4 3, Point 3 4, Point 0 5
    , Point (-3) 4, Point (-4) 3, Point (-5) 0, Point (-4) (-3)
    , Point (-3) (-4), Point 0 (-5), Point 3 (-4), Point 4 (-3)
    ]
  checkIntegerRing points = do
    built <- requirePointBuild "nearest integer cocircular ring" points
    query <- requireQueryPoint "nearest integer cocircular ring center" (Point 0 0)
    assertNearestMatchesExactArgmin "nearest integer cocircular ring" (buildTriangulation built) query

assertNearestInputMapping
  :: String
  -> [Point]
  -> BuildResult mode Point () () ()
  -> IO ()
assertNearestInputMapping label points built = do
  let mapping = buildInputVertices built
      mesh = buildTriangulation built
  assertEqual (label <> " input mapping cardinality") (length points) (sizeofPrimArray mapping)
  traverse_
    (\(index, point) ->
      assertEqual (label <> " input mapping coordinates") (canonicalPoint point)
        (vertexPoint mesh (VertexId (indexPrimArray mapping index)))
    )
    (zip [0 ..] points)

-- Independent oracle: construct Rational distances and sort their values,
-- rather than reusing the production dyadic difference comparator or argmin.
rationalSquaredDistance :: Point -> Point -> Rational
rationalSquaredDistance (Point qx qy) (Point px py) =
  let dx = toRational px - toRational qx
      dy = toRational py - toRational qy
   in dx * dx + dy * dy

assertNearestMatchesExactArgmin
  :: String
  -> Triangulation mode vertex directed undirected face
  -> QueryPoint
  -> IO ()
assertNearestMatchesExactArgmin label mesh query = do
  let residentSites = vertices mesh
      queryValue = queryPointValue query
      exactKey vertex = (rationalSquaredDistance queryValue (vertexPoint mesh vertex), vertex)
      expected = case List.sortOn exactKey residentSites of
        [] -> Nothing
        nearest : _ -> Just nearest
      hints = Nothing : fmap Just residentSites <> [Just (VertexId maxBound)]
  traverse_
    (\hint -> do
      let result = nearestNeighbor mesh hint query
      assertEqual (label <> " exact argmin from " <> show hint) expected (fmap fst result)
      case result of
        Nothing -> pure ()
        Just (_, stats) -> do
          assertEqual (label <> " exact descent is well-founded") True
            (nearestWalkSteps stats < numVertices mesh)
          when (numConstraints mesh > 0) $
            assertEqual (label <> " constrained global comparison count") (numVertices mesh - 1)
              (nearestDistanceTests stats)
    )
    hints
  traverse_
    (\firstSite -> traverse_
      (\secondSite ->
        assertEqual (label <> " exact comparator")
          (compare (fst (exactKey firstSite)) (fst (exactKey secondSite)))
          (Admitted.compareSquaredDistances queryValue (vertexPoint mesh firstSite) (vertexPoint mesh secondSite))
      )
      residentSites
    )
    residentSites
