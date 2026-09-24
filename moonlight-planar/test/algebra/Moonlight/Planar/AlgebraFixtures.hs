-- | The operand meshes and mesh observations both algebra slices are stated
-- over, built against the facade alone.
module Moonlight.Planar.AlgebraFixtures
  ( Mesh
  , meshOf
  , inputOrderedMeshOf
  , operands
  , separatedOperands
  , cocircularRing
  , collinearSites
  , latticeSites
  , pointsOf
  , dedupeAscending
  , siteList
  , siteSet
  , assertMesh
  , assertMeshEquivalent
  , pairs
  , triples
  , advance
  , scramble
  , randomSites
  ) where

import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import qualified Data.Set as Set
import qualified Data.Vector as V
import Data.Word (Word64)
import Moonlight.Planar.BulkLoad (delaunay, delaunayGeometry)
import Moonlight.Planar.Canonical (canonicalize)
import Moonlight.Planar.Dcel (numFaces, numUndirectedEdges, numVertices)
import Moonlight.Planar.Handles.Iterators.FixedIterators (vertices)
import Moonlight.Planar.Payload (geometryOnlyPublication, mapVertices)
import Moonlight.Planar.Types (DelaunayTriangulation, buildTriangulation, unitElementDefaults)
import Moonlight.Planar.Point (Point (Point))
import Moonlight.Planar.MeshFixtures (edgeKeys, siteKey)
import Support (requireRight)

-- | The carrier. Geometry and nothing else: no vertex payload to need a
-- commutative combining rule, no element payloads to survive a rewrite that
-- destroys the elements they labelled.
type Mesh = DelaunayTriangulation ()

-- ── operands ─────────────────────────────────────────────────────────────────

-- | The site families the laws are exercised over.
--
-- Sized so that the cubic sweep in the associativity law stays cheap. Coverage
-- here is by /kind/ of degeneracy, not by count: what breaks a join is a
-- cocircular quad whose tie-break went the other way, or a mesh with no faces
-- at all, and neither of those becomes more likely at ten thousand sites.
families :: [(String, [Point])]
families =
  [ ("void", [])
  , ("single", [Point 3 (-7)])
  , ("pair", [Point 0 0, Point 4 1])
  , ("collinear", collinearSites)
  , ("cocircular", cocircularRing)
  , ("lattice", latticeSites)
  , ("repeated", concatMap (replicate 3) [Point 0 0, Point 5 0, Point 0 5, Point 5 5, Point 2 3])
  , ("scattered", randomSites 0xC0FFEEBABE 32)
  , ("extreme", [Point 1.0e-8 1.0e-8, Point 1.0e8 (-1.0e8), Point (-1.0e8) 1.0e8, Point 0 0, Point 1 1])
  ]

-- | Exactly cocircular lattice points on @x² + y² = 625@.
--
-- Trigonometric points would be cocircular only to within rounding, and the
-- rule this is here to exercise — the diagonal tie-break that fires when the
-- lifted quadrilateral is exactly flat — would then never fire at all.
cocircularRing :: [Point]
cocircularRing =
  [ Point (fromIntegral (signX * x)) (fromIntegral (signY * y))
  | (x, y) <- [(25, 0), (0, 25), (7, 24), (24, 7), (15, 20), (20, 15)] :: [(Int, Int)]
  , signX <- [1, -1]
  , signY <- [1, -1]
  ]

collinearSites :: [Point]
collinearSites = [Point (fromIntegral k) (2 * fromIntegral k - 1) | k <- [0 .. 9 :: Int]]

latticeSites :: [Point]
latticeSites = [Point (fromIntegral i) (fromIntegral j) | i <- [0 .. 4 :: Int], j <- [0 .. 4 :: Int]]

-- | The meshes every pairwise and triple law runs over.
--
-- Chosen for overlap structure rather than for size: disjoint operands, nested
-- ones, partially overlapping ones, the empty one, and — the case that matters
-- most — two meshes standing on the /same/ sites built in different orders, so
-- they are geometrically identical and structurally distinct.
operands :: IO [(String, Mesh)]
operands = do
  let sites name = fromMaybe [] (lookup name families)
      lattice = sites "lattice"
  void' <- meshOf "void" []
  single <- meshOf "single" (sites "single")
  collinear <- meshOf "collinear" (sites "collinear")
  ring <- meshOf "cocircular" (sites "cocircular")
  lower <- meshOf "lattice-lower" (take 15 lattice)
  upper <- meshOf "lattice-upper" (drop 10 lattice)
  repeated <- meshOf "repeated" (sites "repeated")
  scatterA <- meshOf "scattered" (sites "scattered")
  scatterB <- meshOf "scattered-scrambled" (scramble 0x5EED (sites "scattered"))
  pure
    [ ("void", void')
    , ("single", single)
    , ("collinear", collinear)
    , ("cocircular", ring)
    , ("lattice-lower", lower)
    , ("lattice-upper", upper)
    , ("repeated", repeated)
    , ("scattered", scatterA)
    , ("scattered-scrambled", scatterB)
    ]

-- | Operand pairs whose sites are separated by a vertical line, which is the
-- stratum a seam schedule is defined on and the one the old-edge identity is
-- sharpest over: with no shared site, every edge is unambiguously left, right
-- or cross.
separatedOperands :: IO [((String, Mesh), (String, Mesh))]
separatedOperands =
  traverse
    ( \(name, seed, count, shift) -> do
        let sites = randomSites seed count
        left <- meshOf (name <> "-left") sites
        right <- meshOf (name <> "-right") [Point (x + shift) y | Point x y <- sites]
        pure ((name <> "-left", left), (name <> "-right", right))
    )
    [ ("split-distant", 0x51DE1, 60, 1000)
    , ("split-near", 0x51DE2, 60, 3)
    , ("split-abutting", 0x51DE3, 60, 2.05)
    ]

-- ── construction ─────────────────────────────────────────────────────────────

meshOf :: String -> [Point] -> IO Mesh
meshOf label points =
  geometryOnlyPublication
    <$> requireRight ("build geometry " <> label) (delaunayGeometry (V.fromList points))

-- | Build through the annotated entrance when a law deliberately needs the
-- physical numbering induced by input order. Geometry-only construction is
-- free to choose the cheaper numbering because it publishes no input mapping.
inputOrderedMeshOf :: String -> [Point] -> IO Mesh
inputOrderedMeshOf label points =
  mapVertices (const ()) . buildTriangulation
    <$> requireRight
          ("build input-ordered geometry " <> label)
          (delaunay unitElementDefaults (V.fromList points))

pointsOf :: [(Double, Double)] -> [Point]
pointsOf keys = [Point x y | (x, y) <- keys]

dedupeAscending :: [(Double, Double)] -> [Point]
dedupeAscending sorted = [Point x y | (x, y) <- dropAdjacentDuplicates sorted]

dropAdjacentDuplicates :: Eq a => [a] -> [a]
dropAdjacentDuplicates (first : second : rest)
  | first == second = dropAdjacentDuplicates (second : rest)
  | otherwise = first : dropAdjacentDuplicates (second : rest)
dropAdjacentDuplicates rest = rest

-- ── observation ──────────────────────────────────────────────────────────────

siteList :: Mesh -> [(Double, Double)]
siteList mesh = [siteKey mesh vertex | vertex <- vertices mesh]

siteSet :: Mesh -> Set.Set (Double, Double)
siteSet = Set.fromList . siteList

-- | What a mesh looks like when two of them were supposed to be equal.
--
-- The interesting failure is the one where the coordinate-keyed edge sets
-- agree and the values do not: that is the join having become correct only up
-- to DCEL isomorphism, which is precisely what these laws exist to forbid, and
-- a report that only printed counts would hide it.
assertMesh :: String -> Mesh -> Mesh -> IO ()
assertMesh label expected actual
  | expected == actual = pure ()
  | otherwise = fail (label <> ": " <> report)
 where
  report
    | expectedEdges == actualEdges =
        "same geometry, different representation — "
          <> meshCounts expected
          <> " vs "
          <> meshCounts actual
    | otherwise =
        meshCounts expected
          <> " vs "
          <> meshCounts actual
          <> "; edges only in expected: "
          <> show (take 4 (Set.toList (Set.difference expectedEdges actualEdges)))
          <> "; only in actual: "
          <> show (take 4 (Set.toList (Set.difference actualEdges expectedEdges)))
  expectedEdges = edgeKeys expected
  actualEdges = edgeKeys actual

assertMeshEquivalent :: String -> Mesh -> Mesh -> IO ()
assertMeshEquivalent label expected actual = do
  canonicalExpected <-
    requireRight (label <> ": canonical expected") (canonicalize expected)
  canonicalActual <-
    requireRight (label <> ": canonical actual") (canonicalize actual)
  assertMesh label canonicalExpected canonicalActual

meshCounts :: Mesh -> String
meshCounts mesh =
  show (numVertices mesh)
    <> "v/"
    <> show (numUndirectedEdges mesh)
    <> "e/"
    <> show (numFaces mesh)
    <> "f"

-- ── combinatorics and pseudo-randomness ──────────────────────────────────────

pairs :: [a] -> [(a, a)]
pairs values = [(left, right) | left <- values, right <- values]

triples :: [a] -> [(a, a, a)]
triples values = [(a, b, c) | a <- values, b <- values, c <- values]

advance :: Word64 -> Word64
advance state = state * 6364136223846793005 + 1442695040888963407

randomWords :: Word64 -> Int -> [Word64]
randomWords seed count = take count (drop 1 (iterate advance seed))

-- | A deterministic permutation: decorate, sort by the key, discard it.
scramble :: Word64 -> [a] -> [a]
scramble seed values =
  map snd (sortBy (comparing fst) (zip (randomWords seed (length values)) values))

randomSites :: Word64 -> Int -> [Point]
randomSites seed count =
  [ Point (unitCoordinate first) (unitCoordinate second)
  | (first, second) <- take count (chunkPairs (randomWords seed (2 * count)))
  ]

chunkPairs :: [a] -> [(a, a)]
chunkPairs (first : second : rest) = (first, second) : chunkPairs rest
chunkPairs _ = []

unitCoordinate :: Word64 -> Double
unitCoordinate value = 2 * fromIntegral (value `div` 2048) / 9007199254740992 - 1
