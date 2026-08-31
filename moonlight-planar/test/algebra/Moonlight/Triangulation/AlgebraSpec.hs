{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The laws of finite union, stated against the typed facade a caller has.
--
-- Algebraic laws are observed through 'canonicalize'. Structural 'Eq' remains
-- the exact physical-representation observation used by round trips and caches.
module Moonlight.Triangulation.AlgebraSpec (tests) where

import Control.Monad (forM_, unless, when)
import Data.Foldable (traverse_)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Word (Word64)
import qualified Data.Vector as V
import Moonlight.Triangulation
  ( BuildError
  , ConstraintMode (..)
  , DelaunayTriangulation
  , DuplicatePayloadPolicy (..)
  , HasPosition (..)
  , JoinSemilattice (..)
  , Point (Point)
  , Triangulation
  , buildTriangulation
  , delaunay
  , delaunayFromCoordinates
  , delaunayGeometry
  , unitElementDefaults
  , vertexData
  , vertexPoint
  , vertices
  , SiteRelation (..)
  , canonicalize
  , difference
  , intersection
  , intersectionWith
  , mapVertices
  , numVertices
  , siteRelation
  , symmetricDifference
  , union
  , unions
  , validateTriangulation
  )
import Moonlight.Triangulation.AlgebraFixtures
  ( Mesh
  , advance
  , assertMesh
  , assertMeshEquivalent
  , cocircularRing
  , collinearSites
  , dedupeAscending
  , edgeKeys
  , inputOrderedMeshOf
  , latticeSites
  , meshOf
  , operands
  , pairs
  , pointMeshOf
  , pointsOf
  , randomSites
  , scramble
  , separatedOperands
  , siteList
  , siteSet
  , triples
  )
import Support (requireRight)

tests :: IO ()
tests = do
  testConstructionEntrances
  testJoinIdentity
  testJoinCommutative
  testJoinAssociative
  testJoinIdempotent
  testJoinGeneralPathIdempotent
  testJoinSiteUnion
  testJoinGluesAnnotations
  testJoinIsUnionRebuild
  testJoinResultValid
  testSkewedJoinPreservesBaseHandles
  testJoinBalancedFold
  testJoinPartitionTrees
  testConstructionOrderIndependent
  testCanonicalPublication
  testSiteRelationCoherence
  testMeetLaws
  testEmptySetIdentitiesPreserveVerbatim
  testSkewedSetOperationsRemainValid
  testDifferenceLaws
  testSymmetricDifferenceLaws
  testLatticeLaws
  testSetOperationsPublishCanonical
  testAnnotationPreservation
  testOldEdgeAccounting
  putStrLn "algebra: ok"

testConstructionEntrances :: IO ()
testConstructionEntrances = do
  let coordinates = V.fromList [Point 0 0, Point 3 0, Point 0 4, Point 0 0]
      payloads = V.fromList [2 :: Int, 3, 5, 7]
  geometry <- requireRight "geometry-only construction" (delaunayGeometry coordinates)
  legacy <-
    mapVertices (const ()) . buildTriangulation
      <$> requireRight "annotated point construction" (delaunay unitElementDefaults coordinates)
  assertMeshEquivalent
    "geometry-only construction agrees with annotated point construction"
    legacy
    geometry
  annotated <-
    buildTriangulation
      <$> requireRight
            "coordinate/payload construction"
            ( delaunayFromCoordinates
                unitElementDefaults
                coordinates
                payloads
                (CombineDuplicatePayload (+))
            )
  unless (validateTriangulation geometry == [] && validateTriangulation annotated == []) $
    fail "public construction entrance produced an invalid triangulation"
  let observed =
        Map.fromList
          [ ((x, y), vertexData annotated vertex)
          | vertex <- vertices annotated
          , let Point x y = vertexPoint annotated vertex
          ]
      expected = Map.fromList [((0, 0), 9), ((3, 0), 3), ((0, 4), 5)]
  unless (observed == expected) $
    fail "delaunayFromCoordinates did not apply its duplicate payload policy"

-- ── laws ─────────────────────────────────────────────────────────────────────

testJoinIdentity :: IO ()
testJoinIdentity = do
  values <- operands
  identity <- requireRight "union identity" (unions [])
  forM_ values $ \(name, mesh) -> do
    left <- requireRight ("left identity at " <> name) (union identity mesh)
    right <- requireRight ("right identity at " <> name) (union mesh identity)
    assertMesh ("left identity at " <> name) mesh left
    assertMesh ("right identity at " <> name) mesh right
  idempotent <- requireRight "identity is idempotent" (union identity identity)
  assertMesh "identity is idempotent" identity idempotent

testJoinCommutative :: IO ()
testJoinCommutative = do
  values <- operands
  forM_ (pairs values) $ \((leftName, left), (rightName, right)) -> do
    leftRight <- requireRight "commutative union left/right" (union left right)
    rightLeft <- requireRight "commutative union right/left" (union right left)
    assertMeshEquivalent
      ("commutativity at " <> leftName <> " ⋄ " <> rightName)
      leftRight
      rightLeft

testJoinAssociative :: IO ()
testJoinAssociative = do
  values <- operands
  forM_ (triples values) $ \((aName, a), (bName, b), (cName, c)) -> do
    leftAssociated <- requireRight "left-associated union" (union a b >>= (`union` c))
    rightAssociated <- requireRight "right-associated union" (union b c >>= union a)
    assertMeshEquivalent
      ("associativity at " <> aName <> " ⋄ " <> bName <> " ⋄ " <> cName)
      leftAssociated
      rightAssociated

testJoinIdempotent :: IO ()
testJoinIdempotent = do
  values <- operands
  forM_ values $ \(name, mesh) -> do
    result <- requireRight ("idempotence at " <> name) (union mesh mesh)
    assertMesh ("idempotence at " <> name) mesh result

-- | Idempotence with the structural-equality shortcut deliberately disarmed.
--
-- @a \<\> a@ is answered by a shortcut that compares the operands and returns
-- one verbatim, so on its own it says nothing about the operator underneath.
-- Three meshes standing on the same sites, built in three different orders, are
-- pairwise distinct as values — the test asserts that before relying on it —
-- so every join below takes the general path, and joining a fourth
-- representation of the same site set onto the result must still change
-- nothing.
testJoinGeneralPathIdempotent :: IO ()
testJoinGeneralPathIdempotent = do
  let base = randomSites 0xA11CE 40
  first <- inputOrderedMeshOf "order-1" base
  second <- inputOrderedMeshOf "order-2" (scramble 0x1111 base)
  third <- inputOrderedMeshOf "order-3" (scramble 0x2222 base)
  fourth <- inputOrderedMeshOf "order-4" (reverse base)
  unless (first /= second && second /= third && third /= fourth) $
    fail "general-path idempotence: the four builds are not distinct values, so the shortcut is not disarmed"
  joined <- requireRight "general-path union" (union first second)
  thirdResult <- requireRight "absorbing a third representation" (union joined third)
  fourthResult <- requireRight "absorbing a fourth representation" (union joined fourth)
  symmetric <- requireRight "the union of two representations is symmetric" (union second first)
  assertMeshEquivalent "absorbing a third representation" joined thirdResult
  assertMeshEquivalent "absorbing a fourth representation" joined fourthResult
  assertMeshEquivalent "the union of two representations is symmetric" joined symmetric

-- | The sites of a join are exactly the union of the operands' sites: none
-- dropped, none invented, each stored once.
testJoinSiteUnion :: IO ()
testJoinSiteUnion = do
  values <- operands
  forM_ (pairs values) $ \((leftName, left), (rightName, right)) -> do
    let label = leftName <> " ⋄ " <> rightName
        expected = Set.union (siteSet left) (siteSet right)
    joined <- requireRight ("site union at " <> label) (union left right)
    let actual = siteSet joined
    unless (expected == actual) $
      fail
        ( "site union at "
            <> label
            <> ": dropped "
            <> show (Set.toList (Set.difference expected actual))
            <> ", invented "
            <> show (Set.toList (Set.difference actual expected))
        )
    unless (Set.size actual == numVertices joined) $
      fail ("site union at " <> label <> ": a site is stored more than once")

-- | The reference semantics: a join /is/ a rebuild of the union when both are
-- observed canonically.
--
-- The union is rebuilt three times, in three unrelated orders, and all three
-- must canonicalize to the canonical observation of the join. Comparing
-- against a single rebuild would only say the two agree; comparing against
-- three says that what they agree on is a function of the site set and not of
-- any build schedule, which is the actual claim.
--
-- Both shortcut cases are excluded, because a shortcut returns an operand
-- verbatim and an operand need not be canonically published. The count of
-- pairs that actually reached the general path is asserted, so this cannot
-- quietly become a test of nothing.
testJoinIsUnionRebuild :: IO ()
testJoinIsUnionRebuild = do
  values <- operands
  exercised <- newIORef (0 :: Int)
  forM_ (pairs values) $ \((leftName, left), (rightName, right)) ->
    when (numVertices left > 0 && numVertices right > 0 && left /= right) $ do
      let label = leftName <> " ⋄ " <> rightName
          unionSites = siteList left <> siteList right
      joined <- requireRight ("union of " <> label) (union left right)
      canonicalJoined <- requireRight ("canonical union of " <> label) (canonicalize joined)
      forM_ [("ranked", dedupeAscending (sort unionSites)), ("reversed", pointsOf (reverse unionSites)), ("scrambled", pointsOf (scramble 0x7A57E unionSites))] $
        \(order, sites) -> do
          rebuilt <- meshOf ("rebuild of " <> label <> " in " <> order <> " order") sites
          canonical <- requireRight ("canonical rebuild of " <> label <> " in " <> order <> " order") (canonicalize rebuilt)
          assertMesh
            ("join equals canonical rebuild at " <> label <> " (" <> order <> ")")
            canonical
            canonicalJoined
      modifyIORef' exercised (+ 1)
  count <- readIORef exercised
  unless (count >= 40) $
    fail ("join-equals-rebuild exercised only " <> show count <> " general-path pairs")

-- | Canonical publication in its own right, since the union laws lean on it.
--
-- Renumbering must not move geometry, must be a fixed point, must leave a
-- valid triangulation, and — the load-bearing one — must send every build
-- order of a site set to the same value. The distinctness of the inputs is
-- asserted first, so a canonicalization that did nothing at all would fail
-- here rather than pass everything.
testCanonicalPublication :: IO ()
testCanonicalPublication = do
  values <- operands
  forM_ values $ \(name, mesh) -> do
    canonical <- requireRight ("canonicalization at " <> name) (canonicalize mesh)
    unless (siteSet canonical == siteSet mesh) $
      fail ("canonicalization at " <> name <> " moved the site set")
    unless (edgeKeys canonical == edgeKeys mesh) $
      fail ("canonicalization at " <> name <> " changed the triangulation")
    case validateTriangulation canonical of
      [] -> pure ()
      violations -> fail ("canonicalization at " <> name <> " is invalid: " <> show violations)
    fixedPoint <- requireRight ("canonicalization fixed point at " <> name) (canonicalize canonical)
    assertMesh ("canonicalization at " <> name <> " is a fixed point") canonical fixedPoint
  forM_ [("scattered", randomSites 0xB0A710 48), ("cocircular", cocircularRing), ("lattice", latticeSites), ("collinear", collinearSites)] $
    \(name, base) -> do
      built <-
        traverse
          (\(order, sites) -> (,) order <$> inputOrderedMeshOf (name <> "/" <> order) sites)
          [ ("input", base)
          , ("ranked", dedupeAscending (sort [(x, y) | Point x y <- base]))
          , ("reversed", reverse base)
          , ("scrambled", scramble 0x6666 base)
          ]
      case built of
        [] -> fail "canonical publication: nothing built"
        (_, reference) : rest -> do
          unless (any (\(_, mesh) -> mesh /= reference) rest) $
            fail ("canonical publication at " <> name <> ": every order already agreed, so this asserts nothing")
          canonicalReference <- requireRight ("canonical reference at " <> name) (canonicalize reference)
          forM_ rest $ \(order, mesh) -> do
            canonicalMesh <- requireRight ("canonical publication at " <> name <> " from " <> order <> " order") (canonicalize mesh)
            assertMesh
              ("canonical publication at " <> name <> " from " <> order <> " order")
              canonicalReference
              canonicalMesh

testJoinResultValid :: IO ()
testJoinResultValid = do
  values <- operands
  forM_ (pairs values) $ \((leftName, left), (rightName, right)) -> do
    joined <- requireRight ("valid union at " <> leftName <> " ⋄ " <> rightName) (union left right)
    case validateTriangulation joined of
      [] -> pure ()
      violations ->
        fail ("join at " <> leftName <> " ⋄ " <> rightName <> " is invalid: " <> show violations)

testSkewedJoinPreservesBaseHandles :: IO ()
testSkewedJoinPreservesBaseHandles = do
  let baseSites = randomSites 0x5A71E 256
      extensionSites =
        fmap
          (\(Point x y) -> Point (x + 4) y)
          (randomSites 0xE71E 16)
  base <- meshOf "persistent base" baseSites
  extension <- meshOf "persistent extension" extensionSites
  joined <- requireRight "persistent skewed union" (union base extension)
  let baseObservations = fmap (\vertex -> (vertex, vertexPoint base vertex)) (vertices base)
  forM_ baseObservations $ \(vertex, expectedPoint) ->
    unless (vertexPoint joined vertex == expectedPoint) $
      fail
        ( "persistent skewed union moved base handle "
            <> show vertex
            <> " from "
            <> show expectedPoint
            <> " to "
            <> show (vertexPoint joined vertex)
        )
  canonicalJoined <- requireRight "canonical persistent skewed union" (canonicalize joined)
  reference <- meshOf "persistent union reference" (baseSites <> extensionSites)
  canonicalReference <- requireRight "canonical persistent union reference" (canonicalize reference)
  assertMesh "persistent skewed union agrees with canonical reference" canonicalReference canonicalJoined

-- | The balanced tournament agrees with every typed fold, in both directions.
testJoinBalancedFold :: IO ()
testJoinBalancedFold = do
  shards <- shardMeshes 6 (randomSites 0xBA5EBA11 48)
  identity <- requireRight "empty union" (unions [])
  case shards of
    [] -> fail "balanced fold: no shards were built"
    firstShard : _ -> do
      expected <- requireRight "balanced unions" (unions shards)
      foldRight <- requireRight "right-folded unions" (foldr (\shard result -> result >>= union shard) (Right identity) shards)
      foldLeft <- requireRight "left-folded unions" (foldl (\result shard -> result >>= (`union` shard)) (Right identity) shards)
      singleton <- requireRight "singleton unions" (unions [firstShard])
      repeated <- requireRight "repeated unions" (unions [firstShard, firstShard])
      assertMeshEquivalent "balanced unions agree with foldr" expected foldRight
      assertMeshEquivalent "balanced unions agree with foldl" expected foldLeft
      assertMesh "unions of one shard is that shard" firstShard singleton
      assertMesh "unions respects the idempotence shortcut" firstShard repeated

-- | The document's partition test: one site set, many shardings, many
-- bracketings, one canonical observation.
testJoinPartitionTrees :: IO ()
testJoinPartitionTrees = do
  let base = randomSites 0xD15EA5E 54
  whole <- meshOf "whole" base
  reference <- requireRight "canonical whole partition reference" (canonicalize whole)
  forM_ ([2, 3, 5, 7] :: [Int]) $ \shardCount -> do
    shards <- shardMeshes shardCount base
    forM_ ([0 .. 7] :: [Int]) $ \shape -> do
      let scrambled = scramble (fromIntegral shape * 7919 + 13) shards
      result <-
        requireRight
          ("partition tree " <> show shardCount <> "/" <> show shape)
          (bracketRandomly (fromIntegral shape * 104729 + 7) scrambled)
      assertMeshEquivalent
        ("partition tree " <> show shardCount <> "/" <> show shape)
        reference
        result

-- | Delaunay uniqueness, stated as a law about this construction: the same
-- sites in any order give the same triangulation, and differ only in how it is
-- numbered.
--
-- Both halves are asserted, and the second is the one that carries weight. If
-- the numbering did /not/ differ, canonical publication would be a no-op and
-- the quotient this test names would be trivial. Because it does differ, this
-- law licenses construction in whichever order is cheapest and an explicit
-- canonical observation only where physical agreement is required.
--
-- The cocircular ring and the lattice are here because they are where it could
-- fail: an exactly flat lifted quadrilateral has two legal diagonals, and the
-- rule that picks between them is keyed on coordinates rather than on vertex
-- identifiers precisely so that this law holds.
testConstructionOrderIndependent :: IO ()
testConstructionOrderIndependent = do
  forM_ [("scattered", randomSites 0x0DDBA11 60), ("cocircular", cocircularRing), ("lattice", latticeSites)] $
    \(name, base) -> do
      let ranked = dedupeAscending (sort [(x, y) | Point x y <- base])
      meshes <-
        traverse
          (\(order, sites) -> (,) order <$> inputOrderedMeshOf (name <> "/" <> order) sites)
          [ ("input", base)
          , ("ranked", ranked)
          , ("reversed", reverse base)
          , ("scrambled-a", scramble 0x3333 base)
          , ("scrambled-b", scramble 0x4444 base)
          ]
      case meshes of
        [] -> fail "order independence: nothing built"
        (referenceOrder, reference) : rest -> do
          forM_ rest $ \(order, mesh) ->
            unless (edgeKeys reference == edgeKeys mesh) $
              fail
                ( "order independence at "
                    <> name
                    <> ": "
                    <> referenceOrder
                    <> " and "
                    <> order
                    <> " reached different triangulations"
                )
          unless (any (\(_, mesh) -> mesh /= reference) rest) $
            fail
              ( "order independence at "
                  <> name
                  <> ": every build order produced the identical value, so the"
                  <> " numbering quotient is trivial and this law asserts nothing"
              )

-- | The Guibas–Stolfi old-edge theorem, asserted as a law of the operator.
--
-- Adding sites to a Delaunay triangulation cannot create an edge between two
-- sites that were already there. So every edge of a join whose endpoints both
-- stood in one operand must already have been an edge of that operand, and any
-- other edge is bichromatic — it joins a site exclusive to the left to a site
-- exclusive to the right.
--
-- This is strictly stronger than checking that the result is a valid Delaunay
-- triangulation, and it is stronger in exactly the direction a merge schedule
-- fails in. A seam that stitches the wrong pair of hull vertices, or that
-- retracts one edge too far before it stops deleting, produces a mesh that is
-- still Delaunay for /some/ site set and still passes validation; what it does
-- not do is leave the two interiors alone. The identity names that.
--
-- It holds for every schedule, including the rebuild the operator uses today,
-- because it is a property of the Delaunay triangulation of the union and not
-- of the route taken to it. That is why it can be asserted before a second
-- schedule exists: it is the gate one would have to pass, green on the path
-- that is already trusted.
--
-- Degeneracy does not weaken it. The tie-break on an exactly cocircular
-- quadrilateral is keyed on the four coordinates alone, so the same quad
-- resolves the same way whatever else stands nearby; a cocircular diagonal can
-- therefore be lost when a site lands inside its circle, which the theorem
-- permits, but cannot be exchanged for the other diagonal, which it forbids.
testOldEdgeAccounting :: IO ()
testOldEdgeAccounting = do
  overlapping <- operands
  separated <- separatedOperands
  census <- newIORef (0 :: Int, 0 :: Int)
  forM_ ([(l, r) | l <- overlapping, r <- overlapping] <> separated) $ \((leftName, left), (rightName, right)) -> do
    let label = "old-edge " <> leftName <> " / " <> rightName
    joined <- requireRight label (union left right)
    let leftSites = siteSet left
        rightSites = siteSet right
        leftEdges = edgeKeys left
        rightEdges = edgeKeys right
    forM_ (Set.toList (edgeKeys joined)) $ \edge -> do
      let (from, to) = edge
          spans sites = Set.member from sites && Set.member to sites
          monochromeLeft = spans leftSites
          monochromeRight = spans rightSites
      when (monochromeLeft && not (Set.member edge leftEdges)) $
        fail (label <> ": join created " <> show edge <> ", an edge between two sites of the left operand")
      when (monochromeRight && not (Set.member edge rightEdges)) $
        fail (label <> ": join created " <> show edge <> ", an edge between two sites of the right operand")
      modifyIORef' census $ \(retained, cross) ->
        if monochromeLeft || monochromeRight
          then (retained + 1, cross)
          else (retained, cross + 1)
  (retained, cross) <- readIORef census
  when (retained < 1000 || cross < 100) $
    fail
      ( "old-edge accounting: the census is too thin to have asserted anything — "
          <> show retained
          <> " retained and "
          <> show cross
          <> " cross edges"
      )

-- ── finite-set descent laws ─────────────────────────────────────────────────

-- | The public support relation is the exact order observation of the same
-- site sets consumed by union, meet and relative complement. Its overlap
-- witness must therefore agree with every operation's cardinality rather than
-- merely with another classification routine.
testSiteRelationCoherence :: IO ()
testSiteRelationCoherence = do
  values <- operands
  traverse_ (uncurry checkRelation) (pairs values)
 where
  checkRelation (leftName, left) (rightName, right) = do
    let label = "site relation at " <> leftName <> " / " <> rightName
        leftSites = siteSet left
        rightSites = siteSet right
        overlap = Set.size (Set.intersection leftSites rightSites)
        expected = referenceSiteRelation leftSites rightSites
        coldActual = siteRelation left right
    unless (coldActual == expected) $
      fail (label <> ": expected " <> show expected <> ", got " <> show coldActual)
    let repeatedActual = siteRelation left right
    unless (repeatedActual == coldActual) $
      fail (label <> ": repeated observation changed the relation")
    met <- requireRight (label <> " intersection") (intersection left right)
    removed <- requireRight (label <> " difference") (difference left right)
    joined <- requireRight (label <> " union") (union left right)
    unless (numVertices met == overlap) $
      fail (label <> ": intersection cardinality disagrees with overlap")
    unless (numVertices removed == Set.size leftSites - overlap) $
      fail (label <> ": difference cardinality disagrees with overlap")
    unless (numVertices joined == Set.size leftSites + Set.size rightSites - overlap) $
      fail (label <> ": union cardinality disagrees with overlap")

referenceSiteRelation
  :: Set.Set (Double, Double)
  -> Set.Set (Double, Double)
  -> SiteRelation
referenceSiteRelation left right
  | left == right = EqualSites
  | left `Set.isProperSubsetOf` right = LeftProperSubset
  | right `Set.isProperSubsetOf` left = RightProperSubset
  | Set.null overlap = DisjointSites
  | otherwise = PartialOverlap (Set.size overlap)
 where
  overlap = Set.intersection left right

testMeetLaws :: IO ()
testMeetLaws = do
  values <- setLawOperands
  traverse_
    ( \(name, mesh) ->
        assertSetEquation
          ("meet idempotence at " <> name)
          (intersection mesh mesh)
          (canonicalize mesh)
    )
    values
  traverse_
    ( \((leftName, left), (rightName, right)) ->
        assertSetEquation
          ("meet commutativity at " <> leftName <> " / " <> rightName)
          (intersection left right)
          (intersection right left)
    )
    (pairs values)
  traverse_
    ( \((aName, a), (bName, b), (cName, c)) ->
        assertSetEquation
          ("meet associativity at " <> aName <> " / " <> bName <> " / " <> cName)
          (intersection a b >>= (`intersection` c))
          (intersection b c >>= intersection a)
    )
    (triples values)

testEmptySetIdentitiesPreserveVerbatim :: IO ()
testEmptySetIdentitiesPreserveVerbatim = do
  let sites = scramble 0xE771D3 (randomSites 0xE771D4 48)
  mesh <- inputOrderedMeshOf "verbatim empty identity source" sites
  canonical <- requireRight "canonical verbatim empty identity source" (canonicalize mesh)
  unless (mesh /= canonical) $
    fail "verbatim empty identities: the source was already canonical, so this asserts nothing"
  empty <- requireRight "verbatim empty identity" (unions [])
  differenceRightIdentity <- requireRight "verbatim difference right identity" (difference mesh empty)
  differenceLeftZero <- requireRight "verbatim difference left zero" (difference empty mesh)
  symmetricRightIdentity <- requireRight "verbatim symmetric difference right identity" (symmetricDifference mesh empty)
  symmetricLeftIdentity <- requireRight "verbatim symmetric difference left identity" (symmetricDifference empty mesh)
  assertMesh "difference by empty preserves the left representative verbatim" mesh differenceRightIdentity
  assertMesh "empty difference remains the empty representative" empty differenceLeftZero
  assertMesh "symmetric difference by empty preserves the left representative verbatim" mesh symmetricRightIdentity
  assertMesh "empty symmetric difference preserves the right representative verbatim" mesh symmetricLeftIdentity

testSkewedSetOperationsRemainValid :: IO ()
testSkewedSetOperationsRemainValid = do
  let baseSites = randomSites 0xD1FFE7 1024
      removedSites = take 4 (scramble 0xD1FFE8 baseSites)
      retainedSites = filter (`notElem` removedSites) baseSites
      disjointIncomingSites = fmap (\(Point x y) -> Point (x + 4) y) (randomSites 0xD1FFE9 4)
      nearFullIntersectionSites = retainedSites <> disjointIncomingSites
      overlappingRemovedSites = take 2 removedSites
      overlappingIncomingSites = overlappingRemovedSites <> take 2 disjointIncomingSites
      xorDisjointSites = baseSites <> disjointIncomingSites
      xorOverlappingSites = filter (`notElem` overlappingRemovedSites) baseSites <> take 2 disjointIncomingSites
  base <- meshOf "skewed set-operation base" baseSites
  removed <- meshOf "skewed difference subset" removedSites
  disjointIncoming <- meshOf "skewed difference disjoint" disjointIncomingSites
  nearFullIntersection <- meshOf "skewed geometry-only intersection" nearFullIntersectionSites
  overlappingIncoming <- meshOf "skewed xor overlapping" overlappingIncomingSites
  differenceSubset <- requireRight "skewed difference subset" (difference base removed)
  differenceDisjoint <- requireRight "skewed difference disjoint" (difference base disjointIncoming)
  intersectionNearFull <- requireRight "skewed geometry-only intersection" (intersection base nearFullIntersection)
  xorDisjoint <- requireRight "skewed xor disjoint" (symmetricDifference base disjointIncoming)
  xorOverlapping <- requireRight "skewed xor overlapping" (symmetricDifference base overlappingIncoming)
  assertRawSetResult "skewed difference subset" retainedSites differenceSubset
  assertRawSetResult "skewed difference disjoint" baseSites differenceDisjoint
  assertMesh "skewed difference disjoint preserves the base verbatim" base differenceDisjoint
  assertRawSetResult "skewed geometry-only intersection" retainedSites intersectionNearFull
  assertRawSetResult "skewed xor disjoint" xorDisjointSites xorDisjoint
  assertRawSetResult "skewed xor overlapping" xorOverlappingSites xorOverlapping

assertRawSetResult :: String -> [Point] -> Mesh -> IO ()
assertRawSetResult label expectedSites result = do
  case validateTriangulation result of
    [] -> pure ()
    violations -> fail (label <> " raw result is invalid: " <> show violations)
  reference <- meshOf (label <> " fresh rebuild") (scramble 0xD1FFEA expectedSites)
  assertMeshEquivalent (label <> " agrees canonically with a fresh rebuild") reference result

testDifferenceLaws :: IO ()
testDifferenceLaws = do
  values <- setLawOperands
  identity <- requireRight "difference identity" (unions [])
  traverse_
    ( \(name, mesh) -> do
        assertSetEquation
          ("difference cancellation at " <> name)
          (difference mesh mesh)
          (Right identity)
        assertSetEquation
          ("difference right identity at " <> name)
          (difference mesh identity)
          (canonicalize mesh)
    )
    values
  traverse_
    ( \((leftName, left), (rightName, right)) -> do
        let label = "difference partition at " <> leftName <> " / " <> rightName
            commonAndRemainder = do
              common <- intersection left right
              remainder <- difference left right
              union common remainder
            remainderMeetRight = difference left right >>= (`intersection` right)
        assertSetEquation label commonAndRemainder (canonicalize left)
        assertSetEquation (label <> " disjointness") remainderMeetRight (Right identity)
    )
    (pairs values)

testSymmetricDifferenceLaws :: IO ()
testSymmetricDifferenceLaws = do
  values <- setLawOperands
  identity <- requireRight "symmetric-difference identity" (unions [])
  traverse_
    ( \(name, mesh) -> do
        assertSetEquation
          ("symmetric-difference cancellation at " <> name)
          (symmetricDifference mesh mesh)
          (Right identity)
        assertSetEquation
          ("symmetric-difference identity at " <> name)
          (symmetricDifference mesh identity)
          (canonicalize mesh)
    )
    values
  traverse_
    ( \((leftName, left), (rightName, right)) -> do
        let label = "symmetric difference at " <> leftName <> " / " <> rightName
            decomposed = do
              leftOnly <- difference left right
              rightOnly <- difference right left
              union leftOnly rightOnly
        assertSetEquation
          (label <> " commutativity")
          (symmetricDifference left right)
          (symmetricDifference right left)
        assertSetEquation
          (label <> " decomposition")
          (symmetricDifference left right)
          decomposed
    )
    (pairs values)
  traverse_
    ( \((aName, a), (bName, b), (cName, c)) ->
        assertSetEquation
          ("symmetric-difference associativity at " <> aName <> " / " <> bName <> " / " <> cName)
          (symmetricDifference a b >>= (`symmetricDifference` c))
          (symmetricDifference b c >>= symmetricDifference a)
    )
    (triples values)

testLatticeLaws :: IO ()
testLatticeLaws = do
  values <- setLawOperands
  traverse_
    ( \((leftName, left), (rightName, right)) -> do
        let label = "absorption at " <> leftName <> " / " <> rightName
        assertSetEquation
          (label <> " meet-over-join")
          (union left right >>= intersection left)
          (canonicalize left)
        assertSetEquation
          (label <> " join-over-meet")
          (intersection left right >>= union left)
          (canonicalize left)
    )
    (pairs values)
  traverse_
    ( \((aName, a), (bName, b), (cName, c)) -> do
        let label = "distributivity at " <> aName <> " / " <> bName <> " / " <> cName
            meetOverJoin = union b c >>= intersection a
            joinedMeets = do
              left <- intersection a b
              right <- intersection a c
              union left right
            joinOverMeet = intersection b c >>= union a
            metJoins = do
              left <- union a b
              right <- union a c
              intersection left right
        assertSetEquation (label <> " meet-over-join") meetOverJoin joinedMeets
        assertSetEquation (label <> " join-over-meet") joinOverMeet metJoins
    )
    (triples values)

testSetOperationsPublishCanonical :: IO ()
testSetOperationsPublishCanonical = do
  values <- setLawOperands
  traverse_
    ( \((leftName, left), (rightName, right)) -> do
        let label = "canonical set publication at " <> leftName <> " / " <> rightName
        case siteRelation left right of
          DisjointSites -> assertCanonicalFixedPoint (label <> " intersection") (intersection left right)
          PartialOverlap _ -> assertCanonicalFixedPoint (label <> " intersection") (intersection left right)
          EqualSites -> pure ()
          LeftProperSubset -> pure ()
          RightProperSubset -> pure ()
        when (numVertices left > 0 && numVertices right > 0) $ do
          assertCanonicalFixedPoint (label <> " difference") (difference left right)
          assertCanonicalFixedPoint (label <> " symmetric difference") (symmetricDifference left right)
    )
    (pairs values)

-- | Payloads descend with their coordinates. Intersection combines only the
-- common sections, difference preserves the left section, and symmetric
-- difference preserves whichever unique section survives. The independently
-- built expected meshes ensure this is not a restatement of the implementation.
testAnnotationPreservation :: IO ()
testAnnotationPreservation = do
  let shared = randomSites 0xA6607A7E 12
      leftOnly = randomSites 0x1EF7 8
      rightOnly = randomSites 0xA1167 10
      leftPoints = leftOnly <> shared
      rightPoints = shared <> rightOnly
  left <- pointMeshOf "annotation-left" leftPoints
  rightPointsMesh <- pointMeshOf "annotation-right" rightPoints
  sharedReference <- canonicalPointMesh "annotation-shared" shared
  leftOnlyReference <- canonicalPointMesh "annotation-left-only" leftOnly
  exclusiveReference <- canonicalPointMesh "annotation-exclusive" (leftOnly <> rightOnly)
  let right = mapVertices rightAnnotation rightPointsMesh
      expectedCombined = mapVertices (\point -> (point, rightAnnotation point)) sharedReference
      taggedLeft = mapVertices (\point -> ExclusiveAnnotation point LeftOperand) left
      taggedRight = mapVertices (\point -> ExclusiveAnnotation point RightOperand) rightPointsMesh
      taggedExclusive =
        mapVertices
          (\point -> ExclusiveAnnotation point (if point `elem` leftOnly then LeftOperand else RightOperand))
          exclusiveReference
  unless (siteRelation left right == PartialOverlap (length shared)) $
    fail "siteRelation changed when the vertex annotation type changed"
  combined <- requireRight "annotation intersectionWith" (intersectionWith (,) left right)
  restricted <- requireRight "annotation restriction" (intersectionWith const left right)
  removed <- requireRight "annotation difference" (difference left right)
  exclusive <- requireRight "annotation symmetric difference" (symmetricDifference taggedLeft taggedRight)
  assertAnnotatedMesh "intersectionWith combines left then right" expectedCombined combined
  assertAnnotatedMesh "intersectionWith const restricts the left section" sharedReference restricted
  assertAnnotatedMesh "difference preserves left annotations" leftOnlyReference removed
  assertAnnotatedMesh "symmetric difference preserves the annotation of each exclusive owner" taggedExclusive exclusive

data ExclusiveOperand
  = LeftOperand
  | RightOperand
  deriving stock (Eq)

data ExclusiveAnnotation = ExclusiveAnnotation !(Point) !ExclusiveOperand
  deriving stock (Eq)

rightAnnotation :: Point -> (Double, Double)
rightAnnotation (Point x y) = (x + y, x - y)

canonicalPointMesh :: String -> [Point] -> IO (DelaunayTriangulation (Point))
canonicalPointMesh label points =
  pointMeshOf label points
    >>= requireRight ("canonicalize " <> label) . canonicalize

assertAnnotatedMesh :: Eq mesh => String -> mesh -> mesh -> IO ()
assertAnnotatedMesh label expected actual =
  unless (expected == actual) (fail (label <> ": structural values differ"))

assertSetEquation
  :: String
  -> Either BuildError Mesh
  -> Either BuildError Mesh
  -> IO ()
assertSetEquation label leftExpression rightExpression = do
  left <- requireRight (label <> " left") leftExpression
  right <- requireRight (label <> " right") rightExpression
  leftCanonical <- requireRight (label <> " canonical left") (canonicalize left)
  rightCanonical <- requireRight (label <> " canonical right") (canonicalize right)
  assertMesh label leftCanonical rightCanonical

assertCanonicalFixedPoint :: String -> Either BuildError Mesh -> IO ()
assertCanonicalFixedPoint label expression = do
  result <- requireRight label expression
  fixedPoint <- requireRight (label <> " fixed point") (canonicalize result)
  assertMesh label result fixedPoint

-- | Six representatives retain empty, degenerate, nested, partially
-- overlapping and equal-support/different-publication cases without turning
-- every ternary law into the full nine-cubed join corpus already exercised
-- above.
setLawOperands :: IO [(String, Mesh)]
setLawOperands =
  filter
    (\(name, _) -> name `elem` ["void", "single", "lattice-lower", "lattice-upper", "scattered", "scattered-scrambled"])
    <$> operands

-- ── construction ─────────────────────────────────────────────────────────────

-- | Deal the sites round-robin into @count@ shards, so every shard spans the
-- whole extent and the joins are genuinely overlapping rather than separable.
shardMeshes :: Int -> [Point] -> IO [Mesh]
shardMeshes count sites =
  traverse
    (\shard -> meshOf ("shard " <> show shard) [site | (index, site) <- indexed, index `mod` count == shard])
    [0 .. count - 1]
 where
  indexed = zip [0 :: Int ..] sites

-- | Combine by repeatedly joining a pseudo-randomly chosen adjacent pair, so
-- that successive seeds give genuinely different bracketings.
bracketRandomly :: Word64 -> [Mesh] -> Either BuildError Mesh
bracketRandomly _ [] = unions []
bracketRandomly _ [single] = Right single
bracketRandomly seed values =
  let !stepped = advance seed
      !cut = fromIntegral (stepped `mod` fromIntegral (length values - 1))
   in joinAdjacent cut values >>= bracketRandomly stepped

-- | Replace the pair at @index@ with its join, leaving everything else alone.
joinAdjacent :: Int -> [Mesh] -> Either BuildError [Mesh]
joinAdjacent index (left : right : rest)
  | index <= 0 = (: rest) <$> union left right
  | otherwise = (left :) <$> joinAdjacent (index - 1) (right : rest)
joinAdjacent _ rest = Right rest

-- | A site is a coordinate carrying a tag. The join acts on tags alone: two
-- annotations only ever meet at a shared coordinate, where the positions
-- already agree, so geometry is never authored by the payload.
data Site = Site !(Point) !Int
  deriving stock (Eq, Show)

instance HasPosition Site where
  position (Site point _) = point

instance JoinSemilattice Site where
  joinAnnotations (Site point left) (Site _ right) = Site point (max left right)

type AnnotatedMesh = Triangulation 'Unconstrained Site () () ()

annotatedMesh :: String -> [Site] -> IO AnnotatedMesh
annotatedMesh label sites =
  buildTriangulation <$> requireRight label (delaunay unitElementDefaults (V.fromList sites))

-- Keyed on the coordinate plane rather than on the payload's own point,
-- because geometry is authoritative and a payload that disagreed with it must
-- not be able to hide behind itself.
tagsOf :: AnnotatedMesh -> Map.Map (Double, Double) Int
tagsOf mesh =
  Map.fromList
    [ ((x, y), tag)
    | vertex <- vertices mesh
    , let Point x y = vertexPoint mesh vertex
    , let Site _ tag = vertexData mesh vertex
    ]

-- The whole law in one equation: the union's annotation at every site is the
-- join of whatever the operands carried there.
assertGluing :: String -> [Site] -> [Site] -> IO ()
assertGluing label leftSites rightSites = do
  left <- annotatedMesh (label <> " left") leftSites
  right <- annotatedMesh (label <> " right") rightSites
  glued <- requireRight (label <> " union") (union left right)
  assertEqualTags
    (label <> ": union glues shared annotations by join")
    (Map.unionWith max (tagsOf left) (tagsOf right))
    (tagsOf glued)

assertEqualTags :: String -> Map.Map (Double, Double) Int -> Map.Map (Double, Double) Int -> IO ()
assertEqualTags label expected actual =
  unless (expected == actual) $
    fail (label <> ": expected " <> show (Map.toList expected) <> ", got " <> show (Map.toList actual))

-- The plan is chosen by size: a small addition is inserted into the larger
-- operand, and only comparably sized operands are rebuilt canonically. Both
-- paths glue, and only running both shows it.
testJoinGluesAnnotations :: IO ()
testJoinGluesAnnotations = do
  assertGluing
    "insertion plan"
    [Site (Point 0 0) 1, Site (Point 6 0) 1, Site (Point 0 6) 1, Site (Point 2 1) 7]
    [Site (Point 0 0) 5, Site (Point 6 0) 2, Site (Point 0 6) 3, Site (Point 4 3) 9]
  assertGluing "rebuild plan" (grid 0 1) (grid 6 2)
 where
  -- Two hundred-odd sites overlapping in half, so neither operand is small
  -- enough to be inserted into the other and the canonical rebuild is taken.
  grid offset tag =
    [ Site (Point (fromIntegral column) (fromIntegral row)) (tag * (column + row))
    | column <- [offset .. offset + 11 :: Int]
    , row <- [0 .. 9 :: Int]
    ]
