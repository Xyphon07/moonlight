{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NumericUnderscores #-}

-- | What the join costs, and against what.
--
-- These lanes compare the public union schedules with rebuild, local insertion,
-- and explicit canonical observation. Schedule claims live or die by these
-- measurements rather than by asymptotic theatre.
module Moonlight.Planar.JoinBench
  ( benchmarks
  , publicationBenchmarks
  ) where

import BenchMeasure (requireRight, timedValue)
import BenchSupport (randomPoints)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (foldM, unless)
import Data.List (sort, sortBy)
import Data.Ord (comparing)
import qualified Data.Set as Set
import qualified Data.Vector as V
import Data.Word (Word64)
import Moonlight.Planar.BulkLoad (delaunay, delaunayGeometry, insertMany)
import Moonlight.Planar.Canonical (canonicalize)
import Moonlight.Planar.Cdt (ConstrainedSeamResult, constrainedDelaunay, constrainedSeamCachedFrontierPointReads, constrainedSeamFaceFirstPoint, constrainedSeamFaceSecondPoint, constrainedSeamFaceThirdPoint, constrainedSeamJoinFaces, constrainedSeamLeftConstraintCount, constrainedSeamPublicationStats, constrainedSeamResultTriangulation, constrainedSeamRightFaceEvidence, constrainedSeamTargetFace, constraintSegments, joinSeparatedConstrained, segmentEnd, segmentStart)
import Moonlight.Planar.Dcel (numFaces)
import Moonlight.Planar.Handles.HandleDefs (FaceId, UndirectedEdgeId, asUndirected, reverseEdge)
import Moonlight.Planar.Payload (geometryOnlyPublication)
import Moonlight.Planar.Refinement (refineWithinDomain, withAdditionalVertexBudget, withConstraintPreservation, withMaximumArea, withoutMaximumRadiusEdgeRatio)
import Moonlight.Planar.SetAlgebra (difference, intersection, intersectionWith, siteRelation, symmetricDifference, union, unions)
import Moonlight.Planar.Types (BuildError (..), ClosureStats (..), ConstrainedDelaunayTriangulation, DelaunayTriangulation, buildTriangulation, defaultRefinementParameters, refinedTriangulation, refinementAddedVertices, refinementAttemptedBoundaryCrossings, refinementClosureStats, refinementComplete, refinementCreatedFaces, refinementDomainReceipt, refinementDomainResult, refinementFinalPermittedFaces, refinementInterfaceBoundaryReads, refinementPublicationStats, refinementStats, refinementTouchedEdges, unitElementDefaults)
import Moonlight.Planar.BuildStats (BuildStats)
import Moonlight.Planar.Point (Point (..))
import Moonlight.Planar.Validation (validateTriangulation)
import qualified Moonlight.Planar.Dcel as Dcel
import Moonlight.Planar.Internal.BoxedPaged (boxedMaterializedPageCount)
import Moonlight.Planar.Internal.Paged (PublicationStats (..), pagedOverlayPageCount)
import Moonlight.Planar.Internal.Representation
  ( Triangulation
      ( triConstraint
      , triDirectedData
      , triFaceData
      , triFaceEdge
      , triHalfTopology
      , triPointX
      , triPointY
      , triUndirectedData
      , triVertexData
      , triVertexOut
      )
  )

type Mesh = DelaunayTriangulation ()
type SiteMesh = DelaunayTriangulation (Point)
type ConstrainedMesh = ConstrainedDelaunayTriangulation ()

data ExtensionSignature = ExtensionSignature
  { extensionOmegaGeometry :: !(Set.Set (Point, Point, Point))
  , extensionGammaGeometry :: !(Set.Set (Point, Point))
  , extensionSeamGeometry :: !(Set.Set (Point, Point, Point))
  , extensionConstraintGeometry :: !(Set.Set (Point, Point))
  }
  deriving stock (Eq, Show)

data PreparedSeparatedExtension = PreparedSeparatedExtension
  { preparedBase :: !ConstrainedMesh
  , preparedExtension :: !ConstrainedMesh
  , preparedSignature :: !ExtensionSignature
  }

-- | The scale comparison retains only observations emitted by the local
-- interpreters.  Resident face count is deliberately absent: it is the
-- independent variable, not permission to normalize local work by A.
data ExtensionLocalityReceipt = ExtensionLocalityReceipt
  { extensionLocalitySeamPublication :: !PublicationStats
  , extensionLocalityRefinementPublication :: !PublicationStats
  , extensionLocalityCachedFrontierPointReads :: !Int
  , extensionLocalityValidationClosure :: !ClosureStats
  , extensionLocalityRefinementStats :: !BuildStats
  , extensionLocalityFinalOmegaGeometry :: !(Set.Set (Point, Point, Point))
  , extensionLocalityAddedVertices :: !Int
  , extensionLocalityFinalOmegaFaces :: !Int
  , extensionLocalityTouchedEdges :: !Int
  , extensionLocalityCreatedFaces :: !Int
  , extensionLocalityInterfaceReads :: !Int
  , extensionLocalityBoundaryCrossings :: !Int
  }
  deriving stock (Eq, Show)

rectangleBoundarySites :: Double -> Double -> [Point]
rectangleBoundarySites left right =
  [ Point left (-1)
  , Point right (-1)
  ]
    <> fmap (Point right) [-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75]
    <> [Point right 1, Point left 1]

rectangleCollarSites :: Double -> Double -> [Point]
rectangleCollarSites left right =
  let ys = [-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75]
      width = right - left
   in fmap (Point (left + 0.2 * width)) ys
        <> fmap (Point (right - 0.2 * width)) ys

rectangleWorldSites :: Word64 -> Int -> Double -> Double -> [Point]
rectangleWorldSites seed count left right =
  rectangleFixedSites left right
    <> fmap
      (\(Point x y) ->
         Point
           (left + 0.35 * (right - left) + 0.15 * (right - left) * (x + 1))
           (-0.6 + 0.6 * (y + 1)))
      (randomPoints seed (max 0 (count - length (rectangleFixedSites left right))))

rectangleFixedSites :: Double -> Double -> [Point]
rectangleFixedSites left right = rectangleBoundarySites left right <> rectangleCollarSites left right

rectangleContour :: Int -> [(Int, Int)]
rectangleContour count =
  [ (index, (index + 1) `mod` count)
  | index <- [0 .. count - 1]
  ]

rectangleConstraintPairs :: [(Int, Int)]
rectangleConstraintPairs =
  rectangleContour boundaryCount
    <> chain collarLeftStart
    <> chain collarRightStart
 where
  boundaryCount = length (rectangleBoundarySites 0 1)
  collarLength = length (rectangleCollarSites 0 1) `quot` 2
  collarLeftStart = boundaryCount
  collarRightStart = collarLeftStart + collarLength
  chain start =
    [ (start + index, start + index + 1)
    | index <- [0 .. collarLength - 2]
    ]

contourPairs :: [Point] -> [(Point, Point)]
contourPairs points =
  case points of
    [] -> []
    first : rest -> zip points (rest <> [first])

orderedPointPair :: (Point, Point) -> (Point, Point)
orderedPointPair (first, second)
  | first <= second = (first, second)
  | otherwise = (second, first)

contourGeometry :: [Point] -> Set.Set (Point, Point)
contourGeometry = Set.fromList . fmap orderedPointPair . contourPairs

rectangleConstraintGeometry :: Double -> Double -> Set.Set (Point, Point)
rectangleConstraintGeometry left right =
  let boundary = rectangleBoundarySites left right
      collar = rectangleCollarSites left right
      collarLength = length collar `quot` 2
      (leftCollar, rightCollar) = splitAt collarLength collar
   in Set.unions
        [ contourGeometry boundary
        , openChainGeometry leftCollar
        , openChainGeometry rightCollar
        ]

openChainGeometry :: [Point] -> Set.Set (Point, Point)
openChainGeometry points =
  Set.fromList
    [ orderedPointPair pair
    | pair <- zip points (drop 1 points)
    ]

constraintGeometry :: ConstrainedMesh -> Set.Set (Point, Point)
constraintGeometry mesh =
  Set.fromList
    [ orderedPointPair (segmentStart segment, segmentEnd segment)
    | segment <- V.toList (constraintSegments mesh)
    ]

constrainedGeometryMesh :: [Point] -> IO ConstrainedMesh
constrainedGeometryMesh points =
  geometryOnlyPublication . buildTriangulation
    <$> requireRight
      ( constrainedDelaunay
          unitElementDefaults
          (V.fromList points)
          (V.fromList rectangleConstraintPairs)
      )

boundaryOfPermittedFaces
  :: ConstrainedMesh
  -> Set.Set FaceId
  -> Set.Set UndirectedEdgeId
boundaryOfPermittedFaces mesh faces =
  Set.fromList
    [ asUndirected edge
    | face <- Set.toList faces
    , edge <- Dcel.faceDirectedEdges mesh face
    , let adjacent = Dcel.incidentFace mesh (reverseEdge edge)
    , adjacent /= Dcel.outerFace
    , Set.notMember adjacent faces
    ]

faceGeometry :: ConstrainedMesh -> FaceId -> Maybe (Point, Point, Point)
faceGeometry mesh face =
  case sort (fmap (Dcel.vertexPoint mesh) (Dcel.faceVertices mesh face)) of
    [first, second, third] -> Just (first, second, third)
    _ -> Nothing

faceGeometrySet
  :: ConstrainedMesh
  -> Set.Set FaceId
  -> Either String (Set.Set (Point, Point, Point))
faceGeometrySet mesh faces =
  Set.fromList
    <$> traverse
      (\face ->
         maybe
           (Left ("local refinement receipt named a non-triangular face: " <> show face))
           Right
           (faceGeometry mesh face))
      (Set.toList faces)

edgeGeometry :: ConstrainedMesh -> UndirectedEdgeId -> (Point, Point)
edgeGeometry mesh edge =
  let (fromVertex, toVertex) = Dcel.undirectedEndpoints mesh edge
   in orderedPointPair
        (Dcel.vertexPoint mesh fromVertex, Dcel.vertexPoint mesh toVertex)

extensionSignature :: ConstrainedSeamResult () -> Either String ExtensionSignature
extensionSignature receipt =
  let joined = constrainedSeamResultTriangulation receipt
      permitted =
        Set.union
          (Set.fromList (fmap constrainedSeamTargetFace (V.toList (constrainedSeamRightFaceEvidence receipt))))
          (Set.fromList (V.toList (constrainedSeamJoinFaces receipt)))
      interface = boundaryOfPermittedFaces joined permitted
   in do
        seamFaces <-
          Set.fromList
            <$> traverse
              (\face ->
                 case faceGeometry joined face of
                   Just geometry -> Right geometry
                   Nothing -> Left "seam receipt named a non-triangular face")
              (V.toList (constrainedSeamJoinFaces receipt))
        let omegaFaces =
              Set.union
                ( Set.fromList
                    [ ( constrainedSeamFaceFirstPoint evidence
                      , constrainedSeamFaceSecondPoint evidence
                      , constrainedSeamFaceThirdPoint evidence
                      )
                    | evidence <- V.toList (constrainedSeamRightFaceEvidence receipt)
                    ]
                )
                seamFaces
        pure
          ExtensionSignature
            { extensionOmegaGeometry = omegaFaces
            , extensionGammaGeometry = Set.fromList (fmap (edgeGeometry joined) (Set.toList interface))
            , extensionSeamGeometry = seamFaces
            , extensionConstraintGeometry = constraintGeometry joined
            }

refinementInterfaceGeometry :: ConstrainedMesh -> Set.Set FaceId -> Set.Set (Point, Point)
refinementInterfaceGeometry mesh faces =
  Set.fromList
    [ edgeGeometry mesh edge
    | edge <- Set.toList (boundaryOfPermittedFaces mesh faces)
    ]

assertLocalPublicationStats :: String -> PublicationStats -> IO ()
assertLocalPublicationStats label stats = do
  unless (publicationUnboxedBasePageEnumerations stats == 0) $
    fail (label <> " enumerated resident unboxed base pages")
  unless (publicationUnboxedBasePageFreezes stats == 0) $
    fail (label <> " froze resident unboxed base pages")
  unless (publicationBoxedBasePageEnumerations stats == 0) $
    fail (label <> " enumerated resident boxed base pages")
  unless (publicationBoxedBasePageFreezes stats == 0) $
    fail (label <> " froze resident boxed base pages")

-- These are post-publication representation observations. They do not count
-- base-page opens or freeze enumeration and therefore are not a proof that
-- those events were absent from the timed action.
publicationPageReceipt :: ConstrainedMesh -> (Int, Int)
publicationPageReceipt mesh =
  ( sum
      [ pagedOverlayPageCount (triPointX mesh)
      , pagedOverlayPageCount (triPointY mesh)
      , pagedOverlayPageCount (triVertexOut mesh)
      , pagedOverlayPageCount (triHalfTopology mesh)
      , pagedOverlayPageCount (triFaceEdge mesh)
      , pagedOverlayPageCount (triConstraint mesh)
      ]
  , sum
      [ boxedMaterializedPageCount (triVertexData mesh)
      , boxedMaterializedPageCount (triDirectedData mesh)
      , boxedMaterializedPageCount (triUndirectedData mesh)
      , boxedMaterializedPageCount (triFaceData mesh)
      ]
  )

benchmarks :: IO ()
benchmarks = do
  benchmarkBalanced 20_000
  benchmarkSeparated 20_000
  benchmarkSkew 20_000 200
  benchmarkOverlap 20_000
  benchmarkTournament 20_000 16
  benchmarkSpatialTournament 20_000 16
  benchmarkTournamentScaling 20_000
  benchmarkCanonicalize 20_000
  benchmarkSetAlgebra 20_000

publicationBenchmarks :: IO ()
publicationBenchmarks = do
  -- These are face bands, not site-count labels.  A planar Delaunay mesh is
  -- approximately twice as many faces as sites, so the resident fixtures use
  -- 125k and 250k sites to exercise the requested 250k/500k-face lanes, and
  -- the incoming small world uses 15k sites for its approximately 30k faces.
  prepared250k <- preparePersistentSeparatedExtension 125_000 15_000
  prepared500k <- preparePersistentSeparatedExtension 250_000 15_000
  assertFaceBand "250k-faces" 245_000 255_000 (preparedBase prepared250k)
  assertFaceBand "500k-faces" 490_000 510_000 (preparedBase prepared500k)
  assertFaceBand "30k-extension-faces" 29_000 31_000 (preparedExtension prepared250k)
  assertFaceBand "30k-extension-faces-repeat" 29_000 31_000 (preparedExtension prepared500k)
  unless (preparedSignature prepared250k == preparedSignature prepared500k) $
    fail "separated extension changed its fixed collar, Gamma, or seam geometry across A sizes"
  receipt250k <- benchmarkPersistentSeparatedExtension "250k-faces" prepared250k
  receipt500k <- benchmarkPersistentSeparatedExtension "500k-faces" prepared500k
  assertIdenticalFrontierLocality receipt250k receipt500k

assertIdenticalFrontierLocality
  :: ExtensionLocalityReceipt
  -> ExtensionLocalityReceipt
  -> IO ()
assertIdenticalFrontierLocality smaller larger = do
  assertPublicationAccounting "250k seam" (extensionLocalitySeamPublication smaller)
  assertPublicationAccounting "500k seam" (extensionLocalitySeamPublication larger)
  assertPublicationAccounting "250k refinement" (extensionLocalityRefinementPublication smaller)
  assertPublicationAccounting "500k refinement" (extensionLocalityRefinementPublication larger)
  assertPageAlignedPublicationLocality
    "seam"
    (extensionLocalitySeamPublication smaller)
    (extensionLocalitySeamPublication larger)
  assertPageAlignedPublicationLocality
    "refinement"
    (extensionLocalityRefinementPublication smaller)
    (extensionLocalityRefinementPublication larger)
  unless
    ( extensionLocalityCachedFrontierPointReads smaller
        == extensionLocalityCachedFrontierPointReads larger
    ) $
    fail "identical-frontier cached-read work grew with resident A"
  unless
    ( extensionLocalityValidationClosure smaller
        == extensionLocalityValidationClosure larger
    ) $
    fail "identical local refinement produced an A-dependent closure"
  unless
    ( and
        [ extensionLocalityRefinementStats smaller == extensionLocalityRefinementStats larger
        , extensionLocalityFinalOmegaGeometry smaller == extensionLocalityFinalOmegaGeometry larger
        , extensionLocalityAddedVertices smaller == extensionLocalityAddedVertices larger
        , extensionLocalityFinalOmegaFaces smaller == extensionLocalityFinalOmegaFaces larger
        , extensionLocalityTouchedEdges smaller == extensionLocalityTouchedEdges larger
        , extensionLocalityCreatedFaces smaller == extensionLocalityCreatedFaces larger
        , extensionLocalityInterfaceReads smaller == extensionLocalityInterfaceReads larger
        , extensionLocalityBoundaryCrossings smaller == extensionLocalityBoundaryCrossings larger
        ]
    ) $
    fail "identical-frontier refinement changed its canonical local work receipt"

-- The receipt counts page opens and copied cells at the storage boundary.  It
-- intentionally does not update a counter for every logical cell write: that
-- instrumentation used to put a strict fourteen-field STRef update on every
-- dense build mutation.  Local page accounting is exact and remains the
-- observation used by the extension lane.
assertPublicationAccounting :: String -> PublicationStats -> IO ()
assertPublicationAccounting label stats = do
  unless
    ( publicationUnboxedBasePageOpens stats
        == publicationUnboxedDirtyBasePages stats
        && publicationBoxedBasePageOpens stats
          == publicationBoxedDirtyBasePages stats
        && publicationUnboxedCopiedCells stats
          <= 1_024 * publicationUnboxedBasePageOpens stats
        && publicationBoxedCopiedCells stats
          <= 256 * publicationBoxedBasePageOpens stats
    ) $
    fail (label <> " page publication accounting is inconsistent")

-- A larger resident shifts the same local write intervals within fixed-size
-- pages.  At most two interval boundaries per store may therefore change page
-- classification or copy one extra page; the semantic local-work receipt below
-- remains exact.
assertPageAlignedPublicationLocality :: String -> PublicationStats -> PublicationStats -> IO ()
assertPageAlignedPublicationLocality label smaller larger = do
  let unboxedPageSlack :: Int
      unboxedPageSlack = 2 * 6
      boxedPageSlack :: Int
      boxedPageSlack = 2 * 4
      dirtyUnboxed :: PublicationStats -> Int
      dirtyUnboxed stats =
        publicationUnboxedDirtyBasePages stats
          + publicationUnboxedDirtyAppendedPages stats
      dirtyBoxed :: PublicationStats -> Int
      dirtyBoxed stats =
        publicationBoxedDirtyBasePages stats
          + publicationBoxedDirtyAppendedPages stats
      within :: Int -> Int -> Int -> Bool
      within slack left right = abs (left - right) <= slack
  unless
    ( and
        [ within unboxedPageSlack (dirtyUnboxed smaller) (dirtyUnboxed larger)
        , within (1_024 * unboxedPageSlack) (publicationUnboxedCopiedCells smaller) (publicationUnboxedCopiedCells larger)
        , within boxedPageSlack (dirtyBoxed smaller) (dirtyBoxed larger)
        , within (256 * boxedPageSlack) (publicationBoxedCopiedCells smaller) (publicationBoxedCopiedCells larger)
        ]
    ) $
    fail ("identical-frontier " <> label <> " publication escaped its fixed page-alignment envelope")

assertFaceBand :: String -> Int -> Int -> ConstrainedMesh -> IO ()
assertFaceBand label lower upper mesh = do
  let actual = numFaces mesh
  unless (lower <= actual && actual <= upper) $
    fail
      ( "publication-separated-"
          <> label
          <> " expected base faces in ["
          <> show lower
          <> ","
          <> show upper
          <> "], got "
          <> show actual
      )

-- | Two halves of one point set, joined.
--
-- The balanced pair rebuilds from its combined site set. The input-order and
-- ranked lanes distinguish ordinary construction from construction whose
-- vertex numbering is already canonical.
benchmarkBalanced :: Int -> IO ()
benchmarkBalanced total = do
  let sites = randomPoints 0x9e3779b97f4a7c15 total
      (left, right) = splitAt (total `div` 2) sites
  leftMesh <- geometryMesh left
  rightMesh <- geometryMesh right
  _ <- evaluate (force (leftMesh, rightMesh))
  _ <- timedValue "join-balanced" (requireRight (union leftMesh rightMesh))
  _ <- timedValue "join-balanced-rebuild-input-order" (geometryMesh sites)
  _ <- timedValue "join-balanced-rebuild-ranked-order" (geometryMesh (canonical sites))
  pure ()

-- | Two operands whose sites are separated by a vertical line.
--
-- This is the stratum a seam merge is defined on, and the number here is the
-- one it has to beat: the reference schedule does not know the operands are
-- separated and rebuilds the union regardless. A linear-time merge wins
-- asymptotically over an @O(n log n)@ rebuild; whether it wins at the sizes
-- anything actually merges at is this measurement and not an argument.
--
-- Three gap widths, because the seam's work is the cross-edge chain and the
-- deletions it drives, and how far the two clouds stand apart decides how much
-- of each interior the chain disturbs. A distant pair is the easy case — the
-- chain is short and nothing inside either operand dies. An abutting pair is
-- the hard one.
benchmarkSeparated :: Int -> IO ()
benchmarkSeparated total = do
  let half = total `div` 2
      sites = randomPoints 0xd1b54a32d192ed03 half
      extent = 2 * maximum [abs x | Point x _ <- sites]
  leftMesh <- geometryMesh sites
  _ <- evaluate (force leftMesh)
  mapM_
    ( \(name, gap) -> do
        let shifted = [Point (x + gap * extent) y | Point x y <- sites]
        rightMesh <- geometryMesh shifted
        _ <- evaluate (force rightMesh)
        _ <- timedValue ("join-separated-" <> name) (requireRight (union leftMesh rightMesh))
        pure ()
    )
    [("distant" :: String, 8), ("near", 2), ("abutting", 1.02)]

-- | A large mesh joined with a small one. Rebuilding costs the whole union;
-- inserting the small operand's sites into the large mesh costs only the
-- insertions. This is the ratio that says whether a skewed lane is worth
-- having, and it needs no new algorithm — 'insertMany' is already the
-- one-transaction batch path.
--
-- Both lanes carry the same vertex payload so the comparison is of the
-- schedules and not of the stores.
benchmarkSkew :: Int -> Int -> IO ()
benchmarkSkew large small = do
  let bulk = randomPoints 0xbf58476d1ce4e5b9 large
      addition = randomPoints 0x94d049bb133111eb small
  bulkMesh <- siteMesh bulk
  _ <- evaluate (force bulkMesh)
  _ <-
    timedValue
      "join-skew-rebuild"
      (siteMesh (canonical (bulk <> addition)))
  _ <-
    timedValue
      "join-skew-insert-many"
      (buildTriangulation <$> requireRight (insertMany bulkMesh (V.fromList addition)))
  pure ()

-- | Prepare the fixed-frontier workload once. The preview join is outside the
-- timed lanes: it certifies that changing only the resident interior did not
-- alter the B/J geometry or the exact Gamma section.
preparePersistentSeparatedExtension :: Int -> Int -> IO PreparedSeparatedExtension
preparePersistentSeparatedExtension baseCount extensionCount = do
  let baseSites = rectangleWorldSites 0x243f6a8885a308d3 baseCount (-1) 0
      extensionSites = rectangleWorldSites 0x13198a2e03707344 extensionCount 4 5
      expectedBaseConstraints = rectangleConstraintGeometry (-1) 0
      expectedExtensionConstraints = rectangleConstraintGeometry 4 5
  base <- constrainedGeometryMesh baseSites
  extension <- constrainedGeometryMesh extensionSites
  _ <- evaluate (force (base, extension))
  unless (constraintGeometry base == expectedBaseConstraints) $
    fail "fixed A collar constraints were not retained by the source build"
  unless (constraintGeometry extension == expectedExtensionConstraints) $
    fail "fixed B collar constraints were not retained by the source build"
  preview <- requireRight (joinSeparatedConstrained (\_ _ -> True) defaultRefinementParameters base extension)
  signature <- requireRight (extensionSignature preview)
  let expectedConstraints = Set.union expectedBaseConstraints expectedExtensionConstraints
      actualConstraints = extensionConstraintGeometry signature
  unless (expectedConstraints `Set.isSubsetOf` actualConstraints) $
    fail
      ( "source-preserving seam did not retain the fixed A/B contour constraints; missing="
          <> show (Set.toAscList (Set.difference expectedConstraints actualConstraints))
      )
  pure
    PreparedSeparatedExtension
      { preparedBase = base
      , preparedExtension = extension
      , preparedSignature = signature
      }

-- | The extension lane is the operation the persistent-world plan actually
-- promises: geometry-only publication prepares the two frontier indexes once,
-- the caller-left constrained source stays resident, and refinement is
-- restricted to the right-source faces plus the seam faces emitted by that
-- interpretation. Construction, frontier preparation, receipt descent, and
-- the final validity observation are outside the timed extension actions;
-- neither action invokes generic union, canonical numbering, or global
-- validation.
benchmarkPersistentSeparatedExtension :: String -> PreparedSeparatedExtension -> IO ExtensionLocalityReceipt
benchmarkPersistentSeparatedExtension label prepared = do
  let base = preparedBase prepared
      extension = preparedExtension prepared
  joinedReceipt <-
    timedValue
      ("publication-separated-" <> label <> "-join")
      (requireRight (joinSeparatedConstrained (\_ _ -> True) defaultRefinementParameters base extension))
  let joined = constrainedSeamResultTriangulation joinedReceipt
      permitted =
        Set.union
          (Set.fromList (fmap constrainedSeamTargetFace (V.toList (constrainedSeamRightFaceEvidence joinedReceipt))))
          (Set.fromList (V.toList (constrainedSeamJoinFaces joinedReceipt)))
      interface = boundaryOfPermittedFaces joined permitted
  parameters <-
    requireRight
      ( fmap
          (withConstraintPreservation True . withoutMaximumRadiusEdgeRatio)
          ( withAdditionalVertexBudget 4_096 defaultRefinementParameters
              >>= withMaximumArea 0.0002
          )
      )
  signature <- requireRight (extensionSignature joinedReceipt)
  unless (signature == preparedSignature prepared) $
    fail ("publication-separated-" <> label <> " changed its prepared Omega/Gamma/J geometry")
  unless (refinementInterfaceGeometry joined permitted == extensionGammaGeometry signature) $
    fail ("publication-separated-" <> label <> " changed its certified Gamma geometry")
  refined <-
    timedValue
      ("publication-separated-" <> label <> "-local-refine")
      ( requireRight
          ( refineWithinDomain
              (const ())
              parameters
              permitted
              joined
          )
      )
  let result = refinementDomainResult refined
      receipt = refinementDomainReceipt refined
      seamPublication = constrainedSeamPublicationStats joinedReceipt
      refinementPublication = refinementPublicationStats receipt
      validationClosure = refinementClosureStats receipt
      finalFaces = Set.fromList (V.toList (refinementFinalPermittedFaces receipt))
  finalOmegaGeometry <- requireRight (faceGeometrySet (refinedTriangulation result) finalFaces)
  assertLocalPublicationStats
    ("publication-separated-" <> label <> " seam")
    seamPublication
  assertLocalPublicationStats
    ("publication-separated-" <> label <> " refinement")
    refinementPublication
  unless
    ( publicationUnboxedDirtyBasePages seamPublication
        + publicationUnboxedDirtyAppendedPages seamPublication
        > 0
    ) $
    fail ("publication-separated-" <> label <> " seam published no appended unboxed writes")
  unless
    ( publicationUnboxedDirtyBasePages refinementPublication
        + publicationUnboxedDirtyAppendedPages refinementPublication
        > 0
    ) $
    fail ("publication-separated-" <> label <> " refinement published no appended unboxed writes")
  unless (refinementComplete result) $
    fail ("publication-separated-" <> label <> " exhausted its finite local refinement budget")
  unless (refinementInterfaceBoundaryReads receipt > 0) $
    fail ("publication-separated-" <> label <> " performed no positive Gamma-boundary descent")
  unless (refinementAddedVertices result > 0) $
    fail ("publication-separated-" <> label <> " performed no positive local refinement")
  case validateTriangulation (refinedTriangulation result) of
    [] -> pure ()
    violations -> fail ("publication-separated-" <> label <> " invalid: " <> show violations)
  putStrLn
    ( "publication-separated-"
        <> label
        <> "-base-faces="
        <> show (numFaces base)
        <> " extension-faces="
        <> show (numFaces extension)
        <> " receipt: omega="
        <> show (Set.size permitted)
        <> " b-faces="
        <> show (V.length (constrainedSeamRightFaceEvidence joinedReceipt))
        <> " j-faces="
        <> show (V.length (constrainedSeamJoinFaces joinedReceipt))
        <> " a-constraints="
        <> show (constrainedSeamLeftConstraintCount joinedReceipt)
        <> " gamma="
        <> show (Set.size interface)
        <> " final-omega="
        <> show (V.length (refinementFinalPermittedFaces receipt))
        <> " steiner="
        <> show (refinementAddedVertices result)
    )
  renderPublicationStats
    ("publication-separated-" <> label <> "-seam-publication")
    seamPublication
  putStrLn
    ( "publication-separated-"
        <> label
        <> "-seam-cached-frontier-point-reads="
        <> show (constrainedSeamCachedFrontierPointReads joinedReceipt)
    )
  renderPublicationStats
    ("publication-separated-" <> label <> "-refinement-publication")
    refinementPublication
  putStrLn
    ( "publication-separated-"
        <> label
        <> "-closure: faces="
        <> show (closureFaces validationClosure)
        <> " directed-edges="
        <> show (closureDirectedEdges validationClosure)
        <> " vertices="
        <> show (closureVertices validationClosure)
        <> " interface-pairs="
        <> show (closureInterfacePairs validationClosure)
        <> " constraint-pairs="
        <> show (closureConstraintPairs validationClosure)
    )
  let (unboxedOverlayPages, boxedMaterializedPages) = publicationPageReceipt (refinedTriangulation result)
  putStrLn
    ( "publication-separated-"
        <> label
        <> "-observed-final-pages: unboxed-overlay="
        <> show unboxedOverlayPages
        <> " boxed-materialized="
        <> show boxedMaterializedPages
        <> " local-touched-edges="
        <> show (V.length (refinementTouchedEdges receipt))
        <> " local-created-faces="
        <> show (V.length (refinementCreatedFaces receipt))
    )
  pure
    ExtensionLocalityReceipt
      { extensionLocalitySeamPublication = seamPublication
      , extensionLocalityRefinementPublication = refinementPublication
      , extensionLocalityCachedFrontierPointReads = constrainedSeamCachedFrontierPointReads joinedReceipt
      , extensionLocalityValidationClosure = validationClosure
      , extensionLocalityRefinementStats = refinementStats result
      , extensionLocalityFinalOmegaGeometry = finalOmegaGeometry
      , extensionLocalityAddedVertices = refinementAddedVertices result
      , extensionLocalityFinalOmegaFaces = Set.size finalFaces
      , extensionLocalityTouchedEdges = V.length (refinementTouchedEdges receipt)
      , extensionLocalityCreatedFaces = V.length (refinementCreatedFaces receipt)
      , extensionLocalityInterfaceReads = refinementInterfaceBoundaryReads receipt
      , extensionLocalityBoundaryCrossings = refinementAttemptedBoundaryCrossings receipt
      }

renderPublicationStats :: String -> PublicationStats -> IO ()
renderPublicationStats label stats =
  putStrLn
    ( label
        <> ": unboxed-base-enumerations="
        <> show (publicationUnboxedBasePageEnumerations stats)
        <> " unboxed-base-opens="
        <> show (publicationUnboxedBasePageOpens stats)
        <> " unboxed-base-freezes="
        <> show (publicationUnboxedBasePageFreezes stats)
        <> " unboxed-dirty-base-pages="
        <> show (publicationUnboxedDirtyBasePages stats)
        <> " unboxed-dirty-appended-pages="
        <> show (publicationUnboxedDirtyAppendedPages stats)
        <> " unboxed-copied-cells="
        <> show (publicationUnboxedCopiedCells stats)
        <> " boxed-base-enumerations="
        <> show (publicationBoxedBasePageEnumerations stats)
        <> " boxed-base-opens="
        <> show (publicationBoxedBasePageOpens stats)
        <> " boxed-base-freezes="
        <> show (publicationBoxedBasePageFreezes stats)
        <> " boxed-dirty-base-pages="
        <> show (publicationBoxedDirtyBasePages stats)
        <> " boxed-dirty-appended-pages="
        <> show (publicationBoxedDirtyAppendedPages stats)
        <> " boxed-copied-cells="
        <> show (publicationBoxedCopiedCells stats)
    )

-- | The same operand sizes at three overlap fractions. A join is sized by the
-- union, so wholly overlapping operands must cost what one of them costs.
benchmarkOverlap :: Int -> IO ()
benchmarkOverlap total = do
  let sites = randomPoints 0x2545f4914f6cdd1d total
      half = total `div` 2
  disjointLeft <- geometryMesh (take half sites)
  disjointRight <- geometryMesh (drop half sites)
  halfLeft <- geometryMesh (take half sites)
  halfRight <- geometryMesh (drop (half `div` 2) (take (half + half `div` 2) sites))
  sameLeft <- geometryMesh (take half sites)
  sameRight <- geometryMesh (reverse (take half sites))
  _ <- evaluate (force (disjointLeft, disjointRight, halfLeft, halfRight, sameLeft, sameRight))
  _ <- timedValue "join-overlap-000" (requireRight (union disjointLeft disjointRight))
  _ <- timedValue "join-overlap-050" (requireRight (union halfLeft halfRight))
  _ <- timedValue "join-overlap-100" (requireRight (union sameLeft sameRight))
  pure ()

-- | 'unions' is a balanced tournament and not a fold, which is a cost claim
-- and therefore has to be measured rather than asserted. A fold republishes an
-- accumulator that grows by one shard per step.
--
-- This lane once reported the fold as the faster of the two, which was true and
-- was not a fact about the schedules: @joinBalanced@ carried no specialization,
-- so every join inside the tournament ran through a dictionary while
-- the left-associated schedule at a known element type ran specialized. The tournament was
-- paying twice for arithmetic, and that swamped the asymptotic gap it was
-- supposed to be demonstrating.
--
-- The shards are dealt round-robin, so every one of them spans the whole extent
-- and no join in the tournament is separable. That is deliberate: it is the
-- adversarial sharding, and it measures the operator with no structure to
-- exploit. 'benchmarkSpatialTournament' is the same tournament over the
-- sharding a caller who wanted it to be fast would actually choose.
benchmarkTournament :: Int -> Int -> IO ()
benchmarkTournament total shardCount = do
  let sites = randomPoints 0x14057b7ef767814f total
      indexed = zip [0 :: Int ..] sites
  shards <-
    traverse
      (\shard -> geometryMesh [site | (index, site) <- indexed, index `mod` shardCount == shard])
      [0 .. shardCount - 1]
  _ <- evaluate (force shards)
  _ <- timedValue "join-tournament" (requireRight (unions shards))
  _ <- timedValue "join-left-fold" (requireRight (unionsLeftAssociated shards))
  pure ()

-- | Where the tournament's advantage over the fold actually appears.
--
-- A fold republishes an accumulator that grows by one shard per step, so it
-- rebuilds @Θ(nk)@ sites over @k@ shards where halving rebuilds @Θ(n log k)@.
-- That is a statement about @k@, and at the sixteen shards the lane above uses
-- the predicted factor is barely two — small enough to be swamped by the
-- per-join costs both schedules pay fifteen times each. This sweep is here
-- because a cost claim that only holds asymptotically has to say at what size
-- it starts holding, and the answer has to be measured rather than asserted.
benchmarkTournamentScaling :: Int -> IO ()
benchmarkTournamentScaling total =
  mapM_
    ( \shardCount -> do
        let sites = randomPoints 0x9e3779b97f4a7c15 total
            indexed = zip [0 :: Int ..] sites
        shards <-
          traverse
            (\shard -> geometryMesh [site | (index, site) <- indexed, index `mod` shardCount == shard])
            [0 .. shardCount - 1]
        _ <- evaluate (force shards)
        _ <- timedValue ("join-shards-" <> show shardCount <> "-tournament") (requireRight (unions shards))
        _ <- timedValue ("join-shards-" <> show shardCount <> "-fold") (requireRight (unionsLeftAssociated shards))
        pure ()
    )
    [4 :: Int, 16, 64]


-- | The same tournament over shards cut by abscissa rather than dealt.
--
-- This is the workload a seam schedule exists for, and the only one where it
-- can pay off more than once. Shards cut into contiguous x-ranges are pairwise
-- separated; so is every intermediate result, because the union of two adjacent
-- ranges is a range. Every one of the fifteen joins in the tournament is
-- therefore separable — the tournament /is/ the divide-and-conquer recursion,
-- entered from the leaves.
--
-- Against the reference schedule this must cost about what the dealt
-- tournament costs, since a rebuild cannot tell the two shardings apart. That
-- agreement is the baseline; the gap that opens between these two lanes is the
-- whole return on a merge kernel.
benchmarkSpatialTournament :: Int -> Int -> IO ()
benchmarkSpatialTournament total shardCount = do
  let sites = randomPoints 0x3c6ef372fe94f82a total
      ordered = sortBy (comparing (\(Point x _) -> x)) sites
      width = (total + shardCount - 1) `div` shardCount
  shards <-
    traverse
      (\shard -> geometryMesh (take width (drop (shard * width) ordered)))
      [0 .. shardCount - 1]
  _ <- evaluate (force shards)
  _ <- timedValue "join-spatial-tournament" (requireRight (unions shards))
  pure ()

-- | The renumbering pass on its own, against the construction it follows.
benchmarkCanonicalize :: Int -> IO ()
benchmarkCanonicalize total = do
  let sites = randomPoints 0x27d4eb2f165667c5 total
  mesh <- geometryMesh sites
  _ <- evaluate (force mesh)
  _ <- timedValue "canonicalize-alone" (evaluate . force =<< requireRight (canonicalize mesh))
  pure ()

-- | The shared canonical rebuild boundary under half overlap, on both its
-- geometry-only specializations and its annotation-preserving surface. Setup
-- and source publication are forced before every clock; the measurements are
-- therefore the exact site classification, rebuild, canonical publication and
-- payload transport the public operations own.
benchmarkSetAlgebra :: Int -> IO ()
benchmarkSetAlgebra total = do
  let common = total `quot` 2
      sites = randomPoints 0x6A09E667F3BCC909 (total + common)
      leftPoints = take total sites
      rightPoints = drop common sites
  leftGeometry <- geometryMesh leftPoints
  rightGeometry <- geometryMesh rightPoints
  emptyGeometry <- requireRight (unions [])
  _ <- evaluate (force (leftGeometry, rightGeometry, emptyGeometry))
  _ <- timedValue "set-intersection-unit" (requireRight (intersection leftGeometry rightGeometry))
  _ <- timedValue "set-difference-unit" (requireRight (difference leftGeometry rightGeometry))
  _ <-
    timedValue
      "set-symmetric-difference-unit"
      (requireRight (symmetricDifference leftGeometry rightGeometry))
  _ <-
    timedValue
      "set-difference-right-empty"
      (requireRight (difference leftGeometry emptyGeometry))
  _ <-
    timedValue
      "set-symmetric-difference-left-empty"
      (requireRight (symmetricDifference emptyGeometry leftGeometry))
  _ <- timedValue "set-relation-half-overlap" (evaluate (siteRelation leftGeometry rightGeometry))
  leftAnnotated <- siteMesh leftPoints
  rightAnnotated <- siteMesh rightPoints
  _ <- evaluate (force (leftAnnotated, rightAnnotated))
  _ <-
    timedValue
      "set-intersection-annotated"
      (requireRight (intersectionWith (,) leftAnnotated rightAnnotated))
  _ <-
    timedValue
      "set-difference-annotated"
      (requireRight (difference leftAnnotated rightAnnotated))
  _ <-
    timedValue
      "set-symmetric-difference-annotated"
      (requireRight (symmetricDifference leftAnnotated rightAnnotated))
  pure ()


canonical :: [Point] -> [Point]
canonical points = [Point x y | (x, y) <- dropAdjacentDuplicates (sort [(x, y) | Point x y <- points])]

dropAdjacentDuplicates :: Eq a => [a] -> [a]
dropAdjacentDuplicates (first : second : rest)
  | first == second = dropAdjacentDuplicates (second : rest)
  | otherwise = first : dropAdjacentDuplicates (second : rest)
dropAdjacentDuplicates rest = rest

geometryMesh :: [Point] -> IO Mesh
geometryMesh points = requireRight (delaunayGeometry (V.fromList points))

siteMesh :: [Point] -> IO SiteMesh
siteMesh points =
  buildTriangulation <$> requireRight (delaunay unitElementDefaults (V.fromList points))

unionsLeftAssociated :: [Mesh] -> Either BuildError Mesh
unionsLeftAssociated meshes = unions [] >>= \identity -> foldM union identity meshes
