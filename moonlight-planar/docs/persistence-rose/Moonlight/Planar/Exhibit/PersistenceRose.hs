{-# LANGUAGE NamedFieldPuns #-}

-- | A deterministic, pure observatory assembled from Moonlight's existing
-- geometry, exact filtration, interpolation, and persistence owners.
module Moonlight.Planar.Exhibit.PersistenceRose
  ( PersistenceRoseError (..),
    PersistenceRoseReceipt (..),
    PersistenceRoseScene,
    PersistenceRoseFrame,
    PersistenceRoseAnimationReceipt (..),
    AlphaEclipseReceipt (..),
    AlphaEclipseScene,
    buildPersistenceRoseScene,
    buildAlphaEclipseScene,
    persistenceRoseReceipt,
    alphaEclipseReceipt,
    renderPersistenceRoseReceipt,
    renderAlphaEclipseReceipt,
    renderPersistenceRosePoster,
    renderAlphaEclipse,
    persistenceRoseFrameSpecs,
    renderPersistenceRoseFrame,
    persistenceRoseAnimationReceipt,
    renderPersistenceRoseAnimationReceipt,
    stampPersistenceRoseGif,
    verifyPersistenceRoseGif,
  )
where

import Control.Monad (foldM)
import Control.Monad.ST (runST)
import Data.Bifunctor (first)
import Data.Bits ((.&.), xor)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.List (find, intercalate, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..))
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Data.Vector qualified as Vector
import Data.Word (Word64, Word8)
import Moonlight.Homology.Chain
  ( HomologicalDegree (..),
    HomologyFailure,
    PersistencePair (..),
  )
import Moonlight.Homology.Persistence
  ( criticalBettiVectors,
    mod2PersistentPairsWithCriticalBettiTable,
  )
import Moonlight.Planar.Alpha (AlphaBirth, AlphaFiltration, AlphaFiltrationError, alphaBirthDenominator, alphaBirthNumerator, alphaBirthToDouble, alphaEdgeBirth, alphaFaceBirth, alphaFiltration, alphaFiltrationCriticalBirths)
import Moonlight.Planar.BulkLoad (delaunayGeometry)
import Moonlight.Planar.Convex (ConvexError, convexPolygon)
import Moonlight.Planar.Dcel (innerFaceVertexTriples, undirectedEndpoints, vertexPoint)
import Moonlight.Planar.Exact (exactPointFromPoint, exactPointToEmbeddingCandidate)
import Moonlight.Planar.Handles.Iterators.FixedIterators (innerFaces, undirectedEdges, vertices)
import Moonlight.Planar.Point (mkQueryPoint)
import Moonlight.Planar.Minkowski (MinkowskiError, polygonOffset, structuringElement)
import Moonlight.Planar.Overlay (OverlayError, overlayLayers, overlaySelectedRegion)
import Moonlight.Planar.Region (ExactLoop, PlanarRegion, PolygonComponent, RegionPublicationError, RegionValidationError, exactLoop, exactLoopPoints, planarLayer, planarRegion, planarRegionComponents, polygonComponent, polygonHoleLoops, polygonOuterLoop)
import Moonlight.Planar.Types (BuildError, DelaunayTriangulation)
import Moonlight.Planar.Point (Point (..), PointValidationError, QueryPoint, queryPointValue)
import Moonlight.Planar.CellComplex
  ( DCELError,
    filteredAlphaComplex,
  )
import Moonlight.Planar.FloodFillIterator
  ( BoundaryObstruction,
    FaceComponent,
    boundaryLoopVertices,
    componentBoundaryLoops,
    faceComponentFaces,
    faceComponentsBy,
    outerFaceFloodFill,
  )
import Moonlight.Planar.Internal.HandleDefs
  ( FaceId (..),
    UndirectedEdgeId,
    VertexId,
    normalizedDirected,
  )
import Moonlight.Planar.Interpolation
  ( InterpolationStats (..),
    NaturalNeighborResult (..),
    naturalNeighborWeights,
    newNaturalNeighborWorkspace,
  )
import Moonlight.Planar.Voronoi
  ( VoronoiEdgeGeometry (..),
    asDirectedVoronoiEdge,
    voronoiEdgeGeometry,
  )
import Numeric (showFFloat, showHex)

-- | Every boundary at which the exhibit may be refused. The renderer never
-- invents a fallback geometry or topology state.
data PersistenceRoseError
  = RoseDelaunayBuildFailed !BuildError
  | RoseAlphaFiltrationFailed !AlphaFiltrationError
  | RoseCellComplexLoweringFailed !DCELError
  | RosePersistenceFailed !HomologyFailure
  | RoseCriticalProfileCountMismatch !Int !Int
  | RoseCriticalProfileDegreeCountMismatch !Int !Int
  | RoseBirthRankMissing !AlphaBirth
  | RoseEdgeBirthMissing !UndirectedEdgeId
  | RoseFaceBirthMissing !FaceId
  | RoseBoundaryDescentFailed !Int !BoundaryObstruction
  | RoseNoCriticalStates
  | RoseNoMaskRegions
  | RoseNoPersistentHole
  | RoseNoCoherentHero !Int ![(Int, Int)]
  | RoseVoronoiGeometryMissing !UndirectedEdgeId
  | RoseNaturalNeighborFallbacks !Int
  | RosePointInvalid !PointValidationError
  | RoseRegionInvalid !RegionValidationError
  | RoseOverlayFailed !OverlayError
  | RoseOverlayPublicationFailed !RegionPublicationError
  | RoseMinkowskiFailed !MinkowskiError
  | RoseConvexFailed !ConvexError
  | RoseFrameCountTooSmall !Int
  | RoseEclipseFiniteHoleCountMismatch !Int
  | RoseEclipseFiniteHoleMissingDeath
  | RoseEclipseBettiProfileMismatch !Int !Int !Int
  | RoseGifInvalidSignature
  | RoseGifTruncated !Int
  | RoseGifUnexpectedBlock !Int !Word8
  | RoseGifDimensionMismatch !Int !Int !Int !Int
  | RoseGifFrameCountMismatch !Int !Int
  | RoseGifFrameDelayCountMismatch !Int !Int
  | RoseGifDurationMismatch !Int !Int
  | RoseGifProvenanceMismatch
  | RoseGifMarkerTooLong !Int
  deriving stock (Show)

-- | A compact, test-readable projection of everything the scene actually
-- computed. It is intentionally not another geometry representation.
data PersistenceRoseReceipt = PersistenceRoseReceipt
  { roseSiteCount :: !Int,
    roseEdgeCount :: !Int,
    roseFaceCount :: !Int,
    roseCriticalBirthCount :: !Int,
    roseFinitePersistentHoleCount :: !Int,
    roseEssentialPersistentHoleCount :: !Int,
    roseHeroCriticalRank :: !Int,
    roseHeroBettiOne :: !Int,
    roseHeroGeometricHoleCount :: !Int,
    roseVoronoiSegmentCount :: !Int,
    roseNaturalNeighborSampleCount :: !Int,
    roseNaturalNeighborFallbackCount :: !Int,
    roseMaskComponentCount :: !Int
  }
  deriving stock (Eq, Show)

data PersistenceRoseScene = PersistenceRoseScene
  { sceneSites :: ![Point],
    sceneEdges :: ![RankedSegment],
    sceneFaces :: ![RankedTriangle],
    sceneCriticalStates :: !(NonEmpty CriticalState),
    sceneHeroState :: !CriticalState,
    sceneVoronoiSegments :: ![Segment],
    sceneFieldSamples :: ![FieldSample],
    sceneMaskComponents :: ![MaskComponent],
    scenePersistenceArcs :: ![PersistenceArc],
    sceneReceipt :: !PersistenceRoseReceipt
  }

-- | The canonical source projection carried inside the retained GIF. The GIF
-- codec remains an effect-boundary view; this receipt binds that binary view
-- to the exact deterministic SVG frame section used to produce it.
data PersistenceRoseAnimationReceipt = PersistenceRoseAnimationReceipt
  { roseAnimationFrameCount :: !Int,
    roseAnimationFramesPerSecond :: !Int,
    roseAnimationWidth :: !Int,
    roseAnimationHeight :: !Int,
    roseAnimationSourceFingerprint :: !Word64
  }
  deriving stock (Eq, Show)

-- | The exact, test-readable facts shown by the alpha eclipse. The numerator
-- and denominator fields are projections of opaque 'AlphaBirth' values; the
-- renderer never chooses thresholds independently of the filtration.
data AlphaEclipseReceipt = AlphaEclipseReceipt
  { eclipseSiteCount :: !Int,
    eclipseEdgeCount :: !Int,
    eclipseFaceCount :: !Int,
    eclipseHoleBirthNumerator :: !Integer,
    eclipseHoleBirthDenominator :: !Integer,
    eclipseHoleDeathNumerator :: !Integer,
    eclipseHoleDeathDenominator :: !Integer,
    eclipseBettiBeforeBirth :: !Int,
    eclipseBettiAtBirth :: !Int,
    eclipseBettiAtDeath :: !Int
  }
  deriving stock (Eq, Show)

-- | A three-section observation of one fixed resident Delaunay DCEL. Every
-- displayed edge and face is projected from that DCEL with its exact birth
-- rank; only the selected sublevel differs between sections.
data AlphaEclipseScene = AlphaEclipseScene
  { eclipseSites :: ![Point],
    eclipseEdges :: ![RankedSegment],
    eclipseFaces :: ![RankedTriangle],
    eclipseBeforeState :: !CriticalState,
    eclipseBirthState :: !CriticalState,
    eclipseDeathState :: !CriticalState,
    eclipseReceipt :: !AlphaEclipseReceipt
  }

-- | The shared local descent used by both exhibits. This is not a second
-- filtration carrier: it retains the canonical mesh and filtration beside
-- their checked Homology projection and only the ranked rendering views.
data AlphaProjection = AlphaProjection
  { projectionTriangulation :: !(DelaunayTriangulation ()),
    projectionPersistencePairs :: ![PersistencePair AlphaBirth],
    projectionCriticalStates :: !(NonEmpty CriticalState),
    projectionBirthRanks :: !(Map AlphaBirth Int),
    projectionEdges :: ![RankedSegment],
    projectionFaces :: ![RankedTriangle]
  }

newtype PersistenceRoseFrame = PersistenceRoseFrame
  { frameProgress :: Double
  }
  deriving stock (Eq, Show)

data CriticalState = CriticalState
  { criticalRank :: !Int,
    criticalBirth :: !AlphaBirth,
    criticalBettiOne :: !Int,
    criticalHoles :: ![HoleComponent]
  }

data RankedSegment = RankedSegment
  { rankedSegmentGeometry :: !Segment,
    rankedSegmentBirthRank :: !Int
  }

data RankedTriangle = RankedTriangle
  { rankedTrianglePoints :: !(Point, Point, Point),
    rankedTriangleBirthRank :: !Int
  }

data Segment = Segment !Point !Point

newtype HoleComponent = HoleComponent [[Point]]

data FieldSample = FieldSample
  { fieldSamplePoint :: !Point,
    fieldSampleValue :: !Double,
    fieldSampleNeighborCount :: !Int
  }

data MaskComponent = MaskComponent
  { maskOuterLoop :: ![Point],
    maskHoleLoops :: ![[Point]]
  }

data PersistenceArc = PersistenceArc
  { persistenceArcBirthRank :: !Int,
    persistenceArcDeathRank :: !Int
  }

buildPersistenceRoseScene :: Either PersistenceRoseError PersistenceRoseScene
buildPersistenceRoseScene = do
  projection <- buildAlphaProjection roseSites
  let triangulation = projectionTriangulation projection
      criticalStates = projectionCriticalStates projection
      persistencePairs = projectionPersistencePairs projection
      criticalBirthCount = NonEmpty.length criticalStates
  heroState <- chooseHeroState criticalStates
  finiteVoronoiSegments <- projectFiniteVoronoiSegments triangulation
  (fieldSamples, fallbackCount) <- naturalNeighborField triangulation
  if fallbackCount == 0
    then pure ()
    else Left (RoseNaturalNeighborFallbacks fallbackCount)
  maskComponents <- buildRoseMask
  persistenceArcs <-
    projectPersistenceArcs
      (projectionBirthRanks projection)
      criticalBirthCount
      persistencePairs
  let finiteHoles = filter isFinitePositiveHole persistencePairs
      essentialHoles = filter isEssentialHole persistencePairs
      heroHoleCount = length (criticalHoles heroState)
      receipt =
        PersistenceRoseReceipt
          { roseSiteCount = length (vertices triangulation),
            roseEdgeCount = length (projectionEdges projection),
            roseFaceCount = length (projectionFaces projection),
            roseCriticalBirthCount = criticalBirthCount,
            roseFinitePersistentHoleCount = length finiteHoles,
            roseEssentialPersistentHoleCount = length essentialHoles,
            roseHeroCriticalRank = criticalRank heroState,
            roseHeroBettiOne = criticalBettiOne heroState,
            roseHeroGeometricHoleCount = heroHoleCount,
            roseVoronoiSegmentCount = length finiteVoronoiSegments,
            roseNaturalNeighborSampleCount = length fieldSamples,
            roseNaturalNeighborFallbackCount = fallbackCount,
            roseMaskComponentCount = length maskComponents
          }
  pure
    PersistenceRoseScene
      { sceneSites = fmap (vertexPoint triangulation) (vertices triangulation),
        sceneEdges = projectionEdges projection,
        sceneFaces = projectionFaces projection,
        sceneCriticalStates = criticalStates,
        sceneHeroState = heroState,
        sceneVoronoiSegments = finiteVoronoiSegments,
        sceneFieldSamples = fieldSamples,
        sceneMaskComponents = maskComponents,
        scenePersistenceArcs = persistenceArcs,
        sceneReceipt = receipt
      }

buildAlphaProjection :: [Point] -> Either PersistenceRoseError AlphaProjection
buildAlphaProjection sites = do
  triangulation <- first RoseDelaunayBuildFailed (delaunayGeometry (Vector.fromList sites))
  filtration <- first RoseAlphaFiltrationFailed (alphaFiltration triangulation)
  filteredComplex <- first RoseCellComplexLoweringFailed (filteredAlphaComplex filtration)
  (persistencePairs, criticalBettiTable) <-
    first RosePersistenceFailed
      (mod2PersistentPairsWithCriticalBettiTable filteredComplex)
  let criticalBirths = alphaFiltrationCriticalBirths filtration
      criticalProfiles = criticalBettiVectors criticalBettiTable
      birthRanks = Map.fromDistinctAscList (zip criticalBirths [0 ..])
  if length criticalBirths == length criticalProfiles
    then pure ()
    else Left (RoseCriticalProfileCountMismatch (length criticalBirths) (length criticalProfiles))
  criticalStates <-
    traverse
      (buildCriticalState triangulation filtration)
      (zip3 [0 ..] criticalBirths criticalProfiles)
      >>= maybe (Left RoseNoCriticalStates) Right . NonEmpty.nonEmpty
  rankedEdges <-
    traverse
      (projectRankedEdge triangulation filtration birthRanks)
      (undirectedEdges triangulation)
  rankedFaces <-
    traverse
      (projectRankedFace triangulation filtration birthRanks)
      (zip [1 ..] (Vector.toList (innerFaceVertexTriples triangulation)))
  pure
    AlphaProjection
      { projectionTriangulation = triangulation,
        projectionPersistencePairs = persistencePairs,
        projectionCriticalStates = criticalStates,
        projectionBirthRanks = birthRanks,
        projectionEdges = rankedEdges,
        projectionFaces = rankedFaces
      }

buildAlphaEclipseScene :: Either PersistenceRoseError AlphaEclipseScene
buildAlphaEclipseScene = do
  projection <- buildAlphaProjection alphaEclipseSites
  finiteHoles <-
    case filter isFinitePositiveHole (projectionPersistencePairs projection) of
      [finiteHole] -> Right finiteHole
      holes -> Left (RoseEclipseFiniteHoleCountMismatch (length holes))
  deathBirth <-
    maybe
      (Left RoseEclipseFiniteHoleMissingDeath)
      Right
      (persistenceDeath finiteHoles)
  let birth = persistenceBirth finiteHoles
      birthRanks = projectionBirthRanks projection
      states = projectionCriticalStates projection
  birthRank <- maybe (Left (RoseBirthRankMissing birth)) Right (Map.lookup birth birthRanks)
  deathRank <- maybe (Left (RoseBirthRankMissing deathBirth)) Right (Map.lookup deathBirth birthRanks)
  let beforeState = criticalStateAt (max 0 (birthRank - 1)) states
      birthState = criticalStateAt birthRank states
      deathState = criticalStateAt deathRank states
      bettiProfile =
        ( criticalBettiOne beforeState,
          criticalBettiOne birthState,
          criticalBettiOne deathState
        )
  case bettiProfile of
    (0, 1, 0) -> pure ()
    (beforeBetti, birthBetti, deathBetti) ->
      Left (RoseEclipseBettiProfileMismatch beforeBetti birthBetti deathBetti)
  let triangulation = projectionTriangulation projection
      receipt =
        AlphaEclipseReceipt
          { eclipseSiteCount = length (vertices triangulation),
            eclipseEdgeCount = length (projectionEdges projection),
            eclipseFaceCount = length (projectionFaces projection),
            eclipseHoleBirthNumerator = alphaBirthNumerator birth,
            eclipseHoleBirthDenominator = alphaBirthDenominator birth,
            eclipseHoleDeathNumerator = alphaBirthNumerator deathBirth,
            eclipseHoleDeathDenominator = alphaBirthDenominator deathBirth,
            eclipseBettiBeforeBirth = criticalBettiOne beforeState,
            eclipseBettiAtBirth = criticalBettiOne birthState,
            eclipseBettiAtDeath = criticalBettiOne deathState
          }
  pure
    AlphaEclipseScene
      { eclipseSites = fmap (vertexPoint triangulation) (vertices triangulation),
        eclipseEdges = projectionEdges projection,
        eclipseFaces = projectionFaces projection,
        eclipseBeforeState = beforeState,
        eclipseBirthState = birthState,
        eclipseDeathState = deathState,
        eclipseReceipt = receipt
      }

persistenceRoseReceipt :: PersistenceRoseScene -> PersistenceRoseReceipt
persistenceRoseReceipt = sceneReceipt

alphaEclipseReceipt :: AlphaEclipseScene -> AlphaEclipseReceipt
alphaEclipseReceipt = eclipseReceipt

renderPersistenceRoseReceipt :: PersistenceRoseReceipt -> String
renderPersistenceRoseReceipt receipt =
  intercalate
    " · "
    [ "sites " <> show (roseSiteCount receipt),
      "edges " <> show (roseEdgeCount receipt),
      "faces " <> show (roseFaceCount receipt),
      "critical births " <> show (roseCriticalBirthCount receipt),
      "finite H1 intervals " <> show (roseFinitePersistentHoleCount receipt),
      "essential H1 intervals " <> show (roseEssentialPersistentHoleCount receipt),
      "hero rank " <> show (roseHeroCriticalRank receipt),
      "beta1 " <> show (roseHeroBettiOne receipt),
      "geometric holes " <> show (roseHeroGeometricHoleCount receipt),
      "Voronoi segments " <> show (roseVoronoiSegmentCount receipt),
      "Sibson samples " <> show (roseNaturalNeighborSampleCount receipt),
      "fallbacks " <> show (roseNaturalNeighborFallbackCount receipt),
      "mask components " <> show (roseMaskComponentCount receipt)
    ]

renderAlphaEclipseReceipt :: AlphaEclipseReceipt -> String
renderAlphaEclipseReceipt receipt =
  intercalate
    " · "
    [ "sites " <> show (eclipseSiteCount receipt),
      "edges " <> show (eclipseEdgeCount receipt),
      "faces " <> show (eclipseFaceCount receipt),
      "H1 ["
        <> renderExactRatio (eclipseHoleBirthNumerator receipt) (eclipseHoleBirthDenominator receipt)
        <> ", "
        <> renderExactRatio (eclipseHoleDeathNumerator receipt) (eclipseHoleDeathDenominator receipt)
        <> ")",
      "beta1 "
        <> show
          ( eclipseBettiBeforeBirth receipt,
            eclipseBettiAtBirth receipt,
            eclipseBettiAtDeath receipt
          )
    ]

buildCriticalState
  :: DelaunayTriangulation ()
  -> AlphaFiltration
  -> (Int, AlphaBirth, [Int])
  -> Either PersistenceRoseError CriticalState
buildCriticalState triangulation filtration (rankValue, birthValue, profile) = do
  bettiOne <-
    case profile of
      _ : secondDegree : _ : _ -> Right secondDegree
      _ -> Left (RoseCriticalProfileDegreeCountMismatch rankValue (length profile))
  holes <- holeComponentsAt rankValue triangulation filtration birthValue
  pure
    CriticalState
      { criticalRank = rankValue,
        criticalBirth = birthValue,
        criticalBettiOne = bettiOne,
        criticalHoles = holes
      }

chooseHeroState :: NonEmpty CriticalState -> Either PersistenceRoseError CriticalState
chooseHeroState states =
  let stateList = NonEmpty.toList states
      maximumBettiOne = foldl' (\best state -> max best (criticalBettiOne state)) 0 stateList
      maximalStates = filter ((== maximumBettiOne) . criticalBettiOne) stateList
      agreements = fmap (\state -> (criticalBettiOne state, length (criticalHoles state))) maximalStates
      coherentStates =
        filter
          (\state -> criticalBettiOne state > 0 && criticalBettiOne state == length (criticalHoles state))
          stateList
      sevenPetalStates = filter ((== rosePetalCount) . criticalBettiOne) coherentStates
   in if maximumBettiOne <= 0
        then Left RoseNoPersistentHole
        else
          case NonEmpty.nonEmpty sevenPetalStates of
            Just preferredStates -> Right (latestCriticalState preferredStates)
            Nothing ->
              maybe
                (Left (RoseNoCoherentHero maximumBettiOne agreements))
                Right
                (find (\state -> criticalBettiOne state == length (criticalHoles state)) maximalStates)

latestCriticalState :: NonEmpty CriticalState -> CriticalState
latestCriticalState (firstState :| remainingStates) =
  foldl'
    (\latest candidate -> if criticalRank candidate > criticalRank latest then candidate else latest)
    firstState
    remainingStates

holeComponentsAt
  :: Int
  -> DelaunayTriangulation ()
  -> AlphaFiltration
  -> AlphaBirth
  -> Either PersistenceRoseError [HoleComponent]
holeComponentsAt rankValue triangulation filtration threshold = do
  inactiveEdges <-
    inactiveHandlesAt
      RoseEdgeBirthMissing
      (alphaEdgeBirth filtration)
      (undirectedEdges triangulation)
      threshold
  inactiveFaces <-
    inactiveHandlesAt
      RoseFaceBirthMissing
      (alphaFaceBirth filtration)
      (innerFaces triangulation)
      threshold
  let edgeInactive edge = Set.member edge inactiveEdges
      faceInactive face = Set.member face inactiveFaces
      exteriorFaces = Set.fromList (outerFaceFloodFill triangulation edgeInactive)
      inactiveComponents =
        fmap snd
          ( faceComponentsBy
              triangulation
              (\face -> if faceInactive face then Just () else Nothing)
              edgeInactive
          )
      boundedComponents =
        filter
          (all (`Set.notMember` exteriorFaces) . faceComponentFaces)
          inactiveComponents
  traverse
    (projectHoleComponent rankValue triangulation)
    boundedComponents

inactiveHandlesAt
  :: Ord handle
  => (handle -> PersistenceRoseError)
  -> (handle -> Maybe AlphaBirth)
  -> [handle]
  -> AlphaBirth
  -> Either PersistenceRoseError (Set.Set handle)
inactiveHandlesAt missingBirth birthAt handles threshold =
  Set.fromList . mapMaybe id
    <$> traverse
      ( \handle ->
          maybe
            (Left (missingBirth handle))
            (\birth -> Right (if birth > threshold then Just handle else Nothing))
            (birthAt handle)
      )
      handles

projectHoleComponent
  :: Int
  -> DelaunayTriangulation ()
  -> FaceComponent
  -> Either PersistenceRoseError HoleComponent
projectHoleComponent rankValue triangulation component = do
  loops <-
    first (RoseBoundaryDescentFailed rankValue)
      (componentBoundaryLoops triangulation component)
  pure
    ( HoleComponent
        ( fmap
            (fmap (vertexPoint triangulation) . NonEmpty.toList . boundaryLoopVertices)
            (NonEmpty.toList loops)
        )
    )

projectRankedEdge
  :: DelaunayTriangulation ()
  -> AlphaFiltration
  -> Map AlphaBirth Int
  -> UndirectedEdgeId
  -> Either PersistenceRoseError RankedSegment
projectRankedEdge triangulation filtration birthRanks edge = do
  birth <- maybe (Left (RoseEdgeBirthMissing edge)) Right (alphaEdgeBirth filtration edge)
  rankValue <- maybe (Left (RoseBirthRankMissing birth)) Right (Map.lookup birth birthRanks)
  let (fromVertex, toVertex) = undirectedEndpoints triangulation edge
  pure
    RankedSegment
      { rankedSegmentGeometry = Segment (vertexPoint triangulation fromVertex) (vertexPoint triangulation toVertex),
        rankedSegmentBirthRank = rankValue
      }

projectRankedFace
  :: DelaunayTriangulation ()
  -> AlphaFiltration
  -> Map AlphaBirth Int
  -> (Int, (VertexId, VertexId, VertexId))
  -> Either PersistenceRoseError RankedTriangle
projectRankedFace triangulation filtration birthRanks (rawFace, (firstVertex, secondVertex, thirdVertex)) = do
  let face = toFaceId rawFace
  birth <- maybe (Left (RoseFaceBirthMissing face)) Right (alphaFaceBirth filtration face)
  rankValue <- maybe (Left (RoseBirthRankMissing birth)) Right (Map.lookup birth birthRanks)
  pure
    RankedTriangle
      { rankedTrianglePoints =
          ( vertexPoint triangulation firstVertex,
            vertexPoint triangulation secondVertex,
            vertexPoint triangulation thirdVertex
          ),
        rankedTriangleBirthRank = rankValue
      }

toFaceId :: Int -> FaceId
toFaceId = FaceId . fromIntegral

projectFiniteVoronoiSegments
  :: DelaunayTriangulation ()
  -> Either PersistenceRoseError [Segment]
projectFiniteVoronoiSegments triangulation =
  fmap (mapMaybe id)
    . traverse projectSegment
    $ undirectedEdges triangulation
 where
  projectSegment :: UndirectedEdgeId -> Either PersistenceRoseError (Maybe Segment)
  projectSegment edge =
    case voronoiEdgeGeometry triangulation (asDirectedVoronoiEdge (normalizedDirected edge)) of
      Nothing -> Left (RoseVoronoiGeometryMissing edge)
      Just (VoronoiSegment fromPoint toPoint) -> Right (Just (Segment fromPoint toPoint))
      Just (VoronoiRay _ _) -> Right Nothing
      Just (VoronoiLine _ _) -> Right Nothing

naturalNeighborField
  :: DelaunayTriangulation ()
  -> Either PersistenceRoseError ([FieldSample], Int)
naturalNeighborField triangulation = do
  admittedQueries <- traverse (first RosePointInvalid . mkQueryPoint) fieldQueryPoints
  let results = runST $ do
        workspace <- newNaturalNeighborWorkspace triangulation
        traverse (naturalNeighborWeights workspace Nothing) admittedQueries
      pairedResults = zip admittedQueries results
      fallbackCount = length (filter (interpolationUsedFallback . naturalNeighborStats . snd) pairedResults)
      samples = mapMaybe (fieldSampleFromResult triangulation) pairedResults
  pure (samples, fallbackCount)

fieldSampleFromResult
  :: DelaunayTriangulation ()
  -> (QueryPoint, NaturalNeighborResult)
  -> Maybe FieldSample
fieldSampleFromResult triangulation (query, result)
  | Vector.null weights = Nothing
  | interpolationUsedFallback (naturalNeighborStats result) = Nothing
  | otherwise =
      let weightedValue =
            Vector.foldl'
              (\total (vertex, weight) -> total + weight * siteSignal (vertexPoint triangulation vertex))
              0
              weights
       in Just
            FieldSample
              { fieldSamplePoint = queryPointValue query,
                fieldSampleValue = clamp 0 1 weightedValue,
                fieldSampleNeighborCount = Vector.length weights
              }
 where
  weights = naturalNeighborValues result

siteSignal :: Point -> Double
siteSignal (Point x y) =
  clamp 0 1 (0.5 + 0.24 * sin (1.31 * x - 0.73 * y) + 0.18 * cos (0.61 * x + 1.17 * y))

fieldQueryPoints :: [Point]
fieldQueryPoints =
  [ Point (fromIntegral column * 0.34 - 5.1) (fromIntegral row * 0.34 - 4.42)
  | row <- [0 .. 26 :: Int],
    column <- [0 .. 30 :: Int]
  ]

projectPersistenceArcs
  :: Map AlphaBirth Int
  -> Int
  -> [PersistencePair AlphaBirth]
  -> Either PersistenceRoseError [PersistenceArc]
projectPersistenceArcs birthRanks criticalCount pairs = do
  arcs <- traverse pairArc (filter isPositiveHole pairs)
  pure (take 36 (sortOn (Down . persistenceArcLifetime) arcs))
 where
  finalRank = max 0 (criticalCount - 1)
  pairArc pair = do
    birthRank <- rankOf (persistenceBirth pair)
    deathRank <- maybe (Right finalRank) rankOf (persistenceDeath pair)
    pure
      PersistenceArc
        { persistenceArcBirthRank = birthRank,
          persistenceArcDeathRank = deathRank
        }
  rankOf birth = maybe (Left (RoseBirthRankMissing birth)) Right (Map.lookup birth birthRanks)

persistenceArcLifetime :: PersistenceArc -> Int
persistenceArcLifetime PersistenceArc{persistenceArcBirthRank, persistenceArcDeathRank} =
  max 0 (persistenceArcDeathRank - persistenceArcBirthRank)

isPositiveHole :: PersistencePair AlphaBirth -> Bool
isPositiveHole pair =
  persistenceDegree pair == HomologicalDegree 1
    && maybe True (> persistenceBirth pair) (persistenceDeath pair)

isFinitePositiveHole :: PersistencePair AlphaBirth -> Bool
isFinitePositiveHole pair = isPositiveHole pair && maybe False (> persistenceBirth pair) (persistenceDeath pair)

isEssentialHole :: PersistencePair AlphaBirth -> Bool
isEssentialHole pair = persistenceDegree pair == HomologicalDegree 1 && persistenceDeath pair == Nothing

buildRoseMask :: Either PersistenceRoseError [MaskComponent]
buildRoseMask = do
  kernelPoints <- traverse (first RosePointInvalid . exactPointFromPoint) minkowskiKernel
  kernelPolygon <- first RoseConvexFailed (convexPolygon kernelPoints)
  kernel <- first RoseMinkowskiFailed (structuringElement kernelPolygon)
  authoredRegions <- traverse authoredRegion roseMaskLoops
  offsetRegions <-
    traverse
      (fmap fst . first RoseMinkowskiFailed . polygonOffset kernel)
      authoredRegions
  unioned <-
    case NonEmpty.nonEmpty offsetRegions of
      Nothing -> Left RoseNoMaskRegions
      Just (firstRegion :| remainingRegions) -> foldM exactRegionUnion firstRegion remainingRegions
  traverse projectMaskComponent (planarRegionComponents unioned)

authoredRegion :: NonEmpty Point -> Either PersistenceRoseError PlanarRegion
authoredRegion points = do
  exactPoints <- traverse (first RosePointInvalid . exactPointFromPoint) points
  loop <- first RoseRegionInvalid (exactLoop exactPoints)
  component <- first RoseRegionInvalid (polygonComponent loop [])
  first RoseRegionInvalid (planarRegion [component])

exactRegionUnion :: PlanarRegion -> PlanarRegion -> Either PersistenceRoseError PlanarRegion
exactRegionUnion leftRegion rightRegion = do
  leftLayer <- first RoseRegionInvalid (planarLayer False (Map.singleton True leftRegion))
  rightLayer <- first RoseRegionInvalid (planarLayer False (Map.singleton True rightRegion))
  overlaid <- first RoseOverlayFailed (overlayLayers leftLayer rightLayer)
  first RoseOverlayPublicationFailed (overlaySelectedRegion (uncurry (||)) overlaid)

projectMaskComponent :: PolygonComponent -> Either PersistenceRoseError MaskComponent
projectMaskComponent component =
  MaskComponent
    <$> projectExactLoop (polygonOuterLoop component)
    <*> traverse projectExactLoop (polygonHoleLoops component)

projectExactLoop :: ExactLoop -> Either PersistenceRoseError [Point]
projectExactLoop =
  traverse
    (fmap queryPointValue . first RosePointInvalid . exactPointToEmbeddingCandidate)
    . NonEmpty.toList
    . exactLoopPoints

roseSites :: [Point]
roseSites =
  concatMap petalSites [0 .. rosePetalCount - 1]
    <> centerSites

petalSites :: Int -> [Point]
petalSites petalIndex =
  fmap
    (petalPoint petalIndex 2.52 1.82 0.69)
    (sampleAngles 22 (0.021 * fromIntegral petalIndex))

centerSites :: [Point]
centerSites =
  Point 0 0
    : fmap
      (\angle -> Point (0.56 * cos angle) (0.56 * sin angle))
      (sampleAngles 14 0.037)

roseMaskLoops :: [NonEmpty Point]
roseMaskLoops =
  fmap
    (\petalIndex -> fmap (petalPoint petalIndex 2.42 2.12 0.9) (sampleAnglesNonEmpty 34 0))
    [0 .. rosePetalCount - 1]
    <> [fmap (\angle -> Point (1.08 * cos angle) (1.08 * sin angle)) (sampleAnglesNonEmpty 30 0)]

petalPoint :: Int -> Double -> Double -> Double -> Double -> Point
petalPoint petalIndex centerRadius radialRadius tangentialRadius localAngle =
  let orientation = twoPi * fromIntegral petalIndex / fromIntegral rosePetalCount - pi / 2
      radial = centerRadius + radialRadius * cos localAngle
      tangential = tangentialRadius * sin localAngle * (0.91 + 0.09 * cos (2 * localAngle))
      radialX = cos orientation
      radialY = sin orientation
      tangentX = -radialY
      tangentY = radialX
   in Point
        (radial * radialX + tangential * tangentX)
        (radial * radialY + tangential * tangentY)

sampleAngles :: Int -> Double -> [Double]
sampleAngles sampleCount phase =
  fmap
    (\sampleIndex -> twoPi * fromIntegral sampleIndex / fromIntegral sampleCount + phase)
    [0 .. sampleCount - 1]

sampleAnglesNonEmpty :: Int -> Double -> NonEmpty Double
sampleAnglesNonEmpty sampleCount phase =
  phase
    :| fmap
      (\sampleIndex -> twoPi * fromIntegral sampleIndex / fromIntegral sampleCount + phase)
      [1 .. max 1 sampleCount - 1]

minkowskiKernel :: NonEmpty Point
minkowskiKernel =
  Point 0.085 0
    :| [ Point 0.043 0.074,
         Point (-0.043) 0.074,
         Point (-0.085) 0,
         Point (-0.043) (-0.074),
         Point 0.043 (-0.074)
       ]

rosePetalCount :: Int
rosePetalCount = 7

-- | The package's canonical exact persistence witness. Its four hull edges
-- enter at squared radius one; the resident diagonal and two resident faces
-- enter at squared radius two. Homology therefore observes exactly one finite
-- degree-one interval @[1,2)@ over this fixed DCEL.
alphaEclipseSites :: [Point]
alphaEclipseSites =
  [ Point (-1) (-1),
    Point 1 (-1),
    Point 1 1,
    Point (-1) 1
  ]

renderPersistenceRosePoster :: PersistenceRoseScene -> String
renderPersistenceRosePoster scene =
  renderScene scene (criticalRank (sceneHeroState scene)) 1 True

renderAlphaEclipse :: AlphaEclipseScene -> String
renderAlphaEclipse scene =
  let receipt = eclipseReceipt scene
      birthLabel =
        renderExactRatio
          (eclipseHoleBirthNumerator receipt)
          (eclipseHoleBirthDenominator receipt)
      deathLabel =
        renderExactRatio
          (eclipseHoleDeathNumerator receipt)
          (eclipseHoleDeathDenominator receipt)
   in concat
        [ "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 1440 620\" width=\"1440\" height=\"620\" role=\"img\" aria-labelledby=\"title description\">\n",
          "<title id=\"title\">Moonlight Triangulation alpha eclipse</title>\n",
          "<desc id=\"description\">One fixed four-site Delaunay DCEL at three exact alpha sublevels. Its resident boundary creates one hole at squared radius ",
          birthLabel,
          ", and its resident diagonal and faces kill that hole at squared radius ",
          deathLabel,
          ".</desc>\n",
          renderAlphaEclipseDefinitions,
          "<rect width=\"1440\" height=\"620\" fill=\"url(#eclipse-background)\"/>\n",
          "<circle cx=\"720\" cy=\"300\" r=\"246\" fill=\"#ff68bb\" opacity=\"0.035\" filter=\"url(#eclipse-soft-glow)\"/>\n",
          "<text x=\"58\" y=\"58\" fill=\"#84f3ff\" font-family=\"Inter,ui-sans-serif,system-ui,sans-serif\" font-size=\"17\" font-weight=\"600\" letter-spacing=\"6\">MOONLIGHT TRIANGULATION</text>\n",
          "<text x=\"58\" y=\"101\" fill=\"#f7f3ff\" font-family=\"Inter,ui-sans-serif,system-ui,sans-serif\" font-size=\"38\" font-weight=\"420\">ALPHA ECLIPSE</text>\n",
          "<text x=\"1382\" y=\"61\" text-anchor=\"end\" fill=\"#9baedb\" font-family=\"Inter,ui-sans-serif,system-ui,sans-serif\" font-size=\"14\" letter-spacing=\"1.5\">ONE FIXED DELAUNAY DCEL · EXACT CRITICAL BIRTHS</text>\n",
          renderAlphaEclipseStage
            scene
            245
            "BEFORE BIRTH"
            "four isolated sites"
            (eclipseBeforeState scene),
          renderAlphaEclipseStage
            scene
            720
            "LOOP BORN"
            "resident boundary closes"
            (eclipseBirthState scene),
          renderAlphaEclipseStage
            scene
            1195
            "CYCLE KILLED"
            "resident faces fill it"
            (eclipseDeathState scene),
          "<g font-family=\"Inter,ui-sans-serif,system-ui,sans-serif\">\n",
          "<text x=\"58\" y=\"516\" fill=\"#9badd8\" font-size=\"13\" letter-spacing=\"2.5\">ONE EXACT H₁ INTERVAL</text>\n",
          "<line x1=\"58\" y1=\"559\" x2=\"1382\" y2=\"559\" stroke=\"#344070\" stroke-width=\"3\"/>\n",
          "<line x1=\"535\" y1=\"559\" x2=\"1042\" y2=\"559\" stroke=\"#ff72c6\" stroke-width=\"9\" stroke-linecap=\"round\"/>\n",
          "<circle cx=\"535\" cy=\"559\" r=\"8\" fill=\"#fff6d6\" stroke=\"#ff72c6\" stroke-width=\"3\"/>\n",
          "<rect x=\"1034\" y=\"551\" width=\"16\" height=\"16\" fill=\"#fff6d6\" stroke=\"#66efff\" stroke-width=\"3\"/>\n",
          "<text x=\"58\" y=\"589\" fill=\"#7588ba\" font-size=\"12\">α² = 0</text>\n",
          "<text x=\"535\" y=\"589\" text-anchor=\"middle\" fill=\"#f7f3ff\" font-size=\"12\">birth α² = ",
          birthLabel,
          "</text>\n",
          "<text x=\"788\" y=\"548\" text-anchor=\"middle\" fill=\"#fff6d6\" font-size=\"14\">the eclipse hole persists</text>\n",
          "<text x=\"1042\" y=\"589\" text-anchor=\"middle\" fill=\"#f7f3ff\" font-size=\"12\">death α² = ",
          deathLabel,
          "</text>\n",
          "<text x=\"1382\" y=\"589\" text-anchor=\"end\" fill=\"#7588ba\" font-size=\"12\">exact α² increases →</text>\n",
          "</g>\n",
          "</svg>\n"
        ]

renderAlphaEclipseDefinitions :: String
renderAlphaEclipseDefinitions =
  concat
    [ "<defs>\n",
      "<radialGradient id=\"eclipse-background\" cx=\"50%\" cy=\"38%\" r=\"78%\"><stop offset=\"0\" stop-color=\"#111744\"/><stop offset=\"0.55\" stop-color=\"#070b25\"/><stop offset=\"1\" stop-color=\"#02040f\"/></radialGradient>\n",
      "<linearGradient id=\"eclipse-corona\" x1=\"0\" y1=\"0\" x2=\"1\" y2=\"1\"><stop offset=\"0\" stop-color=\"#66efff\"/><stop offset=\"0.48\" stop-color=\"#ff72c6\"/><stop offset=\"1\" stop-color=\"#ffd86b\"/></linearGradient>\n",
      "<linearGradient id=\"eclipse-face\" x1=\"0\" y1=\"0\" x2=\"1\" y2=\"1\"><stop offset=\"0\" stop-color=\"#7c4dff\"/><stop offset=\"0.5\" stop-color=\"#ff4fa3\"/><stop offset=\"1\" stop-color=\"#ffbf5b\"/></linearGradient>\n",
      "<filter id=\"eclipse-soft-glow\" x=\"-30%\" y=\"-30%\" width=\"160%\" height=\"160%\"><feGaussianBlur stdDeviation=\"7\" result=\"blur\"/><feMerge><feMergeNode in=\"blur\"/><feMergeNode in=\"SourceGraphic\"/></feMerge></filter>\n",
      "<filter id=\"eclipse-dot-glow\" x=\"-100%\" y=\"-100%\" width=\"300%\" height=\"300%\"><feGaussianBlur stdDeviation=\"3\" result=\"blur\"/><feMerge><feMergeNode in=\"blur\"/><feMergeNode in=\"SourceGraphic\"/></feMerge></filter>\n",
      "</defs>\n"
    ]

renderAlphaEclipseStage
  :: AlphaEclipseScene
  -> Double
  -> String
  -> String
  -> CriticalState
  -> String
renderAlphaEclipseStage scene centerX stageLabel explanation state =
  let rankValue = criticalRank state
      activeFaces = filter ((<= rankValue) . rankedTriangleBirthRank) (eclipseFaces scene)
      activeEdges = filter ((<= rankValue) . rankedSegmentBirthRank) (eclipseEdges scene)
      highlighted = criticalBettiOne state > 0
      frameStroke = if highlighted then "#ff6fbd" else "#344070"
      frameOpacity = if highlighted then "0.72" else "0.55"
   in concat
        [ "<g transform=\"translate(",
          formatDouble centerX,
          " 292)\">\n",
          "<rect x=\"-194\" y=\"-150\" width=\"388\" height=\"292\" rx=\"28\" fill=\"#090d26\" fill-opacity=\"0.72\" stroke=\"",
          frameStroke,
          "\" stroke-opacity=\"",
          frameOpacity,
          "\"/>\n",
          "<text x=\"-162\" y=\"-114\" fill=\"",
          if highlighted then "#ff89cb" else "#8fa4d6",
          "\" font-family=\"Inter,ui-sans-serif,system-ui,sans-serif\" font-size=\"13\" letter-spacing=\"2.4\">",
          stageLabel,
          "</text>\n",
          "<circle r=\"104\" fill=\"#01020a\" stroke=\"#7fefff\" stroke-opacity=\"0.18\" stroke-width=\"2\"/>\n",
          "<g fill=\"url(#eclipse-face)\" fill-opacity=\"0.58\" stroke=\"#ffd56d\" stroke-opacity=\"0.48\" stroke-width=\"1.2\">",
          concatMap renderAlphaEclipseTriangle activeFaces,
          "</g>\n",
          "<g fill=\"none\" stroke=\"url(#eclipse-corona)\" stroke-width=\"5\" stroke-linejoin=\"round\" stroke-linecap=\"round\" filter=\"url(#eclipse-soft-glow)\">",
          concatMap renderAlphaEclipseSegment activeEdges,
          "</g>\n",
          "<g fill=\"none\" stroke=\"#fff7da\" stroke-width=\"3.2\" stroke-linejoin=\"round\" opacity=\"0.86\">",
          concatMap renderAlphaEclipseHole (criticalHoles state),
          "</g>\n",
          "<g fill=\"#fff8df\" stroke=\"#ff75c5\" stroke-width=\"2\" filter=\"url(#eclipse-dot-glow)\">",
          concatMap renderAlphaEclipseSite (eclipseSites scene),
          "</g>\n",
          "<rect x=\"-126\" y=\"-18\" width=\"252\" height=\"36\" rx=\"18\" fill=\"#05071a\" fill-opacity=\"0.82\"/>\n",
          "<text y=\"5\" text-anchor=\"middle\" fill=\"#fff7da\" font-family=\"Inter,ui-sans-serif,system-ui,sans-serif\" font-size=\"14\" letter-spacing=\"1.2\">",
          explanation,
          "</text>\n",
          "<text x=\"-162\" y=\"121\" fill=\"#fff4cc\" font-family=\"Inter,ui-sans-serif,system-ui,sans-serif\" font-size=\"25\">β₁ = ",
          show (criticalBettiOne state),
          "</text>\n",
          "<text x=\"162\" y=\"121\" text-anchor=\"end\" fill=\"#8495c4\" font-family=\"Inter,ui-sans-serif,system-ui,sans-serif\" font-size=\"12\">α² = ",
          renderAlphaBirth (criticalBirth state),
          "</text>\n",
          "</g>\n"
        ]

renderAlphaEclipseTriangle :: RankedTriangle -> String
renderAlphaEclipseTriangle RankedTriangle{rankedTrianglePoints = (firstPoint, secondPoint, thirdPoint)} =
  "<polygon points=\""
    <> renderScreenPoints (fmap toAlphaEclipseLocal [firstPoint, secondPoint, thirdPoint])
    <> "\"/>\n"

renderAlphaEclipseSegment :: RankedSegment -> String
renderAlphaEclipseSegment RankedSegment{rankedSegmentGeometry = Segment firstPoint secondPoint} =
  let Point firstX firstY = toAlphaEclipseLocal firstPoint
      Point secondX secondY = toAlphaEclipseLocal secondPoint
   in "<line x1=\""
        <> formatDouble firstX
        <> "\" y1=\""
        <> formatDouble firstY
        <> "\" x2=\""
        <> formatDouble secondX
        <> "\" y2=\""
        <> formatDouble secondY
        <> "\"/>\n"

renderAlphaEclipseHole :: HoleComponent -> String
renderAlphaEclipseHole (HoleComponent loops) =
  concatMap
    (\points -> "<path d=\"" <> renderClosedPath (fmap toAlphaEclipseLocal points) <> "\"/>\n")
    loops

renderAlphaEclipseSite :: Point -> String
renderAlphaEclipseSite point =
  let Point x y = toAlphaEclipseLocal point
   in "<circle cx=\""
        <> formatDouble x
        <> "\" cy=\""
        <> formatDouble y
        <> "\" r=\"6\"/>\n"

toAlphaEclipseLocal :: Point -> Point
toAlphaEclipseLocal (Point x y) =
  let scale = 72
      inverseSquareRootTwo = sqrt 0.5
   in Point
        (scale * inverseSquareRootTwo * (x - y))
        (scale * inverseSquareRootTwo * (x + y))

renderAlphaBirth :: AlphaBirth -> String
renderAlphaBirth birth =
  renderExactRatio (alphaBirthNumerator birth) (alphaBirthDenominator birth)

renderExactRatio :: Integer -> Integer -> String
renderExactRatio numerator denominator
  | denominator == 1 = show numerator
  | otherwise = show numerator <> "/" <> show denominator

persistenceRoseFrameSpecs :: Int -> Either PersistenceRoseError [PersistenceRoseFrame]
persistenceRoseFrameSpecs frameCount
  | frameCount < 2 = Left (RoseFrameCountTooSmall frameCount)
  | otherwise =
      Right
        ( fmap
            (\frameIndex ->
               PersistenceRoseFrame
                 { frameProgress = frameProgressAt frameCount frameIndex
                 })
            [0 .. frameCount - 1]
        )

renderPersistenceRoseFrame :: PersistenceRoseScene -> PersistenceRoseFrame -> String
renderPersistenceRoseFrame scene frame =
  let finalRank = criticalRank (NonEmpty.last (sceneCriticalStates scene))
      selectedRank = clampInt 0 finalRank (floor (frameProgress frame * fromIntegral finalRank))
   in renderScene scene selectedRank (frameProgress frame) False

persistenceRoseAnimationReceipt
  :: PersistenceRoseScene
  -> Either PersistenceRoseError PersistenceRoseAnimationReceipt
persistenceRoseAnimationReceipt scene = do
  frames <- persistenceRoseFrameSpecs canonicalAnimationFrameCount
  let renderedFrames = fmap (renderPersistenceRoseFrame scene) frames
  pure
    PersistenceRoseAnimationReceipt
      { roseAnimationFrameCount = canonicalAnimationFrameCount,
        roseAnimationFramesPerSecond = canonicalAnimationFramesPerSecond,
        roseAnimationWidth = canonicalAnimationWidth,
        roseAnimationHeight = canonicalAnimationHeight,
        roseAnimationSourceFingerprint = fingerprintSections renderedFrames
      }

renderPersistenceRoseAnimationReceipt :: PersistenceRoseAnimationReceipt -> String
renderPersistenceRoseAnimationReceipt receipt =
  intercalate
    " "
    [ "MOONLIGHT_PERSISTENCE_ROSE",
      "frame-count=" <> show (roseAnimationFrameCount receipt),
      "fps=" <> show (roseAnimationFramesPerSecond receipt),
      "size=" <> show (roseAnimationWidth receipt) <> "x" <> show (roseAnimationHeight receipt),
      "source-fnv1a64=" <> renderWord64Hex (roseAnimationSourceFingerprint receipt)
    ]

stampPersistenceRoseGif
  :: PersistenceRoseScene
  -> ByteString
  -> Either PersistenceRoseError (PersistenceRoseAnimationReceipt, ByteString)
stampPersistenceRoseGif scene gifBytes = do
  receipt <- persistenceRoseAnimationReceipt scene
  summary <- inspectGif gifBytes
  validateGifSummary receipt summary
  let marker = ByteString.Char8.pack (renderPersistenceRoseAnimationReceipt receipt)
      markerLength = ByteString.length marker
  if markerLength > 255
    then Left (RoseGifMarkerTooLong markerLength)
    else
      if marker `ByteString.isInfixOf` gifBytes
        then Right (receipt, gifBytes)
        else
          if gifMarkerPrefix `ByteString.isInfixOf` gifBytes
            then Left RoseGifProvenanceMismatch
            else do
              insertionOffset <- gifDataStart gifBytes
              let (prefix, suffix) = ByteString.splitAt insertionOffset gifBytes
                  commentExtension =
                    ByteString.pack [0x21, 0xfe, fromIntegral markerLength]
                      <> marker
                      <> ByteString.singleton 0
              pure (receipt, prefix <> commentExtension <> suffix)

verifyPersistenceRoseGif
  :: PersistenceRoseScene
  -> ByteString
  -> Either PersistenceRoseError PersistenceRoseAnimationReceipt
verifyPersistenceRoseGif scene gifBytes = do
  receipt <- persistenceRoseAnimationReceipt scene
  summary <- inspectGif gifBytes
  validateGifSummary receipt summary
  let marker = ByteString.Char8.pack (renderPersistenceRoseAnimationReceipt receipt)
  if marker `ByteString.isInfixOf` gifBytes
    then Right receipt
    else Left RoseGifProvenanceMismatch

data GifSummary = GifSummary
  { gifSummaryWidth :: !Int,
    gifSummaryHeight :: !Int,
    gifSummaryFrameCount :: !Int,
    gifSummaryFrameDelayCount :: !Int,
    gifSummaryDurationCentiseconds :: !Int
  }

inspectGif :: ByteString -> Either PersistenceRoseError GifSummary
inspectGif gifBytes = do
  dataStart <- gifDataStart gifBytes
  width <- gifLittleEndianWord16 gifBytes 6
  height <- gifLittleEndianWord16 gifBytes 8
  (frameCount, frameDelayCount, durationCentiseconds) <-
    countGifFrames gifBytes dataStart 0 0 0
  pure
    GifSummary
      { gifSummaryWidth = width,
        gifSummaryHeight = height,
        gifSummaryFrameCount = frameCount,
        gifSummaryFrameDelayCount = frameDelayCount,
        gifSummaryDurationCentiseconds = durationCentiseconds
      }

gifDataStart :: ByteString -> Either PersistenceRoseError Int
gifDataStart gifBytes = do
  signature <- maybe (Left (RoseGifTruncated 0)) Right (gifSlice gifBytes 0 6)
  if signature == ByteString.Char8.pack "GIF87a" || signature == ByteString.Char8.pack "GIF89a"
    then pure ()
    else Left RoseGifInvalidSignature
  packed <- maybe (Left (RoseGifTruncated 10)) Right (gifByteAt gifBytes 10)
  let globalColourTableBytes =
        if packed .&. 0x80 == 0
          then 0
          else 3 * (2 ^ (fromIntegral (packed .&. 0x07) + 1 :: Int))
      dataStart = 13 + globalColourTableBytes
  maybe (Left (RoseGifTruncated dataStart)) (const (Right dataStart)) (gifSlice gifBytes 0 dataStart)

countGifFrames
  :: ByteString
  -> Int
  -> Int
  -> Int
  -> Int
  -> Either PersistenceRoseError (Int, Int, Int)
countGifFrames gifBytes offset frameCount frameDelayCount durationCentiseconds = do
  block <- maybe (Left (RoseGifTruncated offset)) Right (gifByteAt gifBytes offset)
  case block of
    0x3b -> Right (frameCount, frameDelayCount, durationCentiseconds)
    0x21 -> do
      extensionLabel <- maybe (Left (RoseGifTruncated (offset + 1))) Right (gifByteAt gifBytes (offset + 1))
      nextOffset <- skipGifSubBlocks gifBytes (offset + 2)
      if extensionLabel == 0xf9
        then do
          delay <- gifLittleEndianWord16 gifBytes (offset + 4)
          countGifFrames
            gifBytes
            nextOffset
            frameCount
            (frameDelayCount + 1)
            (durationCentiseconds + delay)
        else
          countGifFrames
            gifBytes
            nextOffset
            frameCount
            frameDelayCount
            durationCentiseconds
    0x2c -> do
      packed <- maybe (Left (RoseGifTruncated (offset + 9))) Right (gifByteAt gifBytes (offset + 9))
      let localColourTableBytes =
            if packed .&. 0x80 == 0
              then 0
              else 3 * (2 ^ (fromIntegral (packed .&. 0x07) + 1 :: Int))
          imageDataStart = offset + 11 + localColourTableBytes
      maybe (Left (RoseGifTruncated imageDataStart)) (const (Right ())) (gifByteAt gifBytes (imageDataStart - 1))
      skipGifSubBlocks gifBytes imageDataStart >>= \next ->
        countGifFrames
          gifBytes
          next
          (frameCount + 1)
          frameDelayCount
          durationCentiseconds
    0x00 ->
      countGifFrames
        gifBytes
        (offset + 1)
        frameCount
        frameDelayCount
        durationCentiseconds
    unexpected -> Left (RoseGifUnexpectedBlock offset unexpected)

skipGifSubBlocks :: ByteString -> Int -> Either PersistenceRoseError Int
skipGifSubBlocks gifBytes offset = do
  blockLength <- maybe (Left (RoseGifTruncated offset)) (Right . fromIntegral) (gifByteAt gifBytes offset)
  if blockLength == 0
    then Right (offset + 1)
    else do
      let nextOffset = offset + 1 + blockLength
      maybe (Left (RoseGifTruncated nextOffset)) (const (Right ())) (gifSlice gifBytes offset (1 + blockLength))
      skipGifSubBlocks gifBytes nextOffset

validateGifSummary
  :: PersistenceRoseAnimationReceipt
  -> GifSummary
  -> Either PersistenceRoseError ()
validateGifSummary receipt summary
  | actualWidth /= expectedWidth || actualHeight /= expectedHeight =
      Left (RoseGifDimensionMismatch expectedWidth expectedHeight actualWidth actualHeight)
  | actualFrameCount /= expectedFrameCount =
      Left (RoseGifFrameCountMismatch expectedFrameCount actualFrameCount)
  | actualFrameDelayCount /= expectedFrameCount =
      Left (RoseGifFrameDelayCountMismatch expectedFrameCount actualFrameDelayCount)
  | actualDurationCentiseconds /= canonicalAnimationDurationCentiseconds =
      Left
        ( RoseGifDurationMismatch
            canonicalAnimationDurationCentiseconds
            actualDurationCentiseconds
        )
  | otherwise = Right ()
 where
  expectedWidth = roseAnimationWidth receipt
  expectedHeight = roseAnimationHeight receipt
  actualWidth = gifSummaryWidth summary
  actualHeight = gifSummaryHeight summary
  expectedFrameCount = roseAnimationFrameCount receipt
  actualFrameCount = gifSummaryFrameCount summary
  actualFrameDelayCount = gifSummaryFrameDelayCount summary
  actualDurationCentiseconds = gifSummaryDurationCentiseconds summary

gifLittleEndianWord16 :: ByteString -> Int -> Either PersistenceRoseError Int
gifLittleEndianWord16 gifBytes offset = do
  low <- maybe (Left (RoseGifTruncated offset)) (Right . fromIntegral) (gifByteAt gifBytes offset)
  high <- maybe (Left (RoseGifTruncated (offset + 1))) (Right . fromIntegral) (gifByteAt gifBytes (offset + 1))
  pure (low + 256 * high)

gifByteAt :: ByteString -> Int -> Maybe Word8
gifByteAt gifBytes offset = fst <$> ByteString.uncons (ByteString.drop offset gifBytes)

gifSlice :: ByteString -> Int -> Int -> Maybe ByteString
gifSlice gifBytes offset width =
  let selected = ByteString.take width (ByteString.drop offset gifBytes)
   in if ByteString.length selected == width then Just selected else Nothing

fingerprintSections :: [String] -> Word64
fingerprintSections =
  foldl'
    (\fingerprint section -> fingerprintByte (fingerprintText fingerprint section) 0)
    fnvOffsetBasis

fingerprintText :: Word64 -> String -> Word64
fingerprintText fingerprint =
  ByteString.foldl' fingerprintByte fingerprint
    . Text.Encoding.encodeUtf8
    . Text.pack

fingerprintByte :: Word64 -> Word8 -> Word64
fingerprintByte fingerprint byte =
  (fingerprint `xor` fromIntegral byte) * fnvPrime

renderWord64Hex :: Word64 -> String
renderWord64Hex value =
  let rendered = showHex value ""
   in replicate (16 - length rendered) '0' <> rendered

gifMarkerPrefix :: ByteString
gifMarkerPrefix = ByteString.Char8.pack "MOONLIGHT_PERSISTENCE_ROSE "

canonicalAnimationFrameCount :: Int
canonicalAnimationFrameCount = 65

canonicalAnimationFramesPerSecond :: Int
canonicalAnimationFramesPerSecond = 8

canonicalAnimationWidth :: Int
canonicalAnimationWidth = 960

canonicalAnimationHeight :: Int
canonicalAnimationHeight = 540

canonicalAnimationDurationCentiseconds :: Int
canonicalAnimationDurationCentiseconds = 813

fnvOffsetBasis :: Word64
fnvOffsetBasis = 14695981039346656037

fnvPrime :: Word64
fnvPrime = 1099511628211

frameProgressAt :: Int -> Int -> Double
frameProgressAt frameCount frameIndex
  | frameIndex <= 0 || frameIndex >= frameCount - 1 = 0
  | otherwise =
      let phase = fromIntegral frameIndex / fromIntegral (frameCount - 1)
       in 0.5 - 0.5 * cos (twoPi * phase)

renderScene :: PersistenceRoseScene -> Int -> Double -> Bool -> String
renderScene scene selectedRank progress poster =
  let selectedState = criticalStateAt selectedRank (sceneCriticalStates scene)
      finalRank = max 1 (criticalRank (NonEmpty.last (sceneCriticalStates scene)))
      activation = if poster then fromIntegral selectedRank / fromIntegral finalRank else progress
   in concat
        [ "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 1920 1080\" width=\"1920\" height=\"1080\" role=\"img\" aria-labelledby=\"title description\">\n",
          "<title id=\"title\">Moonlight Triangulation Persistence Rose</title>\n",
          "<desc id=\"description\">Exact Delaunay alpha filtration, persistent homology, Voronoi dual, Sibson interpolation, exact overlay, and Minkowski morphology composed into a seven-petal rose.</desc>\n",
          renderDefinitions (sceneMaskComponents scene),
          "<rect width=\"1920\" height=\"1080\" fill=\"url(#background)\"/>\n",
          renderAmbientOrbits activation,
          renderPersistenceHalo finalRank selectedRank (scenePersistenceArcs scene),
          "<g clip-path=\"url(#rose-clip)\">\n",
          "<path d=\"", renderMaskPath (sceneMaskComponents scene), "\" fill=\"url(#rose-wash)\" opacity=\"0.28\"/>\n",
          "<g filter=\"url(#field-soften)\">", concatMap renderFieldSample (sceneFieldSamples scene), "</g>\n",
          "<g opacity=\"0.58\">", concatMap renderVoronoiSegment (sceneVoronoiSegments scene), "</g>\n",
          concatMap (renderRankedTriangle finalRank activation) (sceneFaces scene),
          concatMap (renderRankedSegment finalRank activation) (sceneEdges scene),
          "<g filter=\"url(#hole-glow)\">", concatMap renderHoleComponent (criticalHoles selectedState), "</g>\n",
          concatMap (renderSite activation) (sceneSites scene),
          "</g>\n",
          "<path d=\"", renderMaskPath (sceneMaskComponents scene), "\" fill=\"none\" stroke=\"url(#mask-stroke)\" stroke-width=\"2.2\" opacity=\"0.92\" filter=\"url(#edge-glow)\"/>\n",
          renderTitle selectedState,
          "</svg>\n"
        ]

criticalStateAt :: Int -> NonEmpty CriticalState -> CriticalState
criticalStateAt target (firstState :| remainingStates) =
  foldl'
    (\chosen candidate -> if criticalRank candidate <= target then candidate else chosen)
    firstState
    remainingStates

renderDefinitions :: [MaskComponent] -> String
renderDefinitions maskComponents =
  concat
    [ "<defs>\n",
      "<radialGradient id=\"background\" cx=\"52%\" cy=\"48%\" r=\"72%\"><stop offset=\"0\" stop-color=\"#10143d\"/><stop offset=\"0.48\" stop-color=\"#070b24\"/><stop offset=\"1\" stop-color=\"#02040e\"/></radialGradient>\n",
      "<radialGradient id=\"rose-wash\" cx=\"48%\" cy=\"42%\" r=\"63%\"><stop offset=\"0\" stop-color=\"#ffe7a3\"/><stop offset=\"0.38\" stop-color=\"#ff5fb7\"/><stop offset=\"0.72\" stop-color=\"#7c4dff\"/><stop offset=\"1\" stop-color=\"#00d9ff\"/></radialGradient>\n",
      "<linearGradient id=\"mask-stroke\" x1=\"0\" y1=\"0\" x2=\"1\" y2=\"1\"><stop offset=\"0\" stop-color=\"#ffd86b\"/><stop offset=\"0.48\" stop-color=\"#ff72c6\"/><stop offset=\"1\" stop-color=\"#66efff\"/></linearGradient>\n",
      "<linearGradient id=\"hole-stroke\" x1=\"0\" y1=\"0\" x2=\"1\" y2=\"1\"><stop offset=\"0\" stop-color=\"#fff9db\"/><stop offset=\"0.52\" stop-color=\"#ff9ad7\"/><stop offset=\"1\" stop-color=\"#9ff7ff\"/></linearGradient>\n",
      "<filter id=\"field-soften\" x=\"-20%\" y=\"-20%\" width=\"140%\" height=\"140%\"><feGaussianBlur stdDeviation=\"24\"/></filter>\n",
      "<filter id=\"hole-glow\" x=\"-30%\" y=\"-30%\" width=\"160%\" height=\"160%\"><feGaussianBlur stdDeviation=\"7\" result=\"b\"/><feMerge><feMergeNode in=\"b\"/><feMergeNode in=\"SourceGraphic\"/></feMerge></filter>\n",
      "<filter id=\"edge-glow\" x=\"-20%\" y=\"-20%\" width=\"140%\" height=\"140%\"><feGaussianBlur stdDeviation=\"3\" result=\"b\"/><feMerge><feMergeNode in=\"b\"/><feMergeNode in=\"SourceGraphic\"/></feMerge></filter>\n",
      "<clipPath id=\"rose-clip\"><path d=\"",
      renderMaskPath maskComponents,
      "\" fill-rule=\"evenodd\"/></clipPath>\n",
      "</defs>\n"
    ]

renderAmbientOrbits :: Double -> String
renderAmbientOrbits activation =
  concatMap
    ( \orbit ->
        let radius = 322 + fromIntegral orbit * 34
            opacity = 0.055 + activation * 0.025
         in "<circle cx=\"960\" cy=\"552\" r=\""
              <> formatDouble radius
              <> "\" fill=\"none\" stroke=\"#8ba5ff\" stroke-width=\"0.8\" stroke-dasharray=\"2 12\" opacity=\""
              <> formatDouble opacity
              <> "\"/>\n"
    )
    [0 .. 5 :: Int]

renderPersistenceHalo :: Int -> Int -> [PersistenceArc] -> String
renderPersistenceHalo finalRank selectedRank arcs =
  "<g fill=\"none\" stroke-linecap=\"round\">\n"
    <> concatMap renderArc (zip ([0 :: Int ..]) arcs)
    <> "</g>\n"
 where
  renderArc :: (Int, PersistenceArc) -> String
  renderArc (index, arc) =
    let active = persistenceArcBirthRank arc <= selectedRank && selectedRank < persistenceArcDeathRank arc
        radius = 414 + fromIntegral (index `mod` 7) * 7.5
        start = rankAngle finalRank (persistenceArcBirthRank arc)
        finish = rankAngle finalRank (persistenceArcDeathRank arc)
        points = arcPolyline radius start finish 42
        hue = 188 + (index * 17 `mod` 122)
        opacity = if active then 0.72 else 0.16
        width = if active then 2.6 else 1.1
     in "<polyline points=\""
          <> renderScreenPoints points
          <> "\" stroke=\"hsl("
          <> show hue
          <> " 92% 72%)\" stroke-width=\""
          <> formatDouble width
          <> "\" opacity=\""
          <> formatDouble opacity
          <> "\"/>\n"

rankAngle :: Int -> Int -> Double
rankAngle finalRank rankValue =
  -pi / 2 + twoPi * fromIntegral rankValue / fromIntegral (max 1 finalRank)

arcPolyline :: Double -> Double -> Double -> Int -> [Point]
arcPolyline radius start finish sampleCount =
  fmap
    ( \sampleIndex ->
        let fraction = fromIntegral sampleIndex / fromIntegral sampleCount
            angle = start + fraction * (finish - start)
         in screenPointFromPolar radius angle
    )
    [0 .. sampleCount]

screenPointFromPolar :: Double -> Double -> Point
screenPointFromPolar radius angle =
  Point (960 + radius * cos angle) (552 + radius * sin angle)

renderFieldSample :: FieldSample -> String
renderFieldSample FieldSample{fieldSamplePoint, fieldSampleValue, fieldSampleNeighborCount} =
  let Point x y = toScreen fieldSamplePoint
      hue = round (186 + 126 * fieldSampleValue) :: Int
      radius = 30 + min 5 (fromIntegral fieldSampleNeighborCount)
      opacity = 0.16 + 0.16 * fieldSampleValue
   in "<circle cx=\""
        <> formatDouble x
        <> "\" cy=\""
        <> formatDouble y
        <> "\" r=\""
        <> formatDouble radius
        <> "\" fill=\"hsl("
        <> show hue
        <> " 94% 61%)\" opacity=\""
        <> formatDouble opacity
        <> "\"/>\n"

renderVoronoiSegment :: Segment -> String
renderVoronoiSegment segment =
  renderLine segment "#61efff" 1.15 0.5 ""

renderRankedTriangle :: Int -> Double -> RankedTriangle -> String
renderRankedTriangle finalRank activation RankedTriangle{rankedTrianglePoints = (firstPoint, secondPoint, thirdPoint), rankedTriangleBirthRank} =
  let opacity = activationOpacity finalRank activation rankedTriangleBirthRank
      hue :: Int
      hue = 282 - round (94 * fromIntegral rankedTriangleBirthRank / fromIntegral (max 1 finalRank) :: Double)
   in if opacity <= 0.001
        then ""
        else
          "<polygon points=\""
            <> renderWorldPoints [firstPoint, secondPoint, thirdPoint]
            <> "\" fill=\"hsl("
            <> show hue
            <> " 88% 60%)\" opacity=\""
            <> formatDouble (0.06 + 0.17 * opacity)
            <> "\"/>\n"

renderRankedSegment :: Int -> Double -> RankedSegment -> String
renderRankedSegment finalRank activation RankedSegment{rankedSegmentGeometry, rankedSegmentBirthRank} =
  let opacity = activationOpacity finalRank activation rankedSegmentBirthRank
      width = 0.75 + 1.25 * opacity
      color = if opacity > 0.72 then "#ffd56a" else "#dd78ff"
   in renderLine rankedSegmentGeometry color width (0.06 + 0.82 * opacity) "filter=\"url(#edge-glow)\""

activationOpacity :: Int -> Double -> Int -> Double
activationOpacity finalRank activation birthRank =
  let level = activation * fromIntegral finalRank
      distance = level - fromIntegral birthRank
   in smoothstep (distance + 0.6)

smoothstep :: Double -> Double
smoothstep value =
  let bounded = clamp 0 1 value
   in bounded * bounded * (3 - 2 * bounded)

renderHoleComponent :: HoleComponent -> String
renderHoleComponent (HoleComponent loops) =
  concatMap
    ( \points ->
        "<path d=\""
          <> renderClosedWorldPath points
          <> "\" fill=\"none\" stroke=\"url(#hole-stroke)\" stroke-width=\"5.4\" stroke-linejoin=\"round\" opacity=\"0.98\"/>\n"
    )
    loops

renderSite :: Double -> Point -> String
renderSite activation point =
  let Point x y = toScreen point
      radius = 2.5 + 1.5 * activation
   in "<circle cx=\""
        <> formatDouble x
        <> "\" cy=\""
        <> formatDouble y
        <> "\" r=\""
        <> formatDouble radius
        <> "\" fill=\"#fff8d9\" stroke=\"#ff9ad7\" stroke-width=\"1.2\" opacity=\"0.95\"/>\n"

renderLine :: Segment -> String -> Double -> Double -> String -> String
renderLine (Segment fromPoint toPoint) color width opacity extraAttributes =
  let Point fromX fromY = toScreen fromPoint
      Point toX toY = toScreen toPoint
   in "<line x1=\""
        <> formatDouble fromX
        <> "\" y1=\""
        <> formatDouble fromY
        <> "\" x2=\""
        <> formatDouble toX
        <> "\" y2=\""
        <> formatDouble toY
        <> "\" stroke=\""
        <> color
        <> "\" stroke-width=\""
        <> formatDouble width
        <> "\" stroke-linecap=\"round\" opacity=\""
        <> formatDouble opacity
        <> "\" "
        <> extraAttributes
        <> "/>\n"

renderTitle :: CriticalState -> String
renderTitle state =
  let alphaFraction = exactFractionSummary (criticalBirth state)
      alphaDecimal = formatDouble (alphaBirthToDouble (criticalBirth state))
      bettiMeaning = bettiOneMeaning (criticalBettiOne state)
   in concat
        [ "<g font-family=\"Inter, Avenir Next, Helvetica, Arial, sans-serif\" fill=\"#f7f3ff\">\n",
          "<text x=\"72\" y=\"82\" font-size=\"24\" font-weight=\"600\" letter-spacing=\"8\" fill=\"#9ff7ff\">MOONLIGHT TRIANGULATION</text>\n",
          "<text x=\"70\" y=\"145\" font-size=\"58\" font-weight=\"780\" letter-spacing=\"-1.5\">PERSISTENCE ROSE</text>\n",
          "<g transform=\"translate(1500 790)\">\n",
          "<text x=\"0\" y=\"0\" font-size=\"18\" letter-spacing=\"2.4\" fill=\"#91a8d8\">TOPOLOGICAL HOLES AT THIS SCALE</text>\n",
          "<text x=\"0\" y=\"74\" font-size=\"78\" font-weight=\"720\" fill=\"#fff5cd\">β₁ = ",
          show (criticalBettiOne state),
          "</text>\n",
          "<text x=\"2\" y=\"116\" font-size=\"17\" fill=\"#c9d3ff\">",
          bettiMeaning,
          "</text>\n",
          "<text x=\"2\" y=\"144\" font-size=\"15\" fill=\"#9ba8d4\">in the exact alpha complex</text>\n",
          "<text x=\"2\" y=\"176\" font-size=\"14\" fill=\"#7682aa\">critical rank ",
          show (criticalRank state),
          " · α² = ",
          alphaDecimal,
          "</text>\n",
          "<text x=\"2\" y=\"200\" font-size=\"12\" fill=\"#596486\">",
          alphaFraction,
          "</text>\n",
          "</g>\n",
          "</g>\n"
        ]

bettiOneMeaning :: Int -> String
bettiOneMeaning holeCount =
  case holeCount of
    0 -> "no independent one-dimensional holes"
    1 -> "1 independent one-dimensional hole"
    _ -> show holeCount <> " independent one-dimensional holes"

exactFractionSummary :: AlphaBirth -> String
exactFractionSummary birth =
  let numeratorDigits = length (show (abs (alphaBirthNumerator birth)))
      denominatorDigits = length (show (alphaBirthDenominator birth))
   in "exact " <> show numeratorDigits <> "-digit / " <> show denominatorDigits <> "-digit rational"

renderMaskPath :: [MaskComponent] -> String
renderMaskPath =
  intercalate " "
    . concatMap
      (\MaskComponent{maskOuterLoop, maskHoleLoops} -> renderClosedScreenPath maskOuterLoop : fmap renderClosedScreenPath maskHoleLoops)

renderClosedWorldPath :: [Point] -> String
renderClosedWorldPath = renderClosedPath . fmap toScreen

renderClosedScreenPath :: [Point] -> String
renderClosedScreenPath = renderClosedPath . fmap toScreen

renderClosedPath :: [Point] -> String
renderClosedPath points =
  case points of
    [] -> ""
    firstPoint : remainingPoints ->
      "M " <> renderPoint firstPoint <> concatMap (" L " <>) (fmap renderPoint remainingPoints) <> " Z"

renderWorldPoints :: [Point] -> String
renderWorldPoints = renderScreenPoints . fmap toScreen

renderScreenPoints :: [Point] -> String
renderScreenPoints = unwords . fmap renderPoint

renderPoint :: Point -> String
renderPoint (Point x y) = formatDouble x <> "," <> formatDouble y

toScreen :: Point -> Point
toScreen (Point x y) = Point (960 + x * 101) (552 + y * 101)

formatDouble :: Double -> String
formatDouble value = showFFloat (Just 3) value ""

clamp :: Double -> Double -> Double -> Double
clamp lower upper = min upper . max lower

clampInt :: Int -> Int -> Int -> Int
clampInt lower upper = min upper . max lower

twoPi :: Double
twoPi = 2 * pi
