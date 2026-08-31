-- | Exact alpha-complex persistence across non-nested activation depths.
--
-- Each observed depth is rebuilt independently from stable labelled points.
-- Consecutive complexes are compared through the canonical union cospan
-- @K_i -> K_i union K_(i+1) <- K_(i+1)@.  Stable simplex labels, never DCEL
-- handles, own cross-mesh identity.  Homology remains the sole owner of the
-- zigzag interval decomposition.
module Moonlight.Triangulation.Zigzag
  ( ActivationPoint (..),
    ActivationSlice (..),
    ActivationStage (..),
    ActivationZigzagError (..),
    ActivationComplex,
    activationComplexStage,
    activationComplexCells,
    activationComplexBasisAt,
    activationComplexChainComplex,
    AdjacentUnionWitness,
    adjacentUnionStage,
    adjacentCommonCells,
    adjacentUnionComplex,
    adjacentLeftInclusion,
    adjacentRightInclusion,
    ActivationZigzag,
    activationZigzagStages,
    activationZigzagComplexes,
    activationZigzagCorrespondences,
    activationZigzagDiagram,
    ZigzagInterval (..),
    ActivationInterval,
    activationAlphaZigzag,
    activationZigzagIntervals,
    activationAlphaPersistence,
  )
where

import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Moonlight.Core (firstDuplicate)
import Moonlight.Homology.Boundary
  ( BoundaryIncidence,
    FiniteChainComplex,
    overlapBoundaryIncidence,
  )
import Moonlight.Homology.Chain
  ( HomologicalDegree (..),
  )
import Moonlight.Homology.Persistence
  ( FiniteChainMap,
    FiniteChainZigzag,
    ZigzagArrow (..),
    ZigzagFailure,
    ZigzagInterval (..),
    mkFiniteChainMapChecked,
    mkFiniteChainZigzag,
    rationalZigzagIntervals,
  )
import Moonlight.Homology.Topology qualified as CellComplex
import Moonlight.Triangulation.Alpha
  ( AlphaFiltrationError,
    alphaComplexAtRadius,
    alphaFiltration,
  )
import Moonlight.Triangulation.BulkLoad
  ( DuplicatePayloadPolicy (KeepFirstPayload),
    delaunayFromCoordinates,
  )
import Moonlight.Triangulation.CellSet (CellSelectionError)
import Moonlight.Triangulation.CellComplex
  ( DCELComplex,
    fromExactCellSet,
  )
import Moonlight.Triangulation.LabelledComplex
  ( PlanarComplexInterpretationError,
    finitePlanarComplex,
  )
import Moonlight.Triangulation.Simplex
  ( PlanarComplex,
    PlanarComplexError,
    PlanarSimplex,
    PlanarSimplexError,
    SimplexDimension (..),
    intersectPlanarComplex,
    planarComplex,
    planarComplexBasisAt,
    planarComplexCells,
    planarEdge,
    planarFace,
    planarVertex,
    unionPlanarComplex,
  )
import Moonlight.Triangulation.Dcel qualified as Dcel
import Moonlight.Triangulation.Handles.HandleDefs
  ( FaceId,
    UndirectedEdgeId,
  )
import Moonlight.Triangulation.Types
  ( BuildError,
    BuildResult (..),
    BuildStats (..),
    DelaunayTriangulation,
    Point,
    RadiusSquared,
    unitElementDefaults,
  )

-- | One stable observation identity and its two-dimensional embedding.  The
-- coordinates may change at every depth; the label is the correspondence.
data ActivationPoint label = ActivationPoint
  { activationPointLabel :: !label,
    activationPointCoordinates :: !Point
  }
  deriving stock (Eq, Show)

-- | One independently sampled depth and the exact squared-radius alpha
-- threshold to observe there.
data ActivationSlice depth label = ActivationSlice
  { activationSliceDepth :: !depth,
    activationSliceRadiusSquared :: !RadiusSquared,
    activationSlicePoints :: !(Vector (ActivationPoint label))
  }
  deriving stock (Eq, Show)

-- | Vertices of the expanded zigzag alternate between observed depths and
-- their adjacent common ambient unions.
data ActivationStage depth
  = ObservedDepth !depth
  | AdjacentDepthUnion !depth !depth
  deriving stock (Eq, Ord, Show)

data ActivationZigzagError depth label
  = ActivationDuplicateDepth !depth
  | ActivationDuplicateLabel !depth !label
  | ActivationCoordinateCollapse !depth !Int !Int
  | ActivationBuildFailed !depth !BuildError
  | ActivationFiltrationFailed !depth !AlphaFiltrationError
  | ActivationSelectionFailed !depth !CellSelectionError
  | ActivationInnerFaceInvalid !(ActivationStage depth) !FaceId
  | ActivationSimplexInvalid !(ActivationStage depth) !(PlanarSimplexError label)
  | ActivationComplexInvalid !(ActivationStage depth) !(PlanarComplexError label)
  | ActivationChainComplexFailed
      !(ActivationStage depth)
      !(PlanarComplexInterpretationError label)
  | ActivationZigzagFailed !ZigzagFailure
  | ActivationStageIndexMissing !Int
  deriving stock (Eq, Show)

-- | A checked, canonically labelled simplicial interpretation of one observed
-- alpha complex or adjacent union.
data ActivationComplex depth label = ActivationComplex
  { activationComplexStage :: !(ActivationStage depth),
    storedActivationPlanarComplex :: !(PlanarComplex label),
    activationComplexChainComplex :: !(FiniteChainComplex Int)
  }

activationComplexCells
  :: ActivationComplex depth label
  -> Set (PlanarSimplex label)
activationComplexCells = planarComplexCells . storedActivationPlanarComplex

-- | The canonical labelled basis corresponding to the numeric basis of
-- 'activationComplexChainComplex' at one degree.
activationComplexBasisAt ::
  ActivationComplex depth label ->
  HomologicalDegree ->
  [PlanarSimplex label]
activationComplexBasisAt complexValue degreeValue =
  maybe
    []
    (Vector.toList . (`planarComplexBasisAt` storedActivationPlanarComplex complexValue))
    (simplexDimensionAt degreeValue)

-- | The two checked inclusion legs proving how adjacent observations meet in
-- their union.  The common-cell set is a derived inspection surface.
data AdjacentUnionWitness depth label = AdjacentUnionWitness
  { adjacentCommonCells :: !(Set (PlanarSimplex label)),
    adjacentUnionComplex :: !(ActivationComplex depth label),
    adjacentLeftInclusion :: !(FiniteChainMap Int),
    adjacentRightInclusion :: !(FiniteChainMap Int)
  }

adjacentUnionStage :: AdjacentUnionWitness depth label -> ActivationStage depth
adjacentUnionStage = activationComplexStage . adjacentUnionComplex

-- | The expanded stages, admitted observations, correspondence witnesses, and
-- authoritative Homology diagram travel together.
data ActivationZigzag depth label = ActivationZigzag
  { activationZigzagComplexes :: !(NonEmpty (ActivationComplex depth label)),
    activationZigzagCorrespondences :: !(Vector (AdjacentUnionWitness depth label)),
    activationZigzagDiagram :: !(FiniteChainZigzag Int)
  }

-- | The labelled view of the Homology diagram.  It is derived from the
-- admitted observations and their union witnesses rather than stored as a
-- second sequence that could disagree with them.
activationZigzagStages :: ActivationZigzag depth label -> Vector (ActivationStage depth)
activationZigzagStages zigzag =
  let firstComplex :| remainingComplexes = activationZigzagComplexes zigzag
   in Vector.fromList
        ( activationComplexStage firstComplex
            : concat
              ( zipWith
                  (\witness rightComplex ->
                      [adjacentUnionStage witness, activationComplexStage rightComplex]
                  )
                  (Vector.toList (activationZigzagCorrespondences zigzag))
                  remainingComplexes
              )
        )

-- | The Homology interval carrier with endpoints resolved into the activation
-- stage vocabulary.  This is a specialization, not a parallel record.
type ActivationInterval depth = ZigzagInterval (ActivationStage depth)

activationAlphaZigzag ::
  (Ord depth, Ord label) =>
  NonEmpty (ActivationSlice depth label) ->
  Either (ActivationZigzagError depth label) (ActivationZigzag depth label)
activationAlphaZigzag slices = do
  case firstDuplicate (fmap activationSliceDepth (NonEmpty.toList slices)) of
    Just duplicateDepth -> Left (ActivationDuplicateDepth duplicateDepth)
    Nothing -> Right ()
  observedComplexes <- traverse activationComplexFromSlice slices
  buildActivationZigzag
    (NonEmpty.zip (fmap activationSliceDepth slices) observedComplexes)

activationZigzagIntervals ::
  ActivationZigzag depth label ->
  Either (ActivationZigzagError depth label) [ActivationInterval depth]
activationZigzagIntervals zigzag = do
  intervals <-
    first ActivationZigzagFailed
      (rationalZigzagIntervals (activationZigzagDiagram zigzag))
  traverse (traverse (requireStage (activationZigzagStages zigzag))) intervals

activationAlphaPersistence ::
  (Ord depth, Ord label) =>
  NonEmpty (ActivationSlice depth label) ->
  Either (ActivationZigzagError depth label) [ActivationInterval depth]
activationAlphaPersistence slices =
  activationAlphaZigzag slices >>= activationZigzagIntervals

activationComplexFromSlice ::
  Ord label =>
  ActivationSlice depth label ->
  Either (ActivationZigzagError depth label) (ActivationComplex depth label)
activationComplexFromSlice slice = do
  let depthValue = activationSliceDepth slice
      points = activationSlicePoints slice
      labels = fmap activationPointLabel points
  case firstDuplicate (Vector.toList labels) of
    Just duplicateLabel -> Left (ActivationDuplicateLabel depthValue duplicateLabel)
    Nothing -> Right ()
  buildResult <-
    first (ActivationBuildFailed depthValue)
      ( delaunayFromCoordinates
          unitElementDefaults
          (fmap activationPointCoordinates points)
          labels
          KeepFirstPayload
      )
  let inputCount = Vector.length points
      uniqueCount = statUniquePoints (buildStats buildResult)
  if uniqueCount /= inputCount
    then Left (ActivationCoordinateCollapse depthValue inputCount uniqueCount)
    else do
      let triangulation = buildTriangulation buildResult
      filtration <-
        first (ActivationFiltrationFailed depthValue) (alphaFiltration triangulation)
      selectedCells <-
        first (ActivationSelectionFailed depthValue)
          (alphaComplexAtRadius (activationSliceRadiusSquared slice) filtration)
      let stage = ObservedDepth depthValue
      cellSet <- activationCellsFromDcel stage triangulation (fromExactCellSet selectedCells)
      compileActivationComplex stage cellSet

activationCellsFromDcel ::
  Ord label =>
  ActivationStage depth ->
  DelaunayTriangulation label ->
  DCELComplex ->
  Either (ActivationZigzagError depth label) (PlanarComplex label)
activationCellsFromDcel stage triangulation complexValue = do
  let vertexCells =
        fmap
          (planarVertex . Dcel.vertexData triangulation)
          (CellComplex.vertices complexValue)
  edgeCells <-
    traverse
      (activationEdgeFromDcel stage triangulation complexValue)
      (CellComplex.edges complexValue)
  faceCells <-
    traverse
      (activationFaceFromDcel stage triangulation)
      (CellComplex.faces complexValue)
  first (ActivationComplexInvalid stage)
    (planarComplex (Set.fromList (vertexCells <> edgeCells <> faceCells)))

activationEdgeFromDcel ::
  Ord label =>
  ActivationStage depth ->
  DelaunayTriangulation label ->
  DCELComplex ->
  UndirectedEdgeId ->
  Either (ActivationZigzagError depth label) (PlanarSimplex label)
activationEdgeFromDcel stage triangulation complexValue edgeValue =
  let (firstVertex, secondVertex) = CellComplex.edgeBoundary complexValue edgeValue
   in first (ActivationSimplexInvalid stage)
        ( planarEdge
            (Dcel.vertexData triangulation firstVertex)
            (Dcel.vertexData triangulation secondVertex)
        )

activationFaceFromDcel ::
  Ord label =>
  ActivationStage depth ->
  DelaunayTriangulation label ->
  FaceId ->
  Either (ActivationZigzagError depth label) (PlanarSimplex label)
activationFaceFromDcel stage triangulation faceValue =
  case Dcel.innerFaceVertices triangulation faceValue of
    Nothing -> Left (ActivationInnerFaceInvalid stage faceValue)
    Just (firstVertex, secondVertex, thirdVertex) ->
      first (ActivationSimplexInvalid stage)
        ( planarFace
            (Dcel.vertexData triangulation firstVertex)
            (Dcel.vertexData triangulation secondVertex)
            (Dcel.vertexData triangulation thirdVertex)
        )

compileActivationComplex ::
  Ord label =>
  ActivationStage depth ->
  PlanarComplex label ->
  Either (ActivationZigzagError depth label) (ActivationComplex depth label)
compileActivationComplex stage complexValue = do
  chainComplex <-
    first (ActivationChainComplexFailed stage)
      (finitePlanarComplex complexValue)
  pure
    ActivationComplex
      { activationComplexStage = stage,
        storedActivationPlanarComplex = complexValue,
        activationComplexChainComplex = chainComplex
      }

buildActivationZigzag ::
  Ord label =>
  NonEmpty (depth, ActivationComplex depth label) ->
  Either (ActivationZigzagError depth label) (ActivationZigzag depth label)
buildActivationZigzag observedEntries@((_, firstComplex) :| remainingEntries) = do
  correspondences <-
    traverse
      ( \((leftDepth, leftComplex), (rightDepth, rightComplex)) ->
          adjacentUnionWitness leftDepth rightDepth leftComplex rightComplex
      )
      (zip (NonEmpty.toList observedEntries) remainingEntries)
  let arrows =
        correspondences
          >>= ( \witness ->
                  [ ForwardArrow (adjacentLeftInclusion witness),
                    BackwardArrow (adjacentRightInclusion witness)
                  ]
              )
  diagram <-
    first ActivationZigzagFailed
      ( mkFiniteChainZigzag
          (activationComplexChainComplex firstComplex)
          arrows
      )
  pure
    ActivationZigzag
      { activationZigzagComplexes = fmap snd observedEntries,
        activationZigzagCorrespondences = Vector.fromList correspondences,
        activationZigzagDiagram = diagram
      }

adjacentUnionWitness ::
  Ord label =>
  depth ->
  depth ->
  ActivationComplex depth label ->
  ActivationComplex depth label ->
  Either (ActivationZigzagError depth label) (AdjacentUnionWitness depth label)
adjacentUnionWitness leftDepth rightDepth leftComplex rightComplex = do
  let unionStage =
        AdjacentDepthUnion leftDepth rightDepth
      unionPlanar =
        unionPlanarComplex
          (storedActivationPlanarComplex leftComplex)
          (storedActivationPlanarComplex rightComplex)
  unionComplex <- compileActivationComplex unionStage unionPlanar
  leftInclusion <- activationInclusion leftComplex unionComplex
  rightInclusion <- activationInclusion rightComplex unionComplex
  pure
    AdjacentUnionWitness
      { adjacentCommonCells =
          planarComplexCells
            ( intersectPlanarComplex
                (storedActivationPlanarComplex leftComplex)
                (storedActivationPlanarComplex rightComplex)
            ),
        adjacentUnionComplex = unionComplex,
        adjacentLeftInclusion = leftInclusion,
        adjacentRightInclusion = rightInclusion
      }

activationInclusion ::
  Ord label =>
  ActivationComplex depth label ->
  ActivationComplex depth label ->
  Either (ActivationZigzagError depth label) (FiniteChainMap Int)
activationInclusion sourceComplex targetComplex =
  first ActivationZigzagFailed
    ( mkFiniteChainMapChecked
        (activationComplexChainComplex sourceComplex)
        (activationComplexChainComplex targetComplex)
        (inclusionAtDegree sourceComplex targetComplex)
    )

inclusionAtDegree ::
  Ord label =>
  ActivationComplex depth label ->
  ActivationComplex depth label ->
  HomologicalDegree ->
  BoundaryIncidence Int
inclusionAtDegree sourceComplex targetComplex degreeValue =
  overlapBoundaryIncidence
    (1 :: Int)
    (activationComplexBasisAt sourceComplex degreeValue)
    (activationComplexBasisAt targetComplex degreeValue)

simplexDimensionAt :: HomologicalDegree -> Maybe SimplexDimension
simplexDimensionAt (HomologicalDegree degreeIndex) =
  case degreeIndex of
    0 -> Just SimplexDimension0
    1 -> Just SimplexDimension1
    2 -> Just SimplexDimension2
    _ -> Nothing

requireStage ::
  Vector (ActivationStage depth) ->
  Int ->
  Either (ActivationZigzagError depth label) (ActivationStage depth)
requireStage stages stageIndex =
  maybe
    (Left (ActivationStageIndexMissing stageIndex))
    Right
    (stages Vector.!? stageIndex)
