{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}

-- | Exact persistence for finite, non-monotone diagrams of chain complexes.
--
-- A 'FiniteChainMap' is admitted only after its component maps commute with
-- the two boundary operators.  A 'FiniteChainZigzag' then glues checked maps
-- along structurally equal endpoints.  Persistence is computed on rational
-- homology by propagating the right filtration of each type-A prefix; quotient
-- layers that fail descent close at that arrow, and the terminal layers are
-- the surviving intervals.
module Moonlight.Homology.Pure.Topology.Zigzag
  ( ChainMapEndpoint (..),
    ZigzagDirection (..),
    ZigzagFailure (..),
    FiniteChainMap,
    finiteChainMapSource,
    finiteChainMapTarget,
    finiteChainMapAt,
    mkFiniteChainMapChecked,
    ZigzagArrow (..),
    zigzagArrowDirection,
    FiniteChainZigzag,
    mkFiniteChainZigzag,
    finiteChainZigzagComplexes,
    finiteChainZigzagArrows,
    ZigzagInterval (..),
    rationalZigzagIntervals,
    zigzagBettiAt,
  )
where

import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Moonlight.Core (Semiring)
import Moonlight.Homology.Boundary.Finite
  ( FiniteChainComplex,
    degreeCardinality,
    incidenceMatrixAt,
    maxHomologicalDegree,
    validateFiniteChainComplexShape,
  )
import Moonlight.Homology.Boundary.LinAlg
  ( BoundaryIncidence,
    BoundaryIncidenceShapeError,
    composeBoundaryIncidence,
    emptyBoundaryIncidenceOf,
    sourceCardinality,
    targetCardinality,
  )
import Moonlight.Homology.Pure.Chain
  ( HomologicalDegree (..),
    RepresentativeChain (..),
  )
import Moonlight.Homology.Pure.Failure (HomologyFailure)
import Moonlight.Homology.Pure.Matrix.SparseLinAlg
  ( SparseCoordinateBasis,
    SparseColumnEchelon (..),
    SparseRow,
    compactSparseRow,
    sparseBoundaryColumns,
    sparseCoordinateBasis,
    sparseCoordinatesInBasis,
    sparseColumnEchelon,
    sparseEchelonBasis,
    sparseExtendEchelonBasis,
    sparseLinearCombination,
  )
import Moonlight.Homology.Pure.Topology.SparseAlgebra
  ( sparseHomologyBasisAt,
  )
import Numeric.Natural (Natural)

type ChainMapEndpoint :: Type
data ChainMapEndpoint
  = ChainMapSource
  | ChainMapTarget
  deriving stock (Eq, Ord, Show)

type ZigzagDirection :: Type
data ZigzagDirection
  = ZigzagForward
  | ZigzagBackward
  deriving stock (Eq, Ord, Show)

-- | Every refusal names the exact descent obligation that failed.  In
-- particular, absence of a persistence class is a successful empty result,
-- never one of these failures.
type ZigzagFailure :: Type
data ZigzagFailure
  = ZigzagComplexInvalid !ChainMapEndpoint !HomologyFailure
  | ZigzagMapSourceCardinalityMismatch !HomologicalDegree !Int !Int
  | ZigzagMapTargetCardinalityMismatch !HomologicalDegree !Int !Int
  | ZigzagMapComponentInvalid !HomologicalDegree !BoundaryIncidenceShapeError
  | ZigzagMapCompositionInvalid !HomologicalDegree !BoundaryIncidenceShapeError
  | ZigzagChainMapLawViolation !HomologicalDegree
  | ZigzagEndpointMismatch !Int !ZigzagDirection
  | ZigzagHomologyCoordinatesMissing !Int !HomologicalDegree
  | ZigzagNegativeIntervalMultiplicity !HomologicalDegree !Int !Int !Int
  deriving stock (Eq, Show)

-- | A checked degree-preserving chain map.  Its source, target, and materialized
-- degree components are retained together so no caller can later pair the map
-- with different complexes.
type FiniteChainMap :: Type -> Type
data FiniteChainMap r = FiniteChainMap
  { storedChainMapSource :: !(FiniteChainComplex r),
    storedChainMapTarget :: !(FiniteChainComplex r),
    storedChainMapComponents :: !(Vector (BoundaryIncidence r))
  }

finiteChainMapSource :: FiniteChainMap r -> FiniteChainComplex r
finiteChainMapSource = storedChainMapSource

finiteChainMapTarget :: FiniteChainMap r -> FiniteChainComplex r
finiteChainMapTarget = storedChainMapTarget

finiteChainMapAt :: FiniteChainMap r -> HomologicalDegree -> BoundaryIncidence r
finiteChainMapAt chainMap (HomologicalDegree degreeIndex) =
  if degreeIndex < 0
    then emptyBoundaryIncidenceOf 0 0
    else
      case storedChainMapComponents chainMap Vector.!? degreeIndex of
        Just component -> component
        Nothing -> emptyBoundaryIncidenceOf 0 0

-- | Admit a finite chain map after materializing its relevant degrees and
-- proving @d_target . f = f . d_source@ at every positive degree.
mkFiniteChainMapChecked ::
  (Eq r, Num r, Semiring r) =>
  FiniteChainComplex r ->
  FiniteChainComplex r ->
  (HomologicalDegree -> BoundaryIncidence r) ->
  Either ZigzagFailure (FiniteChainMap r)
mkFiniteChainMapChecked sourceComplex targetComplex componentAt = do
  first (ZigzagComplexInvalid ChainMapSource) (validateFiniteChainComplexShape sourceComplex)
  first (ZigzagComplexInvalid ChainMapTarget) (validateFiniteChainComplexShape targetComplex)
  let maximumDegree = max (maximumDegreeOf sourceComplex) (maximumDegreeOf targetComplex)
      degreeValues = fmap HomologicalDegree [0 .. maximumDegree]
      componentList = fmap componentAt degreeValues
      components = Vector.fromList componentList
  traverse_
    (uncurry (validateComponentShape sourceComplex targetComplex))
    (zip degreeValues componentList)
  traverse_
    (\(degreeValue, precedingComponent, degreeComponent) ->
        validateChainMapLaw sourceComplex targetComplex degreeValue precedingComponent degreeComponent
    )
    (zip3 (drop 1 degreeValues) componentList (drop 1 componentList))
  pure
    FiniteChainMap
      { storedChainMapSource = sourceComplex,
        storedChainMapTarget = targetComplex,
        storedChainMapComponents = components
      }

validateComponentShape ::
  FiniteChainComplex r ->
  FiniteChainComplex r ->
  HomologicalDegree ->
  BoundaryIncidence r ->
  Either ZigzagFailure ()
validateComponentShape sourceComplex targetComplex degreeValue component
  | sourceCardinality component /= expectedSource =
      Left (ZigzagMapSourceCardinalityMismatch degreeValue expectedSource (sourceCardinality component))
  | targetCardinality component /= expectedTarget =
      Left (ZigzagMapTargetCardinalityMismatch degreeValue expectedTarget (targetCardinality component))
  | otherwise = Right ()
 where
  expectedSource = degreeCardinality sourceComplex degreeValue
  expectedTarget = degreeCardinality targetComplex degreeValue

validateChainMapLaw ::
  (Eq r, Num r, Semiring r) =>
  FiniteChainComplex r ->
  FiniteChainComplex r ->
  HomologicalDegree ->
  BoundaryIncidence r ->
  BoundaryIncidence r ->
  Either ZigzagFailure ()
validateChainMapLaw sourceComplex targetComplex degreeValue precedingComponent degreeComponent = do
  targetAfterMap <-
    first (ZigzagMapCompositionInvalid degreeValue)
      ( composeBoundaryIncidence
          (finiteBoundaryAt targetComplex degreeValue)
          degreeComponent
      )
  mapAfterSource <-
    first (ZigzagMapCompositionInvalid degreeValue)
      ( composeBoundaryIncidence
          precedingComponent
          (finiteBoundaryAt sourceComplex degreeValue)
      )
  if targetAfterMap == mapAfterSource
    then Right ()
    else Left (ZigzagChainMapLawViolation degreeValue)

-- | Orientation separated from its payload.  The same functor carries checked
-- chain maps during authoring and exact linear maps during reduction.
type ZigzagArrow :: Type -> Type
data ZigzagArrow map
  = ForwardArrow !map
  | BackwardArrow !map
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

zigzagArrowDirection :: ZigzagArrow r -> ZigzagDirection
zigzagArrowDirection = \case
  ForwardArrow _ -> ZigzagForward
  BackwardArrow _ -> ZigzagBackward

type FiniteChainZigzag :: Type -> Type
data FiniteChainZigzag r = FiniteChainZigzag
  { storedZigzagFirstComplex :: !(FiniteChainComplex r),
    storedZigzagArrows :: !(Vector (ZigzagArrow (FiniteChainMap r)))
  }

-- | Glue a line of already checked maps.  A forward arrow is interpreted as
-- @current -> next@; a backward arrow is @current <- next@.
mkFiniteChainZigzag ::
  Eq r =>
  FiniteChainComplex r ->
  [ZigzagArrow (FiniteChainMap r)] ->
  Either ZigzagFailure (FiniteChainZigzag r)
mkFiniteChainZigzag firstComplex arrows = do
  _ <- foldM glueZigzagArrow firstComplex (zip [0 :: Int ..] arrows)
  pure
    FiniteChainZigzag
      { storedZigzagFirstComplex = firstComplex,
        storedZigzagArrows = Vector.fromList arrows
      }

glueZigzagArrow ::
  Eq r =>
  FiniteChainComplex r ->
  (Int, ZigzagArrow (FiniteChainMap r)) ->
  Either ZigzagFailure (FiniteChainComplex r)
glueZigzagArrow currentComplex (arrowIndex, arrow) =
  let (requiredCurrent, nextComplex) = finiteArrowEndpoints arrow
   in if finiteComplexesAgree currentComplex requiredCurrent
        then Right nextComplex
        else Left (ZigzagEndpointMismatch arrowIndex (zigzagArrowDirection arrow))

finiteArrowEndpoints :: ZigzagArrow (FiniteChainMap r) -> (FiniteChainComplex r, FiniteChainComplex r)
finiteArrowEndpoints = \case
  ForwardArrow chainMap -> (finiteChainMapSource chainMap, finiteChainMapTarget chainMap)
  BackwardArrow chainMap -> (finiteChainMapTarget chainMap, finiteChainMapSource chainMap)

finiteChainZigzagComplexes :: FiniteChainZigzag r -> NonEmpty (FiniteChainComplex r)
finiteChainZigzagComplexes zigzag =
  storedZigzagFirstComplex zigzag
    :| Vector.toList
      (fmap (snd . finiteArrowEndpoints) (storedZigzagArrows zigzag))

finiteChainZigzagArrows :: FiniteChainZigzag r -> [ZigzagArrow (FiniteChainMap r)]
finiteChainZigzagArrows = Vector.toList . storedZigzagArrows

zigzagComplexVector :: FiniteChainZigzag r -> Vector (FiniteChainComplex r)
zigzagComplexVector zigzag =
  Vector.cons
    (storedZigzagFirstComplex zigzag)
    (fmap (snd . finiteArrowEndpoints) (storedZigzagArrows zigzag))

-- | One indecomposable interval of a zigzag barcode.  Both endpoints are
-- inclusive.  'Traversable' transports the same interval from numeric diagram
-- indices to a caller's stage vocabulary without defining a parallel carrier.
type ZigzagInterval :: Type -> Type
data ZigzagInterval endpoint = ZigzagInterval
  { zigzagIntervalDegree :: !HomologicalDegree,
    zigzagIntervalFirst :: !endpoint,
    zigzagIntervalLast :: !endpoint,
    zigzagIntervalMultiplicity :: !Int
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | Compute the unique interval decomposition of the rational homology
-- zigzag.  Chain homology and every induced adjacent map are prepared once per
-- degree; one right-filtration descent then closes every interval.
rationalZigzagIntervals ::
  Integral r =>
  FiniteChainZigzag r ->
  Either ZigzagFailure [ZigzagInterval Int]
rationalZigzagIntervals zigzag =
  let complexes = zigzagComplexVector zigzag
      arrows = storedZigzagArrows zigzag
      maximumDegree =
        complexes
          & Vector.foldl'
            (\currentMaximum complexValue -> max currentMaximum (maximumDegreeOf complexValue))
            0
   in concat
        <$> traverse
          ( intervalsAtDegree
              (storedZigzagFirstComplex zigzag)
              arrows
              . HomologicalDegree
          )
          [0 .. maximumDegree]

-- | Betti numbers reconstructed from a barcode at one diagram index.  The map
-- is sparse; absent degrees have dimension zero.
zigzagBettiAt :: Int -> [ZigzagInterval Int] -> Map HomologicalDegree Int
zigzagBettiAt diagramIndex =
  Map.fromListWith (+)
    . fmap
      (\interval -> (zigzagIntervalDegree interval, zigzagIntervalMultiplicity interval))
    . filter
      ( \interval ->
          zigzagIntervalFirst interval <= diagramIndex
            && diagramIndex <= zigzagIntervalLast interval
      )

type HomologyPresentation :: Type
data HomologyPresentation = HomologyPresentation
  { homologyBasisVectors :: !(Vector SparseRow),
    homologyCoordinateBasis :: !SparseCoordinateBasis
  }

homologyDimension :: HomologyPresentation -> Int
homologyDimension = Vector.length . homologyBasisVectors

type RationalLinearMap :: Type
data RationalLinearMap = RationalLinearMap
  { rationalMapTargetDimension :: !Int,
    rationalMapColumns :: !(Vector SparseRow)
  }

-- | One quotient layer of the right filtration on the current endpoint.  The
-- vectors form a basis for that layer modulo every preceding layer; the layer
-- order, not numeric birth order, is the zigzag orientation witness.
type RightFiltrationLayer :: Type
data RightFiltrationLayer = RightFiltrationLayer
  { rightLayerBirthIndex :: !Int,
    rightLayerBasis :: ![SparseRow]
  }

type IntervalMultiplicities :: Type
type IntervalMultiplicities = Map (Int, Int) Int

intervalsAtDegree ::
  Integral r =>
  FiniteChainComplex r ->
  Vector (ZigzagArrow (FiniteChainMap r)) ->
  HomologicalDegree ->
  Either ZigzagFailure [ZigzagInterval Int]
intervalsAtDegree firstComplex arrows degreeValue = do
  let initialPresentation = homologyPresentationAt firstComplex degreeValue
      initialFiltration =
        [ RightFiltrationLayer
            { rightLayerBirthIndex = 0,
              rightLayerBasis = standardSparseBasis (homologyDimension initialPresentation)
            }
        ]
  -- Prepare, induce, and descend one arrow at a time: only the adjacent
  -- presentations and current filtration remain live.
  (_, terminalFiltration, closedIntervals) <-
    Vector.ifoldM'
      ( \(leftPresentation, currentFiltration, intervals) arrowIndex arrow -> do
          let rightPresentation =
                homologyPresentationAt
                  (snd (finiteArrowEndpoints arrow))
                  degreeValue
          inducedArrow <-
            inducedArrowAt
              degreeValue
              arrowIndex
              (leftPresentation, rightPresentation, arrow)
          (nextFiltration, nextIntervals) <-
            advanceRightFiltration
              degreeValue
              (currentFiltration, intervals)
              (arrowIndex, inducedArrow)
          pure (rightPresentation, nextFiltration, nextIntervals)
      )
      (initialPresentation, initialFiltration, Map.empty)
      arrows
  terminalIntervals <-
    closeTerminalIntervals
      degreeValue
      (Vector.length arrows)
      terminalFiltration
      closedIntervals
  pure (intervalsFromMultiplicities degreeValue terminalIntervals)

homologyPresentationAt ::
  Integral r =>
  FiniteChainComplex r ->
  HomologicalDegree ->
  HomologyPresentation
homologyPresentationAt finite degreeValue@(HomologicalDegree degreeIndex)
  | degreeIndex < 0 || degreeIndex > maximumDegreeOf finite =
      HomologyPresentation
        { homologyBasisVectors = Vector.empty,
          homologyCoordinateBasis = sparseCoordinateBasis []
        }
  | otherwise =
      let basisRows =
            sparseHomologyBasisAt finite degreeValue
              & fmap representativeSparseRow
          basisVectors = Vector.fromList basisRows
          boundaryGenerators =
            finiteBoundaryAt finite (HomologicalDegree (degreeIndex + 1))
              & sparseBoundaryColumns
       in HomologyPresentation
            { homologyBasisVectors = basisVectors,
              homologyCoordinateBasis =
                sparseCoordinateBasis
                  (basisRows <> Vector.toList boundaryGenerators)
            }

inducedArrowAt ::
  Integral r =>
  HomologicalDegree ->
  Int ->
  (HomologyPresentation, HomologyPresentation, ZigzagArrow (FiniteChainMap r)) ->
  Either ZigzagFailure (ZigzagArrow RationalLinearMap)
inducedArrowAt degreeValue arrowIndex (leftPresentation, rightPresentation, arrow) =
  case arrow of
    ForwardArrow chainMap ->
      ForwardArrow
        <$> inducedHomologyMap arrowIndex degreeValue chainMap leftPresentation rightPresentation
    BackwardArrow chainMap ->
      BackwardArrow
        <$> inducedHomologyMap arrowIndex degreeValue chainMap rightPresentation leftPresentation

inducedHomologyMap ::
  Integral r =>
  Int ->
  HomologicalDegree ->
  FiniteChainMap r ->
  HomologyPresentation ->
  HomologyPresentation ->
  Either ZigzagFailure RationalLinearMap
inducedHomologyMap arrowIndex degreeValue chainMap sourcePresentation targetPresentation = do
  let rationalColumns =
        sparseBoundaryColumns (finiteChainMapAt chainMap degreeValue)
  imageCoordinates <-
    traverse
      ( \sourceCycle -> do
          let mappedCycle = sparseLinearCombination rationalColumns sourceCycle
          coordinates <-
            maybe
              (Left (ZigzagHomologyCoordinatesMissing arrowIndex degreeValue))
              Right
              ( sparseCoordinatesInBasis
                  (homologyCoordinateBasis targetPresentation)
                  mappedCycle
              )
          pure
            ( IntMap.filterWithKey
                (\coordinateIndex _ -> coordinateIndex < homologyDimension targetPresentation)
                coordinates
            )
      )
      (homologyBasisVectors sourcePresentation)
  pure
    RationalLinearMap
      { rationalMapTargetDimension = homologyDimension targetPresentation,
        rationalMapColumns = imageCoordinates
      }

applyRationalLinearMap :: RationalLinearMap -> SparseRow -> SparseRow
applyRationalLinearMap = sparseLinearCombination . rationalMapColumns

advanceRightFiltration ::
  HomologicalDegree ->
  ([RightFiltrationLayer], IntervalMultiplicities) ->
  (Int, ZigzagArrow RationalLinearMap) ->
  Either ZigzagFailure ([RightFiltrationLayer], IntervalMultiplicities)
advanceRightFiltration degreeValue (currentFiltration, intervals) (arrowIndex, arrow) = do
  let nextBirthIndex = arrowIndex + 1
  (nextFiltration, survivingLayers) <-
    case arrow of
      ForwardArrow linearMap ->
        Right
          ( forwardRightFiltration
              nextBirthIndex
              linearMap
              currentFiltration
          )
      BackwardArrow linearMap ->
        backwardRightFiltration
          arrowIndex
          degreeValue
          nextBirthIndex
          linearMap
          currentFiltration
  updatedIntervals <-
    closeExpiredIntervals
      degreeValue
      arrowIndex
      currentFiltration
      survivingLayers
      intervals
  pure (nextFiltration, updatedIntervals)

forwardRightFiltration ::
  Int ->
  RationalLinearMap ->
  [RightFiltrationLayer] ->
  ([RightFiltrationLayer], [RightFiltrationLayer])
forwardRightFiltration nextBirthIndex linearMap currentFiltration =
  let targetDimension = rationalMapTargetDimension linearMap
      (reversedSurvivingLayers, imageBasis) =
        List.foldl'
          ( \(reversedLayers, accumulatedBasis) layer ->
              let mappedVectors = fmap (applyRationalLinearMap linearMap) (rightLayerBasis layer)
                  (independentImages, extendedBasis) =
                    sparseExtendEchelonBasis accumulatedBasis mappedVectors
               in ( layer {rightLayerBasis = independentImages} : reversedLayers,
                    extendedBasis
                  )
          )
          ([], sparseEchelonBasis [])
          currentFiltration
      survivingLayers = reverse reversedSurvivingLayers
      (newLayerBasis, _) =
        sparseExtendEchelonBasis imageBasis (standardSparseBasis targetDimension)
      nextFiltration =
        survivingLayers
          <> [ RightFiltrationLayer
                 { rightLayerBirthIndex = nextBirthIndex,
                   rightLayerBasis = newLayerBasis
                 }
             ]
   in (nextFiltration, survivingLayers)

backwardRightFiltration ::
  Int ->
  HomologicalDegree ->
  Int ->
  RationalLinearMap ->
  [RightFiltrationLayer] ->
  Either ZigzagFailure ([RightFiltrationLayer], [RightFiltrationLayer])
backwardRightFiltration arrowIndex degreeValue nextBirthIndex linearMap currentFiltration = do
  let targetDimension = rationalMapTargetDimension linearMap
      targetFiltrationBasis = currentFiltration >>= rightLayerBasis
      targetCoordinates = sparseCoordinateBasis targetFiltrationBasis
  imageCoordinateColumns <-
    traverse
      ( \imageColumn ->
          maybe
            (Left (ZigzagHomologyCoordinatesMissing arrowIndex degreeValue))
            Right
            (sparseCoordinatesInBasis targetCoordinates imageColumn)
      )
      (rationalMapColumns linearMap)
  let columnEchelon =
        sparseColumnEchelon
          (fmap (reverseSparseCoordinates targetDimension) imageCoordinateColumns)
      ascendingPivotPreimages =
        sparseColumnPivotPreimages columnEchelon
          & fmap
            (\(reversedPivot, preimage) -> (targetDimension - reversedPivot - 1, preimage))
          & reverse
      (_, survivingLayers) =
        List.mapAccumL
          pullbackLayer
          (0, ascendingPivotPreimages)
          currentFiltration
      kernelLayer =
        RightFiltrationLayer
          { rightLayerBirthIndex = nextBirthIndex,
            rightLayerBasis = sparseColumnKernelBasis columnEchelon
          }
  pure (kernelLayer : survivingLayers, survivingLayers)
 where
  pullbackLayer (lowerBound, remainingPivots) layer =
    let upperBound = lowerBound + length (rightLayerBasis layer)
        (layerPivots, laterPivots) =
          span ((< upperBound) . fst) remainingPivots
     in ( (upperBound, laterPivots),
          layer {rightLayerBasis = fmap snd layerPivots}
        )

reverseSparseCoordinates :: Int -> SparseRow -> SparseRow
reverseSparseCoordinates dimensionValue =
  IntMap.fromDistinctAscList
    . fmap (\(coordinateIndex, coefficient) -> (dimensionValue - coordinateIndex - 1, coefficient))
    . IntMap.toDescList

closeExpiredIntervals ::
  HomologicalDegree ->
  Int ->
  [RightFiltrationLayer] ->
  [RightFiltrationLayer] ->
  IntervalMultiplicities ->
  Either ZigzagFailure IntervalMultiplicities
closeExpiredIntervals degreeValue deathIndex currentLayers survivingLayers intervals =
  foldM
    ( \currentIntervals (currentLayer, survivingLayer) ->
        recordIntervalMultiplicity
          degreeValue
          (rightLayerBirthIndex currentLayer)
          deathIndex
          (length (rightLayerBasis currentLayer) - length (rightLayerBasis survivingLayer))
          currentIntervals
    )
    intervals
    (zip currentLayers survivingLayers)

closeTerminalIntervals ::
  HomologicalDegree ->
  Int ->
  [RightFiltrationLayer] ->
  IntervalMultiplicities ->
  Either ZigzagFailure IntervalMultiplicities
closeTerminalIntervals degreeValue deathIndex terminalFiltration intervals =
  foldM
    ( \currentIntervals layer ->
        recordIntervalMultiplicity
          degreeValue
          (rightLayerBirthIndex layer)
          deathIndex
          (length (rightLayerBasis layer))
          currentIntervals
    )
    intervals
    terminalFiltration

recordIntervalMultiplicity ::
  HomologicalDegree ->
  Int ->
  Int ->
  Int ->
  IntervalMultiplicities ->
  Either ZigzagFailure IntervalMultiplicities
recordIntervalMultiplicity degreeValue firstIndex lastIndex multiplicity intervals
  | multiplicity < 0 =
      Left (ZigzagNegativeIntervalMultiplicity degreeValue firstIndex lastIndex multiplicity)
  | multiplicity == 0 = Right intervals
  | otherwise = Right (Map.insertWith (+) (firstIndex, lastIndex) multiplicity intervals)

intervalsFromMultiplicities :: HomologicalDegree -> IntervalMultiplicities -> [ZigzagInterval Int]
intervalsFromMultiplicities degreeValue =
  fmap
    ( \((firstIndex, lastIndex), multiplicity) ->
        ZigzagInterval
          { zigzagIntervalDegree = degreeValue,
            zigzagIntervalFirst = firstIndex,
            zigzagIntervalLast = lastIndex,
            zigzagIntervalMultiplicity = multiplicity
          }
    )
    . Map.toAscList

standardSparseBasis :: Int -> [SparseRow]
standardSparseBasis dimensionValue =
  fmap (\coordinateIndex -> IntMap.singleton coordinateIndex 1) [0 .. dimensionValue - 1]

representativeSparseRow :: RepresentativeChain Rational Int -> SparseRow
representativeSparseRow representative =
  representativeTerms representative
    & fmap (\(coefficient, basisIndex) -> (basisIndex, coefficient))
    & IntMap.fromListWith (+)
    & compactSparseRow

finiteComplexesAgree :: Eq r => FiniteChainComplex r -> FiniteChainComplex r -> Bool
finiteComplexesAgree left right =
  let maximumDegree = max (maximumDegreeOf left) (maximumDegreeOf right)
   in all
        ( \degreeIndex ->
            finiteBoundaryAt left (HomologicalDegree degreeIndex)
              == finiteBoundaryAt right (HomologicalDegree degreeIndex)
        )
        [0 .. maximumDegree]

finiteBoundaryAt :: FiniteChainComplex r -> HomologicalDegree -> BoundaryIncidence r
finiteBoundaryAt finite degreeValue@(HomologicalDegree degreeIndex) =
  if degreeIndex >= 0 && degreeIndex <= maximumDegreeOf finite
    then incidenceMatrixAt finite degreeValue
    else
      emptyBoundaryIncidenceOf
        (naturalCardinality (degreeCardinality finite degreeValue))
        (naturalCardinality (degreeCardinality finite (HomologicalDegree (degreeIndex - 1))))

maximumDegreeOf :: FiniteChainComplex r -> Int
maximumDegreeOf = unHomologicalDegree . maxHomologicalDegree

naturalCardinality :: Int -> Natural
naturalCardinality = fromIntegral . max 0
