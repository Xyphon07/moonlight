{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Homology.Pure.Topology.BlockSchur
  ( BasisBlock (..),
    BlockSchurPivot (..),
    BlockPivotOps (..),
    integerUnimodularBlockPivotOps,
    rationalBlockPivotOps,
    gf2BlockPivotOps,
    BlockSchurTranscript (..),
    BlockSchurReduction (..),
    BlockSchurFailure (..),
    blockSchurReduceWith,
  )
where

import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.Kind (Type)
import Data.List (transpose)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Moonlight.Core (Semiring)
import Moonlight.Homology.Boundary.Finite
  ( FiniteChainComplex,
    degreeCardinality,
    incidenceMatrixAt,
    maxHomologicalDegree,
    mkFiniteChainComplex,
  )
import Moonlight.Homology.Boundary.LinAlg
  ( BoundaryEntry,
    BoundaryIncidence,
    BoundaryIncidenceShapeError (..),
    boundaryCoefficient,
    boundaryEntries,
    boundaryEntriesBySource,
    composeBoundaryIncidence,
    emptyBoundaryIncidenceOf,
    mkBoundaryEntry,
    mkBoundaryIncidenceFromOrderedEntries,
    sourceCardinality,
    sourceIndex,
    targetCardinality,
    targetIndex,
  )
import Moonlight.Homology.Pure.Chain (HomologicalDegree (..))
import Moonlight.Homology.Pure.Failure (HomologyLaw (..))
import Moonlight.Homology.Pure.LinearCombination qualified as LC
import Moonlight.Homology.Pure.Reductions (ChainHomotopy (..), ChainMap (..))
import Moonlight.Homology.Pure.Carrier (BasisCellRef (..))
import Moonlight.Homology.Pure.Topology.Core (allBasisCellRefs, dimensionsOf)
import Moonlight.LinAlg.Pure.Dense.Block
  ( BlockMatrixFailure (..),
    invertGF2Block,
    invertRationalBlock,
    invertUnimodularIntegerBlock,
  )
import Moonlight.LinAlg.Pure.Dense.GF2 (GF2 (..))

-- | A finite ordered block of basis coordinates inside one chain degree.
type BasisBlock :: Type
data BasisBlock = BasisBlock
  { basisBlockDegree :: !HomologicalDegree,
    basisBlockIndices :: ![Int]
  }
  deriving stock (Eq, Ord, Show)

-- | A cancellable block in d_n: upper degree n sources contract with lower
-- degree n-1 targets through an invertible square submatrix.
type BlockSchurPivot :: Type
data BlockSchurPivot = BlockSchurPivot
  { bspUpperBlock :: !BasisBlock,
    bspLowerBlock :: !BasisBlock
  }
  deriving stock (Eq, Ord, Show)

type BlockPivotOps :: Type -> Type
data BlockPivotOps coefficient = BlockPivotOps
  { bpoZero :: !coefficient,
    bpoOne :: !coefficient,
    bpoIsZero :: coefficient -> Bool,
    bpoAdd :: coefficient -> coefficient -> coefficient,
    bpoNegate :: coefficient -> coefficient,
    bpoMultiply :: coefficient -> coefficient -> coefficient,
    bpoInvertBlock :: [[coefficient]] -> Either BlockMatrixFailure [[coefficient]]
  }

integerUnimodularBlockPivotOps :: BlockPivotOps Integer
integerUnimodularBlockPivotOps =
  BlockPivotOps
    { bpoZero = 0,
      bpoOne = 1,
      bpoIsZero = (== 0),
      bpoAdd = (+),
      bpoNegate = negate,
      bpoMultiply = (*),
      bpoInvertBlock = invertUnimodularIntegerBlock
    }

rationalBlockPivotOps :: BlockPivotOps Rational
rationalBlockPivotOps =
  BlockPivotOps
    { bpoZero = 0,
      bpoOne = 1,
      bpoIsZero = (== 0),
      bpoAdd = (+),
      bpoNegate = negate,
      bpoMultiply = (*),
      bpoInvertBlock = invertRationalBlock
    }

gf2BlockPivotOps :: BlockPivotOps GF2
gf2BlockPivotOps =
  BlockPivotOps
    { bpoZero = GF2Zero,
      bpoOne = GF2One,
      bpoIsZero = (== GF2Zero),
      bpoAdd = (+),
      bpoNegate = id,
      bpoMultiply = (*),
      bpoInvertBlock = invertGF2Block
    }

type BlockSchurTranscript :: Type -> Type
data BlockSchurTranscript coefficient = BlockSchurTranscript
  { bstPivot :: !BlockSchurPivot,
    bstPivotMatrix :: ![[coefficient]],
    bstPivotInverse :: ![[coefficient]],
    bstResidualBoundary :: !(BoundaryIncidence coefficient),
    bstRemainingSourceIndices :: ![Int],
    bstRemainingTargetIndices :: ![Int]
  }
  deriving stock (Eq, Show)

type BlockSchurReduction :: Type -> Type
data BlockSchurReduction coefficient = BlockSchurReduction
  { bsrOriginalComplex :: !(FiniteChainComplex coefficient),
    bsrReducedComplex :: !(FiniteChainComplex coefficient),
    bsrTranscript :: !(BlockSchurTranscript coefficient),
    bsrProjection :: !(ChainMap BasisCellRef BasisCellRef coefficient),
    bsrInclusion :: !(ChainMap BasisCellRef BasisCellRef coefficient),
    bsrHomotopy :: !(ChainHomotopy BasisCellRef coefficient)
  }

type BlockSchurFailure :: Type -> Type
data BlockSchurFailure coefficient
  = BlockSchurUpperBlockEmpty !BasisBlock
  | BlockSchurLowerBlockEmpty !BasisBlock
  | BlockSchurPivotDegreeMismatch !HomologicalDegree !HomologicalDegree
  | BlockSchurPivotSizeMismatch !Int !Int
  | BlockSchurPivotIndexOutOfBounds !HomologicalDegree !Int !Int
  | BlockSchurPivotDuplicateIndex !BasisBlock
  | BlockSchurPivotMatrixFailed !BlockMatrixFailure
  | BlockSchurBoundaryShapeFailed !BoundaryIncidenceShapeError
  | BlockSchurReducedNilpotenceFailed !HomologicalDegree !HomologicalDegree !Int !Int !coefficient
  | BlockSchurReductionLawFailed !HomologyLaw
  deriving stock (Eq, Show)

blockSchurReduceWith ::
  (Eq coefficient, Num coefficient, Semiring coefficient) =>
  BlockPivotOps coefficient ->
  FiniteChainComplex coefficient ->
  BlockSchurPivot ->
  Either (BlockSchurFailure coefficient) (BlockSchurReduction coefficient)
blockSchurReduceWith ops complex pivot = do
  validatePivotBlocks complex pivot
  let upperBlock = bspUpperBlock pivot
      lowerBlock = bspLowerBlock pivot
      upperDegree = basisBlockDegree upperBlock
      boundary = incidenceMatrixAt complex upperDegree
      boundaryCoefficients = boundaryCoefficientMap boundary
      upperIndices = basisBlockIndices upperBlock
      lowerIndices = basisBlockIndices lowerBlock
      remainingSources = remainingIndices upperIndices (sourceCardinality boundary)
      remainingTargets = remainingIndices lowerIndices (targetCardinality boundary)
      pivotMatrix = submatrixOf ops boundaryCoefficients upperIndices lowerIndices
      aMatrix = submatrixOf ops boundaryCoefficients remainingSources lowerIndices
      bMatrix = submatrixOf ops boundaryCoefficients upperIndices remainingTargets
      cMatrix = submatrixOf ops boundaryCoefficients remainingSources remainingTargets
  pivotInverse <- first BlockSchurPivotMatrixFailed (bpoInvertBlock ops pivotMatrix)
  residualMatrix <- residualSchurMatrix ops cMatrix bMatrix pivotInverse aMatrix
  residualBoundary <-
    first BlockSchurBoundaryShapeFailed $
      matrixBoundaryIncidence ops (length remainingSources) (length remainingTargets) residualMatrix
  let reindexing = blockSchurReindexing complex pivot
      reducedComplex =
        mkFiniteChainComplex
          (maxHomologicalDegree complex)
          (reducedBoundaryFor ops complex pivot reindexing residualBoundary)
      transcript =
        BlockSchurTranscript
          { bstPivot = pivot,
            bstPivotMatrix = pivotMatrix,
            bstPivotInverse = pivotInverse,
            bstResidualBoundary = residualBoundary,
            bstRemainingSourceIndices = remainingSources,
            bstRemainingTargetIndices = remainingTargets
          }
      projectionMap = projectionFor ops pivot reindexing bMatrix pivotInverse
      inclusionMap = inclusionFor ops pivot reindexing pivotInverse aMatrix
      homotopyMap = homotopyFor ops pivot pivotInverse
  validateReducedNilpotence ops reducedComplex
  validateReductionLaws ops complex reducedComplex projectionMap inclusionMap homotopyMap
  pure
    BlockSchurReduction
      { bsrOriginalComplex = complex,
        bsrReducedComplex = reducedComplex,
        bsrTranscript = transcript,
        bsrProjection = projectionMap,
        bsrInclusion = inclusionMap,
        bsrHomotopy = homotopyMap
      }

validatePivotBlocks ::
  FiniteChainComplex coefficient ->
  BlockSchurPivot ->
  Either (BlockSchurFailure coefficient) ()
validatePivotBlocks complex pivot = do
  validateBlockNotEmpty BlockSchurUpperBlockEmpty upperBlock
  validateBlockNotEmpty BlockSchurLowerBlockEmpty lowerBlock
  validateDuplicateFree upperBlock
  validateDuplicateFree lowerBlock
  validateDegree
  validateSize
  traverse_ (validateIndex upperDegree upperDimension) upperIndices
  traverse_ (validateIndex lowerDegree lowerDimension) lowerIndices
  where
    upperBlock = bspUpperBlock pivot
    lowerBlock = bspLowerBlock pivot
    upperDegree@(HomologicalDegree upperDegreeInt) = basisBlockDegree upperBlock
    lowerDegree@(HomologicalDegree lowerDegreeInt) = basisBlockDegree lowerBlock
    upperIndices = basisBlockIndices upperBlock
    lowerIndices = basisBlockIndices lowerBlock
    upperDimension = degreeCardinality complex upperDegree
    lowerDimension = degreeCardinality complex lowerDegree

    validateDegree =
      if upperDegreeInt == lowerDegreeInt + 1
        then Right ()
        else Left (BlockSchurPivotDegreeMismatch upperDegree lowerDegree)

    validateSize =
      if length upperIndices == length lowerIndices
        then Right ()
        else Left (BlockSchurPivotSizeMismatch (length upperIndices) (length lowerIndices))

    validateIndex :: HomologicalDegree -> Int -> Int -> Either (BlockSchurFailure coefficient) ()
    validateIndex degreeValue dimension indexValue =
      if indexValue >= 0 && indexValue < dimension
        then Right ()
        else Left (BlockSchurPivotIndexOutOfBounds degreeValue indexValue dimension)

validateBlockNotEmpty ::
  (BasisBlock -> BlockSchurFailure coefficient) ->
  BasisBlock ->
  Either (BlockSchurFailure coefficient) ()
validateBlockNotEmpty failure block =
  case basisBlockIndices block of
    [] -> Left (failure block)
    _ : _ -> Right ()

validateDuplicateFree ::
  BasisBlock ->
  Either (BlockSchurFailure coefficient) ()
validateDuplicateFree block =
  if Set.size (Set.fromList (basisBlockIndices block)) == length (basisBlockIndices block)
    then Right ()
    else Left (BlockSchurPivotDuplicateIndex block)

remainingIndices :: [Int] -> Int -> [Int]
remainingIndices removed dimension =
  let removedSet = Set.fromList removed
   in filter (`Set.notMember` removedSet) [0 .. dimension - 1]
{-# INLINEABLE remainingIndices #-}

type BlockSchurReindexing :: Type
data BlockSchurReindexing = BlockSchurReindexing
  { bsrOldToNewByDegree :: !(Map HomologicalDegree (Map Int Int)),
    bsrNewToOldByDegree :: !(Map HomologicalDegree (Map Int Int))
  }
  deriving stock (Eq, Show)

blockSchurReindexing :: FiniteChainComplex coefficient -> BlockSchurPivot -> BlockSchurReindexing
blockSchurReindexing complex pivot =
  BlockSchurReindexing
    { bsrOldToNewByDegree = oldToNew,
      bsrNewToOldByDegree = newToOld
    }
  where
    HomologicalDegree maxDegreeInt = maxHomologicalDegree complex
    degrees = fmap HomologicalDegree [0 .. maxDegreeInt]
    upperBlock = bspUpperBlock pivot
    lowerBlock = bspLowerBlock pivot
    removedAt degreeValue
      | degreeValue == basisBlockDegree upperBlock = basisBlockIndices upperBlock
      | degreeValue == basisBlockDegree lowerBlock = basisBlockIndices lowerBlock
      | otherwise = []
    remainingAt degreeValue = remainingIndices (removedAt degreeValue) (degreeCardinality complex degreeValue)
    oldToNew = Map.fromList [(degreeValue, Map.fromList (zip (remainingAt degreeValue) [0 ..])) | degreeValue <- degrees]
    newToOld = Map.fromList [(degreeValue, Map.fromList (zip [0 ..] (remainingAt degreeValue))) | degreeValue <- degrees]

reducedCardinalityAt :: BlockSchurReindexing -> HomologicalDegree -> Int
reducedCardinalityAt reindexing degreeValue =
  maybe 0 Map.size (Map.lookup degreeValue (bsrOldToNewByDegree reindexing))
{-# INLINEABLE reducedCardinalityAt #-}

oldToNewIndex :: BlockSchurReindexing -> HomologicalDegree -> Int -> Maybe Int
oldToNewIndex reindexing degreeValue indexValue = do
  degreeMap <- Map.lookup degreeValue (bsrOldToNewByDegree reindexing)
  Map.lookup indexValue degreeMap
{-# INLINEABLE oldToNewIndex #-}

newToOldIndex :: BlockSchurReindexing -> HomologicalDegree -> Int -> Maybe Int
newToOldIndex reindexing degreeValue indexValue = do
  degreeMap <- Map.lookup degreeValue (bsrNewToOldByDegree reindexing)
  Map.lookup indexValue degreeMap
{-# INLINEABLE newToOldIndex #-}

reducedBoundaryFor ::
  (Eq coefficient, Semiring coefficient) =>
  BlockPivotOps coefficient ->
  FiniteChainComplex coefficient ->
  BlockSchurPivot ->
  BlockSchurReindexing ->
  BoundaryIncidence coefficient ->
  HomologicalDegree ->
  BoundaryIncidence coefficient
reducedBoundaryFor ops complex pivot reindexing residualBoundary degreeValue@(HomologicalDegree degreeInt)
  | degreeInt <= 0 =
      emptyBoundaryIncidenceOf
        (fromIntegral (reducedCardinalityAt reindexing (HomologicalDegree 0)))
        0
  | degreeValue == basisBlockDegree (bspUpperBlock pivot) =
      residualBoundary
  | otherwise =
      either
        (const (emptyBoundaryIncidenceOf sourceCount targetCount))
        id
        ( mkBoundaryIncidenceFromOrderedEntries
            sourceCount
            targetCount
            (restrictedBoundaryEntries (incidenceMatrixAt complex degreeValue))
        )
  where
    targetDegree = HomologicalDegree (degreeInt - 1)
    sourceCount = fromIntegral (reducedCardinalityAt reindexing degreeValue)
    targetCount = fromIntegral (reducedCardinalityAt reindexing targetDegree)

    restrictedBoundaryEntries incidence =
      mapMaybe
        ( \entry -> do
            newSource <- oldToNewIndex reindexing degreeValue (sourceIndex entry)
            newTarget <- oldToNewIndex reindexing targetDegree (targetIndex entry)
            if bpoIsZero ops (boundaryCoefficient entry)
              then Nothing
              else
                Just
                  ( mkBoundaryEntry
                      (fromIntegral newSource)
                      (fromIntegral newTarget)
                      (boundaryCoefficient entry)
                  )
        )
        (boundaryEntries incidence)

submatrixOf :: BlockPivotOps coefficient -> Map (Int, Int) coefficient -> [Int] -> [Int] -> [[coefficient]]
submatrixOf ops coefficients sourceIndices targetIndices =
  [ [ Map.findWithDefault (bpoZero ops) (sourceValue, targetValue) coefficients
      | sourceValue <- sourceIndices
    ]
    | targetValue <- targetIndices
  ]
{-# INLINEABLE submatrixOf #-}

boundaryCoefficientMap :: BoundaryIncidence coefficient -> Map (Int, Int) coefficient
boundaryCoefficientMap =
  Map.fromListWith const
    . fmap (\entry -> ((sourceIndex entry, targetIndex entry), boundaryCoefficient entry))
    . boundaryEntries
{-# INLINEABLE boundaryCoefficientMap #-}

residualSchurMatrix ::
  BlockPivotOps coefficient ->
  [[coefficient]] ->
  [[coefficient]] ->
  [[coefficient]] ->
  [[coefficient]] ->
  Either (BlockSchurFailure coefficient) [[coefficient]]
residualSchurMatrix ops cMatrix bMatrix pivotInverse aMatrix = do
  pivotTimesA <- matrixProduct ops pivotInverse aMatrix
  correction <- matrixProduct ops bMatrix pivotTimesA
  pure (matrixSubtract ops cMatrix correction)
{-# INLINEABLE residualSchurMatrix #-}

matrixProduct ::
  BlockPivotOps coefficient ->
  [[coefficient]] ->
  [[coefficient]] ->
  Either (BlockSchurFailure coefficient) [[coefficient]]
matrixProduct ops left right =
  let leftWidths = fmap length left
      rightRowCount = length right
   in if all (== rightRowCount) leftWidths
        then Right (matrixProductUnchecked ops left right)
        else Left (BlockSchurPivotMatrixFailed (BlockMatrixNotSquare rightRowCount leftWidths))
{-# INLINEABLE matrixProduct #-}

matrixProductUnchecked :: BlockPivotOps coefficient -> [[coefficient]] -> [[coefficient]] -> [[coefficient]]
matrixProductUnchecked ops left right =
  let rightColumns = transpose right
   in fmap
        (\leftRow -> fmap (dotWith ops leftRow) rightColumns)
        left
{-# INLINEABLE matrixProductUnchecked #-}

matrixSubtract :: BlockPivotOps coefficient -> [[coefficient]] -> [[coefficient]] -> [[coefficient]]
matrixSubtract ops left right =
  zipWith
    (zipWith (\leftEntry rightEntry -> bpoAdd ops leftEntry (bpoNegate ops rightEntry)))
    left
    right
{-# INLINEABLE matrixSubtract #-}

dotWith :: BlockPivotOps coefficient -> [coefficient] -> [coefficient] -> coefficient
dotWith ops left right =
  foldl'
    (bpoAdd ops)
    (bpoZero ops)
    (zipWith (bpoMultiply ops) left right)
{-# INLINEABLE dotWith #-}

matrixBoundaryIncidence ::
  (Eq coefficient, Semiring coefficient) =>
  BlockPivotOps coefficient ->
  Int ->
  Int ->
  [[coefficient]] ->
  Either BoundaryIncidenceShapeError (BoundaryIncidence coefficient)
matrixBoundaryIncidence ops sourceCount targetCount matrix
  | length matrix /= targetCount || any (/= sourceCount) (fmap length matrix) =
      Left
        ( BoundaryIncidenceBlockShapeMismatch
            sourceCount
            targetCount
            (maximumMaybe 0 (fmap length matrix))
            (length matrix)
        )
  | otherwise =
      mkBoundaryIncidenceFromOrderedEntries
        (fromIntegral sourceCount)
        (fromIntegral targetCount)
        entries
  where
    entries =
      [ mkBoundaryEntry (fromIntegral sourceLocal) (fromIntegral targetLocal) coefficientValue
        | (targetLocal, rowValues) <- zip [0 :: Int ..] matrix,
          (sourceLocal, coefficientValue) <- zip [0 :: Int ..] rowValues,
          not (bpoIsZero ops coefficientValue)
      ]
{-# INLINEABLE matrixBoundaryIncidence #-}

maximumMaybe :: Int -> [Int] -> Int
maximumMaybe fallback =
  foldr max fallback
{-# INLINE maximumMaybe #-}

projectionFor ::
  BlockPivotOps coefficient ->
  BlockSchurPivot ->
  BlockSchurReindexing ->
  [[coefficient]] ->
  [[coefficient]] ->
  ChainMap BasisCellRef BasisCellRef coefficient
projectionFor ops pivot reindexing bMatrix pivotInverse =
  ChainMap projectBasisRef
  where
    lowerBlock = bspLowerBlock pivot
    upperBlock = bspUpperBlock pivot
    upperDegree = basisBlockDegree upperBlock
    lowerDegree = basisBlockDegree lowerBlock
    lowerIndices = basisBlockIndices lowerBlock
    bPinv = matrixProductUnchecked ops bMatrix pivotInverse
    lowerProjectionColumns =
      Map.fromList
        [ ( lowerIndex,
            [ (bpoNegate ops coefficientValue, basisRefAt lowerDegree rowLocal)
              | (rowLocal, rowValues) <- zip [0 ..] bPinv,
                (colLocal, coefficientValue) <- zip [0 :: Int ..] rowValues,
                maybe False (== lowerIndex) (entryAt colLocal lowerIndices),
                not (bpoIsZero ops coefficientValue)
            ]
          )
          | lowerIndex <- lowerIndices
        ]

    projectBasisRef basisRef
      | cellDegree basisRef == lowerDegree =
          case oldToNewIndex reindexing lowerDegree (cellIndex basisRef) of
            Just newIndex -> [(bpoOne ops, basisRefAt lowerDegree newIndex)]
            Nothing -> Map.findWithDefault [] (cellIndex basisRef) lowerProjectionColumns
      | cellDegree basisRef == upperDegree =
          identityProjection basisRef
      | otherwise =
          identityProjection basisRef

    identityProjection basisRef =
      maybe [] (\newIndex -> [(bpoOne ops, basisRefAt (cellDegree basisRef) newIndex)]) (oldToNewIndex reindexing (cellDegree basisRef) (cellIndex basisRef))

inclusionFor ::
  BlockPivotOps coefficient ->
  BlockSchurPivot ->
  BlockSchurReindexing ->
  [[coefficient]] ->
  [[coefficient]] ->
  ChainMap BasisCellRef BasisCellRef coefficient
inclusionFor ops pivot reindexing pivotInverse aMatrix =
  ChainMap includeBasisRef
  where
    upperBlock = bspUpperBlock pivot
    upperDegree = basisBlockDegree upperBlock
    upperIndices = basisBlockIndices upperBlock
    pInvA = matrixProductUnchecked ops pivotInverse aMatrix

    includeBasisRef basisRef
      | cellDegree basisRef == upperDegree =
          case newToOldIndex reindexing upperDegree (cellIndex basisRef) of
            Nothing -> []
            Just oldSourceIndex ->
              (bpoOne ops, basisRefAt upperDegree oldSourceIndex)
                : [ (bpoNegate ops coefficientValue, basisRefAt upperDegree upperIndex)
                    | (upperLocal, rowValues) <- zip [0 ..] pInvA,
                      (sourceLocal, coefficientValue) <- zip [0 :: Int ..] rowValues,
                      sourceLocal == cellIndex basisRef,
                      Just upperIndex <- [entryAt upperLocal upperIndices],
                      not (bpoIsZero ops coefficientValue)
                  ]
      | otherwise =
          maybe [] (\oldIndex -> [(bpoOne ops, basisRefAt (cellDegree basisRef) oldIndex)]) (newToOldIndex reindexing (cellDegree basisRef) (cellIndex basisRef))

homotopyFor ::
  BlockPivotOps coefficient ->
  BlockSchurPivot ->
  [[coefficient]] ->
  ChainHomotopy BasisCellRef coefficient
homotopyFor ops pivot pivotInverse =
  ChainHomotopy homotopyBasisRef
  where
    upperBlock = bspUpperBlock pivot
    lowerBlock = bspLowerBlock pivot
    upperDegree = basisBlockDegree upperBlock
    lowerDegree = basisBlockDegree lowerBlock
    upperIndices = basisBlockIndices upperBlock
    lowerIndices = basisBlockIndices lowerBlock

    homotopyBasisRef basisRef
      | cellDegree basisRef == lowerDegree =
          case indexOf (cellIndex basisRef) lowerIndices of
            Nothing -> []
            Just lowerLocal ->
              [ (coefficientValue, basisRefAt upperDegree upperIndex)
                | (upperLocal, rowValues) <- zip [0 ..] pivotInverse,
                  (lowerLocalCandidate, coefficientValue) <- zip [0 :: Int ..] rowValues,
                  lowerLocalCandidate == lowerLocal,
                  Just upperIndex <- [entryAt upperLocal upperIndices],
                  not (bpoIsZero ops coefficientValue)
              ]
      | otherwise = []

basisRefAt :: HomologicalDegree -> Int -> BasisCellRef
basisRefAt degreeValue indexValue =
  BasisCellRef
    { cellDegree = degreeValue,
      cellIndex = indexValue
    }
{-# INLINE basisRefAt #-}

entryAt :: Int -> [entry] -> Maybe entry
entryAt indexValue entries
  | indexValue < 0 = Nothing
  | otherwise =
      case drop indexValue entries of
        entryValue : _ -> Just entryValue
        [] -> Nothing
{-# INLINE entryAt #-}

indexOf :: Eq value => value -> [value] -> Maybe Int
indexOf target =
  fmap fst . findFirst ((== target) . snd) . zip [0 ..]
{-# INLINEABLE indexOf #-}

findFirst :: (a -> Bool) -> [a] -> Maybe a
findFirst predicate =
  foldr (\value rest -> if predicate value then Just value else rest) Nothing
{-# INLINE findFirst #-}

validateReducedNilpotence ::
  (Eq coefficient, Num coefficient, Semiring coefficient) =>
  BlockPivotOps coefficient ->
  FiniteChainComplex coefficient ->
  Either (BlockSchurFailure coefficient) ()
validateReducedNilpotence ops complex =
  traverse_ validateDegree [1 .. maxDegreeInt]
  where
    HomologicalDegree maxDegreeInt = maxHomologicalDegree complex

    validateDegree degreeInt = do
      let rightDegree = HomologicalDegree degreeInt
          leftDegree = HomologicalDegree (degreeInt - 1)
      composite <-
        first BlockSchurBoundaryShapeFailed $
          composeBoundaryIncidence
            (incidenceMatrixAt complex leftDegree)
            (incidenceMatrixAt complex rightDegree)
      case firstNonZeroBoundaryEntry ops composite of
        Nothing -> Right ()
        Just entry ->
          Left
            ( BlockSchurReducedNilpotenceFailed
                rightDegree
                leftDegree
                (sourceIndex entry)
                (targetIndex entry)
                (boundaryCoefficient entry)
            )

firstNonZeroBoundaryEntry :: BlockPivotOps coefficient -> BoundaryIncidence coefficient -> Maybe (BoundaryEntry coefficient)
firstNonZeroBoundaryEntry ops =
  findFirst (not . bpoIsZero ops . boundaryCoefficient) . boundaryEntries
{-# INLINEABLE firstNonZeroBoundaryEntry #-}

validateReductionLaws ::
  Eq coefficient =>
  BlockPivotOps coefficient ->
  FiniteChainComplex coefficient ->
  FiniteChainComplex coefficient ->
  ChainMap BasisCellRef BasisCellRef coefficient ->
  ChainMap BasisCellRef BasisCellRef coefficient ->
  ChainHomotopy BasisCellRef coefficient ->
  Either (BlockSchurFailure coefficient) ()
validateReductionLaws ops largeComplex smallComplex projectionMap inclusionMap homotopyMap = do
  checkLaw ReductionProjectionChainMapLaw (allBasisCellRefs largeComplex) projectionLeft projectionRight
  checkLaw ReductionInclusionChainMapLaw (allBasisCellRefs smallComplex) inclusionLeft inclusionRight
  checkLaw ReductionHomotopyLaw (allBasisCellRefs largeComplex) homotopyLeft homotopyRight
  where
    arithmetic = blockArithmetic ops
    largeBoundary = boundaryOf (boundaryEntriesByDegree largeComplex)
    smallBoundary = boundaryOf (boundaryEntriesByDegree smallComplex)

    projectionLeft largeCell =
      LC.composeWith arithmetic smallBoundary (runChainMap projectionMap largeCell)
    projectionRight largeCell =
      LC.composeWith arithmetic (runChainMap projectionMap) (largeBoundary largeCell)

    inclusionLeft smallCell =
      LC.composeWith arithmetic largeBoundary (runChainMap inclusionMap smallCell)
    inclusionRight smallCell =
      LC.composeWith arithmetic (runChainMap inclusionMap) (smallBoundary smallCell)

    homotopyLeft largeCell =
      LC.subtractWith
        arithmetic
        (LC.identityWith arithmetic largeCell)
        (LC.composeWith arithmetic (runChainMap inclusionMap) (runChainMap projectionMap largeCell))
    homotopyRight largeCell =
      LC.addWith
        arithmetic
        (LC.composeWith arithmetic largeBoundary (runChainHomotopy homotopyMap largeCell))
        (LC.composeWith arithmetic (runChainHomotopy homotopyMap) (largeBoundary largeCell))

    checkLaw law basisValues leftSide rightSide =
      if all (lawHolds leftSide rightSide) basisValues
        then Right ()
        else Left (BlockSchurReductionLawFailed law)

    lawHolds leftSide rightSide basisValue =
      LC.normalizeWith arithmetic (leftSide basisValue) == LC.normalizeWith arithmetic (rightSide basisValue)

blockArithmetic :: BlockPivotOps coefficient -> LC.LinearCombinationArithmetic coefficient
blockArithmetic ops =
  LC.LinearCombinationArithmetic
    { LC.lcaZero = bpoZero ops,
      LC.lcaOne = bpoOne ops,
      LC.lcaAdd = bpoAdd ops,
      LC.lcaNegate = bpoNegate ops,
      LC.lcaMultiply = bpoMultiply ops
    }

boundaryOf ::
  Map HomologicalDegree (Vector.Vector [BoundaryEntry coefficient]) ->
  BasisCellRef ->
  [(coefficient, BasisCellRef)]
boundaryOf groupedBoundaryEntries basisRef =
  case cellDegree basisRef of
    HomologicalDegree degreeInt
      | degreeInt <= 0 -> []
      | otherwise ->
          let targetDegree = HomologicalDegree (degreeInt - 1)
              degreeEntries = Map.findWithDefault Vector.empty (HomologicalDegree degreeInt) groupedBoundaryEntries
           in [ (boundaryCoefficient entry, basisRefAt targetDegree (targetIndex entry))
                | entry <- maybe [] id (degreeEntries Vector.!? cellIndex basisRef)
              ]

boundaryEntriesByDegree ::
  FiniteChainComplex coefficient ->
  Map HomologicalDegree (Vector.Vector [BoundaryEntry coefficient])
boundaryEntriesByDegree complex =
  Map.fromList
    ( fmap
        (\degreeValue -> (degreeValue, boundaryEntriesBySource (incidenceMatrixAt complex degreeValue)))
        (dimensionsOf complex)
    )
