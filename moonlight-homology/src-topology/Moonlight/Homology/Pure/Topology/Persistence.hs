module Moonlight.Homology.Pure.Topology.Persistence
  ( mkFilteredFiniteChainComplex,
    mod2PersistentPairs,
    CriticalBettiTable,
    criticalBettiDegreeCount,
    criticalBettiRankCount,
    criticalBettiTableValues,
    criticalBettiVectors,
    mod2PersistentPairsWithCriticalBettiTable,
    persistentBettiAt,
    persistentBettiAtMany,
    persistentBettiAtCriticalValues,
    mod2PersistenceTopologyWitness,
    mod2PersistentBoundaryColumn,
    persistentPairs,
    persistenceTopologyWitness,
    persistentBoundaryColumn,
    persistenceEssentialBirths,
    materializeFinitePersistencePair,
    materializeEssentialPersistencePair,
    orderedFilteredCells,
    validateBirthCoverage,
    validateBirthExactness,
    validateFiltrationMonotonicity,
  )
where

import Control.Monad (foldM)
import Control.Monad.ST (ST, runST)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Vector qualified as Vector
import Data.Vector.Mutable qualified as BoxedMutableVector
import Data.Vector.Unboxed qualified as UnboxedVector
import Data.Vector.Unboxed.Mutable qualified as MutableVector
import Moonlight.Homology.Boundary.Finite
  ( FiniteChainComplex,
    incidenceMatrixAt,
  )
import Moonlight.Homology.Boundary.LinAlg
  ( BoundaryEntry,
    BoundaryIncidence,
    boundaryCoefficient,
    boundaryEntriesBySource,
    sourceIndex,
    targetIndex,
  )
import Moonlight.Homology.Pure.Chain
  ( HomologicalDegree (..),
    PersistencePair (..),
    TopologyWitness (..),
    decrementDegree,
    emptyTopologyWitness,
  )
import Moonlight.Homology.Pure.Failure (HomologyFailure (..))
import Moonlight.Homology.Pure.Topology.Core

type OrderedFilteredCell :: Type -> Type
data OrderedFilteredCell filtration = OrderedFilteredCell
  { orderedCellIdentity :: BasisCellRef,
    orderedCellBirth :: filtration,
    orderedCellBirthRank :: Int
  }
  deriving stock (Eq, Show)

-- | Dense Betti profiles over the finite complex's critical-rank and degree
-- cover. Values are stored rank-major in one unboxed vector; the constructor
-- is withheld so the two dimensions cannot disagree with its storage.
data CriticalBettiTable = CriticalBettiTable
  { criticalBettiDegreeCount :: !Int,
    criticalBettiRankCount :: !Int,
    criticalBettiTableValues :: !(UnboxedVector.Vector Int)
  }
  deriving stock (Eq, Show)

-- | Materialize the familiar rank-indexed Betti vectors from the dense table.
criticalBettiVectors :: CriticalBettiTable -> [[Int]]
criticalBettiVectors table =
  fmap profileAt [0 .. criticalBettiRankCount table - 1]
 where
  degreeCount = criticalBettiDegreeCount table
  values = criticalBettiTableValues table
  profileAt rankValue =
    values
      & UnboxedVector.drop (rankValue * degreeCount)
      & UnboxedVector.take degreeCount
      & UnboxedVector.toList

type PersistenceState :: Type
data PersistenceState = PersistenceState
  { persistencePairsByIndex :: ![(Int, Int)],
    persistencePairedBirths :: !IntSet.IntSet,
    persistenceCreators :: !IntSet.IntSet
  }

type BettiSweepPoint :: Type
data BettiSweepPoint = BettiSweepPoint
  { bettiSweepDelta :: !(Map.Map HomologicalDegree Int),
    bettiSweepRequested :: !Bool
  }

emptyPersistenceState :: PersistenceState
emptyPersistenceState =
  PersistenceState
    { persistencePairsByIndex = [],
      persistencePairedBirths = IntSet.empty,
      persistenceCreators = IntSet.empty
    }

mkFilteredFiniteChainComplex ::
  (Integral r, Ord filtration) =>
  FiniteChainComplex r ->
  [(BasisCellRef, filtration)] ->
  Either HomologyFailure (FilteredFiniteChainComplex filtration r)
mkFilteredFiniteChainComplex finite births = do
  let birthMap = Map.fromList births
  validateBirthUniqueness births birthMap
  denseBirthsByDegree <- denseBirthSection finite birthMap
  let expectedCellCount = Vector.sum (fmap Vector.length denseBirthsByDegree)
  if Map.size birthMap == expectedCellCount
    then Right ()
    else validateBirthExactness finite birthMap
  let birthSections =
        Map.foldlWithKey'
          (\sections cellRefValue birthValue -> Map.insertWith (<>) birthValue [cellRefValue] sections)
          Map.empty
          birthMap
          & Map.toAscList
      rankedBirthSections = zip [0 :: Int ..] birthSections
      criticalValues = fmap (fst . snd) rankedBirthSections
      birthValueRanks =
        rankedBirthSections
          & fmap (\(rankValue, (birthValue, _)) -> (birthValue, rankValue))
          & Map.fromDistinctAscList
      cellBirthRanks =
        rankedBirthSections
          >>= ( \(rankValue, (_, cellsAtBirth)) ->
                  fmap (\cellRefValue -> (cellRefValue, rankValue)) cellsAtBirth
              )
          & Map.fromList
      rankedCells =
        rankedBirthSections
          >>= ( \(rankValue, (birthValue, cellsAtBirth)) ->
                  reverse cellsAtBirth
                    & fmap (\cellRefValue -> (cellRefValue, birthValue, rankValue))
              )
  validateDenseFiltrationMonotonicity finite denseBirthsByDegree
  pure
    FilteredFiniteChainComplex
      { filteredBaseComplex = finite,
        filteredCellBirths = birthMap,
        filteredCriticalValues = criticalValues,
        filteredBirthValueRanks = birthValueRanks,
        filteredCellBirthRanks = cellBirthRanks,
        filteredRankedCells = rankedCells
      }

mod2PersistentPairs ::
  Integral r =>
  FilteredFiniteChainComplex filtration r ->
  Either HomologyFailure [PersistencePair filtration]
mod2PersistentPairs filtered =
  let (orderedCellByIndex, stateAfterReduction) = reduceMod2Persistence filtered
   in Right (materializePersistencePairs orderedCellByIndex stateAfterReduction)

-- | Reduce once, then derive both the exact barcode and all critical-rank
-- Betti profiles from the same indexed witness. The fused observation avoids
-- throwing away proven cell ranks only to recover them through exact-value
-- map lookups in 'persistentBettiAtCriticalValues'.
mod2PersistentPairsWithCriticalBettiTable ::
  Integral r =>
  FilteredFiniteChainComplex filtration r ->
  Either
    HomologyFailure
    ( [PersistencePair filtration]
    , CriticalBettiTable
    )
mod2PersistentPairsWithCriticalBettiTable filtered =
  let (orderedCellByIndex, stateAfterReduction) = reduceMod2Persistence filtered
      pairs = materializePersistencePairs orderedCellByIndex stateAfterReduction
      criticalBettiTable =
        criticalBettiTableFromRankedEvents
          (length (dimensionsOf (filteredBaseComplex filtered)))
          (length (filteredCriticalValues filtered))
          (persistenceStateRankedEvents orderedCellByIndex stateAfterReduction)
   in Right (pairs, criticalBettiTable)

reduceMod2Persistence ::
  Integral r =>
  FilteredFiniteChainComplex filtration r ->
  (Vector.Vector (OrderedFilteredCell filtration), PersistenceState)
reduceMod2Persistence filtered =
  let orderedCells = orderedFilteredCells filtered
      orderedCellByIndex =
        orderedCells
          & Vector.fromList
      globalIndicesByDegree =
        denseGlobalIndicesByDegree
          (filteredBaseComplex filtered)
          (zip [0 :: Int ..] orderedCells)
      groupedBoundaryEntries = boundaryEntriesByDegree (filteredBaseComplex filtered)
      degreeCount = length (dimensionsOf (filteredBaseComplex filtered))
      indexedCellsByDegree =
        zip [0 :: Int ..] orderedCells
          & mapMaybe (indexedCellDegreeUpdate degreeCount)
          & Vector.accum
            (flip (:))
            (Vector.replicate degreeCount [])
          & fmap reverse
      higherDegreeColumns =
        indexedCellsByDegree
          & Vector.drop 2
          & fmap
            ( fmap
                ( \(columnIndexValue, orderedCell) ->
                    ( columnIndexValue
                    , mod2PersistentBoundaryColumnFromIndex
                        groupedBoundaryEntries
                        globalIndicesByDegree
                        orderedCell
                    )
                )
            )
          & Vector.toList
          & reverse
      degreeOneColumns =
        maybe [] id (indexedCellsByDegree Vector.!? 1)
          & fmap
            ( \(columnIndexValue, orderedCell) ->
                ( columnIndexValue
                , mod2PersistentBoundaryTargetsFromIndex
                    groupedBoundaryEntries
                    globalIndicesByDegree
                    orderedCell
                )
            )
      degreeZeroIndices =
        maybe [] id (indexedCellsByDegree Vector.!? 0)
          & fmap fst
      stateAfterReduction =
        reducePersistentColumnsByDegree
          (length orderedCells)
          higherDegreeColumns
          degreeOneColumns
          degreeZeroIndices
   in (orderedCellByIndex, stateAfterReduction)

indexedCellDegreeUpdate ::
  Int ->
  (Int, OrderedFilteredCell filtration) ->
  Maybe (Int, (Int, OrderedFilteredCell filtration))
indexedCellDegreeUpdate degreeCount indexedCell@(_, orderedCell) =
  let degreeIndex =
        unHomologicalDegree (cellDegree (orderedCellIdentity orderedCell))
   in if degreeIndex < 0 || degreeIndex >= degreeCount
        then Nothing
        else Just (degreeIndex, indexedCell)

materializePersistencePairs ::
  Vector.Vector (OrderedFilteredCell filtration) ->
  PersistenceState ->
  [PersistencePair filtration]
materializePersistencePairs orderedCellByIndex stateAfterReduction =
  let finitePairs =
        persistencePairsByIndex stateAfterReduction
          & List.sortOn snd
          & mapMaybe (uncurry (materializeFinitePersistencePair orderedCellByIndex))
      essentialPairs =
        persistenceEssentialBirths stateAfterReduction
          & IntSet.toAscList
          & mapMaybe (materializeEssentialPersistencePair orderedCellByIndex)
   in finitePairs <> essentialPairs

persistenceStateRankedEvents ::
  Vector.Vector (OrderedFilteredCell filtration) ->
  PersistenceState ->
  [(Int, (Int, Int))]
persistenceStateRankedEvents orderedCellByIndex stateAfterReduction =
  let finiteEvents =
        persistencePairsByIndex stateAfterReduction
          >>= ( \(birthIndexValue, deathIndexValue) ->
                  case
                      ( orderedCellByIndex Vector.!? birthIndexValue
                      , orderedCellByIndex Vector.!? deathIndexValue
                      )
                    of
                      (Just birthCell, Just deathCell) ->
                        rankedIntervalEvents
                          (cellDegree (orderedCellIdentity birthCell))
                          (orderedCellBirthRank birthCell)
                          (Just (orderedCellBirthRank deathCell))
                      _ -> []
              )
      essentialEvents =
        persistenceEssentialBirths stateAfterReduction
          & IntSet.toAscList
          >>= ( \birthIndexValue ->
                  maybe
                    []
                    ( \birthCell ->
                        rankedIntervalEvents
                          (cellDegree (orderedCellIdentity birthCell))
                          (orderedCellBirthRank birthCell)
                          Nothing
                    )
                    (orderedCellByIndex Vector.!? birthIndexValue)
              )
   in finiteEvents <> essentialEvents

-- | Read the Betti numbers of one closed filtration sublevel from an already
-- reduced barcode. The returned map is sparse: absent degrees have Betti
-- number zero. Finite persistence intervals are half-open, so a class whose
-- death equals the requested threshold is no longer present after every cell
-- at that threshold has entered the complex.
persistentBettiAt ::
  Ord filtration =>
  filtration ->
  [PersistencePair filtration] ->
  Map.Map HomologicalDegree Int
persistentBettiAt threshold =
  Map.fromListWith (+)
    . fmap (\pairValue -> (persistenceDegree pairValue, 1))
    . filter (persistencePairAliveAt threshold)

-- | Read several closed filtration sublevels in one ordered sweep over the
-- barcode. Results retain the supplied threshold order and multiplicity.
-- Unlike mapping 'persistentBettiAt', this does not rescan every persistence
-- interval for every requested threshold.
--
-- Finite intervals use the same half-open semantics as 'persistentBettiAt'. A
-- birth and death at the same threshold cancel before that threshold is
-- observed. Malformed finite intervals whose death does not follow their birth
-- contribute nowhere, matching the pointwise query.
persistentBettiAtMany ::
  Ord filtration =>
  [filtration] ->
  [PersistencePair filtration] ->
  [Map.Map HomologicalDegree Int]
persistentBettiAtMany thresholds pairs =
  let requestedPoints =
        thresholds
          & fmap (\threshold -> (threshold, requestedBettiSweepPoint))
          & Map.fromListWith mergeBettiSweepPoints
      eventPoints =
        foldl'
          insertPersistencePairSweepPoints
          Map.empty
          pairs
      sweepPoints = Map.unionWith mergeBettiSweepPoints eventPoints requestedPoints
      observedByThreshold =
        sweepPoints
          & Map.mapAccumWithKey applyBettiSweepPoint Map.empty
          & snd
          & Map.mapMaybe id
   in fmap
        (\threshold -> Map.findWithDefault Map.empty threshold observedByThreshold)
        thresholds

-- | Read every exact critical sublevel of one admitted filtered complex. The
-- complex's cached exact-value quotient transports pair endpoints to dense
-- ranks, performs the sweep over 'Int', then returns profiles aligned with
-- 'filteredCriticalValues'. Exact births remain authoritative; ranks never
-- escape this derived observation.
persistentBettiAtCriticalValues ::
  Ord filtration =>
  FilteredFiniteChainComplex filtration r ->
  [PersistencePair filtration] ->
  Either HomologyFailure [Map.Map HomologicalDegree Int]
persistentBettiAtCriticalValues filtered pairs = do
  rankedPairs <- traverse (rankPersistencePair filtered) pairs
  pure
    ( criticalProfilesFromRankedEvents
        (length (filteredCriticalValues filtered))
        (rankedPairs >>= rankedPersistencePairEvents)
    )

criticalProfilesFromRankedEvents ::
  Int ->
  [(Int, (Int, Int))] ->
  [Map.Map HomologicalDegree Int]
criticalProfilesFromRankedEvents criticalRankCount rankedEvents =
  let eventDeltas =
        Vector.accum
          insertRankedBettiEvent
          (Vector.replicate criticalRankCount IntMap.empty)
          rankedEvents
      (_, criticalProfiles) =
        List.mapAccumL
          applyRankedBettiEvents
          IntMap.empty
          (Vector.toList eventDeltas)
   in criticalProfiles

criticalBettiTableFromRankedEvents ::
  Int ->
  Int ->
  [(Int, (Int, Int))] ->
  CriticalBettiTable
criticalBettiTableFromRankedEvents degreeCount criticalRankCount rankedEvents =
  let eventDeltas =
        Vector.accum
          insertRankedBettiEvent
          (Vector.replicate criticalRankCount IntMap.empty)
          rankedEvents
      degreeIndices = [0 .. degreeCount - 1]
      values =
        UnboxedVector.create $ do
          mutableValues <-
            MutableVector.new (max 0 (degreeCount * criticalRankCount))
          _ <-
            Vector.ifoldM'
              ( \currentBetti rankValue rankEvents -> do
                  let nextBetti = nextRankedBetti currentBetti rankEvents
                  traverse_
                    ( \degreeValue ->
                        MutableVector.write
                          mutableValues
                          (rankValue * degreeCount + degreeValue)
                          (IntMap.findWithDefault 0 degreeValue nextBetti)
                    )
                    degreeIndices
                  pure nextBetti
              )
              IntMap.empty
              eventDeltas
          pure mutableValues
   in CriticalBettiTable
        { criticalBettiDegreeCount = degreeCount,
          criticalBettiRankCount = criticalRankCount,
          criticalBettiTableValues = values
        }

rankPersistencePair ::
  Ord filtration =>
  FilteredFiniteChainComplex filtration r ->
  PersistencePair filtration ->
  Either HomologyFailure (PersistencePair Int)
rankPersistencePair filtered pairValue = do
  birthRank <-
    maybe
      (Left (InvalidTopologyInput "persistence-pair birth is absent from the filtered complex"))
      Right
      (Map.lookup (persistenceBirth pairValue) (filteredBirthValueRanks filtered))
  deathRank <-
    traverse
      ( \deathValue ->
          maybe
            (Left (InvalidTopologyInput "persistence-pair death is absent from the filtered complex"))
            Right
            (Map.lookup deathValue (filteredBirthValueRanks filtered))
      )
      (persistenceDeath pairValue)
  pure
    PersistencePair
      { persistenceDegree = persistenceDegree pairValue,
        persistenceBirth = birthRank,
        persistenceDeath = deathRank
      }

rankedPersistencePairEvents ::
  PersistencePair Int ->
  [(Int, (Int, Int))]
rankedPersistencePairEvents pairValue =
  rankedIntervalEvents
    (persistenceDegree pairValue)
    (persistenceBirth pairValue)
    (persistenceDeath pairValue)

rankedIntervalEvents ::
  HomologicalDegree ->
  Int ->
  Maybe Int ->
  [(Int, (Int, Int))]
rankedIntervalEvents degreeValue birthRank deathRank =
  case deathRank of
    Nothing -> [rankedBettiEventAt 1 birthRank degreeValue]
    Just finiteDeathRank ->
      if birthRank < finiteDeathRank
        then
          [ rankedBettiEventAt 1 birthRank degreeValue
          , rankedBettiEventAt (-1) finiteDeathRank degreeValue
          ]
        else []

rankedBettiEventAt ::
  Int ->
  Int ->
  HomologicalDegree ->
  (Int, (Int, Int))
rankedBettiEventAt deltaValue criticalRank degreeValue =
  (criticalRank, (unHomologicalDegree degreeValue, deltaValue))

insertRankedBettiEvent ::
  IntMap.IntMap Int ->
  (Int, Int) ->
  IntMap.IntMap Int
insertRankedBettiEvent eventDeltas (degreeValue, deltaValue) =
  IntMap.insertWith
    (+)
    degreeValue
    deltaValue
    eventDeltas

applyRankedBettiEvents ::
  IntMap.IntMap Int ->
  IntMap.IntMap Int ->
  (IntMap.IntMap Int, Map.Map HomologicalDegree Int)
applyRankedBettiEvents currentBetti eventDeltas =
  let nextBetti = nextRankedBetti currentBetti eventDeltas
      publicBetti =
        nextBetti
          & IntMap.foldrWithKey
            ( \degreeValue countValue remainingDegrees ->
                (HomologicalDegree degreeValue, countValue) : remainingDegrees
            )
            []
          & Map.fromDistinctAscList
   in (nextBetti, publicBetti)

nextRankedBetti ::
  IntMap.IntMap Int ->
  IntMap.IntMap Int ->
  IntMap.IntMap Int
nextRankedBetti currentBetti eventDeltas =
  IntMap.unionWith (+) currentBetti eventDeltas
    & IntMap.filter (/= 0)

requestedBettiSweepPoint :: BettiSweepPoint
requestedBettiSweepPoint =
  BettiSweepPoint
    { bettiSweepDelta = Map.empty,
      bettiSweepRequested = True
    }

insertPersistencePairSweepPoints ::
  Ord filtration =>
  Map.Map filtration BettiSweepPoint ->
  PersistencePair filtration ->
  Map.Map filtration BettiSweepPoint
insertPersistencePairSweepPoints eventPoints pairValue =
  case persistenceDeath pairValue of
    Nothing -> insertPersistencePairDelta 1 (persistenceBirth pairValue) pairValue eventPoints
    Just deathValue ->
      if persistenceBirth pairValue < deathValue
        then
          eventPoints
            & insertPersistencePairDelta 1 (persistenceBirth pairValue) pairValue
            & insertPersistencePairDelta (-1) deathValue pairValue
        else eventPoints

insertPersistencePairDelta ::
  Ord filtration =>
  Int ->
  filtration ->
  PersistencePair filtration ->
  Map.Map filtration BettiSweepPoint ->
  Map.Map filtration BettiSweepPoint
insertPersistencePairDelta deltaValue threshold pairValue =
  Map.insertWith
    mergeBettiSweepPoints
    threshold
    (persistencePairDelta deltaValue pairValue)

persistencePairDelta :: Int -> PersistencePair filtration -> BettiSweepPoint
persistencePairDelta deltaValue pairValue =
  BettiSweepPoint
    { bettiSweepDelta = Map.singleton (persistenceDegree pairValue) deltaValue,
      bettiSweepRequested = False
    }

mergeBettiSweepPoints :: BettiSweepPoint -> BettiSweepPoint -> BettiSweepPoint
mergeBettiSweepPoints leftPoint rightPoint =
  BettiSweepPoint
    { bettiSweepDelta = Map.unionWith (+) (bettiSweepDelta leftPoint) (bettiSweepDelta rightPoint),
      bettiSweepRequested = bettiSweepRequested leftPoint || bettiSweepRequested rightPoint
    }

applyBettiSweepPoint ::
  Map.Map HomologicalDegree Int ->
  filtration ->
  BettiSweepPoint ->
  (Map.Map HomologicalDegree Int, Maybe (Map.Map HomologicalDegree Int))
applyBettiSweepPoint currentBetti _ pointValue =
  let nextBetti =
        Map.unionWith (+) currentBetti (bettiSweepDelta pointValue)
          & Map.filter (/= 0)
      observation =
        if bettiSweepRequested pointValue
          then Just nextBetti
          else Nothing
   in (nextBetti, observation)

persistencePairAliveAt ::
  Ord filtration =>
  filtration ->
  PersistencePair filtration ->
  Bool
persistencePairAliveAt threshold pairValue =
  persistenceBirth pairValue <= threshold
    && maybe True (threshold <) (persistenceDeath pairValue)

persistentPairs ::
  Integral r =>
  FilteredFiniteChainComplex filtration r ->
  Either HomologyFailure [PersistencePair filtration]
{-# DEPRECATED persistentPairs "Use mod2PersistentPairs — this computes mod-2 persistence despite its Integral constraint" #-}
persistentPairs = mod2PersistentPairs

mod2PersistenceTopologyWitness ::
  Integral r =>
  FilteredFiniteChainComplex filtration r ->
  Either HomologyFailure (TopologyWitness scaffold spectral filtration coefficient basis)
mod2PersistenceTopologyWitness filtered = do
  pairs <- mod2PersistentPairs filtered
  pure
    emptyTopologyWitness
      { topologyPersistencePairs = pairs
      }

persistenceTopologyWitness ::
  Integral r =>
  FilteredFiniteChainComplex filtration r ->
  Either HomologyFailure (TopologyWitness scaffold spectral filtration coefficient basis)
{-# DEPRECATED persistenceTopologyWitness "Use mod2PersistenceTopologyWitness — this computes mod-2 persistence despite its Integral constraint" #-}
persistenceTopologyWitness = mod2PersistenceTopologyWitness

persistenceEssentialBirths :: PersistenceState -> IntSet.IntSet
persistenceEssentialBirths stateValue =
  persistenceCreators stateValue
    `IntSet.difference` persistencePairedBirths stateValue

-- | Reduce one homological degree after all higher-degree pairings are known.
-- A cell already paired as the low simplex of a higher-dimensional column is
-- a proven creator, so the standard persistence clearing theorem lets us
-- replace its column by zero rather than rediscovering that fact by reduction.
reducePersistentDegree ::
  Int ->
  PersistenceState ->
  [(Int, IntSet.IntSet)] ->
  PersistenceState
reducePersistentDegree totalCellCount stateValue indexedColumns =
  let clearedColumnIndices = persistencePairedBirths stateValue
   in runST $ do
        pivotColumns <- BoxedMutableVector.replicate totalCellCount Nothing
        foldM
          (reduceOrClearPersistentColumn pivotColumns clearedColumnIndices)
          stateValue
          indexedColumns

reduceOrClearPersistentColumn ::
  BoxedMutableVector.MVector s (Maybe IntSet.IntSet) ->
  IntSet.IntSet ->
  PersistenceState ->
  (Int, IntSet.IntSet) ->
  ST s PersistenceState
reduceOrClearPersistentColumn pivotColumns clearedColumnIndices stateValue indexedColumn@(columnIndexValue, _) =
  if IntSet.member columnIndexValue clearedColumnIndices
    then
      pure
        stateValue
          { persistenceCreators = IntSet.insert columnIndexValue (persistenceCreators stateValue)
          }
    else reducePersistentColumn pivotColumns stateValue indexedColumn

-- | Reduce one sparse column against the dense pivot section for its
-- homological degree. Global row identifiers are assigned from @[0 .. n)@
-- before boundary materialization, so the fresh arena is a total local view;
-- it cannot escape this degree's sealed 'ST' descent.
reducePersistentColumn ::
  BoxedMutableVector.MVector s (Maybe IntSet.IntSet) ->
  PersistenceState ->
  (Int, IntSet.IntSet) ->
  ST s PersistenceState
reducePersistentColumn pivotColumns stateValue (columnIndexValue, initialColumn) = do
  reducedColumn <- reduceBoundaryColumn pivotColumns initialColumn
  case IntSet.lookupMax reducedColumn of
    Nothing ->
      pure
        stateValue
          { persistenceCreators = IntSet.insert columnIndexValue (persistenceCreators stateValue)
          }
    Just lowValue -> do
      BoxedMutableVector.write pivotColumns lowValue (Just reducedColumn)
      pure
        stateValue
          { persistencePairsByIndex = (lowValue, columnIndexValue) : persistencePairsByIndex stateValue,
            persistencePairedBirths = IntSet.insert lowValue (persistencePairedBirths stateValue)
          }

-- | Descend through the degree cover. Higher-dimensional columns use the
-- general sparse reducer; the graph boundary uses its equivalent elder-rule
-- union-find section when every mod-two one-cell boundary has cardinality zero
-- or two. Any non-graph fiber is a typed local incompatibility with that
-- specialization and glues back to the general reducer instead.
reducePersistentColumnsByDegree ::
  Int ->
  [[(Int, IntSet.IntSet)]] ->
  [(Int, [Int])] ->
  [Int] ->
  PersistenceState
reducePersistentColumnsByDegree totalCellCount higherDegreeColumns degreeOneColumns degreeZeroIndices =
  let stateAfterHigherDegrees =
        foldl'
          (reducePersistentDegree totalCellCount)
          emptyPersistenceState
          higherDegreeColumns
      clearedDegreeOneIndices =
        persistencePairedBirths stateAfterHigherDegrees
      graphReduction =
        reduceGraphBoundaryDegree
          totalCellCount
          clearedDegreeOneIndices
          degreeZeroIndices
          degreeOneColumns
   in case graphReduction of
        Just (graphPairs, graphCreators) ->
          stateAfterHigherDegrees
            { persistencePairsByIndex = graphPairs <> persistencePairsByIndex stateAfterHigherDegrees,
              persistencePairedBirths =
                IntSet.union
                  (IntSet.fromList (fmap fst graphPairs))
                  (persistencePairedBirths stateAfterHigherDegrees),
              persistenceCreators = IntSet.union graphCreators (persistenceCreators stateAfterHigherDegrees)
            }
        Nothing ->
          let sparseDegreeOneColumns =
                fmap
                  (\(columnIndexValue, targets) -> (columnIndexValue, IntSet.fromList targets))
                  degreeOneColumns
              degreeZeroColumns =
                fmap (\columnIndexValue -> (columnIndexValue, IntSet.empty)) degreeZeroIndices
           in foldl'
                (reducePersistentDegree totalCellCount)
                stateAfterHigherDegrees
                [sparseDegreeOneColumns, degreeZeroColumns]

reduceGraphBoundaryDegree ::
  Int ->
  IntSet.IntSet ->
  [Int] ->
  [(Int, [Int])] ->
  Maybe ([(Int, Int)], IntSet.IntSet)
reduceGraphBoundaryDegree totalCellCount clearedEdgeIndices vertexIndices edgeColumns =
  runST $ do
    parents <- MutableVector.generate totalCellCount id
    foldM
      (reduceGraphBoundaryColumn parents clearedEdgeIndices)
      (Just ([], IntSet.fromList vertexIndices))
      edgeColumns

reduceGraphBoundaryColumn ::
  MutableVector.MVector s Int ->
  IntSet.IntSet ->
  Maybe ([(Int, Int)], IntSet.IntSet) ->
  (Int, [Int]) ->
  ST s (Maybe ([(Int, Int)], IntSet.IntSet))
reduceGraphBoundaryColumn _ _ Nothing _ = pure Nothing
reduceGraphBoundaryColumn parents clearedEdgeIndices (Just (pairsByIndex, creators)) (edgeIndex, boundaryTargets) =
  if IntSet.member edgeIndex clearedEdgeIndices
    then pure (Just (pairsByIndex, IntSet.insert edgeIndex creators))
    else
      case boundaryTargets of
        [] -> pure (Just (pairsByIndex, IntSet.insert edgeIndex creators))
        [firstVertex, secondVertex] -> do
          firstRoot <- findGraphComponentRoot parents firstVertex
          secondRoot <- findGraphComponentRoot parents secondVertex
          case (firstRoot, secondRoot) of
            (Just firstRootIndex, Just secondRootIndex) ->
              if firstRootIndex == secondRootIndex
                then pure (Just (pairsByIndex, IntSet.insert edgeIndex creators))
                else do
                  let olderRoot = min firstRootIndex secondRootIndex
                      youngerRoot = max firstRootIndex secondRootIndex
                  MutableVector.write parents youngerRoot olderRoot
                  pure (Just ((youngerRoot, edgeIndex) : pairsByIndex, creators))
            _ -> pure Nothing
        _ -> pure Nothing

findGraphComponentRoot ::
  MutableVector.MVector s Int ->
  Int ->
  ST s (Maybe Int)
findGraphComponentRoot parents indexValue =
  if indexValue < 0 || indexValue >= MutableVector.length parents
    then pure Nothing
    else do
      parentIndex <- MutableVector.read parents indexValue
      if parentIndex == indexValue
        then pure (Just indexValue)
        else do
          rootIndex <- findGraphComponentRoot parents parentIndex
          traverse
            (\rootValue -> MutableVector.write parents indexValue rootValue >> pure rootValue)
            rootIndex

reduceBoundaryColumn ::
  BoxedMutableVector.MVector s (Maybe IntSet.IntSet) ->
  IntSet.IntSet ->
  ST s IntSet.IntSet
reduceBoundaryColumn pivotColumns columnValue =
  case IntSet.lookupMax columnValue of
    Nothing -> pure columnValue
    Just lowValue -> do
      pivotColumn <- BoxedMutableVector.read pivotColumns lowValue
      case pivotColumn of
        Nothing -> pure columnValue
        Just existingPivot ->
          reduceBoundaryColumn
            pivotColumns
            (IntSet.symmetricDifference columnValue existingPivot)

materializeFinitePersistencePair ::
  Vector.Vector (OrderedFilteredCell filtration) ->
  Int ->
  Int ->
  Maybe (PersistencePair filtration)
materializeFinitePersistencePair orderedCellByIndex birthIndexValue deathIndexValue =
  case (orderedCellByIndex Vector.!? birthIndexValue, orderedCellByIndex Vector.!? deathIndexValue) of
    (Just birthCell, Just deathCell) ->
      Just
        PersistencePair
          { persistenceDegree = cellDegree (orderedCellIdentity birthCell),
            persistenceBirth = orderedCellBirth birthCell,
            persistenceDeath = Just (orderedCellBirth deathCell)
          }
    _ -> Nothing

materializeEssentialPersistencePair ::
  Vector.Vector (OrderedFilteredCell filtration) ->
  Int ->
  Maybe (PersistencePair filtration)
materializeEssentialPersistencePair orderedCellByIndex birthIndexValue =
  orderedCellByIndex Vector.!? birthIndexValue
    & fmap
      ( \birthCell ->
          PersistencePair
            { persistenceDegree = cellDegree (orderedCellIdentity birthCell),
              persistenceBirth = orderedCellBirth birthCell,
              persistenceDeath = Nothing
            }
      )

orderedFilteredCells ::
  FilteredFiniteChainComplex filtration r ->
  [OrderedFilteredCell filtration]
orderedFilteredCells filtered =
  filteredRankedCells filtered
    & fmap
      ( \(cellRefValue, birthValue, birthRank) ->
          OrderedFilteredCell
            { orderedCellIdentity = cellRefValue,
              orderedCellBirth = birthValue,
              orderedCellBirthRank = birthRank
            }
      )

mod2PersistentBoundaryColumn ::
  Integral r =>
  FilteredFiniteChainComplex filtration r ->
  Map.Map BasisCellRef Int ->
  OrderedFilteredCell filtration ->
  IntSet.IntSet
mod2PersistentBoundaryColumn filtered globalIndexByCell orderedCell =
  let cellRefValue = orderedCellIdentity orderedCell
      degreeValue = cellDegree cellRefValue
      incidence = incidenceMatrixAt (filteredBaseComplex filtered) degreeValue
   in mod2PersistentBoundaryColumnFromEntries (boundaryEntriesBySource incidence) globalIndexByCell orderedCell

mod2PersistentBoundaryColumnFromIndex ::
  Integral r =>
  Vector.Vector (Vector.Vector [BoundaryEntry r]) ->
  Vector.Vector (Vector.Vector (Maybe Int)) ->
  OrderedFilteredCell filtration ->
  IntSet.IntSet
mod2PersistentBoundaryColumnFromIndex groupedBoundaryEntries globalIndicesByDegree =
  IntSet.fromList
    . mod2PersistentBoundaryTargetsFromIndex groupedBoundaryEntries globalIndicesByDegree

mod2PersistentBoundaryTargetsFromIndex ::
  Integral r =>
  Vector.Vector (Vector.Vector [BoundaryEntry r]) ->
  Vector.Vector (Vector.Vector (Maybe Int)) ->
  OrderedFilteredCell filtration ->
  [Int]
mod2PersistentBoundaryTargetsFromIndex groupedBoundaryEntries globalIndicesByDegree orderedCell =
  let cellRefValue = orderedCellIdentity orderedCell
      degreeValue = unHomologicalDegree (cellDegree cellRefValue)
      degreeEntries =
        maybe Vector.empty id (groupedBoundaryEntries Vector.!? degreeValue)
      targetIndices =
        maybe
          Vector.empty
          id
          (globalIndicesByDegree Vector.!? (degreeValue - 1))
   in maybe [] id (degreeEntries Vector.!? cellIndex cellRefValue)
        & filter (\entry -> odd (abs (boundaryCoefficient entry)))
        & mapMaybe (\entry -> targetIndices Vector.!? (targetIndex entry) >>= id)

mod2PersistentBoundaryColumnFromEntries ::
  Integral r =>
  Vector.Vector [BoundaryEntry r] ->
  Map.Map BasisCellRef Int ->
  OrderedFilteredCell filtration ->
  IntSet.IntSet
mod2PersistentBoundaryColumnFromEntries groupedEntries globalIndexByCell orderedCell =
  let cellRefValue = orderedCellIdentity orderedCell
      degreeValue = cellDegree cellRefValue
   in maybe [] id (groupedEntries Vector.!? cellIndex cellRefValue)
        & filter (\entry -> odd (abs (boundaryCoefficient entry)))
        & fmap
          ( \entry ->
              BasisCellRef
                { cellDegree = decrementDegree degreeValue,
                  cellIndex = targetIndex entry
                }
          )
        & mapMaybeWithLookup globalIndexByCell
        & IntSet.fromList

boundaryEntriesByDegree ::
  FiniteChainComplex r ->
  Vector.Vector (Vector.Vector [BoundaryEntry r])
boundaryEntriesByDegree finite =
  dimensionsOf finite
    & fmap
      ( \degreeValue ->
          boundaryEntriesBySource (incidenceMatrixAt finite degreeValue)
      )
    & Vector.fromList

denseGlobalIndicesByDegree ::
  FiniteChainComplex r ->
  [(Int, OrderedFilteredCell filtration)] ->
  Vector.Vector (Vector.Vector (Maybe Int))
denseGlobalIndicesByDegree finite indexedCells =
  let degreeCount = length (dimensionsOf finite)
      updatesByDegree =
        Vector.accum
          (flip (:))
          (Vector.replicate degreeCount [])
          (mapMaybe (globalIndexUpdate degreeCount) indexedCells)
   in Vector.imap
        ( \degreeIndex updates ->
            Vector.accum
              (\_ globalIndexValue -> Just globalIndexValue)
              ( Vector.replicate
                  (cellCountAtDegree finite (HomologicalDegree degreeIndex))
                  Nothing
              )
              updates
        )
        updatesByDegree
  where
    globalIndexUpdate ::
      Int ->
      (Int, OrderedFilteredCell filtration) ->
      Maybe (Int, (Int, Int))
    globalIndexUpdate degreeCountValue (globalIndexValue, orderedCell) =
      let cellRefValue = orderedCellIdentity orderedCell
          degreeIndex = unHomologicalDegree (cellDegree cellRefValue)
       in if degreeIndex < 0 || degreeIndex >= degreeCountValue
            then Nothing
            else Just (degreeIndex, (cellIndex cellRefValue, globalIndexValue))

persistentBoundaryColumn ::
  Integral r =>
  FilteredFiniteChainComplex filtration r ->
  Map.Map BasisCellRef Int ->
  OrderedFilteredCell filtration ->
  IntSet.IntSet
{-# DEPRECATED persistentBoundaryColumn "Use mod2PersistentBoundaryColumn — this computes mod-2 boundary despite its Integral constraint" #-}
persistentBoundaryColumn = mod2PersistentBoundaryColumn

validateBirthUniqueness ::
  [(BasisCellRef, filtration)] ->
  Map.Map BasisCellRef filtration ->
  Either HomologyFailure ()
validateBirthUniqueness births birthMap =
  if length births == Map.size birthMap
    then Right ()
    else Left (InvalidTopologyInput "duplicate birth assignments for the same cell")

validateBirthCoverage ::
  FiniteChainComplex r ->
  Map.Map BasisCellRef filtration ->
  Either HomologyFailure ()
validateBirthCoverage finite birthMap =
  () <$ denseBirthSection finite birthMap

validateBirthExactness ::
  FiniteChainComplex r ->
  Map.Map BasisCellRef filtration ->
  Either HomologyFailure ()
validateBirthExactness finite birthMap =
  let basisSet = Set.fromList (allBasisCellRefs finite)
      extraKeys = Map.keysSet birthMap `Set.difference` basisSet
   in if Set.null extraKeys
        then Right ()
        else
          Left
            ( InvalidTopologyInput
                ( "birth map contains cells absent from the chain complex: "
                    <> show (Set.toList extraKeys)
                )
            )

validateFiltrationMonotonicity ::
  (Integral r, Ord filtration) =>
  FiniteChainComplex r ->
  Map.Map BasisCellRef filtration ->
  Either HomologyFailure ()
validateFiltrationMonotonicity finite birthMap = do
  denseBirthsByDegree <- denseBirthSection finite birthMap
  validateDenseFiltrationMonotonicity finite denseBirthsByDegree

denseBirthSection ::
  FiniteChainComplex r ->
  Map.Map BasisCellRef filtration ->
  Either HomologyFailure (Vector.Vector (Vector.Vector filtration))
denseBirthSection finite birthMap =
  dimensionsOf finite
    & traverse
      ( \degreeValue ->
          Vector.generateM
            (cellCountAtDegree finite degreeValue)
            ( \indexValue ->
                let cellRefValue = BasisCellRef degreeValue indexValue
                 in maybe
                      ( Left
                          ( InvalidTopologyInput
                              ("missing filtration value for cell " <> show cellRefValue)
                          )
                      )
                      Right
                      (Map.lookup cellRefValue birthMap)
            )
      )
    & fmap Vector.fromList

validateDenseFiltrationMonotonicity ::
  (Integral r, Ord filtration) =>
  FiniteChainComplex r ->
  Vector.Vector (Vector.Vector filtration) ->
  Either HomologyFailure ()
validateDenseFiltrationMonotonicity finite denseBirthsByDegree =
  dimensionsOf finite
    & mapMaybe
      ( \degreeValue ->
          firstFiltrationViolation
            denseBirthsByDegree
            degreeValue
            (incidenceMatrixAt finite degreeValue)
      )
    & List.find (const True)
    & maybe (Right ()) (Left . InvalidTopologyInput)

firstFiltrationViolation ::
  (Integral r, Ord filtration) =>
  Vector.Vector (Vector.Vector filtration) ->
  HomologicalDegree ->
  BoundaryIncidence r ->
  Maybe String
firstFiltrationViolation denseBirthsByDegree degreeValue@(HomologicalDegree degreeIndex) incidence
  | degreeIndex <= 0 = Nothing
  | otherwise =
      Vector.foldr
        (firstColumnFiltrationViolation denseBirthsByDegree degreeValue)
        Nothing
        (boundaryEntriesBySource incidence)

firstColumnFiltrationViolation ::
  (Integral r, Ord filtration) =>
  Vector.Vector (Vector.Vector filtration) ->
  HomologicalDegree ->
  [BoundaryEntry r] ->
  Maybe String ->
  Maybe String
firstColumnFiltrationViolation denseBirthsByDegree degreeValue entries remainingViolation =
  case List.find (const True) (mapMaybe (filtrationViolationAtEntry denseBirthsByDegree degreeValue) entries) of
    Just violation -> Just violation
    Nothing -> remainingViolation

filtrationViolationAtEntry ::
  (Integral r, Ord filtration) =>
  Vector.Vector (Vector.Vector filtration) ->
  HomologicalDegree ->
  BoundaryEntry r ->
  Maybe String
filtrationViolationAtEntry denseBirthsByDegree degreeValue entry
  | boundaryCoefficient entry == 0 = Nothing
  | otherwise =
      let sourceCell =
            BasisCellRef
              { cellDegree = degreeValue,
                cellIndex = sourceIndex entry
              }
          targetCell =
            BasisCellRef
              { cellDegree = decrementDegree degreeValue,
                cellIndex = targetIndex entry
              }
       in case (denseBirthAt denseBirthsByDegree sourceCell, denseBirthAt denseBirthsByDegree targetCell) of
            (Just sourceBirth, Just targetBirth)
              | targetBirth > sourceBirth ->
                  Just
                    ( "filtration violates face monotonicity for "
                        <> show sourceCell
                        <> " -> "
                        <> show targetCell
                    )
            _ -> Nothing

denseBirthAt ::
  Vector.Vector (Vector.Vector filtration) ->
  BasisCellRef ->
  Maybe filtration
denseBirthAt denseBirthsByDegree cellRefValue =
  case cellDegree cellRefValue of
    HomologicalDegree degreeIndex ->
      denseBirthsByDegree Vector.!? degreeIndex
        >>= (\birthsAtDegree -> birthsAtDegree Vector.!? cellIndex cellRefValue)
