module CompilePass.PublicApiCleanup
  ( publicAggregateApi,
    publicCuratedModulesApi,
  )
where

import Data.Bifunctor (first)
import qualified Moonlight.Homology as H
import qualified Moonlight.Homology.Boundary as Boundary
import qualified Moonlight.Homology.Persistence as Persistence

publicAggregateApi :: Either H.HomologyFailure (H.BoundaryIncidence Int, [H.PersistencePair H.FiltrationValue], H.TopologyWitness () () H.FiltrationValue () ())
publicAggregateApi = do
  incidence <- first (H.InvalidBoundaryIncidence . show) (H.mkBoundaryIncidence 1 1 [H.mkBoundaryEntry 0 0 (1 :: Int)])
  finite <- pointComplex
  filtered <- H.mkFilteredFiniteChainComplex finite pointBirths
  pairs <- H.mod2PersistentPairs filtered
  witness <- H.mod2PersistenceTopologyWitness filtered
  pure (incidence, pairs, witness)

publicCuratedModulesApi :: Either H.HomologyFailure (H.BoundaryIncidence Int, [H.PersistencePair H.FiltrationValue], H.TopologyWitness () () H.FiltrationValue () ())
publicCuratedModulesApi = do
  incidence <- first (H.InvalidBoundaryIncidence . show) (Boundary.mkBoundaryIncidence 1 1 [Boundary.mkBoundaryEntry 0 0 (1 :: Int)])
  finite <- pointComplex
  filtered <- Persistence.mkFilteredFiniteChainComplex finite pointBirths
  pairs <- Persistence.mod2PersistentPairs filtered
  witness <- Persistence.mod2PersistenceTopologyWitness filtered
  pure (incidence, pairs, witness)

pointComplex :: Either H.HomologyFailure (H.FiniteChainComplex Int)
pointComplex =
  H.mkFiniteChainComplexChecked (H.HomologicalDegree 0) (const (H.emptyBoundaryIncidenceOf 1 0))

pointBirths :: [(H.BasisCellRef, H.FiltrationValue)]
pointBirths =
  [(H.BasisCellRef {H.cellDegree = H.HomologicalDegree 0, H.cellIndex = 0}, H.FiltrationValue 0)]
