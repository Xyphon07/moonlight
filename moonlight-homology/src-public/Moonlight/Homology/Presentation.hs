module Moonlight.Homology.Presentation
  ( ChainSpec (..),
    ChainBuildError (..),
    compileChain,
  )
where

import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import Moonlight.Core (Semiring)
import Moonlight.Homology.Boundary.Finite
  ( FiniteChainComplex,
    mkFiniteChainComplexChecked,
  )
import Moonlight.Homology.Boundary.LinAlg
  ( BoundaryIncidence,
    BoundaryIncidenceShapeError,
    emptyBoundaryIncidence,
    emptyBoundaryIncidenceOf,
    mkBoundaryEntryFromInts,
    mkBoundaryIncidence,
  )
import Moonlight.Homology.Pure.Degree (HomologicalDegree (..))
import Moonlight.Homology.Pure.Failure (HomologyFailure)

type ChainSpec :: Type -> Type
data ChainSpec r = ChainSpec
  { chainCellCounts :: [Int],
    chainBoundaries :: [[(Int, Int, r)]]
  }
  deriving stock (Eq, Show)

type ChainBuildError :: Type
data ChainBuildError
  = ChainBuildEmptySpec
  | ChainBuildNegativeCellCount Int Int
  | ChainBuildBoundaryCountMismatch Int Int
  | ChainBuildIncidenceFault Int BoundaryIncidenceShapeError
  | ChainBuildComplexFault HomologyFailure
  deriving stock (Eq, Show)

compileChain ::
  (Eq r, Num r, Semiring r) =>
  ChainSpec r ->
  Either ChainBuildError (FiniteChainComplex r)
compileChain spec = do
  cellCounts <- declaredCellCounts spec
  incidences <- incidenceTable cellCounts (chainBoundaries spec)
  first
    ChainBuildComplexFault
    ( mkFiniteChainComplexChecked
        (HomologicalDegree (length cellCounts - 1))
        (incidenceFor cellCounts incidences)
    )

declaredCellCounts :: ChainSpec r -> Either ChainBuildError [Int]
declaredCellCounts spec =
  case chainCellCounts spec of
    [] -> Left ChainBuildEmptySpec
    cellCounts -> do
      traverse_
        ( \(degreeIndex, cellCount) ->
            if cellCount < 0
              then Left (ChainBuildNegativeCellCount degreeIndex cellCount)
              else Right ()
        )
        (zip [0 ..] cellCounts)
      let expectedBoundaryCount = length cellCounts - 1
          observedBoundaryCount = length (chainBoundaries spec)
      if observedBoundaryCount == expectedBoundaryCount
        then Right cellCounts
        else Left (ChainBuildBoundaryCountMismatch expectedBoundaryCount observedBoundaryCount)

incidenceTable ::
  (Eq r, Semiring r) =>
  [Int] ->
  [[(Int, Int, r)]] ->
  Either ChainBuildError (Map.Map Int (BoundaryIncidence r))
incidenceTable cellCounts boundaries =
  Map.fromList
    <$> traverse
      ( \(degreeIndex, (sourceCount, targetCount), entryTriples) ->
          (,) degreeIndex
            <$> first
              (ChainBuildIncidenceFault degreeIndex)
              ( mkBoundaryIncidence
                  (fromIntegral sourceCount)
                  (fromIntegral targetCount)
                  ( fmap
                      ( \(sourceValue, targetValue, coefficientValue) ->
                          mkBoundaryEntryFromInts sourceValue targetValue coefficientValue
                      )
                      entryTriples
                  )
              )
      )
      (zip3 [1 ..] (zip (drop 1 cellCounts) cellCounts) boundaries)

incidenceFor ::
  [Int] ->
  Map.Map Int (BoundaryIncidence r) ->
  HomologicalDegree ->
  BoundaryIncidence r
incidenceFor cellCounts incidences (HomologicalDegree degreeIndex)
  | degreeIndex == 0 =
      case cellCounts of
        zeroCellCount : _ -> emptyBoundaryIncidenceOf (fromIntegral zeroCellCount) 0
        [] -> emptyBoundaryIncidence
  | otherwise = Map.findWithDefault emptyBoundaryIncidence degreeIndex incidences
