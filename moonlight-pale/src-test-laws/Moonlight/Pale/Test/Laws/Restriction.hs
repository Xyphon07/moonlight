{-| Checked finite restriction systems and their functoriality law suites. -}
module Moonlight.Pale.Test.Laws.Restriction
  ( FiniteRestrictionLaw,
    FiniteRestrictionError (..),
    compileFiniteRestrictionLaw,
    finiteRestrictionLaws,
  )
where

import Data.Foldable (traverse_)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Moonlight.Pale.Test.Laws.Suite (LawSuite, hUnitLaw, lawGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure)

type FiniteRestrictionLaw :: Type -> Type -> Type
data FiniteRestrictionLaw cell val = FiniteRestrictionLaw
  { finiteRestrictionName :: String,
    finiteRestrictionCells :: !(Vector cell),
    finiteRestrictionUpperSets :: !(Vector IntSet),
    finiteRestrictionSections :: !(Vector [val]),
    finiteRestrictionMap :: cell -> cell -> val -> val
  }

type FiniteRestrictionError :: Type -> Type
data FiniteRestrictionError cell
  = DuplicateRestrictionCell cell !Int !Int
  | SectionCellOutsideUniverse cell
  | RestrictionRelationNotReflexive cell
  | RestrictionRelationNotAntisymmetric cell cell
  | RestrictionRelationNotTransitive cell cell
  deriving stock (Eq, Show)

data RestrictionUniverseCompilation cell = RestrictionUniverseCompilation
  { restrictionUniverseIndex :: !(Map cell Int),
    restrictionUniverseErrorsReversed :: ![FiniteRestrictionError cell]
  }

data RestrictionDenseObstruction
  = RestrictionCellIndexOutOfBounds !Int
  | RestrictionUpperSetIndexOutOfBounds !Int
  deriving stock (Eq, Show)

compileFiniteRestrictionLaw ::
  Ord cell =>
  String ->
  NonEmpty cell ->
  (cell -> cell -> Bool) ->
  [(cell, val)] ->
  (cell -> cell -> val -> val) ->
  Either (NonEmpty (FiniteRestrictionError cell)) (FiniteRestrictionLaw cell val)
compileFiniteRestrictionLaw name cellUniverse leq sections restrict = do
  let cells = Vector.fromList (NonEmpty.toList cellUniverse)
  cellIndex <- compileRestrictionUniverse cells
  let upperSets = compileUpperSets leq cells
      validationErrors =
        validateFinitePoset cells upperSets
          <> unknownSectionErrors cellIndex sections
  case NonEmpty.nonEmpty validationErrors of
    Just errors -> Left errors
    Nothing ->
      Right
        FiniteRestrictionLaw
          { finiteRestrictionName = name,
            finiteRestrictionCells = cells,
            finiteRestrictionUpperSets = upperSets,
            finiteRestrictionSections = compileSectionsByCell cells sections,
            finiteRestrictionMap = restrict
          }

compileRestrictionUniverse ::
  Ord cell =>
  Vector cell ->
  Either (NonEmpty (FiniteRestrictionError cell)) (Map cell Int)
compileRestrictionUniverse cells =
  let compilation =
        Vector.ifoldl'
          insertRestrictionCell
          (RestrictionUniverseCompilation Map.empty [])
          cells
   in case NonEmpty.nonEmpty (reverse (restrictionUniverseErrorsReversed compilation)) of
        Just errors -> Left errors
        Nothing -> Right (restrictionUniverseIndex compilation)

insertRestrictionCell ::
  Ord cell =>
  RestrictionUniverseCompilation cell ->
  Int ->
  cell ->
  RestrictionUniverseCompilation cell
insertRestrictionCell compilation duplicatePosition cell =
  case Map.lookup cell (restrictionUniverseIndex compilation) of
    Just originalPosition ->
      compilation
        { restrictionUniverseErrorsReversed =
            DuplicateRestrictionCell cell originalPosition duplicatePosition
              : restrictionUniverseErrorsReversed compilation
        }
    Nothing ->
      compilation
        { restrictionUniverseIndex =
            Map.insert cell duplicatePosition (restrictionUniverseIndex compilation)
        }

compileUpperSets :: (cell -> cell -> Bool) -> Vector cell -> Vector IntSet
compileUpperSets leq cells =
  Vector.map
    (\sourceCell ->
       Vector.ifoldl'
         (\upperSet targetIndex targetCell ->
            if leq sourceCell targetCell
              then IntSet.insert targetIndex upperSet
              else upperSet
         )
         IntSet.empty
         cells
    )
    cells

validateFinitePoset :: Vector cell -> Vector IntSet -> [FiniteRestrictionError cell]
validateFinitePoset cells upperSets =
  validateReflexivity indexedRows
    <> validateAntisymmetry indexedRows
    <> validateTransitivity indexedRows
  where
    indexedRows = Vector.indexed (Vector.zip cells upperSets)

validateReflexivity ::
  Vector (Int, (cell, IntSet)) ->
  [FiniteRestrictionError cell]
validateReflexivity =
  Vector.foldr
    (\(cellIndex, (cell, upperSet)) errors ->
       if IntSet.member cellIndex upperSet
         then errors
         else RestrictionRelationNotReflexive cell : errors
    )
    []

validateAntisymmetry ::
  Vector (Int, (cell, IntSet)) ->
  [FiniteRestrictionError cell]
validateAntisymmetry indexedRows =
  reverse $
    Vector.foldl'
      (\errors (leftIndex, (leftCell, leftUpperSet)) ->
         Vector.foldl'
           (\nestedErrors (rightIndex, (rightCell, rightUpperSet)) ->
              if
                  leftIndex < rightIndex
                    && IntSet.member rightIndex leftUpperSet
                    && IntSet.member leftIndex rightUpperSet
                then
                  RestrictionRelationNotAntisymmetric leftCell rightCell
                    : nestedErrors
                else nestedErrors
           )
           errors
           indexedRows
      )
      []
      indexedRows

validateTransitivity ::
  Vector (Int, (cell, IntSet)) ->
  [FiniteRestrictionError cell]
validateTransitivity indexedRows =
  reverse $
    Vector.foldl'
      (\errors (_, (sourceCell, sourceUpperSet)) ->
         Vector.foldl'
           (\nestedErrors (middleIndex, (middleCell, middleUpperSet)) ->
              if
                  IntSet.member middleIndex sourceUpperSet
                    && not (middleUpperSet `IntSet.isSubsetOf` sourceUpperSet)
                then
                  RestrictionRelationNotTransitive sourceCell middleCell
                    : nestedErrors
                else nestedErrors
           )
           errors
           indexedRows
      )
      []
      indexedRows

unknownSectionErrors ::
  Ord cell =>
  Map cell Int ->
  [(cell, val)] ->
  [FiniteRestrictionError cell]
unknownSectionErrors cellIndex =
  foldr
    (\(cell, _) errors ->
       if Map.member cell cellIndex
         then errors
         else SectionCellOutsideUniverse cell : errors
    )
    []

compileSectionsByCell ::
  Ord cell =>
  Vector cell ->
  [(cell, val)] ->
  Vector [val]
compileSectionsByCell cells sections =
  let reversedSections =
        foldl'
          (\sectionsByCell (cell, value) ->
             Map.insertWith (<>) cell [value] sectionsByCell
          )
          Map.empty
          sections
   in Vector.map
        (\cell -> reverse (Map.findWithDefault [] cell reversedSections))
        cells

finiteRestrictionLaws ::
  (Show cell, Show val, Eq val) =>
  FiniteRestrictionLaw cell val ->
  [LawSuite]
finiteRestrictionLaws restrictionLaw =
  [ lawGroup
      (finiteRestrictionName restrictionLaw <> " restriction laws")
      [ restrictionIdentity restrictionLaw,
        restrictionComposition restrictionLaw,
        restrictionSourceIdentity restrictionLaw,
        restrictionTargetIdentity restrictionLaw
      ]
  ]

restrictionIdentity ::
  (Show cell, Show val, Eq val) =>
  FiniteRestrictionLaw cell val ->
  LawSuite
restrictionIdentity restrictionLaw =
  hUnitLaw "restriction identity" $
    traverse_
      (\(cell, sections) ->
         traverse_
           (\section ->
              assertEqual
                ("identity at " <> show cell)
                section
                (finiteRestrictionMap restrictionLaw cell cell section)
           )
           sections
      )
      (Vector.zip (finiteRestrictionCells restrictionLaw) (finiteRestrictionSections restrictionLaw))

restrictionComposition ::
  (Show cell, Show val, Eq val) =>
  FiniteRestrictionLaw cell val ->
  LawSuite
restrictionComposition restrictionLaw =
  relatedRestrictionTripleLaw restrictionLaw "restriction composition" $
    \sourceCell middleCell targetCell section ->
      assertEqual
        ("composition along " <> show (sourceCell, middleCell, targetCell))
        (finiteRestrictionMap restrictionLaw sourceCell targetCell section)
        ( finiteRestrictionMap restrictionLaw middleCell targetCell
            (finiteRestrictionMap restrictionLaw sourceCell middleCell section)
        )

restrictionSourceIdentity ::
  (Show cell, Show val, Eq val) =>
  FiniteRestrictionLaw cell val ->
  LawSuite
restrictionSourceIdentity restrictionLaw =
  relatedRestrictionPairLaw restrictionLaw "restriction source identity" $
    \sourceCell targetCell section ->
      assertEqual
        ("source identity along " <> show (sourceCell, targetCell))
        (finiteRestrictionMap restrictionLaw sourceCell targetCell section)
        ( finiteRestrictionMap restrictionLaw sourceCell targetCell
            (finiteRestrictionMap restrictionLaw sourceCell sourceCell section)
        )

restrictionTargetIdentity ::
  (Show cell, Show val, Eq val) =>
  FiniteRestrictionLaw cell val ->
  LawSuite
restrictionTargetIdentity restrictionLaw =
  relatedRestrictionPairLaw restrictionLaw "restriction target identity" $
    \sourceCell targetCell section ->
      assertEqual
        ("target identity along " <> show (sourceCell, targetCell))
        (finiteRestrictionMap restrictionLaw sourceCell targetCell section)
        ( finiteRestrictionMap restrictionLaw targetCell targetCell
            (finiteRestrictionMap restrictionLaw sourceCell targetCell section)
        )

relatedRestrictionPairLaw ::
  FiniteRestrictionLaw cell val ->
  String ->
  (cell -> cell -> val -> Assertion) ->
  LawSuite
relatedRestrictionPairLaw restrictionLaw label check =
  hUnitLaw label $
    traverse_
      (\(sourceCell, sourceUpperSet, sourceSections) ->
         traverseIntSet_ sourceUpperSet $ \targetIndex ->
           withRestrictionCell restrictionLaw targetIndex $ \targetCell ->
             traverse_ (check sourceCell targetCell) sourceSections
      )
      (restrictionRows restrictionLaw)

relatedRestrictionTripleLaw ::
  FiniteRestrictionLaw cell val ->
  String ->
  (cell -> cell -> cell -> val -> Assertion) ->
  LawSuite
relatedRestrictionTripleLaw restrictionLaw label check =
  hUnitLaw label $
    traverse_
      (\(sourceCell, sourceUpperSet, sourceSections) ->
         traverseIntSet_ sourceUpperSet $ \middleIndex ->
           withRestrictionCell restrictionLaw middleIndex $ \middleCell ->
             withRestrictionUpperSet restrictionLaw middleIndex $ \middleUpperSet ->
               traverseIntSet_ middleUpperSet $ \targetIndex ->
                 withRestrictionCell restrictionLaw targetIndex $ \targetCell ->
                   traverse_ (check sourceCell middleCell targetCell) sourceSections
      )
      (restrictionRows restrictionLaw)

restrictionRows ::
  FiniteRestrictionLaw cell val ->
  Vector (cell, IntSet, [val])
restrictionRows restrictionLaw =
  Vector.zip3
    (finiteRestrictionCells restrictionLaw)
    (finiteRestrictionUpperSets restrictionLaw)
    (finiteRestrictionSections restrictionLaw)

traverseIntSet_ :: IntSet -> (Int -> Assertion) -> Assertion
traverseIntSet_ indices action =
  IntSet.foldr (\denseIndex rest -> action denseIndex *> rest) (pure ()) indices

withRestrictionCell ::
  FiniteRestrictionLaw cell val ->
  Int ->
  (cell -> Assertion) ->
  Assertion
withRestrictionCell restrictionLaw denseIndex useCell =
  case finiteRestrictionCells restrictionLaw Vector.!? denseIndex of
    Nothing ->
      assertFailure
        ( "finite restriction cell obstruction: "
            <> show (RestrictionCellIndexOutOfBounds denseIndex)
        )
    Just cell -> useCell cell

withRestrictionUpperSet ::
  FiniteRestrictionLaw cell val ->
  Int ->
  (IntSet -> Assertion) ->
  Assertion
withRestrictionUpperSet restrictionLaw denseIndex useUpperSet =
  case finiteRestrictionUpperSets restrictionLaw Vector.!? denseIndex of
    Nothing ->
      assertFailure
        ( "finite restriction relation obstruction: "
            <> show (RestrictionUpperSetIndexOutOfBounds denseIndex)
        )
    Just upperSet -> useUpperSet upperSet
