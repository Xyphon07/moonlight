{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wmissing-local-signatures #-}

module RestrictionSpec
  ( tests,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Moonlight.Pale.Test.Laws.Restriction
  ( FiniteRestrictionError (..),
    FiniteRestrictionLaw,
    compileFiniteRestrictionLaw,
    finiteRestrictionLaws,
  )
import Moonlight.Pale.Test.Laws.Suite (lawGroup, renderLawSuite)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

data ChainCell
  = ChainBottom
  | ChainMiddle
  | ChainTop
  deriving stock (Eq, Ord, Show)

data OwnedSection
  = OwnedSection !ChainCell !Int
  | RestrictionSourceMismatch !ChainCell !ChainCell !Int
  deriving stock (Eq, Show)

tests :: TestTree
tests =
  testGroup
    "Moonlight.Pale.Test.Laws.Restriction"
    [ renderFiniteRestriction "chain restriction suite" chainRestriction,
      testCase "source and target identities apply identity at the typed object" $ do
        let sourceSection = OwnedSection ChainBottom 7
            direct = restrictOwnedSection ChainBottom ChainMiddle sourceSection
            reversedSourceIdentity =
              restrictOwnedSection
                ChainBottom
                ChainBottom
                (restrictOwnedSection ChainBottom ChainMiddle sourceSection)
            reversedTargetIdentity =
              restrictOwnedSection
                ChainBottom
                ChainMiddle
                (restrictOwnedSection ChainMiddle ChainMiddle sourceSection)
        assertEqual
          "source identity"
          direct
          ( restrictOwnedSection
              ChainBottom
              ChainMiddle
              (restrictOwnedSection ChainBottom ChainBottom sourceSection)
          )
        assertEqual
          "target identity"
          direct
          ( restrictOwnedSection
              ChainMiddle
              ChainMiddle
              (restrictOwnedSection ChainBottom ChainMiddle sourceSection)
          )
        assertBool
          "the former source equation applies the identity to the wrong fiber"
          (reversedSourceIdentity /= direct)
        assertBool
          "the former target equation applies the identity before entering its fiber"
          (reversedTargetIdentity /= direct),
      testCase "duplicate cells retain both dense positions" $
        assertRestrictionErrors
          "duplicate ChainBottom"
          (DuplicateRestrictionCell ChainBottom 0 2 :| [])
          duplicateCellRestriction,
      testCase "sections outside the finite cell universe are rejected" $
        assertRestrictionErrors
          "unknown source cell"
          (SectionCellOutsideUniverse ChainTop :| [])
          unknownSectionRestriction,
      testCase "non-reflexive relations cannot compile" $
        assertRestrictionErrors
          "both missing identities are reported"
          ( RestrictionRelationNotReflexive ChainBottom
              :| [RestrictionRelationNotReflexive ChainMiddle]
          )
          nonReflexiveRestriction,
      testCase "contradictory two-way order cannot compile" $
        assertRestrictionErrors
          "antisymmetry rejects distinct mutually related cells"
          (RestrictionRelationNotAntisymmetric ChainBottom ChainMiddle :| [])
          contradictoryRestriction,
      testCase "non-transitive relation cannot compile" $
        assertRestrictionErrors
          "the missing bottom-to-top edge is typed"
          (RestrictionRelationNotTransitive ChainBottom ChainMiddle :| [])
          nonTransitiveRestriction
    ]

chainRestriction ::
  Either
    (NonEmpty (FiniteRestrictionError ChainCell))
    (FiniteRestrictionLaw ChainCell OwnedSection)
chainRestriction =
  compileFiniteRestrictionLaw
    "chain"
    chainCells
    chainLeq
    chainSections
    restrictOwnedSection

duplicateCellRestriction ::
  Either
    (NonEmpty (FiniteRestrictionError ChainCell))
    (FiniteRestrictionLaw ChainCell OwnedSection)
duplicateCellRestriction =
  compileFiniteRestrictionLaw
    "duplicate"
    (ChainBottom :| [ChainMiddle, ChainBottom])
    chainLeq
    []
    restrictOwnedSection

unknownSectionRestriction ::
  Either
    (NonEmpty (FiniteRestrictionError ChainCell))
    (FiniteRestrictionLaw ChainCell OwnedSection)
unknownSectionRestriction =
  compileFiniteRestrictionLaw
    "unknown section"
    (ChainBottom :| [ChainMiddle])
    chainLeq
    [(ChainTop, OwnedSection ChainTop 0)]
    restrictOwnedSection

nonReflexiveRestriction ::
  Either
    (NonEmpty (FiniteRestrictionError ChainCell))
    (FiniteRestrictionLaw ChainCell OwnedSection)
nonReflexiveRestriction =
  compileFiniteRestrictionLaw
    "non-reflexive"
    (ChainBottom :| [ChainMiddle])
    (\_ _ -> False)
    []
    restrictOwnedSection

contradictoryRestriction ::
  Either
    (NonEmpty (FiniteRestrictionError ChainCell))
    (FiniteRestrictionLaw ChainCell OwnedSection)
contradictoryRestriction =
  compileFiniteRestrictionLaw
    "contradictory"
    (ChainBottom :| [ChainMiddle])
    (\_ _ -> True)
    []
    restrictOwnedSection

nonTransitiveRestriction ::
  Either
    (NonEmpty (FiniteRestrictionError ChainCell))
    (FiniteRestrictionLaw ChainCell OwnedSection)
nonTransitiveRestriction =
  compileFiniteRestrictionLaw
    "non-transitive"
    chainCells
    adjacentChainLeq
    []
    restrictOwnedSection

chainCells :: NonEmpty ChainCell
chainCells = ChainBottom :| [ChainMiddle, ChainTop]

chainSections :: [(ChainCell, OwnedSection)]
chainSections =
  fmap
    (\cell -> (cell, OwnedSection cell (chainRank cell)))
    (toList chainCells)

restrictOwnedSection :: ChainCell -> ChainCell -> OwnedSection -> OwnedSection
restrictOwnedSection sourceCell targetCell section =
  case section of
    OwnedSection owner payload
      | owner == sourceCell -> OwnedSection targetCell payload
      | otherwise -> RestrictionSourceMismatch sourceCell owner payload
    RestrictionSourceMismatch expectedSource actualSource payload ->
      RestrictionSourceMismatch expectedSource actualSource payload

chainLeq :: ChainCell -> ChainCell -> Bool
chainLeq leftCell rightCell =
  chainRank leftCell <= chainRank rightCell

adjacentChainLeq :: ChainCell -> ChainCell -> Bool
adjacentChainLeq leftCell rightCell =
  leftCell == rightCell
    || (leftCell == ChainBottom && rightCell == ChainMiddle)
    || (leftCell == ChainMiddle && rightCell == ChainTop)

chainRank :: ChainCell -> Int
chainRank cell =
  case cell of
    ChainBottom -> 0
    ChainMiddle -> 1
    ChainTop -> 2

renderFiniteRestriction ::
  (Show cell, Show val, Eq val) =>
  String ->
  Either (NonEmpty (FiniteRestrictionError cell)) (FiniteRestrictionLaw cell val) ->
  TestTree
renderFiniteRestriction label restrictionResult =
  case restrictionResult of
    Left errors ->
      testCase (label <> " compiles") $
        assertFailure ("expected valid finite restriction law: " <> show errors)
    Right restrictionLaw ->
      renderLawSuite (lawGroup label (finiteRestrictionLaws restrictionLaw))

assertRestrictionErrors ::
  (Eq cell, Show cell) =>
  String ->
  NonEmpty (FiniteRestrictionError cell) ->
  Either
    (NonEmpty (FiniteRestrictionError cell))
    (FiniteRestrictionLaw cell val) ->
  IO ()
assertRestrictionErrors label expectedErrors restrictionResult =
  case restrictionResult of
    Left actualErrors -> assertEqual label expectedErrors actualErrors
    Right _ -> assertFailure (label <> ": expected finite restriction compilation to fail")

toList :: NonEmpty a -> [a]
toList (firstValue :| remainingValues) =
  firstValue : remainingValues
