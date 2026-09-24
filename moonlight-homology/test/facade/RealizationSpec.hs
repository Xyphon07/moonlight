{-# LANGUAGE StandaloneKindSignatures #-}

module RealizationSpec
  ( tests,
  )
where

import Data.Bifunctor (first)
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Moonlight.Homology
  ( CellCarrierError,
    DirectionFieldError,
    DirectionSymmetryOrderError,
    HomologicalDegree (..),
    MacroScaffoldIR (..),
    MorseReebScaffold (..),
    PotentialNormalization (..),
    RawCellData,
    RealizationBudget (..),
    ScalarPotentialFieldError,
    mkCellCarrier,
    mkDirectionAngleField,
    mkDirectionSymmetryOrder,
    mkScalarPotentialFieldFromSamples,
    realizeScaffoldRaw,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( Assertion,
    assertEqual,
    assertFailure,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "realization"
    [testCase "public homology realization API realizes an empty scaffold" publicRealizationApiSpec]

type PublicRealizationFixtureFailure :: Type
data PublicRealizationFixtureFailure
  = PublicCarrierFailure CellCarrierError
  | PublicScalarPotentialFailure ScalarPotentialFieldError
  | PublicDirectionSymmetryFailure DirectionSymmetryOrderError
  | PublicDirectionFieldFailure DirectionFieldError
  deriving stock (Eq, Show)

publicRealizationApiSpec :: Assertion
publicRealizationApiSpec =
  case emptyPublicScaffold of
    Left fixtureFailure ->
      assertFailure ("public homology fixture failed: " <> show fixtureFailure)
    Right scaffold ->
      case realizeScaffoldRaw scaffold (RealizationBudget 0) of
        Left realizationFailure ->
          assertFailure ("public homology realization failed: " <> show realizationFailure)
        Right rawCells ->
          assertEqual "empty scaffold realizes to empty raw cells" (mempty :: RawCellData) rawCells

emptyPublicScaffold :: Either PublicRealizationFixtureFailure MacroScaffoldIR
emptyPublicScaffold = do
  carrier <- first PublicCarrierFailure (mkCellCarrier (HomologicalDegree 0) [])
  scalarPotential <-
    first
      PublicScalarPotentialFailure
      (mkScalarPotentialFieldFromSamples carrier NativePotentialScale Map.empty)
  directionSymmetry <-
    first
      PublicDirectionSymmetryFailure
      (mkDirectionSymmetryOrder 1)
  directionField <-
    first
      PublicDirectionFieldFailure
      (mkDirectionAngleField carrier directionSymmetry Map.empty)
  pure
    MacroScaffoldIR
      { macroScaffoldScalarPotential = scalarPotential,
        macroScaffoldReeb = MorseReebScaffold [] [],
        macroScaffoldDirectionField = directionField,
        macroScaffoldSingularities = [],
        macroScaffoldHarmonicLoops = []
      }
