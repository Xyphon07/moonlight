module Moonlight.Category.Effect.Laws.Algebra
  ( lawSuites,
  )
where

import qualified Moonlight.Category.Effect.Harness as Harness
import Moonlight.Category.Effect.LawNames (LawName (..))
import Moonlight.Category.Effect.Laws.Generators
  ( SampleOrdinalLower (..),
    SampleOrdinalUpper (..),
  )
import Moonlight.Category.Pure.Poset
  ( OrdinalLower,
    OrdinalUpper,
  )
import Moonlight.Pale.Test.Laws.Suite (LawSuite, lawGroup, namedQuickCheckLaw)

galoisAdjointProp :: SampleOrdinalLower -> SampleOrdinalUpper -> Bool
galoisAdjointProp (SampleOrdinalLower leftValue) (SampleOrdinalUpper rightValue) =
  Harness.galoisAdjoint @OrdinalLower @OrdinalUpper leftValue rightValue

galoisDeflationProp :: SampleOrdinalUpper -> Bool
galoisDeflationProp (SampleOrdinalUpper rightValue) =
  Harness.galoisDeflation @OrdinalLower @OrdinalUpper rightValue

galoisInflationProp :: SampleOrdinalLower -> Bool
galoisInflationProp (SampleOrdinalLower leftValue) =
  Harness.galoisInflation @OrdinalLower @OrdinalUpper leftValue

galoisRetractionProp :: SampleOrdinalLower -> Bool
galoisRetractionProp (SampleOrdinalLower leftValue) =
  Harness.galoisRetraction @OrdinalLower @OrdinalUpper leftValue

ordinalMonotoneProp :: Bool
ordinalMonotoneProp = Harness.ordinalGaloisMonotone @OrdinalLower @OrdinalUpper

lawSuites :: [LawSuite]
lawSuites =
  [ lawGroup
      "galois"
      [ namedQuickCheckLaw GaloisAdjoint galoisAdjointProp,
        namedQuickCheckLaw GaloisDeflation galoisDeflationProp,
        namedQuickCheckLaw GaloisInflation galoisInflationProp,
        namedQuickCheckLaw GaloisRetraction galoisRetractionProp,
        namedQuickCheckLaw OrdinalGaloisMonotone ordinalMonotoneProp
      ]
  ]
