{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module BoundarySpec (tests) where

import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Core (BoundaryOps (..))
import Moonlight.Core (IsLawName (..), constructorLawName)
import LawProperty (lawProperty)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
  ( Arbitrary (..),
    Gen,
    Property,
    chooseInt,
    counterexample,
    listOf,
    property,
    (===),
    (.&&.),
  )

newtype BoundaryProbe = BoundaryProbe (Set Int)
  deriving stock (Eq, Ord, Show)

instance Arbitrary BoundaryProbe where
  arbitrary =
    BoundaryProbe . Set.fromList <$> listOf boundaryPoint

data BoundaryLaw
  = BoundaryOverlapCommutative
  | BoundaryRestrictionGluing
  | BoundaryRestrictionSubsumedByOperands
  | BoundaryCompatibilityAgreesWithOverlap
  | BoundarySubsumptionPartialOrder
  | BoundarySubsumptionOverlapIdentity
  deriving stock (Bounded, Enum, Eq, Ord, Show)

instance IsLawName BoundaryLaw where
  lawNameText =
    constructorLawName . show

instance BoundaryOps BoundaryProbe where
  type BoundaryOverlap BoundaryProbe = BoundaryProbe

  overlapBetweenBoundary (BoundaryProbe left) (BoundaryProbe right) =
    BoundaryProbe (Set.intersection left right)

  restrictBoundaryRaw (BoundaryProbe overlap) (BoundaryProbe boundary) =
    BoundaryProbe (Set.intersection overlap boundary)

  compatibleBoundaryRaw left right =
    let overlap =
          overlapBetweenBoundary left right
     in if boundaryProbeNull overlap
          then Left overlap
          else Right overlap

  subsumesBoundaryRaw (BoundaryProbe superset) (BoundaryProbe subset) =
    subset `Set.isSubsetOf` superset

tests :: TestTree
tests =
  testGroup
    "Boundary"
    [ lawProperty BoundaryOverlapCommutative propOverlapCommutative,
      lawProperty BoundaryRestrictionGluing propRestrictionGluing,
      lawProperty BoundaryRestrictionSubsumedByOperands propRestrictionSubsumedByOperands,
      lawProperty BoundaryCompatibilityAgreesWithOverlap propCompatibilityAgreesWithOverlap,
      lawProperty BoundarySubsumptionPartialOrder propSubsumptionPartialOrder,
      lawProperty BoundarySubsumptionOverlapIdentity propSubsumptionOverlapIdentity
    ]

boundaryPoint :: Gen Int
boundaryPoint =
  chooseInt (-64, 64)

boundaryProbeNull :: BoundaryProbe -> Bool
boundaryProbeNull (BoundaryProbe points) =
  Set.null points

propOverlapCommutative :: BoundaryProbe -> BoundaryProbe -> Property
propOverlapCommutative left right =
  overlapBetweenBoundary left right === overlapBetweenBoundary right left

propRestrictionGluing :: BoundaryProbe -> BoundaryProbe -> Property
propRestrictionGluing left right =
  let overlap =
        overlapBetweenBoundary left right
   in (restrictBoundaryRaw overlap left, restrictBoundaryRaw overlap right)
        === (overlap, overlap)

propRestrictionSubsumedByOperands :: BoundaryProbe -> BoundaryProbe -> Property
propRestrictionSubsumedByOperands left right =
  let restricted =
        restrictBoundaryRaw (overlapBetweenBoundary left right) left
   in property (subsumesBoundaryRaw left restricted)
        .&&. property (subsumesBoundaryRaw right restricted)

propCompatibilityAgreesWithOverlap :: BoundaryProbe -> BoundaryProbe -> Property
propCompatibilityAgreesWithOverlap left right =
  compatibleBoundaryRaw left right === expectedCompatibility left right

propSubsumptionPartialOrder :: BoundaryProbe -> BoundaryProbe -> BoundaryProbe -> Property
propSubsumptionPartialOrder left middle right =
  property (subsumesBoundaryRaw left left)
    .&&. counterexample "subsumption is not antisymmetric" antisymmetric
    .&&. counterexample "subsumption is not transitive" transitive
  where
    antisymmetric =
      not (subsumesBoundaryRaw left middle && subsumesBoundaryRaw middle left)
        || left == middle
    transitive =
      not (subsumesBoundaryRaw left middle && subsumesBoundaryRaw middle right)
        || subsumesBoundaryRaw left right

propSubsumptionOverlapIdentity :: BoundaryProbe -> BoundaryProbe -> Property
propSubsumptionOverlapIdentity superset subset =
  counterexample "subsumed boundary is not recovered as overlap" $
    not (subsumesBoundaryRaw superset subset)
      || overlapBetweenBoundary superset subset == subset

expectedCompatibility :: BoundaryProbe -> BoundaryProbe -> Either BoundaryProbe BoundaryProbe
expectedCompatibility left right =
  let overlap =
        overlapBetweenBoundary left right
   in if boundaryProbeNull overlap
        then Left overlap
        else Right overlap
