module OrdinalSpec
  ( tests,
  )
where

import Data.Function ((&))
import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Category.Simplicial
  ( SomeMonotone (..),
    SomeNormalizedMonotone (..),
    composeSomeMonotone,
    denormalizeSomeNormalizedMonotone,
    mkSomeMonotone,
    monotoneCodomainDimension,
    monotoneDomainDimension,
    monotoneValues,
    normalizeSomeMonotone,
    normalizedInjectionValues,
    normalizedSurjectionValues,
    someMonotoneEqualByNormalForm,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import qualified Test.Tasty.QuickCheck as QC

someMonotoneSignature :: SomeMonotone -> (Integer, Integer, [Integer])
someMonotoneSignature (SomeMonotone _ _ monotone) =
  ( fromIntegral (monotoneDomainDimension monotone),
    fromIntegral (monotoneCodomainDimension monotone),
    monotoneValues monotone & map fromIntegral
  )

someMonotoneEqualByShape :: SomeMonotone -> SomeMonotone -> Bool
someMonotoneEqualByShape left right =
  someMonotoneSignature left == someMonotoneSignature right

genSomeMonotoneWithDimensions :: Integer -> Integer -> QC.Gen SomeMonotone
genSomeMonotoneWithDimensions domainDimension codomainDimension = do
  sampledRow <- QC.vectorOf (fromIntegral domainDimension + 1) (QC.chooseInt (0, fromIntegral codomainDimension))
  let monotoneRow = sampledRow & sort & map fromIntegral
  case mkSomeMonotone (fromIntegral domainDimension) (fromIntegral codomainDimension) monotoneRow of
    Nothing -> genSomeMonotoneWithDimensions domainDimension codomainDimension
    Just morphism -> pure morphism

genSomeMonotone :: QC.Gen SomeMonotone
genSomeMonotone = do
  domainDimension <- QC.chooseInteger (0, 4)
  codomainDimension <- QC.chooseInteger (0, 4)
  genSomeMonotoneWithDimensions domainDimension codomainDimension

genComposableTriple :: QC.Gen (SomeMonotone, SomeMonotone, SomeMonotone)
genComposableTriple = do
  sourceDimension <- QC.chooseInteger (0, 3)
  middleLeftDimension <- QC.chooseInteger (0, 3)
  middleRightDimension <- QC.chooseInteger (0, 3)
  targetDimension <- QC.chooseInteger (0, 3)
  inner <- genSomeMonotoneWithDimensions sourceDimension middleLeftDimension
  middle <- genSomeMonotoneWithDimensions middleLeftDimension middleRightDimension
  outer <- genSomeMonotoneWithDimensions middleRightDimension targetDimension
  pure (outer, middle, inner)

identitySomeMonotone :: Integer -> Maybe SomeMonotone
identitySomeMonotone dimensionValue =
  mkSomeMonotone
    (fromIntegral dimensionValue)
    (fromIntegral dimensionValue)
    [0 .. fromIntegral dimensionValue]

identityLawHolds :: SomeMonotone -> Bool
identityLawHolds morphism =
  case someMonotoneSignature morphism of
    (domainDimension, codomainDimension, _) ->
      case (identitySomeMonotone codomainDimension, identitySomeMonotone domainDimension) of
        (Just leftIdentity, Just rightIdentity) ->
          case (composeSomeMonotone leftIdentity morphism, composeSomeMonotone morphism rightIdentity) of
            (Just leftComposed, Just rightComposed) ->
              someMonotoneEqualByShape leftComposed morphism
                && someMonotoneEqualByShape rightComposed morphism
            _ -> False
        _ -> False

associativityLawHolds :: (SomeMonotone, SomeMonotone, SomeMonotone) -> Bool
associativityLawHolds (outer, middle, inner) =
  let leftComposed = composeSomeMonotone outer =<< composeSomeMonotone middle inner
      rightComposed = (`composeSomeMonotone` inner) =<< composeSomeMonotone outer middle
   in case (leftComposed, rightComposed) of
        (Just leftValue, Just rightValue) -> someMonotoneEqualByShape leftValue rightValue
        (Nothing, Nothing) -> True
        _ -> False

normalizationRoundtripHolds :: SomeMonotone -> Bool
normalizationRoundtripHolds monotone =
  case normalizeSomeMonotone monotone >>= denormalizeSomeNormalizedMonotone of
    Nothing -> False
    Just reconstructed ->
      someMonotoneEqualByShape monotone reconstructed
        && someMonotoneEqualByNormalForm monotone reconstructed

normalizationCanonicalRanksHold :: SomeMonotone -> Bool
normalizationCanonicalRanksHold monotone@(SomeMonotone _ _ monotoneValue) =
  case normalizeSomeMonotone monotone of
    Nothing -> False
    Just (SomeNormalizedMonotone _ _ normalized) ->
      let imageValues =
            monotoneValues monotoneValue
              & Set.fromList
              & Set.toAscList
          imageRanks = Map.fromList (zip imageValues [0 ..])
       in normalizedInjectionValues normalized == imageValues
            && traverse (`Map.lookup` imageRanks) (monotoneValues monotoneValue)
              == Just (normalizedSurjectionValues normalized)

tests :: TestTree
tests =
  testGroup
    "Ordinal"
    [ testCase "normalization roundtrip for a coface-style morphism" $
        case mkSomeMonotone 2 3 [0, 2, 3] of
          Nothing -> assertBool "expected valid monotone" False
          Just monotone ->
            case normalizeSomeMonotone monotone >>= denormalizeSomeNormalizedMonotone of
              Nothing -> assertBool "expected normal form roundtrip" False
              Just reconstructed -> assertEqual "roundtrip" (someMonotoneSignature monotone) (someMonotoneSignature reconstructed),
      QC.testProperty "identity law for monotone ordinal maps" (QC.withNumTests 400 (QC.forAllBlind genSomeMonotone identityLawHolds)),
      QC.testProperty "associativity for typed monotone composition" (QC.withNumTests 400 (QC.forAllBlind genComposableTriple associativityLawHolds)),
      QC.testProperty "normalization roundtrip preserves monotone map" (QC.withNumTests 400 (QC.forAllBlind genSomeMonotone normalizationRoundtripHolds)),
      QC.testProperty "normalization retains canonical image ranks" (QC.withNumTests 400 (QC.forAllBlind genSomeMonotone normalizationCanonicalRanksHold))
    ]
