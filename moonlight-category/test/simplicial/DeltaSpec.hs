
module DeltaSpec
  ( tests,
  )
where

import Data.List (sort)
import Data.Function ((&))
import GHC.TypeNats (KnownNat)
import Numeric.Natural (Natural)
import Moonlight.Category.Simplicial
  ( Coface (..),
    Codegeneracy (..),
    DeltaMorphism,
    allDeltaMorphisms,
    cofaceMorphism,
    codegeneracyMorphism,
    composeDeltaMorphism,
    deltaIdentity,
    deltaDomainDimension,
    deltaCodomainDimension,
    deltaMapValues,
    denormalizeDeltaNormalForm,
    deltaMorphismEqual,
    injectionMissingIndices,
    mkDeltaMorphism,
    normalizeDeltaMorphism,
    surjectionDegeneracyIndices,
  )
import Moonlight.Category.Simplicial (Dimension (..), mkFinOffset)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import qualified Test.Tasty.QuickCheck as QC

genDeltaMorphism :: QC.Gen DeltaMorphism
genDeltaMorphism = do
  domainDimension <- QC.chooseInt (0, 4)
  codomainDimension <- QC.chooseInt (0, 4)
  sampledRow <- QC.vectorOf (domainDimension + 1) (QC.chooseInt (0, codomainDimension))
  let monotoneRow = sampledRow & sort & map fromIntegral
  case mkDeltaMorphism (fromIntegral domainDimension) (fromIntegral codomainDimension) monotoneRow of
    Nothing -> genDeltaMorphism
    Just morphism -> pure morphism

genComposableTriple :: QC.Gen (DeltaMorphism, DeltaMorphism, DeltaMorphism)
genComposableTriple = do
  nValue <- QC.chooseInt (0, 3)
  mValue <- QC.chooseInt (0, 3)
  kValue <- QC.chooseInt (0, 3)
  lValue <- QC.chooseInt (0, 3)
  case (allDeltaMorphisms (fromIntegral nValue) (fromIntegral mValue), allDeltaMorphisms (fromIntegral mValue) (fromIntegral kValue), allDeltaMorphisms (fromIntegral kValue) (fromIntegral lValue)) of
    ([], _, _) -> genComposableTriple
    (_, [], _) -> genComposableTriple
    (_, _, []) -> genComposableTriple
    (fCandidates, gCandidates, hCandidates) -> do
      fMorphism <- QC.elements fCandidates
      gMorphism <- QC.elements gCandidates
      hMorphism <- QC.elements hCandidates
      pure (hMorphism, gMorphism, fMorphism)

identityLawHolds :: DeltaMorphism -> Bool
identityLawHolds morphism =
  composeDeltaMorphism (deltaIdentity (deltaCodomainDimension morphism)) morphism == Just morphism
    && composeDeltaMorphism morphism (deltaIdentity (deltaDomainDimension morphism)) == Just morphism

associativityLawHolds :: (DeltaMorphism, DeltaMorphism, DeltaMorphism) -> Bool
associativityLawHolds (outer, middle, inner) =
  let leftComposed = composeDeltaMorphism outer =<< composeDeltaMorphism middle inner
      rightComposed = (\composed -> composeDeltaMorphism composed inner) =<< composeDeltaMorphism outer middle
   in case (leftComposed, rightComposed) of
        (Just leftValue, Just rightValue) -> deltaMorphismEqual leftValue rightValue
        (Nothing, Nothing) -> True
        _ -> False

normalizationRoundtripHolds :: DeltaMorphism -> Bool
normalizationRoundtripHolds morphism =
  case normalizeDeltaMorphism morphism >>= denormalizeDeltaNormalForm of
    Nothing -> False
    Just reconstructed -> deltaMorphismEqual morphism reconstructed


cofaceAt :: forall n. KnownNat n => Dimension n -> Natural -> Maybe DeltaMorphism
cofaceAt _ faceIndex =
  cofaceMorphism . CofaceMap <$> mkFinOffset @n @2 (Dimension @n) faceIndex

codegeneracyAt :: forall n. KnownNat n => Dimension n -> Natural -> Maybe DeltaMorphism
codegeneracyAt _ degeneracyIndex =
  codegeneracyMorphism . CodegeneracyMap <$> mkFinOffset @n @1 (Dimension @n) degeneracyIndex

composeMaybeDelta :: Maybe DeltaMorphism -> Maybe DeltaMorphism -> Maybe DeltaMorphism
composeMaybeDelta maybeOuter maybeInner = do
  outer <- maybeOuter
  inner <- maybeInner
  composeDeltaMorphism outer inner

assertDeltaCompositionEqual :: String -> Maybe DeltaMorphism -> Maybe DeltaMorphism -> IO ()
assertDeltaCompositionEqual label left right =
  assertEqual label left right

cofaceExample :: Maybe DeltaMorphism
cofaceExample =
  cofaceAt (Dimension @2) 1

codegeneracyExample :: Maybe DeltaMorphism
codegeneracyExample =
  codegeneracyAt (Dimension @2) 1

tests :: TestTree
tests =
  testGroup
    "Delta"
    [ testCase "coface generator maps into the next simplex dimension" $
        case cofaceExample of
          Nothing -> assertBool "expected coface morphism" False
          Just morphism -> do
            assertEqual "coface domain" 2 (deltaDomainDimension morphism)
            assertEqual "coface codomain" 3 (deltaCodomainDimension morphism)
            assertEqual "coface map" [0, 2, 3] (deltaMapValues morphism),
      testCase "codegeneracy generator collapses adjacent index" $
        case codegeneracyExample of
          Nothing -> assertBool "expected codegeneracy morphism" False
          Just morphism -> do
            assertEqual "codegeneracy domain" 3 (deltaDomainDimension morphism)
            assertEqual "codegeneracy codomain" 2 (deltaCodomainDimension morphism)
            assertEqual "codegeneracy map" [0, 1, 1, 2] (deltaMapValues morphism),
      testCase "coface generators satisfy the coface/coface identity" $
        assertDeltaCompositionEqual
          "δ₂δ₀ = δ₀δ₁"
          (composeMaybeDelta (cofaceAt (Dimension @2) 2) (cofaceAt (Dimension @1) 0))
          (composeMaybeDelta (cofaceAt (Dimension @2) 0) (cofaceAt (Dimension @1) 1)),
      testCase "codegeneracy generators satisfy the codegeneracy/codegeneracy identity" $
        assertDeltaCompositionEqual
          "σ₀σ₀ = σ₀σ₁"
          (composeMaybeDelta (codegeneracyAt (Dimension @1) 0) (codegeneracyAt (Dimension @2) 0))
          (composeMaybeDelta (codegeneracyAt (Dimension @1) 0) (codegeneracyAt (Dimension @2) 1)),
      testCase "mixed generators satisfy the left relation" $
        assertDeltaCompositionEqual
          "σ₁δ₀ = δ₀σ₀"
          (composeMaybeDelta (codegeneracyAt (Dimension @2) 1) (cofaceAt (Dimension @2) 0))
          (composeMaybeDelta (cofaceAt (Dimension @1) 0) (codegeneracyAt (Dimension @1) 0)),
      testCase "mixed generators collapse matching adjacent faces to identity" $ do
        assertDeltaCompositionEqual
          "σ₁δ₁ = id"
          (composeMaybeDelta (codegeneracyAt (Dimension @2) 1) (cofaceAt (Dimension @2) 1))
          (Just (deltaIdentity 2))
        assertDeltaCompositionEqual
          "σ₁δ₂ = id"
          (composeMaybeDelta (codegeneracyAt (Dimension @2) 1) (cofaceAt (Dimension @2) 2))
          (Just (deltaIdentity 2)),
      testCase "mixed generators satisfy the right relation" $
        assertDeltaCompositionEqual
          "σ₀δ₂ = δ₁σ₀"
          (composeMaybeDelta (codegeneracyAt (Dimension @2) 0) (cofaceAt (Dimension @2) 2))
          (composeMaybeDelta (cofaceAt (Dimension @1) 1) (codegeneracyAt (Dimension @1) 0)),
      testCase "mkDeltaMorphism rejects invalid length" $
        assertEqual "invalid length rejected" Nothing (mkDeltaMorphism 2 3 [0, 1]),
      testCase "mkDeltaMorphism rejects out-of-bounds values" $
        assertEqual "out-of-bounds value rejected" Nothing (mkDeltaMorphism 2 1 [0, 1, 2]),
      testCase "mkDeltaMorphism rejects non-monotone values" $
        assertEqual "non-monotone value rejected" Nothing (mkDeltaMorphism 2 3 [0, 2, 1]),
      testCase "degeneracy indices retain run positions for arbitrary rows" $
        assertEqual "run positions" [0, 2] (surjectionDegeneracyIndices [0, 0, 1, 0, 0]),
      testCase "injection complements ignore input order and duplicates" $
        assertEqual "missing indices" [0, 2, 4] (injectionMissingIndices 4 [3, 1, 1]),
      QC.testProperty "identity law for Delta morphisms" (QC.withNumTests 400 (QC.forAll genDeltaMorphism identityLawHolds)),
      QC.testProperty "associativity law for Delta morphism composition" (QC.withNumTests 400 (QC.forAll genComposableTriple associativityLawHolds)),
      QC.testProperty "normalization roundtrip preserves Delta morphism" (QC.withNumTests 400 (QC.forAll genDeltaMorphism normalizationRoundtripHolds))
    ]
