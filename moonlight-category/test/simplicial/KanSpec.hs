module KanSpec
  ( tests,
  )
where

import Data.List (genericSplitAt)
import Moonlight.Category.Simplicial
  ( Horn,
    HornError (..),
    HornFrameError (..),
    HornIndexError (..),
    InnerHornError (..),
    hornToIndexedHorn,
    mkHorn,
    mkHornFrame,
    mkInnerHorn,
  )
import Numeric.Natural (Natural)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

removeAt :: Natural -> [a] -> Maybe [a]
removeAt targetIndex values =
  case genericSplitAt targetIndex values of
    (_, []) -> Nothing
    (prefix, _ : suffix) -> Just (prefix <> suffix)

listFaceAt :: Natural -> Natural -> [Natural] -> Maybe [Natural]
listFaceAt _ = removeAt

undefinedFaceAt :: Natural -> Natural -> [Natural] -> Maybe [Natural]
undefinedFaceAt _ _ _ = Nothing

listSimplexDimension :: [Natural] -> Natural
listSimplexDimension values =
  case values of
    [] -> 0
    _ : _ -> fromIntegral (length values - 1)

validInnerHorn :: Either (HornError [Natural]) (Horn [Natural])
validInnerHorn =
  mkHorn listSimplexDimension listFaceAt 2 1 [(0, [5, 6]), (2, [7, 5])]

validOuterHorn :: Either (HornError [Natural]) (Horn [Natural])
validOuterHorn =
  mkHorn listSimplexDimension listFaceAt 2 0 [(1, [0, 0]), (2, [0, 0])]

tests :: TestTree
tests =
  testGroup
    "Kan"
    [ testCase "mkHornFrame rejects dimension zero" $
        assertEqual
          "dimension zero"
          (Left HornDimensionZero)
          (() <$ mkHornFrame 0 0 ([] :: [(Natural, ())])),
      testCase "mkHornFrame rejects out-of-bounds missing face" $
        assertEqual
          "missing face out of bounds"
          (Left (HornMissingFaceOutOfBounds 2 3))
          (() <$ mkHornFrame 2 3 ([] :: [(Natural, ())])),
      testCase "mkHornFrame rejects duplicate supplied faces" $
        assertEqual
          "duplicate face"
          (Left (HornDuplicateFace 0))
          (() <$ mkHornFrame 2 1 [(0, ()), (0, ()), (2, ())]),
      testCase "mkHornFrame rejects unexpected face indices" $
        assertEqual
          "unexpected face"
          (Left (HornUnexpectedFace 2 3))
          (() <$ mkHornFrame 2 1 [(0, ()), (2, ()), (3, ())]),
      testCase "mkHornFrame rejects the supplied missing face" $
        assertEqual
          "supplied missing face"
          (Left (HornSuppliedMissingFace 1))
          (() <$ mkHornFrame 2 1 [(0, ()), (1, ()), (2, ())]),
      testCase "mkHornFrame reports missing required faces" $
        assertEqual
          "missing required face"
          (Left (HornMissingRequiredFaces [2]))
          (() <$ mkHornFrame 2 1 [(0, ())]),
      testCase "mkHorn rejects undefined overlaps" $
        assertEqual
          "overlap undefined"
          (Left (HornOverlapUndefined 0 2 Nothing Nothing))
          (() <$ mkHorn listSimplexDimension undefinedFaceAt 2 1 [(0, [0, 1]), (2, [1, 2])]),
      testCase "mkHorn rejects mismatched overlaps" $
        assertEqual
          "overlap mismatch"
          (Left (HornOverlapMismatch 0 2 [2] [0]))
          (() <$ mkHorn listSimplexDimension listFaceAt 2 1 [(0, [0, 1]), (2, [1, 2])]),
      testCase "mkHorn rejects faces in the wrong dimension" $
        assertEqual
          "face dimension mismatch"
          (Left (HornFaceDimensionMismatch 0 1 2))
          (() <$ mkHorn listSimplexDimension listFaceAt 2 1 [(0, [0, 1, 2]), (2, [1, 2])]),
      testCase "mkInnerHorn accepts compatible inner horns" $
        case validInnerHorn of
          Left obstruction -> assertEqual "expected valid horn" Nothing (Just obstruction)
          Right hornValue -> assertEqual "inner horn" True (either (const False) (const True) (mkInnerHorn hornValue)),
      testCase "mkInnerHorn rejects outer horns" $
        case validOuterHorn of
          Left obstruction -> assertEqual "expected valid outer horn" Nothing (Just obstruction)
          Right hornValue ->
            assertEqual
              "outer horn rejected"
              (Left (HornNotInner 2 0))
              (() <$ mkInnerHorn hornValue),
      testCase "hornToIndexedHorn rejects dimension mismatch" $
        case validInnerHorn of
          Left obstruction -> assertEqual "expected valid horn" Nothing (Just obstruction)
          Right hornValue ->
            assertEqual
              "dimension mismatch"
              (Left (HornIndexDimensionMismatch 3 2))
              (() <$ hornToIndexedHorn @2 hornValue)
    ]
