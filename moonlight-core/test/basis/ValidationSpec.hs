{-# LANGUAGE DerivingStrategies #-}

module ValidationSpec (tests) where

import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Core (IsLawName (..), constructorLawName)
import LawProperty (lawProperty)
import Moonlight.Core
  ( Validation (..),
    collectEither,
    eitherToValidation,
    mapValidationError,
    validationToEither,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
  ( Arbitrary (..),
    Gen,
    Property,
    chooseInt,
    listOf,
    (===),
    (.&&.),
  )

newtype ErrorBag = ErrorBag (Set Int)
  deriving stock (Eq, Show)

instance Semigroup ErrorBag where
  ErrorBag left <> ErrorBag right =
    ErrorBag (Set.union left right)

instance Arbitrary ErrorBag where
  arbitrary =
    ErrorBag . Set.fromList <$> listOf validationErrorCode

data ValidationLaw
  = ValidationEitherRoundTrip
  | ValidationErrorAccumulationAssociative
  | ValidationErrorAccumulationCommutativeForCommutativeErrors
  | ValidationApplicativeNeverDropsErrors
  | ValidationCollectEitherNeverDropsErrors
  | ValidationMapErrorFunctor
  deriving stock (Bounded, Enum, Eq, Ord, Show)

instance IsLawName ValidationLaw where
  lawNameText =
    constructorLawName . show

tests :: TestTree
tests =
  testGroup
    "Validation"
    [ lawProperty ValidationEitherRoundTrip propEitherRoundTrip,
      lawProperty ValidationErrorAccumulationAssociative propErrorAccumulationAssociative,
      lawProperty ValidationErrorAccumulationCommutativeForCommutativeErrors propErrorAccumulationCommutative,
      lawProperty ValidationApplicativeNeverDropsErrors propApplicativeNeverDropsErrors,
      lawProperty ValidationCollectEitherNeverDropsErrors propCollectEitherNeverDropsErrors,
      lawProperty ValidationMapErrorFunctor propMapErrorFunctor
    ]

validationErrorCode :: Gen Int
validationErrorCode =
  chooseInt (-64, 64)

propEitherRoundTrip :: Either [Int] Int -> Property
propEitherRoundTrip eitherValue =
  validationToEither validationValue === eitherValue
    .&&. eitherToValidation (validationToEither validationValue) === validationValue
  where
    validationValue =
      eitherToValidation eitherValue

propErrorAccumulationAssociative :: Int -> Int -> Int -> Property
propErrorAccumulationAssociative left middle right =
  leftGrouped === (Invalid [left, middle, right] :: Validation [Int] ((Int, Int), Int))
    .&&. rightGrouped === (Invalid [left, middle, right] :: Validation [Int] (Int, (Int, Int)))
  where
    leftGrouped =
      liftA2
        (,)
        (liftA2 (,) (Invalid [left] :: Validation [Int] Int) (Invalid [middle] :: Validation [Int] Int))
        (Invalid [right] :: Validation [Int] Int)
    rightGrouped =
      liftA2
        (,)
        (Invalid [left] :: Validation [Int] Int)
        (liftA2 (,) (Invalid [middle] :: Validation [Int] Int) (Invalid [right] :: Validation [Int] Int))

propErrorAccumulationCommutative :: ErrorBag -> ErrorBag -> Property
propErrorAccumulationCommutative left right =
  leftFirst === rightFirst
  where
    leftFirst =
      (Invalid left :: Validation ErrorBag (Int -> Int))
        <*> (Invalid right :: Validation ErrorBag Int)
    rightFirst =
      (Invalid right :: Validation ErrorBag (Int -> Int))
        <*> (Invalid left :: Validation ErrorBag Int)

propApplicativeNeverDropsErrors :: Int -> Int -> Property
propApplicativeNeverDropsErrors left right =
  ((Invalid [left] :: Validation [Int] (Int -> Int)) <*> (Invalid [right] :: Validation [Int] Int))
    === (Invalid [left, right] :: Validation [Int] Int)

propCollectEitherNeverDropsErrors :: Int -> Int -> [Int] -> Property
propCollectEitherNeverDropsErrors left right validValues =
  collectEither eitherValues === Left [left, right]
  where
    eitherValues =
      Left [left] : (Right <$> validValues) <> [Left [right]]

propMapErrorFunctor :: Either Int String -> Property
propMapErrorFunctor eitherValue =
  mapValidationError id validationValue === validationValue
    .&&. mapValidationError ((+ 1) . (* 2)) validationValue
      === mapValidationError (+ 1) (mapValidationError (* 2) validationValue)
  where
    validationValue =
      eitherToValidation eitherValue
