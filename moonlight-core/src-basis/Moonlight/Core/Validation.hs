-- | Error-accumulating counterpart of 'Either': 'collectEither' gathers every failure instead of stopping at the first.
module Moonlight.Core.Validation
  ( Validation (..),
    eitherToValidation,
    validationToEither,
    mapValidationError,
    collectEither,
    validationFromErrors,
  )
where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Prelude
  ( Applicative (..),
    Either (..),
    Eq,
    Functor (..),
    Semigroup ((<>)),
    Show,
    Traversable (traverse),
    maybe,
  )

type Validation :: Type -> Type -> Type
data Validation err value
  = Invalid err
  | Valid value
  deriving stock (Eq, Show)

instance Functor (Validation err) where
  fmap mapper validationValue =
    case validationValue of
      Invalid err -> Invalid err
      Valid value -> Valid (mapper value)

instance Semigroup err => Applicative (Validation err) where
  pure = Valid
  validationFunction <*> validationValue =
    case (validationFunction, validationValue) of
      (Valid mapper, Valid value) -> Valid (mapper value)
      (Invalid leftErr, Invalid rightErr) -> Invalid (leftErr <> rightErr)
      (Invalid err, Valid _) -> Invalid err
      (Valid _, Invalid err) -> Invalid err

eitherToValidation :: Either err value -> Validation err value
eitherToValidation eitherValue =
  case eitherValue of
    Left err -> Invalid err
    Right value -> Valid value

validationToEither :: Validation err value -> Either err value
validationToEither validationValue =
  case validationValue of
    Invalid err -> Left err
    Valid value -> Right value

mapValidationError ::
  (leftErr -> rightErr) ->
  Validation leftErr value ->
  Validation rightErr value
mapValidationError transform validationValue =
  case validationValue of
    Invalid err -> Invalid (transform err)
    Valid value -> Valid value

collectEither :: Semigroup err => [Either err value] -> Either err [value]
collectEither eitherValues =
  validationToEither
    (traverse eitherToValidation eitherValues)

validationFromErrors :: [error] -> Validation (NonEmpty error) ()
validationFromErrors errors =
  maybe (Valid ()) Invalid (nonEmpty errors)
