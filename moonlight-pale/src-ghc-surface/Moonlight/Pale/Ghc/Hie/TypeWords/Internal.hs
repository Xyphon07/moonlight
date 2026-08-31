{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Pale.Ghc.Hie.TypeWords.Internal
  ( TypeWords,
    TypeWord (..),
    TypeWordOpcode (..),
    TypeArgumentVisibility (..),
    TypeVariableFlavor (..),
    TypeWireFailure (..),
    typeWords,
    trustedTypeWords,
    typeWordsList,
    outputTypeWords,
    stringTypeWords,
  )
where

import Data.Kind (Type)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Word (Word64)
import GHC.Utils.Outputable (Outputable, ppr, showSDocUnsafe)
import Numeric.Natural (Natural)

type TypeWords :: Type
newtype TypeWords = TypeWords (Vector TypeWord)
  deriving stock (Eq, Ord, Show)

type TypeWord :: Type
data TypeWord
  = TypeOpcode !TypeWordOpcode
  | TypeDefinitionCount !Natural
  | TypeDefinitionId !Natural
  | TypeRootReference !Natural
  | TypeNodeReference !Natural
  | TypeArgumentCount !Natural
  | TypeArgumentVisibilityWord !TypeArgumentVisibility
  | TypeVariableFlavorWord !TypeVariableFlavor
  | TypeBinderReference !Natural
  | TypeNameIdentity !Word64
  | TypeOutputText !String
  deriving stock (Eq, Ord, Show)

type TypeWordOpcode :: Type
data TypeWordOpcode
  = TypeGraphOpcode
  | TypeDefinitionOpcode
  | TypeAppOpcode
  | TypeFunOpcode
  | TypeQualOpcode
  | TypeForAllOpcode
  | TypeVariableOpcode
  | TypeCastOpcode
  | TypeCoercionOpcode
  | TypeTyConAppOpcode
  | TypeLiteralOpcode
  deriving stock (Eq, Ord, Show, Enum, Bounded)

type TypeArgumentVisibility :: Type
data TypeArgumentVisibility
  = TypeArgumentHidden
  | TypeArgumentVisible
  deriving stock (Eq, Ord, Show, Enum, Bounded)

type TypeVariableFlavor :: Type
data TypeVariableFlavor
  = TypeFreeVariableFlavor
  | TypeBoundVariableFlavor
  deriving stock (Eq, Ord, Show, Enum, Bounded)

type TypeWireFailure :: Type
data TypeWireFailure
  = TypeNaturalExceedsWord64 !TypeWord
  deriving stock (Eq, Ord, Show)

typeWords :: [TypeWord] -> Either TypeWireFailure TypeWords
typeWords wordsValue =
  TypeWords (Vector.fromList wordsValue)
    <$ traverse validateTypeWord wordsValue

trustedTypeWords :: [TypeWord] -> TypeWords
trustedTypeWords =
  TypeWords . Vector.fromList

typeWordsList :: TypeWords -> [Word64]
typeWordsList (TypeWords wordsValue) =
  foldMap renderTypeWord wordsValue

outputTypeWords :: Outputable value => value -> [TypeWord]
outputTypeWords =
  stringTypeWords . outputString

stringTypeWords :: String -> [TypeWord]
stringTypeWords textValue =
  [TypeOutputText textValue]

validateTypeWord :: TypeWord -> Either TypeWireFailure ()
validateTypeWord wordValue =
  case wordNatural wordValue of
    Nothing ->
      Right ()
    Just naturalValue
      | naturalValue <= fromIntegral (maxBound :: Word64) ->
          Right ()
      | otherwise ->
          Left (TypeNaturalExceedsWord64 wordValue)

wordNatural :: TypeWord -> Maybe Natural
wordNatural = \case
  TypeDefinitionCount value ->
    Just value
  TypeDefinitionId value ->
    Just value
  TypeRootReference value ->
    Just value
  TypeNodeReference value ->
    Just value
  TypeArgumentCount value ->
    Just value
  TypeBinderReference value ->
    Just value
  _ ->
    Nothing

renderTypeWord :: TypeWord -> [Word64]
renderTypeWord = \case
  TypeOpcode opcode ->
    [boundedTagWord opcode]
  TypeDefinitionCount count ->
    [boundedTagWord TypeGraphOpcode, naturalWord count]
  TypeDefinitionId definitionId ->
    [boundedTagWord TypeDefinitionOpcode, naturalWord definitionId]
  TypeRootReference rootId ->
    [naturalWord rootId]
  TypeNodeReference nodeId ->
    [naturalWord nodeId]
  TypeArgumentCount count ->
    [naturalWord count]
  TypeArgumentVisibilityWord visibility ->
    [boundedTagWord visibility]
  TypeVariableFlavorWord flavor ->
    [boundedTagWord flavor]
  TypeBinderReference binderId ->
    [naturalWord binderId]
  TypeNameIdentity uniqueWord ->
    [uniqueWord]
  TypeOutputText textValue ->
    fromIntegral (length textValue) : fmap (fromIntegral . fromEnum) textValue

boundedTagWord :: Enum tag => tag -> Word64
boundedTagWord tagValue =
  fromIntegral (fromEnum tagValue + 1)

naturalWord :: Natural -> Word64
naturalWord =
  fromIntegral

outputString :: Outputable value => value -> String
outputString =
  showSDocUnsafe . ppr
