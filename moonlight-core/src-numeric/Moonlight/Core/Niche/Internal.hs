module Moonlight.Core.Niche.Internal
  ( NicheValidationError (..),
    TopoSample,
    mkTopoSample,
    flatTopoSample,
    topoSlope,
    topoAspect,
    topoCurvature,
    StressorId,
    mkStressorId,
    unsafeTrustStressorId,
    renderStressorId,
    defaultStressorId,
    ActiveStressor,
    mkActiveStressor,
    activeStressorId,
    activeStressorIntensity,
    ActiveStressorSet,
    emptyActiveStressorSet,
    activeStressorSetFromList,
    activeStressorSetEntries,
    activeStressorSetTopEntries,
    ContextSignature,
    emptyContextSignature,
    mkContextSignature,
    contextSignatureBins,
  )
where

import Data.Bifunctor (first)
import Data.Kind (Type)
import Data.List (genericTake, sortBy)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..), comparing)
import Data.Text (Text, pack)
import Data.Text qualified as Text
import Data.Word (Word32)
import Moonlight.Core.Canon (canonicalize)
import Moonlight.Core.Error (MoonlightError)
import Moonlight.Core.Identifier
  ( IdentifierToken,
    mkScopedIdentifier,
    renderIdentifierToken,
  )
import Moonlight.Internal.Unsound (TrustJustification, unsafelyTrustIdentifierToken)
import Numeric.Natural (Natural)
import Prelude
  ( Bool (..),
    Double,
    Either (..),
    Eq,
    Maybe (..),
    Ord,
    Show,
    compare,
    map,
    max,
    otherwise,
    pure,
    uncurry,
    (.),
    (==),
    (>),
    (>=),
  )

type NicheValidationError :: Type
data NicheValidationError
  = NicheCanonicalScalarRejected !MoonlightError
  | NegativeTopoSlope !Double
  | NonPositiveStressorIntensity !Double
  deriving stock (Eq, Show)

type TopoSample :: Type
data TopoSample = TopoSample
  { topoSlope :: Double,
    topoAspect :: Double,
    topoCurvature :: Double
  }
  deriving stock (Eq, Show)

mkTopoSample :: Double -> Double -> Double -> Either NicheValidationError TopoSample
mkTopoSample slopeValue aspectValue curvatureValue = do
  canonicalSlope <- canonicalizeScalar slopeValue
  canonicalAspect <- canonicalizeScalar aspectValue
  canonicalCurvature <- canonicalizeScalar curvatureValue
  if canonicalSlope >= 0.0
    then pure (TopoSample canonicalSlope canonicalAspect canonicalCurvature)
    else Left (NegativeTopoSlope canonicalSlope)

flatTopoSample :: TopoSample
flatTopoSample = TopoSample 0.0 0.0 0.0

type StressorIdNamespace :: Type
data StressorIdNamespace

type StressorId :: Type
data StressorId
  = DefaultStressorId
  | StressorId !(IdentifierToken StressorIdNamespace)
  deriving stock (Show)

instance Eq StressorId where
  left == right =
    case (left, right) of
      (DefaultStressorId, DefaultStressorId) ->
        True
      (StressorId leftToken, StressorId rightToken) ->
        leftToken == rightToken
      _ ->
        renderStressorId left == renderStressorId right

instance Ord StressorId where
  compare left right =
    case (left, right) of
      (DefaultStressorId, DefaultStressorId) ->
        compare (renderStressorId left) (renderStressorId right)
      (StressorId leftToken, StressorId rightToken) ->
        compare leftToken rightToken
      _ ->
        compare (renderStressorId left) (renderStressorId right)

mkStressorId :: Text -> Maybe StressorId
mkStressorId rawInput
  | Text.strip rawInput == defaultStressorIdText =
      Just DefaultStressorId
  | otherwise =
      mkScopedIdentifier StressorId rawInput

unsafeTrustStressorId :: TrustJustification -> Text -> StressorId
unsafeTrustStressorId justification rawInput
  | Text.strip rawInput == defaultStressorIdText =
      DefaultStressorId
  | otherwise =
      StressorId (unsafelyTrustIdentifierToken justification rawInput)

renderStressorId :: StressorId -> Text
renderStressorId stressorId =
  case stressorId of
    DefaultStressorId ->
      defaultStressorIdText
    StressorId identifierToken ->
      renderIdentifierToken identifierToken

defaultStressorId :: StressorId
defaultStressorId =
  defaultStressorIdFrom (mkStressorId defaultStressorIdText)

defaultStressorIdText :: Text
defaultStressorIdText =
  pack "default"

defaultStressorIdFrom :: Maybe StressorId -> StressorId
defaultStressorIdFrom maybeStressorId =
  case maybeStressorId of
    Just stressorId -> stressorId
    Nothing -> DefaultStressorId

type ActiveStressor :: Type
data ActiveStressor = ActiveStressor
  { activeStressorId :: StressorId,
    activeStressorIntensity :: Double
  }
  deriving stock (Eq, Show)

mkActiveStressor :: StressorId -> Double -> Either NicheValidationError ActiveStressor
mkActiveStressor stressorId intensityValue = do
  canonicalIntensity <- canonicalizeScalar intensityValue
  if canonicalIntensity > 0.0
    then pure (ActiveStressor stressorId canonicalIntensity)
    else Left (NonPositiveStressorIntensity canonicalIntensity)

type ActiveStressorSet :: Type
newtype ActiveStressorSet = ActiveStressorSet [ActiveStressor]
  deriving stock (Eq, Show)

emptyActiveStressorSet :: ActiveStressorSet
emptyActiveStressorSet = ActiveStressorSet []

activeStressorSetFromList :: [ActiveStressor] -> ActiveStressorSet
activeStressorSetFromList stressors =
  ActiveStressorSet
    ( sortBy canonicalOrder
        ( map (uncurry ActiveStressor)
            (Map.toList (Map.fromListWith max (map stressorEntry stressors)))
        )
    )
  where
    stressorEntry stressor =
      (activeStressorId stressor, activeStressorIntensity stressor)
    canonicalOrder =
      comparing
        (\stressor -> (Down (activeStressorIntensity stressor), activeStressorId stressor))

activeStressorSetEntries :: ActiveStressorSet -> [ActiveStressor]
activeStressorSetEntries (ActiveStressorSet stressors) = stressors

activeStressorSetTopEntries :: Natural -> ActiveStressorSet -> [ActiveStressor]
activeStressorSetTopEntries limit =
  genericTake limit . activeStressorSetEntries

type ContextSignature :: Type
newtype ContextSignature = ContextSignature [Word32]
  deriving stock (Eq, Ord, Show)

emptyContextSignature :: ContextSignature
emptyContextSignature = ContextSignature []

mkContextSignature :: [Word32] -> ContextSignature
mkContextSignature = ContextSignature

contextSignatureBins :: ContextSignature -> [Word32]
contextSignatureBins (ContextSignature bins) = bins

canonicalizeScalar :: Double -> Either NicheValidationError Double
canonicalizeScalar = first NicheCanonicalScalarRejected . canonicalize
