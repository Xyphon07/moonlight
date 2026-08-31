{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Pale.Ghc.Expr.Syntax
  ( HsVarRef (..),
    BinderAnn (..),
    HsOpaqueTag (..),
    HsPatOpaqueTag (..),
    HsRecPatFieldValue (..),
    HsRecPatItem (..),
    HsPatF (..),
    patBinders,
    traversePatBinders,
    LetRecursion (..),
    BindingComponent (..),
    BindingComponentRecursion (..),
    FixityAssociativity (..),
    FixityDeclaration (..),
    TypeSignature (..),
    ExactIntegral,
    exactIntegralSource,
    exactIntegralNegative,
    exactIntegralValue,
    exactIntegralToInteger,
    exactIntegralFromInteger,
    ExactFractional,
    exactFractionalSource,
    exactFractionalNegative,
    exactFractionalSignificand,
    exactFractionalExponent,
    exactFractionalBase,
    exactFractionalToRational,
    exactFractionalFromRational,
    NormalizedLit (..),
    normalizeHsLit,
    NormalizedOverLit (..),
    normalizeHsOverLit,
    NormalizedFieldLabel (..),
    normalizeFieldLabel,
    NormalizedTypeText (..),
    NormalizedArithSeq (..),
    TupleBoxity (..),
    TupleSlot (..),
    SourceRegion (..),
    SourceCharRange,
    SourceEndConvention (..),
    SourceRangeFailure (..),
    sourceRegionFromSrcSpan,
    sourceRegionFromRealSrcSpan,
    sourceCharRangeStart,
    sourceCharRangeEnd,
    sourceCharRangeFromOffsets,
    sourceRegionCharRange,
    sourceRegionCharRangeWith,
    sourceCharRangeRegion,
    sourceCharRangeRegionWith,
    sourceCharRangeText,
    HsExprF (..),
    HsStmtF (..),
    HsGuardStmtF (..),
    GuardedAltF (..),
    Expr (..),
    eraseExpr,
    HsExprTag (..),
    TagSignature (..),
    tagSignatureFromTag,
    tagSignatureMember,
  )
where

import Data.Bits (bit, testBit, (.|.))
import Data.ByteString (ByteString)
import Data.Foldable (toList)
import Data.Functor (void)
import Data.Functor.Classes (Eq1 (..), Show1 (..))
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Word (Word64)
import GHC.Data.FastString (unpackFS)
import GHC.Hs (GhcPs, HsLit (..), HsOverLit (..), OverLitVal (..))
import GHC.Types.FieldLabel
  ( DuplicateRecordFields (..),
    FieldLabel,
    FieldSelectors (..),
    flHasDuplicateRecordFields,
    flHasFieldSelector,
    flSelector,
  )
import GHC.Types.Name (nameOccName)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (RdrName, rdrNameOcc)
import GHC.Types.SourceText (FractionalExponentBase (..), FractionalLit (..), IntegralLit (..), SourceText (..))
import GHC.Types.SrcLoc
  ( RealSrcSpan,
    SrcSpan (..),
    srcSpanEndCol,
    srcSpanEndLine,
    srcSpanStartCol,
    srcSpanStartLine,
  )
import Moonlight.Core (BinderId, HasConstructorTag (..), Pattern (..), ZipMatch (..), zipSameNodeShape)
import Moonlight.Pale.Ghc.Expr.Opaque (HsOpaqueTag (..), HsPatOpaqueTag (..))
import Moonlight.Pale.Ghc.Expr.Scope (FreeScopeSummary, ScopeId)

type HsVarRef :: Type
data HsVarRef
  = GlobalName !RdrName
  | LocalName !BinderAnn
  deriving stock (Eq, Ord)

type BinderAnn :: Type
data BinderAnn = BinderAnn
  { baId :: !BinderId,
    baName :: !RdrName
  }
  deriving stock (Eq, Ord)

instance Show HsVarRef where
  show = \case
    GlobalName rdrName -> "GlobalName " <> occNameString (rdrNameOcc rdrName)
    LocalName binderAnn -> "LocalName " <> show binderAnn

instance Show BinderAnn where
  show binderAnn =
    "BinderAnn { baId = " <> show (baId binderAnn) <> ", baName = " <> occNameString (rdrNameOcc (baName binderAnn)) <> " }"

type HsRecPatFieldValue :: Type
data HsRecPatFieldValue
  = HsRecPatExplicit !HsPatF
  | HsRecPatPun !BinderAnn
  deriving stock (Eq, Ord)

instance Show HsRecPatFieldValue where
  show = \case
    HsRecPatExplicit fieldPattern ->
      "HsRecPatExplicit (" <> show fieldPattern <> ")"
    HsRecPatPun binderAnn ->
      "HsRecPatPun (" <> show binderAnn <> ")"

type HsRecPatItem :: Type
data HsRecPatItem
  = HsRecPatField !RdrName !HsRecPatFieldValue
  | HsRecPatWildcard !SourceRegion ![BinderAnn]
  deriving stock (Eq, Ord)

instance Show HsRecPatItem where
  show = \case
    HsRecPatField fieldName fieldValue ->
      "HsRecPatField "
        <> occNameString (rdrNameOcc fieldName)
        <> " ("
        <> show fieldValue
        <> ")"
    HsRecPatWildcard wildcardRegion wildcardBinders ->
      "HsRecPatWildcard "
        <> show wildcardRegion
        <> " "
        <> show wildcardBinders

type HsPatF :: Type
data HsPatF
  = PVarP !BinderAnn
  | PWildP
  | PConP !RdrName ![HsPatF]
  | PTupleP !TupleBoxity ![HsPatF]
  | PListP ![HsPatF]
  | PLitP !NormalizedLit
  | POverLitP !NormalizedOverLit
  | PAsP !BinderAnn !HsPatF
  | PBangP !HsPatF
  | PLazyP !HsPatF
  | PParP !HsPatF
  | PRecP !RdrName ![HsRecPatItem]
  deriving stock (Eq, Ord)

instance Show HsPatF where
  show = \case
    PVarP binderAnn -> "PVarP (" <> show binderAnn <> ")"
    PWildP -> "PWildP"
    PConP conName subPatterns -> "PConP " <> occNameString (rdrNameOcc conName) <> " " <> show subPatterns
    PTupleP boxity subPatterns -> "PTupleP " <> show boxity <> " " <> show subPatterns
    PListP subPatterns -> "PListP " <> show subPatterns
    PLitP literalValue -> "PLitP (" <> show literalValue <> ")"
    POverLitP literalValue -> "POverLitP (" <> show literalValue <> ")"
    PAsP binderAnn subPattern -> "PAsP (" <> show binderAnn <> ") (" <> show subPattern <> ")"
    PBangP subPattern -> "PBangP (" <> show subPattern <> ")"
    PLazyP subPattern -> "PLazyP (" <> show subPattern <> ")"
    PParP subPattern -> "PParP (" <> show subPattern <> ")"
    PRecP conName recordItems -> "PRecP " <> occNameString (rdrNameOcc conName) <> " " <> show recordItems

patBinders :: HsPatF -> [BinderAnn]
patBinders = \case
  PVarP binderAnn -> [binderAnn]
  PWildP -> []
  PConP _ subPatterns -> foldMap patBinders subPatterns
  PTupleP _ subPatterns -> foldMap patBinders subPatterns
  PListP subPatterns -> foldMap patBinders subPatterns
  PLitP _ -> []
  POverLitP _ -> []
  PAsP binderAnn subPattern -> binderAnn : patBinders subPattern
  PBangP subPattern -> patBinders subPattern
  PLazyP subPattern -> patBinders subPattern
  PParP subPattern -> patBinders subPattern
  PRecP _ recordItems -> foldMap recordItemBinders recordItems

traversePatBinders :: Applicative f => (BinderAnn -> f BinderAnn) -> HsPatF -> f HsPatF
traversePatBinders onBinder = go
  where
    go = \case
      PVarP binderAnn -> PVarP <$> onBinder binderAnn
      PWildP -> pure PWildP
      PConP conName subPatterns -> PConP conName <$> traverse go subPatterns
      PTupleP boxity subPatterns -> PTupleP boxity <$> traverse go subPatterns
      PListP subPatterns -> PListP <$> traverse go subPatterns
      PLitP literalValue -> pure (PLitP literalValue)
      POverLitP literalValue -> pure (POverLitP literalValue)
      PAsP binderAnn subPattern -> PAsP <$> onBinder binderAnn <*> go subPattern
      PBangP subPattern -> PBangP <$> go subPattern
      PLazyP subPattern -> PLazyP <$> go subPattern
      PParP subPattern -> PParP <$> go subPattern
      PRecP conName recordItems ->
        PRecP conName <$> traverse (traverseRecordItemBinders onBinder) recordItems

recordItemBinders :: HsRecPatItem -> [BinderAnn]
recordItemBinders = \case
  HsRecPatField _ (HsRecPatExplicit fieldPattern) ->
    patBinders fieldPattern
  HsRecPatField _ (HsRecPatPun binderAnn) ->
    [binderAnn]
  HsRecPatWildcard _ wildcardBinders ->
    wildcardBinders

traverseRecordItemBinders ::
  Applicative f =>
  (BinderAnn -> f BinderAnn) ->
  HsRecPatItem ->
  f HsRecPatItem
traverseRecordItemBinders onBinder = \case
  HsRecPatField fieldName (HsRecPatExplicit fieldPattern) ->
    HsRecPatField fieldName . HsRecPatExplicit
      <$> traversePatBinders onBinder fieldPattern
  HsRecPatField fieldName (HsRecPatPun binderAnn) ->
    HsRecPatField fieldName . HsRecPatPun
      <$> onBinder binderAnn
  HsRecPatWildcard wildcardRegion wildcardBinders ->
    HsRecPatWildcard wildcardRegion
      <$> traverse onBinder wildcardBinders

type LetRecursion :: Type
data LetRecursion
  = NonRecursiveBinds
  | AcyclicDependentBinds
  | RecursiveBinds
  deriving stock (Eq, Ord, Show)

type BindingComponent :: Type
data BindingComponent = BindingComponent
  { bindingComponentRows :: !(NonEmpty Int),
    bindingComponentBinders :: ![BinderId],
    bindingComponentDependencies :: ![BinderId],
    bindingComponentRecursion :: !BindingComponentRecursion
  }
  deriving stock (Eq, Ord, Show)

type BindingComponentRecursion :: Type
data BindingComponentRecursion
  = AcyclicBindingComponent
  | RecursiveBindingComponent
  deriving stock (Eq, Ord, Show)

type FixityAssociativity :: Type
data FixityAssociativity
  = FixityLeft
  | FixityRight
  | FixityNone
  deriving stock (Eq, Ord, Show)

type FixityDeclaration :: Type
data FixityDeclaration = FixityDeclaration
  { fixityAssociativity :: !FixityAssociativity,
    fixityPrecedence :: !Int,
    fixityOperators :: !(NonEmpty RdrName)
  }
  deriving stock (Eq, Ord)

instance Show FixityDeclaration where
  show declaration =
    "FixityDeclaration "
      <> show (fixityAssociativity declaration)
      <> " "
      <> show (fixityPrecedence declaration)
      <> " "
      <> show (fmap (occNameString . rdrNameOcc) (fixityOperators declaration))

type TypeSignature :: Type
data TypeSignature = TypeSignature
  { typeSignatureNames :: !(NonEmpty RdrName),
    typeSignatureType :: !NormalizedTypeText
  }
  deriving stock (Eq, Ord)

instance Show TypeSignature where
  show signature =
    "TypeSignature "
      <> show (fmap (occNameString . rdrNameOcc) (typeSignatureNames signature))
      <> " "
      <> show (typeSignatureType signature)

type NormalizedLit :: Type
data NormalizedLit
  = NormalizedChar !Char
  | NormalizedCharPrim !Char
  | NormalizedString !String
  | NormalizedMultilineString !String
  | NormalizedStringPrim !ByteString
  | NormalizedInt !ExactIntegral
  | NormalizedIntPrim !ExactIntegral
  | NormalizedWordPrim !ExactIntegral
  | NormalizedInt8Prim !ExactIntegral
  | NormalizedInt16Prim !ExactIntegral
  | NormalizedInt32Prim !ExactIntegral
  | NormalizedInt64Prim !ExactIntegral
  | NormalizedWord8Prim !ExactIntegral
  | NormalizedWord16Prim !ExactIntegral
  | NormalizedWord32Prim !ExactIntegral
  | NormalizedWord64Prim !ExactIntegral
  | NormalizedFloatPrim !ExactFractional
  | NormalizedDoublePrim !ExactFractional
  deriving stock (Eq, Ord, Show)

type NormalizedOverLit :: Type
data NormalizedOverLit
  = NormalizedIntegralOverLit !ExactIntegral
  | NormalizedFractionalOverLit !ExactFractional
  | NormalizedStringOverLit !String
  deriving stock (Eq, Ord, Show)

type NormalizedTypeText :: Type
newtype NormalizedTypeText = NormalizedTypeText
  { nttText :: String
  }
  deriving stock (Eq, Ord, Show)

type NormalizedFieldLabel :: Type
data NormalizedFieldLabel = NormalizedFieldLabel
  { nflSelector :: !String,
    nflAllowsDuplicateRecordFields :: !Bool,
    nflHasSelector :: !Bool
  }
  deriving stock (Eq, Ord, Show)

type NormalizedArithSeq :: Type -> Type
data NormalizedArithSeq r
  = ArithSeqFrom !r
  | ArithSeqFromThen !r !r
  | ArithSeqFromTo !r !r
  | ArithSeqFromThenTo !r !r !r
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

type ExactIntegral :: Type
data ExactIntegral = ExactIntegral
  { exactIntegralSource :: !(Maybe String),
    exactIntegralNegative :: !Bool,
    exactIntegralValue :: !Integer
  }
  deriving stock (Eq, Ord, Show)

type ExactFractional :: Type
data ExactFractional = ExactFractional
  { exactFractionalSource :: !(Maybe String),
    exactFractionalNegative :: !Bool,
    exactFractionalSignificand :: !Rational,
    exactFractionalExponent :: !Integer,
    exactFractionalBase :: !FractionalExponentBase
  }
  deriving stock (Eq, Ord, Show)

exactIntegralToInteger :: ExactIntegral -> Integer
exactIntegralToInteger exactValue =
  (if exactIntegralNegative exactValue then negate else id)
    (exactIntegralValue exactValue)

exactIntegralFromInteger :: Integer -> ExactIntegral
exactIntegralFromInteger value =
  ExactIntegral
    { exactIntegralSource = Nothing,
      exactIntegralNegative = value < 0,
      exactIntegralValue = abs value
    }

exactFractionalToRational :: ExactFractional -> Rational
exactFractionalToRational exactValue =
  let exponentFactor =
        case exactFractionalBase exactValue of
          Base10 -> rationalPower 10 (exactFractionalExponent exactValue)
          Base2 -> rationalPower 2 (exactFractionalExponent exactValue)
      unsignedValue =
        exactFractionalSignificand exactValue * exponentFactor
   in (if exactFractionalNegative exactValue then negate else id) unsignedValue

exactFractionalFromRational :: Rational -> ExactFractional
exactFractionalFromRational value =
  ExactFractional
    { exactFractionalSource = Nothing,
      exactFractionalNegative = value < 0,
      exactFractionalSignificand = abs value,
      exactFractionalExponent = 0,
      exactFractionalBase = Base10
    }

rationalPower :: Integer -> Integer -> Rational
rationalPower baseValue exponentValue
  | exponentValue < 0 =
      1 / fromInteger (baseValue ^ negate exponentValue)
  | otherwise =
      fromInteger (baseValue ^ exponentValue)

type TupleSlot :: Type -> Type
data TupleSlot r
  = TuplePresent !r
  | TupleMissing
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

type TupleBoxity :: Type
data TupleBoxity
  = BoxedTuple
  | UnboxedTuple
  deriving stock (Eq, Ord, Show)

type SourceRegion :: Type
data SourceRegion = SourceRegion
  { srStartLine :: !Int,
    srStartCol :: !Int,
    srEndLine :: !Int,
    srEndCol :: !Int
  }
  deriving stock (Eq, Ord, Show)

type SourceCharRange :: Type
data SourceCharRange = SourceCharRange
  { sourceCharRangeStart :: !Int,
    sourceCharRangeEnd :: !Int
  }
  deriving stock (Eq, Ord, Show)

type SourceEndConvention :: Type
data SourceEndConvention
  = SourceEndHalfOpen
  | SourceEndInclusive
  deriving stock (Eq, Ord, Show)

type SourceRangeFailure :: Type
data SourceRangeFailure
  = SourceRangePositionOutsideSource !Int !Int
  | SourceRangePositionInsideTab !Int !Int
  | SourceRangeEndsBeforeStart !SourceRegion
  | SourceRangeOffsetsInvalid !Int !Int
  | SourceRangeInvalidTabStop !Int
  | SourceRangeCarriageReturnUnsupported
  deriving stock (Eq, Ord, Show)

sourceRegionFromSrcSpan :: SrcSpan -> Maybe SourceRegion
sourceRegionFromSrcSpan = \case
  RealSrcSpan realSpan _ ->
    Just (sourceRegionFromRealSrcSpan realSpan)
  UnhelpfulSpan _ ->
    Nothing

sourceRegionFromRealSrcSpan :: RealSrcSpan -> SourceRegion
sourceRegionFromRealSrcSpan realSpan =
  SourceRegion
    { srStartLine = srcSpanStartLine realSpan,
      srStartCol = srcSpanStartCol realSpan,
      srEndLine = srcSpanEndLine realSpan,
      srEndCol = srcSpanEndCol realSpan
    }

sourceCharRangeFromOffsets :: Int -> Int -> Either SourceRangeFailure SourceCharRange
sourceCharRangeFromOffsets startOffset endOffset
  | startOffset < 0 || endOffset < startOffset =
      Left (SourceRangeOffsetsInvalid startOffset endOffset)
  | otherwise =
      Right (SourceCharRange startOffset endOffset)

sourceRegionCharRange :: String -> SourceRegion -> Either SourceRangeFailure SourceCharRange
sourceRegionCharRange = sourceRegionCharRangeWith 8 SourceEndHalfOpen

sourceRegionCharRangeWith :: Int -> SourceEndConvention -> String -> SourceRegion -> Either SourceRangeFailure SourceCharRange
sourceRegionCharRangeWith tabStop endConvention source region = do
  if tabStop > 0
    then Right ()
    else Left (SourceRangeInvalidTabStop tabStop)
  if '\r' `elem` source
    then Left SourceRangeCarriageReturnUnsupported
    else Right ()
  startOffset <- sourcePositionOffset tabStop source (srStartLine region) (srStartCol region)
  endOffset <-
    case endConvention of
      SourceEndHalfOpen -> sourcePositionOffset tabStop source (srEndLine region) (srEndCol region)
      SourceEndInclusive -> sourceInclusivePositionEndOffset tabStop source (srEndLine region) (srEndCol region)
  if startOffset <= endOffset
    then Right (SourceCharRange startOffset endOffset)
    else Left (SourceRangeEndsBeforeStart region)

sourceCharRangeRegion :: String -> SourceCharRange -> Either SourceRangeFailure SourceRegion
sourceCharRangeRegion = sourceCharRangeRegionWith 8

sourceCharRangeRegionWith :: Int -> String -> SourceCharRange -> Either SourceRangeFailure SourceRegion
sourceCharRangeRegionWith tabStop source sourceRange@(SourceCharRange startOffset endOffset) = do
  if tabStop > 0
    then Right ()
    else Left (SourceRangeInvalidTabStop tabStop)
  if '\r' `elem` source
    then Left SourceRangeCarriageReturnUnsupported
    else Right ()
  _ <- sourceCharRangeText source sourceRange
  (startLine, startColumn) <- sourcePositionAtOffset tabStop source startOffset
  (endLine, endColumn) <- sourcePositionAtOffset tabStop source endOffset
  Right
    SourceRegion
      { srStartLine = startLine,
        srStartCol = startColumn,
        srEndLine = endLine,
        srEndCol = endColumn
      }

sourceCharRangeText :: String -> SourceCharRange -> Either SourceRangeFailure String
sourceCharRangeText source (SourceCharRange startOffset endOffset)
  | startOffset >= 0 && startOffset <= endOffset && endOffset <= length source =
      Right (take (endOffset - startOffset) (drop startOffset source))
  | otherwise =
      Left (SourceRangeOffsetsInvalid startOffset endOffset)

sourcePositionOffset :: Int -> String -> Int -> Int -> Either SourceRangeFailure Int
sourcePositionOffset tabStop source lineNumber columnNumber = do
  (lineStart, lineText) <- sourceLineAt source lineNumber columnNumber
  localOffset <- sourceLineBoundaryOffset tabStop lineNumber columnNumber lineText
  Right (lineStart + localOffset)

sourceInclusivePositionEndOffset :: Int -> String -> Int -> Int -> Either SourceRangeFailure Int
sourceInclusivePositionEndOffset tabStop source lineNumber columnNumber = do
  (lineStart, lineText) <- sourceLineAt source lineNumber columnNumber
  localOffset <- sourceLineBoundaryOffset tabStop lineNumber columnNumber lineText
  if localOffset < length lineText
    then Right (lineStart + localOffset + 1)
    else Left (SourceRangePositionOutsideSource lineNumber columnNumber)

sourceLineAt :: String -> Int -> Int -> Either SourceRangeFailure (Int, String)
sourceLineAt source lineNumber columnNumber =
  case drop (lineNumber - 1) (sourceLineRows source) of
    lineRow : _
      | lineNumber >= 1 && columnNumber >= 1 -> Right lineRow
    _ -> Left (SourceRangePositionOutsideSource lineNumber columnNumber)

sourceLineRows :: String -> [(Int, String)]
sourceLineRows source =
  let lineTexts = splitCanonicalLines source
      lineStarts = scanl (\lineStart lineText -> lineStart + length lineText + 1) 0 lineTexts
   in zip lineStarts lineTexts

sourcePositionAtOffset :: Int -> String -> Int -> Either SourceRangeFailure (Int, Int)
sourcePositionAtOffset tabStop source sourceOffset =
  case containingLine of
    Nothing -> Left (SourceRangeOffsetsInvalid sourceOffset sourceOffset)
    Just (lineNumber, lineStart, lineText) ->
      let localOffset = sourceOffset - lineStart
          visualColumn = foldl (advanceVisualColumn tabStop) 1 (take localOffset lineText)
       in if localOffset <= length lineText
            then Right (lineNumber, visualColumn)
            else Left (SourceRangeOffsetsInvalid sourceOffset sourceOffset)
  where
    containingLine =
      foldl
        (\selected row -> if lineStartOf row <= sourceOffset then Just row else selected)
        Nothing
        (zipWith toNumberedLine [1 ..] (sourceLineRows source))
    toNumberedLine :: Int -> (Int, String) -> (Int, Int, String)
    toNumberedLine lineNumber (lineStart, lineText) = (lineNumber, lineStart, lineText)
    lineStartOf :: (Int, Int, String) -> Int
    lineStartOf (_, lineStart, _) = lineStart

splitCanonicalLines :: String -> [String]
splitCanonicalLines source =
  case break (== '\n') source of
    (lineText, []) -> [lineText]
    (lineText, _ : remaining) -> lineText : splitCanonicalLines remaining

sourceLineBoundaryOffset :: Int -> Int -> Int -> String -> Either SourceRangeFailure Int
sourceLineBoundaryOffset tabStop lineNumber targetColumn = resolve 0 1
  where
    resolve localOffset visualColumn remaining
      | targetColumn == visualColumn = Right localOffset
      | otherwise =
          case remaining of
            [] -> Left (SourceRangePositionOutsideSource lineNumber targetColumn)
            character : rest ->
              let nextVisualColumn = advanceVisualColumn tabStop visualColumn character
               in if targetColumn > visualColumn && targetColumn < nextVisualColumn
                    then Left (SourceRangePositionInsideTab lineNumber targetColumn)
                    else resolve (localOffset + 1) nextVisualColumn rest

advanceVisualColumn :: Int -> Int -> Char -> Int
advanceVisualColumn tabStop visualColumn character
  | character == '\t' = ((visualColumn - 1) `div` tabStop + 1) * tabStop + 1
  | otherwise = visualColumn + 1

type HsExprF :: Type -> Type
data HsExprF r
  = VarF !HsVarRef
  | AppF !r !r
  | LamF !BinderAnn !r
  | LetF !LetRecursion ![(HsPatF, r)] !r
  | OpChainF !r !(NonEmpty (r, r))
  | SectionLF !r !r
  | SectionRF !r !r
  | ParF !r
  | LitF !NormalizedLit
  | OverLitF !NormalizedOverLit
  | IfF !r !r !r
  | CaseF !r ![(HsPatF, r)]
  | DoF ![HsStmtF r]
  | NegF !r
  | ExplicitListF ![r]
  | ExplicitTupleF !TupleBoxity ![TupleSlot r]
  | RecordConF !r ![(NormalizedFieldLabel, r)]
  | RecordUpdF !r ![(NormalizedFieldLabel, r)]
  | ArithSeqF !(NormalizedArithSeq r)
  | GuardedF ![GuardedAltF r]
  | ClausesF ![([HsPatF], r)]
  | MultiIfF ![GuardedAltF r]
  | ExprWithTySigF !r !NormalizedTypeText
  | AppTypeF !r !NormalizedTypeText
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | Lifted equality and rendering for the expression layer, so @Fix HsExprF@
-- carries @Eq@ and @Show@; shape equality via 'void' pins every constructor
-- and scalar field, and 'toList' pairs the children positionally.
instance Eq1 HsExprF where
  liftEq eq left right =
    void left == void right
      && and (zipWith eq (toList left) (toList right))

newtype ShownChild = ShownChild (Int -> ShowS)

instance Show ShownChild where
  showsPrec depth (ShownChild render) = render depth

instance Show1 HsExprF where
  liftShowsPrec render _ depth =
    showsPrec depth
      . fmap (\child -> ShownChild (\precedence -> render precedence child))

type HsGuardStmtF :: Type -> Type
data HsGuardStmtF r
  = GuardBoolF !r
  | GuardPatF !HsPatF !r
  | GuardLetF !LetRecursion ![(HsPatF, r)]
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

type GuardedAltF :: Type -> Type
data GuardedAltF r = GuardedAltF
  { gaGuards :: ![HsGuardStmtF r],
    gaBody :: !r
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

type HsStmtF :: Type -> Type
data HsStmtF r
  = BindStmtF !HsPatF !r
  | BodyStmtF !r
  | LetStmtF !LetRecursion ![(HsPatF, r)]
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

type Expr :: Type
data Expr = Expr
  { exprRegion :: !(Maybe SourceRegion),
    exprScope :: !ScopeId,
    exprFreeScopes :: !FreeScopeSummary,
    exprNode :: !(HsExprF Expr)
  }
  deriving stock (Eq, Ord, Show)

type HsExprTag :: Type
data HsExprTag
  = VarTag
  | AppTag
  | LamTag
  | LetTag
  | OpChainTag
  | SectionLTag
  | SectionRTag
  | ParTag
  | LitTag
  | OverLitTag
  | IfTag
  | CaseTag
  | DoTag
  | NegTag
  | ExplicitListTag
  | ExplicitTupleTag
  | RecordConTag
  | RecordUpdTag
  | ArithSeqTag
  | GuardedTag
  | ClausesTag
  | MultiIfTag
  | ExprWithTySigTag
  | AppTypeTag
  deriving stock (Eq, Ord, Show, Enum, Bounded)

type TagSignature :: Type
newtype TagSignature = TagSignature Word64
  deriving stock (Eq, Ord, Show)

tagSignatureFromTag :: HsExprTag -> TagSignature
tagSignatureFromTag tag =
  TagSignature (bit (fromEnum tag))

tagSignatureMember :: HsExprTag -> TagSignature -> Bool
tagSignatureMember tag (TagSignature signature) =
  testBit signature (fromEnum tag)

instance Semigroup TagSignature where
  TagSignature left <> TagSignature right =
    TagSignature (left .|. right)

instance Monoid TagSignature where
  mempty =
    TagSignature 0

instance HasConstructorTag HsExprF where
  type ConstructorTag HsExprF = HsExprTag

  constructorTag = \case
    VarF {} -> VarTag
    AppF {} -> AppTag
    LamF {} -> LamTag
    LetF {} -> LetTag
    OpChainF {} -> OpChainTag
    SectionLF {} -> SectionLTag
    SectionRF {} -> SectionRTag
    ParF {} -> ParTag
    LitF {} -> LitTag
    OverLitF {} -> OverLitTag
    IfF {} -> IfTag
    CaseF {} -> CaseTag
    DoF {} -> DoTag
    NegF {} -> NegTag
    ExplicitListF {} -> ExplicitListTag
    ExplicitTupleF {} -> ExplicitTupleTag
    RecordConF {} -> RecordConTag
    RecordUpdF {} -> RecordUpdTag
    ArithSeqF {} -> ArithSeqTag
    GuardedF {} -> GuardedTag
    ClausesF {} -> ClausesTag
    MultiIfF {} -> MultiIfTag
    ExprWithTySigF {} -> ExprWithTySigTag
    AppTypeF {} -> AppTypeTag

instance ZipMatch HsExprF where
  zipMatch =
    zipSameNodeShape

normalizeHsLit :: HsLit GhcPs -> NormalizedLit
normalizeHsLit = \case
  HsChar _ value -> NormalizedChar value
  HsCharPrim _ value -> NormalizedCharPrim value
  HsString _ value -> NormalizedString (unpackFS value)
  HsMultilineString _ value -> NormalizedMultilineString (unpackFS value)
  HsStringPrim _ value -> NormalizedStringPrim value
  HsInt _ value -> NormalizedInt (exactIntegral value)
  HsIntPrim sourceText value -> NormalizedIntPrim (primitiveIntegral sourceText value)
  HsWordPrim sourceText value -> NormalizedWordPrim (primitiveIntegral sourceText value)
  HsInt8Prim sourceText value -> NormalizedInt8Prim (primitiveIntegral sourceText value)
  HsInt16Prim sourceText value -> NormalizedInt16Prim (primitiveIntegral sourceText value)
  HsInt32Prim sourceText value -> NormalizedInt32Prim (primitiveIntegral sourceText value)
  HsInt64Prim sourceText value -> NormalizedInt64Prim (primitiveIntegral sourceText value)
  HsWord8Prim sourceText value -> NormalizedWord8Prim (primitiveIntegral sourceText value)
  HsWord16Prim sourceText value -> NormalizedWord16Prim (primitiveIntegral sourceText value)
  HsWord32Prim sourceText value -> NormalizedWord32Prim (primitiveIntegral sourceText value)
  HsWord64Prim sourceText value -> NormalizedWord64Prim (primitiveIntegral sourceText value)
  HsFloatPrim _ value -> NormalizedFloatPrim (exactFractional value)
  HsDoublePrim _ value -> NormalizedDoublePrim (exactFractional value)

normalizeHsOverLit :: HsOverLit GhcPs -> NormalizedOverLit
normalizeHsOverLit = \case
  OverLit {ol_val = value} -> normalizeOverLitVal value

normalizeFieldLabel :: FieldLabel -> NormalizedFieldLabel
normalizeFieldLabel fieldLabelValue =
  NormalizedFieldLabel
    { nflSelector = occNameString (nameOccName (flSelector fieldLabelValue)),
      nflAllowsDuplicateRecordFields = duplicateFieldFlag (flHasDuplicateRecordFields fieldLabelValue),
      nflHasSelector = selectorFieldFlag (flHasFieldSelector fieldLabelValue)
    }

eraseExpr :: Expr -> Pattern HsExprF
eraseExpr expressionValue =
  PatternNode (fmap eraseExpr (exprNode expressionValue))

normalizeOverLitVal :: OverLitVal -> NormalizedOverLit
normalizeOverLitVal = \case
  HsIntegral value -> NormalizedIntegralOverLit (exactIntegral value)
  HsFractional value -> NormalizedFractionalOverLit (exactFractional value)
  HsIsString _ value -> NormalizedStringOverLit (unpackFS value)

exactIntegral :: IntegralLit -> ExactIntegral
exactIntegral (IL sourceText isNegative value) =
  ExactIntegral
    { exactIntegralSource = sourceTextString sourceText,
      exactIntegralNegative = isNegative,
      exactIntegralValue = value
    }

primitiveIntegral :: SourceText -> Integer -> ExactIntegral
primitiveIntegral sourceText value =
  ExactIntegral
    { exactIntegralSource = sourceTextString sourceText,
      exactIntegralNegative = value < 0,
      exactIntegralValue = abs value
    }

exactFractional :: FractionalLit -> ExactFractional
exactFractional fractionalLit =
  ExactFractional
    { exactFractionalSource = sourceTextString (fl_text fractionalLit),
      exactFractionalNegative = fl_neg fractionalLit,
      exactFractionalSignificand = fl_signi fractionalLit,
      exactFractionalExponent = fl_exp fractionalLit,
      exactFractionalBase = fl_exp_base fractionalLit
    }

sourceTextString :: SourceText -> Maybe String
sourceTextString = \case
  SourceText sourceText -> Just (unpackFS sourceText)
  NoSourceText -> Nothing

duplicateFieldFlag :: DuplicateRecordFields -> Bool
duplicateFieldFlag = \case
  DuplicateRecordFields -> True
  NoDuplicateRecordFields -> False

selectorFieldFlag :: FieldSelectors -> Bool
selectorFieldFlag = \case
  FieldSelectors -> True
  NoFieldSelectors -> False
