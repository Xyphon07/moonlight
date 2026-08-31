{-| GHC-parsed module identities, imports, and exports. -}
module Moonlight.Pale.Ghc.ModuleSurface
  ( ParsedModuleName,
    mkParsedModuleName,
    unParsedModuleName,
    ParsedName,
    mkParsedName,
    unParsedName,
    ModuleSurface (..),
    ModuleSurfaceError (..),
    ExportSpec (..),
    ExportItem (..),
    ExportChildSpec (..),
    explicitExportNames,
    GhcParseFailure (..),
    renderGhcParseFailure,
    parseWithGhcParser,
    parseHsModule,
    moduleIdentity,
    moduleImportNames,
    moduleExportNames,
    moduleExportIdentifiers,
    exportedIdentifier,
    wrappedNameIdentifier,
    rdrNameIdentifier,
    moduleSurfaceFromGhcPs,
  )
where

import Data.Kind (Type)
import Data.Function ((&))
import Data.List (find)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified GHC.Data.EnumSet as EnumSet
import GHC.Data.FastString (mkFastString)
import GHC.Data.StringBuffer (StringBuffer, stringToStringBuffer)
import GHC.Driver.DynFlags (Language (..), languageExtensions)
import GHC.Driver.Flags (OnOff (..), WarningFlag, impliedXFlags)
import GHC.Driver.Session (flagSpecFlag, flagSpecName, xFlags)
import GHC.Hs
  ( GhcPs,
    HsModule (..),
    IE (..),
    IEWildcard (..),
    IEWrappedName (..),
    ImportDecl (..),
    LIE,
    LIEWrappedName,
  )
import GHC.LanguageExtensions.Type (Extension)
import GHC.Parser (parseModule)
import GHC.Parser.Errors.Ppr ()
import GHC.Parser.Header (getOptions)
import GHC.Parser.Lexer
  ( P (..),
    ParseResult (..),
    PState,
    getPsErrorMessages,
    initParserState,
    mkParserOpts,
  )
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (RdrName, rdrNameOcc)
import GHC.Types.SrcLoc (GenLocated, mkRealSrcLoc, unLoc)
import GHC.Types.Error (defaultOpts, isEmptyMessages)
import GHC.Unit.Module.Warnings (emptyWarningCategorySet)
import GHC.Utils.Error
  ( DiagOpts (..),
    pprMessages,
  )
import GHC.Utils.Outputable (defaultSDocContext, showSDocUnsafe)
import Language.Haskell.Syntax.Module.Name (moduleNameString)
import Moonlight.Core (IdentifierToken, mkIdentifierTokenWith, renderIdentifierToken)
import Moonlight.Core (isCompactName, isQualifiedModuleName)

type ParsedModuleNameNamespace :: Type
data ParsedModuleNameNamespace

type ParsedModuleName :: Type
newtype ParsedModuleName = ParsedModuleName (IdentifierToken ParsedModuleNameNamespace)
  deriving stock (Eq, Ord, Show)

type ParsedNameNamespace :: Type
data ParsedNameNamespace

type ParsedName :: Type
newtype ParsedName = ParsedName (IdentifierToken ParsedNameNamespace)
  deriving stock (Eq, Ord, Show)

type ModuleSurface :: Type
data ModuleSurface = ModuleSurface
  { surfaceModuleName :: Maybe ParsedModuleName,
    surfaceImportedModules :: Set ParsedModuleName,
    surfaceExports :: !ExportSpec
  }
  deriving stock (Eq, Show)

type ExportSpec :: Type
data ExportSpec
  = ImplicitExports
  | ExplicitExports ![ExportItem]
  deriving stock (Eq, Ord, Show)

type ExportItem :: Type
data ExportItem
  = ExportValue !ParsedName
  | ExportType !ParsedName !ExportChildSpec
  | ExportPattern !ParsedName
  | ExportModule !ParsedModuleName
  deriving stock (Eq, Ord, Show)

type ExportChildSpec :: Type
data ExportChildSpec
  = NoExportedChildren
  | AllExportedChildren
  | ExplicitExportedChildren ![ParsedName]
  deriving stock (Eq, Ord, Show)

type ModuleSurfaceError :: Type
data ModuleSurfaceError
  = InvalidSurfaceModuleName !String
  | InvalidImportedModuleName !String
  | InvalidExportedName !String
  | InvalidReexportedModuleName !String
  deriving stock (Eq, Ord, Show)

type GhcParseFailure :: Type
data GhcParseFailure
  = LanguagePragmaHeaderRejected !FilePath !String
  | SourceParseRejected !FilePath !String
  deriving stock (Eq, Ord, Show)

renderGhcParseFailure :: GhcParseFailure -> String
renderGhcParseFailure = \case
  LanguagePragmaHeaderRejected sourcePath rendered ->
    sourcePath <> ": malformed LANGUAGE/OPTIONS_GHC header\n" <> rendered
  SourceParseRejected sourcePath rendered ->
    sourcePath <> ": parse failure\n" <> rendered

mkParsedModuleName :: String -> Maybe ParsedModuleName
mkParsedModuleName =
  fmap ParsedModuleName . mkIdentifierTokenWith isQualifiedModuleName . Text.pack

unParsedModuleName :: ParsedModuleName -> String
unParsedModuleName (ParsedModuleName identifierToken) =
  Text.unpack (renderIdentifierToken identifierToken)

mkParsedName :: String -> Maybe ParsedName
mkParsedName =
  fmap ParsedName . mkIdentifierTokenWith isCompactName . Text.pack

unParsedName :: ParsedName -> String
unParsedName (ParsedName identifierToken) =
  Text.unpack (renderIdentifierToken identifierToken)

parseHsModule :: FilePath -> String -> Either GhcParseFailure (HsModule GhcPs)
parseHsModule sourcePath moduleContents =
  unLoc <$> parseWithGhcParser sourcePath moduleContents parseModule

parseWithGhcParser :: FilePath -> String -> P a -> Either GhcParseFailure a
parseWithGhcParser sourcePath sourceContents parser = do
  enabledExtensions <- parserExtensions sourcePath sourceBuffer
  let parserState =
        initParserState
          (mkParserOpts enabledExtensions parserDiagOpts False False False False)
          sourceBuffer
          (mkRealSrcLoc (mkFastString sourcePath) 1 1)
  case unP parser parserState of
    POk _ parsedValue -> Right parsedValue
    PFailed parserStateValue ->
      Left (SourceParseRejected sourcePath (renderParseFailure parserStateValue))
  where
    sourceBuffer = stringToStringBuffer sourceContents

moduleIdentity :: HsModule GhcPs -> Either ModuleSurfaceError (Maybe String)
moduleIdentity =
  fmap (fmap unParsedModuleName . surfaceModuleName)
    . moduleSurfaceFromGhcPs

moduleImportNames :: HsModule GhcPs -> Either ModuleSurfaceError (Set String)
moduleImportNames =
  fmap (Set.map unParsedModuleName . surfaceImportedModules)
    . moduleSurfaceFromGhcPs

moduleExportNames :: HsModule GhcPs -> Either ModuleSurfaceError (Maybe (Set String))
moduleExportNames =
  fmap
    (fmap (Set.map unParsedName) . explicitExportNames . surfaceExports)
    . moduleSurfaceFromGhcPs

moduleExportIdentifiers :: Maybe (GenLocated l [LIE GhcPs]) -> Either ModuleSurfaceError (Maybe (Set String))
moduleExportIdentifiers maybeExports =
  case maybeExports of
    Nothing -> Right Nothing
    Just exports ->
      Just . Set.fromList
        <$> traverse exportedIdentifier (unLoc exports)

moduleSurfaceFromGhcPs :: HsModule GhcPs -> Either ModuleSurfaceError ModuleSurface
moduleSurfaceFromGhcPs moduleAst =
  ModuleSurface
    <$> traverse checkedSurfaceModuleName (moduleNameString . unLoc <$> hsmodName moduleAst)
    <*> (Set.fromList <$> traverse checkedImportedModuleName importedNames)
    <*> traverseExportSpec (hsmodExports moduleAst)
  where
    importedNames =
      fmap (moduleNameString . unLoc . ideclName . unLoc) (hsmodImports moduleAst)

    checkedSurfaceModuleName rawName =
      maybe (Left (InvalidSurfaceModuleName rawName)) Right (mkParsedModuleName rawName)

    checkedImportedModuleName rawName =
      maybe (Left (InvalidImportedModuleName rawName)) Right (mkParsedModuleName rawName)

traverseExportSpec :: Maybe (GenLocated l [LIE GhcPs]) -> Either ModuleSurfaceError ExportSpec
traverseExportSpec =
  maybe
    (Right ImplicitExports)
    (fmap ExplicitExports . traverse exportItem . unLoc)

exportedIdentifier :: LIE GhcPs -> Either ModuleSurfaceError String
exportedIdentifier =
  fmap exportItemIdentifier . exportItem

exportItem :: LIE GhcPs -> Either ModuleSurfaceError ExportItem
exportItem exportEntry =
  case unLoc exportEntry of
    IEVar _ wrappedNameValue _ ->
      exportWrappedName wrappedNameValue
    IEThingAbs _ wrappedNameValue _ ->
      ExportType <$> checkedWrappedName wrappedNameValue <*> pure NoExportedChildren
    IEThingAll _ wrappedNameValue _ ->
      ExportType <$> checkedWrappedName wrappedNameValue <*> pure AllExportedChildren
    IEThingWith _ wrappedNameValue wildcardValue childNames _ ->
      ExportType
        <$> checkedWrappedName wrappedNameValue
        <*> case wildcardValue of
          NoIEWildcard ->
            ExplicitExportedChildren <$> traverse checkedWrappedName childNames
          IEWildcard _ ->
            pure AllExportedChildren
    IEModuleContents _ moduleName ->
      let rawName = moduleNameString (unLoc moduleName)
       in maybe
            (Left (InvalidReexportedModuleName rawName))
            (Right . ExportModule)
            (mkParsedModuleName rawName)
    IEGroup {} ->
      Left (InvalidExportedName "<documentation-group>")
    IEDoc {} ->
      Left (InvalidExportedName "<documentation>")
    IEDocNamed {} ->
      Left (InvalidExportedName "<named-documentation>")

exportWrappedName :: LIEWrappedName GhcPs -> Either ModuleSurfaceError ExportItem
exportWrappedName wrappedNameValue =
  case unLoc wrappedNameValue of
    IEPattern {} -> ExportPattern <$> checkedWrappedName wrappedNameValue
    IEType {} -> ExportType <$> checkedWrappedName wrappedNameValue <*> pure NoExportedChildren
    IEData {} -> ExportType <$> checkedWrappedName wrappedNameValue <*> pure NoExportedChildren
    _ -> ExportValue <$> checkedWrappedName wrappedNameValue

wrappedNameIdentifier :: LIEWrappedName GhcPs -> Either ModuleSurfaceError String
wrappedNameIdentifier =
  fmap unParsedName . checkedWrappedName

checkedWrappedName :: LIEWrappedName GhcPs -> Either ModuleSurfaceError ParsedName
checkedWrappedName wrappedNameValue =
  maybe
    (Left (InvalidExportedName rawName))
    Right
    (mkParsedName rawName)
  where
    rawName = rdrNameIdentifier (unLoc wrappedLocatedName)
    wrappedLocatedName =
      case unLoc wrappedNameValue of
        IEName _ name -> name
        IEPattern _ name -> name
        IEType _ name -> name
        IEDefault _ name -> name
        IEData _ name -> name

explicitExportNames :: ExportSpec -> Maybe (Set ParsedName)
explicitExportNames = \case
  ImplicitExports ->
    Nothing
  ExplicitExports exportItems ->
    Just (Set.fromList (foldMap exportItemNames exportItems))

exportItemNames :: ExportItem -> [ParsedName]
exportItemNames = \case
  ExportValue parsedName -> [parsedName]
  ExportType parsedName childSpec -> parsedName : childNames childSpec
  ExportPattern parsedName -> [parsedName]
  ExportModule _ -> []
  where
    childNames = \case
      ExplicitExportedChildren names -> names
      NoExportedChildren -> []
      AllExportedChildren -> []

exportItemIdentifier :: ExportItem -> String
exportItemIdentifier = \case
  ExportValue parsedName -> unParsedName parsedName
  ExportType parsedName _ -> unParsedName parsedName
  ExportPattern parsedName -> unParsedName parsedName
  ExportModule parsedModuleName -> unParsedModuleName parsedModuleName

rdrNameIdentifier :: RdrName -> String
rdrNameIdentifier =
  occNameString . rdrNameOcc

renderParseFailure :: PState -> String
renderParseFailure parserStateValue =
  parserStateValue
    & getPsErrorMessages
    & pprMessages defaultOpts
    & showSDocUnsafe

type LanguagePragmaDirective :: Type
data LanguagePragmaDirective
  = UseLanguage !Language
  | EnableExtension !Extension
  | DisableExtension !Extension
  deriving stock (Eq, Show)

type ParserExtensionState :: Type
data ParserExtensionState = ParserExtensionState
  { pesEnabled :: !(EnumSet.EnumSet Extension),
    pesExplicitlyDisabled :: !(EnumSet.EnumSet Extension)
  }

parserExtensions ::
  FilePath ->
  StringBuffer ->
  Either GhcParseFailure (EnumSet.EnumSet Extension)
parserExtensions sourcePath sourceBuffer =
  let headerParserOptions =
        mkParserOpts
          (pesEnabled ghc2024ParserExtensionState)
          parserDiagOpts
          False
          False
          False
          False
      (headerMessages, locatedOptions) =
        getOptions
          headerParserOptions
          supportedLanguagePragmas
          sourceBuffer
          sourcePath
   in if isEmptyMessages headerMessages
        then
          Right
            ( pesEnabled
                ( foldl'
                    applyLanguagePragmaDirective
                    ghc2024ParserExtensionState
                    (mapMaybe (languagePragmaDirective . unLoc) locatedOptions)
                )
            )
        else
          Left
            ( LanguagePragmaHeaderRejected
                sourcePath
                (showSDocUnsafe (pprMessages defaultOpts headerMessages))
            )

parserExtensionStateForLanguage :: Language -> ParserExtensionState
parserExtensionStateForLanguage languageValue =
  closeImpliedExtensions
    ParserExtensionState
      { pesEnabled = EnumSet.fromList (languageExtensions (Just languageValue)),
        pesExplicitlyDisabled = EnumSet.empty
      }

ghc2024ParserExtensionState :: ParserExtensionState
ghc2024ParserExtensionState =
  parserExtensionStateForLanguage GHC2024

applyLanguagePragmaDirective :: ParserExtensionState -> LanguagePragmaDirective -> ParserExtensionState
applyLanguagePragmaDirective _ (UseLanguage languageValue) =
  parserExtensionStateForLanguage languageValue
applyLanguagePragmaDirective parserExtensionState (EnableExtension extensionValue) =
  closeImpliedExtensions
    parserExtensionState
      { pesEnabled = EnumSet.insert extensionValue (pesEnabled parserExtensionState),
        pesExplicitlyDisabled = EnumSet.delete extensionValue (pesExplicitlyDisabled parserExtensionState)
      }
applyLanguagePragmaDirective parserExtensionState (DisableExtension extensionValue) =
  closeImpliedExtensions
    parserExtensionState
      { pesEnabled = EnumSet.delete extensionValue (pesEnabled parserExtensionState),
        pesExplicitlyDisabled = EnumSet.insert extensionValue (pesExplicitlyDisabled parserExtensionState)
      }

closeImpliedExtensions :: ParserExtensionState -> ParserExtensionState
closeImpliedExtensions parserExtensionState =
  let nextState =
        foldl'
          applyImpliedExtension
          parserExtensionState
          impliedXFlags
   in if sameParserExtensionState nextState parserExtensionState
        then parserExtensionState
        else closeImpliedExtensions nextState

sameParserExtensionState :: ParserExtensionState -> ParserExtensionState -> Bool
sameParserExtensionState leftState rightState =
  EnumSet.toList (pesEnabled leftState) == EnumSet.toList (pesEnabled rightState)
    && EnumSet.toList (pesExplicitlyDisabled leftState) == EnumSet.toList (pesExplicitlyDisabled rightState)

applyImpliedExtension :: ParserExtensionState -> (Extension, OnOff Extension) -> ParserExtensionState
applyImpliedExtension parserExtensionState (triggerExtension, impliedDirective)
  | EnumSet.member triggerExtension (pesEnabled parserExtensionState) =
      applyImpliedDirective parserExtensionState impliedDirective
  | otherwise =
      parserExtensionState

applyImpliedDirective :: ParserExtensionState -> OnOff Extension -> ParserExtensionState
applyImpliedDirective parserExtensionState (On extensionValue)
  | EnumSet.member extensionValue (pesExplicitlyDisabled parserExtensionState) =
      parserExtensionState
  | otherwise =
      parserExtensionState
        { pesEnabled = EnumSet.insert extensionValue (pesEnabled parserExtensionState)
        }
applyImpliedDirective parserExtensionState (Off extensionValue) =
  parserExtensionState
    { pesEnabled = EnumSet.delete extensionValue (pesEnabled parserExtensionState)
    }

languagePragmaDirective :: String -> Maybe LanguagePragmaDirective
languagePragmaDirective optionToken =
  Text.stripPrefix (Text.pack "-X") (Text.pack optionToken)
    >>= directiveForName . Text.unpack
  where
    directiveForName token =
      case languageName token of
        Just languageValue ->
          Just (UseLanguage languageValue)
        Nothing ->
          case extensionNamed token of
            Just extensionValue ->
              Just (EnableExtension extensionValue)
            Nothing ->
              DisableExtension <$> noExtension token

languageName :: String -> Maybe Language
languageName token =
  find
    ((== token) . show)
    ([minBound .. maxBound] :: [Language])

noExtension :: String -> Maybe Extension
noExtension token =
  case Text.stripPrefix (Text.pack "No") (Text.pack token) of
    Nothing -> Nothing
    Just extensionName -> extensionNamed (Text.unpack extensionName)

extensionNamed :: String -> Maybe Extension
extensionNamed extensionName =
  flagSpecFlag
    <$> find
      ((== extensionName) . flagSpecName)
      xFlags

supportedLanguagePragmas :: [String]
supportedLanguagePragmas =
  languageNames
    <> extensionNames
    <> fmap ("No" <>) extensionNames
  where
    languageNames =
      fmap show ([minBound .. maxBound] :: [Language])
    extensionNames =
      fmap flagSpecName xFlags

parserDiagOpts :: DiagOpts
parserDiagOpts =
  DiagOpts
    { diag_warning_flags = EnumSet.empty :: EnumSet.EnumSet WarningFlag,
      diag_fatal_warning_flags = EnumSet.empty :: EnumSet.EnumSet WarningFlag,
      diag_custom_warning_categories = emptyWarningCategorySet,
      diag_fatal_custom_warning_categories = emptyWarningCategorySet,
      diag_warn_is_error = False,
      diag_reverse_errors = False,
      diag_max_errors = Nothing,
      diag_ppr_ctx = defaultSDocContext
    }
