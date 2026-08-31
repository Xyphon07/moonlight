module SourceShape
  ( assertSourceShape,
    assertTopLevelDeclarationExcludesToken,
  )
where

import Data.Char (isAlphaNum, isSpace)
import Data.Foldable (traverse_)
import Data.List (isInfixOf, isPrefixOf)
import System.FilePath ((</>), joinPath, normalise, splitDirectories)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure)

assertSourceShape :: FilePath -> FilePath -> [String] -> [String] -> Assertion
assertSourceShape testModulePath relativeSourcePath requiredFragments forbiddenFragments =
  case sourcePathFromTestModule testModulePath relativeSourcePath of
    Left pathFailure ->
      assertFailure pathFailure
    Right sourcePath ->
      readFile sourcePath
        >>= assertSourceTextFragments requiredFragments forbiddenFragments

assertTopLevelDeclarationExcludesToken :: FilePath -> FilePath -> String -> String -> Assertion
assertTopLevelDeclarationExcludesToken testModulePath relativeSourcePath declarationPrefix forbiddenToken = do
  case sourcePathFromTestModule testModulePath relativeSourcePath of
    Left pathFailure ->
      assertFailure pathFailure
    Right sourcePath -> do
      sourceText <- readFile sourcePath
      case topLevelDeclarationBlock declarationPrefix sourceText of
        Left parseFailure ->
          assertFailure parseFailure
        Right declarationBlock ->
          assertBool
            ( "expected "
                <> declarationPrefix
                <> " declaration in "
                <> relativeSourcePath
                <> " to exclude token: "
                <> forbiddenToken
                <> "\n\nDeclaration:\n"
                <> declarationBlock
            )
            (not (forbiddenToken `elem` sourceTokens declarationBlock))

topLevelDeclarationBlock :: String -> String -> Either String String
topLevelDeclarationBlock declarationPrefix sourceText =
  case dropWhile (not . isTargetDeclaration) (lines sourceText) of
    [] ->
      Left ("expected to find top-level declaration: " <> declarationPrefix)
    declarationLine : remainingLines ->
      Right (unlines (declarationLine : takeWhile isDeclarationContinuation remainingLines))
  where
    isTargetDeclaration sourceLine =
      declarationPrefix `isPrefixOf` trimLeft sourceLine

isDeclarationContinuation :: String -> Bool
isDeclarationContinuation sourceLine =
  null sourceLine || startsWithSpace sourceLine

sourceTokens :: String -> [String]
sourceTokens =
  words . fmap tokenCharacter
  where
    tokenCharacter character
      | isAlphaNum character || character == '_' || character == '\'' = character
      | otherwise = ' '

startsWithSpace :: String -> Bool
startsWithSpace sourceLine =
  case sourceLine of
    leadingCharacter : _ -> isSpace leadingCharacter
    [] -> False

trimLeft :: String -> String
trimLeft =
  dropWhile isSpace

sourcePathFromTestModule :: FilePath -> FilePath -> Either String FilePath
sourcePathFromTestModule testModulePath relativeSourcePath =
  (</> relativeSourcePath) <$> packageRootFromTestModule testModulePath

packageRootFromTestModule :: FilePath -> Either String FilePath
packageRootFromTestModule testModulePath =
  case span (/= packageDirectoryName) (reverse pathDirectories) of
    (_afterPackageRoot, packageDirectory : reversedPrefix)
      | packageDirectory == packageDirectoryName ->
          Right (joinPath (reverse (packageDirectory : reversedPrefix)))
    _ ->
      case pathDirectories of
        testDirectory : _remainingPath
          | testDirectory == "test" ->
              Right "."
        _ ->
          Left ("expected test module path to contain " <> packageDirectoryName <> " package root: " <> testModulePath)
  where
    pathDirectories =
      splitDirectories (normalise testModulePath)

packageDirectoryName :: FilePath
packageDirectoryName =
  "moonlight-core"

assertSourceTextFragments :: [String] -> [String] -> String -> Assertion
assertSourceTextFragments requiredFragments forbiddenFragments sourceText =
  do
    traverse_ assertRequiredFragment requiredFragments
    traverse_ assertForbiddenFragment forbiddenFragments
  where
    assertRequiredFragment fragment =
      assertBool
        ("expected source shape to contain: " <> fragment)
        (fragment `isInfixOf` sourceText)
    assertForbiddenFragment fragment =
      assertBool
        ("expected source shape to exclude: " <> fragment)
        (not (fragment `isInfixOf` sourceText))
