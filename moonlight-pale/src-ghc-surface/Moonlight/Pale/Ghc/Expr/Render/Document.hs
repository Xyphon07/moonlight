{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Moonlight.Pale.Ghc.Expr.Render.Document
  ( PageWidth (..),
    defaultPageWidth,
    mkPageWidth,
    shrinkPageWidth,
    LayoutPolicy (..),
    RenderMode (..),
    RenderDocument (..),
    CompactDocument (..),
    PrettyDocument (..),
    intercalateDocument,
    (<+>),
    nest,
    renderCompactDocument,
    renderPrettyDocument,
    renderBreakableApplication,
    renderDelimitedSequence,
    renderLayoutDelimitedSequence,
    renderDelimitedExpression,
    renderBlock,
    wrapParen,
    hcat,
    intercalateDoc
  )
where

import Data.Kind (Constraint, Type)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LazyText
import Data.Text.Lazy.Builder qualified as TextBuilder
import Prettyprinter qualified as Pretty
import Prettyprinter.Render.Text qualified as PrettyText
type PageWidth :: Type
newtype PageWidth = PageWidth Int
  deriving stock (Eq, Ord, Show)

defaultPageWidth :: PageWidth
defaultPageWidth =
  PageWidth 80

mkPageWidth :: Int -> Maybe PageWidth
mkPageWidth columnCount
  | columnCount > 0 =
      Just (PageWidth columnCount)
  | otherwise =
      Nothing

shrinkPageWidth :: Int -> PageWidth -> PageWidth
shrinkPageWidth indentationAmount (PageWidth columnCount) =
  PageWidth
    (max 1 (columnCount - max 0 indentationAmount))

type LayoutPolicy :: Type
data LayoutPolicy
  = CompactLayout
  | PrettyLayout !PageWidth
  deriving stock (Eq, Ord, Show)

type RenderMode :: Type
data RenderMode
  = CompactRender
  | GeneratedRender

type RenderDocument :: Type -> Constraint
class Monoid document => RenderDocument document where
  text :: String -> document
  hangingIndent :: Int -> document -> document
  vcat :: [document] -> document
  hsep :: [document] -> document
  group :: document -> document
  line :: document
  softline :: document
  softbreak :: document
  parenthesizeDoc :: document -> document

newtype CompactDocument = CompactDocument
  { compactDocumentBuilder :: TextBuilder.Builder
  }
  deriving newtype (Semigroup, Monoid)

instance RenderDocument CompactDocument where
  text =
    CompactDocument . TextBuilder.fromString
  hangingIndent _ documentValue =
    documentValue
  vcat =
    intercalateDocument (text "\n")
  hsep =
    intercalateDocument (text " ")
  group =
    id
  line =
    text "\n"
  softline =
    text " "
  softbreak =
    mempty
  parenthesizeDoc documentValue =
    text "(" <> documentValue <> text ")"

newtype PrettyDocument = PrettyDocument
  { prettyDocumentValue :: Pretty.Doc ()
  }
  deriving newtype (Semigroup, Monoid)

instance RenderDocument PrettyDocument where
  text =
    PrettyDocument . Pretty.pretty
  hangingIndent indentationAmount (PrettyDocument documentValue) =
    PrettyDocument (Pretty.nest indentationAmount documentValue)
  vcat documentValues =
    PrettyDocument
      ( Pretty.concatWith
          (\leftDocument rightDocument -> leftDocument <> Pretty.hardline <> rightDocument)
          (fmap prettyDocumentValue documentValues)
      )
  hsep =
    PrettyDocument . Pretty.hsep . fmap prettyDocumentValue
  group =
    PrettyDocument . Pretty.group . prettyDocumentValue
  line =
    PrettyDocument Pretty.line
  softline =
    PrettyDocument Pretty.line
  softbreak =
    PrettyDocument Pretty.line'
  parenthesizeDoc =
    PrettyDocument . Pretty.parens . prettyDocumentValue

intercalateDocument ::
  Monoid document =>
  document ->
  [document] ->
  document
intercalateDocument separatorDocument documentValues =
  case documentValues of
    [] ->
      mempty
    firstDocument : remainingDocuments ->
      firstDocument
        <> foldMap (separatorDocument <>) remainingDocuments

(<+>) ::
  RenderDocument document =>
  document ->
  document ->
  document
leftDocument <+> rightDocument =
  leftDocument <> text " " <> rightDocument

renderBreakableApplication ::
  RenderDocument document =>
  document ->
  document ->
  document
renderBreakableApplication functionDocument argumentDocument =
  group
    ( functionDocument
        <> hangingIndent 2 (softline <> argumentDocument)
    )

renderDelimitedSequence ::
  RenderDocument document =>
  String ->
  String ->
  [document] ->
  document
renderDelimitedSequence =
  renderDelimitedSequenceWith 0 2

renderLayoutDelimitedSequence ::
  RenderDocument document =>
  String ->
  String ->
  [document] ->
  document
renderLayoutDelimitedSequence =
  renderDelimitedSequenceWith 2 0

renderDelimitedSequenceWith ::
  RenderDocument document =>
  Int ->
  Int ->
  String ->
  String ->
  [document] ->
  document
renderDelimitedSequenceWith outerIndent contentIndent openingDelimiter closingDelimiter = \case
  [] ->
    text (openingDelimiter <> closingDelimiter)
  firstDocument : remainingDocuments ->
    group
      ( hangingIndent
          outerIndent
          ( text openingDelimiter
              <> hangingIndent
                contentIndent
                ( softbreak
                    <> firstDocument
                    <> foldMap
                      (\documentValue -> text "," <> softline <> documentValue)
                      remainingDocuments
                )
              <> softbreak
              <> text closingDelimiter
          )
      )

-- Indent a block, first line included; 'hangingIndent' alone moves only the
-- continuation lines and so must not be substituted here.
nest ::
  RenderDocument document =>
  Int ->
  document ->
  document
nest indentationAmount documentValue =
  text (replicate indentationAmount ' ')
    <> hangingIndent indentationAmount documentValue

renderCompactDocument :: CompactDocument -> Text.Text
renderCompactDocument =
  LazyText.toStrict
    . TextBuilder.toLazyText
    . compactDocumentBuilder

renderPrettyDocument :: PageWidth -> PrettyDocument -> Text.Text
renderPrettyDocument (PageWidth columnCount) =
  PrettyText.renderStrict
    . Pretty.layoutPretty
      (Pretty.LayoutOptions (Pretty.AvailablePerLine columnCount 1.0))
    . prettyDocumentValue

renderDelimitedExpression ::
  RenderDocument document =>
  RenderMode ->
  document ->
  String ->
  document ->
  document
renderDelimitedExpression renderMode lhsDoc delimiter bodyDoc =
  case renderMode of
    CompactRender ->
      lhsDoc <+> text delimiter <+> bodyDoc
    GeneratedRender ->
      group
        ( lhsDoc
            <+> text delimiter
            <> hangingIndent 2 (softline <> bodyDoc)
        )

-- Shared brace-and-semicolon (compact) / indented (generated) block layout for a
-- keyword followed by a list of item docs (@let@, @where@, @\\case@, @\\cases@, @do@).
renderBlock ::
  RenderDocument document =>
  RenderMode ->
  String ->
  [document] ->
  document
renderBlock renderMode keyword itemDocs =
  case renderMode of
    CompactRender ->
      text (keyword <> " { ") <> intercalateDoc (text "; ") itemDocs <> text " }"
    GeneratedRender ->
      vcat [text keyword, nest 2 (vcat itemDocs)]

wrapParen ::
  RenderDocument document =>
  Bool ->
  document ->
  document
wrapParen shouldWrap innerDoc =
  if shouldWrap then parenthesizeDoc innerDoc else innerDoc

hcat ::
  Monoid document =>
  [document] ->
  document
hcat =
  mconcat

intercalateDoc ::
  Monoid document =>
  document ->
  [document] ->
  document
intercalateDoc separatorDoc docValues =
  intercalateDocument separatorDoc docValues
