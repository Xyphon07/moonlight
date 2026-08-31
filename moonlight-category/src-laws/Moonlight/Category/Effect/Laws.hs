-- | The aggregated law suite: every subtree from the per-domain law modules,
-- rendered as one tasty tree.
module Moonlight.Category.Effect.Laws
  ( tests,
  )
where

import qualified Moonlight.Category.Effect.Laws.Algebra as Algebra
import qualified Moonlight.Category.Effect.Laws.Adhesive as Adhesive
import qualified Moonlight.Category.Effect.Laws.Category as Category
import qualified Moonlight.Category.Effect.Laws.Generators as Generators
import qualified Moonlight.Category.Effect.Laws.Higher as Higher
import qualified Moonlight.Category.Effect.Laws.Limits as Limits
import qualified Moonlight.Category.Effect.Laws.Site as Site
import Moonlight.Pale.Test.Laws.Suite (LawSuite, lawGroup, renderLawSuite)
import Test.Tasty (TestTree)

tests :: TestTree
tests =
  renderLawSuite (lawGroup "moonlight-category" categoryLawSuites)

categoryLawSuites :: [LawSuite]
categoryLawSuites =
  Site.lawSuites
    <> Category.lawSuites
    <> Algebra.lawSuites
    <> Limits.lawSuites
    <> Adhesive.lawSuites
    <> Higher.lawSuites
    <> Generators.lawSuites
