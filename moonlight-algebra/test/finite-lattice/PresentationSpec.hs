{-# LANGUAGE DerivingStrategies #-}

module PresentationSpec
  ( tests,
  )
where

import Data.Set qualified as Set
import Moonlight.FiniteLattice
  ( ContextLattice,
    ContextLatticeCompileError (..),
    LatticeBuildError (..),
    below,
    belowAll,
    boundedLatticeOf,
    clBottom,
    clTop,
    compileContextLattice,
    contextLatticeElements,
    contextOrderDecl,
    element,
    elements,
    joinContext,
    latticeOf,
    leqContext,
    meetContext,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertFailure,
    testCase,
    (@?=),
  )

data DiamondContext
  = DiamondBottom
  | DiamondLeft
  | DiamondRight
  | DiamondTop
  deriving stock (Eq, Ord, Show, Read)

data BowtieContext
  = BowtieBottom
  | BowtieA
  | BowtieB
  | BowtieJoinLeft
  | BowtieJoinRight
  | BowtieTop
  deriving stock (Eq, Ord, Show, Read)

tests :: TestTree
tests =
  testGroup
    "finite lattice presentations"
    [ testCase "the diamond builder reproduces the hand-written declaration" testBuiltDiamondMatchesDeclaration,
      testCase "the bounded batched builder reproduces the hand-written declaration" testBoundedBatchedDiamondMatchesDeclaration,
      testCase "the builder infers top and bottom from the order" testInferredTopAndBottom,
      testCase "below a b means a is at or under b" testEdgeDirection,
      testCase "two maximal elements are an ambiguous top" testAmbiguousTop,
      testCase "a cyclic order has no maximal element" testCyclicOrderHasNoTop,
      testCase "an empty presentation is rejected" testEmptyPresentation,
      testCase "a duplicate element is rejected" testDuplicateElement,
      testCase "a non-lattice poset surfaces the compile obstruction" testNonLatticeSurfacesCompileError
    ]

-- | The centrepiece: a diamond declared by name binding produces the same lattice as
-- the hand-written 'contextOrderDecl', observed through top, bottom, the element list,
-- and the order\/join\/meet of every pair.
builtDiamond :: Either (LatticeBuildError DiamondContext) (ContextLattice DiamondContext)
builtDiamond =
  latticeOf $ do
    [bottom, left, right, top] <-
      elements [DiamondBottom, DiamondLeft, DiamondRight, DiamondTop]
    below bottom left
    below bottom right
    below left top
    below right top

boundedBatchedDiamond :: Either (LatticeBuildError DiamondContext) (ContextLattice DiamondContext)
boundedBatchedDiamond =
  boundedLatticeOf DiamondTop DiamondBottom $ do
    [bottom, left, right, top] <-
      elements [DiamondBottom, DiamondLeft, DiamondRight, DiamondTop]
    belowAll
      [ (bottom, left),
        (bottom, right),
        (left, top),
        (right, top)
      ]

declaredDiamond :: Either (ContextLatticeCompileError DiamondContext) (ContextLattice DiamondContext)
declaredDiamond =
  compileContextLattice
    (Set.fromList [DiamondBottom, DiamondLeft, DiamondRight, DiamondTop])
    ( contextOrderDecl
        DiamondTop
        DiamondBottom
        [ (DiamondBottom, DiamondLeft),
          (DiamondBottom, DiamondRight),
          (DiamondLeft, DiamondTop),
          (DiamondRight, DiamondTop)
        ]
    )

testBuiltDiamondMatchesDeclaration :: Assertion
testBuiltDiamondMatchesDeclaration =
  assertMatchesDeclaredDiamond builtDiamond

testBoundedBatchedDiamondMatchesDeclaration :: Assertion
testBoundedBatchedDiamondMatchesDeclaration =
  assertMatchesDeclaredDiamond boundedBatchedDiamond

assertMatchesDeclaredDiamond :: Either (LatticeBuildError DiamondContext) (ContextLattice DiamondContext) -> Assertion
assertMatchesDeclaredDiamond diamond =
  case (diamond, declaredDiamond) of
    (Right built, Right declared) -> do
      clTop built @?= clTop declared
      clBottom built @?= clBottom declared
      contextLatticeElements built @?= contextLatticeElements declared
      mapM_
        ( \(leftContext, rightContext) -> do
            leqContext built leftContext rightContext
              @?= leqContext declared leftContext rightContext
            joinContext built leftContext rightContext
              @?= joinContext declared leftContext rightContext
            meetContext built leftContext rightContext
              @?= meetContext declared leftContext rightContext
        )
        [ (leftContext, rightContext)
          | leftContext <- contextLatticeElements declared,
            rightContext <- contextLatticeElements declared
        ]
    (Left buildError, _) ->
      assertFailure ("expected the presentation diamond, got " <> show buildError)
    (_, Left compileError) ->
      assertFailure ("expected the declared diamond, got " <> show compileError)

testInferredTopAndBottom :: Assertion
testInferredTopAndBottom =
  case builtDiamond of
    Left buildError ->
      assertFailure ("expected the built diamond, got " <> show buildError)
    Right built -> do
      clTop built @?= DiamondTop
      clBottom built @?= DiamondBottom
      joinContext built DiamondLeft DiamondRight @?= Right DiamondTop
      meetContext built DiamondLeft DiamondRight @?= Right DiamondBottom

testEdgeDirection :: Assertion
testEdgeDirection =
  case builtDiamond of
    Left buildError ->
      assertFailure ("expected the built diamond, got " <> show buildError)
    Right built -> do
      leqContext built DiamondBottom DiamondTop @?= Right True
      leqContext built DiamondTop DiamondBottom @?= Right False

testAmbiguousTop :: Assertion
testAmbiguousTop =
  case latticeOf forkWithoutTop of
    Left (AmbiguousTop candidates) ->
      Set.fromList candidates @?= Set.fromList [DiamondLeft, DiamondRight]
    Left otherError ->
      assertFailure ("expected AmbiguousTop, got " <> show otherError)
    Right _ ->
      assertFailure "expected AmbiguousTop, got a compiled lattice"
  where
    forkWithoutTop = do
      [bottom, left, right] <- elements [DiamondBottom, DiamondLeft, DiamondRight]
      below bottom left
      below bottom right

testCyclicOrderHasNoTop :: Assertion
testCyclicOrderHasNoTop =
  latticeOf cyclicPresentation `shouldFailWith` NoTop
  where
    cyclicPresentation = do
      [a, b] <- elements [DiamondLeft, DiamondRight]
      below a b
      below b a

testEmptyPresentation :: Assertion
testEmptyPresentation =
  ( latticeOf (pure ()) ::
      Either (LatticeBuildError DiamondContext) (ContextLattice DiamondContext)
  )
    `shouldFailWith` EmptyLattice

testDuplicateElement :: Assertion
testDuplicateElement =
  latticeOf duplicatePresentation `shouldFailWith` DuplicateElement DiamondTop
  where
    duplicatePresentation = do
      _ <- element DiamondTop
      _ <- element DiamondTop
      pure ()

-- | A poset with a unique top and bottom whose incomparable pair @A@, @B@ has two
-- minimal upper bounds. Inference succeeds; @compileContextLattice@ rejects it, and the
-- builder forwards that obstruction verbatim through 'InvalidLattice'.
testNonLatticeSurfacesCompileError :: Assertion
testNonLatticeSurfacesCompileError =
  latticeOf bowtiePresentation
    `shouldFailWith` InvalidLattice
      (ContextLatticeJoinDoesNotExist BowtieA BowtieB (Set.fromList [BowtieJoinLeft, BowtieJoinRight]))
  where
    bowtiePresentation = do
      [bottom, a, b, joinLeft, joinRight, top] <-
        elements
          [ BowtieBottom,
            BowtieA,
            BowtieB,
            BowtieJoinLeft,
            BowtieJoinRight,
            BowtieTop
          ]
      below bottom a
      below bottom b
      below a joinLeft
      below b joinLeft
      below a joinRight
      below b joinRight
      below joinLeft top
      below joinRight top

-- | Assert a presentation fails with a given build error, inspecting only the 'Left'
-- so the lattice on the 'Right' (which has no 'Eq') is never compared.
shouldFailWith ::
  (Eq buildError, Show buildError) =>
  Either buildError lattice ->
  buildError ->
  Assertion
shouldFailWith result expected =
  case result of
    Left actual -> actual @?= expected
    Right _ -> assertFailure "expected a lattice presentation failure"
