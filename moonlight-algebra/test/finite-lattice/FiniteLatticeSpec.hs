{-# LANGUAGE GHC2024 #-}

module FiniteLatticeSpec
  ( tests,
  )
where

import Data.Bits ((.&.), (.|.))
import Data.Foldable (for_, traverse_)
import Data.Set qualified as Set
import Moonlight.FiniteLattice
  ( ContextHeyting,
    ContextHeytingCompileError (..),
    ContextCompileLimits (..),
    ContextLattice (clBottom, clTop),
    ContextLatticeCompileError (..),
    ContextLatticeLookupError (..),
    ContextMonotoneMapError (..),
    ContextOrderDecl,
    ContextRepresentation (..),
    compileContextHeyting,
    compileContextLattice,
    compileContextLatticeWith,
    contextLatticeElements,
    contextLatticeFromClosedOrder,
    contextLatticeFromClosedOrderWith,
    contextOrderDecl,
    coverPairs,
    greatestContextFixpoint,
    impliesContext,
    joinContext,
    leastContextFixpoint,
    leqContext,
    lowerCovers,
    meetContext,
    defaultContextCompileLimits,
    residentContextElementForKey,
    residentContextElementKey,
    residentContextElementValue,
    residentHeytingBaseContext,
    residentImplies,
    residentJoin,
    residentJoinMeetKeys,
    residentMeet,
    residentSupportContainsElement,
    residentSupportFromElements,
    residentSupportReachableElements,
    residentSupportWithClosure,
    singletonContextLattice,
    strictOrderPairs,
    upperCovers,
    checkResidentContext,
    withResidentContext,
    withResidentHeytingContext,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
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

data ChainContext
  = ChainBottom
  | ChainX
  | ChainY
  | ChainZ
  | ChainTop
  deriving stock (Eq, Ord, Show, Read)

data NonLatticeContext
  = NBottom
  | NA
  | NB
  | NL
  | NM
  | NU
  | NV
  | NTop
  deriving stock (Eq, Ord, Show, Read)

data M3
  = M3Bottom
  | M3A
  | M3B
  | M3C
  | M3Top
  deriving stock (Eq, Ord, Show, Read)

data N5
  = N5Bottom
  | N5A
  | N5B
  | N5C
  | N5Top
  deriving stock (Eq, Ord, Show, Read)

tests :: TestTree
tests =
  testGroup
    "finite context lattice declarations"
    [ testCase "diamond order compiles to the expected lattice" testDiamondLattice,
      testCase "resident context handles branded total lattice operations" testResidentContextOperations,
      testCase "resident support materializes upward closure without value lookups" testResidentSupportClosure,
      testCase "boolean cube cover graph uses Boolean lattice semantics" testBooleanCubeLattice,
      testCase "unknown context fails checked join" testUnknownJoinContext,
      testCase "unknown context fails checked meet" testUnknownMeetContext,
      testCase "unknown context fails checked order" testUnknownLeqContext,
      testCase "non-monotone endomap is rejected before fixpoint iteration" testRejectsNonMonotoneFixpoint,
      testCase "least context fixpoint ascends a finite chain" testLeastContextFixpointAscendsChain,
      testCase "greatest context fixpoint descends a finite chain" testGreatestContextFixpointDescendsChain,
      testCase "context fixpoint reports endomap closure obstruction" testContextFixpointClosureObstruction,
      testCase "checked context implication derives the finite residuum when Heyting" testImpliesContextUnique,
      testCase "M3 is a lattice but not Heyting" testRejectsM3Residual,
      testCase "Boolean implication satisfies the adjunction" testBooleanAdjunction,
      testCase "dense distributive Heyting implication satisfies the adjunction" testDenseHeytingAdjunction,
      testCase "tableless distributive lattice agrees with dense operations" testTablelessDistributiveMatchesDense,
      testCase "tableless N5 lattice matches dense operations and keeps Heyting witness" testTablelessN5MatchesDense,
      testCase "tableless 64x2 product implication handles one-word masks" testTablelessProduct64ImplicationBoundary,
      testCase "tableless 66x2 product implication handles multi-word masks" testTablelessProduct66ImplicationBoundary,
      testCase "downset-generated distributive lattices satisfy Heyting adjunction" testDownsetGeneratedHeytingAdjunction,
      testCase "dense N5 is rejected as non-Heyting by first missing residual" testRejectsDenseN5Residual,
      testCase "strict pairs and covers are lower-to-upper" testCoverOrientation,
      testCase "unknown top fails before closure" testUnknownTop,
      testCase "unknown bottom fails before closure" testUnknownBottom,
      testCase "unknown relation endpoint fails before closure" testUnknownRelationEndpoint,
      testCase "antisymmetry violation fails after closure" testAntisymmetryViolation,
      testCase "declared top must be greatest" testTopNotGreatest,
      testCase "declared bottom must be least" testBottomNotLeast,
      testCase "missing unique join is rejected" testMissingJoin,
      testCase "closed-order constructor reproduces the compiled diamond lattice" testClosedOrderReproducesDiamond,
      testCase "closed-order constructor rejects an invalid join function" testClosedOrderRejectsInvalidJoin,
      testCase "closed-order constructor rejects joins outside the universe" testClosedOrderRejectsOutsideJoin,
      testCase "closed-order constructor rejects meets outside the universe" testClosedOrderRejectsOutsideMeet,
      testCase "closed-order constructor rejects an intransitive order" testClosedOrderRejectsIntransitiveOrder,
      testCase "closed-order constructor rejects relation layouts over the byte limit" testClosedOrderRejectsRelationLimit
    ]

diamondLeq :: DiamondContext -> DiamondContext -> Bool
diamondLeq leftContext rightContext =
  leftContext == rightContext
    || leftContext == DiamondBottom
    || rightContext == DiamondTop

diamondJoin :: DiamondContext -> DiamondContext -> DiamondContext
diamondJoin leftContext rightContext
  | diamondLeq leftContext rightContext = rightContext
  | diamondLeq rightContext leftContext = leftContext
  | otherwise = DiamondTop

diamondMeet :: DiamondContext -> DiamondContext -> DiamondContext
diamondMeet leftContext rightContext
  | diamondLeq leftContext rightContext = leftContext
  | diamondLeq rightContext leftContext = rightContext
  | otherwise = DiamondBottom

testClosedOrderReproducesDiamond :: Assertion
testClosedOrderReproducesDiamond =
  case ( compileContextLattice diamondUniverse diamondDecl,
         contextLatticeFromClosedOrder
           DiamondTop
           DiamondBottom
           (Set.toAscList diamondUniverse)
           diamondLeq
           diamondJoin
           diamondMeet
       ) of
    (Right compiled, Right closed) -> do
      clTop closed @?= clTop compiled
      clBottom closed @?= clBottom compiled
      contextLatticeElements closed @?= contextLatticeElements compiled
      traverse_
        ( \(leftContext, rightContext) -> do
            leqContext closed leftContext rightContext
              @?= leqContext compiled leftContext rightContext
            joinContext closed leftContext rightContext
              @?= joinContext compiled leftContext rightContext
            meetContext closed leftContext rightContext
              @?= meetContext compiled leftContext rightContext
        )
        [ (leftContext, rightContext)
        | leftContext <- contextLatticeElements compiled,
          rightContext <- contextLatticeElements compiled
        ]
    (Left err, _) ->
      assertFailure ("expected compiled diamond lattice, got " <> show err)
    (_, Left err) ->
      assertFailure ("expected closed-order diamond lattice, got " <> show err)

testClosedOrderRejectsInvalidJoin :: Assertion
testClosedOrderRejectsInvalidJoin =
  contextLatticeFromClosedOrder
    DiamondTop
    DiamondBottom
    (Set.toAscList diamondUniverse)
    diamondLeq
    ( \leftContext rightContext ->
        if leftContext == DiamondLeft && rightContext == DiamondRight
          then DiamondBottom
          else diamondJoin leftContext rightContext
    )
    diamondMeet
    `shouldFailWith` ContextLatticeInvalidJoin DiamondLeft DiamondRight DiamondBottom

testClosedOrderRejectsRelationLimit :: Assertion
testClosedOrderRejectsRelationLimit =
  contextLatticeFromClosedOrderWith
    defaultContextCompileLimits {cclMaximumRelationBytes = Just 0}
    DiamondTop
    DiamondBottom
    (Set.toAscList diamondUniverse)
    diamondLeq
    diamondJoin
    diamondMeet
    `shouldFailWith` ContextLatticeRepresentationLimitExceeded ContextRelationWords 64 0

closedChainUniverse :: [ChainContext]
closedChainUniverse =
  [ChainBottom, ChainX, ChainY, ChainTop]

closedChainLeq :: ChainContext -> ChainContext -> Bool
closedChainLeq leftContext rightContext =
  leftContext == rightContext
    || leftContext == ChainBottom
    || rightContext == ChainTop
    || (leftContext == ChainX && rightContext == ChainY)

closedChainJoin :: ChainContext -> ChainContext -> ChainContext
closedChainJoin leftContext rightContext
  | closedChainLeq leftContext rightContext = rightContext
  | closedChainLeq rightContext leftContext = leftContext
  | otherwise = ChainTop

closedChainMeet :: ChainContext -> ChainContext -> ChainContext
closedChainMeet leftContext rightContext
  | closedChainLeq leftContext rightContext = leftContext
  | closedChainLeq rightContext leftContext = rightContext
  | otherwise = ChainBottom

testClosedOrderRejectsOutsideJoin :: Assertion
testClosedOrderRejectsOutsideJoin =
  contextLatticeFromClosedOrder
    ChainTop
    ChainBottom
    closedChainUniverse
    closedChainLeq
    ( \leftContext rightContext ->
        if leftContext == ChainX && rightContext == ChainY
          then ChainZ
          else closedChainJoin leftContext rightContext
    )
    closedChainMeet
    `shouldFailWith` ContextLatticeJoinOutsideUniverse ChainX ChainY ChainZ

testClosedOrderRejectsOutsideMeet :: Assertion
testClosedOrderRejectsOutsideMeet =
  contextLatticeFromClosedOrder
    ChainTop
    ChainBottom
    closedChainUniverse
    closedChainLeq
    closedChainJoin
    ( \leftContext rightContext ->
        if leftContext == ChainX && rightContext == ChainY
          then ChainZ
          else closedChainMeet leftContext rightContext
    )
    `shouldFailWith` ContextLatticeMeetOutsideUniverse ChainX ChainY ChainZ

testClosedOrderRejectsIntransitiveOrder :: Assertion
testClosedOrderRejectsIntransitiveOrder =
  contextLatticeFromClosedOrder
    ChainTop
    ChainBottom
    [ChainBottom, ChainX, ChainY, ChainZ, ChainTop]
    intransitiveLeq
    (\_ _ -> ChainTop)
    (\_ _ -> ChainBottom)
    `shouldFailWith` ContextLatticeNotTransitive ChainX ChainY ChainZ
  where
    intransitiveLeq leftContext rightContext =
      leftContext == rightContext
        || leftContext == ChainBottom
        || rightContext == ChainTop
        || (leftContext == ChainX && rightContext == ChainY)
        || (leftContext == ChainY && rightContext == ChainZ)

testDiamondLattice :: Assertion
testDiamondLattice =
  case compileContextLattice diamondUniverse diamondDecl of
    Left err ->
      assertFailure ("expected diamond lattice, got " <> show err)
    Right lattice -> do
      clTop lattice @?= DiamondTop
      clBottom lattice @?= DiamondBottom
      contextLatticeElements lattice @?= [DiamondBottom, DiamondLeft, DiamondRight, DiamondTop]
      joinContext lattice DiamondLeft DiamondRight @?= Right DiamondTop
      meetContext lattice DiamondLeft DiamondRight @?= Right DiamondBottom
      leqContext lattice DiamondBottom DiamondTop @?= Right True
      leqContext lattice DiamondLeft DiamondTop @?= Right True
      leqContext lattice DiamondRight DiamondTop @?= Right True
      assertBool "left and right are incomparable" $
        leqContext lattice DiamondLeft DiamondRight == Right False
          && leqContext lattice DiamondRight DiamondLeft == Right False

testResidentContextOperations :: Assertion
testResidentContextOperations =
  case compileContextLattice diamondUniverse diamondDecl of
    Left err ->
      assertFailure ("expected diamond lattice, got " <> show err)
    Right lattice ->
      case compileContextHeyting lattice of
        Left err ->
          assertFailure ("expected diamond Heyting proof, got " <> show err)
        Right heyting ->
          withResidentHeytingContext heyting $ \heytingContext -> do
            let residentContext = residentHeytingBaseContext heytingContext
            leftElement <- requireRight (checkResidentContext residentContext DiamondLeft)
            rightElement <- requireRight (checkResidentContext residentContext DiamondRight)
            bottomElement <- requireRight (checkResidentContext residentContext DiamondBottom)
            let joinResult = residentJoin residentContext leftElement rightElement
                meetResult = residentMeet residentContext leftElement rightElement
                (joinKey, meetKey) =
                  residentJoinMeetKeys
                    residentContext
                    (residentContextElementKey leftElement)
                    (residentContextElementKey rightElement)
                implicationResult = residentImplies heytingContext leftElement bottomElement
            residentContextElementValue joinResult @?= DiamondTop
            residentContextElementValue meetResult @?= DiamondBottom
            residentContextElementValue (residentContextElementForKey residentContext joinKey) @?= DiamondTop
            residentContextElementValue (residentContextElementForKey residentContext meetKey) @?= DiamondBottom
            residentContextElementValue implicationResult @?= DiamondRight

testResidentSupportClosure :: Assertion
testResidentSupportClosure =
  case compileContextLattice diamondUniverse diamondDecl of
    Left err ->
      assertFailure ("expected diamond lattice, got " <> show err)
    Right lattice ->
      withResidentContext lattice $ \residentContext -> do
        support <- requireRight (residentSupportFromElements residentContext [DiamondLeft])
        topElement <- requireRight (checkResidentContext residentContext DiamondTop)
        rightElement <- requireRight (checkResidentContext residentContext DiamondRight)
        let cachedSupport = residentSupportWithClosure residentContext support
        residentSupportContainsElement residentContext cachedSupport topElement @?= True
        residentSupportContainsElement residentContext cachedSupport rightElement @?= False
        fmap residentContextElementValue (residentSupportReachableElements residentContext support)
          @?= [DiamondLeft, DiamondTop]

testBooleanCubeLattice :: Assertion
testBooleanCubeLattice =
  case compileContextLattice booleanCubeUniverse booleanCubeDecl of
    Left err ->
      assertFailure ("expected boolean cube lattice, got " <> show err)
    Right lattice ->
      case compileContextHeyting lattice of
        Left err -> assertFailure ("expected boolean Heyting proof, got " <> show err)
        Right heyting -> do
          contextLatticeElements lattice @?= [0 .. 7]
          joinContext lattice 3 5 @?= Right 7
          meetContext lattice 3 5 @?= Right 1
          impliesContext heyting 3 1 @?= Right 5
          leqContext lattice 3 7 @?= Right True
          leqContext lattice 3 5 @?= Right False

booleanCubeUniverse :: Set.Set Int
booleanCubeUniverse =
  Set.fromAscList [0 .. 7]

booleanCubeDecl :: ContextOrderDecl Int
booleanCubeDecl =
  contextOrderDecl 7 0 booleanCubeCoverEdges

booleanCubeCoverEdges :: [(Int, Int)]
booleanCubeCoverEdges =
  [ (mask, mask + bitValue)
  | mask <- [0 .. 7],
    bitValue <- [1, 2, 4],
    mask .&. bitValue == 0
  ]

testUnknownJoinContext :: Assertion
testUnknownJoinContext =
  joinContext (singletonContextLattice DiamondBottom) DiamondBottom DiamondTop
    @?= Left (ContextLatticeUnknownContext DiamondTop)

testUnknownMeetContext :: Assertion
testUnknownMeetContext =
  meetContext (singletonContextLattice DiamondBottom) DiamondTop DiamondBottom
    @?= Left (ContextLatticeUnknownContext DiamondTop)

testUnknownLeqContext :: Assertion
testUnknownLeqContext =
  leqContext (singletonContextLattice DiamondBottom) DiamondBottom DiamondTop
    @?= Left (ContextLatticeUnknownContext DiamondTop)

testRejectsNonMonotoneFixpoint :: Assertion
testRejectsNonMonotoneFixpoint =
  case boolLattice of
    Left compileError ->
      assertFailure ("unexpected compile failure: " <> show compileError)
    Right lattice ->
      leastContextFixpoint lattice not
        @?= Left (ContextEndomapNotMonotone False True True False)

testLeastContextFixpointAscendsChain :: Assertion
testLeastContextFixpointAscendsChain =
  case chainLattice of
    Left err ->
      assertFailure ("expected chain lattice, got " <> show err)
    Right lattice ->
      leastContextFixpoint lattice advanceChain
        @?= Right ChainTop

testGreatestContextFixpointDescendsChain :: Assertion
testGreatestContextFixpointDescendsChain =
  case chainLattice of
    Left err ->
      assertFailure ("expected chain lattice, got " <> show err)
    Right lattice ->
      greatestContextFixpoint lattice retreatChain
        @?= Right ChainBottom

testContextFixpointClosureObstruction :: Assertion
testContextFixpointClosureObstruction =
  leastContextFixpoint (singletonContextLattice DiamondBottom) (const DiamondTop)
    @?= Left (ContextEndomapOutsideUniverse DiamondBottom DiamondTop)

testImpliesContextUnique :: Assertion
testImpliesContextUnique =
  case (compileContextLattice diamondUniverse diamondDecl, chainLattice) of
    (Right diamond, Right chain) -> do
      diamondHeyting <- requireRight (compileContextHeyting diamond)
      chainHeyting <- requireRight (compileContextHeyting chain)
      impliesContext diamondHeyting DiamondLeft DiamondRight @?= Right DiamondRight
      impliesContext diamondHeyting DiamondLeft DiamondBottom @?= Right DiamondRight
      impliesContext chainHeyting ChainX ChainY @?= Right ChainTop
    (Left err, _) ->
      assertFailure ("expected diamond lattice, got " <> show err)
    (_, Left err) ->
      assertFailure ("expected chain lattice, got " <> show err)

testRejectsM3Residual :: Assertion
testRejectsM3Residual =
  case m3Lattice of
    Left compileError ->
      assertFailure ("unexpected lattice failure: " <> show compileError)
    Right lattice ->
      compileContextHeyting lattice
        `shouldFailWith` ContextResidualDoesNotExist M3A M3Bottom M3Top

testBooleanAdjunction :: Assertion
testBooleanAdjunction =
  case boolean4 of
    Left compileError ->
      assertFailure ("unexpected lattice failure: " <> show compileError)
    Right lattice ->
      case compileContextHeyting lattice of
        Left heytingError ->
          assertFailure ("unexpected Heyting failure: " <> show heytingError)
        Right heyting -> do
          impliesContext heyting 1 0 @?= Right 2
          for_ [(a, b, x) | a <- [0 .. 3], b <- [0 .. 3], x <- [0 .. 3]] $
            \(a, b, x) -> assertAdjunction lattice heyting a b x

testDenseHeytingAdjunction :: Assertion
testDenseHeytingAdjunction =
  case divisor12Lattice of
    Left compileError ->
      assertFailure ("unexpected lattice failure: " <> show compileError)
    Right lattice ->
      case compileContextHeyting lattice of
        Left heytingError ->
          assertFailure ("unexpected Heyting failure: " <> show heytingError)
        Right heyting -> do
          impliesContext heyting 2 3 @?= Right 3
          impliesContext heyting 4 2 @?= Right 6
          impliesContext heyting 6 2 @?= Right 4
          traverse_
            ( \(a, b, x) ->
                assertAdjunction lattice heyting a b x
            )
            [ (a, b, x)
            | a <- divisor12Universe,
              b <- divisor12Universe,
              x <- divisor12Universe
            ]

testTablelessDistributiveMatchesDense :: Assertion
testTablelessDistributiveMatchesDense = do
  defaultLattice <- requireRight divisor12Lattice
  tablelessLattice <- requireRight divisor12TablelessLattice
  assertLatticeAgreement "divisor12" divisor12Universe defaultLattice tablelessLattice
  defaultHeyting <- requireRight (compileContextHeyting defaultLattice)
  tablelessHeyting <- requireRight (compileContextHeyting tablelessLattice)
  assertHeytingAgreement "divisor12" divisor12Universe defaultHeyting tablelessHeyting
  traverse_
    ( \(antecedent, consequent, candidate) ->
        assertAdjunction tablelessLattice tablelessHeyting antecedent consequent candidate
    )
    [ (antecedent, consequent, candidate)
    | antecedent <- divisor12Universe,
      consequent <- divisor12Universe,
      candidate <- divisor12Universe
    ]

testTablelessN5MatchesDense :: Assertion
testTablelessN5MatchesDense = do
  defaultLattice <- requireRight n5Lattice
  tablelessLattice <- requireRight n5TablelessLattice
  assertLatticeAgreement "N5" (Set.toAscList n5Universe) defaultLattice tablelessLattice
  compileContextHeyting defaultLattice
    `shouldFailWith` ContextResidualDoesNotExist N5C N5A N5Top
  compileContextHeyting tablelessLattice
    `shouldFailWith` ContextResidualDoesNotExist N5C N5A N5Top

testTablelessProduct64ImplicationBoundary :: Assertion
testTablelessProduct64ImplicationBoundary =
  assertProductTablelessImplicationBoundary
    64
    [ (productContext 63 1, productContext 61 0, productContext 61 0),
      (productContext 62 0, productContext 63 1, productContext 63 1),
      (productContext 63 0, productContext 62 1, productContext 62 1)
    ]
    [61 .. 63]

testTablelessProduct66ImplicationBoundary :: Assertion
testTablelessProduct66ImplicationBoundary =
  assertProductTablelessImplicationBoundary
    66
    [ (productContext 65 1, productContext 64 0, productContext 64 0),
      (productContext 64 0, productContext 65 1, productContext 65 1),
      (productContext 63 1, productContext 64 0, productContext 65 0)
    ]
    [63 .. 65]

testRejectsDenseN5Residual :: Assertion
testRejectsDenseN5Residual =
  case n5Lattice of
    Left compileError ->
      assertFailure ("unexpected lattice failure: " <> show compileError)
    Right lattice ->
      compileContextHeyting lattice
        `shouldFailWith` ContextResidualDoesNotExist N5C N5A N5Top

assertAdjunction :: (Ord c, Show c) => ContextLattice c -> ContextHeyting c -> c -> c -> c -> Assertion
assertAdjunction lattice heyting antecedent consequent candidate =
  case (impliesContext heyting antecedent consequent, meetContext lattice candidate antecedent) of
    (Right residual, Right candidateMeet) ->
      case (leqContext lattice candidate residual, leqContext lattice candidateMeet consequent) of
        (Right leftSide, Right rightSide) -> leftSide @?= rightSide
        (leftResult, rightResult) ->
          assertFailure ("unexpected lookup failure: " <> show (leftResult, rightResult))
    (residualResult, meetResult) ->
      assertFailure ("unexpected lookup failure: " <> show (residualResult, meetResult))

assertLatticeAgreement :: (Ord c, Show c) => String -> [c] -> ContextLattice c -> ContextLattice c -> Assertion
assertLatticeAgreement label universe expected actual = do
  assertEqual (label <> " elements") (contextLatticeElements expected) (contextLatticeElements actual)
  assertEqual (label <> " top") (clTop expected) (clTop actual)
  assertEqual (label <> " bottom") (clBottom expected) (clBottom actual)
  for_ universe $ \contextValue -> do
    assertEqual (label <> " upper covers of " <> show contextValue) (upperCovers expected contextValue) (upperCovers actual contextValue)
    assertEqual (label <> " lower covers of " <> show contextValue) (lowerCovers expected contextValue) (lowerCovers actual contextValue)
  for_ [(leftContext, rightContext) | leftContext <- universe, rightContext <- universe] $
    \(leftContext, rightContext) -> do
      assertEqual (label <> " leq " <> show (leftContext, rightContext)) (leqContext expected leftContext rightContext) (leqContext actual leftContext rightContext)
      assertEqual (label <> " join " <> show (leftContext, rightContext)) (joinContext expected leftContext rightContext) (joinContext actual leftContext rightContext)
      assertEqual (label <> " meet " <> show (leftContext, rightContext)) (meetContext expected leftContext rightContext) (meetContext actual leftContext rightContext)

assertHeytingAgreement :: (Ord c, Show c) => String -> [c] -> ContextHeyting c -> ContextHeyting c -> Assertion
assertHeytingAgreement label universe expected actual =
  for_ [(antecedent, consequent) | antecedent <- universe, consequent <- universe] $
    \(antecedent, consequent) ->
      assertEqual (label <> " implication " <> show (antecedent, consequent)) (impliesContext expected antecedent consequent) (impliesContext actual antecedent consequent)

assertProductTablelessImplicationBoundary :: Int -> [(Int, Int, Int)] -> [Int] -> Assertion
assertProductTablelessImplicationBoundary rowCount implicationExamples adjunctionRows = do
  lattice <- requireRight (productLatticeWith noBinaryOperationTablesLimits rowCount)
  heyting <- requireRight (compileContextHeyting lattice)
  for_ implicationExamples $ \(antecedent, consequent, residual) ->
    assertEqual
      ("product implication " <> show (productCoordinates antecedent, productCoordinates consequent))
      (Right residual)
      (impliesContext heyting antecedent consequent)
  traverse_
    ( \(antecedent, consequent, candidate) ->
        assertAdjunction lattice heyting antecedent consequent candidate
    )
    [ (antecedent, consequent, candidate)
    | antecedent <- targetedProductContexts adjunctionRows,
      consequent <- targetedProductContexts adjunctionRows,
      candidate <- targetedProductContexts adjunctionRows
    ]

targetedProductContexts :: [Int] -> [Int]
targetedProductContexts rows =
  [productContext row column | row <- rows, column <- [0, 1]]

productLatticeWith :: ContextCompileLimits -> Int -> Either (ContextLatticeCompileError Int) (ContextLattice Int)
productLatticeWith limits rowCount =
  contextLatticeFromClosedOrderWith
    limits
    (productContext (rowCount - 1) 1)
    (productContext 0 0)
    (productUniverse rowCount)
    productLeq
    productJoin
    productMeet

productUniverse :: Int -> [Int]
productUniverse rowCount =
  [productContext row column | row <- [0 .. rowCount - 1], column <- [0, 1]]

productContext :: Int -> Int -> Int
productContext row column =
  row * 2 + column

productCoordinates :: Int -> (Int, Int)
productCoordinates contextValue =
  (contextValue `div` 2, contextValue `mod` 2)

productLeq :: Int -> Int -> Bool
productLeq leftContext rightContext =
  leftRow <= rightRow && leftColumn <= rightColumn
  where
    (leftRow, leftColumn) = productCoordinates leftContext
    (rightRow, rightColumn) = productCoordinates rightContext

productJoin :: Int -> Int -> Int
productJoin leftContext rightContext =
  productContext (max leftRow rightRow) (max leftColumn rightColumn)
  where
    (leftRow, leftColumn) = productCoordinates leftContext
    (rightRow, rightColumn) = productCoordinates rightContext

productMeet :: Int -> Int -> Int
productMeet leftContext rightContext =
  productContext (min leftRow rightRow) (min leftColumn rightColumn)
  where
    (leftRow, leftColumn) = productCoordinates leftContext
    (rightRow, rightColumn) = productCoordinates rightContext


type PosetLeq = Int -> Int -> Bool

type DownsetCase = (String, Int, PosetLeq, Int, [(Int, Int, Int)])

testDownsetGeneratedHeytingAdjunction :: Assertion
testDownsetGeneratedHeytingAdjunction =
  for_ downsetLatticeCases $ \(caseName, elementCount, posetLeq, expectedSize, implicationWitnesses) -> do
    let universe = downsetUniverse elementCount posetLeq
    assertEqual (caseName <> " downset count") expectedSize (length universe)
    case downsetLattice elementCount posetLeq of
      Left compileError ->
        assertFailure (caseName <> " unexpected lattice failure: " <> show compileError)
      Right lattice ->
        case compileContextHeyting lattice of
          Left heytingError ->
            assertFailure (caseName <> " unexpected Heyting failure: " <> show heytingError)
          Right heyting -> do
            for_ implicationWitnesses $ \(antecedent, consequent, residual) ->
              impliesContext heyting antecedent consequent @?= Right residual
            traverse_
              ( \(antecedent, consequent, candidate) ->
                  assertAdjunction lattice heyting antecedent consequent candidate
              )
              [ (antecedent, consequent, candidate)
              | antecedent <- universe,
                consequent <- universe,
                candidate <- universe
              ]

downsetLatticeCases :: [DownsetCase]
downsetLatticeCases =
  [ ( "two-point antichain",
      2,
      discretePosetLeq,
      4,
      [(1, 0, 2)]
    ),
    ( "two-element chain",
      2,
      chain2PlusIsolatedPosetLeq,
      3,
      [(1, 0, 0)]
    ),
    ( "vee poset",
      3,
      veePosetLeq,
      5,
      [(1, 2, 2)]
    ),
    ( "2x3 grid from a chain plus an isolated point",
      3,
      chain2PlusIsolatedPosetLeq,
      6,
      [(1, 0, 4)]
    )
  ]

downsetLattice :: Int -> PosetLeq -> Either (ContextLatticeCompileError Int) (ContextLattice Int)
downsetLattice elementCount posetLeq =
  compileContextLattice
    (Set.fromList universe)
    (contextOrderDecl (downsetTop elementCount) 0 (downsetCoverEdges universe))
  where
    universe = downsetUniverse elementCount posetLeq

downsetUniverse :: Int -> PosetLeq -> [Int]
downsetUniverse elementCount posetLeq =
  [mask | mask <- [0 .. downsetTop elementCount], isDownset mask]
  where
    elements = [0 .. elementCount - 1]
    isDownset mask =
      all
        ( \upper ->
            not (maskContains mask upper)
              || all
                ( \lower ->
                    not (posetLeq lower upper) || maskContains mask lower
                )
                elements
        )
        elements

downsetCoverEdges :: [Int] -> [(Int, Int)]
downsetCoverEdges universe =
  [ (lower, upper)
  | lower <- universe,
    upper <- universe,
    lower /= upper,
    downsetSubset lower upper,
    not (hasStrictIntermediate lower upper)
  ]
  where
    hasStrictIntermediate lower upper =
      any
        ( \candidate ->
            candidate /= lower
              && candidate /= upper
              && downsetSubset lower candidate
              && downsetSubset candidate upper
        )
        universe

downsetTop :: Int -> Int
downsetTop elementCount =
  2 ^ elementCount - 1

downsetSubset :: Int -> Int -> Bool
downsetSubset lower upper =
  lower .&. upper == lower

maskContains :: Int -> Int -> Bool
maskContains mask elementIndex =
  mask .&. (2 ^ elementIndex) /= 0

discretePosetLeq :: PosetLeq
discretePosetLeq left right =
  left == right

chain2PlusIsolatedPosetLeq :: PosetLeq
chain2PlusIsolatedPosetLeq left right =
  left == right || (left == 0 && right == 1)

veePosetLeq :: PosetLeq
veePosetLeq left right =
  left == right || (left == 0 && right == 2) || (left == 1 && right == 2)

testCoverOrientation :: Assertion
testCoverOrientation =
  case chain3 of
    Left compileError ->
      assertFailure ("unexpected lattice failure: " <> show compileError)
    Right lattice -> do
      strictOrderPairs lattice @?= [(0, 1), (0, 2), (1, 2)]
      coverPairs lattice @?= [(0, 1), (1, 2)]
      upperCovers lattice 0 @?= Right [1]
      lowerCovers lattice 2 @?= Right [1]

testUnknownTop :: Assertion
testUnknownTop =
  compileContextLattice
    (Set.fromList [DiamondBottom, DiamondLeft])
    (contextOrderDecl DiamondTop DiamondBottom [])
    `shouldFailWith` ContextLatticeUnknownTop DiamondTop

testUnknownBottom :: Assertion
testUnknownBottom =
  compileContextLattice
    (Set.fromList [DiamondLeft, DiamondTop])
    (contextOrderDecl DiamondTop DiamondBottom [])
    `shouldFailWith` ContextLatticeUnknownBottom DiamondBottom

testUnknownRelationEndpoint :: Assertion
testUnknownRelationEndpoint =
  compileContextLattice
    (Set.fromList [DiamondBottom, DiamondTop])
    (contextOrderDecl DiamondTop DiamondBottom [(DiamondBottom, DiamondLeft)])
    `shouldFailWith` ContextLatticeUnknownRelationEndpoint (DiamondBottom, DiamondLeft)

testAntisymmetryViolation :: Assertion
testAntisymmetryViolation =
  compileContextLattice
    (Set.fromList [DiamondBottom, DiamondTop])
    ( contextOrderDecl
        DiamondTop
        DiamondBottom
        [ (DiamondBottom, DiamondTop),
          (DiamondTop, DiamondBottom)
        ]
    )
    `shouldFailWith` ContextLatticeAntisymmetryViolation DiamondBottom DiamondTop

testTopNotGreatest :: Assertion
testTopNotGreatest =
  compileContextLattice
    (Set.fromList [DiamondBottom, DiamondLeft, DiamondTop])
    (contextOrderDecl DiamondTop DiamondBottom [(DiamondBottom, DiamondTop)])
    `shouldFailWith` ContextLatticeTopNotGreatest DiamondLeft

testBottomNotLeast :: Assertion
testBottomNotLeast =
  compileContextLattice
    (Set.fromList [DiamondBottom, DiamondLeft, DiamondTop])
    ( contextOrderDecl
        DiamondTop
        DiamondBottom
        [ (DiamondLeft, DiamondTop),
          (DiamondBottom, DiamondTop)
        ]
    )
    `shouldFailWith` ContextLatticeBottomNotLeast DiamondLeft

testMissingJoin :: Assertion
testMissingJoin =
  compileContextLattice
    (Set.fromList [NBottom, NA, NB, NU, NV, NTop])
    ( contextOrderDecl
        NTop
        NBottom
        [ (NBottom, NA),
          (NBottom, NB),
          (NA, NU),
          (NB, NU),
          (NA, NV),
          (NB, NV),
          (NU, NTop),
          (NV, NTop)
        ]
    )
    `shouldFailWith` ContextLatticeJoinDoesNotExist NA NB (Set.fromList [NU, NV])

shouldFailWith :: (Eq errorValue, Show errorValue) => Either errorValue value -> errorValue -> Assertion
shouldFailWith result expected =
  case result of
    Left actual -> actual @?= expected
    Right _ -> assertFailure "expected lattice compile failure"

requireRight :: Show errorValue => Either errorValue value -> IO value
requireRight result =
  case result of
    Left errorValue ->
      assertFailure ("expected Right, got " <> show errorValue)
    Right value ->
      pure value

diamondUniverse :: Set.Set DiamondContext
diamondUniverse =
  Set.fromList [DiamondBottom, DiamondLeft, DiamondRight, DiamondTop]

diamondDecl :: ContextOrderDecl DiamondContext
diamondDecl =
  contextOrderDecl
    DiamondTop
    DiamondBottom
    [ (DiamondBottom, DiamondLeft),
      (DiamondBottom, DiamondRight),
      (DiamondLeft, DiamondTop),
      (DiamondRight, DiamondTop)
    ]

chainLattice :: Either (ContextLatticeCompileError ChainContext) (ContextLattice ChainContext)
chainLattice =
  contextLatticeFromClosedOrder
    ChainTop
    ChainBottom
    closedChainUniverse
    closedChainLeq
    closedChainJoin
    closedChainMeet

advanceChain :: ChainContext -> ChainContext
advanceChain contextValue =
  case contextValue of
    ChainBottom -> ChainX
    ChainX -> ChainY
    ChainY -> ChainTop
    ChainZ -> ChainTop
    ChainTop -> ChainTop

retreatChain :: ChainContext -> ChainContext
retreatChain contextValue =
  case contextValue of
    ChainTop -> ChainY
    ChainY -> ChainX
    ChainX -> ChainBottom
    ChainZ -> ChainBottom
    ChainBottom -> ChainBottom

boolLattice :: Either (ContextLatticeCompileError Bool) (ContextLattice Bool)
boolLattice =
  contextLatticeFromClosedOrder True False [False, True] (<=) (||) (&&)

noBinaryOperationTablesLimits :: ContextCompileLimits
noBinaryOperationTablesLimits =
  defaultContextCompileLimits {cclMaximumBinaryTableBytes = Just 0}

m3Lattice :: Either (ContextLatticeCompileError M3) (ContextLattice M3)
m3Lattice =
  compileContextLattice
    (Set.fromList [M3Bottom, M3A, M3B, M3C, M3Top])
    ( contextOrderDecl
        M3Top
        M3Bottom
        [ (M3Bottom, M3A),
          (M3Bottom, M3B),
          (M3Bottom, M3C),
          (M3A, M3Top),
          (M3B, M3Top),
          (M3C, M3Top)
        ]
    )

n5Universe :: Set.Set N5
n5Universe =
  Set.fromList [N5Bottom, N5A, N5B, N5C, N5Top]

n5Decl :: ContextOrderDecl N5
n5Decl =
  contextOrderDecl
    N5Top
    N5Bottom
    [ (N5Bottom, N5A),
      (N5A, N5C),
      (N5C, N5Top),
      (N5Bottom, N5B),
      (N5B, N5Top)
    ]

n5Lattice :: Either (ContextLatticeCompileError N5) (ContextLattice N5)
n5Lattice =
  compileContextLattice n5Universe n5Decl

n5TablelessLattice :: Either (ContextLatticeCompileError N5) (ContextLattice N5)
n5TablelessLattice =
  compileContextLatticeWith noBinaryOperationTablesLimits n5Universe n5Decl

boolean4 :: Either (ContextLatticeCompileError Int) (ContextLattice Int)
boolean4 =
  contextLatticeFromClosedOrder
    3
    0
    [0 .. 3]
    (\left right -> left .&. right == left)
    (.|.)
    (.&.)

divisor12Lattice :: Either (ContextLatticeCompileError Int) (ContextLattice Int)
divisor12Lattice =
  contextLatticeFromClosedOrder
    12
    1
    divisor12Universe
    divisor12Leq
    lcm
    gcd


divisor12TablelessLattice :: Either (ContextLatticeCompileError Int) (ContextLattice Int)
divisor12TablelessLattice =
  contextLatticeFromClosedOrderWith
    noBinaryOperationTablesLimits
    12
    1
    divisor12Universe
    divisor12Leq
    lcm
    gcd

divisor12Universe :: [Int]
divisor12Universe =
  [1, 2, 3, 4, 6, 12]

divisor12Leq :: Int -> Int -> Bool
divisor12Leq left right =
  left > 0 && right `mod` left == 0

chain3 :: Either (ContextLatticeCompileError Int) (ContextLattice Int)
chain3 =
  contextLatticeFromClosedOrder 2 0 [0, 1, 2] (<=) max min
