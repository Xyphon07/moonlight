{-# LANGUAGE GHC2024 #-}

-- | Property-based law congregation for finite context lattices.
--
-- Where 'FiniteLatticeSpec' pins named witnesses (the diamond, M3, N5,
-- divisor lattices), this module quantifies the algebraic laws over a random
-- family of lattices. The generator is Birkhoff's representation theorem made
-- operational: the downsets of any finite poset, ordered by inclusion, form a
-- finite distributive (hence Heyting) lattice, so a random poset yields a random
-- lawful lattice for free. Each law is then exhausted over every element of the
-- generated carrier — random across lattices, total within each.
module LawSpec
  ( tests,
  )
where

import Data.Bits (bit, (.&.))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.FiniteLattice
  ( ContextHeyting,
    ContextLattice (clBottom, clTop),
    ContextLatticeCompileError,
    ContextLatticeLookupError,
    compileContextHeyting,
    compileContextLattice,
    contextLatticeElements,
    contextOrderDecl,
    coverPairs,
    greatestContextFixpoint,
    impliesContext,
    joinContext,
    leastContextFixpoint,
    leqContext,
    meetContext,
    principalSupport,
    strictOrderPairs,
    SupportBasis,
    supportBasis,
    supportReachableLatticeContexts,
    supportUnion,
    upperCovers,
  )
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.QuickCheck
  ( Gen,
    Property,
    QuickCheckTests (..),
    choose,
    conjoin,
    counterexample,
    elements,
    forAll,
    frequency,
    listOf,
    oneof,
    property,
    resize,
    sublistOf,
    testProperty,
  )

-- ---------------------------------------------------------------------------
-- Generated carriers
-- ---------------------------------------------------------------------------

-- | A poset on @[0 .. prN - 1]@, represented by its (reflexive-transitively
-- closed) order relation so the generator has a legible 'Show'.
data PosetRep = PosetRep
  { prN :: Int,
    prOrder :: Set (Int, Int)
  }
  deriving stock (Show)

posetLeq :: PosetRep -> Int -> Int -> Bool
posetLeq poset lower upper =
  Set.member (lower, upper) (prOrder poset)

-- | A random finite poset: strict edges are drawn only among @i < j@, which
-- keeps the relation acyclic (hence antisymmetric); its reflexive-transitive
-- closure is then a genuine partial order.
genPosetRep :: Gen PosetRep
genPosetRep = do
  elementCount <- choose (1, 4)
  strictEdges <-
    sublistOf
      [(lower, upper) | lower <- [0 .. elementCount - 1], upper <- [0 .. elementCount - 1], lower < upper]
  pure (PosetRep elementCount (reflexiveTransitiveClosure elementCount strictEdges))

reflexiveTransitiveClosure :: Int -> [(Int, Int)] -> Set (Int, Int)
reflexiveTransitiveClosure elementCount strictEdges =
  fixpoint (Set.fromList (strictEdges <> [(index, index) | index <- [0 .. elementCount - 1]]))
  where
    fixpoint :: Set (Int, Int) -> Set (Int, Int)
    fixpoint relation =
      let grown = Set.union relation (composeOnce relation)
       in if grown == relation then relation else fixpoint grown
    composeOnce :: Set (Int, Int) -> Set (Int, Int)
    composeOnce relation =
      Set.fromList
        [ (left, right)
        | (left, mid) <- Set.toList relation,
          (mid', right) <- Set.toList relation,
          mid == mid'
        ]

-- | A carrier plus a legible description for QuickCheck counterexamples.
data LatSample = LatSample
  { lsDescription :: String,
    lsLattice :: ContextLattice Int,
    lsElements :: [Int]
  }

instance Show LatSample where
  show sample =
    lsDescription sample <> " {elements=" <> show (lsElements sample) <> "}"

mkSample :: String -> ContextLattice Int -> LatSample
mkSample description lattice =
  LatSample description lattice (contextLatticeElements lattice)

-- | The downset lattice of a poset: elements are down-closed subsets encoded as
-- bitmasks over @[0 .. n-1]@, ordered by inclusion. Always a finite
-- distributive lattice, with @0@ (empty set) least and @2^n - 1@ (full set)
-- greatest.
downsetLattice ::
  PosetRep ->
  Either (ContextLatticeCompileError Int) (ContextLattice Int)
downsetLattice poset =
  compileContextLattice
    (Set.fromList universe)
    (contextOrderDecl (fullMask (prN poset)) 0 (downsetCoverEdges universe))
  where
    universe = downsetUniverse poset

downsetUniverse :: PosetRep -> [Int]
downsetUniverse poset =
  [mask | mask <- [0 .. fullMask (prN poset)], isDownset mask]
  where
    base = [0 .. prN poset - 1]
    isDownset mask =
      all
        ( \upper ->
            not (maskContains mask upper)
              || all (\lower -> not (posetLeq poset lower upper) || maskContains mask lower) base
        )
        base

downsetCoverEdges :: [Int] -> [(Int, Int)]
downsetCoverEdges universe =
  [ (lower, upper)
  | lower <- universe,
    upper <- universe,
    lower /= upper,
    maskSubset lower upper,
    not (hasStrictIntermediate lower upper)
  ]
  where
    hasStrictIntermediate lower upper =
      any
        ( \mid ->
            mid /= lower
              && mid /= upper
              && maskSubset lower mid
              && maskSubset mid upper
        )
        universe

fullMask :: Int -> Int
fullMask elementCount = bit elementCount - 1

maskContains :: Int -> Int -> Bool
maskContains mask index = mask .&. bit index /= 0

maskSubset :: Int -> Int -> Bool
maskSubset lower upper = lower .&. upper == lower

describePoset :: PosetRep -> String
describePoset poset =
  "downset(n="
    <> show (prN poset)
    <> ", strictOrder="
    <> show [(lower, upper) | (lower, upper) <- Set.toList (prOrder poset), lower /= upper]
    <> ")"

data SampleAttempt
  = DistributiveSampleAttempt
      !PosetRep
      !(Either (ContextLatticeCompileError Int) (ContextLattice Int))
  | NamedSampleAttempt
      !String
      !(Either (ContextLatticeCompileError Int) (ContextLattice Int))

instance Show SampleAttempt where
  show attempt =
    case attempt of
      DistributiveSampleAttempt poset _ -> describePoset poset
      NamedSampleAttempt name _ -> name

genDistributiveSampleAttempt :: Gen SampleAttempt
genDistributiveSampleAttempt = do
  poset <- genPosetRep
  pure (DistributiveSampleAttempt poset (downsetLattice poset))

-- | Named non-distributive witnesses, encoded as @Int@ carriers so every sample
-- shares one element type. M3 and N5 are lattices that are not distributive;
-- they exercise the general lattice laws without claiming Heyting structure.
namedRoster :: [(String, Either (ContextLatticeCompileError Int) (ContextLattice Int))]
namedRoster =
  [ ("M3", latticeOf [0, 1, 2, 3, 4] 4 0 [(0, 1), (0, 2), (0, 3), (1, 4), (2, 4), (3, 4)]),
    ("N5", latticeOf [0, 1, 2, 3, 4] 4 0 [(0, 1), (1, 3), (3, 4), (0, 2), (2, 4)]),
    ("diamond2x2", latticeOf [0, 1, 2, 3] 3 0 [(0, 1), (0, 2), (1, 3), (2, 3)]),
    ("chain5", latticeOf [0, 1, 2, 3, 4] 4 0 [(0, 1), (1, 2), (2, 3), (3, 4)])
  ]
  where
    latticeOf :: [Int] -> Int -> Int -> [(Int, Int)] -> Either (ContextLatticeCompileError Int) (ContextLattice Int)
    latticeOf universe topValue bottomValue edges =
      compileContextLattice (Set.fromList universe) (contextOrderDecl topValue bottomValue edges)

namedSampleAttempts :: [SampleAttempt]
namedSampleAttempts =
  fmap (uncurry NamedSampleAttempt) namedRoster

genAnySampleAttempt :: Gen SampleAttempt
genAnySampleAttempt =
  frequency
    [ (3, genDistributiveSampleAttempt),
      (2, elements namedSampleAttempts)
    ]

withSampleAttempt :: (LatSample -> Property) -> SampleAttempt -> Property
withSampleAttempt law attempt =
  case attempt of
    DistributiveSampleAttempt poset compiled ->
      withCompiledSample (describePoset poset) compiled
    NamedSampleAttempt name compiled ->
      withCompiledSample name compiled
  where
    withCompiledSample description compiled =
      case compiled of
        Left compileError ->
          counterexample
            (description <> " failed to compile: " <> show compileError)
            (property False)
        Right lattice -> law (mkSample description lattice)

-- ---------------------------------------------------------------------------
-- Resolved operation tables
-- ---------------------------------------------------------------------------

-- | Total join/meet/order tables for a carrier, resolved once from the checked
-- @Either@ queries. Every lookup is over declared elements, so a 'Left' here is
-- itself a defect and fails the surrounding property.
data Tables = Tables
  { tEls :: [Int],
    tTop :: Int,
    tBot :: Int,
    tJoin :: Int -> Int -> Int,
    tMeet :: Int -> Int -> Int,
    tLeq :: Int -> Int -> Bool
  }

buildTables :: ContextLattice Int -> Either String Tables
buildTables lattice = do
  let els = contextLatticeElements lattice
  joinTable <- pairTable "join" (joinContext lattice) els
  meetTable <- pairTable "meet" (meetContext lattice) els
  leqTable <- pairTable "leq" (leqContext lattice) els
  pure
    Tables
      { tEls = els,
        tTop = clTop lattice,
        tBot = clBottom lattice,
        tJoin = \a b -> joinTable Map.! (a, b),
        tMeet = \a b -> meetTable Map.! (a, b),
        tLeq = \a b -> leqTable Map.! (a, b)
      }

pairTable ::
  Show err =>
  String ->
  (Int -> Int -> Either err result) ->
  [Int] ->
  Either String (Map (Int, Int) result)
pairTable label query els =
  Map.fromList <$> traverse resolve [(a, b) | a <- els, b <- els]
  where
    resolve (a, b) =
      case query a b of
        Left err -> Left (label <> " lookup failed @ " <> show (a, b) <> ": " <> show err)
        Right value -> Right ((a, b), value)

withTables :: LatSample -> (Tables -> Property) -> Property
withTables sample continuation =
  case buildTables (lsLattice sample) of
    Left err -> counterexample ("table build failed: " <> err) (property False)
    Right tables -> continuation tables

check :: String -> Bool -> Property
check message ok = counterexample message (property ok)

allPairs :: Tables -> [(Int, Int)]
allPairs tables = [(a, b) | a <- tEls tables, b <- tEls tables]

allTriples :: Tables -> [(Int, Int, Int)]
allTriples tables = [(a, b, c) | a <- tEls tables, b <- tEls tables, c <- tEls tables]

-- ---------------------------------------------------------------------------
-- General lattice laws (hold for every lattice, distributive or not)
-- ---------------------------------------------------------------------------

lawSemilattice :: LatSample -> Property
lawSemilattice sample =
  withTables sample $ \t ->
    conjoin $
      [check ("join idempotent @ " <> show a) (tJoin t a a == a) | a <- tEls t]
        <> [check ("meet idempotent @ " <> show a) (tMeet t a a == a) | a <- tEls t]
        <> [check ("join commutative @ " <> show (a, b)) (tJoin t a b == tJoin t b a) | (a, b) <- allPairs t]
        <> [check ("meet commutative @ " <> show (a, b)) (tMeet t a b == tMeet t b a) | (a, b) <- allPairs t]
        <> [ check ("join associative @ " <> show (a, b, c)) (tJoin t (tJoin t a b) c == tJoin t a (tJoin t b c))
           | (a, b, c) <- allTriples t
           ]
        <> [ check ("meet associative @ " <> show (a, b, c)) (tMeet t (tMeet t a b) c == tMeet t a (tMeet t b c))
           | (a, b, c) <- allTriples t
           ]

lawAbsorption :: LatSample -> Property
lawAbsorption sample =
  withTables sample $ \t ->
    conjoin $
      [check ("absorb join-of-meet @ " <> show (a, b)) (tJoin t a (tMeet t a b) == a) | (a, b) <- allPairs t]
        <> [check ("absorb meet-of-join @ " <> show (a, b)) (tMeet t a (tJoin t a b) == a) | (a, b) <- allPairs t]

lawOrderConsistency :: LatSample -> Property
lawOrderConsistency sample =
  withTables sample $ \t ->
    conjoin $
      [check ("leq iff join-absorbs @ " <> show (a, b)) (tLeq t a b == (tJoin t a b == b)) | (a, b) <- allPairs t]
        <> [check ("leq iff meet-absorbs @ " <> show (a, b)) (tLeq t a b == (tMeet t a b == a)) | (a, b) <- allPairs t]

lawPartialOrder :: LatSample -> Property
lawPartialOrder sample =
  withTables sample $ \t ->
    conjoin $
      [check ("reflexive @ " <> show a) (tLeq t a a) | a <- tEls t]
        <> [ check ("antisymmetric @ " <> show (a, b)) (not (tLeq t a b && tLeq t b a) || a == b)
           | (a, b) <- allPairs t
           ]
        <> [ check ("transitive @ " <> show (a, b, c)) (not (tLeq t a b && tLeq t b c) || tLeq t a c)
           | (a, b, c) <- allTriples t
           ]

lawBounds :: LatSample -> Property
lawBounds sample =
  withTables sample $ \t ->
    conjoin $
      [check ("bottom is least @ " <> show a) (tLeq t (tBot t) a) | a <- tEls t]
        <> [check ("top is greatest @ " <> show a) (tLeq t a (tTop t)) | a <- tEls t]
        <> [check ("join-bottom identity @ " <> show a) (tJoin t a (tBot t) == a) | a <- tEls t]
        <> [check ("meet-top identity @ " <> show a) (tMeet t a (tTop t) == a) | a <- tEls t]
        <> [check ("join-top absorbing @ " <> show a) (tJoin t a (tTop t) == tTop t) | a <- tEls t]
        <> [check ("meet-bottom absorbing @ " <> show a) (tMeet t a (tBot t) == tBot t) | a <- tEls t]

lawCoverStructure :: LatSample -> Property
lawCoverStructure sample =
  withTables sample $ \t ->
    let lattice = lsLattice sample
        strict = Set.fromList (strictOrderPairs lattice)
        expectedStrict = Set.fromList [(a, b) | (a, b) <- allPairs t, a /= b, tLeq t a b]
        covers = coverPairs lattice
     in conjoin $
          [ check "strictOrderPairs equals the strict order relation" (strict == expectedStrict),
            check "coverPairs is contained in the strict order" (Set.fromList covers `Set.isSubsetOf` strict)
          ]
            <> [ check ("cover " <> show (a, b) <> " has no strict intermediate") (null (strictIntermediates t a b))
               | (a, b) <- covers
               ]
            <> [ check ("upperCovers agrees with coverPairs @ " <> show a) (upperCoverAgrees lattice covers a)
               | a <- tEls t
               ]
  where
    strictIntermediates :: Tables -> Int -> Int -> [Int]
    strictIntermediates t a b =
      [mid | mid <- tEls t, mid /= a, mid /= b, tLeq t a mid, tLeq t mid b]
    upperCoverAgrees :: ContextLattice Int -> [(Int, Int)] -> Int -> Bool
    upperCoverAgrees lattice covers a =
      fmap Set.fromList (upperCovers lattice a) == Right (Set.fromList [upper | (lower, upper) <- covers, lower == a])

-- ---------------------------------------------------------------------------
-- Fixpoint laws (Knaster–Tarski over a monotone endomap)
-- ---------------------------------------------------------------------------

-- | A monotone endomap built as a composition of @join k@/@meet k@ steps; each
-- step is monotone, so the composite is monotone by construction.
data MonoStep
  = JoinBy Int
  | MeetBy Int
  deriving stock (Show)

genMonoSteps :: [Int] -> Gen [MonoStep]
genMonoSteps els =
  resize 5 (listOf (oneof [JoinBy <$> elements els, MeetBy <$> elements els]))

applyMonoSteps :: Tables -> [MonoStep] -> Int -> Int
applyMonoSteps t steps start =
  foldl' step start steps
  where
    step acc (JoinBy k) = tJoin t acc k
    step acc (MeetBy k) = tMeet t acc k

lawFixpoint :: LatSample -> Property
lawFixpoint sample =
  withTables sample $ \t ->
    forAll (genMonoSteps (tEls t)) $ \steps ->
      let lattice = lsLattice sample
          endomap = applyMonoSteps t steps
       in case (leastContextFixpoint lattice endomap, greatestContextFixpoint lattice endomap) of
            (Right lfp, Right gfp) ->
              conjoin $
                [ check ("least fixpoint is fixed; lfp=" <> show lfp) (endomap lfp == lfp),
                  check ("greatest fixpoint is fixed; gfp=" <> show gfp) (endomap gfp == gfp),
                  check ("lfp <= gfp; " <> show (lfp, gfp)) (tLeq t lfp gfp)
                ]
                  <> [check ("lfp <= fixed point " <> show p) (endomap p /= p || tLeq t lfp p) | p <- tEls t]
                  <> [check ("fixed point " <> show p <> " <= gfp") (endomap p /= p || tLeq t p gfp) | p <- tEls t]
            (leastResult, greatestResult) ->
              counterexample
                ("monotone endomap rejected before iteration: " <> show (leastResult, greatestResult))
                (property False)

-- ---------------------------------------------------------------------------
-- Distributive / Heyting laws (downset carriers only)
-- ---------------------------------------------------------------------------

lawDistributive :: LatSample -> Property
lawDistributive sample =
  withTables sample $ \t ->
    conjoin
      [ check
          ("meet distributes over join @ " <> show (a, b, c))
          (tMeet t a (tJoin t b c) == tJoin t (tMeet t a b) (tMeet t a c))
      | (a, b, c) <- allTriples t
      ]

lawHeyting :: LatSample -> Property
lawHeyting sample =
  withTables sample $ \t ->
    case compileContextHeyting (lsLattice sample) of
      Left err ->
        counterexample ("a distributive lattice must be Heyting, but compilation failed: " <> show err) (property False)
      Right heyting ->
        case implicationTable heyting (tEls t) of
          Left err -> counterexample err (property False)
          Right implies ->
            conjoin $
              [ check
                  ("Heyting adjunction @ " <> show (a, b, x))
                  (tLeq t x (implies a b) == tLeq t (tMeet t x a) b)
              | (a, b, x) <- allTriples t
              ]
                <> [ check
                       ("residuum is the greatest witness @ " <> show (a, b))
                       (implies a b == relativePseudocomplement t a b)
                   | (a, b) <- allPairs t
                   ]

implicationTable ::
  ContextHeyting Int ->
  [Int] ->
  Either String (Int -> Int -> Int)
implicationTable heyting els = do
  entries <- traverse resolve [(a, b) | a <- els, b <- els]
  let table = Map.fromList entries
  pure (\a b -> table Map.! (a, b))
  where
    resolve (a, b) =
      case impliesContext heyting a b of
        Left err -> Left ("implication lookup failed @ " <> show (a, b) <> ": " <> show err)
        Right value -> Right ((a, b), value)

relativePseudocomplement :: Tables -> Int -> Int -> Int
relativePseudocomplement t antecedent consequent =
  foldl' (tJoin t) (tBot t) [x | x <- tEls t, tLeq t (tMeet t x antecedent) consequent]

-- ---------------------------------------------------------------------------
-- Support laws
-- ---------------------------------------------------------------------------

lawSupport :: LatSample -> Property
lawSupport sample =
  withTables sample $ \t ->
    let lattice = lsLattice sample
     in conjoin $
          [ check
              ("principal support reaches the up-closure @ " <> show a)
              ( fmap Set.fromList (supportReachableLatticeContexts lattice (principalSupport a))
                  == Right (Set.fromList [x | x <- tEls t, tLeq t a x])
              )
          | a <- tEls t
          ]
            <> [ check ("support union commutes @ " <> show (a, b)) (unionOf lattice a b == unionOf lattice b a)
               | (a, b) <- allPairs t
               ]
  where
    unionOf :: ContextLattice Int -> Int -> Int -> Either (ContextLatticeLookupError Int) (SupportBasis Int)
    unionOf lattice a b = do
      supportA <- supportBasis lattice [a]
      supportB <- supportBasis lattice [b]
      supportUnion lattice supportA supportB

-- ---------------------------------------------------------------------------
-- Generator soundness
-- ---------------------------------------------------------------------------

prop_downsetsCompile :: Property
prop_downsetsCompile =
  forAll genPosetRep $ \poset ->
    case downsetLattice poset of
      Right _ -> property True
      Left err -> counterexample ("downset construction failed to compile: " <> show err) (property False)

prop_namedRosterCompiles :: Property
prop_namedRosterCompiles =
  conjoin (fmap (withSampleAttempt (const (property True))) namedSampleAttempts)

-- ---------------------------------------------------------------------------
-- Suite
-- ---------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup
    "finite lattice laws (property-based)"
    [ localOption (QuickCheckTests 300) $
        testGroup
          "generators are lawful"
          [ testProperty "downsets of a random poset always compile" prop_downsetsCompile,
            testProperty "named non-distributive witnesses compile" prop_namedRosterCompiles
          ],
      localOption (QuickCheckTests 120) $
        testGroup
          "order and lattice axioms"
          [ testProperty "leq is a partial order" (forAll genAnySampleAttempt (withSampleAttempt lawPartialOrder)),
            testProperty "join and meet are semilattices" (forAll genAnySampleAttempt (withSampleAttempt lawSemilattice)),
            testProperty "absorption holds" (forAll genAnySampleAttempt (withSampleAttempt lawAbsorption)),
            testProperty "order agrees with join and meet" (forAll genAnySampleAttempt (withSampleAttempt lawOrderConsistency)),
            testProperty "top and bottom are the bounds" (forAll genAnySampleAttempt (withSampleAttempt lawBounds)),
            testProperty "covers are the transitive reduction of the order" (forAll genAnySampleAttempt (withSampleAttempt lawCoverStructure)),
            testProperty "support union commutes and reaches up-closures" (forAll genAnySampleAttempt (withSampleAttempt lawSupport))
          ],
      localOption (QuickCheckTests 120) $
        testGroup
          "fixpoints"
          [testProperty "monotone endomaps have bracketing least and greatest fixpoints" (forAll genAnySampleAttempt (withSampleAttempt lawFixpoint))],
      localOption (QuickCheckTests 100) $
        testGroup
          "distributive and Heyting structure"
          [ testProperty "meet distributes over join on downset lattices" (forAll genDistributiveSampleAttempt (withSampleAttempt lawDistributive)),
            testProperty "implication is the relative pseudocomplement (adjunction)" (forAll genDistributiveSampleAttempt (withSampleAttempt lawHeyting))
          ]
    ]
