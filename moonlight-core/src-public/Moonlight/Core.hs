{-|
The single, total import of the @moonlight-core@ foundation — the vocabulary
every layer above shares. Every name reachable from @Moonlight.Core@ is
/total/: failure is a value in the return type, never an exception. @tryInv@
returns @Maybe@, @makeSet@ returns @Either UnionFindAllocationError@, and
@fixpointBounded@ returns @Either (FixpointDivergence a)@; operations with no
failure case (@magnitude@, @neg@, @union@) return bare values.

Direct @Moonlight.Core.*@ imports elsewhere are internal-use-at-own-risk, and
the unsafe refinement constructors are not re-exported here. The one deliberate
hole in construction-time checking is "Moonlight.Core.Unsound", whose
constructors assert a refinement the checker cannot see.

The re-exported vocabulary, by role:

* Numeric tower — @Scalar@, @Numeric@ (@OrderedRing@, @OrderedField@,
  @ContinuousField@) and @ApproxEq@, with canonical and exact numbers via
  @Canon@, @CanonicalNumber@, @ExactToken@.
* Validation and refinement — @Validation@, @Refinement@, @Niche@: the
  construction-time checks that back the totality contract.
* Identity and language — @DomainId@, @Identifier@ (with the e-graph
  identifiers and @PatternVar@), @Capability@, @ModuleIdentifier@, and the
  @Language@ / @ZipMatch@ node algebra.
* Theories, patterns and matching — @Theory@, @Pattern@, @Substitution@,
  @Match@: the one-layer structural-matching seam the e-graph and rewriting
  layers stand on.
* Identity hashing — @StableHash@, @Hash@, @DenseKey@.
* Order, finiteness and fixpoints — @Finite@, @Order@, @OrdCollection@,
  @Fix.Order@, and @Fixpoint@ (with the dense solver), which report
  non-convergence as a value.
* Collections and registries — @Aggregate@, @MapAccum@, @MapInvert@, @Queue@,
  @Dedup@, @Scan@, @Relational@, checked-total @TotalRegistry@, and the
  persistent and transactional @UnionFind@.
* Programs and terms — the host-neutral @EGraph.Program@ algebra and the
  relational @Term.Database@ with its canonicalization.
* Manifests and metadata — @ProofManifest@, @LawName@, @Guidance@, @Boundary@,
  @Cardinality@, @IsoNorm@, @TypeLevel@, @Error@.

Each module's Haddock carries the signatures; the laws are named through
@LawName@ and machine-checked in the package law suites, not restated here.

Worked recipes follow: the total path returns a value, the failure path a typed
sum. Each assumes @import Moonlight.Core@.

/Persistent union-find./ @makeSet@ mints fresh classes through a checked
allocation boundary, @union@ merges them, and the structure is persistent —
older versions stay valid after a merge.

> partition :: Either UnionFindAllocationError (Bool, Bool)
> partition = do
>   (a, uf1) <- makeSet emptyUnionFind
>   (b, uf2) <- makeSet uf1
>   (c, uf3) <- makeSet uf2
>   let uf = union a b uf3
>   pure (equivalent a b uf, equivalent a c uf)   -- Right (True, False)

/Bounded fixpoints./ @fixpointBounded@ iterates a step to a fixed point under an
explicit fuel bound; non-convergence returns a typed @Left@ rather than looping.

> converge :: Either (FixpointDivergence Int) Int
> converge = fixpointBounded 100 (\n -> n `div` 2) 37   -- Right 0
>
> diverges :: Either (FixpointDivergence Int) Int
> diverges = fixpointBounded 5 (+1) 0                   -- Left (out of fuel)

/Checked total maps./ @mkTotalRegistry@ demands every key of a finite universe;
a gap is a typed @Left [missingKeys]@. In exchange @lookupTotal@ returns a bare
value once the registry is constructed (assumes @Data.List.NonEmpty (NonEmpty (..))@
and a qualified @Data.Map.Strict@).

> data Slot = Alpha | Beta | Gamma
>   deriving (Eq, Ord, Show)
>
> instance FiniteUniverse Slot where
>   finiteUniverse = Alpha :| [Beta, Gamma]
>
> palette :: Either [Slot] (TotalRegistry Slot String)
> palette = mkTotalRegistry (Map.fromList [(Alpha, "a"), (Beta, "b"), (Gamma, "c")])
>
> labelOfBeta :: Either [Slot] String
> labelOfBeta = fmap (\reg -> lookupTotal reg Beta) palette   -- Right "b"
> -- mkTotalRegistry (Map.fromList [(Alpha, "a")])  ==  Left [Beta, Gamma]

/The matching seam./ @ZipMatch@ is one-layer structural matching: @zipMatch@
succeeds iff two nodes share a shape, pairing their children positionally. It is
the seam the e-graph and rewriting layers are built on.

> data ExprF a = Lit Int | Add a a
>   deriving (Functor, Foldable, Traversable, Eq, Ord, Show)
>
> instance ZipMatch ExprF where
>   zipMatch (Lit m)     (Lit n)     | m == n = Just (Lit m)
>   zipMatch (Add a1 b1) (Add a2 b2)          = Just (Add (a1, a2) (b1, b2))
>   zipMatch _           _                    = Nothing
>
> aligned :: Maybe (ExprF (Int, Int))
> aligned = zipMatch (Add 1 2) (Add 10 20)   -- Just (Add (1,10) (2,20))
-}
module Moonlight.Core
  ( -- * Numeric tower
    module ScalarX,
    module NumericX,
    module ApproxEqX,
    -- * Canonical and exact numbers
    module CanonX,
    module CanonicalNumberX,
    module ExactTokenX,
    -- * Validation, refinement and niches
    module ValidationX,
    module RefinementX,
    module NicheX,
    -- * Type-level utilities
    module TypeLevelX,
    -- * Identifiers, domains, capabilities and language
    module DomainIdX,
    PatternVar,
    mkPatternVar,
    module IdentifierX,
    module IdentifierEGraphX,
    module CapabilityX,
    module LanguageX,
    module ModuleIdentifierX,
    -- * Theories and patterns
    module TheoryX,
    module PatternX,
    module MatchX,
    module SubstitutionX,
    -- * Errors
    module ErrorX,
    -- * Hashing
    module StableHashX,
    module HashX,
    module DenseKeyX,
    -- * Finiteness, order and fixpoints
    module FiniteX,
    module OrderX,
    module OrdCollectionX,
    module FixOrderX,
    module FixpointX,
    module FixpointDenseX,
    -- * Maps, registries, queues and normalization
    module AggregateX,
    module BoundaryX,
    module CardinalityX,
    module DedupX,
    module GuidanceX,
    module LawNameX,
    module MapAccumX,
    module MapInvertX,
    module ProofManifestX,
    module QueueX,
    module RelationalX,
    module ScanX,
    module EGraphProgramX,
    module TermDatabaseX,
    module TermDatabaseCanonicalizeX,
    module TotalRegistryX,
    module UnionFindX,
    module UnionFindTransactionX,
    module IsoNormX,
  )
where

import Moonlight.Core.Aggregate as AggregateX
import Moonlight.Core.ApproxEq as ApproxEqX
import Moonlight.Core.Boundary as BoundaryX
import Moonlight.Core.Cardinality as CardinalityX
import Moonlight.Core.Canon as CanonX
import Moonlight.Core.CanonicalNumber as CanonicalNumberX
  ( CanonicalNumber (..),
    CanonicalFiniteValue,
    mkCanonicalFiniteValue,
    mkCanonicalFiniteNumber,
    canonicalFiniteValue,
    canonicalNumberFromDouble,
    canonicalNumberToMaybeDouble,
  )
import Moonlight.Core.Capability as CapabilityX
import Moonlight.Core.Dedup as DedupX
import Moonlight.Core.DenseKey as DenseKeyX
import Moonlight.Core.DomainId as DomainIdX
import Moonlight.Core.Error as ErrorX
import Moonlight.Core.EGraph.Program as EGraphProgramX
import Moonlight.Core.ExactToken as ExactTokenX
import Moonlight.Core.Finite as FiniteX
import Moonlight.Core.Fix.Order as FixOrderX
import Moonlight.Core.Fixpoint as FixpointX
import Moonlight.Core.Fixpoint.Dense as FixpointDenseX
import Moonlight.Core.Guidance as GuidanceX
import Moonlight.Core.Hash as HashX
import Moonlight.Core.Identifier as IdentifierX
import Moonlight.Core.Identifier.EGraph (PatternVar, mkPatternVar)
import Moonlight.Core.Identifier.EGraph as IdentifierEGraphX hiding (PatternVar, mkPatternVar)
import Moonlight.Core.IsoNorm as IsoNormX
import Moonlight.Core.Language as LanguageX
import Moonlight.Core.LawName as LawNameX
import Moonlight.Core.MapAccum as MapAccumX
import Moonlight.Core.MapInvert as MapInvertX
import Moonlight.Core.Match as MatchX
import Moonlight.Core.ModuleIdentifier as ModuleIdentifierX
import Moonlight.Core.Niche as NicheX
import Moonlight.Core.Numeric as NumericX
  ( OrderedRing,
    OrderedField,
    ContinuousField,
  )
import Moonlight.Core.OrdCollection as OrdCollectionX
import Moonlight.Core.Order as OrderX
import Moonlight.Core.Pattern as PatternX
import Moonlight.Core.ProofManifest as ProofManifestX
import Moonlight.Core.Queue as QueueX
import Moonlight.Core.Refinement as RefinementX
import Moonlight.Core.Relational as RelationalX
import Moonlight.Core.Scan as ScanX
import Moonlight.Core.Scalar as ScalarX
import Moonlight.Core.StableHash as StableHashX
import Moonlight.Core.Substitution as SubstitutionX
import Moonlight.Core.Term.Database as TermDatabaseX
import Moonlight.Core.Term.Database.Canonicalize as TermDatabaseCanonicalizeX
import Moonlight.Core.Theory as TheoryX
import Moonlight.Core.TotalRegistry as TotalRegistryX
import Moonlight.Core.TypeLevel as TypeLevelX
import Moonlight.Core.UnionFind as UnionFindX
import Moonlight.Core.UnionFind.Transaction as UnionFindTransactionX
import Moonlight.Core.Validation as ValidationX
