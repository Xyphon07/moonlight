{-|
The single import of the @moonlight-algebra@ tower: the named structures of
abstract algebra — groups, lattices, rings and their domains, modules and
polynomials — as law-governed classes and concrete carriers layered over
"Moonlight.Core". Each structure is a thin class or newtype boundary whose laws
are stated in its own module and machine-checked in the law suite.

The operation-bearing numeric classes — @AdditiveMonoid@, @AdditiveGroup@,
@MultiplicativeMonoid@, @Semiring@, @Ring@, @Field@ — are owned by
"Moonlight.Core" and re-exported here; @moonlight-algebra@ adds the stronger
refinements, the concrete carriers, and the constructions, reusing core's
@zero@, @one@, @add@ and @mul@ rather than defining a second arithmetic.

For finer control, import the individual @Moonlight.Algebra.Pure.*@ modules
directly. The re-exported tower, by family:

* Groups — standard @Semigroup@/@Monoid@, the @Group@/@AbelianGroup@ refinements,
  the operation-selecting @Additive@/@Multiplicative@ wrappers, and @LaneVector@,
  a 16-lane @Word64@ abelian group.
* Free structures and actions — @FreeMonoid@, @FreeAbelianGroup@, @EndoPatch@,
  and monoid @Action@s with their invertible, group-acting refinement.
* Lattices — join/meet semilattices up through distributive, Heyting and Boolean
  algebras, quantales (commutative, residuated) with the Viterbi, Łukasiewicz
  and tropical carriers, and the two-element sign @Orientation@ group. Compiled
  finite lattices live in the public @finite-lattice@ sublibrary, @Moonlight.FiniteLattice@.
* Rings and arithmetic — the semiring and commutative-ring laws extended with the
  @IntegralDomain@, @GCDDomain@ and @EuclideanDomain@ refinements, modular
  arithmetic (@Zn@), quotient rings @R/(n)@ (@Quotient@), @NumberTheory@ and @GCD@.
* Modules and magnitude — @Module@, free modules, vector and bilinear spaces over
  a field, and real-valued @Magnitude@ for obstructions.
* Polynomials — univariate @Polynomial@s over a coefficient ring, a free module
  on monomial degrees.
* Constructions — @PowerSet@ lattices, finite n-fold @Product@ algebras with
  coordinatewise structure, and @SparseVec@ with finite support.

The laws are named and exercised in the private law-suite sublibrary, not
restated here; each module's Haddock carries its signatures.

The operation-selecting wrappers, in one example:

> import Moonlight.Algebra
>
> Additive 3 <> Additive 5              -- Additive 8         (<> selects +)
> Multiplicative 3 <> Multiplicative 5  -- Multiplicative 15  (<> selects *)
-}
module Moonlight.Algebra
  ( -- * Standard semigroups, monoids and groups
    module Group,
    module LaneVector,
    -- * Free structures
    module FreeMonoid,
    module FreeAbelianGroup,
    module EndoPatch,
    -- * Actions
    module Action,
    -- * Lattices, quantales and orientation
    module Lattice,
    module Quantale,
    module Orientation,
    -- * Rings, modular arithmetic and number theory
    module Ring,
    module Zn,
    module NumberTheory,
    module GCD,
    -- * Modules, vector spaces and magnitude
    module Module,
    module Magnitude,
    -- * Polynomials
    module Polynomial,
    -- * Power sets, products and quotients
    module PowerSet,
    module Product,
    module Quotient,
    -- * Sparse vectors
    module SparseVec,
    -- * Selected re-exports from "Moonlight.Core"
    AdditiveGroup,
    AdditiveMonoid (..),
    Field (..),
    MultiplicativeMonoid (..),
    Ring,
  )
where

import Moonlight.Algebra.Pure.Action as Action
import Moonlight.Algebra.Pure.EndoPatch as EndoPatch
import Moonlight.Algebra.Pure.FreeAbelianGroup as FreeAbelianGroup
import Moonlight.Algebra.Pure.FreeMonoid as FreeMonoid
import Moonlight.Algebra.Pure.GCD as GCD
import Moonlight.Algebra.Pure.Group as Group
import Moonlight.Algebra.Pure.Lattice as Lattice
import Moonlight.Algebra.Pure.LaneVector as LaneVector
import Moonlight.Algebra.Pure.Magnitude as Magnitude
import Moonlight.Algebra.Pure.Module as Module
import Moonlight.Algebra.Pure.NumberTheory as NumberTheory
import Moonlight.Algebra.Pure.Orientation as Orientation
import Moonlight.Algebra.Pure.Polynomial as Polynomial
import Moonlight.Algebra.Pure.PowerSet as PowerSet
import Moonlight.Algebra.Pure.Product as Product
import Moonlight.Algebra.Pure.Quantale as Quantale
import Moonlight.Algebra.Pure.Quotient as Quotient
import Moonlight.Algebra.Pure.Ring as Ring
import Moonlight.Algebra.Pure.SparseVec as SparseVec
import Moonlight.Algebra.Pure.Zn as Zn
import Moonlight.Core
  ( AdditiveGroup,
    AdditiveMonoid (..),
    Field (..),
    MultiplicativeMonoid (..),
    Ring,
  )
