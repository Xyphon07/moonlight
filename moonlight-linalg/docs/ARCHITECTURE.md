# moonlight-linalg architecture

`moonlight-linalg` supplies Moonlight's numerical linear-algebra tier: typed dense,
sparse, finite-field, geometric, and Krylov/spectral machinery beneath homology,
analysis, sheaf, and solver packages. This document records the sublibrary slice
map, the boundary verdicts behind it, and the numeric-floor rulings the campaign
established; `README.md` owns usage.

## Sublibrary slices

The implementation is split into graded public sublibraries, one source directory
each, with the dependency DAG enforced by cabal and guarded by
`ArchitectureSpec` slice-discipline source checks:

| slice | contents | in-package deps |
|---|---|---|
| `carrier` | `Internal.{Primitives, DenseList, Storage, VectorOps, Discrete, GF2.*}`, `Pure.Dense.{Types, Rows, Flat}` | — |
| `structured` | `Pure.Structured.{Tridiagonal, BlockTridiagonal}` | — |
| `eigen` | `Internal.Eigen.*` | carrier |
| `geometry` | `Pure.Geometry.*` | carrier |
| `dense` | `Pure.Dense.*` (rest), `Internal.Backend.*`, `Internal.Dense.*` | carrier, eigen |
| `domain` | `Pure.Domain.{Bareiss, Smith, Smith.Multimodular, Smith.Witnessed}` | carrier, dense |
| `sparse` | `Pure.Sparse.*` | carrier, structured |
| `statics` | `Pure.Statics.*` | carrier, dense, geometry |
| `spectral` | `Pure.Operator(+.Internal)`, `Pure.Krylov.*`, `Pure.Spectral.*` | carrier, eigen, sparse, structured |
| `native` | `Effect.Native.{Dispatch, LAPACK}` + cbits | carrier, dense, eigen, spectral, structured |
| `laws` | `src-laws/**` harnesses and registry | public library |
| public | `src-public/*` facades, unchanged surface | all slices |

`moonlight-linalg-native` is the sole owner of the C bits and the
Accelerate (Darwin) / LAPACK+BLAS (elsewhere) linkage; the quarantine is
cabal-enforced, not conventional.

Two edges the campaign plan predicted turned out dead in live imports and were
omitted rather than transcribed: `sparse` does not depend on `dense` (its solvers
speak carrier and structured vocabulary only), and `spectral` does not depend on
`dense` (its dense fallback speaks `Internal.Eigen.Symmetric` and the carrier
`Pure.Dense.Flat` directly). `native` depends on `eigen`, which the plan missed.

## Facades

The public library re-exports through `Moonlight.LinAlg` and the focused facades
(`.Dense`, `.Sparse`, `.Operator`, `.Spectral`, `.Krylov`, `.Native`, `.Domain`,
`.Geometry`, `.Statics`). `Pure.*`, `Internal.*`, and `Effect.*` module names are
slice ownership, not a second public vocabulary; downstream packages import
facades only, and the boundary is verified clean (zero `Internal.*`/`Pure.*`
imports outside the package).

`Moonlight.LinAlg.Native` is a true re-export facade: its implementation body
lives in `Effect.Native.Dispatch` inside the native slice, so the public module
carries no logic above the quarantine.

## Boundary verdicts

- **Carrier redraw (V1).** Three alleged DAG violations dissolved by reclassifying
  `Pure.Dense.{Types, Rows, Flat}` and `Internal.Storage` as carriers: they import
  only carrier siblings and core, so the bottom slice owns them and the
  `Internal.Storage → Rows` inversion never existed. `Pure.Structured.*` imports
  nothing in-package and owns its own low slice, dissolving the alleged
  spectral↔sparse cycle. `Dense.Decomposition → Internal.Eigen.Symmetric` is
  lawful grading (dense sits above eigen), not a wound.
- **Geometry gate (V2) — PASSED.** 2×2/3×3 symmetric eigendecomposition is
  closed-form (analytic 2×2; stable hybrid trigonometric 3×3) behind the
  preserved `eigendecomposeSymmetric{2,3}With` injection seams. The
  `GeometrySymmetricEigen{Reconstructs,Orthonormal}` laws, including generated
  near-degenerate spectra, adjudicated the gate; geometry consequently depends
  only on carrier.
- **Gram-SVD deleted (V4).** `thinSvdFullColumnRank` is one-sided Jacobi with a
  typed failure ADT; the condition-squaring Gram path is gone with no fallback.
  The reconstruction law is the reference.
- **Certification is a result distinction, not a parallel API (V5).**
  `symmetricEigenPairs` returns fast unchecked results;
  `certifySymmetricEigenResult` is the explicit certification morphism.
  `SymmetricEigenUncheckedPassesCertification` prevents drift.
- **Orphan ruling (V7).** The `-Wno-orphans` pragma was vestigial — all
  `Internal.Discrete` instances are for the locally-defined `GF2`. Deleted; the
  rebuild surfaced no genuine orphan.

## Numeric-floor rulings

- **Extreme-exponent literals are not constant-folded.** GHC 9.14 does not fold
  `fromRational` for decimal scientific literals near the `Double` range limits;
  their exact rationals survive to runtime as bignum arithmetic, and an `INLINE`
  pragma on such a constant pastes GMP division into every consumer's hot loop.
  `epsDouble`, `safeMinimumDouble`, and `maxFiniteDouble` are therefore
  `encodeFloat` forms under `NOINLINE` (`Internal.Eigen.Kernels`), and new
  numeric constants at extreme exponents must follow that shape.
- **Libraries build at `-O2`.** cabal defaults libraries to `-O1`; SpecConstr and
  LiberateCase are worth ×3–4 on the Krylov and tridiagonal inner loops
  (verified 89.3ms → 23.6ms on identical source). `-O2` lives in the cabal
  `shared-properties` block.
- **Delivery floors are absorbed, never re-tightened.** A gate above a
  certified-approximate engine must use the engine's certified tolerance tier:
  Ritz lock gates lift candidates and re-test them against
  `ritzLockToleranceBound` with demotion back to the unlocked pool, and
  agreement laws between the selected (inverse-iteration, floor ≈ 1e7·ε·n·scale)
  and dense routes assert at the residual tier, not the approx tier. A tighter
  tolerance falsifies honestly-agreeing routes.
- **Demand-aware dense fallback dispatch.** The generic CSR spectral fallback
  densifies and dense-solves at or below
  `denseSpectralFallbackDimensionThreshold = 512`. Through dimension 1,024 it
  also keeps requests for at least one quarter of the spectrum on the bounded
  dense route; smaller requests descend to thick-restart Lanczos. Structure is
  still authoritative, so diagonal, path-Laplacian, and tridiagonal sources
  bypass this generic policy. Restart is convergence-bound on clustered
  spectra; the crossover and demand provenance rows live in
  `BENCHMARKS-m4-pro.md`.
- **Certified graph-Laplacian descent.** `graphLaplacianLinearOperator` is the
  sole constructor that can retain `GraphLaplacianCSRSource` provenance after
  checked sparse assembly; an arbitrary symmetric CSR is never promoted by
  inspection. Smallest-mode requests of at most eight columns at dimension
  4,096 and above use private heavy-edge cascadic descent: aggregate masses and
  edge weights form the coarse overlap data, mass-normalized prolongation glues
  coarse sections back to the fine cover, and block Rayleigh--Ritz/Jacobi
  refinement returns only columns whose residuals were recomputed against the
  authoritative fine CSR. Stalled coarsening is the one typed inapplicability
  obstruction that descends to generic Lanczos; rank loss, invalid requests,
  and exhausted refinement remain failures. Values are derived from the pair
  owner rather than solved through a second route.

## Law surface

`moonlight-linalg-laws` exposes the effectful harnesses and a closed `LawName`
ADT of 67 laws across dense algebra, decompositions, field/GF2, domain
(Smith/Bareiss), sparse, operator, preconditioner, Krylov/spectral, geometry,
and statics owners, with a registry-totality manifest test. Tolerance tiers are
stated once in `Effect.Harness.Core` (`approxTolerance` 1e-8,
`residualTolerance` 1e-5, `orthonormalTolerance` 1e-6) — never per-law magic
epsilons. Two agreement obligations live as deterministic anchors in the test
suite rather than the registry: GF2 sparse-column rank versus packed rank
(`GF2Spec`) and native-versus-pure selected eigensolves (`KrylovSpec`), both
fixture-carrier comparisons rather than generated laws.

## Construction and validation

Boundary constructors validate and return `Either MoonlightError`:
`fromListMatrix` and friends for typed dense shapes, `mkSparseCOO`/`cooToCSR`
for sparse carriers, `mkDenseDoubleMatrixRowMajor` for flat dense storage,
`mkSymmetricTridiagonal` for structured operators, and explicit self-adjoint
operator construction in `Pure.Operator`. Failure-prone kernels carry typed
failure ADTs (SVD non-finite/rank/sweep-budget, IC(0) pivot/nullspace
breakdowns, Lanczos state sections) instead of partial numerics.

The exact `[[a]]` elimination tower survives deliberately for `Field`
polymorphism; flat unboxed `Double` kernels own the hot paths. Dense rows are
validated authoring, not hot storage.

## Multimodular Smith engine

The modular/CRT Smith deferral was adjudicated by measured demand: the external
referent harness (`potentialimprovements/linalg-external-referents/`) showed
FLINT `fmpz_mat_snf` beating the classical diagonal route ×11–35 on dense
random n=8–32, a coefficient-explosion gap that widens with n.
`Pure.Domain.Smith.Multimodular` now owns the `Integer` diagonal-only engine:
an unboxed `Word64` word-prime sweep (fixed deterministic 31-bit ladder) gives
per-prime determinant and rank; CRT with symmetric lift recovers the exact
determinant, with the Hadamard minor bound serving only as the
prime-consumption certificate; square full-rank inputs then run Iliopoulos
Smith elimination with all arithmetic mod `R = 2·|det|`, and rectangular or
rank-deficient inputs reach that core through unimodular Hermite-style
compression. The mod-`R` phase runs on flat mutable carriers tiered by modulus
size (`Word64` below 2^32, `Word64` with 128-bit primop intermediates below
2^62, flat boxed `Integer` above). Dispatch is a `NOINLINE`-pinned rewrite rule
on `smithDiagonalForm` at `Integer` — a perf-only substitution, semantically
identical; unoptimized builds fall back to the classical route. Referees: four
fixture agreements with
by-name assertions in `DomainSpec`, the generated
`SmithDiagonalOnlyAgreesWithFull` law, and the external FLINT agreement gate
(exact, including signs). Post-surgery the engine beats the prior classical
diagonal route ×1.8–3.6 across n=8–32 and sits ×2.8–12.3 from FLINT (was
×11–35), with the asymptotic wall removed.

Ruling absorbed on the way: classical Smith diagonals carried algorithm-artifact
unit signs (e.g. `-1, 1, -493`). Both classical routes now canonicalize
invariant factors to nonnegative via `gcdDomain _ zero`, folding the unit flips
into the left witness and its inverse (unimodularity and reconstruction laws
preserved). Smith diagonals are canonical associates everywhere; the FLINT
agreement is exact rather than up-to-units.

## Witnessed Smith engine

Witness-carrying `smithNormalForm` at `Integer` suffered the same
coefficient-explosion wall as the classical diagonal route, but worse: the
alternating Hermite arena reached 111,000-bit work entries at n=32 (69 ms)
where the final diagonal needs 100 bits. Measured mechanism: integer echelon
stays Hadamard-bounded exactly while pivots are ±1, and the first gcd
pair-transform at a non-unit pivot destroys the Schur minor structure — no
pivot policy avoids it. `Pure.Domain.Smith.Witnessed` now dispatches square
inputs of dimension ≥ 25 with certified full rank (word-prime sweep) to a
mod-determinant engine; everything else keeps the alternating arena, which is
faster below the floor and produces pristine witnesses.

The fast path alternates row/column Hermite reduction with all work-matrix
arithmetic bounded mod `R = 2·|det|`, on the Domich–Kannan–Trotter stack
`[A; R·I]`: virtual rows `R·e_j` are carried exactly in the pool, preserving
the invariant `L(rows ∪ R·Zⁿ) = L(A)`. Centering entries mod `R` without the
stack silently shrinks the lattice — the forward transform still verifies while
the inverse fails; the stack is the soundness boundary, not an optimization.
No transform is tracked inside the HNF. Each phase transform is recovered
afterward by per-prime linear solves (upper-triangular back-substitution when
the denominator matrix is triangular — always true after round one — augmented
word Gauss–Jordan otherwise) CRT-combined with symmetric lift, terminated
early when the lift stabilizes across consecutive primes, and then verified
deterministically: the candidate satisfies its congruence mod the accumulated
CRT modulus by construction, so verification consumes fresh primes only until
the combined modulus exceeds the product entry bound, at which point equality
is exact, not probabilistic. The Cramer/Hadamard cap (power-of-two square-root
bound; an upper bound is all a cap needs) is evaluated only if stabilization
never fires, and prime-ladder exhaustion is a typed failure. Phase outputs
equal to their inputs short-circuit to identity transforms. The finale takes
`L·A` from state (maintained compositionally — round one's `L·A` is the row
HNF itself), computes both inverse witnesses by exact diagonal division
(`L⁻¹ = A·R·D⁻¹`, `R⁻¹ = D⁻¹·L·A`) with typed inexact-quotient failures, and
seeds the existing arena for divisibility-chain repair and unit normalization.
`L·A·R = D` needs no final re-verification: it follows by associativity from
the per-phase verified equations.

At n=32 dense random with torsion the engine runs 11.0 ms (was 69 ms), n=24
stays on the alternating arena at 2.2 ms, and witness entries stay ≤ 189 bits.
Referees: the ≥ 25 nonsingular fast-path fixture in `DomainSpec` (strict
diagonal dominance guarantees the dispatch engages), the witnessed
reconstruction and unimodularity assertions, the generated Smith laws, and the
external referent agreement gate.

Recorded 2026-07-06.

## Deferred with cause

MRRR (complexity out of proportion to the selected-pairs need), general-purpose
AMG preconditioning beyond the certified graph-spectral route,
support-graph/KMP preconditioners
(paper-only), M4RI (until measured demand), SIMD kernels (GHC native codegen
rejects `DoubleX2#` on aarch64; LLVM-only), fast approximate normalize
(correctness policy).
