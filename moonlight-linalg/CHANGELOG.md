# Changelog

All notable changes to `moonlight-linalg` are documented here.

## 0.1.0.2 - 2026-10-01

- Simplify README and architecture-guide wording.
- No library or API changes from 0.1.0.1.

## 0.1.0.1 - 2026-08-21

- Rebuilt the multi-library Haddock archive with dependency interfaces glued to
  their Hackage locations, eliminating the broken `Moonlight.Algebra` link.
- Added an exact public release tag through `source-repository this`.
- Documented Hackage's package-wide dependency summary without widening the
  dependencies inherited by ordinary library consumers.

## 0.1.0.0 - 2026-08-21

- Initial public release.
- Typed dense row validation and matrix/vector combinators, with dense rows
  documented as authoring/projection rather than hot dense storage.
- Dynamic dense matrices, affine transforms, vector primitives, exterior helpers,
  and finite-field `GF2` support.
- Sparse COO/CSR/CSC/Packed carriers and sparse solver helpers.
- Row-reduction, PLU, RREF, Smith-normal-form, symmetric eigen, and domain-level
  matrix operations.
- Arnoldi, Lanczos, restarted Lanczos, block Lanczos, projected subspaces,
  tridiagonal and block-tridiagonal projected operators, and spectral request/result
  surfaces above Krylov.
- Selected symmetric-tridiagonal/path-Laplacian spectral fast path documented as
  the package's flagship hot kernel.
- Finalization campaign: perf storm follow-through, laws sublibrary split, thick
  restart hardening, selected certification split, IC(0) sparse preconditioning,
  GF2 sparse reducer, closed-form geometry eigen path, `-O2` shared properties,
  and IEEE constant `encodeFloat` repair.
- Graded sublibrary decomposition: the private core dissolved into ten public
  slices (`carrier` through `native`) with per-slice source directories, the
  dependency DAG cabal-enforced, native LAPACK/Accelerate linkage confined to
  `moonlight-linalg-native`, and `ArchitectureSpec` slice-discipline guards.
  Architecture decisions recorded in `docs/ARCHITECTURE.md`.
- Multimodular Smith engine: `smithDiagonalForm` at `Integer` dispatches (via a
  `NOINLINE`-pinned rewrite rule) to a CRT/Iliopoulos engine — word-prime
  determinant/rank sweep on unboxed carriers, exact determinant by CRT under the
  Hadamard certificate, Smith elimination mod `2·|det|` on tiered flat carriers,
  unimodular Hermite compression for rectangular/rank-deficient inputs. Beats
  the classical diagonal route ×1.8–3.6 on dense random n=8–32 and removes the
  coefficient-explosion scaling wall; verified against the classical route and
  FLINT with exact agreement gates. Classical Smith diagonals (both routes) now
  canonicalize invariant factors to nonnegative canonical associates, with unit
  flips absorbed into the left witnesses.
- Witnessed Smith engine: `smithNormalForm` at `Integer` dispatches square
  certified-nonsingular inputs of dimension ≥ 25 to a mod-determinant fast path —
  Domich–Kannan–Trotter `[A; R·I]` stack Hermite alternation mod `R = 2·|det|`,
  per-phase transform recovery by early-terminated CRT over word primes with
  deterministic exact verification, triangular back-substitution solves, and
  inverse witnesses by exact diagonal division. Kills the 111k-bit intermediate
  explosion (witnesses stay ~100–190 bits at n=32) and runs n=32 dense random
  with torsion in 11 ms (was 69 ms); smaller and rectangular/rank-deficient
  inputs keep the alternating arena unchanged.
- Hackage-facing metadata, documentation, and explicit package bounds.
