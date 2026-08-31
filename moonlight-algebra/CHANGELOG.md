# Changelog

## 0.1.0.2 - 2026-08-21

- Add the public quantale tower: `Quantale`, commutative, residuated, integral,
  and chain refinements; Viterbi, Łukasiewicz, and tropical carriers; and
  law coverage for the complete algebra.
- Align focused and aggregate coherence tests with their actual dependency and
  optimization boundaries.

## 0.1.0.1 - 2026-07-22

- Documentation: the public `finite-lattice` sublibrary front door
  `Moonlight.FiniteLattice` now carries a full module-header overview and worked
  recipes — quick-start lattice construction, Heyting implication, least and
  greatest fixpoints, and the resident fast path.
- README reduced to a dependency/module map and build tooling; the tower's
  shape, contract, and cookbook now live once, in the module headers.
- Removed a stale `extra-doc-files` reference left by an earlier surface
  consolidation.

## 0.1.0.0

- Initial release of the Level-1 algebraic tower over `moonlight-core`.
- Group surface: standard `Semigroup`/`Monoid`, `Group`, `AbelianGroup`, and
  operation-selecting `Additive`/`Multiplicative` wrappers, plus free structures
  (`FreeMonoid`, `FreeAbelianGroup`).
- Lattice tower: join/meet semilattices through distributive, Heyting, and
  Boolean algebras (`Lattice`, `Orientation`). Compiled finite context lattices
  now live in the public `moonlight-algebra:finite-lattice` sublibrary.
- Ring tower: operation-bearing `Semiring`/`Ring`/`CommutativeRing` classes live
  in `moonlight-core`; `moonlight-algebra` adds `IntegralDomain`, `GCDDomain`,
  `EuclideanDomain`, canonical residues, modular arithmetic (`Zn`), number
  theory, and `GCD`.
- Modules, vector spaces, bilinear spaces (`Module`, `Magnitude`), polynomials,
  power sets, products, quotients, and sparse vectors with finite support.
- All `Moonlight.Algebra.Pure.*` modules are now exposed for selective qualified
  import; `Moonlight.Algebra` re-exports the tower, grouped by family, as a
  convenience.
- Algebraic laws documented in module headers and exercised by the private
  `moonlight-algebra-laws` sublibrary.
