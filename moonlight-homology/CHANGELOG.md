# Changelog

All notable changes to `moonlight-homology` are documented here.

## 0.1.0.3 - 2026-08-29

- Add checked finite chain maps, arbitrary forward/backward finite chain
  zigzags, and exact rational interval decomposition through one streaming
  right-filtration descent. Each induced arrow is consumed once rather than
  retained globally. Arrow payloads and interval endpoints are functorial;
  invalid shapes, endpoint gluing, chain-map laws, and interval multiplicities
  remain typed failures.

## 0.1.0.2 - 2026-08-22

- Generalize `FilteredFiniteChainComplex` from the fixed binary64
  `FiltrationValue` to any ordered birth-key type, preserving exact geometric
  filtration order through the canonical persistence reducer. Its constructor
  is now sealed; callers retain read-only projections and construct only
  through the coverage- and monotonicity-checking boundary.
- Add `persistentBettiAt`, the batched `persistentBettiAtMany`, and
  `persistentBettiAtCriticalValues`. Filtered complexes cache exact critical
  equivalence classes and dense complex-relative ranks, so all critical Betti
  profiles are one ordered event sweep rather than one barcode scan per birth.
- Lower mod-two persistence through dense integer indices, `IntMap`/`IntSet`
  columns, degree-wise clearing, and a sealed union-find specialization for
  graph boundaries, with the general sparse reducer retained for incompatible
  boundary fibers.
- Keep the checked finite-chain constructor as the sole public entrance while
  deciding sparse boundary-composition zero directly, without materializing a
  throwaway product matrix on the valid path.

## 0.1.0.1 - 2026-08-21

- Rebuilt the multi-library Haddock archive with external Algebra, Core, and
  Pale interfaces glued to their Hackage locations instead of broken local
  targets.
- Added an exact public release tag through `source-repository this`.
- Documented Hackage's package-wide dependency summary without widening the
  dependencies inherited by ordinary facade or component consumers.

## 0.1.0.0 - 2026-08-21

- Initial public release.
- Finite chain complexes with validated boundary-incidence matrices.
- Phase-gated Betti and spectral-sequence capabilities.
- Field-rank, GF2-rank, and Smith-normal-form homology backends.
- Effective-homology reductions with sampled law witnesses.
- Exact and spectral sequence scaffolding, bidegrees, formal maps, pages, and
  rational spectral-family construction.
- Topological carriers: 2D cell complexes, graph skeletons, macro-scaffolds,
  persistence helpers, harmonic summaries, and graph spectral helpers.
- Discrete Morse and Block-Schur reductions.
- Public `moonlight-homology-laws` sublibrary for boundary, reduction, normalization,
  and determinism harnesses.
- Split into private implementation sublibraries (`moonlight-homology-chain`,
  `moonlight-homology-matrix`, `moonlight-homology-topology`,
  `moonlight-homology-sequence`), a curated public facade, and the public laws
  sublibrary; external proof-assistant certification was removed from the package.
- Consolidated the old Criterion benchmark entrypoints into a single
  `moonlight-homology-bench` `tasty-bench` target matching `moonlight-category`.
- Added Apple M4 Pro benchmark documentation at `docs/BENCHMARKS-m4-pro.md`, with
  large sparse spectral cases gated behind explicit environment variables.
- Pre-Hackage hardening pass (failure-mode audit):
  - Field-rank backends treat degrees above `maxHomologicalDegree` as zero by
    convention instead of consulting the raw incidence function at `max + 1`,
    which silently corrupted the top homology group for malformed inputs.
  - `validatedColumnAt` returns legal columns of zero-row matrices (the
    transpose-based path lost them).
  - Non-nilpotent boundaries surface as `ChainComplexNilpotenceViolation d` on
    every detection path (checked construction, field-rank gate, Morse gate);
    previously the same failure appeared as three different constructors.
  - `restrictComplex` materializes each retained degree exactly once and
    propagates materialization failures instead of collapsing them into
    silently empty boundaries.
  - Sparse echelon rows are compacted at the reduction boundary, eliminating
    phantom zero pivots from uncompacted input; redundant double reversal
    removed from RREF canonicalization.
  - Morse: DAG machinery extracted to
    `Moonlight.Homology.Pure.Topology.Morse.Digraph`; the refined descent
    gates its first stage and trusts theorem-guaranteed later stages instead
    of re-validating per stage; the gradient path-weight oracle is shared per
    upper cell; nilpotence violations are no longer misreported as
    `ReductionInclusionChainMapLaw`.
  - Performance: persistence boundary columns and BlockSchur law sweeps index
    entries by source; filtration-ordered spectral reduction uses vectors
    instead of per-lookup list walks; `nub` quadratics removed from Reeb arc
    seeds, spectral support levels, and torsion-order normalization; strict
    accumulators for persistence state, duplicate-cell detection, and ordered
    entry canonicalization; `materializeBoundary` evaluates the user boundary
    function once per basis element.
  - Documentation: realization budget floor semantics, spectral input
    scrubbing contract, `subtractFromDiagonal` orientation, and the
    forward-looking bi-parameter persistence vocabulary are stated explicitly;
    duplicate export block removed from the `Chain` facade.
  - Packaging: compile-fixture files ship in the sdist via
    `extra-source-files`; upper bounds pinned for test and benchmark
    dependencies.
