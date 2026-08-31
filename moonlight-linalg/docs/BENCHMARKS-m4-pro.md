# moonlight-linalg Benchmarks — Apple M4 Pro

Measured on Apple M4 Pro via `tasty-bench` on macOS.

## Environment

| Field | Value |
|---|---|
| Machine | MacBook Pro |
| Model identifier | Mac16,7 |
| Chip | Apple M4 Pro |
| CPU cores | 14 total: 10 performance, 4 efficiency |
| Memory | 48 GB unified memory |
| Architecture | aarch64 / arm64 macOS |
| macOS | 26.5.1 (25F80) |
| GHC | 9.14.1 |
| Cabal | 3.16.1.0 |
| Benchmark runner | `tasty-bench` |
| Cabal target | `moonlight-linalg:moonlight-linalg-bench` |
| Cabal parallelism | `-j1` |
| Benchmark timeout | `--timeout=1s` |
| Pre-sat2 CSV | `/tmp/moonlight-linalg-pre-sat2-closure.csv` |
| Baseline `HEAD` CSV | `/tmp/moonlight-linalg-baseline-closure.csv` |
| Current overlay CSV | `/tmp/moonlight-linalg-current-closure.csv` |

The pre-sat2 baseline was measured from a clean worktree at commit `a7ad9c832`. The `HEAD` baseline and current overlay were measured from clean detached worktrees so unrelated dirty files in the main checkout could not pollute the result. The current overlay is `HEAD` plus the linalg cut.

## Focused graph-spectral P2 campaign (2026-07-21)

The P1 restart/allocation cut left one honest algorithmic collapse: the real
10,000-vertex homology graph-Laplacian row still timed out after 30 seconds and
allocated 72,968,137,200 bytes. P2 retains graph-Laplacian provenance through
the checked operator constructor and sends only large, low-cardinality smallest
requests through heavy-edge cascadic descent. Arbitrary self-adjoint CSR remains
on the generic dense/restarted-Lanczos policy; symmetry is not counterfeit proof
of graph structure.

The retained hierarchy follows the graph-specific cascadic method rather than
pretending an RHS-dependent approximate linear solve is an exact shift-invert
operator. Heavy-edge aggregates descend locally, mass-normalized prolongation
glues the coarse sections, and the authoritative fine CSR supplies the returned
residual evidence. This is consonant with the algebraic multilevel Fiedler
method of [Urschel and Hu](https://arxiv.org/abs/1412.0565) and the multigrid-
preconditioned block eigensolver evidence in
[BLOPEX](https://arxiv.org/abs/0705.2626).

| Downstream successor carrier | P1 | P2 | P2 allocation / peak |
|---|---:|---:|---:|
| n=1,024, 3 smallest modes | 134.9 ms ± 28.7 ms | 161 ms ± 32 ms | 332 MB / 14 MB |
| n=10,000, 3 smallest modes | >30 s timeout | 48.0 ms ± 14 ms | 127 MB / 19 MB |
| n=50,000, 3 smallest modes | not retained | 265 ms ± 46 ms | 619 MB / 51 MB |
| n=100,000, 3 smallest modes | not retained | 496 ms ± 60 ms | 1.2 GB / 81 MB |

The 10k row is therefore more than 625x faster than the bounded P1 observation
and allocates about 575x less. The 50k and 100k rows preserve the intended
near-linear scaling on this carrier family. The 1k row deliberately remains on
P1's restarted route: its timing intervals overlap and its allocation fell from
354,183,382 bytes to 332 MB, so there is no material small-carrier regression.

```sh
cabal run moonlight-homology:bench:moonlight-homology-bench -j1 -- \
  --hide-progress --stdev 100 --timeout=10s \
  --pattern 'successor-carrier-1k' +RTS -T -s

MOONLIGHT_HOMOLOGY_SPARSE_SPECTRAL_BENCH_ENABLE_LARGE=1 cabal run \
  moonlight-homology:bench:moonlight-homology-bench -j1 -- \
  --hide-progress --stdev 20 --timeout=30s \
  --pattern 'successor-carrier-10k' +RTS -T -s
```

## Focused generic-spectral P1 campaign (2026-07-21)

The current live checkout at `32246311d7` exposed one concentrated failure:
the generic CSR n=384 values request crossed the old n=256 boundary into thick
restart, taking 2.564 s and allocating 10.090 GiB for one evaluation. The
retained cut widens the measured dense crossover to n=512, dispatches bounded
high-cardinality requests by demand through n=1,024, uses a 24-vector default
restart window, fuses scaled subtraction, and gives both projected owners one
generated-output basis-linear-combination kernel.

```sh
MOONLIGHT_LINALG_BENCH_ENABLE_BROAD_LARGE=1 cabal run \
  moonlight-linalg:bench:moonlight-linalg-bench -j1 -- \
  --hide-progress --stdev 20 --timeout=20s \
  --pattern 'generic-csr-dense-fallback-values-pairs-384.values'

MOONLIGHT_LINALG_BENCH_ENABLE_BROAD_LARGE=1 cabal run \
  moonlight-linalg:bench:moonlight-linalg-bench -j1 -- --once +RTS -T
```

| Evidence | Before | After | Result |
|---|---:|---:|---:|
| generic CSR n=384 values, calibrated | 2.564 s ± 97.5 ms | 134.8 ms ± 29.1 ms | 19.0x faster |
| generic CSR n=384 values, once allocation | 10.090 GiB | 9.813 MiB | about 1,053x lower |
| generic CSR n=384 pairs, once | 2.56 s-scale route | 144.3 ms / 10.147 MiB | bounded dense route |
| generic CSR base-threshold boundary | adjacent n=513 restart probe failed its residual gate after 4.66 s | n=512 dense: 304 ms ± 29 ms | measured boundary, not a fabricated restart timing |
| generic CSR n=513, all values | restart is the wrong selected-mode route | 344.5 ms ± 86.2 ms | demand-aware dense route |
| generic CSR n=1,024, all values | restart is the wrong selected-mode route | 2.575 s ± 141 ms / 61.399 MiB once | measured high-demand ceiling |
| default once sweep | 41.517 ms / 36.840 MiB | 36.758 ms / 34.741 MiB | no default regression |

The n=513 low-demand banded fixture was also probed through restart. It failed
honestly at the selected-tridiagonal inverse-iteration residual gate rather than
being laundered into a timing row. The benchmark cover therefore retains the
two authoritative dense rows, while the downstream homology carrier remains the
real convergent restarted-Lanczos stress case. The 2026-07-06 sections below are
historical provenance for the old threshold, not current policy.

```sh
cd /tmp/pale-meridian-linalg-pre-sat2-closure/compiler
cabal run moonlight-linalg:bench:moonlight-linalg-bench -- \
  --hide-progress --stdev 20 --timeout=1s \
  --csv /tmp/moonlight-linalg-pre-sat2-closure.csv

cd /tmp/pale-meridian-linalg-baseline-closure/compiler
cabal run moonlight-linalg:bench:moonlight-linalg-bench -- \
  --hide-progress --stdev 20 --timeout=1s \
  --csv /tmp/moonlight-linalg-baseline-closure.csv

cd /tmp/pale-meridian-linalg-current-closure/compiler
cabal run moonlight-linalg:bench:moonlight-linalg-bench -- \
  --hide-progress --stdev 20 --timeout=1s \
  --baseline /tmp/moonlight-linalg-baseline-closure.csv \
  --csv /tmp/moonlight-linalg-current-closure.csv
```

## Closure comparison

The current overlay preserves `HEAD` performance: 76 shared sampled rows, 0 significant regressions, and `tasty-bench` reported every row as baseline-equivalent within the sampled window. Using `mean delta > max(2*stdev)` as the conservative CSV gate gives 76 noise-equivalent rows, 0 significant improvements, and 0 significant regressions against `HEAD`.

Against the pre-sat2 cut, 36 rows are directly name-matched. The same conservative CSV gate finds 9 significant improvements and 0 significant regressions. These are the real hot-path wins; the rest are noise-equivalent.

| Shared row | Pre-sat2 | Current | Delta |
|---|---:|---:|---:|
| sparse iterative solvers / n=64 / PCG SSOR | 1.869 ms ± 438.619 us | 32.189 us ± 8.142 us | 58.1x faster / 98.3% lower |
| sparse iterative solvers / n=64 / Jacobi diagonal | 75.490 us ± 15.566 us | 13.026 us ± 3.290 us | 5.8x faster / 82.7% lower |
| sparse iterative solvers / n=64 / PCG diagonal | 243.283 us ± 74.419 us | 51.258 us ± 15.002 us | 4.7x faster / 78.9% lower |
| sparse iterative solvers / n=64 / Richardson diagonal | 216.427 us ± 53.428 us | 51.554 us ± 13.876 us | 4.2x faster / 76.2% lower |
| sparse iterative solvers / n=64 / CG | 148.997 us ± 54.511 us | 46.743 us ± 13.892 us | 3.2x faster / 68.6% lower |
| sparse iterative solvers / n=64 / GMRES | 281.931 us ± 105.343 us | 95.055 us ± 37.649 us | 3.0x faster / 66.3% lower |
| sparse storage and packed kernels / n=512 / CSR matvec | 21.499 us ± 7.920 us | 8.631 us ± 3.307 us | 2.5x faster / 59.9% lower |
| sparse storage and packed kernels / n=512 / dense 32x32 -> CSR | 19.000 us ± 6.912 us | 11.761 us ± 3.299 us | 1.6x faster / 38.1% lower |
| selected tridiagonal eigenvalue solve / path-laplacian-10k | 23.030 us ± 7.104 us | 15.379 us ± 3.302 us | 1.5x faster / 33.2% lower |

The projected-block policy rows are not direct name matches because the old fake policy suite was deleted. The replacement rows expose demand and structure explicitly: values rows are values-only, pair rows return `Eigenpairs`, and dense oracle rows are reference evidence rather than production fallback.

| Case | Old reuse-first policy | Old dense-fallback policy | Current structured values | Current structured pairs | Current dense oracle |
|---|---:|---:|---:|---:|---:|
| block-clustered-24 | 981.278 us | 990.648 us | 28.217 us | 90.107 us | 968.766 us |
| block-separated-24 | 953.539 us | 935.845 us | 29.654 us | 87.952 us | 897.297 us |

For the same default fixtures, the current structured projected-block values rows are 32.2x-34.8x faster than the old reuse-first policy rows, and the current pair rows are 10.6x-11.0x faster than the old dense-fallback policy rows. That is not a compatibility story; the old policy surface is gone, and the measured demand-specific replacement is cheaper.

## Current evidence rows

The native LAPACK rows call `Moonlight.LinAlg.Native` with `EigenRequest` and consume the same `Eigenpairs` owner as the pure spectral surface. Benchmark callers do not import raw `FortranIndexRange` or driver functions. DSTEMR, DSYEVX, and DSBEVX vocabulary stays behind the native boundary.

| Row | Mean | 2*Stdev | Purpose |
|---|---:|---:|---|
| path-laplacian-512 DSTEMR selected tridiagonal values modes=4 | 2.92 ms | 993 us | native tridiagonal values-only request |
| path-laplacian-512 DSTEMR selected tridiagonal pairs modes=4 | 3.14 ms | 955 us | native tridiagonal pair request through `Eigenpairs` |
| generic-tridiagonal-512 DSTEMR selected tridiagonal values modes=4 | 582 us | 210 us | native generic-tridiagonal values-only request |
| generic-tridiagonal-512 DSTEMR selected tridiagonal pairs modes=4 | 1.01 ms | 234 us | native generic-tridiagonal pair request through `Eigenpairs` |
| block-clustered-24 DSYEVX dense values | 72.9 us | 28.2 us | dense selected values oracle through public `EigenRequest` |
| block-clustered-24 DSYEVX dense pairs | 111 us | 33.2 us | dense selected pairs oracle through `Eigenpairs` |
| block-separated-24 DSYEVX dense values | 73.9 us | 29.5 us | dense selected values oracle through public `EigenRequest` |
| block-separated-24 DSYEVX dense pairs | 109 us | 26.9 us | dense selected pairs oracle through `Eigenpairs` |
| block-clustered-24 DSBEVX projected block values | 29.1 us | 8.95 us | native symmetric-band selected values through public `EigenRequest` |
| block-clustered-24 DSBEVX projected block pairs | 88.3 us | 26.6 us | native symmetric-band selected pairs through `Eigenpairs` |
| block-separated-24 DSBEVX projected block values | 31.3 us | 11.1 us | native symmetric-band selected values through public `EigenRequest` |
| block-separated-24 DSBEVX projected block pairs | 90.5 us | 28.5 us | native symmetric-band selected pairs through `Eigenpairs` |
| projected tridiagonal-path-512 values | 221 us | 61.2 us | projected tridiagonal values-only path |
| projected tridiagonal-path-512 pairs | 547 us | 145 us | projected tridiagonal pair path with ambient lift and residuals |
| projected block-clustered-24 values | 28.2 us | 7.26 us | projected block values-only path |
| projected block-clustered-24 pairs | 90.1 us | 32.9 us | projected block pair path through the native symmetric-band executor |
| projected block-separated-24 values | 29.7 us | 7.42 us | projected block values-only path |
| projected block-separated-24 pairs | 88.0 us | 26.4 us | projected block pair path through the native symmetric-band executor |

## Current default group totals

| Group | Rows | Sum of row means |
|---|---:|---:|
| dense row validation surface | 4 | 809 us |
| dense decomposition and solvers | 7 | 793 us |
| sparse storage and packed kernels | 8 | 1.44 ms |
| sparse iterative solvers | 6 | 290 us |
| spectral demand dispatch | 20 | 345.19 ms |
| domain algebra, exterior powers, GF2 | 4 | 290 us |
| geometry and statics | 4 | 479 us |
| selected tridiagonal eigenvalue solve | 1 | 15.4 us |
| native LAPACK symmetric eigensolve | 14 | 8.74 ms |
| projected structured eigensolve | 8 | 2.87 ms |
| **Total** | **76** | **360.91 ms** |

## Allocation and retained-live evidence

The once runner reports per-row allocation when RTS stats are enabled. Each row forces a major GC before and after the measured action, records the allocation delta, reports live bytes after the post-row major GC, and reports the RTS process maximum residency observed after the row. Row retained-live is row-local; RTS maximum residency is process cumulative and is therefore labeled as such instead of being smuggled in as per-row truth.

```sh
cd /Users/bluerose/Developer/pale-meridian/compiler
cabal run moonlight-linalg:bench:moonlight-linalg-bench -- --once +RTS -T
```

Current result:

| Rows | CPU once time | Checksum | Heap allocated | Max retained live after row GC | Process maximum residency |
|---:|---:|---:|---:|---:|---:|
| 66 | 360.970 ms | 3436947.625310 | 746.235 MiB | 1.502 MiB | 2.186 MiB |

Highest allocation rows:

| Row | Allocated | Live after major GC | Process maximum residency |
|---|---:|---:|---:|
| spectral demand dispatch / generic-tridiagonal-values-pairs-512 / pairs | 498.052 MiB | 173.648 KiB | 2.186 MiB |
| spectral demand dispatch / reducible-tridiagonal-values-pairs-512 / pairs | 49.684 MiB | 178.445 KiB | 2.186 MiB |
| spectral demand dispatch / reducible-tridiagonal-values-pairs-512 / values | 37.229 MiB | 176.125 KiB | 2.186 MiB |
| spectral demand dispatch / generic-csr-fallback-values-pairs-96 / pairs | 19.916 MiB | 183.242 KiB | 2.186 MiB |
| spectral demand dispatch / generic-csr-fallback-values-pairs-96 / values | 18.298 MiB | 180.922 KiB | 2.186 MiB |
| spectral demand dispatch / generic-tridiagonal-values-pairs-512 / values | 11.277 MiB | 171.328 KiB | 171.328 KiB |
| sparse storage and packed kernels / n=512 / CSR -> CSC | 7.254 MiB | 161.859 KiB | 161.859 KiB |
| sparse storage and packed kernels / n=512 / graph Laplacian construction | 6.057 MiB | 168.820 KiB | 168.820 KiB |
| projected structured eigensolve / block-clustered-24 generic dense oracle | 5.733 MiB | 248.938 KiB | 2.186 MiB |
| projected structured eigensolve / block-separated-24 generic dense oracle | 5.648 MiB | 255.992 KiB | 2.186 MiB |

The detached pre-sat2 runner did not have per-row RTS stats. Its old whole-suite `+RTS -s` aggregate remains useful only as a process-level reference, not a row-normalized comparison: 44 rows, 26.847 ms CPU once time, 133,145,352 bytes allocated, 309,632 bytes maximum residency. The row-matched speed gate remains the CSV comparison above.

## Reading the cut

The benchmark cover now measures the requested spectral demand split instead of smuggling solver-selection toggles through the suite. The slow generic-tridiagonal pair row is intentionally visible; values are cheap, full pairs are not.

Sparse CG, PCG, GMRES, Jacobi, and Richardson now run through a sealed `Double` `ST` workspace. The public preconditioner is an abstract ADT, not a closure-shaped backdoor. The old persistent sparse-solver row walkers were deleted; the mutable arena is sealed behind immutable `U.Vector Double` results.

The native tridiagonal evidence has separate values-only and pair rows. Projected evidence has separate tridiagonal values/pairs, block values/pairs, DSYEVX dense oracle rows, dense projected-space oracle rows, and DSBEVX symmetric-band rows. Comparable current rows carry the no-regression claim, and values-only rows prove demand is no longer silently flattened.

## Wave-2 finalization rows (2026-07-06)

Two package-wide causes were found and repaired during Wave-2 reconciliation, and every row below reflects both:

- The IEEE boundary constants (`epsDouble`, `safeMinimumDouble`, `maxFiniteDouble`) were decimal scientific literals whose exact rationals carry hundreds of digits. GHC 9.14 does not constant-fold `fromRational` at those exponents, and the `INLINE` pragmas pasted the runtime conversion — GMP integer division and gcd — into every consumer's hot loop, including the implicit-QL negligibility test and the Sturm bisection tolerances. They are now `encodeFloat` forms under `NOINLINE`, evaluated once.
- The libraries built at cabal's default `-O1` while every probe and target assumed `-O2`. SpecConstr and LiberateCase are worth ×3–4 on the Krylov and QL inner loops. `-O2` now lives in `shared-properties`.

| Row | Mean | Prior | Purpose |
|---|---:|---:|---|
| symmetric eigen pure 12x12 | 17.6 us | 208 us | dense unchecked eigensolve, certification split off the hot path |
| selected tridiagonal path-laplacian-10k | 12.8 us | 305 ms / 498 MiB | bisection + shifted inverse iteration replacing the QL global solve |
| reducible-tridiagonal-values-pairs-512 pairs | 356 us | 7.43 ms | reducible split + selected solve per block |
| generic-csr-fallback-values-pairs-96 values | 22.8 ms | timeout-scale | bordered Wu–Simon thick restart, capacity 32, 4 modes |
| generic-csr-fallback-values-pairs-192 values | 1.50 s | 6.08 s | same path, 6 modes; convergence-bound, see below |
| generic-csr-thick-restart-values-pairs-384 values | 2.09 s | never completed | gated broad-large row |
| svd 12x12 | 32.4 us | — | one-sided Jacobi, Gram path deleted |

The generic CSR fallback rows are convergence-bound, not overhead-bound: the banded SPD fixture's smallest eigenvalues cluster at gaps near 1e-5, and the restart loop needs ~50 cycles at capacity 32 to lock four of them, with the cycle interior running at flop cost. Locking is honest — candidates pass a scalar projected-residual gate, are lifted, and must then survive the ambient-residual predicate; failures demote back to the unlocked pool. Dense eigensolve of the densified operator beats restart at every benched dimension (~2.5 ms at n=96), so the fallback dispatch gains a dimension threshold in Wave 3; the restart path remains the only route where densification is prohibitive.

## Wave-3 dispatch rows (2026-07-06)

The generic CSR fallback now dispatches on dimension: at or below `denseSpectralFallbackDimensionThreshold = 256` the operator is densified once (one apply per basis vector) and solved by the flat unchecked dense eigensolver, with pair residuals computed against the densified matrix rather than per-pair operator re-applies; above the threshold the thick-restart path stands.

| Row | Mean | Prior (restart-only) | Route |
|---|---:|---:|---|
| generic-csr-fallback-values-pairs-96 values | 2.40 ms | 22.8 ms | dense |
| generic-csr-fallback-values-pairs-192 values | 17.6 ms | 1.50 s | dense |
| generic-csr-thick-restart-values-pairs-384 values | 2.09 s | 2.09 s | restart (above threshold, by design) |
