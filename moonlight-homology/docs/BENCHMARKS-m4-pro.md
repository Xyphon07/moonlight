# moonlight-homology Benchmarks — Apple M4 Pro

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
| Cabal target | `moonlight-homology:moonlight-homology-bench` |
| Cabal parallelism | `-j1` |
| Benchmark timeout | `--timeout=1s` |
| CSV source | `/tmp/moonlight-homology-bench-m4-pro-all.csv` |

Notes:

- The default sweep intentionally uses a short `--timeout=1s` runner so it stays fast.
- `successor-carrier-10k` and `successor-carrier-50k` are opt-in via `MOONLIGHT_HOMOLOGY_SPARSE_SPECTRAL_BENCH_ENABLE_LARGE=1`.
- `successor-carrier-100k` is opt-in via `MOONLIGHT_HOMOLOGY_SPARSE_SPECTRAL_BENCH_ENABLE_100K=1`.
- Morse/spectral rows report whole-operation time for the named prepared path complex.

## Focused linalg P2 graph-spectral evidence (2026-07-21)

The 10,000-vertex successor carrier no longer collapses into repeated restarted
Lanczos windows. Homology now constructs its sparse spectral operator through
linalg's checked graph-Laplacian owner, preserving enough provenance for the
large smallest-mode request to use heavy-edge cascadic descent. Returned modes
still come from the existing `Eigenpairs` owner with residuals recomputed on the
fine matrix; no homology-specific solver or parallel public API was added.

| Benchmark | P1 | P2 | P2 allocation | P2 peak |
|---|---:|---:|---:|---:|
| successor-carrier-1k | 134.9 ms ± 28.7 ms | 161 ms ± 32 ms | 332 MB | 14 MB |
| successor-carrier-10k | >30 s timeout | 48.0 ms ± 14 ms | 127 MB | 19 MB |
| successor-carrier-50k | not retained | 265 ms ± 46 ms | 619 MB | 51 MB |
| successor-carrier-100k | not retained | 496 ms ± 60 ms | 1.2 GB | 81 MB |

Against P1's bounded 10k observation, P2 is more than 625x faster and reduces
allocation from 72,968,137,200 bytes by about 575x. The 50k and 100k observations
then scale approximately with carrier size instead of resurrecting the old
convergence cliff. The 1k request remains on P1's route; its timing intervals
overlap while allocation is modestly lower.

```sh
MOONLIGHT_HOMOLOGY_SPARSE_SPECTRAL_BENCH_ENABLE_LARGE=1 cabal run \
  moonlight-homology:bench:moonlight-homology-bench -j1 -- \
  --hide-progress --stdev 20 --timeout=30s \
  --pattern 'successor-carrier-10k' +RTS -T -s

MOONLIGHT_HOMOLOGY_SPARSE_SPECTRAL_BENCH_ENABLE_100K=1 cabal run \
  moonlight-homology:bench:moonlight-homology-bench -j1 -- \
  --hide-progress --stdev 20 --timeout=30s \
  --pattern 'successor-carrier-100k' +RTS -T -s
```

## Focused linalg P1 downstream evidence (2026-07-21)

The successor carrier is the real generic sparse consumer used to validate the
Moonlight linalg restart repair. With identical benchmark input, the 1,024-row
carrier moved from 298.1 ms ± 39.4 ms and about 1.2 GB allocated per evaluation
to 134.9 ms ± 28.7 ms and 354,183,382 bytes allocated. The result is 2.2x faster
with 70.5% less allocation.

```sh
cabal run moonlight-homology:bench:moonlight-homology-bench -j1 -- \
  --hide-progress --stdev 100 --timeout=10s \
  --pattern 'successor-carrier-1k' +RTS -T -s
```

The gated 10,000-row carrier still exceeds 30 seconds. Its bounded run allocated
72,968,137,200 bytes after the repair versus 147,409,380,400 bytes before it,
but did not converge within the gate. That is explicit evidence that allocation
was repaired while smallest-mode convergence on the tightly clustered graph
Laplacian remains the large-scale obstruction; this document does not pretend a
timeout is a successful 10k solve.

| Group | Benchmark | Mean | 2*Stdev |
|---|---:|---:|---:|
| morse-spectral / path-16 | raw-unreduced-rational-spectral | 147 us | 28.4 us |
| morse-spectral / path-16 | refined-morse-plus-spectral | 624 us | 149 us |
| morse-spectral / path-16 | reduced-rational-spectral-only | 397 ns | 56.2 ns |
| morse-spectral / path-32 | raw-unreduced-rational-spectral | 804 us | 130 us |
| morse-spectral / path-32 | refined-morse-plus-spectral | 2.65 ms | 463 us |
| morse-spectral / path-32 | reduced-rational-spectral-only | 403 ns | 127 ns |
| morse-spectral / path-64 | raw-unreduced-rational-spectral | 8.06 ms | 2.04 ms |
| morse-spectral / path-64 | refined-morse-plus-spectral | 12.7 ms | 2.18 ms |
| morse-spectral / path-64 | reduced-rational-spectral-only | 409 ns | 128 ns |
| successor-like-sparse-spectral | successor-carrier-1k | 134.9 ms | 28.7 ms |
