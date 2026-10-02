# Moonlight–Spade operation comparison

This is Moonlight Triangulation's complete current operation board against
Spade 2.15.1. It is broader than the construction-only
[`delaunay-compare`](../../docs/delaunay-compare/README.md): it exercises construction,
search, constraint recovery, refinement, persistent removal, natural-neighbour
interpolation, Voronoi and DCEL traversal, segment intersection, and hierarchy
behavior.

It is not an exhaustive conformance suite for every Spade API. Barycentric
interpolation, shape/flood-fill iterators, coordinate-validation failures, and
parts of the public handle surface are not represented.

## Run it

From this directory:

```console
./scorecard.sh list
./scorecard.sh render
./scorecard.sh check
./scorecard.sh refresh '<why this tree becomes the new baseline>'
```

`list` builds only the Haskell lane registry and performs no measurement.
`render` derives the report from a complete retained board without building
Rust or running a benchmark, and refuses when that board is absent. `check`
builds both private runners, replays every semantic record, times seven
interleaved fresh-process rounds, and refuses missing lanes or regressions; it
does not replace retained evidence. `refresh` is the only mode that adopts new
records, measurements, or baseline gaps, and therefore requires a provenance
note.

Before building Rust or measuring anything, `check` also refuses a baseline
whose lane keyspace or provenance is incomplete, avoiding timing runs against
an invalid baseline.

## Covered operations

`./scorecard.sh list` prints every registered workload and size directly from
the typed executable registry. A successfully refreshed `SCORECARD.md` adds the
retained timing projection. This README deliberately does not keep a
hand-written second lane table.

The diagnostic lanes measure known complexity cliffs and remain in the
all-lane standings after a repair, preserving comparisons across revisions.

## Semantic descent

Each runner first writes its complete canonical observation into `gates-hs` or
`gates-rs`. Unique results must agree byte-for-byte across implementations.
Two lawful differences are excluded only from that cross-language comparison:

- Refinement may produce different valid meshes for the same budget.
- Natural-neighbour sets agree exactly, while normalized weights can differ in
  their last bits because each implementation accumulates areas in its own
  cyclic neighbour order.

The complete gate directory is still pinned per side, so neither exclusion is
an escape hatch. `divergence-hs.txt` and `divergence-rs.txt` separately retain
the intentional dangling-constraint domain distinction: Spade's documented
winding-number domain requires closed boundaries, while Moonlight treats a
free segment as no region boundary. `stats-hs` pins Moonlight-only sweep
instrumentation.

Only after these local sections satisfy their compatibility obligations does
the driver glue a timing board.

## Timing and retained evidence

Each sample is one timed iteration in a fresh process. Seven rounds interleave
the Spade and Moonlight sides so short-lived scheduler and thermal drift lands
on both sides of a ratio. Fresh processes prevent a forced Haskell result from
being shared across nominal repetitions. The report shows medians and observed
ranges; its regression gate compares the least-contended per-side minima.

Setup is constructed outside the timed action for both sides. Haskell-only
snapshot-publication rows are reported separately against their session
references and never masquerade as Spade comparisons.

Performance evidence is one indivisible retained claim. A successful
`./scorecard.sh refresh <provenance-note>` creates all three paths below from
the same complete run; when they are absent, there is no retained performance
claim rather than a collection of half-current numbers:

- `scorecard.csv` — timing observations and measured-source provenance.
- `baseline.json` — complete median gaps, minimum gaps, snapshot premiums, and
  the same provenance.
- `SCORECARD.md` — generated projection of that run and pin; never edit it
  directly.

Semantic evidence remains independently retained because it is replayable
without minting a timing claim:

- `gates-hs` / `gates-rs` — complete per-implementation semantic corpora.
- `divergence-hs.txt` / `divergence-rs.txt` — intentional domain distinction.
- `stats-hs` — Moonlight-only diagnostic counters.

`Moonlight.Planar.Bench.SpadeCompare.Lane` is the sole owner of lane
class, operation, size, order, display label, and snapshot/session relation.
Shell consumes its execution projection; `scorecard.py` consumes its report
projection and rejects a run whose declared inventory or observations fail to
match it.

## Focused alpha-persistence diagnostic

`alpha-persistence` is a focused diagnostic constructor rather than a retained
board row. It measures the complete operation requested from both ecosystems:
point generation is setup, while Delaunay construction, alpha births, checked
boundary lowering, one mod-two persistence reduction, and Betti queries at
every critical birth are timed. The Rust side is Spade 2.15.1 plus
`alphalpha` 0.1.0 and LoPHAT 0.9.x; the Moonlight side uses its exact rational
birth order and canonical Homology reducer.

The repaired Moonlight path batches the critical-value query, computes exact
geometric births through one-normalization dyadic kernels, lowers ordered
incidence through Homology's chain-law seal, and reduces over dense integer ranks with graph
union-find and degree clearing. Seven fresh-process 1,000-site rounds on the
Apple M4 Pro measured a **34.177 ms Moonlight median** against **11.397 ms for
Spade**, a 3.00x gap. The pre-repair Moonlight median was 941.312 ms, so the
actual defect removal is 27.54x; allocation fell from 3,110,425,952 to
95,989,848 bytes. Both sides emit the exact shared receipt
`3927,1000,2981,1982,2981,1,3747800946422566000`.

The Rust lowering intentionally does not pass `alphalpha::sparsify` directly to
LoPHAT. That helper groups every vertex, then every edge, then every face before
sorting within a dimension. Persistent reduction instead requires one global
`(birth, dimension, stable cell)` order, because an unrelated early face may
precede a later edge. The diagnostic reindexes the helper's sparse columns into
that lawful order and emits a shared receipt containing cell counts, pair
counts, critical-birth count, and a checksum of every Betti profile.

The Haskell diagnostic is a separate executable so the ordinary scorecard
registry does not inherit the Homology closure. The Rust alpha stack is likewise
behind the `alpha-persistence` Cargo feature, so an ordinary scorecard build
still compiles only Spade. After building both diagnostic interpreters, one
semantic or timing observation is:

```console
ALPHA_HS_BIN="$(cabal list-bin moonlight-planar:exe:moonlight-planar-alpha-spade-referent)"
cargo build --release --features alpha-persistence --bin spade-referent-rs --manifest-path rust/Cargo.toml
$ALPHA_HS_BIN alpha-receipt 1000
rust/target/release/spade-referent-rs alpha-receipt 1000
$ALPHA_HS_BIN bench-one alpha-persistence 1000 0
rust/target/release/spade-referent-rs bench-one alpha-persistence 1000 0
```

The lane remains outside `boardLaneSpecs` until a deliberate full-board refresh
adopts it. A focused experiment does not silently invalidate the retained
25-lane comparison corpus.
