# moonlight-algebra finite-lattice benchmarks on Apple M4 Pro

This file records only post-change measurements from the current package
topology; stale timings are never pasted forward.

## Environment

| Field | Value |
|---|---|
| Machine | MacBook Pro |
| Model identifier | Mac16,7 |
| Chip | Apple M4 Pro |
| CPU cores | 14 total: 10 performance, 4 efficiency |
| Memory | 48 GB unified memory |
| Architecture | aarch64 / arm64 macOS |
| macOS | 26.5.2 (25F84) |
| GHC | 9.14.1 |
| Cabal | 3.16.1.0 |
| Benchmark runner | `tasty-bench` |
| Cabal target | `moonlight-algebra:bench:moonlight-algebra-finite-lattice-bench` |
| Cabal parallelism | `-j1` |
| Benchmark timeout | `--timeout=1s` |
| CSV source | `/tmp/moonlight-algebra-finite-lattice-bench-m4-pro-all.csv` |

## Required commands

```sh
cd /Users/bluerose/Developer/pale-meridian/compiler
cabal bench moonlight-algebra:bench:moonlight-algebra-finite-lattice-bench -j1 --benchmark-options='--timeout=1s --csv=/tmp/moonlight-algebra-finite-lattice-bench-m4-pro-all.csv'
```

## 2026-07-20 finite-lattice result

All 373 benchmark cases pass. Times are `tasty-bench` means converted from
picoseconds; the full CSV is at the path above. `n` is the declared element
count of the compiled lattice.

### Compilation cost by topology (`compile/order+operations`)

| Topology | n=16 | n=64 | n=128 |
|---|---:|---:|---:|
| chain-dense | 3.96 µs | 18.2 µs | 39.8 µs |
| fan-sparse | 10.1 µs | 52.6 µs | 117 µs |
| boolean-cube | 13.2 µs | 95.3 µs | 256 µs |
| dense-grid | 66.4 µs | 1.04 ms | 5.91 ms |

Dense-grid grows fastest of the four; at n=128 it is in milliseconds where the
sparser topologies remain in microseconds.

### Compiled-query operations (chain-dense fixture, n=128)

Rows prefixed `key` resolve through resident branded keys (array indexing); the
unprefixed rows resolve each domain value through the value→key map on every
call.

| Operation | Time |
|---|---:|
| key `<=` sweep | 37.7 µs |
| key join/meet sweep | 41.0 µs |
| join/meet sweep | 3.05 ms |
| least/greatest fixpoint | 19.7 µs |
| key Heyting implication sweep | 69.9 µs |
| Heyting implication sweep | 1.44 ms |

### Referent: `finite-lattice` vs Hackage `lattices` (boolean-cube, n=256)

| Operation | moonlight | `lattices` (IntSet) |
|---|---:|---:|
| `<=` sweep (resident key) | 214 µs | 369 µs |
| join sweep (public) | 8.23 ms | 332 µs |
| meet sweep (public) | 8.09 ms | 334 µs |
| join/meet sweep (resident key) | 1.18 ms | 616 µs |

### Referent: compiled `ContextLattice` join/meet vs precomputed `Data.Map` (n=256)

| Topology | ContextLattice | precomputed `Data.Map` |
|---|---:|---:|
| chain-dense | 20.3 ms | 5.58 ms |
| fan-sparse | 20.0 ms | 5.45 ms |
| boolean-cube | 20.7 ms | 5.49 ms |
| dense-grid | 20.3 ms | 5.44 ms |

### Referent: packed context-key rows vs `containers` IntSet rows (membership sweep, n=256)

| Topology | packed rows | IntSet rows |
|---|---:|---:|
| chain-dense | 224 µs | 256 µs |
| fan-sparse | 191 µs | 227 µs |
| boolean-cube | 210 µs | 265 µs |
| dense-grid | 245 µs | 274 µs |
