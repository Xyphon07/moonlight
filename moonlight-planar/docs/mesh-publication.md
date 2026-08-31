# Mesh publication schedules

[Read the set-operation types.](../README.md#set-operation-types)
· [Choose an operation.](../README.md#operations)

Programs choose an operation by its semantic result; Moonlight chooses its
execution schedule. The schedule changes identity reuse, allocation, and
throughput, not the represented site set.

## Persistent publication schedules

These are the currently measured publication choices. `canonicalize` remains
the explicit global observation when construction-independent numbering is
required.

| Operation context | Publication schedule |
| --- | --- |
| `siteRelation left right` | Index the smaller support in one transient exact open-addressed section, scan the other operand, then discard the index; no support maps or published cache state. |
| `difference mesh empty`, `symmetricDifference mesh empty`, `symmetricDifference empty mesh` | Return the surviving representative verbatim. |
| `difference left right` with `size right <= (size left - size right) / 128` | Remove the right support through one local copy-on-write session; return `left` verbatim when the supports are disjoint. Larger masks rebuild. |
| Geometry-only `intersection left right` | Return an existing operand for equality or subset, return the empty mesh for disjoint supports, and locally remove the smaller complement when it is at most `overlap / 128`. Other partial overlaps rebuild. |
| `intersectionWith` | Rebuild, because the annotation combiner may rewrite every surviving payload even when topology changes locally. |
| `symmetricDifference left right` with `small <= (large - small) / 128` | Toggle the smaller operand through one local session. Comparable operands and small-output, large-input cases rebuild. |
| Comparable `symmetricDifference left right` | Partition through one transient exact index and a matched bitset, then rebuild only the exclusive output section. |
| Package-internal `BulkLoad.insert` / `BulkLoad.insertAt` | Dense publication below 10,000 resident sites; copy-on-write publication at 10,000 and above. Internal edit sequences descend through one `Session`. |
| `extendConstrainedWith` | Copy-on-write only for a base of at least 200,000 sites, at most 128 incoming sites, exactly one incoming segment, and a pre-thaw corridor with no resident intersection. Every unmeasured or resident-corridor case stays dense. |
| `refineWithinDomain` | Dense publication. The measured local transaction candidate did not improve wall time. |

## Persistent values at scale

`union` classifies a pair before selecting its schedule: empty and repeated
operands return an existing value, separated operands merge at their seam,
subsets reuse their superset, skewed pairs insert the smaller support, and the
remaining cases rebuild.

```haskell
insertionIsCheaper addition base = addition <= 64 || addition <= base `quot` 8
```

Local schedules publish a fresh immutable mesh from the pages they touched, so
readers continue observing the previous value while the next is built. A failed
operation returns its obstruction with both operands unchanged.

`Moonlight.Triangulation.Parallel.unionsConcurrently` interprets the same pure
join tournament with bounded concurrency.

## Source authority

The executable schedule decisions live in
[`SetAlgebra`](../src-build/Moonlight/Triangulation/SetAlgebra.hs),
[`BulkLoad`](../src-build/Moonlight/Triangulation/BulkLoad.hs),
[`Join.Plan`](../src-build/Moonlight/Triangulation/Internal/Join/Plan.hs), and
[`Cdt.Union`](../src-build/Moonlight/Triangulation/Internal/Cdt/Union.hs). The
focused measurements live in
[`JoinBench`](../bench/join/Moonlight/Triangulation/JoinBench.hs); this page is
their consumer-facing projection.
