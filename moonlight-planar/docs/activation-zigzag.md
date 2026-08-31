# Activation zigzag persistence

Use `Moonlight.Triangulation.Zigzag` when independently observed activation
point clouds do not form a monotone filtration. Each point carries a stable
label, each depth carries its own coordinates and exact squared-radius alpha
threshold, and adjacent observations meet through the canonical cospan

```text
K(depth i) -> K(depth i) union K(depth i+1) <- K(depth i+1).
```

The union is the compatibility context, not another sampled depth. Returned
interval endpoints therefore alternate between `ObservedDepth` and
`AdjacentDepthUnion`; both endpoints are inclusive.

## Use

On GHC 9.14 or newer, depend only on the three public surfaces named by the
workflow:

```cabal
build-depends:
  moonlight-homology >= 0.1.0.3 && < 0.2,
  moonlight-planar:dcel >= 1.0 && < 1.1,
  moonlight-planar:zigzag >= 1.0 && < 1.1,
  vector >= 0.13 && < 0.14
```

Construct a nonempty `ActivationSlice depth label` family, then call
`activationAlphaPersistence` for the stage-labelled barcode. Call
`activationAlphaZigzag` first when the admitted complexes, common cells, union
complexes, or checked inclusion maps are also required.
`activationComplexBasisAt` resolves each numeric chain basis back to its stable
labelled cells.

`ZigzagArrow` and `ZigzagInterval` are functorial carriers: use `fmap` or
`traverse` to relabel payloads and endpoints without rebuilding the checked
diagram or changing degree and multiplicity.

The compile-checked
[`squareLoopAcrossDepths`](./examples/Moonlight/Triangulation/Example/ActivationZigzag.hs)
example observes a square before its boundary appears, while it carries one
loop, and after its Delaunay faces fill that loop. Its degree-one result is:

```haskell
ZigzagInterval
  { zigzagIntervalDegree = HomologicalDegree 1
  , zigzagIntervalFirst = AdjacentDepthUnion 0 1
  , zigzagIntervalLast = ObservedDepth 1
  , zigzagIntervalMultiplicity = 1
  }
```

## Contract

- Labels, not mesh handles or input positions, identify vertices across
  depths. The input is `NonEmpty`, depth keys are unique, labels are unique within each slice, labels
  may appear or disappear between slices, and coordinates may move.
- Distinct labels at the same coordinate are rejected rather than silently
  merged. Every geometric, alpha-selection, chain, or correspondence failure
  is returned through `ActivationZigzagError`.
- Geometry and alpha comparisons are exact for the supplied finite binary64
  `Point` coordinates and `RadiusSquared` values. Induced homology maps and the
  zigzag interval decomposition use exact rational arithmetic.
- Each slice is triangulated once. At each degree, the reducer prepares and
  consumes every adjacent homology map once. It targets finite depth traces,
  not an unbounded stream.

`moonlight-planar:zigzag` owns only labelled alpha ingestion and the
adjacent-union witnesses. `Moonlight.Homology.Persistence` remains the sole
owner of checked chain maps and exact zigzag reduction.
