# Compile-checked usage examples

[Return to the package overview.](../../README.md#use)
· [Browse the documentation.](../README.md)

The ordinary examples import their public geometric and construction owners
directly; there is no `Moonlight.Planar` umbrella.
The activation example names its opt-in `zigzag` component explicitly. Package
tests compile every module and own their deterministic expected results.

| Workflow | Canonical source |
| --- | --- |
| Join and meet overlapping site sets | [`SetAlgebra.hs`](./Moonlight/Planar/Example/SetAlgebra.hs) |
| Recover a constrained annulus interior | [`ConstrainedRegion.hs`](./Moonlight/Planar/Example/ConstrainedRegion.hs) |
| Descend an alpha face section to its boundary | [`AlphaBoundary.hs`](./Moonlight/Planar/Example/AlphaBoundary.hs) |
| Overlay, measure, and morph exact regions | [`PlanarRegion.hs`](./Moonlight/Planar/Example/PlanarRegion.hs) |
| Track an alpha loop across non-nested depths | [`ActivationZigzag.hs`](./Moonlight/Planar/Example/ActivationZigzag.hs) |

[The public example test](../../test/examples/Main.hs) and
[zigzag test](../../test/zigzag/Main.hs) check every published summary against
these sources.
