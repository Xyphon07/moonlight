# Foreign bindings

`moonlight-planar.cabal` owns the private Haskell `ffi` sublibrary and
`moonlight-triangulation-c` foreign library. Python, TypeScript, and Rust are
thin lifecycle and bulk-layout leaves over that sole ABI; none implements
triangulation, overlay, publication, valuation, or morphology.

`Moonlight.Triangulation.Foreign.Contract` is the closed wire authority. Its
pure renderer owns the installed header, Windows export list, Haskell export
module, TypeScript wire vocabulary and raw declarations, Python ctypes
declarations, and Rust extern block. Check or rewrite those derived sections
from `compiler`—never edit them directly:

```bash
../scripts/safe-cabal.sh run \
  moonlight-planar:exe:moonlight-triangulation-ffi-contract \
  --project-file=cabal.project.planar-dev \
  -- check foundation/moonlight-planar
```

From `compiler`, build and locate the shared library:

```bash
../scripts/safe-cabal.sh build \
  moonlight-planar:flib:moonlight-triangulation-c \
  --project-file=cabal.project.planar-dev --enable-shared -j1
cabal list-bin moonlight-planar:flib:moonlight-triangulation-c \
  --project-file=cabal.project.planar-dev --enable-shared
```

Set `MOONLIGHT_TRIANGULATION_LIBRARY` to that `.dylib`, `.so`, or `.dll`; Rust
linking also reads its directory from `MOONLIGHT_TRIANGULATION_LIB_DIR`.

## ABI version 2

Three opaque immutable carriers cross the boundary:

- `ml_mesh` retains a binary64 Delaunay mesh. Its `ml_mesh_site_*` algebra
  combines sites, not polygon interiors.
- `ml_region` retains the authoritative exact rational `PlanarRegion`.
  `ml_region_create_f64` admits every input coordinate exactly as its binary64
  dyadic value in one call, using point counts per loop and loop counts per
  component. The inverse bulk projection returns component/loop offsets and
  binary64 rendering points; it does not replace the exact retained geometry.
- `ml_structuring_element` retains one admitted origin-containing convex
  polygon for repeated offset, inset, opening, and closing calls.

Region union, intersection, difference, and symmetric difference call the one
Haskell overlay and grouped-publication path. Point location is exact. A single
valuation call returns Euler characteristic, exact reduced area as
`numerator/denominator`, and certified conventional-perimeter bounds. Morphology
returns a new region plus the existing operation/work receipt.

Every status-returning call accepts an optional `ml_obstruction` distinguishing
pointer/count/buffer failures from region layout, validation, overlay,
publication, valuation, projection, morphology, and runtime failures. Handles
remain valid until explicitly freed; every operation publishes a fresh output
handle. `ml_runtime_initialize` is idempotent and process-lifetime.

Python and Rust owned handles are thread-affine: create, use, and release each
handle on one thread. Python exposes context management plus `dispose`; Rust
releases through `Drop`. This prevents release from racing a foreign call after
the runtime has received the raw handle.

Batch related mesh edits into one `insert_many` call. Author a whole region with
one component list; there is deliberately no mutable builder or per-vertex FFI
surface.

## Python

```python
from fractions import Fraction
from moonlight_triangulation import Moonlight, PolygonComponent

moonlight = Moonlight()
with moonlight.region([
    PolygonComponent(((0, 0), (2, 0), (2, 2), (0, 2)))
]) as left, moonlight.region([
    PolygonComponent(((1, 0), (3, 0), (3, 2), (1, 2)))
]) as right, left.intersection(right) as intersection:
    assert intersection.valuations.area == Fraction(2, 1)
```

## TypeScript

The [`@moonlight/triangulation` package guide](./typescript/README.md) owns
installation, native-library loading, typed results, and resource lifetime.

## Rust

```rust
use moonlight_triangulation::{Moonlight, PolygonComponent};

let moonlight = Moonlight::initialize()?;
let region = moonlight.region(&[PolygonComponent {
    outer: vec![[0.0, 0.0], [2.0, 0.0], [2.0, 2.0], [0.0, 2.0]],
    holes: vec![],
}])?;
println!("{:?}", region.valuations()?.area);
```

Rust reports native refusals as `MoonlightError::AbiObstruction` and keeps ABI
version, initialization, malformed projection, and unknown-wire failures as
distinct variants; it never forges a native obstruction for a wrapper defect.
