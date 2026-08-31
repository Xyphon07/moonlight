# `@moonlight/triangulation`

[Read the shared ABI contract.](../README.md#abi-version-2)

Typed TypeScript access to Moonlight's immutable Delaunay meshes, exact planar
regions, and polygonal morphology. The package owns JavaScript resource
lifetime and error terms; the generated raw declarations descend from the same
Haskell contract as the C header, Python declarations, and Rust declarations.

This source package does not pretend to contain a native binary. Supply an
explicit path to the ABI-version-2 shared library:

```typescript
import {
  RegionLocation,
  formatMoonlightFailure,
  loadMoonlight,
  type Result,
} from "@moonlight/triangulation";

const moonlight = loadMoonlight({
  kind: "path",
  path: "/absolute/path/to/libmoonlight-triangulation-c.so",
});

if (!moonlight.ok) {
  throw new Error(formatMoonlightFailure(moonlight.error));
}

const region = moonlight.value.region([
  { outer: [[0, 0], [2, 0], [2, 2], [0, 2]] },
]);

if (region.ok) {
  const location: Result<RegionLocation> = region.value.locate([1, 1]);
  region.value.dispose();
}
```

Every operation that can be refused returns `Result<Value, MoonlightFailure>`.
No raw Koffi handle, constructor capability, or numeric status code crosses the
package entrypoint. Owned meshes, regions, and structuring elements implement
both `dispose()` and `Symbol.dispose`; morphology therefore keeps the honest
domain method `region.close(element)`.
