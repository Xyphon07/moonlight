# Exact weighted alpha filtration

`Moonlight.Triangulation.RegularAlpha` derives a signed exact alpha filtration
from the canonical `RegularTriangulation`. It reuses resident regular faces,
edges, weighted-dual points, rays, and lines; no second weighted geometry is
constructed.

```haskell
filtration <- regularAlphaFiltration regular
filtered <-
  filteredPlanarComplex
    (regularAlphaComplex filtration)
    (regularAlphaBirths filtration)
pairs <- mod2PersistentPairs filtered
```

`PowerAlphaBirth` is an opaque exact rational. Negative births are lawful: a
positive site weight can make a simplex present before power radius zero.
Hidden and coincident-subordinate sites remain in the editable regular value but
contribute no simplex. `filteredPlanarComplex` is the only lowering boundary;
Homology owns chain and filtration validation.

Use ordinary `alphaFiltration` for an unweighted Delaunay DCEL. Use
`regularAlphaFiltration` when weights, exact dual rays, or repeated regular-site
edits are semantic inputs.

```console
cabal bench moonlight-triangulation-alpha-bench
```

[Return to the operation index.](../README.md#operations)
· [Edit the regular site section.](./regular-site-algebra.md)
