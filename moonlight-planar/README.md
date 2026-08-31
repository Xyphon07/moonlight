# moonlight-planar

> Part of **Moonlight**, the sheaf-theoretic computation layer beneath
> [Melusine](https://bluerose.blue) and Pale Meridian.

`moonlight-planar` carries native hexagonal regions plus Delaunay and
constrained Delaunay triangulations as lawful finite-set algebras. A hex region
is an immutable packed cell selection; a mesh is observed through its canonical
site set. In both domains operations close over the same kind of value: a
mesh represents its site set, a join returns a valid Delaunay representative,
and the result is a triangulation again — so the operations close, compose, and
fold. Operations return typed obstructions where the finite arena cannot
represent a result.

Delaunay triangulation, constrained Delaunay (CDT), exact rational planar
regions and labelled overlay, intrinsic valuations, polygonal Minkowski
morphology, exact regular/power geometry, the Voronoi dual, natural-neighbour interpolation, Ruppert
refinement, walk point location, convex hull, exact Shewchuk predicates,
exact zigzag persistence across non-nested activation depths, and versioned
binary serialization.

## Persistence rose

![Animated Moonlight persistence rose: exact Delaunay alpha filtration, persistent homology, Voronoi dual, and natural-neighbour field](./docs/persistence-rose/moonlight-triangulation-persistence-rose.gif)

[Open the animation directly.](./docs/persistence-rose/moonlight-triangulation-persistence-rose.gif)
· [Open the static vector poster.](./docs/persistence-rose/moonlight-triangulation-persistence-rose.svg)
· [Read the exhibit guide.](./docs/persistence-rose/README.md)

## Operations

| Operation | Use when | Inputs | Result |
| --- | --- | --- | --- |
| `hexLayout` / `hexNeighbourCoord` | A bounded native hexagonal world needs dense identity and arithmetic adjacency | Axial origin and extents / coordinate and direction | `HexLayout` / optional neighbour / typed layout obstruction |
| `hexRegionUnion` / `hexRegionIntersection` / `hexRegionDifference` / `hexRegionSymmetricDifference` | Native cell selections must combine without triangulation or polygon construction | Two same-layout `HexRegion`s | Packed `HexRegion` / layout mismatch |
| `restrictHexRegion` / `reframeHexRegion` | A local section changes context | Target layout and region | Packed restricted/reframed region / typed restriction obstruction |
| `glueCompatibleHexRegions` | Local sections must descend only after agreeing on overlaps | Nonempty local-region family | Glued `HexRegion` / exact overlap witness |
| `hexRegionPlanarRegion` | Native selected cells need exact polygonal operations | `HexRegion` | Existing `PlanarRegion` / typed boundary or publication obstruction |
| `delaunayGeometry` | Coordinates own the input | `Vector Point` | Geometry-only mesh / `BuildError` |
| `delaunayFromCoordinates` | Payloads have a separate plane | Defaults, points, payloads, duplicate policy | `BuildResult` / `BuildError` |
| `insert` / `insertAt` / `insertMany` | An immutable mesh gains sites or payload replacements | Mesh plus payloads or explicit points | `InsertionResult` / `BuildResult` / `BuildError` |
| `withSession` | Many inserts and removals should publish once | Mesh, peak added-site count, composed `Session` | Result, mesh, and `BuildStats` / `BuildError` |
| `withScopedTriangulation` | Local traversal must make cross-mesh identifiers unrepresentable | Mesh and rank-2 continuation | Zero-cost scoped mesh and identifiers |
| `removeVertex` / `locateAndRemove` | A resident handle or exact position must be removed | Mesh plus `VertexId` / `Point` | `RemovalResult` / optional result / `BuildError` |
| `siteRelation` | Supports need exact classification | Two meshes | `SiteRelation` |
| `union` / `unions` | Unconstrained supports must join | Two meshes / mesh list | Union mesh / `BuildError` |
| `intersection` / `intersectionWith` | Shared support is required | Two meshes; optional payload combiner | Common-site mesh / `BuildError` |
| `difference` / `symmetricDifference` | Left-only / exclusive support is required | Source and mask / two meshes | Result mesh / `BuildError` |
| `constrainedDelaunay` | Segments author topology | Defaults, positioned vertices, index pairs | `BuildResult` / `CdtError` |
| `addConstraintEdge(s)` / `removeConstraintEdge` | Constraints change on a resident mesh | Mesh plus vertex pair(s) / edge | Constraint receipt or mesh / `CdtError` |
| `unionConstrained` / `unionConstrainedWith` | Arbitrary constrained meshes must join | Two meshes; optional payload combiner | Constrained mesh / `ConstrainedUnionError` |
| `extendConstrainedWith` | One constrained mesh owns identity | Combiner, base, extension | `ConstrainedExtensionResult` / `ConstrainedUnionError` |
| `joinSeparatedConstrained` | Inputs are strictly x-separated | Face predicate, refinement parameters, two meshes | `ConstrainedSeamResult` / `ConstrainedUnionError` |
| `refine` | Quality applies globally | Vertex constructor, parameters, mesh | `RefinementResult` / `BuildError` |
| `refineWithinDomain` | Quality applies to admitted faces | Vertex constructor, parameters, face set, mesh | `RefinementDomainResult` / `BuildError` |
| `faceComponents` | Bounded faces must descend by label | Mesh, `FaceId -> label` | `[(label, FaceComponent)]` |
| `componentBoundary` | One component must publish as loops | Mesh, `FaceComponent` | `RegionBoundary` / `BoundaryObstruction` |
| `locatePoint` / `locatePointWithHint` | An admitted point needs an exact cell classification | Mesh, `QueryPoint`, optional hint | `Location`, optionally with `LocationStats` |
| `lineIntersections` | A segment needs its ordered mesh crossings | Mesh and admitted endpoints | `[Intersection]` |
| `verticesInCircle` / `verticesInRectangle` | A metric window selects resident sites | Mesh and circle / rectangle | Handles / typed metric error |
| `exactClipRetainedPolygon` | A convex exact polygon must meet closed half-planes while retaining source lines | Retained polygon and half-plane list | Empty, point, segment, or polygon section plus receipt / `ExactClipError` |
| `overlayLayers` | Layers need one exact arrangement | Two `PlanarLayer`s | `OverlayResult` / `OverlayError` |
| `overlayAll` | A nonempty layer family needs one tuple-labelled common refinement | `NonEmpty (PlanarLayer label)` | `PlanarLayer (NonEmpty label)` / `OverlayError` |
| `overlayClosedUnion` / `overlayClosedIntersection` / `overlayRegularizedDifference` | Boolean output may retain lower cells | Two label predicates, `OverlayResult` | `ExactCellSet` / `OverlaySelectionError` |
| `overlaySelectedRegion` | Selected faces must publish as polygons | Label-pair predicate, `OverlayResult` | `PlanarRegion` / `RegionPublicationError` |
| `overlayMass` / `overlayConfusion` | Only exact selected area or the finite label-pair matrix is needed | Predicate / `OverlayResult` | `ExactArea` / exact mass map |
| `layerCovers` | A labelled layer must cover an admitted polygonal window | `PlanarLayer`, `PolygonComponent` | `()` or `LayerCoverageError` carrying the exact gap |
| `cellValuations` / `regionValuations` | Intrinsic measures are required | `ExactCellSet` / `PlanarRegion` | `PlanarValuations` / `ValuationError` |
| `minkowskiSum` / `polygonOffset` | Regions must add / expand | Two regions / element and region | Region plus receipt / `MinkowskiError` |
| `erodeBy` / `openWith` / `closeWith` | Regularized morphology is required | Structuring element, region | Region plus receipt / `MinkowskiError` |
| `regularTriangulation` | Weighted sites need unbounded topology and exact dual geometry | Nonempty `PowerSite` family | Opaque regular topology, dispositions, segments/rays/lines, and receipt / `PowerDiagramError` |
| `insertRegularSite` / `removeRegularSite` | A persistent regular value gains or loses one stable-labelled site | `PowerSite` / label and `RegularTriangulation` | New regular value, changed-site support, and exact disposition transitions / `RegularEditError` |
| `reweightRegularSites` | Stable sites receive one optimizer or simulation weight update | `Map label PowerWeight`, `RegularTriangulation` | New regular value, changed-site support, and exact disposition transitions / `RegularEditError` |
| `boundedPowerDiagram` | Labelled weighted sites need exact cells inside one finite convex domain | `ConvexPolygon`, nonempty `PowerSite` family | Total labelled dispositions plus rational-width receipt / `PowerDiagramError` |
| `boundedPowerDiagramFromRegular` | An admitted regular value already owns the weighted-site topology | `ConvexPolygon`, `RegularTriangulation` | Exact bounded cells without rebuilding the lifted hull / `PowerDiagramError` |
| `upperEnvelope` | Exact affine forms need their labelled argmax decomposition in a polygonal window | `PolygonComponent`, `Map label AffineForm` | `PlanarLayer (Maybe label)` / `UpperEnvelopeError` |
| `alphaShapeContainsFace` | One face at one radius is enough | `RadiusSquared`, mesh, `FaceId` | `Bool` |
| `alphaFiltration` | Every critical radius matters | Delaunay mesh | `AlphaFiltration` / `AlphaFiltrationError` |
| `regularAlphaFiltration` | Weighted regular topology needs signed exact births | `RegularTriangulation` | `RegularAlphaFiltration` / `RegularAlphaError` |
| `fromExactCellSet` | Exact cells need a generic complex view | `ExactCellSet` | `DCELComplex` |
| `filteredAlphaComplex` | Alpha births must enter persistence | `AlphaFiltration` | Filtered chain complex / `DCELError` |
| `activationAlphaZigzag` | Non-nested labelled depths need checked correspondence | `NonEmpty (ActivationSlice depth label)` | Admitted complexes, adjacent-union witnesses, and checked zigzag / `ActivationZigzagError` |
| `activationZigzagIntervals` / `activationAlphaPersistence` | An admitted zigzag / labelled depth family needs its exact barcode | `ActivationZigzag` / `NonEmpty (ActivationSlice depth label)` | Stage-labelled exact zigzag intervals / `ActivationZigzagError` |
| `canonicalize` | Numbering must ignore construction history | Mesh with unit edge/face payloads | Canonical mesh / `BuildError` |

[Interpret cell sets as incidence categories.](docs/category-observatory/README.md#cell-complex-and-category-interpretation)
· [Compute persistent alpha topology.](docs/persistence-rose/README.md#exact-alpha-filtration-and-persistent-topology)
· [Track topology across non-nested activation depths.](docs/activation-zigzag.md)
· [Compute weighted alpha persistence.](docs/weighted-alpha.md)

## Exact power cells and affine envelopes

`PowerWeight` admits a finite signed additive offset through its own
`PowerWeightError`; `PowerSite` admits its binary64 position once.
`regularTriangulation` constructs the exact lifted upper hull once. It retains
every admitted labelled site as its semantic value; visibility, regular faces,
and weighted-dual segments, rays, full lines, or collapsed degenerate edges are
sealed derived projections. Pure insertion, removal, and batch reweighting
return exhaustive typed disposition transitions. Exact conflict-cavity and
face-star descent update only affected topology; an internal typed refusal
falls back once to the canonical batch constructor.
`boundedPowerDiagram` then clips only regular-neighbour inequalities against the
`ConvexPolygon`; globally hidden sites need no clip, while lower-dimensional
sites use the complete HPI oracle. It retains one full-dimensional,
lower-dimensional, empty, coincident-equivalent, or coincident-dominated result
per label. `powerDiagramPlanarLayer` is only the derived full-dimensional view.
When a regular value is already available, `boundedPowerDiagramFromRegular`
reuses its normalized lifted hull.

`upperEnvelope` maps exact `c0 + cx*x + cy*y` forms into the same owner. Its
planar result contains only two-dimensional winning regions, with `Nothing`
outside the window; nonconvex windows and holes use the canonical overlay.

[Edit and reweight the complete regular-site section.](docs/regular-site-algebra.md)
· [Lower weighted topology into exact persistence.](docs/weighted-alpha.md)

## Foreign bindings

The C ABI and its Python, TypeScript, and Rust consumers are documented in the
[foreign-bindings guide](ffi/bindings/README.md). It owns construction, ABI
versioning, lifecycle, obstruction semantics, and consumer examples.

## Algebraic contract

`union` is idempotent; commutativity and associativity hold after
`canonicalize`. Structural `Eq` is resident equality; use `siteRelation` for
support comparison.

### Set-operation types

```haskell
union :: JoinSemilattice annotation
      => Triangulation 'Unconstrained annotation () () ()
      -> Triangulation 'Unconstrained annotation () () ()
      -> Either BuildError (Triangulation 'Unconstrained annotation () () ())

unions :: JoinSemilattice annotation
       => [Triangulation 'Unconstrained annotation () () ()]
       -> Either BuildError (Triangulation 'Unconstrained annotation () () ())

siteRelation
      :: Triangulation leftMode leftAnnotation leftDirected leftUndirected leftFace
      -> Triangulation rightMode rightAnnotation rightDirected rightUndirected rightFace
      -> SiteRelation

intersection
      :: Triangulation 'Unconstrained () () () ()
      -> Triangulation 'Unconstrained () () () ()
      -> Either BuildError (Triangulation 'Unconstrained () () () ())

intersectionWith
      :: (leftAnnotation -> rightAnnotation -> annotation)
      -> Triangulation 'Unconstrained leftAnnotation () () ()
      -> Triangulation 'Unconstrained rightAnnotation () () ()
      -> Either BuildError (Triangulation 'Unconstrained annotation () () ())

difference
      :: Triangulation 'Unconstrained leftAnnotation () () ()
      -> Triangulation 'Unconstrained rightAnnotation () () ()
      -> Either BuildError (Triangulation 'Unconstrained leftAnnotation () () ())

symmetricDifference
      :: Triangulation 'Unconstrained annotation () () ()
      -> Triangulation 'Unconstrained annotation () () ()
      -> Either BuildError (Triangulation 'Unconstrained annotation () () ())
```

### Zigzag composition

The activation surface composes admitted local geometry with exact global
reduction through the existing typed obstruction:

```haskell
activationAlphaPersistence slices =
  activationAlphaZigzag slices >>= activationZigzagIntervals
```

Each slice yields a canonical labelled subcomplex `K_i`. Adjacent sections glue
through `K_i -> K_i ∪ K_{i+1} <- K_{i+1}`: the union is the join of labelled
subcomplexes, and both legs are checked chain maps satisfying
`boundary . inclusion = inclusion . boundary`. The glued zigzag is therefore a
finite path in chain complexes; rational homology and interval decomposition
produce its authoritative global barcode. Betti profiles are derived views,
while any failed local, overlap, or gluing obligation remains an
`ActivationZigzagError`.

[Publication schedules and scale behavior.](docs/mesh-publication.md)

## Use

For native hexagonal worlds, depend only on the bottom component. It has no
DCEL, overlay, Homology, `containers`, or serialization dependency:

```cabal
build-depends:
  moonlight-planar:hex >= 1.0 && < 1.1
```

```haskell
layout <- hexLayout (HexCoord 0 0) 1024 1024
left <- hexRegionFromCoords layout [HexCoord 4 7, HexCoord 5 7]
right <- hexRegionFromCoords layout [HexCoord 5 7, HexCoord 6 7]
combined <- hexRegionUnion left right
```

The pure Boolean kernels visit one `Word64` per 64 cells. Exact polygons,
Homology chains, and bytes are explicit derived interpretations in the default,
`cell-complex`, and `serialize` components respectively.

Use the broad default library with explicit `Vector` inputs. It exposes the
facade and the safe leaf modules; construction and set algebra retain their
typed obstruction in `Either`:

```cabal
build-depends:
  base >= 4.19 && < 5,
  moonlight-planar >= 1.0 && < 1.1,
  vector >= 0.13 && < 0.14
```

Use `withScopedTriangulation` for allocation-free local incidence traversal.
Use `Handles.Dynamic` when a handle must escape. Fixed `VertexId`/edge/face
values are unchecked resident indices and belong only with the mesh that issued
them.

[`SetAlgebra.hs`](docs/examples/Moonlight/Triangulation/Example/SetAlgebra.hs)
is the smallest compile-checked workflow. The
[example index](docs/examples/README.md) adds constrained interiors,
alpha-boundary descent, exact overlay and valuations, and polygonal morphology.

For a smaller compile/dependency cone, depend on only the component modules you
import:

```cabal
build-depends:
  moonlight-planar:dcel >= 1.0 && < 1.1,
  moonlight-planar:build >= 1.0 && < 1.1
```

## Architecture

The resident DCEL is immutable structure-of-arrays over paged copy-on-write
storage; local mutation is sealed in `ST`. Half-edge twins are index
complements. Exact predicates use bounded machine-word evaluation and fall
through to aligned `Integer` arithmetic when required.

## Consumer libraries

The Cabal manifest owns component visibility and dependencies; this is its
consumer-facing projection. Use a public sublibrary directly when its smaller
dependency cone matters; trusted implementation units remain private.

| Dependency | Imports | Purpose |
| --- | --- | --- |
| `moonlight-planar:hex` | `Moonlight.Hex.Coordinate`, `.Element`, `.Region` | Native axial elements, arithmetic neighbours, packed finite-set algebra, restriction, and compatible gluing; depends only on `base`, `deepseq`, `transformers`, and `vector`. |
| `moonlight-planar` | `Moonlight.Triangulation` plus the public `dcel`, `build`, and `dual` modules | Broad construction, observation, set algebra, CDT, refinement, alpha, Voronoi, interpolation, and exact planar surface. |
| `moonlight-planar:core` | `.Scalar`, `.LineSideInfo` | Low-level exact scalars and paged storage primitives. |
| `moonlight-planar:dcel` | `.Types`, `.Dcel`, `.Handles.*`, `.PointLocation`, `.FloodFillIterator`, `.IntersectionIterator`, `.Alpha`, `.Exact`, `.Region`, `.Valuation` | Immutable mesh vocabulary, exact geometry, scoped/owning observations, and the explicit fixed-index kernel; no construction kernel. |
| `moonlight-planar:build` | `.BulkLoad`, `.Session`, `.Removal`, `.Cdt`, `.Refinement`, `.SetAlgebra` | Construction and persistent editing over the DCEL; no dual, serialization, concurrency, or Homology dependency. |
| `moonlight-planar:dual` | `.Voronoi`, `.Voronoi.Handles`, `.Interpolation` | Voronoi observations and natural-neighbour interpolation. |
| `moonlight-planar:parallel` | `.Parallel` | Bounded concurrent union; adds `async`. |
| `moonlight-planar:serialize` | `Moonlight.Hex.Serialization`, `Moonlight.Triangulation.Serialization` | Versioned binary envelopes; adds `binary`, `bytestring`, and `transformers`. |
| `moonlight-planar:cell-complex` (GHC 9.14+) | `Moonlight.Hex.CellComplex`, `Moonlight.Triangulation.CellComplex` | Native-hex and exact-DCEL chain interpretations; adds Homology. |
| `moonlight-planar:zigzag` (GHC 9.14+) | `.Zigzag` | Labelled activation alpha complexes, adjacent-union witnesses, and stage-labelled zigzag intervals; adds Homology and construction. |

[Build and validate package components locally.](docs/development.md)
