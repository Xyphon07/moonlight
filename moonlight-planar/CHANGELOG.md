# Changelog

## 1.2.0.1 - 2026-10-01

- Simplify README and guide wording; replace machine-specific example paths
  with paths relative to the checkout.
- No library or API changes from 1.2.0.0.

This is release chronology, not current interface authority. The package
[README](./README.md), Cabal manifest, and linked focused guides own the live
surface.

`moonlight-planar` follows the
[Haskell Package Versioning Policy](https://pvp.haskell.org).

The serialization format carries its own version tag, independent of the package
version; any change to it is recorded here explicitly.

## 1.2.0.0 - 2026-09-23

* Breaking: remove `splitStepHalf` from `Moonlight.Planar.Curve`. Use
  `splitStep unitHalf`, or `splitStep` at any exact parameter; there is no
  compatibility alias.
* Add `splitStep`, exact de Casteljau subdivision at an arbitrary
  `UnitInterval` for lines, quadratics, cubics and positive-weight rational
  quadratics. Each child is reparameterized over its own unit interval, and the
  right child is rebased at the split point.
* Add `ParameterSpan` (an ordered, checked pair of unit parameters, refused
  with `ReversedParameterSpan`) and `restrictStep`, which reparameterizes a
  located step to the span through polar forms and relocates it to the source
  point. Equal endpoints yield a stationary step.
* Add `StepJet` and `jetStep`: the value and the first and second derivatives
  with respect to the step's own parameter. The rational quadratic uses the
  homogeneous quotient rule with a positive weight witness. `startJet` and
  `endJet` remain as closed-form endpoint specializations.
* Add `Moonlight.Planar.Curve.Measure`: certified Euclidean arc length of
  lines, quadratics, cubics and positive-weight rational quadratics as an
  immutable `MeasuredTrail` that retains its source subpath, accepted spans and
  cumulative rational enclosures.
  * One chord/control-polygon enclosure kernel serves all four shapes.
  * A global error allocation keeps the total width within the policy
    tolerance.
  * `measurePolicy` is total over a tolerance, a `RadicalPrecision` and an
    admitted `SubdivisionBudget` (depth, leaves, exact bit width), which
    `subdivisionBudget` validates. The bit budget covers source values,
    retained values and query requests. An exhausted budget refuses as
    `MeasureBudgetExhausted` carrying the shared `BudgetObligation`.
  * `pointAtLength` and `pointAtFraction` answer inverse queries as an
    `ArcSample`: a site on its source step with a certified parameter bracket
    and a residual within tolerance.
  * A `TrailSite` is an opaque located position on one step of its source,
    with its step index, `UnitInterval` parameter, exact point, join side and
    jet. `sampleSite` reads the site of an `ArcSample`. The sample itself adds
    only the bracket and the residual.
  * Span and sample parameters, and those carried by `SpanRefused`, are
    `UnitInterval` values admitted once and never rechecked.
  * A distance beyond the certified upper length is refused, and so is one
    between the lower and upper lengths of an inexact total. Neither is
    clamped.
  * Exhausted budgets refuse with the source step, the bracket and the
    obligation.
* Add `Moonlight.Planar.Curve.Frame`: measured tangent frames for
  path-directed placement.
  * `regularFrame` reads a `FrameSite`, either a sampled `ArcSample` or an
    exact `TrailSite` from `exactTrailSite`, through one jet and
    normalization path.
  * The first column is the tangent and the second is its left normal
    `(-t_y, t_x)`. The frame carries its certified scale bound
    (`measuredFrameScaleSquared`).
  * A stationary site, a corner (including a closed seam), an out-of-range
    step or an unresolved normalization is refused. No tangent is invented.
  * `fractionRun` and `sampleRun` place an evenly spaced run of arc fractions.
    An empty run, non-positive spacing and a closed seam repeated at both ends
    are refused.
* Breaking: `Moonlight.Planar.Curve.Region.lowerSimpleRegion` now takes a
  `SubdivisionBudget` and returns `CurveTopologyEvidence` with the region and
  lowered paths. It certifies that the region's components, holes and nesting
  are the curves' own, within a stated domain:
  * each contour subdivides within budget into monotone pieces;
  * each joint is wedge-separated (a cusp refuses);
  * non-adjacent pieces are hull-separated or certified not to cross;
  * the actual lowered loops nest exactly as the curves do.

  Outside that domain it refuses with `CurveTopologyRefused`, a new
  `CurveRegionError` constructor. Its obstructions name the contour spans and
  keep three kinds of case apart:
  * certified contacts, which carry an exact common source point;
  * certified crossings, which carry a certificate and never a point;
  * unresolved contacts, which carry the exhausted budget.

  Every retained piece's located controls and every crossing certificate are
  admitted against the budget's bit width. `certifySimpleRegion` certifies
  without lowering, and
  `certifiedPointLocation` locates a point strictly outside every certified
  piece hull. There is no uncertified variant.
* Add `Moonlight.Planar.Curve.Proximity`: bounded distance and clearance
  between two curves.
  * `curveDistance` returns an enclosure of the distance within the policy
    tolerance, with its live candidate span pairs. The pairs are never a claim
    that the nearest pair is unique.
  * `clearance` is strict: it holds when the distance exceeds the given
    distance, so zero means disjoint.
  * Contact is witnessed in exactly three ways:
    * exact sites closer than the distance;
    * an exact shared site;
    * a certified transversal crossing, which carries no point.
  * Distance and contact are separate: a tangency can have its distance
    decided while its contact stays unresolved.
  * The subdivision budget's bit width bounds every source span, retained
    site, displacement, certificate and enclosure endpoint, and the clearance
    threshold at entry.
    * An oversized request refuses before any work, as
      `ProximityRequestRefused`.
    * More source-step pairs than the budget's leaves refuse from the step
      counts alone, as `ProximityStepPairsRefused`, before any pair is built.
    * A source step too wide for the budget refuses as
      `ProximitySourceRefused`, naming the span.
    * A failure before any enclosure exists refuses without one, as
      `ProximityUnenclosed`.
    * A later failure carries the last admitted enclosure and the live
      candidates, as `ProximityRefused`.
* Region valuation and curve measurement share one private radical-length
  owner. `Moonlight.Planar.Valuation`'s exports, error type and 128-bit
  published results are unchanged.

## 1.1.0.0 - 2026-09-23

* Require `moonlight-algebra >= 0.1.1`, the first release carrying the
  public `join` sublibrary the construction kernels depend on.
* `tested-with` names GHC 9.14.1 alone. Since 1.0.0.0 the package has not
  solved on 9.8, 9.10 or 9.12: ungated components require `base >= 4.22`,
  and so does every `moonlight-algebra` library.

* Nearest queries minimize exact squared distance and then resident handle.
  Constrained meshes use complete site selection; Delaunay ties no longer stop
  at a merely local minimum handle. Real-constructor regressions cover hidden
  constrained minima, cocircular ties and rounded binary64 near-ties.
* Move convex admission, hulls, observations and errors to `Moonlight.Planar.Convex`
  in the focused DCEL component. Remove morphology-owned convex authoring and the
  duplicate generator dual carrier; the generator now owns `PowerDualEdge`.
  Foreign raw convex admission has its own obstruction code 207.
* Add restricted exact prepared power-mass sections, with quadratic areas,
  complete strict cell/window certificates, empty-cell witnesses and exact open
  interval expiry. Existing regular-site dispositions remain unchanged.
* Encode V6 through storage-owned logical page folds without gathering shared
  stores. Wire bytes, default payloads and field-major topology order are unchanged.
* Compute sign-only lifted predicates using positive integer row scaling.
  Rational determinant magnitudes remain authoritative for volume ordering.

* Finish the package rename inside the package (2026-09-04). The module
  namespace `Moonlight.Triangulation` and everything beneath it is now
  `Moonlight.Planar`, and every test suite, benchmark, and executable
  component named `moonlight-triangulation-*` is now `moonlight-planar-*`.
  The C ABI is untouched: the `moonlight-triangulation-c` foreign library,
  the `moonlight_triangulation.h` header, the exported symbol names, and the
  Python, TypeScript, and Rust binding package names all keep their names,
  because the bindings consume them by name.

* Surgery on repeated validation (2026-09-03). A `Triangulation` is
  admitted once, at its trust boundary: `refine` no longer re-validates
  admitted input, local refinement no longer re-walks its closure after
  the transaction, overlay residency no longer re-checks what the
  constrained value certifies, and the region valuation sweep plans its
  events once. The closure statistics are renamed for what they count
  (`ClosureStats`, `closureFaces`, `refinementClosureStats`).
* `CanonicalAdmission` is a lineage witness carried on the value: the
  normal form's owner mints it, decode verifies it in one linear pass
  over the arenas, and canonical publication of a value already in normal
  form is the identity. No serialization format change.
* The fixed-width orientation worker had never run (`MachDeps.h` was not
  included, so the width test silently fell back). It runs now, and a
  fixed-width in-circle worker joins it, cross-checked against the
  arbitrary-precision determinant on 15,000 quadrilaterals.
* `segmentRelationWith` decides collinearity before shared endpoints:
  collinear segments overlapping for a positive length are an overlap
  whether or not they share an endpoint; a straight continuation stays a
  shared endpoint. The frozen relation oracle gains eight contact families.
* `ExactLengthExpression` is a radical normal form: rational squares fold
  into the coefficient, and radicands in one square class merge onto their
  common kernel. The term field is `lengthRadicand :: Integer`
  (was `squaredLength :: ExactRational`).
* The facade is narrowed to the value algebra; statistics, sessions,
  incremental bulk insertion, hint-seeded location and interpolation move
  to `Moonlight.Planar.Telemetry`. `polygonInset` is retired in
  favour of `erodeBy`; three never-raised `OverlayCellWitness`
  constructors and `HexPlanarCoverageInvalid` are removed, and full-cell
  hex coverage is total.
* Local refinement's closure statistics and receipt are counted from the
  domain's own faces and interface pairs; no closure set is materialized
  on the publication path. The set-derived counts survive as the test-side
  oracle (`topologyClosureSelectionStats`) with a law holding the two equal.
* Receipts: refinement allocation down 23 to 30 percent, the local-refine
  publication lane down about 70 percent end to end, hex intersection 65
  to 26 ms, decode of a 20k-site mesh down 23 MB; power, minkowski and
  overlay lanes at or below their previous numbers.

* Add packed native hex dilation, right-adjoint erosion, opening, closing,
  frontiers, connected-component labels, and multi-source shortest distances.
  These operate in the existing `HexRegion` owner without boxed coordinate or
  polygon intermediates.
* Add three exact `PlanarRegion` restrictions onto a caller-supplied
  `HexLayout`: closed center membership, full-cell coverage, and closed
  intersection. Their boundary semantics are distinct in the names and types.
* Centralize pure, effectful, and topology-derived packed construction beneath
  the private `HexRegion` representation owner. Exact full-cell coverage now
  uses `hexRegionGenerateM` and no longer exposes an impossible packed-word
  admission failure; frontier kernels publish their final words without a
  throwaway dilation or erosion vector.
* Replace the package-local annotation semilattice with the canonical
  dependency-light `Moonlight.Algebra.Pure.Join` owner, and hide the raw
  `RefinementParameters` constructor behind checked authoring operations.
* Admit canonical packed hex words without copying, decode versioned hex bytes
  in bulk instead of rebuilding membership cell by cell, and restrict compatible
  gluing to the word spans covered by each local section.
* Move the canonical hex byte interpreter into the dependency-light
  `hex-serialize` component; native hex consumers no longer build the DCEL.
* Lower hex regions into Homology directly from the coordinate fold, without an
  intermediate boxed vector.

## 1.0.0.0 - 2026-08-31

* Rename the planar package from `moonlight-triangulation` to
  `moonlight-planar`. Triangulation modules and foreign symbols retain their
  domain names; the old Hackage package remains immutable history.
* Add the dependency-light public `hex` sublibrary. Global axial coordinates,
  canonical vertices and sides, bounded layouts, and immutable packed regions
  support constant-time neighbours and wordwise union, intersection,
  difference, symmetric difference, complement, restriction, reframing, and
  overlap-compatible gluing with typed witnesses.
* Add exact polygonal, cellular-chain, and versioned serialization
  interpretations without introducing a second planar, Homology, or wire
  authority.

## 1.5.0.0 - 2026-08-30

* Make the complete stable-labelled `PowerSite` section authoritative in
  `RegularTriangulation`. Add pure insertion, removal, and batch reweighting
  with exhaustive typed disposition transitions, plus bounded power-cell
  construction that reuses an already normalized regular value.
* Collapse edit publication to changed-site support plus exact disposition
  transitions. A normalized face/edge/star section supports exact slope walks,
  insertion conflict cavities, interior removal cavities, and atomic reweight
  composition. Local publication certifies incidence, cavity boundary, and
  lifted convexity; unsupported degeneracies reconstruct once through the exact
  batch owner. On the retained GHC 9.14.1 `-O1` 512-site fixture, full-result
  insertion falls from 250.7 ms / 304.7 MB to 0.53 ms / 0.61 MB, removal from
  246.4 ms / 303.5 MB to 0.72 ms / 0.87 MB, and a general one-site reweight
  from 99.4 ms / 119.2 MB to 22.8 ms / 34.2 MB. A common shift of all 512
  weights takes 0.22 ms / 0.27 MB without changing incidence. Remove the
  transient peak-hull counter
  from the persistent topology receipt: unlike its remaining fields, it was a
  construction-history measurement rather than a property of the value.
* Make batch-reweight validation proportional to the patch rather than the
  resident site set, and descend changed sites without an association-list
  copy.
* Add signed exact weighted-alpha filtration over resident regular topology, a
  generic labelled planar-complex lowering into Homology, and focused weighted
  preparation and persistence benchmark lanes.

## 1.4.0.5 - 2026-08-29

* Add the GHC-9.14 `zigzag` sublibrary. Stable labelled activation points become
  independently sampled exact alpha complexes; adjacent observations glue
  through checked union cospans, and Homology returns stage-labelled exact
  zigzag intervals without comparing DCEL handles across meshes.

## 1.4.0.4 - 2026-08-28

* Add exact affine upper envelopes through the existing power-cell owner, with
  a no-overlay convex fast path and exact restriction to nonconvex or holed
  polygonal windows.
* Add balanced n-ary layer refinement, exact window-coverage certification
  carrying the uncovered region, and direct selected/confusion area folds.
  On the 16-layer benchmark fixture, balanced refinement is 13x faster and
  allocates 13.5x less than left association; direct mass and confusion are
  14.6x and 16.5x faster than materializing polygonal regions.
* Add exact regular triangulations over the lifted affine generators, including
  per-label visible/lower-dimensional/hidden dispositions and weighted dual
  segments, rays, full lines, and collapsed degenerate edges. Coplanar hull
  triangles glue before extreme-site extraction, so implementation diagonals
  cannot promote a facet-interior generator.
* Route bounded power cells through the regular-neighbour graph. The production
  lane derives one radical axis per regular edge and clips one small neighbour
  section per visible site; globally hidden sites perform no HPI, while only
  lower-dimensional sites use the complete exact oracle.
* Make regular-topology descent output-sensitive: hull removal touches only the
  removed face stars, the next expansion comes from a strict conflict frontier,
  supporting-plane duals survive into publication, coplanar fans share one dual,
  and collinear classification is one monotone merge. Regular edges retain only
  their admitted generator pair; neighbour and clipping views are derived at
  their respective consumers rather than stored as competing authorities. On
  the retained GHC 9.14.1 `-O1` 512-site fixture, topology and full-construction
  allocation fall by 10.7% and 10.2%; the corresponding wall times improve by
  roughly 1%.
* Consolidate convex erosion and power-cell construction on one exact clipper
  that retains source affine lines, avoiding measured endpoint-coefficient
  swell without claiming smaller reduced output denominators.
* Replace repeated polygon scans with one exact angular closed-half-plane
  descent, including empty, point, segment, parallel, and coincident sections.
  Clip receipts report boundary compatibility, exact intersections, rational
  widths, regular incidence, oracle cells, and maximum per-cell constraints.
* Evaluate exact signs and affine intersections through integer cross-products
  with one rational normalization at publication. This removes intermediate
  `Ratio` normalization from the HPI predicate kernel.
* Consolidate affine and power inputs on direct exact score coefficients,
  publish already-admitted cells without revalidation, and fold clip receipts
  into one strict summary. On the retained GHC 9.14.1 `-O1` 169-site fixtures,
  construction falls from 171.4 ms / 351.2 MB to 79.2 ms / 105.4 MB for the
  well-conditioned family and from 142.3 ms / 296.7 MB to 65.6 ms / 82.4 MB
  for the near-parallel family. Mesh joins and n-ary overlays interpret one
  shared balanced tournament.
* Collapse four exact-planar test runners into one behavioral owner, return the
  canonical overlay obstruction directly from n-ary refinement, and share the
  bounded-cell and strict-turn folds. Direct mass and confusion now allocate
  22% and 5% less on their accepted fixtures.
* Execute the exact-planar, documentation-example, and public-component suites
  in CI. Publish only the local embedding certifier; arrangement constructors
  and resident diagonal schedules live in the private `overlay-internal`
  component shared only with package-owned oracle tests.
* Add nominal rank-2 mesh scopes for zero-cost safe traversal, retain owning
  handles for escaping values, and document fixed identifiers as the unchecked
  issuing-mesh index lane. Convex reflection now preserves its counter-clockwise
  carrier invariant directly.
* Preserve a noncanonical zero-site mesh when it is joined with the canonical
  empty identity. The previous zero-count guard order broke structural right
  identity when both operands represented the empty support differently.

## 1.4.0.3 - 2026-08-28

* Restore public `dcel` and `build` sublibraries as safe restrictions of their
  private implementation owners. The default library reexports the same lawful
  modules, so consumers may choose the broad surface or a smaller dependency
  cone without receiving a second implementation.
* Publish incremental insertion/removal, sealed edit sessions, typed point and
  shape queries, owning handles, payload traversals, dynamic constraint
  operations, and whole-mesh diagnostics through the canonical Haskell facade.
* Keep raw identifier constructors, resident representation, admitted-input
  construction, local transaction scheduling, seam witnesses, and local
  validation closures private.

## 1.4.0.2 - 2026-08-28

* Consolidate usage guidance behind one operation index and exact set-operation
  type contract. Focused guides now uniquely own publication schedules,
  examples, comparisons, exhibits, and foreign-consumer details instead of
  repeating them across the package overview.

## 1.4.0.1 - 2026-08-24

* Break `joinSeparatedConstrained` onto its stronger local-extension contract:
  callers now supply a source-face preservation section and
  `RefinementParameters`, and receive seam provenance and publication
  receipts from the one constrained seam owner.
* Extend `RefinementParameters` with an optional maximum edge length and add
  typed refusals for non-finite, non-positive, interface-crossing, protected
  face, bridge-budget, and final oversized-edge obstructions. Local refinement
  now publishes its final permitted-face witness and validation/publication
  receipts.
* Add source-preserving paged publication receipts, cached seam-frontier
  observations, bounded-region face selection, and exact validation-closure
  statistics without introducing a second mesh carrier.
* Preserve each boundary loop's exact resident DCEL vertex chain through
  `boundaryLoopResidentVertices`; `boundaryLoopVertices` remains the derived
  collinearity-reduced polygon observation.
* Ship the persistence-rose source observatory and replace the decorative alpha
  eclipse with the canonical four-site square interval computed by
  `alphaFiltration` and Moonlight Homology: its one `H1` class is born at exact
  squared radius 1 and dies at 2. The retained SVGs and GIF now have a checked
  regeneration/provenance command, and CI rejects stale exhibit artifacts.

## 1.3.0.3 - 2026-08-22

* Restore the package-overview Delaunay board on Hackage with a release-tagged
  absolute image URL; the chart remains package-owned and shipped in the source
  distribution.

## 1.3.0.2 - 2026-08-22

* Add an opaque exact alpha filtration over every resident Delaunay vertex,
  edge, and bounded face, exact threshold cell selection, checked integral
  cellular-chain lowering, and direct lowering into Moonlight Homology's
  ordered filtered complex.
* Add exact Gabriel/non-Gabriel birth laws, boundary monotonicity, degenerate
  support, and a cocircular-square persistence witness whose degree-one class
  is born at squared radius 1 and dies at squared radius 2.
* Add a focused benchmark comparing one persistence reduction with recomputing
  homology at every exact critical radius over the same prepared geometry.
* Compute exact edge lengths and face circumradii as aligned dyadic integer
  kernels with one rational normalization at publication, and lower already
  ordered DCEL incidence before Homology checks and seals the chain law.
* Batch every critical-radius Betti profile through Homology's dense rank
  quotient. On the shared 1,000-site Spade diagnostic this cuts Moonlight from
  941.312 ms and 3.11 GB allocated to a 34.177 ms median and 96.0 MB while
  preserving the exact receipt.
* Remove the duplicate packed radial-geometry arena from bulk Delaunay ingress.
  Coordinates remain in dense planes while a 32-bit vertex-id plane descends
  through prefix radix order and exact equal-prefix normalization.
* Refresh the complete 120-case construction board. Moonlight's mean is below
  plain Spade and Spade hierarchy on all 24 fixtures, with non-overlapping
  reported 2σ on 18 and 22 fixtures respectively.
* Remove promotional advantage callouts from both SVG projections, ship the
  regenerated small SVG with the package, and reference it directly from the
  README instead of displaying a stale remote mirror copy.

## 1.3.0.0

* Add the public GHC-9.14 `cell-complex` component. It interprets an admitted
  `ExactCellSet` as Homology's generic `CellComplex2D` without copying the mesh
  or introducing a second cell inventory, and the accompanying observatory
  executable derives its incidence category and normalized flag nerve.
* Keep the LLVM-optimized hot modules buildable with GHC 9.8 by restoring the
  legacy LLVM pass manager that its supported LLVM 15 toolchain requires.
* Withdraw `constrainedExtensionTriangulation`,
  `constrainedExtensionConstraintOutcomes` and
  `constrainedExtensionConstraintStats`. A constrained extension now publishes
  one `ConstraintBatchResult` through `constrainedExtensionConstraintBatch`,
  read with `constraintBatchTriangulation`, `constraintBatchOutcomes` and
  `constraintBatchStats`; `constrainedExtensionBuildStats` is unchanged. The
  batch is the same value `recoverConstraints` returns, so extension and
  standalone recovery are now read through one accessor set instead of two.
* Withdraw the aggregator modules `Moonlight.Planar.Handles`,
  `Moonlight.Planar.Handles.Iterators` and
  `Moonlight.Planar.Internal.DcelOperations` from the exposed module
  list. Every leaf they re-exported remains exposed under its own name; import
  the leaf.
* Add `BoundaryOrientation`, `boundaryLoopOrientation` and
  `componentBoundaryLoops`, so a face component publishes all of its boundary
  loops with each loop's orientation rather than only the outer one.
* Add `planarValuationsPerimeter`, the certified perimeter of a planar region
  taken directly from its valuations.
* Advance the C boundary to ABI version 2 with opaque exact-region and reusable
  structuring-element handles, one-call bulk authoring/projection, exact region
  union/intersection/difference/symmetric difference, point location, exact
  area plus Euler and certified perimeter observations, and existing polygonal
  Minkowski morphology with fixed-width receipts.
* Rename the former mesh Boolean symbols and Python, TypeScript, and Rust
  methods to `site_*`; they combine Delaunay site sets and are deliberately not
  compatibility aliases for the new polygonal region operations.
* Restore `-O0` for ordinary test bodies while retaining the filtered-predicate
  allocation witness at module-local `-O1`.
* Move the existing exact Overlay/Minkowski implementations from the `build`
  sublibrary into the main library and move `HintGenerator` from `dual` into
  that same apex owner. The `dual` sublibrary is now dcel-only and can compile
  concurrently with `build`; direct `:dual` consumers of `HintGenerator` must
  depend on the main library instead. No geometry type or runtime operation is
  duplicated.
* Reserve the planar DCEL bound rather than the general one when joining two
  separated triangulations. The separated seam copies two planar sources and
  then only adds, so its peak is its published result; the general reservation
  was a third again as much arena as the merge can ever reach. Measured at
  twenty thousand sites over twenty-one processes per arm, the separated join
  lanes fall 36.4% to 41.3% in allocation and 10.2% to 22.0% in elapsed time,
  with every unaffected lane byte-identical.
* Stop materializing an intermediate validated vector in `mesh_insert_many_f64`.
  Admission is unchanged — a malformed point anywhere still refuses the whole
  batch before any insertion — but the canonical coordinate is now applied
  where the point is used. Against a fifty-thousand-site mesh, allocation falls
  5.3% at a thousand added points and 26.5% at fifty thousand; wall time is
  unmoved, because geometric insertion, not admission, is the critical path.

## 1.2.0.1

* Admit GHC 9.8 as a tested compiler by spelling the package language as
  `GHC2021` plus `DerivingStrategies`, accepting `base-4.19`, and qualifying
  strict list folds through `Data.List`. This restores Hackage build and
  documentation generation without changing the API or binary format.

## 1.2.0.0

* Add exact rational planar regions and labelled common refinement with one
  provenance-bearing overlay carrier, closed 0-/1-/2-cell Boolean selection,
  and grouped polygon publication through the existing DCEL boundary owner.
* Add exact Euler characteristic and rational area plus symbolic radical length
  expressions with certified outward-rounded binary64 bounds.
* Add linear convex-polygon Minkowski convolution, general polygonal addition,
  and regularized polygonal erosion, opening, closing, offset, and inset through
  the existing CDT and overlay owners.
* Curate the exact region algebra through the main Haskell facade. The C ABI and
  language bindings remain intentionally unchanged.
* Change the binary wire format to version 6 so round trips preserve the vertex
  payload plane's optional fill, including the allocation-free unit payload
  used by geometry-only bulk construction. Version 5 is intentionally
  unsupported rather than decoded into a different resident representation.
* Map the dense-storage circle-sweep obstruction exhaustively at the C boundary
  as obstruction code 55.

## 1.1.0.0

* Add labelled bounded-face components and authoritative component boundaries
  with counter-clockwise outer loops, clockwise holes, exact collinear-vertex
  simplification, and typed pinch obstructions.
* Add exact Delaunay 2-simplex alpha filtration through
  `alphaShapeContainsFace` and the shared admitted `RadiusSquared` type.
* Replace `joinSeparatedConstrainedWith` with
  `joinSeparatedConstrained`; strict separation cannot combine coincident
  annotations, so the dead combiner and old name are gone.
* Change the binary wire format to version 5. Structural section counts now
  occupy one prefix and `decodeTriangulation` requires an explicit
  `DecodingBudget` plus `TrustedPayloadDecoders`, validating counts,
  relationships, packed-index bounds, a fixed-body lower bound, and total
  section elements before allocation while making external decoder trust
  explicit. Version 4 is intentionally unsupported.

## 1.0.1.0

* Add the geometry-only `delaunayGeometry` entrance and re-export
  `delaunayFromCoordinates` with its duplicate-payload policy from the main
  facade.
* Add one versioned C ABI over immutable geometry meshes, with typed
  obstruction witnesses and thin Python, TypeScript, and Rust bindings.

## 1.0.0.0

* Specialize the public geometry surface to binary64 and remove the ornamental
  scalar parameter from points, queries, triangulations, sessions, hierarchy
  hints, interpolation workspaces, and result types.
* Add dense coordinate and inner-face vertex projections to the existing DCEL
  surface.
* Add exact support relations and finite-set operations over triangulations,
  with local publication for sparse differences, intersections, and symmetric
  differences.
* Schedule singleton insertion and constrained extension between dense and
  locality-preserving transactions from measured workload evidence.
* Resolve dense coordinate-removal batches through one mutable identity index,
  retaining geometric descent for sparse removals.
* Strengthen constrained extension, refinement, serialization, algebra, and
  hostile-boundary validation.

## 0.1.0.0

* Initial release.
