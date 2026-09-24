# Curve authoring: present boundary and missing algebra

Moonlight Planar has a credible curve core and, since 1.2.0.0, a measured and
certified layer over it. It does **not** yet have a mature
illustration-authoring system. The distinction matters: eliminating hand-traced
Bezier handles and measuring curves honestly is a useful foundation, not
evidence that an agent can reliably author a complete illustration.

## What exists now

The current source of truth is `Moonlight.Planar.Curve`:

- exact relative line, quadratic, cubic, and rational-quadratic steps;
- located open and closed trails with composition, reversal, and affine action;
- exact evaluation, exact subdivision at any `UnitInterval` (`splitStep`),
  restriction to a parameter span (`restrictStep`), value and derivative jets
  at any parameter (`jetStep`), and join classification;
- rational conics for circles and ellipses.

Over that core, each layer below descends to the canonical curve and returns
an observation, never a second copy of the geometry.

- `Moonlight.Planar.Curve.Measure` gives certified arc length as an immutable
  `MeasuredTrail`. Inverse queries (`pointAtLength`, `pointAtFraction`) return
  a site on its source step with a certified parameter bracket. Every
  approximation is enclosed within a stated tolerance, and an exhausted budget
  refuses with the step, the bracket and the obligation.
- `Moonlight.Planar.Curve.Frame` gives approximately unit tangent frames at a
  sampled or exact site. They are exactly orthogonal, carry their certified
  scale bound, and refuse corners and stationary tangents rather than invent a
  direction. `fractionRun` and `sampleRun` place evenly spaced runs by arc
  fraction. The garden leaves and the sword runes are placed this way.
- `Moonlight.Planar.Curve.Proximity` gives bounded distance and strict
  clearance between two curves. Several nearest candidates stay several.
  Contact is witnessed only by exact sites closer than the threshold, an exact
  shared site, or a certified transversal crossing, which carries no point.
- `Moonlight.Planar.Curve.Region.lowerSimpleRegion` lowers outer and hole
  curves to a polygon region only after certifying that the region's
  components, holes and nesting are the curves' own. That domain is four
  predicates within a finite subdivision budget: monotone pieces;
  wedge-separated joints; non-adjacent pieces hull-separated or certified not
  to cross; and lowered loops that nest as the curves do. Outside the domain it
  refuses with a typed obstruction naming the contour spans.

`Moonlight.Planar.Curve.Authoring` adds semantic constructors for cardinal
landmarks, transverse profiles, polygons, and quadratic bows. These constructors
immediately produce the canonical curve representation; they do not retain a
second spline language. `Moonlight.Planar.Illustration` then supplies ordered
paint, typed motif ports, affine placement, geometric layout, and SVG
publication.

The illustration study's acceptance tests use this layer for the two
inequality relations its painting depends on. The fuller is certified strictly
inside the blade, and the horn bases inside the mask, at the default controls
and at the edited bounds the tests name. Those are checks at tested control
values, not whole-range proofs.

## What is still absent

| Missing capability | Why illustration authors need it |
| --- | --- |
| Named differential observations | Jets and tangent frames exist at any parameter or measured distance. Curvature, its extrema and inflections are not offered as observations; an author would derive them from the jet by hand. |
| Point projection and snapping | Distance and clearance are between two curves. There is no dedicated query projecting a free point onto a curve and returning its parameter, and no exhibit snaps a port to a contour: garden sockets are stem knots by construction. |
| Derived outlines | `profileOutline` is a transverse ribbon, not a constant-distance offset. General offsets, fillets, chamfers, geometric stroke outlines, caps, and joins are missing. General Bezier offsets are not rational curves, so this work requires explicit approximation receipts rather than counterfeit exactness. See the capability boundary below. |
| Curve intersection and arrangement | Crossings are certified, never constructed: a crossing point of two rational curves is algebraic in general (the unit circle meets y = x at an irrational point). The library does not node curve-curve intersections, split self-intersections, build a curve arrangement, or perform Boolean fill directly on curves. Tangencies inside a step, rather than at a step endpoint, stay unresolved. |
| Relational authoring | There is no closed algebra for coincidence, parallelism, symmetry, equal length, tangent attachment, or shared proportional dimensions. Every equality relation the exhibits use is constructive, a pure function of shared controls; bounded clearance and containment are checks, not constraints a solver satisfies. |
| Path-directed repetition | Motifs can be placed by arc fraction and aligned to measured frames. They cannot yet be alternated by a typed pattern, spaced by an absolute pitch with a remainder policy, or deformed along a host path. |
| Visual feedback | Topology and proximity refusals name source spans and obligations, but diagnostics still draw controls and part labels, not curvature extrema, crossings, clearance failures, approximation hot spots, or constraint residuals. The system can preserve a bad composition perfectly. |
| Publication parity | SVG is the canonical publication output, but raster-preview behavior is not owned or tested across renderers. A preview conversion can therefore lie about gradients or opacity even when the SVG is correct. |

## Capability boundary (2026-09-23)

The higher-curve plan
(`docs/implementation/plans/foundation/moonlight-planar/higher-curve-algebra-20260923.md`)
ended its outline and relation stage with a recorded boundary, not an
implementation. The exhibits justify bounded inequality checks, which landed
with proximity and certified topology. They justify neither an outline engine
nor an equality solver.

- No outline type exists. Offsets are not claimed to be implemented, nor
  universally impossible; they wait for a real consumer and an approximation
  and topology contract set from the certified domain above.
- The paint cap and join vocabulary stays at the illustration boundary.
- Exact curve Booleans need an algebraic-number representation for
  intersection points. That is a separate design decision, not a small
  extension of `ExactPoint`.
- A relation solver, if one is ever earned, must distinguish inconsistent,
  underdetermined, ambiguous, and budget-exhausted outcomes, and satisfy
  declared residual bounds.

## Required direction

Measurement, frames, distance-directed placement, proximity, and certified
simple-region topology have landed. The next work should extend the canonical
algebra in this order:

1. named differential observations (curvature, extrema) and point projection,
   each with a real consumer;
2. diagnostics that draw the certified obstructions, candidates and
   approximation hot spots the observations already return;
3. curve noding and arrangement receipts over an explicit algebraic
   representation, connecting curve topology to the existing exact region
   owner beyond the simple-region domain;
4. offset and stroke-outline construction with honest error and topology
   witnesses;
5. a pure relational authoring language whose solver either returns a checked
   construction or a typed refusal.

Each layer must descend to `Moonlight.Planar.Curve` or an existing exact planar
owner. None should introduce a mutable builder, stringly path registry, second
evaluator, cached shadow geometry, or SVG-as-source editing model.

## Standard for claiming readiness

Curve authoring is not illustration-ready merely because an example can be
rendered. A credible readiness claim requires all of the following:

- an agent can place and repeat motifs by geometric relations rather than copied
  coordinates;
- edits preserve declared attachment, continuity, spacing, and topology laws;
- unavoidable approximations carry bounds and refuse exhausted budgets;
- diagnostics identify geometric failure rather than merely drawing controls;
- a small corpus of complete illustrations survives semantic edits without
  hand repair or raster tracing.

The third item is now met for measurement, frames, proximity and region
topology. The others are not. Until they are, Moonlight Planar should be
described as a strong exact curve foundation with measured, certified
authoring primitives, and a long way to go before it is a dependable
full-illustration language.
