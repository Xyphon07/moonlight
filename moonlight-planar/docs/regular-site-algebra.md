# Persistent regular-site algebra

`Moonlight.Planar.PowerDiagram` owns the complete weighted-site value.
Use this surface when site identity must survive visibility changes or repeated
weight updates.

## Construct and edit

A `PowerSite label` combines a stable identity, an admitted planar position,
and an exact signed `PowerWeight`. Construction retains every site, including
hidden and coincident sites:

```haskell
regularTriangulation sites

editRegular newSite oldLabel weightPatch regular = do
  inserted <- insertRegularSite newSite regular
  removed <-
    removeRegularSite oldLabel (regularEditTriangulation inserted)
  reweightRegularSites
    weightPatch
    (regularEditTriangulation removed)
```

Each edit returns `RegularEditResult`:

- `regularEditTriangulation` is the new authoritative value.
- `regularEditChangedSites` is the exact support changed by the edit; it is
  empty for idempotent insertion, removal, or reweighting.
- `regularEditTransitions` carries every resulting disposition transition.

Read structural counts from the returned value with
`regularTriangulationReceipt`; the edit result does not duplicate them.

`RegularSiteTransition` distinguishes appearance, disappearance, and a change
between any two `RegularSiteDisposition` values. Thus removing or down-weighting
a visible site can report a previously hidden site becoming visible without a
caller-maintained hidden-site table.

## Identity and obstructions

The label is site identity. Re-inserting the same labelled position and weight
is idempotent. Reusing a label for different geometry returns
`RegularEditSiteConflict`; changing only weights belongs in
`reweightRegularSites`. A batch weight patch is applied once and returns
`RegularEditUnknownSites` before construction if any label is absent.

Distinct labels may occupy the same position. Their exact affine forms remain
separate submissions, and the result explicitly classifies them as visible,
coincident-equivalent, or coincident-dominated rather than silently deleting an
identity.

`emptyRegularTriangulation` is the insertion identity and the lawful result of
removing the final site. `regularSite`, `regularSites`, and `regularSiteCount`
read the canonical site section; faces, edges, neighbours, and visibility are
derived from it.

## Reuse the normalized topology

```haskell
(diagram, receipt) <- boundedPowerDiagramFromRegular domain regular
```

This clips regular-neighbour inequalities without reconstructing the lifted
hull. `boundedPowerDiagram domain sites` remains the one-shot entrance when the
editable regular value is not otherwise needed.

Edits descend through a persistent exact index. Coincident subordinates and
hidden sites update directly. A distinct insertion walks the slope
triangulation, discovers its lifted conflict cavity, glues the exact horizon,
and reattaches only hidden generators supported by removed faces. Interior
removal retriangulates the removed face star from its boundary and attached
hidden candidates. A single general reweight composes removal with insertion
atomically; a uniform shift of every weight translates the resident affine
section without changing topology. Multi-site patches use the direct
topology-preserving cases when exact, and otherwise normalize once through the
batch owner rather than accumulating local edits.

Every local publication checks manifold incidence, exact cavity boundary, and
strict lifted convexity. Degenerate, boundary-changing, or unsupported cases
decline to the exact batch constructor used by `regularTriangulation`; no
uncertified local result is published. On the retained 512-site fixture this
collapses insertion and removal from whole-hull reconstruction to
sub-millisecond face-star work.

[Compute signed weighted alpha persistence from the same value.](./weighted-alpha.md)
