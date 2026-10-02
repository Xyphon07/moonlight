# Vesper, the moonblade

A generative vector sword built through Moonlight Planar's public `build` and
`dcel` components. Its straight broad blade and concave bronze star guard are
constrained Delaunay domains. `boundedRegionFaces` selects their interiors.
Every material facet comes from the native mesh, with intentionally quiet
contrast instead of the earlier prominent triangular mosaic.

The current design follows the supplied bronze-and-turquoise sword reference:
symmetric guard, rigid silhouette, leather-wrapped grip, an illuminated pommel,
and a small constellation held inside a luminous moon-glass blade.

The supplied earlier crescent-guard illustration is also preserved as an animated
variant: rigid hover, traveling blade reflection, and gently pulsing runes and
moonlight. Its pierced fuller remains a real hole, including in the reflection
clip. Both designs use the same native constrained-domain builder. Animation
builds its meshes once and renders 80 immutable frames at 80 ms per frame.

The point curves, materials, lighting, grip, engraving, and typography are art
direction. The result is an illustration, not a manufacturing specification.
Construction and rendering are pure; `main` only reads the path and writes SVG.
The small SVG encoders are shared with the Noctilucent Garden exhibit.

From the repository root:

```sh
scripts/safe-cabal.sh build moonlight-planar:lib:dcel \
  --project-file=cabal.project.planar-dev --builddir=dist-newstyle-planar-dev --offline -j6
scripts/safe-cabal.sh run moonlight-planar:exe:moonlight-planar-moonblade \
  --project-file=cabal.project.planar-dev --builddir=dist-newstyle-planar-dev --offline -j6 -- \
  "$PWD/compiler/foundation/moonlight-planar/docs/moonblade/moonblade.svg"
```

Edit the source and regenerate; do not hand-edit the derived SVG.

To render the crescent animation's 1200-by-900 SVG frames, replace the final
output argument above with `crescent-frames /absolute/path/to/vesper-frames`.
Rasterize those SVGs and encode them at 12.5 fps for a seamless 6.4-second loop.

## Riggable 3D crescent

`crescent-solid OUTPUT.obj` exports the same native blade and guard as solid
meshes in metres, with a raised blade center, thin edge, and open fuller. The
OBJ is a transport view; Blender welds coincident face vertices on import.
No raster image or painted mask supplies the geometry.

Build the rig with Blender 5.1:

```sh
blender --background --factory-startup --python-exit-code 1 \
  --python compiler/foundation/moonlight-planar/docs/moonblade/rig.py -- \
  /absolute/output/vesper.obj /absolute/output
```

This writes `vesper-rig.blend`, `vesper-rig.glb`, `vesper-rig.png`, control
instructions and `deformation-checks.json`. In Pose Mode select `ROOT_Sword`:
its custom properties give side bend ±25°, flat bend ±15°, and twist ±25°.
Zero all three for a rigid weapon. The grip, fittings and blade base do not
deform. Six connected blade bones use normalized smoothstep weights, at most
two influences per vertex; no automatic weighting or simulation is involved.

The blend preserves these bounded controls. GLB carries skinning and sampled
animation, not Blender's custom-property drivers. The provided UVs are surface
projections, not a unique baking atlas. These are artistic deformation controls,
not a physical model of steel or collision certification.

The checks exercise ten poses, including both signs of each axis, combined
extremes and an out-of-range control value. They measure face-area ratios,
finite coordinates and rigid-fitting drift, plus rest mesh manifoldness and
weight normalization. The sample animation occupies frames 1–120 at 24 fps.
To render a 12-fps preview from the saved scene:

```sh
blender --background /absolute/output/vesper-rig.blend \
  --render-output /absolute/output/preview/frame- --render-format PNG \
  --frame-start 1 --frame-end 120 --frame-jump 2 --render-anim
```
