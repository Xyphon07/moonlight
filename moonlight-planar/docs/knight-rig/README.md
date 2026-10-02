# Knight | native 2.5D character rig

Fan art of the little Knight from [Hollow Knight](https://www.hollowknight.com/),
original character by Team Cherry. No game asset or texture was extracted.

Eighteen shallow solid layers are constructed once by Moonlight Planar's native
constrained Delaunay builder. The mask and eye inlays remain rigid. Limbs use
smooth normalized two-bone weights; five cloak panels have independent root/hem
bones. Blender owns IK, skin evaluation, animation storage and export. The
geometry is intended for front and modest three-quarter views, not a full turntable.

## Regenerate

From the repository root (replace the output directory if desired):

```sh
scripts/safe-cabal.sh run moonlight-planar:exe:moonlight-planar-knight-rig \
  --project-file=cabal.project.planar-dev \
  --builddir=dist-newstyle-planar-dev \
  --offline -j6 -- "$PWD/.artifacts/knight-rig/knight.obj"
blender --background --factory-startup --python-exit-code 1 \
  --python compiler/foundation/moonlight-planar/docs/knight-rig/rig.py -- \
  "$PWD/.artifacts/knight-rig/knight.obj" \
  "$PWD/.artifacts/knight-rig"
python3 compiler/foundation/moonlight-planar/docs/knight-rig/package.py \
  "$PWD/.artifacts/knight-rig"
```

The interpreter targets Blender 5.1.1; preview packaging uses Pillow with WebP
support. Outputs include `knight-rig.blend`, `knight-rig.glb`, still/skeleton
previews, `knight-idle.webp`, `knight-walk.webp`, and measured rig/export checks.
The shared native shape owner is `docs/art-common/.../Exhibit/Geometry.hs`;
moonblade and this character import that same implementation, with no facade.

## Pose and animation

In Blender select **KNIGHT | 2.5D rig**, enter Pose Mode, and use ROOT, Hips,
Chest, Head, foot/knee controls, arm/forearm FK bones and individual Cloak/Hem
bones. Clear the active action to pose manually. Scale channels are locked to
avoid accidental limb stretching. Foot IK uses two fixed-length links.

The Action Editor contains **Knight | Idle** (3 s) and **Knight | Walk** (1.333 s),
both sampled at 24 fps. Idle is stationary. Walk advances 0.468 m along +X per
cycle; its stance trajectory cancels root travel, keeping the planted foot at
one world position. Accumulate root displacement when repeating Walk. The
preview camera follows the root so the looping display stays centered.

GLB retains both sampled clips and linear skinning; Blender-only IK controls
are not portable glTF constraints. There is no cloth simulation or motion
capture: this is a deliberately authored stylized gait with cloak follow-through.

Checks cover finite deformation, positive sampled face areas, weights summing
to one, foot-control trajectories and two-bone reach over 34 poses, plus actual
exported meshes, skin joints, clips and binary weights. They do not certify
arbitrary poses, self-collision or anatomical realism. Regenerate from the
source; do not patch generated meshes or pictures.
