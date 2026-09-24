# Moonlight persistence rose

![Animated Moonlight persistence rose](./moonlight-triangulation-persistence-rose.gif)

[Browse all exhibits.](../README.md)
· [Open the animation directly.](./moonlight-triangulation-persistence-rose.gif)
· [Open the static vector poster.](./moonlight-triangulation-persistence-rose.svg)
· [Open the alpha eclipse.](./alpha-eclipse.md)
· [Read the package overview.](../../README.md#persistence-rose)

## Exact alpha filtration and persistent topology

The full alpha path is a different observation of the same resident Delaunay
DCEL. `alphaFiltration` computes exact squared-radius births once: vertices at
zero, Gabriel edges at one quarter of their squared length, other edges at their
least incident-face witness, and faces at their circumradius squared.
`filteredAlphaComplex` lowers that section to the one Homology persistence
owner. `AlphaBirth` is an opaque reduced rational; equality and ordering never
round through `Double`.

This is the planar alpha-complex shortcut to the Cech filtration: the Delaunay
subcomplex has the same homotopy type as the corresponding union of discs, but
has only the resident planar cells. One reduction therefore yields the barcode
for every critical radius. `persistentBettiAtCriticalValues` sweeps all of
those sublevels through dense complex-relative ranks without rebuilding
geometry, reducing another boundary matrix, or rescanning the barcode per
radius.

The external component path names `build` for Delaunay construction and `dcel`
for exact alpha births, then adds the opt-in cell-complex lowering and Homology.

```cabal
build-depends:
  base >= 4.22 && < 5,
  moonlight-planar:dcel >= 1.0 && < 1.1,
  moonlight-planar:build >= 1.0 && < 1.1,
  moonlight-planar:cell-complex >= 1.0 && < 1.1,
  moonlight-homology >= 0.1 && < 0.2,
  vector >= 0.13 && < 0.14
```

```haskell
import Data.Foldable (traverse_)
import qualified Data.Vector as Vector
import Moonlight.Homology.Persistence
  ( filteredCriticalValues
  , mod2PersistentPairs
  , persistentBettiAtCriticalValues
  )
import Moonlight.Planar.Alpha (alphaFiltration)
import Moonlight.Planar.BulkLoad (delaunayGeometry)
import Moonlight.Planar.Point (Point (..))
import Moonlight.Planar.CellComplex (filteredAlphaComplex)

main :: IO ()
main = do
  mesh <- requireRight (delaunayGeometry (Vector.fromList [Point (-1) (-1), Point 1 (-1), Point 1 1, Point (-1) 1]))
  filtration <- requireRight (alphaFiltration mesh)
  filtered <- requireRight (filteredAlphaComplex filtration)
  pairs <- requireRight (mod2PersistentPairs filtered)
  profiles <- requireRight (persistentBettiAtCriticalValues filtered pairs)
  traverse_ print (zip (filteredCriticalValues filtered) profiles)

requireRight :: Show failure => Either failure value -> IO value
requireRight = either (fail . show) pure
```

The focused `moonlight-planar-alpha-bench` measures exact preparation,
one persistence reduction plus all critical-radius queries, and repeated
threshold homology as separate lanes over the same prebuilt Delaunay mesh.

## Source SVG generation

On GHC 9.14, the package-owned generator reproduces the poster or the canonical
65-frame seamless SVG sweep:

```console
cabal run moonlight-planar-persistence-rose -- poster persistence-rose.svg
cabal run moonlight-planar-persistence-rose -- frames 65 persistence-rose-frames
```

## Retained GIF reproduction

The retained GIF is the 960×540, 8 fps projection of those 65 source frames.
Its exact encoding boundary is deliberately outside geometry and Homology:
rasterize the SVG section, encode it, then let the exhibit owner stamp the GIF
with the deterministic source-frame fingerprint it computed itself.

```console
mkdir -p persistence-rose-png
sips -s format png persistence-rose-frames/frame-*.svg --out persistence-rose-png
ffmpeg -framerate 8 -start_number 0 -i persistence-rose-png/frame-%04d.png \
  -vf "scale=960:-2:flags=lanczos,palettegen=max_colors=96:stats_mode=diff" \
  -y persistence-rose-palette.png
ffmpeg -framerate 8 -start_number 0 -i persistence-rose-png/frame-%04d.png \
  -i persistence-rose-palette.png \
  -lavfi "[0:v]scale=960:-2:flags=lanczos[x];[x][1:v]paletteuse=dither=bayer:bayer_scale=4:diff_mode=rectangle" \
  -loop 0 -y persistence-rose-raw.gif
cabal run moonlight-planar-persistence-rose -- \
  stamp-gif persistence-rose-raw.gif docs/persistence-rose/moonlight-triangulation-persistence-rose.gif
cabal run moonlight-planar-persistence-rose -- verify-artifacts .
```

`verify-artifacts` compares both retained SVGs byte-for-byte with their pure
renderers, checks the GIF dimensions, timing, and image-frame inventory, and
requires the GIF's embedded source fingerprint to equal the freshly rendered
canonical frame section. A plausible binary can no longer wander in wearing a
nice hat.
