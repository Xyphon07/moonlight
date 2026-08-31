# Local development

[Return to the package overview.](../README.md#consumer-libraries)
· [Browse the documentation.](./README.md)

Run component-qualified commands from the repository's `compiler/` directory.
The optimized construction kernel uses the triangulation development project:

```console
../scripts/safe-cabal.sh build moonlight-planar:lib:build \
  --project-file=cabal.project.planar-dev \
  --builddir=dist-newstyle-planar-dev
```

For type and interface feedback, use the same component at `-O0`:

```console
../scripts/safe-cabal.sh build moonlight-planar:lib:build \
  --project-file=cabal.project.planar-dev \
  --builddir=dist-newstyle-triangulation-fast \
  --disable-optimization
```

Performance evidence uses the optimized profile; the `-O0` profile is the
shorter feedback path.
