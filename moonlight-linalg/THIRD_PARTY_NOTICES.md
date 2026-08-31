# Third-party notices

No third-party source code is vendored or adapted into `moonlight-linalg`.

The native LAPACK backend links the platform BLAS/LAPACK implementation:

- macOS: Apple Accelerate framework.
- non-macOS Cabal builds: system `lapack` and `blas` libraries.

Those libraries' license and copyright information is governed by the platform or
library distribution that supplies them. `moonlight-linalg` does not vendor LAPACK,
ARPACK, PRIMME, SciPy, or Netlib source code.

The package also depends on Haskell libraries through Cabal. Their license and
copyright information is governed by those packages' own distributions.
