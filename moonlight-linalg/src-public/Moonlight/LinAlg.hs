{-|
Typed dense, sparse, finite-field, and Krylov linear algebra — the single public
surface of @moonlight-linalg@, layered over "Moonlight.Core" and
"Moonlight.Algebra". Dense matrices carry their shape and scalar as type indices,
so an @r@-by-@c@ matrix over @a@ is a @Matrix r c a@ and a product's shared inner
dimension is fixed at the type level. Construction is validated: malformed
matrices, incompatible dimensions, and invalid solver configuration return typed
@MoonlightError@ failures rather than partial indexing or runtime bottoms.

The re-exported surface, by role:

* Dense — "Moonlight.LinAlg.Dense": typed vectors and matrices, validated
  rectangular row authoring, GF(2) and packed bit matrices, exterior algebra,
  decompositions, field operations, direct solvers, and primitives.
* Geometry — "Moonlight.LinAlg.Geometry": @Vec2@, @Vec3@, AABB\/AABB2, frames,
  affine transforms, and compact symmetric 2D\/3D carriers.
* Sparse — "Moonlight.LinAlg.Sparse": COO\/CSR\/CSC\/packed encodings, sealed
  preconditioner families, and sparse iterative solvers for graph and mesh
  operators.
* Operators and spectra — "Moonlight.LinAlg.Operator" affine-normalized linear
  operators with explicit self-adjoint boundaries, "Moonlight.LinAlg.Spectral"
  eigenvalue\/eigenpair demand dispatched by operator structure, and
  "Moonlight.LinAlg.Krylov" Arnoldi\/Lanczos decompositions with projected
  tridiagonal and block-tridiagonal carriers.
* Domain and statics — "Moonlight.LinAlg.Domain" domain-level algebra including
  Smith normal form, and "Moonlight.LinAlg.Statics" assembly, equilibrium
  compilation, and support checking.

The effectful native LAPACK boundary lives apart in "Moonlight.LinAlg.Native"
(not re-exported here); the @Moonlight.LinAlg.Pure.*@ and
@Moonlight.LinAlg.Internal.*@ leaves live in graded implementation sublibraries
behind this vocabulary.

/Quick start./ A whole computation composes in @Either MoonlightError@:

> import Moonlight.LinAlg (Matrix, fromListMatrix, mult, toListMatrix)
> import Moonlight.Core (MoonlightError)
>
> product22 :: Either MoonlightError (Matrix 2 2 Double)
> product22 = do
>   left  <- fromListMatrix @2 @2 @Double [1.0, 2.0, 3.0, 4.0]
>   right <- fromListMatrix @2 @2 @Double [2.0, 0.0, 1.0, 2.0]
>   mult left right

@mult :: Matrix r m a -> Matrix m c a -> Either MoonlightError (Matrix r c a)@
forces the two matrices to meet on @m@; the result reads back as
@toListMatrix \<$\> product22 == Right [4.0, 4.0, 10.0, 8.0]@.
-}
module Moonlight.LinAlg
  ( module Dense,
    module DenseBlock,
    module DenseDecomposition,
    module DenseExterior,
    module DenseField,
    module DenseGF2,
    module DenseSolver,
    module Domain,
    module Geometry,
    module Krylov,
    module Operator,
    module Sparse,
    module Spectral,
    module Statics,
  )
where

import Moonlight.LinAlg.Dense as Dense
import Moonlight.LinAlg.Dense.Block as DenseBlock
import Moonlight.LinAlg.Dense.Decomposition as DenseDecomposition
import Moonlight.LinAlg.Dense.Exterior as DenseExterior
import Moonlight.LinAlg.Dense.Field as DenseField
import Moonlight.LinAlg.Dense.GF2 as DenseGF2
import Moonlight.LinAlg.Dense.Solver as DenseSolver
import Moonlight.LinAlg.Domain as Domain
import Moonlight.LinAlg.Geometry as Geometry
import Moonlight.LinAlg.Krylov as Krylov
import Moonlight.LinAlg.Operator as Operator
import Moonlight.LinAlg.Sparse as Sparse
import Moonlight.LinAlg.Spectral as Spectral
import Moonlight.LinAlg.Statics as Statics
