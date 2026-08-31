# moonlight-algebra

> Part of **Moonlight**, the sheaf-theoretic computation layer beneath
> [Melusine](https://bluerose.blue) and Pale Meridian.

The law-governed algebraic tower the rest of the cathedral stands on.

`moonlight-algebra` is Moonlight's algebraic tier, building on
[`moonlight-core`](../moonlight-core). The front door is the umbrella module
`Moonlight.Algebra`, whose header carries the tower's shape and its relationship
to core; the public `finite-lattice` sublibrary has its own front door,
`Moonlight.FiniteLattice`, whose header carries the compiled-lattice cookbook.

## Public modules

| Module | Surface |
| --- | --- |
| `Moonlight.Algebra` | Umbrella re-export of the whole tower, plus the operation-bearing numeric classes from `moonlight-core`. |
| `Moonlight.Algebra.Pure.Group` | Standard `Semigroup`/`Monoid`, `Group`/`AbelianGroup`, and `Additive`/`Multiplicative` wrappers. |
| `Moonlight.Algebra.Pure.FreeMonoid`, `.FreeAbelianGroup` | Free monoids and free abelian groups. |
| `Moonlight.Algebra.Pure.Action` | Monoid actions (`Action`) and their group-acting refinement (`InvertibleAction`). |
| `Moonlight.Algebra.Pure.Orientation` | The two-element sign/orientation group ℤ/2 with a direct standard monoid/group instance. |
| `Moonlight.Algebra.Pure.Lattice` | Join/meet semilattices up to Heyting and Boolean algebras. |
| `Moonlight.FiniteLattice.*` | Public `finite-lattice` sublibrary: checked finite-order compilation, dense query plans, resident keys, Heyting implication, fixpoints, covers, presentation builders, and support kernels. |
| `Moonlight.Algebra.Pure.Ring` | Re-exported semiring/commutative-ring law classes plus integral/GCD/Euclidean/canonical-domain refinements. |
| `Moonlight.Algebra.Pure.Zn`, `.Quotient` | Modular arithmetic and quotient rings `R/(n)`. |
| `Moonlight.Algebra.Pure.NumberTheory`, `.GCD` | Number theory and gcd. |
| `Moonlight.Algebra.Pure.Polynomial` | Univariate polynomials over a coefficient ring; a free module on monomial degrees. |
| `Moonlight.Algebra.Pure.Module` | Modules, free modules, vector spaces, and bilinear spaces over a field. |
| `Moonlight.Algebra.Pure.Magnitude` | Real-valued magnitudes for obstructions. |
| `Moonlight.Algebra.Pure.PowerSet`, `.Product` | Power-set lattices and finite *n*-fold product algebras (coordinatewise structure). |
| `Moonlight.Algebra.Pure.SparseVec` | Sparse vectors with finite support. |

All `Moonlight.Algebra.Pure.*` leaves are exposed by the public `abstract`
sublibrary and gathered by the `Moonlight.Algebra` surface. The unsafe GCD witness
(`Moonlight.Algebra.Unsafe.GCDWitness`) lives in the private
`moonlight-algebra-internal` sublibrary, the compiled finite lattice realization
lives in the public `finite-lattice` sublibrary, and the law suite
(`Moonlight.Algebra.Effect.Laws`, `Moonlight.Algebra.Test.Generators`) lives in
the private `moonlight-algebra-laws` sublibrary.

## Performance

Compilation and the resident-key and `<=` query paths are fast; join/meet
throughput and the compiled `ContextLattice` query surface are under active
performance work.

## License

MIT; see [`LICENSE`](./LICENSE).
