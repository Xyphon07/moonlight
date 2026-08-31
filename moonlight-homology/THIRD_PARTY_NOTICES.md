# Third-party notices

`moonlight-homology` does not vendor or adapt third-party source code.

The package depends on other Haskell packages through Cabal, including
`algebraic-graphs`, `containers`, and the local Moonlight foundation packages. Those
dependencies remain under their own licenses as resolved by the build plan; no source
from them is copied into this package.

## Acknowledgements (inspiration, no derived code)

The package's finite chain-complex and topological-carrier surfaces are shaped by the
standard algebraic topology literature and by the practical need to make homological
invariants executable inside Pale Meridian. The implementation is local to this
repository.
