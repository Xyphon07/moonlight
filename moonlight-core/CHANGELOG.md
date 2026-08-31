# Changelog

## 0.1.0.4 - 2026-08-21

- Documentation: correct the README selectors for the public `syntax`,
  `automata`, and `egraph-program` sublibraries and the `tests` component;
  align the facade metadata assertion with those canonical names.
- Publish a complete user-uploaded Haddock archive for the current release.

## 0.1.0.3 - 2026-08-20

- Compatibility: the `Moonlight.Core` main-library closure now supports GHC
  9.10.3, 9.12.4, and 9.14.1 (`base >= 4.20`). The independent `automata`
  component retains its existing GHC 9.14 floor.

- Performance: `Moonlight.Core.MapAccum.accumByKey` now folds rightwards, so a
  list-shaped value costs time linear in each key's group rather than
  quadratic. The documented input-order law is unchanged. Its hot consumer is
  saturation's raw-match grouping by rule key, where one rule matching `k`
  roots previously paid `k^2`; measured downstream gain is 2.14x to 2.62x on
  the `moonlight-saturation` engine benchmark lane at its largest scale.

## 0.1.0.2 - 2026-07-22

- Documentation: the `Moonlight.Core` umbrella header now carries the worked
  recipes — persistent union-find, bounded fixpoints, checked total maps, and
  the matching seam. The README is reduced to a dependency and module map.

## 0.1.0.1 - 2026-07-22

- Public named sublibraries drop the redundant package prefix:
  `moonlight-core-automata` becomes `automata`, `moonlight-core-syntax`
  becomes `syntax`, and `moonlight-core-egraph-program` becomes
  `egraph-program`. Depend on them as `moonlight-core:automata`,
  `moonlight-core:syntax`, and `moonlight-core:egraph-program`. The
  aggregate `Moonlight.Core` surface and every exposed module are
  unchanged; only the component names differ.

## 0.1.0.0 - 2026-07-12

- Initial release of the Level-0 foundation: the numeric tower (`Scalar`,
  `Numeric`) and its scalar type classes (`AdditiveGroup`,
  `MultiplicativeMonoid`, `Ring`, `Field`, `Metric`, ordered and continuous
  fields), with instances for the primitive numeric types.
- Approximate equality with absolute, relative, and ULP tolerance kinds
  (`ApproxEq`).
- Canonical and exact numeric representations (`CanonicalNumber`, `Canon`,
  `ExactToken`), including one self-delimiting structural sequence grammar.
- Order and reachability: partial-order classes (`Order`), finite universes
  (`Finite`), bounded fixpoint combinators (`Fixpoint`), and persistent
  union-find (`UnionFind`) with checked persistent and transactional class-id
  allocation.
- Identity and lookup: stable structural hashing (`StableHash`, `Hash`), checked
  total registries (`TotalRegistry`), and the relational term database
  (`Term.Database`).
- Syntax layer: Moonlight-owned authored patterns (`PatternVar`, `PatternNode`),
  `Language`/`ZipMatch`, typed substitutions, structural theories whose
  canonicalization reads node children through `Foldable`, and `Fix.Order`.
- Pure proof-manifest rendering/parsing for the EGraph emitter and Rewrite-owned
  external proof boundary; package-local law tests do not impersonate that
  integration.
- `Moonlight.Core` is the aggregate public surface. Public named sublibraries
  provide syntax, pattern automata, and the host-neutral e-graph program
  algebra; `Moonlight.Core.Unsound` provides the explicit trust boundary.
