# moonlight-pale

> Part of **Moonlight**, the sheaf-theoretic computation layer beneath
> [Melusine](https://bluerose.blue) and Pale Meridian.

`moonlight-pale` centralizes diagnostics, law testing, and GHC/HIE tooling without
forcing `ghc` on Moonlight's foundation packages.

It ships as a family of single-role public sublibraries; depend on the smallest
slice you use. Modules live under `Moonlight.Pale.*`. This README owns package-level
narrative; Haddock adds only terse module summaries, never repetitive per-export prose.

## Surface & boundaries

| Cabal dependency | Front-door import | What you get |
| --- | --- | --- |
| `moonlight-pale:diagnostic` | `Moonlight.Pale.Diagnostic.Core` | Severities and the accumulating `Diagnosed` writer; topology, local-run, aggregation, summary, and derived-view vocabulary; replay statistics in validated refinement types. Pure `base` + `containers`. |
| `moonlight-pale:test` | `Moonlight.Pale.Test.Core` | Validated tolerances, shared budgets, typed assertions, resource discovery, and recursion-coherence predicates. |
| `moonlight-pale:test-surface` | `Moonlight.Pale.Test.ImportDiscipline` | Import-discipline checks over the public layering. |
| `moonlight-pale:test-laws` | `Moonlight.Pale.Test.Laws.Suite` | Algebraic law predicates (`Semigroup`, `Monoid`, lattice, restriction) and the `LawSuite` DSL that names each law it checks. |
| `moonlight-pale:ghc-surface` | `Moonlight.Pale.Ghc.Expr`, `.Hie.Read`, `.ModuleSurface` | A scoped, normalized expression algebra with structural equivalence and faithful rendering; `.hie` reading, source-key indexing, a type-word oracle; and a module-surface summary. The only sublibrary that speaks `ghc`. |
| `moonlight-pale:diagnostic-ghc` | `Moonlight.Pale.TestSupport.CompileDiagnostics` | Compile-diagnostic snapshot capture: drives the compiler as a subprocess and serializes diagnostics for tests. |
| `moonlight-pale:measurement` | `Moonlight.Pale.Bench.Measure` | Checked, process-scoped RTS-cost sampling for benchmark executables that need an explicit allocation receipt. Microbenchmarks use `tasty-bench` directly. |

`moonlight-pale` depends up onto `moonlight-core` only; it never depends on a
higher foundation package, so it introduces no cycle. The `diagnostic` sublibrary
pays for nothing but `base` + `containers`; only `ghc-surface` pulls `ghc`.

## Compiler support

`moonlight-pale:test` and `moonlight-pale:test-laws`, including their own test
suites, support GHC 9.10.3, 9.12.4, and 9.14.1 (`base >= 4.20`).
The remaining explicitly GHC 9.14-only components retain their existing floors
rather than making ordinary test support unavailable to the advertised consumer
matrix.

## Test

```bash
cabal test moonlight-pale
```

## License

MIT. See [`LICENSE`](./LICENSE).
