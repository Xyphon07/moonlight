# moonlight-core

> Part of **Moonlight**, the sheaf-theoretic computation layer beneath
> [Melusine](https://bluerose.blue) and Pale Meridian.

`moonlight-core` is Moonlight's foundation tier. The front door is the total
umbrella module `Moonlight.Core`; its header carries the contract, the API map,
and the worked recipes. This page is the dependency map and the build commands.

## Surface & boundaries

Most downstream code imports the umbrella. Lower-level packages may depend on one
of the public sublibraries.

| Cabal dependency | Import | What you get |
| --- | --- | --- |
| `moonlight-core` | `Moonlight.Core` | The ordinary total vocabulary. This is the default. |
| `moonlight-core:syntax` | `Moonlight.Core.Pattern.AntiUnify` | Patterns and term anti-unification without the full umbrella. |
| `moonlight-core:automata` | `Moonlight.Core.Pattern.Automata` or `.Kernel` | The bottom-up automata substrate and compiled matcher. Depend on syntax too when naming its types directly. |
| `moonlight-core:egraph-program` | `Moonlight.Core.EGraph.Program` | The host-neutral e-graph program algebra without the rest of the foundation. |
| `moonlight-core` | `Moonlight.Core.Unsound` | The explicit trust boundary. |

The private implementation slices remain **basis**, **numeric**, **solver**, and
**term**. Syntax, automata, and e-graph programs are public because other foundation
packages consume those exact owners. The umbrella re-exports the
syntax and e-graph-program vocabulary; automata are imported only through the sublibrary.

## Test

```bash
cabal test moonlight-core:tests
```

## Benchmarks

```bash
cabal bench moonlight-core:moonlight-core-bench --benchmark-options='--timeout=2s --csv=comparison.csv'
```

`hackage:` rows measure a package-level baseline (`containers`, `memory`, `scientific`,
`equivalence`) on the same fixture; `world:` rows are source-equivalent baselines where
no package owner exists. Fixtures assert result agreement with the baseline before
timing. Dated receipts live in `docs/BENCHMARKS-m4-pro.md`.

## License

MIT. See `LICENSE`.
