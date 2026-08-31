# Changelog

All notable changes to `moonlight-pale` are documented here.

## 0.1.0.3 - 2026-08-21

- Release metadata only: retain the 0.1.0.2 public library surface while
  publishing a fresh exact-source tag and a complete user-uploaded Haddock
  archive for every exposed module.

## 0.1.0.2 - 2026-08-21

- Compatibility: make the public `test` and `test-laws` components, with their
  own test suites, portable across GHC 9.10.3, 9.12.4, and 9.14.1
  (`base >= 4.20`).
- Mark only the five test suites whose owning libraries remain GHC 9.14-only
  as unavailable below that compiler floor, so enabling Pale tests yields a
  valid portable component plan rather than a solver contradiction.

## 0.1.0.1 - 2026-08-20

- Compatibility: the public `test-laws` component now supports GHC 9.10.3,
  9.12.4, and 9.14.1 through `moonlight-core-0.1.0.3`. Other Pale components
  retain their existing GHC 9.14 floor; in particular, `ghc-surface`'s genuine
  `ghc >= 9.14` dependency is unchanged.

## 0.1.0.0 - 2026-07-22

Initial release of Moonlight's shared diagnostics, law-testing, and source-reading
package. Depends up onto `moonlight-core` only.

Seven public sublibraries; depend on the smallest slice you use.

- `diagnostic` — severity and the accumulating `Diagnosed` writer, plus boundary,
  homotopy, cohomology, local-run, aggregation, summary, and derived-view vocabulary.
  Pure `base` + `containers`.
- `test` — assertions, fixtures, runners, resource paths, and a bounded-recursion bridge.
- `test-surface` — import-discipline checks over the public layering.
- `test-laws` — algebraic law predicates and the `LawSuite` DSL.
- `ghc-surface` — a scoped, normalized expression algebra with structural equivalence
  and faithful rendering; `.hie` reading, source-key indexing, a type-word oracle; and a
  module-surface summary. The only sublibrary that speaks `ghc`.
- `diagnostic-ghc` — compile-diagnostic snapshot capture.
- `measurement` — checked RTS-cost sampling for benchmark executables.

Each sublibrary carries its own test-suite. Builds clean under `-Wall -Wcompat`.
