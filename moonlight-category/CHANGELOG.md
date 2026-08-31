# Changelog

## 1.1.0.2 - 2026-08-22

- Documentation packaging only: include the README flag-nerve animation and
  its static SVG counterpart without changing the library API.

## 1.1.0.1 - 2026-08-21

- Release metadata only: retain the 1.1.0.0 public API while publishing a fresh
  exact-source tag and a complete user-uploaded Haddock archive for every
  exposed module and public sublibrary.

## 1.1.0.0 - 2026-08-21

- Breaking: replace the lossy import-category projection with a
  provenance-preserving site kernel and total object lookup.
- Breaking: replace fallible nerve-chain vertex recovery with total validated
  chain vertices, and rename the normalized and identity-inclusive nerve
  constructors to make their semantic and resource distinction explicit.
- Compatibility: build the complete test and benchmark closure on GHC 9.10.3,
  9.12.4, and 9.14.1 through `moonlight-pale:test-0.1.0.2` and `base >= 4.20`.

## 1.0.0.0 - 2026-08-20

- Publish the validated GHC 9.10.3, 9.12.4, and 9.14.1 public-library surface
  as the stable 1.0 release.
- Preserve the API and dependency surface of 0.1.0.1 unchanged.

## 0.1.0.1 - 2026-08-20

- Support GHC 9.10.3, 9.12.4, and 9.14.1 across every public library.
- Preserve the optional `laws` component boundary while documenting its
  component-scoped dependency footprint.
- Pin the release to an exact public-source tag.

## 0.1.0.0

Initial release.
