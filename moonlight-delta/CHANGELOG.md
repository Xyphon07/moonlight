# Changelog

## 0.1.0.3 - 2026-08-21

- Breaking: `ResultBudgetExhausted` now retains the ordered irreducible subset of its
  terminal obstructions, so a repairable sibling cannot erase irreducibility
  from the untraced bounded-repair result.
- Align the aggregate coherence suite and optimization policy with the focused
  Delta test owners; the property-heavy epoch suite deliberately retains `-O2`.

## 0.1.0.2 - 2026-07-22

- Documentation: each public front door — `Moonlight.Delta.Patch`,
  `Moonlight.Delta.Signed`, `.Scope`, `.Frontier`, `.Epoch`, and
  `Moonlight.Delta.Repair` — now carries a module-header overview and a worked
  recipe. The README is reduced to a dependency and front-door map.

## 0.1.0.1 - 2026-07-21

- DeltaHash protocol v3 (`deltaHashEncodingVersion`) replaces v2. Merkle leaves
  now commit complete length-framed key and value encodings on both 64-bit lanes,
  forming a 128-bit commitment; empty, leaf, branch, flat, and multiset framing
  share the protocol version. The Merkle trie uses two folds, retiring removed
  paths before admitting additions, so admitted updates agree with the final
  authoritative state. This is a deliberate wire-format break: published
  0.1.0.0 digest bytes are not reproduced.
- DeltaHash digests are indexed by strategy and encoder. In particular,
  `merkleDeltaHashDigest` returns `MerkleDeltaHashDigest` and
  `multisetDeltaHashDigest` returns `MultisetDeltaHashDigest`; comparing the
  two is now a type error. Builders require an explicit
  `DeltaHashEncoderVersion`, and complete digests retain strategy, protocol,
  encoder, and lanes.
- Digest, identity, and lane constructors are opaque, with nominal strategy
  indices and no public raw-lane projection. `encodeDeltaHashDigest` is the
  canonical CBOR serialization of the complete identity. Concrete decoders
  reject malformed, trailing, and identity-mismatched bytes (including
  cross-strategy and cross-encoder inputs) as typed
  `DeltaHashDigestDecodeError` values.
- `composeMerkleDeltaHash` and `composeMultisetDeltaHash` are contextual:
  composing against a state succeeds exactly when sequential application does,
  preserving the resulting authoritative state and indexed digest. Success
  values, definedness, and underlying failures compose associatively; the
  `Older`/`Newer` failure-stage labels describe only the immediate binary
  operation. Contextual composition costs O(|state|); callers composing many
  small patches should fold them first and apply once. Generic `Patch.compose`
  remains endpoint composition and does not reconstruct encoder-sensitive
  intermediate representability.
- A checked-in 64-vector SipHash-2-4 oracle fixes the protocol reference values;
  incremental checked-apply and contextual-composition receipts are documented
  in `docs/BENCHMARKS-m4-pro.md`.
- Lifted the `cborg` bound to `cborg >= 0.2`: the patch implementation imports
  only public `Codec.CBOR.Decoding`, `.Encoding`, `.Read`, and `.Write` modules,
  so no `.Internal` import justifies an upper cap. The `containers` cap remains
  deliberate because this sublibrary imports `Data.Map.Internal`.
- Public named sublibraries drop the redundant package prefix:
  `moonlight-delta-core` becomes `core`, `moonlight-delta-patch` becomes
  `patch`, `moonlight-delta-epoch` becomes `epoch`, and
  `moonlight-delta-repair` becomes `repair`. Depend on them as
  `moonlight-delta:core`, `moonlight-delta:patch`, `moonlight-delta:epoch`,
  and `moonlight-delta:repair`. Every exposed module is unchanged.

## 0.1.0.0 - 2026-07-12

- Initial release of the boundary-aware delta calculus layered over
  `moonlight-core`.
- Checked patch surface (`Moonlight.Delta.Patch`). A `CellPatch` records both
  endpoints of a change — `AssertAbsent`, `Insert`, `Delete before`, `Replace
  before after` — so applying one is a verified transition rather than an
  overwrite. `apply`, `compose`, and `replay` are all checked; `diff` and
  `invert` are total. Stale writes, missing-row races, duplicate inserts, and
  invalid deletes come back as typed `ApplyError`/`ComposeError`/`ReplayError`
  instead of silent last-write-wins.
- Signed deltas (`Moonlight.Delta.Signed`) with normalize, identity,
  associativity, and inverse laws.
- Frontier, support, and monotone modules (`Moonlight.Delta.Frontier`,
  `.Support`, `.Monotone`). Monotone composition holds under an associative
  join; `ResetDelta` is a deliberate left-absorbing rebase, documented as such.
- Epoch calculus (`Moonlight.Delta.Epoch`): unbounded `Version` values,
  versioned `Endpoint` objects, checked `EpochDelta` construction,
  `transportKeys`, `transportView`, and checked composition.
- The layer also ships repair, scope, normalization, operator, and time modules
  (`Moonlight.Delta.Repair`, `.Scope`, `.Normalize`, `.Operator`, `.Time`).
- Law families are named through `Moonlight.Core.LawName` ADTs and checked via a
  proof-manifest helper; a reference model backs the patch and epoch suites.
- Public package surfaces are `Moonlight.Delta.Patch`,
  `Moonlight.Delta.Epoch`, `Moonlight.Delta.Repair`, and the modules in the
  `moonlight-delta-core` sublibrary.
