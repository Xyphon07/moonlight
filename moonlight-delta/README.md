# moonlight-delta

> Part of **Moonlight**, the sheaf-theoretic computation layer beneath
> [Melusine](https://bluerose.blue) and Pale Meridian.

`moonlight-delta` is Moonlight's checked state-change algebra: keyed patches, signed
multiplicities, invalidation scopes, frontiers, epochs, monotone operators, and bounded
repair. Operations return typed incompatibilities where a transition cannot be applied.

Each public front door — `Moonlight.Delta.Patch`, `Moonlight.Delta.Signed`,
`.Scope`, `.Frontier`, `.Epoch`, and `Moonlight.Delta.Repair` — carries its own
overview and a worked recipe in its module header; there is no umbrella. The
table below maps each dependency to its front door.

## Surface & boundaries

There is no umbrella library. Depend on the smallest public slice you use.

| Cabal dependency | Import | What you get |
| --- | --- | --- |
| `moonlight-delta:moonlight-delta-core` | `Moonlight.Delta.Signed`, `.Scope`, `.Frontier`, `.Monotone`, `.Normalize`, `.Support`, `.Operator`, `.Time` | Signed changes, invalidation, progress frontiers, operators, and their shared laws. |
| `moonlight-delta:moonlight-delta-patch` | `Moonlight.Delta.Patch` | Checked keyed transitions, composition, diff, inversion, and replay. |
| `moonlight-delta:moonlight-delta-epoch` | `Moonlight.Delta.Epoch` | Versioned partial key transport and view restamping. |
| `moonlight-delta:moonlight-delta-repair` | `Moonlight.Delta.Repair` | Bounded obstruction repair and kernel composition. |

## Test

```bash
cabal test moonlight-delta:moonlight-delta-test
```

## License

MIT. See `LICENSE`.
