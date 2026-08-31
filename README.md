# Moonlight

Public source mirror of the Moonlight foundation packages published or staged
for Hackage, generated from the private Pale Meridian monorepo with Josh.

`workspace.josh` owns the exact package cut. Package release tags in this
repository are generated views of their authoritative monorepo source.

## Fetch one package

Each Moonlight package has its own Cabal source distribution. To inspect one
package without fetching every public source blob, use Git's sparse partial
clone and select its directory:

```sh
git clone --depth 1 --filter=blob:none --sparse https://github.com/PaleRoses/moonlight.git
cd moonlight
git sparse-checkout set moonlight-core
```
