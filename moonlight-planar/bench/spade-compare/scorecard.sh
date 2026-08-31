#!/bin/zsh
# The spade referent board.
#
#   scorecard.sh list                build the typed registry, then list lanes and records
#   scorecard.sh render              render a complete retained board; no Rust or measurement
#   scorecard.sh check               gate, measure, print the board, fail on regression
#   scorecard.sh refresh <note>      adopt this run as the standard future checks are judged by
#
# check is the default and it writes nothing tracked. refresh is the only mode
# that moves a record, and it requires a note: a baseline is a claim that some
# tree deserved to become the standard, and an unattributed claim is how a live
# regression gets written down as the standard instead.
set -euo pipefail
cd "$(dirname "$0")"

MODE="${1:-check}"
NOTE="${2:-}"

case "$MODE" in
  list|render|check|refresh) ;;
  *) print -u2 "usage: scorecard.sh [list|render|check|refresh <provenance-note>]"; exit 2 ;;
esac

if [[ "$MODE" == refresh && -z "$NOTE" ]]; then
  print -u2 "refresh needs a note saying why this tree becomes the standard, e.g."
  print -u2 "  ./scorecard.sh refresh 'freezePaged on the bulk-load path'"
  exit 2
fi

# The Haskell runner owns the closed lane vocabulary. Even the read-only list
# and render modes consume its typed projection rather than a second hand-kept
# array in shell or Python.
print -u2 ">> building Haskell lane registry"
cabal build moonlight-triangulation:exe:moonlight-triangulation-spade-referent >/dev/null
HS_BIN="$(cabal list-bin moonlight-triangulation:exe:moonlight-triangulation-spade-referent)"
export SPADE_HS_BIN="$HS_BIN"

if [[ "$MODE" == list ]]; then
  exec ./driver.sh list
fi

inventory="$(mktemp "${TMPDIR:-/tmp}/spade-inventory.XXXXXX")"
staged=""
trap 'rm -f "$inventory" "$staged"' EXIT
"$HS_BIN" inventory-csv > "$inventory"

if [[ "$MODE" == render ]]; then
  python3 scorecard.py scorecard.csv "$inventory"
  exit 0
fi

# A missing or stale timing pin makes a check incapable of succeeding. Refuse
# before building Rust, replaying 42 MB of gates, or spending seven rounds on a
# verdict whose comparison side does not exist. Refresh is the operation that
# deliberately establishes a new pin and therefore bypasses this preflight.
if [[ "$MODE" == check ]]; then
  python3 scorecard.py --preflight "$inventory"
fi

# Measurement builds both sides. The provenance fence hashes the source tree
# and the binaries independently, so a board measured against an older binary
# cannot masquerade as a claim about the live tree.
print -u2 ">> building Rust referent"
cargo build --release --manifest-path rust/Cargo.toml >/dev/null 2>&1

# Staged, not written in place. A failed semantic or inventory gate must leave
# no fragment of a purported retained performance board behind.
staged="$(mktemp "${TMPDIR:-/tmp}/spade-board.XXXXXX")"
./driver.sh "$MODE" "$NOTE" > "$staged"

if [[ "$MODE" == refresh ]]; then
  python3 scorecard.py "$staged" "$inventory" --pin "$NOTE"
  # Only a refresh that survived every gate becomes the retained board.
  cp "$staged" scorecard.csv
else
  python3 scorecard.py "$staged" "$inventory" --check
fi
