#!/bin/zsh
set -euo pipefail
cd "$(dirname "$0")"

# driver.sh [check|refresh|list] [note]
#
#   check    confirm every committed record against a freshly produced one,
#            then time both sides. An absent record is a refusal, not an
#            invitation: a harness that mints its own baseline writes down
#            whatever defect happened to be live the first time it ran.
#   refresh  adopt this run's artifacts as the records future runs are judged
#            against. The only mode that writes anything tracked.
#   list     the lane inventory and the record inventory. Runs no benchmark and
#            writes nothing; the Haskell lane registry must already be built.
MODE="${1:-check}"
NOTE="${2:-}"
case "$MODE" in
  check|refresh|list) ;;
  *) print -u2 "usage: driver.sh [check|refresh|list] [note]"; exit 2 ;;
esac

# The Haskell ADT is the board's sole lane owner. Shell receives only boundary
# projections: argument vectors for execution and the complete CSV inventory
# for the report. Adding a row here would recreate the split this driver exists
# to forbid.
HS_BIN="${SPADE_HS_BIN:-$(cabal list-bin moonlight-planar:exe:moonlight-planar-spade-referent)}"
lanes=("${(@f)$("$HS_BIN" lane-specs parity)}")
cliff_lanes=("${(@f)$("$HS_BIN" lane-specs cliff)}")
snapshot_specs=("${(@f)$("$HS_BIN" snapshot-specs)}")

if [[ ${#snapshot_specs[@]} -ne 2 ]]; then
  print -u2 "snapshot lane obstruction: expected 2 typed relationships, got ${#snapshot_specs[@]}"
  exit 1
fi
snapshot_insert=(${=snapshot_specs[1]})
snapshot_removal=(${=snapshot_specs[2]})
if [[ ${#snapshot_insert[@]} -ne 6 || ${#snapshot_removal[@]} -ne 6 ]]; then
  print -u2 "snapshot lane obstruction: each typed relationship must contain two requests"
  exit 1
fi

# The records. Each is a committed artifact that a run either confirms or, under
# refresh, replaces. Naming them in one place is what lets `list` state the
# inventory without reproducing it.
records=(
  "agreement-gate (hs):gates-hs"
  "agreement-gate (rs):gates-rs"
  "constraint-region divergence (hs):divergence-hs.txt"
  "constraint-region divergence (rs):divergence-rs.txt"
  "BuildStats (hs):stats-hs"
)

if [[ "$MODE" == list ]]; then
  "$HS_BIN" inventory-human
  print "records (${#records[@]})"
  for entry in "${records[@]}"; do
    path="${entry#*:}"
    [[ -e "$path" ]] && state=present || state="ABSENT"
    printf '  %-9s %-22s %s\n' "$state" "$path" "${entry%%:*}"
  done
  exit 0
fi

RS_BIN="rust/target/release/spade-referent-rs"
ROUNDS=7

tmp_root="$(mktemp -d "${TMPDIR:-/tmp}/spade-gates.XXXXXX")"
trap 'rm -rf "$tmp_root"' EXIT

# One shape, five records. A record is confirmed or adopted; there is no third
# branch in which a run quietly supplies the standard it is being judged by.
record() {
  local what="$1" committed="$2" fresh="$3"
  if [[ "$MODE" == refresh ]]; then
    rm -rf "$committed"
    cp -R "$fresh" "$committed"
    print -u2 "pinned    $committed"
    return 0
  fi
  if [[ ! -e "$committed" ]]; then
    print -u2 "spade $what record is absent at $committed."
    print -u2 "Adopt one with: ./scorecard.sh refresh <provenance-note>"
    exit 1
  fi
  # Directories are compared by name only: a gate artifact body is large enough
  # that printing it buries the one line naming the file that moved. Both arms
  # report on stderr because stdout here is the CSV, and a diff written there
  # would be swallowed by the report file instead of reaching its reader.
  if [[ -d "$fresh" ]]; then
    diff -rq "$committed" "$fresh" >&2 || { print -u2 "spade $what record moved"; exit 1; }
  else
    diff "$committed" "$fresh" >&2 || { print -u2 "spade $what record moved"; exit 1; }
  fi
}

# The hard gate. It pins canonical Delaunay edges, nearest-neighbour answers,
# the CDT program's accepted request indices and final constrained edge set,
# the removal lanes' surviving edge sets, and the interpolation lanes' natural
# neighbour sets. A disagreement means the corresponding timing is meaningless.
#
# Refinement is excluded from the cross-implementation half. Ruppert's
# algorithm admits many valid outputs for one input, so Steiner count,
# placement and insertion order are ours to move; the two sides are not
# expected to agree and never have. The complete gate corpora are pinned per
# side below, so a refinement change that alters either mesh still has to be a
# deliberate one.
#
# Interpolation weights are also excluded, for a different and precise reason.
# Sibson weights are geometrically determined and both sides compute each
# neighbour's stolen area with the same operation sequence, so the neighbour
# SETS are exact-predicate determined and cross implementations (they are
# diffed above like everything else). But the normalization total accumulates
# those areas in each side's own neighbour order — spade's internal
# edge-buffer order against moonlight's BFS-seeded boundary order, with
# different cyclic start points — and floating-point addition is not
# associative, so the normalized weights cannot be bit-identical across
# implementations. The complete gate corpora are pinned per side below, so an
# arithmetic change on either side still has to be a deliberate one.
"$HS_BIN" gate "$tmp_root/hs"
"$RS_BIN" gate "$tmp_root/rs"
if ! diff -rq -x 'refine-*' -x 'interpolation-*-weights.txt' "$tmp_root/hs" "$tmp_root/rs" >&2; then
  print -u2 "spade agreement gate failed: Haskell and Rust canonical artifacts differ"
  exit 1
fi

# The full per-side corpora also pin the refinement and floating-point weight
# sections excluded from the cross-language comparison above. Extracting those
# files into duplicate record directories would prove the same bytes twice.
for side in hs rs; do
  record "agreement-gate ($side)" "gates-$side" "$tmp_root/$side"
done

# Both implementations now run budget-zero refinement and agree on closed
# regions, including nested holes. They still part company on dangling
# constraints because spade's documented winding-number domain requires closed
# shapes while moonlight treats a free segment as no region boundary. Keep this
# as a characterization rather than a hard agreement gate: the committed
# baselines pin both answers, and the driver fails if EITHER side moves.
"$HS_BIN" divergence "$tmp_root/divergence-hs.txt"
"$RS_BIN" divergence "$tmp_root/divergence-rs.txt"
for side in hs rs; do
  record "constraint-region divergence ($side)" \
    "divergence-$side.txt" "$tmp_root/divergence-$side.txt"
done

# BuildStats pin. Nothing else in this driver reads the counters: the canonical
# edge sets pin topology and the refinement summaries pin mesh size, so a change
# that rehomes or drops a counter charge leaves every existing gate green while
# silently corrupting the diagnostics the campaign reasons from. This pins them
# per side — spade exposes no equivalent, so there is no cross-implementation
# half — and fails if a count moves without someone deciding it should.
for count in 1000 10000; do
  "$HS_BIN" sweep-stats "$count" > "$tmp_root/sweep-$count.txt"
done
mkdir -p "$tmp_root/stats-hs"
cp "$tmp_root"/sweep-*.txt "$tmp_root/stats-hs/"
record "BuildStats (hs)" stats-hs "$tmp_root/stats-hs"

# Provenance fence. A board is a claim about a specific tree, and a reader who
# cannot name that tree cannot attribute a number to a source line. The source
# digest and executable build IDs therefore travel with every timing board.
repo_root="$(git -C "$PWD" rev-parse --show-toplevel)"
commit="$(git -C "$repo_root" rev-parse HEAD)"
# Hash only the local sections that determine this board: the Moonlight
# library components in the Haskell closure, both runner sources, the owning
# package description, and the execution/projection programs. Documentation, tests,
# other benchmarks, gates, baselines and rendered prose are neither runtime
# source nor hidden inputs and therefore do not belong in the digest.
source_files=("${(@f)$(comm -23 \
  <(git -C "$repo_root" ls-files --cached --others --exclude-standard -- \
    compiler/foundation/moonlight-planar/moonlight-planar.cabal \
    compiler/foundation/moonlight-planar/src-core \
    compiler/foundation/moonlight-planar/src-dcel \
    compiler/foundation/moonlight-planar/src-build \
    compiler/foundation/moonlight-planar/src-build-public \
    compiler/foundation/moonlight-planar/src-dual \
    compiler/foundation/moonlight-planar/src-hex \
    compiler/foundation/moonlight-planar/src-overlay \
    compiler/foundation/moonlight-planar/src-public \
    compiler/foundation/moonlight-planar/bench/spade-compare/cabal.project \
    compiler/foundation/moonlight-planar/bench/spade-compare/driver.sh \
    compiler/foundation/moonlight-planar/bench/spade-compare/scorecard.py \
    compiler/foundation/moonlight-planar/bench/spade-compare/scorecard.sh \
    compiler/foundation/moonlight-planar/bench/spade-compare/hs \
    compiler/foundation/moonlight-planar/bench/spade-compare/rust/Cargo.lock \
    compiler/foundation/moonlight-planar/bench/spade-compare/rust/Cargo.toml \
    compiler/foundation/moonlight-planar/bench/spade-compare/rust/rust-toolchain.toml \
    compiler/foundation/moonlight-planar/bench/spade-compare/rust/src | sort) \
  <(git -C "$repo_root" diff --name-only --diff-filter=D --no-renames | sort))}")
source_digest="$({
  printf '%s\n' "${source_files[@]}"
  printf '%s\n' "${source_files[@]}" | (cd "$repo_root" && git hash-object --stdin-paths)
} | shasum -a 256 | cut -d' ' -f1)"
print "provenance,commit,$commit"
print "provenance,source-digest,$source_digest"
print "provenance,hs-build-id,$(shasum -a 256 "$HS_BIN" | cut -d' ' -f1)"
print "provenance,rs-build-id,$(shasum -a 256 "$RS_BIN" | cut -d' ' -f1)"
print "provenance,mode,$MODE"
# The note annotates; it never substitutes. Everything above is machine-derived
# and a refresh cannot talk its way past a tree it did not measure.
if [[ -n "$NOTE" ]]; then print "provenance,note,$NOTE"; fi

# The same typed registry that supplied the execution vectors supplies class,
# order, display names and snapshot/session relationships to the report.
"$HS_BIN" inventory-csv

# Interleaved, so thermal and power drift lands on both sides of every
# comparison rather than on whichever one ran second.
echo "side,round,label,ns"
for round in $(seq 1 $ROUNDS); do
  for spec in "${lanes[@]}" "${cliff_lanes[@]}"; do
    echo "rs,$round,$($RS_BIN bench-one ${=spec})"
    echo "hs,$round,$($HS_BIN bench-one ${=spec})"
  done

  # Snapshot publication is a Haskell-only stress comparison. Keep each
  # session/snapshot pair in explicit ABBA order so process startup and thermal
  # drift land symmetrically on both sides. The ordinary rs/hs parity rows above
  # remain the cross-language comparison; these rows deliberately have no rs
  # counterpart.
  echo "hs-session,$round,$("$HS_BIN" bench-one ${snapshot_insert[1,3]})"
  echo "hs-snapshot,$round,$("$HS_BIN" bench-one ${snapshot_insert[4,6]})"
  echo "hs-snapshot,$round,$("$HS_BIN" bench-one ${snapshot_insert[4,6]})"
  echo "hs-session,$round,$("$HS_BIN" bench-one ${snapshot_insert[1,3]})"
  echo "hs-session,$round,$("$HS_BIN" bench-one ${snapshot_removal[1,3]})"
  echo "hs-snapshot,$round,$("$HS_BIN" bench-one ${snapshot_removal[4,6]})"
  echo "hs-snapshot,$round,$("$HS_BIN" bench-one ${snapshot_removal[4,6]})"
  echo "hs-session,$round,$("$HS_BIN" bench-one ${snapshot_removal[1,3]})"
done
