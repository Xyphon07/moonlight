#!/usr/bin/env python3
"""Turn driver.sh CSV into the spade-vs-moonlight table, against a pinned baseline.

    ./scorecard.sh check                 run both sides, print the table, fail on regression
    ./scorecard.sh refresh <note>        adopt this run as the new baseline
    python3 scorecard.py run.csv inventory.csv
        re-render an existing run against the typed Haskell inventory
    python3 scorecard.py run.csv inventory.csv --check
        exit non-zero on a regressed or missing lane
    python3 scorecard.py run.csv inventory.csv --pin NOTE
        adopt this run as the new baseline
    python3 scorecard.py --preflight inventory.csv
        refuse an incomplete timing pin before any measurement
"""

from __future__ import annotations

import csv
import json
import statistics
import sys
from pathlib import Path
from typing import NamedTuple

HERE = Path(__file__).parent
RETAINED_RUN = HERE / "scorecard.csv"
BASELINE = HERE / "baseline.json"
REPORT = HERE / "SCORECARD.md"


class LaneSpec(NamedTuple):
    """One displayable row projected from the typed Haskell lane registry."""

    lane: str
    display: str


class SnapshotStressLane(NamedTuple):
    """A Haskell-only publication stress lane and its session reference."""

    snapshot_lane: str
    session_lane: str
    display: str


class Inventory(NamedTuple):
    """The authoritative board vocabulary after boundary validation."""

    parity: tuple[LaneSpec, ...]
    cliff: tuple[LaneSpec, ...]
    snapshots: tuple[SnapshotStressLane, ...]


TIMING_SIDES = frozenset(("rs", "hs", "hs-session", "hs-snapshot"))


def read_inventory(path: Path) -> Inventory:
    """Validate the CSV projection emitted by the typed Haskell owner."""
    rows = tuple(
        row
        for row in csv.reader(path.read_text().splitlines())
        if row and row[0] == "lane"
    )
    malformed = tuple(
        row
        for row in rows
        if len(row) < 4 or (row[1] == "snapshot" and len(row) < 5)
    )
    unknown_classes = sorted(
        {
            row[1]
            for row in rows
            if len(row) >= 2 and row[1] not in {"parity", "cliff", "snapshot"}
        }
    )
    if malformed:
        raise SystemExit(
            f"inventory obstruction: {len(malformed)} malformed lane declaration(s)"
        )
    if unknown_classes:
        raise SystemExit(
            "inventory obstruction: unknown lane class(es): "
            + ", ".join(unknown_classes)
        )

    parity = tuple(LaneSpec(row[2], row[3]) for row in rows if row[1] == "parity")
    cliff = tuple(LaneSpec(row[2], row[3]) for row in rows if row[1] == "cliff")
    snapshots = tuple(
        SnapshotStressLane(row[2], row[4], row[3])
        for row in rows
        if row[1] == "snapshot"
    )
    identifiers = tuple(
        specification.lane for specification in parity + cliff
    ) + tuple(specification.snapshot_lane for specification in snapshots)
    duplicates = sorted(
        {identifier for identifier in identifiers if identifiers.count(identifier) > 1}
    )
    board_identifiers = frozenset(
        specification.lane for specification in parity + cliff
    )
    orphaned_sessions = sorted(
        specification.session_lane
        for specification in snapshots
        if specification.session_lane not in board_identifiers
    )
    empty_displays = tuple(
        specification.lane
        for specification in parity + cliff
        if not specification.display
    ) + tuple(
        specification.snapshot_lane
        for specification in snapshots
        if not specification.display
    )
    obstructions = tuple(
        filter(
            None,
            (
                "no parity lanes" if not parity else "",
                "no cliff lanes" if not cliff else "",
                "no snapshot relationships" if not snapshots else "",
                f"duplicate lane IDs: {', '.join(duplicates)}" if duplicates else "",
                (
                    f"snapshot sessions outside the board: {', '.join(orphaned_sessions)}"
                    if orphaned_sessions
                    else ""
                ),
                f"empty display labels: {', '.join(empty_displays)}"
                if empty_displays
                else "",
            ),
        )
    )
    if obstructions:
        raise SystemExit("inventory obstruction: " + "; ".join(obstructions))
    return Inventory(parity, cliff, snapshots)


def read_samples(path: Path) -> dict[tuple[str, str], list[float]]:
    """Map each measured side and lane to its fresh-process millisecond samples."""
    rows = tuple(
        row
        for row in csv.reader(path.read_text().splitlines())
        if len(row) >= 4 and row[0] in TIMING_SIDES
    )
    keys = frozenset((row[0], row[2]) for row in rows)
    return {
        key: [
            int(row[3]) / 1e6
            for row in rows
            if (row[0], row[2]) == key
        ]
        for key in keys
    }


def read_provenance(path: Path) -> dict[str, str]:
    """The tree the driver measured. Written by driver.sh ahead of the samples."""
    return {
        row[1]: row[2]
        for row in csv.reader(path.read_text().splitlines())
        if len(row) >= 3 and row[0] == "provenance"
    }


def read_declared_lanes(path: Path) -> dict[str, list[str]]:
    """The inventory the driver says it ran, keyed by table."""
    rows = tuple(
        row
        for row in csv.reader(path.read_text().splitlines())
        if len(row) >= 3 and row[0] == "lane"
    )
    return {
        table: [row[2] for row in rows if row[1] == table]
        for table in frozenset(row[1] for row in rows)
    }


def lane_table(specifications: tuple[LaneSpec, ...]) -> list[tuple[str, str]]:
    """Project report rows without acquiring ownership of their vocabulary."""
    return [
        (specification.lane, specification.display)
        for specification in specifications
    ]


def inventory_difference(
    table: str, expected: list[str], declared: list[str]
) -> str:
    """Describe one failed descent from the canonical registry into a run."""
    missing = [lane for lane in expected if lane not in declared]
    unknown = [lane for lane in declared if lane not in expected]
    detail = ", ".join(
        filter(
            None,
            (
                f"absent from run: {' '.join(missing)}" if missing else "",
                f"unknown to registry: {' '.join(unknown)}" if unknown else "",
                (
                    "same lanes, different order"
                    if expected != declared and not missing and not unknown
                    else ""
                ),
            ),
        )
    )
    return f"{table} run inventory disagrees with typed registry — {detail}" if detail else ""


def inventory_complaints(
    declared: dict[str, list[str]], inventory: Inventory
) -> list[str]:
    """Reject a run whose declared rows are not the registry's exact descent."""
    if not declared:
        return ["the run declares no lane inventory; regenerate it with ./scorecard.sh check"]
    expected = {
        "parity": [specification.lane for specification in inventory.parity],
        "cliff": [specification.lane for specification in inventory.cliff],
        "snapshot": [
            specification.snapshot_lane for specification in inventory.snapshots
        ],
    }
    unknown_classes = sorted(set(declared) - set(expected))
    policy_unknown = sorted(
        NON_REPEATING
        - frozenset(expected["parity"])
        - frozenset(expected["cliff"])
    )
    table_complaints = tuple(
        inventory_difference(table, lanes, declared.get(table, []))
        for table, lanes in expected.items()
    )
    return list(
        filter(
            None,
            (
                *table_complaints,
                (
                    "run declares unknown lane classes: " + ", ".join(unknown_classes)
                    if unknown_classes
                    else ""
                ),
                (
                    "non-repeating policy names lanes outside the registry: "
                    + ", ".join(policy_unknown)
                    if policy_unknown
                    else ""
                ),
            ),
        )
    )


def sample_complaints(
    samples: dict[tuple[str, str], list[float]], inventory: Inventory
) -> list[str]:
    """Every canonical row must have the observations its comparison requires."""
    board_requirements = tuple(
        (side, specification.lane)
        for specification in inventory.parity + inventory.cliff
        for side in ("rs", "hs")
    )
    snapshot_requirements = tuple(
        requirement
        for specification in inventory.snapshots
        for requirement in (
            ("hs-session", specification.session_lane),
            ("hs-snapshot", specification.snapshot_lane),
        )
    )
    required = board_requirements + snapshot_requirements
    required_keys = frozenset(required)
    missing = tuple(
        f"{side}/{lane}" for side, lane in required if not samples.get((side, lane))
    )
    unknown = tuple(
        f"{side}/{lane}"
        for side, lane in sorted(samples)
        if (side, lane) not in required_keys
    )
    return list(
        filter(
            None,
            (
                f"missing observations: {', '.join(missing)}" if missing else "",
                f"observations outside the registry: {', '.join(unknown)}"
                if unknown
                else "",
            ),
        )
    )


def keyspace_complaint(
    label: str, expected: tuple[str, ...], actual: dict[str, float]
) -> str:
    """Describe a baseline section that cannot cover the canonical inventory."""
    if not actual:
        return f"{label} section is absent"
    missing = tuple(identifier for identifier in expected if identifier not in actual)
    unknown = tuple(identifier for identifier in actual if identifier not in expected)
    detail = ", ".join(
        filter(
            None,
            (
                f"missing {' '.join(missing)}" if missing else "",
                f"unknown {' '.join(unknown)}" if unknown else "",
            ),
        )
    )
    return f"{label} keyspace disagrees with typed registry — {detail}" if detail else ""


def baseline_complaints(
    inventory: Inventory, baseline_document: dict
) -> list[str]:
    """A check may start only against a complete, attributable timing pin."""
    board_identifiers = tuple(
        specification.lane for specification in inventory.parity + inventory.cliff
    )
    snapshot_identifiers = tuple(
        specification.snapshot_lane for specification in inventory.snapshots
    )
    provenance_missing = tuple(
        field
        for field in ("commit", "source-digest", "source")
        if not baseline_document.get(field)
    )
    return list(
        filter(
            None,
            (
                "baseline document is absent" if not baseline_document else "",
                keyspace_complaint(
                    "median gaps", board_identifiers, baseline_document.get("gaps", {})
                ),
                keyspace_complaint(
                    "minimum gaps",
                    board_identifiers,
                    baseline_document.get(GATE_KEY, {}),
                ),
                keyspace_complaint(
                    "snapshot premiums",
                    snapshot_identifiers,
                    baseline_document.get("snapshot-premiums", {}),
                ),
                (
                    "baseline provenance is missing " + " ".join(provenance_missing)
                    if provenance_missing
                    else ""
                ),
            ),
        )
    )


# A lane may regress by this much against the pin before the gate calls it a
# regression. Timing on a laptop under an unpinned scheduler is not a 2% signal,
# and a gate that fires on ordinary drift is one people learn to pass with -f.
REGRESSION_TOLERANCE = 0.25

# The gate reads minimum-over-rounds, not the median the tables display.
#
# This was settled by measurement, not preference. The same three lanes were
# read off a quiet machine and off one running 13.5 GB into swap with nine of
# twenty-three lanes failing to repeat at all; every stable lane's minimum gap
# and median gap agreed to within 0.1x across both. The ratio survives
# contention because the driver interleaves rs and hs inside each round, so load
# lands on both sides of every comparison. The minimum is then the closer thing
# to an uncontended sample at no cost in agreement, and it cannot be inflated by
# a machine that was merely busy.
GATE_STATISTIC = min
GATE_KEY = "minimum-gaps"

# Lanes that do not repeat well enough to be gated on, named rather than
# inferred. UNSTABLE_SPREAD already drops a noisy lane out of the SUMMARY, but a
# threshold that decides at runtime which lanes may fail is a gate that quietly
# excuses whatever is loudest; interpolation at 100k has been seen moving its
# own gap 1.61x -> 0.93x between consecutive runs of one binary. These two are
# still measured, still tabled, and still read — they are not a pass/fail claim.
NON_REPEATING = frozenset(
    (
        "interpolation-100000-2000",
        "constraint-incremental-8000-800",
    )
)


def regressions(
    samples: dict[tuple[str, str], list[float]],
    baseline: dict[str, float],
    parity_lanes: list[tuple[str, str]],
    cliff_lanes: list[tuple[str, str]],
) -> list[str]:
    """Lanes whose minimum gap has widened past tolerance since the pin."""
    now = {
        **gaps(samples, parity_lanes, GATE_STATISTIC),
        **gaps(samples, cliff_lanes, GATE_STATISTIC),
    }
    return [
        f"{lane}: {before:.2f}× -> {after:.2f}× "
        f"({(after - before) / before * 100:+.0f}%)"
        for lane, after in sorted(now.items())
        if lane not in NON_REPEATING
        and (before := baseline.get(lane)) is not None
        and after > before * (1 + REGRESSION_TOLERANCE)
    ]


def gaps(
    samples: dict[tuple[str, str], list[float]],
    lanes: list[tuple[str, str]],
    statistic=statistics.median,
) -> dict[str, float]:
    return {
        lane: statistic(samples[("hs", lane)]) / statistic(samples[("rs", lane)])
        for lane, _ in lanes
        if samples.get(("hs", lane)) and samples.get(("rs", lane))
    }


def spread(values: list[float]) -> str:
    return f"{statistics.median(values):.3f} [{min(values):.3f}–{max(values):.3f}]"


# A lane whose own samples range wider than this cannot support a two-figure
# gap: interpolation at 100k has been seen spanning 31.9-71.2 ms on the spade
# side alone, moving its median 33.3 -> 59.4 between consecutive runs of the
# same binary and its gap 1.61x -> 0.93x. Reported unqualified, that reads as
# overtaking spade. The threshold is deliberately loose -- every stable lane
# here sits under 1.2 -- so it fires on lanes that are broken as measurements
# rather than on ordinary jitter.
UNSTABLE_SPREAD = 1.5


def instability(samples: dict[tuple[str, str], list[float]], lane: str) -> float:
    """Widest within-side sample range, as a ratio. 1.0 is a perfect repeat."""
    return max(
        max(values) / min(values)
        for side in ("rs", "hs")
        if (values := samples.get((side, lane))) and min(values) > 0
    )


def side_instability(
    samples: dict[tuple[str, str], list[float]], side: str, lane: str
) -> float | None:
    """One side's repeat ratio, or ``None`` when that side is absent."""
    values = samples.get((side, lane))
    if not values or min(values) <= 0:
        return None
    return max(values) / min(values)


def snapshot_premiums(
    samples: dict[tuple[str, str], list[float]],
    snapshot_lanes: tuple[SnapshotStressLane, ...],
) -> dict[str, float]:
    """Snapshot/session medians keyed by the snapshot lane, never a Spade gap."""
    return {
        row.snapshot_lane: statistics.median(samples[("hs-snapshot", row.snapshot_lane)])
        / statistics.median(samples[("hs-session", row.session_lane)])
        for row in snapshot_lanes
        if samples.get(("hs-session", row.session_lane))
        and samples.get(("hs-snapshot", row.snapshot_lane))
    }


def verdict(now: float, before: float | None) -> str:
    if before is None:
        return "—"
    change = (now - before) / before * 100
    if abs(change) < 2:
        return "flat"
    return f"{'better' if change < 0 else 'WORSE'} {abs(change):.0f}%"


def render(
    samples: dict[tuple[str, str], list[float]],
    baseline: dict[str, float],
    lanes: list[tuple[str, str]],
) -> str:
    header = (
        "| Lane | Spade median [range] | Moonlight median [range] |"
        " Median gap | Minimum gap | Baseline gap | Verdict |\n"
        "|---|---:|---:|---:|---:|---:|:--|"
    )

    def render_row(lane: str, display: str) -> str:
        rs, hs = samples.get(("rs", lane)), samples.get(("hs", lane))
        if not rs or not hs:
            return f"| {display} | — | — | — | — | — | missing |"
        median_gap = statistics.median(hs) / statistics.median(rs)
        minimum_gap = min(hs) / min(rs)
        before = baseline.get(lane)
        return (
            f"| {display} | {spread(rs)} | {spread(hs)} | **{median_gap:.2f}×** |"
            f" {minimum_gap:.2f}× | {before:.2f}× | {verdict(median_gap, before)} |"
            if before is not None
            else f"| {display} | {spread(rs)} | {spread(hs)} | **{median_gap:.2f}×** |"
            f" {minimum_gap:.2f}× | — | — |"
        )

    rows = [render_row(lane, display) for lane, display in lanes]
    return "\n".join([header, *rows])


def render_snapshot_stress(
    samples: dict[tuple[str, str], list[float]],
    baseline: dict[str, float],
    snapshot_lanes: tuple[SnapshotStressLane, ...],
) -> str:
    """Render Haskell session-vs-publication cost without inventing a Spade gap."""
    header = (
        "| Workload | Session reference ms | Snapshot publication ms | Premium |"
        " Repeat / instability | Baseline premium | Verdict |\n"
        "|---|---:|---:|---:|:--|---:|:--|"
    )

    def render_row(row: SnapshotStressLane) -> str:
        session = samples.get(("hs-session", row.session_lane))
        snapshot = samples.get(("hs-snapshot", row.snapshot_lane))
        if not session or not snapshot:
            return f"| {row.display} | — | — | — | missing | — | missing |"
        premium = statistics.median(snapshot) / statistics.median(session)
        session_repeat = side_instability(samples, "hs-session", row.session_lane)
        snapshot_repeat = side_instability(samples, "hs-snapshot", row.snapshot_lane)
        repeats = (
            f"session {session_repeat:.2f}× / snapshot {snapshot_repeat:.2f}×"
            if session_repeat is not None and snapshot_repeat is not None
            else "missing"
        )
        unstable = max(session_repeat or 0.0, snapshot_repeat or 0.0) > UNSTABLE_SPREAD
        before = baseline.get(row.snapshot_lane)
        baseline_text = f"{before:.2f}×" if before is not None else "—"
        verdict_text = "—" if unstable else verdict(premium, before)
        marker = " ⚠" if unstable else ""
        return (
            f"| {row.display} | {spread(session)} | {spread(snapshot)} |"
            f" **{premium:.2f}×**{marker} | {repeats} | {baseline_text} |"
            f" {verdict_text} |"
        )

    rows = [render_row(row) for row in snapshot_lanes]
    return "\n".join([header, *rows])


# Every Spade-comparable lane, worst first. Snapshot publication is deliberately
# absent: it has no rs side and is rendered by render_snapshot_stress instead.
def standings(
    samples: dict[tuple[str, str], list[float]],
    baseline: dict[str, float],
    parity_lanes: list[tuple[str, str]],
    cliff_lanes: list[tuple[str, str]],
) -> str:
    kinds = {
        **{lane: "parity" for lane, _ in parity_lanes},
        **{lane: "cliff" for lane, _ in cliff_lanes},
    }
    displays = dict(parity_lanes + cliff_lanes)

    measured = sorted(
        [
            (
                statistics.median(samples[("hs", lane)])
                / statistics.median(samples[("rs", lane)]),
                lane,
            )
            for lane in kinds
            if samples.get(("rs", lane)) and samples.get(("hs", lane))
        ],
        reverse=True,
    )

    header = (
        "| # | Lane | Kind | Spade ms | Moonlight ms | Gap | Repeat | vs pin |\n"
        "|--:|---|:--|---:|---:|---:|---:|:--|"
    )

    def render_row(rank: int, gap: float, lane: str) -> str:
        wobble = instability(samples, lane)
        unstable = wobble > UNSTABLE_SPREAD
        return (
            f"| {rank} | {displays[lane]} | {kinds[lane]} |"
            f" {statistics.median(samples[('rs', lane)]):.3f} |"
            f" {statistics.median(samples[('hs', lane)]):.3f} |"
            f" **{gap:.2f}×**{' ⚠' if unstable else ''} |"
            f" {wobble:.2f}× |"
            f" {'—' if unstable else verdict(gap, baseline.get(lane))} |"
        )

    rows = [
        render_row(rank, gap, lane)
        for rank, (gap, lane) in enumerate(measured, start=1)
    ]
    return "\n".join([header, *rows])


def standings_summary(
    samples: dict[tuple[str, str], list[float]],
    lanes: list[tuple[str, str]],
) -> str:
    measured = gaps(samples, lanes)
    if not measured:
        return "no lanes measured"
    # Every claim below is over the lanes that repeated. A lane whose own samples
    # span more than UNSTABLE_SPREAD is reported as unmeasured rather than folded
    # in, because its gap moves further between runs than any change we would
    # make to it -- and the direction it moves is as likely to flatter as not.
    stable = {
        lane: gap
        for lane, gap in measured.items()
        if instability(samples, lane) <= UNSTABLE_SPREAD
    }
    noisy = sorted(set(measured) - set(stable))
    total = len(stable)
    if not stable:
        return f"no lane repeated within {UNSTABLE_SPREAD:g}×; nothing here is measured"
    worst = max(stable.items(), key=lambda item: item[1])
    best = min(stable.items(), key=lambda item: item[1])
    bands = [
        (sum(1 for value in stable.values() if value <= bound), bound)
        for bound in (2.0, 5.0, 10.0)
    ]
    ahead = sum(1 for value in stable.values() if value < 1.0)
    return (
        f"{total} lanes measured. "
        f"Best **{best[0]}** at **{best[1]:.2f}×**; "
        f"worst **{worst[0]}** at **{worst[1]:.2f}×**. "
        f"Lanes where moonlight is faster: **{ahead}/{total}**. "
        + ", ".join(f"within {bound:g}×: **{count}/{total}**" for count, bound in bands)
        + "."
        + (
            f" **{len(noisy)} lane(s) did not repeat and are excluded**: "
            + ", ".join(f"{lane} ({instability(samples, lane):.2f}×)" for lane in noisy)
            + ". Their gaps are not evidence in either direction."
            if noisy
            else ""
        )
    )


# Printed first and placed at the head of the report so the timing and semantic
# obligations are inseparable from the numbers they qualify.
PROTOCOL_NOTICE = (
    "> **Protocol.** Both sides prepare equivalent input outside the timed action,\n"
    "> then run one timed iteration in each fresh process for seven interleaved\n"
    "> rounds. The report shows medians and observed ranges; the regression gate\n"
    "> compares per-side minima. Exact semantic gates run before timing.\n"
    "> Haskell-only snapshot publication is reported separately against its\n"
    "> session reference and is never counted as a Spade lane.\n"
)

CLIFF_PREAMBLE = (
    "## Cliff lanes\n\n"
    "These are **expected to be red**, and a red row here is not a regression —\n"
    "it is the measurement working. Each one runs a singleton or degenerate-input\n"
    "entry point against the spade call that has no complexity defect on it, so\n"
    "the gap reports the size of the defect. They are gated exactly as hard as the\n"
    "parity lanes: every one pins its canonical egress against spade first, because\n"
    "timing a quadratic path against a linear one proves nothing until the two are\n"
    "known to compute the same thing. Read the baseline column, not the gap: these\n"
    "exist so that a defect getting *worse*, or a fix landing, has somewhere to\n"
    "show up.\n\n"
    "These rows are included in the Spade standings and `ALL SPADE` summary above;\n"
    "only the explicitly labelled `PARITY` summary excludes them. The small cliff\n"
    "inputs keep a round affordable, not because the underlying defect is small.\n"
    "Snapshot publication stress is reported separately and is not part of either\n"
    "Spade summary.\n"
)


def pin_description(baseline_document: dict) -> str:
    """What the `vs pin` column is measuring against.

    Without this the column reads as progress against spade when it is progress
    against whatever we happened to pin, and a board pinned during a regression
    makes recovery from our own defect look like a win.
    """
    source = baseline_document.get("source")
    commit = baseline_document.get("commit")
    if not source and not commit:
        return (
            "**The pinned baseline records no provenance**, so `vs pin` names no tree."
            " Re-pin with this scorecard to fix that."
        )
    digest = baseline_document.get("source-digest")
    return (
        "`vs pin` compares against "
        + (f"`{commit[:10]}`" if commit else "an unrecorded commit")
        + (f" with measured-source digest `{digest[:10]}`" if digest else "")
        + (f" — {source}" if source else "")
        + ". It is movement against **that tree**, not against spade: a board pinned"
        " while a regression was live makes recovering from our own defect read as"
        " a win. The `Gap` column is the only number that is about spade; the"
        " snapshot `Premium` column is a Haskell session-to-publication measure."
    )


def main() -> None:
    if len(sys.argv) == 3 and sys.argv[1] == "--preflight":
        inventory = read_inventory(Path(sys.argv[2]))
        baseline_document = (
            json.loads(BASELINE.read_text()) if BASELINE.exists() else {}
        )
        complaints = baseline_complaints(inventory, baseline_document)
        if complaints:
            print(
                "baseline preflight refused:\n  "
                + "\n  ".join(complaints)
                + "\nAdopt a complete pin with: ./scorecard.sh refresh <provenance-note>"
            )
            raise SystemExit(1)
        print(
            "baseline preflight passed: "
            f"{len(inventory.parity) + len(inventory.cliff)} Spade lanes and "
            f"{len(inventory.snapshots)} snapshot relationships pinned"
        )
        return

    if len(sys.argv) < 3:
        raise SystemExit(
            "usage: scorecard.py RUN.csv INVENTORY.csv [--check | --pin NOTE]\n"
            "       scorecard.py --preflight INVENTORY.csv"
        )
    run = Path(sys.argv[1])
    if not run.is_file():
        suggestion = (
            " Establish one with: ./scorecard.sh refresh <provenance-note>"
            if run.resolve() == RETAINED_RUN.resolve()
            else ""
        )
        raise SystemExit(
            f"run evidence obstruction: {run} does not exist.{suggestion}"
        )
    inventory = read_inventory(Path(sys.argv[2]))
    options = sys.argv[3:]
    check_requested = "--check" in options
    pin_requested = "--pin" in options
    unknown_options = sorted(
        option
        for option in options
        if option.startswith("--") and option not in {"--check", "--pin"}
    )
    if unknown_options or (check_requested and pin_requested):
        raise SystemExit(
            "option obstruction: "
            + (
                "unknown option(s): " + ", ".join(unknown_options)
                if unknown_options
                else "--check and --pin are mutually exclusive"
            )
        )

    note = next((argument for argument in options if not argument.startswith("--")), None)
    if pin_requested and not note:
        raise SystemExit(
            "pin provenance obstruction: --pin requires a note saying why this tree "
            "becomes the standard"
        )

    samples = read_samples(run)
    provenance = read_provenance(run)
    complaints = inventory_complaints(
        read_declared_lanes(run), inventory
    ) + sample_complaints(samples, inventory)
    if complaints:
        print("".join(f"\ninventory: {line}" for line in complaints))
        print(f"\n{REPORT.name} not written; incomplete observations cannot form a board")
        raise SystemExit(1)

    current_baseline_document = (
        json.loads(BASELINE.read_text()) if BASELINE.exists() else {}
    )

    parity_lanes = lane_table(inventory.parity)
    cliff_lanes = lane_table(inventory.cliff)
    all_lanes = parity_lanes + cliff_lanes
    pinned_document = {
        "gaps": {**gaps(samples, parity_lanes), **gaps(samples, cliff_lanes)},
        GATE_KEY: {
            **gaps(samples, parity_lanes, GATE_STATISTIC),
            **gaps(samples, cliff_lanes, GATE_STATISTIC),
        },
        "snapshot-premiums": snapshot_premiums(samples, inventory.snapshots),
        "commit": provenance.get("commit", ""),
        "source-digest": provenance.get("source-digest", ""),
        "source": note or "",
    }
    baseline_document = pinned_document if pin_requested else current_baseline_document
    baseline_obstructions = baseline_complaints(inventory, baseline_document)
    if baseline_obstructions:
        print(
            "baseline evidence refused:\n  "
            + "\n  ".join(baseline_obstructions)
            + "\nEstablish a complete board with: "
            "./scorecard.sh refresh <provenance-note>"
        )
        raise SystemExit(1)

    baseline = baseline_document["gaps"]
    snapshot_baseline = baseline_document["snapshot-premiums"]
    standings_table = standings(samples, baseline, parity_lanes, cliff_lanes)
    overall = standings_summary(samples, all_lanes)
    parity_table = render(samples, baseline, parity_lanes)
    cliff_table = render(samples, baseline, cliff_lanes)
    parity_summary = standings_summary(samples, parity_lanes)
    snapshot_table = render_snapshot_stress(
        samples, snapshot_baseline, inventory.snapshots
    )

    print(PROTOCOL_NOTICE)
    print("## Spade standings — every comparable lane, worst first\n")
    print(standings_table)
    print()
    print(overall.replace("**", ""))
    print()
    print(parity_table)
    print()
    print(cliff_table)
    print("\n## Moonlight snapshot publication stress\n")
    print(snapshot_table)
    print(f"\nALL SPADE  {overall.replace('**', '')}")
    print(f"PARITY     {parity_summary.replace('**', '')}")

    report = (
        "# spade 2.15.1 vs moonlight-triangulation\n\n"
        f"{PROTOCOL_NOTICE}\n"
        "Derived from `scorecard.csv` and the typed Haskell lane registry by\n"
        "`scorecard.py`. Adopt a fresh board with\n"
        "`./scorecard.sh refresh <note>`. Spade gaps are Moonlight / Spade, lower is better;\n"
        "snapshot premiums are Haskell publication / session, lower is better. The agreement\n"
        "gate runs first; a disagreement aborts before any timing.\n\n"
        "## Spade standings — every comparable lane, worst first\n\n"
        "This table answers *where are we behind* on cross-language work. Snapshot\n"
        "publication has no Spade timing row and appears in its own table below.\n\n"
        f"{standings_table}\n\n"
        f"{overall}\n\n"
        f"{pin_description(baseline_document)}\n\n"
        "## Parity lanes\n\n"
        f"{parity_table}\n\n"
        f"{parity_summary}\n\n"
        f"{CLIFF_PREAMBLE}\n"
        f"{cliff_table}\n\n"
        "## Moonlight snapshot publication stress\n\n"
        "These rows compare the Haskell session reference with immutable snapshot\n"
        "publication. They are not Spade gaps; the premium is snapshot/session.\n\n"
        f"{snapshot_table}\n"
    )

    if check_requested:
        # Two verdicts, kept apart on purpose. The agreement gate has already
        # run inside driver.sh and a disagreement never reached this file: it is
        # a claim about correctness and it aborts before anything is timed.
        # What follows is the weaker, noisier claim about speed, and collapsing
        # the two would let a timing wobble read with the authority of a proof.
        gate_pin = baseline_document[GATE_KEY]
        slower = regressions(samples, gate_pin, parity_lanes, cliff_lanes)
        if slower:
            print(
                f"\n{len(slower)} lane(s) regressed past "
                f"{REGRESSION_TOLERANCE:.0%} against the pin:"
            )
            print("\n".join(f"  {line}" for line in slower))
            print(
                "Re-run to rule out drift. If the widening is real and intended,"
                " adopt it with: ./scorecard.sh refresh <provenance-note>"
            )
            print(f"\n{REPORT.name} not written; this run did not earn it")
            raise SystemExit(1)
        print(
            f"\ncheck passed: agreement gated, {len(all_lanes)}"
            f" lanes accounted for, none regressed past {REGRESSION_TOLERANCE:.0%}"
            f" ({len(NON_REPEATING)} lane(s) measured but not gated:"
            f" {', '.join(sorted(NON_REPEATING))})"
        )
        return

    # A refresh writes the pin and its projection from the same validated local
    # sections. The report therefore describes the baseline being adopted, not
    # the obsolete baseline that happened to exist before the command began.
    if pin_requested:
        BASELINE.write_text(
            json.dumps(baseline_document, indent=2, sort_keys=True) + "\n"
        )
        print(
            f"\npinned {len(baseline_document['gaps'])} Spade gaps and "
            f"{len(baseline_document['snapshot-premiums'])} snapshot premiums "
            f"at {provenance['commit'][:10]}"
        )
    REPORT.write_text(report)


if __name__ == "__main__":
    main()
