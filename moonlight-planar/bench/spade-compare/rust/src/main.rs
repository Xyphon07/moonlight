//! Rust half of the spade external referent.
//!
//! `moonlight-planar` is a port of this crate, so spade is the referent
//! that grades it. Both halves generate their inputs from the same LCG with the
//! same seeds and emit their gate output in the same encoding, so neither side
//! can reformat a disagreement into agreement.
//!
//! Vertex handles are NOT comparable across the two implementations — both bulk
//! loaders reorder. Anything naming a vertex names it by input index or by
//! coordinate, never by handle.

#[cfg(feature = "alpha-persistence")]
use std::collections::{BTreeSet, HashMap};
#[cfg(feature = "alpha-persistence")]
use std::fmt;
use std::fs;
use std::path::Path;
use std::time::Instant;

#[cfg(feature = "alpha-persistence")]
use alphalpha::{alpha_filtration, sparsify};
#[cfg(feature = "alpha-persistence")]
use itertools::Itertools;
#[cfg(feature = "alpha-persistence")]
use lophat::{
    algorithms::{RVDecomposition, SerialAlgorithm},
    columns::{Column, VecColumn},
};
use spade::handles::{DirectedEdgeHandle, DirectedVoronoiEdge, FixedVertexHandle, VoronoiVertex};
use spade::{
    AngleLimit, ConstrainedDelaunayTriangulation, DelaunayTriangulation,
    HierarchyHintGeneratorWithBranchFactor, Intersection, LineIntersectionIterator, Point2,
    PositionInTriangulation, RefinementParameters, Triangulation,
};
use spade_referent_rs::random_points;

type Hierarchy = HierarchyHintGeneratorWithBranchFactor<f64, 16>;
type HierarchyTriangulation = DelaunayTriangulation<Point2<f64>, (), (), (), Hierarchy>;
type Plain = DelaunayTriangulation<Point2<f64>>;
type Cdt = ConstrainedDelaunayTriangulation<Point2<f64>>;
type PrimalEdge<'a> = DirectedEdgeHandle<'a, Point2<f64>, (), (), ()>;
type DualEdge<'a> = DirectedVoronoiEdge<'a, Point2<f64>, (), (), ()>;
type DualVertex<'a> = VoronoiVertex<'a, Point2<f64>, (), (), ()>;

#[cfg(feature = "alpha-persistence")]
mod alpha_persistence_support {
    use super::*;

    struct SourceAlphaCell {
        old_index: usize,
        birth: f64,
        column: VecColumn,
    }

    #[derive(Clone, Copy)]
    struct OrderedAlphaCell {
        birth: f64,
        dimension: usize,
    }

    #[derive(Clone, Copy)]
    struct AlphaInterval {
        dimension: usize,
        birth: f64,
        death: Option<f64>,
    }

    #[derive(Debug)]
    enum AlphaPersistenceError {
        BirthColumnCountMismatch {
            births: usize,
            columns: usize,
        },
        NonFiniteBirth {
            dimension: usize,
            index: usize,
            birth: f64,
        },
        BoundaryCellMissing {
            source: usize,
            target: usize,
        },
        PersistenceCellMissing {
            index: usize,
        },
    }

    impl fmt::Display for AlphaPersistenceError {
        fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Self::BirthColumnCountMismatch { births, columns } => write!(
                    formatter,
                    "alpha birth/column count mismatch: {births} births for {columns} columns",
                ),
                Self::NonFiniteBirth {
                    dimension,
                    index,
                    birth,
                } => write!(
                    formatter,
                    "non-finite alpha birth at dimension {dimension}, handle {index}: {birth}",
                ),
                Self::BoundaryCellMissing { source, target } => write!(
                    formatter,
                    "alpha column {source} references absent boundary column {target}",
                ),
                Self::PersistenceCellMissing { index } => {
                    write!(formatter, "persistence returned absent cell index {index}")
                }
            }
        }
    }

    impl std::error::Error for AlphaPersistenceError {}

    #[derive(Debug)]
    struct AlphaPersistenceReceipt {
        critical_births: usize,
        vertex_cells: usize,
        edge_cells: usize,
        face_cells: usize,
        finite_pairs: usize,
        essential_pairs: usize,
        profile_checksum: u64,
    }

    impl AlphaPersistenceReceipt {
        fn render(&self) -> String {
            format!(
                "{},{},{},{},{},{},{}",
                self.critical_births,
                self.vertex_cells,
                self.edge_cells,
                self.face_cells,
                self.finite_pairs,
                self.essential_pairs,
                self.profile_checksum,
            )
        }
    }

    /// End-to-end Spade referent for Moonlight's exact alpha-persistence path.
    /// `alphalpha` owns the Spade construction and birth formulas; this boundary
    /// deliberately replaces its dimension-first `sparsify` order with the lawful
    /// global `(birth, dimension, handle)` filtration order before LoPHAT reduces
    /// the matrix. An unrelated face may precede a later edge, so dimension-first
    /// order is not a persistence filtration.
    fn alpha_persistence_receipt(
        points: Vec<Point2<f64>>,
    ) -> Result<AlphaPersistenceReceipt, AlphaPersistenceError> {
        let filtration = alpha_filtration(points);
        let (dimension_ordered_columns, dimension_ordered_births) = sparsify(&filtration);
        let (columns, ordered_cells) =
            globally_order_alpha_columns(dimension_ordered_columns, dimension_ordered_births)?;
        let diagram = SerialAlgorithm::decompose(columns.into_iter(), None).diagram();
        let finite_intervals = diagram
            .paired
            .iter()
            .map(|&(birth_index, death_index)| {
                let birth_cell = persistence_cell(&ordered_cells, birth_index)?;
                let death_cell = persistence_cell(&ordered_cells, death_index)?;
                Ok(AlphaInterval {
                    dimension: birth_cell.dimension,
                    birth: birth_cell.birth,
                    death: Some(death_cell.birth),
                })
            })
            .collect::<Result<Vec<_>, AlphaPersistenceError>>()?;
        let essential_intervals = diagram
            .unpaired
            .iter()
            .map(|&birth_index| {
                let birth_cell = persistence_cell(&ordered_cells, birth_index)?;
                Ok(AlphaInterval {
                    dimension: birth_cell.dimension,
                    birth: birth_cell.birth,
                    death: None,
                })
            })
            .collect::<Result<Vec<_>, AlphaPersistenceError>>()?;
        let intervals = finite_intervals
            .iter()
            .chain(essential_intervals.iter())
            .copied()
            .collect::<Vec<_>>();
        let critical_births = ordered_cells
            .iter()
            .map(|cell| canonical_nonnegative_bits(cell.birth))
            .collect::<BTreeSet<_>>()
            .into_iter()
            .map(f64::from_bits)
            .collect::<Vec<_>>();
        let [vertex_cells, edge_cells, face_cells] =
            ordered_cells
                .iter()
                .fold([0usize; 3], |counts, cell| match (counts, cell.dimension) {
                    ([vertices, edges, faces], 0) => [vertices + 1, edges, faces],
                    ([vertices, edges, faces], 1) => [vertices, edges + 1, faces],
                    ([vertices, edges, faces], _) => [vertices, edges, faces + 1],
                });
        let profile_checksum = critical_births
            .iter()
            .flat_map(|&threshold| betti_profile(&intervals, threshold))
            .fold(FNV_OFFSET, mix_checksum);
        Ok(AlphaPersistenceReceipt {
            critical_births: critical_births.len(),
            vertex_cells,
            edge_cells,
            face_cells,
            finite_pairs: finite_intervals.len(),
            essential_pairs: essential_intervals.len(),
            profile_checksum,
        })
    }

    fn globally_order_alpha_columns(
        columns: Vec<VecColumn>,
        births: Vec<f64>,
    ) -> Result<(Vec<VecColumn>, Vec<OrderedAlphaCell>), AlphaPersistenceError> {
        (columns.len() == births.len()).then_some(()).ok_or(
            AlphaPersistenceError::BirthColumnCountMismatch {
                births: births.len(),
                columns: columns.len(),
            },
        )?;
        let source_cells = columns
            .into_iter()
            .zip(births)
            .enumerate()
            .map(|(old_index, (column, birth))| SourceAlphaCell {
                old_index,
                birth,
                column,
            })
            .sorted_by(|left, right| {
                left.birth
                    .total_cmp(&right.birth)
                    .then(left.column.dimension().cmp(&right.column.dimension()))
                    .then(left.old_index.cmp(&right.old_index))
            })
            .collect::<Vec<_>>();
        source_cells
            .iter()
            .find(|cell| !cell.birth.is_finite() || cell.birth < 0.0)
            .map(|cell| AlphaPersistenceError::NonFiniteBirth {
                dimension: cell.column.dimension(),
                index: cell.old_index,
                birth: cell.birth,
            })
            .map_or(Ok(()), Err)?;
        let new_index_by_old = source_cells
            .iter()
            .enumerate()
            .map(|(new_index, cell)| (cell.old_index, new_index))
            .collect::<HashMap<_, _>>();
        let ordered_cells = source_cells
            .iter()
            .map(|cell| OrderedAlphaCell {
                birth: cell.birth,
                dimension: cell.column.dimension(),
            })
            .collect::<Vec<_>>();
        let reordered_columns = source_cells
            .iter()
            .map(|cell| {
                let boundary = cell
                    .column
                    .entries()
                    .map(|old_target| {
                        new_index_by_old.get(&old_target).copied().ok_or(
                            AlphaPersistenceError::BoundaryCellMissing {
                                source: cell.old_index,
                                target: old_target,
                            },
                        )
                    })
                    .collect::<Result<Vec<_>, _>>()?
                    .into_iter()
                    .sorted()
                    .collect::<Vec<_>>();
                Ok(VecColumn::from((cell.column.dimension(), boundary)))
            })
            .collect::<Result<Vec<_>, _>>()?;
        Ok((reordered_columns, ordered_cells))
    }

    fn persistence_cell(
        ordered_cells: &[OrderedAlphaCell],
        index: usize,
    ) -> Result<OrderedAlphaCell, AlphaPersistenceError> {
        ordered_cells
            .get(index)
            .copied()
            .ok_or(AlphaPersistenceError::PersistenceCellMissing { index })
    }

    fn canonical_nonnegative_bits(value: f64) -> u64 {
        if value == 0.0 {
            0.0f64.to_bits()
        } else {
            value.to_bits()
        }
    }

    fn betti_profile(intervals: &[AlphaInterval], threshold: f64) -> [u64; 3] {
        intervals
            .iter()
            .filter(|interval| {
                interval.birth <= threshold
                    && interval.death.map_or(true, |death| threshold < death)
            })
            .fold([0u64; 3], |counts, interval| {
                match (counts, interval.dimension) {
                    ([degree_zero, degree_one, degree_two], 0) => {
                        [degree_zero + 1, degree_one, degree_two]
                    }
                    ([degree_zero, degree_one, degree_two], 1) => {
                        [degree_zero, degree_one + 1, degree_two]
                    }
                    ([degree_zero, degree_one, degree_two], _) => {
                        [degree_zero, degree_one, degree_two + 1]
                    }
                }
            })
    }

    const FNV_OFFSET: u64 = 14_695_981_039_346_656_037;
    const FNV_PRIME: u64 = 1_099_511_628_211;

    fn mix_checksum(accumulator: u64, value: u64) -> u64 {
        (accumulator ^ value).wrapping_mul(FNV_PRIME)
    }

    fn require_alpha_persistence(
        result: Result<AlphaPersistenceReceipt, AlphaPersistenceError>,
    ) -> AlphaPersistenceReceipt {
        match result {
            Ok(receipt) => receipt,
            Err(obstruction) => {
                eprintln!("alpha persistence refused: {obstruction}");
                std::process::exit(1);
            }
        }
    }

    fn parse_point_count(raw: &str) -> usize {
        match raw.parse() {
            Ok(count) => count,
            Err(obstruction) => {
                eprintln!("invalid alpha-persistence point count {raw:?}: {obstruction}");
                std::process::exit(2);
            }
        }
    }

    pub(super) fn report(raw_count: &str) {
        let receipt = require_alpha_persistence(alpha_persistence_receipt(random_points(
            0x9e37_79b9_7f4a_7c15,
            parse_point_count(raw_count),
        )));
        println!("{}", receipt.render());
    }

    pub(super) fn bench_one(point_count: usize, second: usize) {
        let points = random_points(0x9e37_79b9_7f4a_7c15, point_count);
        super::measure("alpha-persistence", point_count, second, move || {
            require_alpha_persistence(alpha_persistence_receipt(points))
        });
    }
}

/// The same generated points flattened onto a sliver a picometre thick. No
/// coordinate becomes equal to another and no four points become cocircular, so
/// the input stays in general position and the load stays on the ordinary
/// face-building path; what collapses is the angular spread the bulk loaders
/// index their hulls by. That collapse is the whole of what this input
/// measures, and it does not reach the face-less location path — that one needs
/// exact collinearity, which `exactly_collinear_points` supplies and gates.
fn near_collinear_points(seed: u64, count: usize) -> Vec<Point2<f64>> {
    random_points(seed, count)
        .into_iter()
        .map(|point| Point2::new(point.x, point.y * 1.0e-12))
        .collect()
}

/// Points on one exact line: `(i, 0)`, every coordinate an integer that an
/// `f64` holds exactly. No three are in general position, so the orientation
/// predicate returns an exact zero on both sides and no face is ever built.
///
/// Uniqueness does not need general position, only one valid answer. A
/// collinear set admits exactly one triangulation — the chain of consecutive
/// segments, `n - 1` of them and no triangle — so the canonical edge set is
/// fully determined and gates bit for bit against the Moonlight side. Both
/// implementations state that same count as their own degenerate Euler
/// invariant, which is what makes this a shared answer rather than a
/// coincidence.
fn exactly_collinear_points(count: usize) -> Vec<Point2<f64>> {
    (0..count)
        .map(|index| Point2::new(index as f64, 0.0))
        .collect()
}

/// Disjoint split cells, stacked in y. Band `i` carries a constraint from
/// (16, 4i) to (16, 4i + 2) and a crossing request from (0, 4i + a) to
/// (32, 4i + b), with a and b jittered inside the band, so every request
/// crosses exactly one constraint and no request reaches another band.
///
/// The geometry is chosen so the split point is not merely close on the two
/// sides but identical. spade solves the two line equations by Cramer's rule
/// and moonlight walks the parametric form; here every coefficient either
/// vanishes or is a power of two, both reduce to one rounding of
/// `u + (v - u) / 2`, and neither can round it differently. A generic crossing
/// would leave the two formulas free to disagree in the last bit and there
/// would be nothing to gate.
fn split_bands(band_count: usize) -> (Vec<Point2<f64>>, Vec<[usize; 2]>, Vec<(usize, usize)>) {
    let jitter = random_points(0x51ed_270b_7c1f_d1a3, band_count);
    let vertices: Vec<Point2<f64>> = jitter
        .iter()
        .enumerate()
        .flat_map(|(index, offset)| {
            let base = 4.0 * index as f64;
            [
                Point2::new(16.0, base),
                Point2::new(16.0, base + 2.0),
                Point2::new(0.0, base + 1.0 + 0.5 * offset.x),
                Point2::new(32.0, base + 1.0 + 0.5 * offset.y),
            ]
        })
        .collect();
    let edges = (0..band_count)
        .map(|index| [index * 4, index * 4 + 1])
        .collect();
    let crossings = (0..band_count)
        .map(|index| (index * 4 + 2, index * 4 + 3))
        .collect();
    (vertices, edges, crossings)
}

/// Canonical edge set: each undirected edge as its two endpoint coordinates in
/// IEEE-754 bit patterns, endpoint-ordered then globally sorted. Formatting
/// cannot introduce a spurious disagreement and rounding cannot hide a real one.
fn canonical_edges<T: Triangulation<Vertex = Point2<f64>>>(triangulation: &T) -> Vec<String> {
    let mut edges: Vec<String> = triangulation
        .undirected_edges()
        .map(|edge| {
            let [a, b] = edge.vertices();
            encode_canonical_edge(a.position(), b.position())
        })
        .collect();
    edges.sort_unstable();
    edges
}

fn canonical_constraint_edges(triangulation: &Cdt) -> Vec<String> {
    let mut edges: Vec<String> = triangulation
        .undirected_edges()
        .filter(|edge| edge.is_constraint_edge())
        .map(|edge| {
            let [a, b] = edge.vertices();
            encode_canonical_edge(a.position(), b.position())
        })
        .collect();
    edges.sort_unstable();
    edges
}

fn coord_hex(point: Point2<f64>) -> String {
    format!("{:016x}{:016x}", point.x.to_bits(), point.y.to_bits())
}

/// A cycle, rotated to begin at its smallest encoded element.
///
/// Both implementations keep one entry edge per vertex and per face — whichever
/// their builder wrote last — so the two walk the same cycle from different
/// places. Rotating pins the order, which the mesh determines, and declines to
/// pin the starting point, which it does not. The rotation compares the encoded
/// strings rather than the coordinates so that both sides order by the same
/// comparison: the bit pattern of a negative double does not sort in numeric
/// order, so a Haskell `Ord` on `Point` and a Rust `partial_cmp` on a tuple
/// would each have to be argued to agree where two byte strings simply do.
fn rotate_to_smallest(mut ring: Vec<String>) -> Vec<String> {
    let start = ring
        .iter()
        .enumerate()
        .min_by(|left, right| left.1.cmp(right.1))
        .map(|(index, _)| index)
        .unwrap_or(0);
    ring.rotate_left(start);
    ring
}

fn encode_canonical_edge(p: Point2<f64>, q: Point2<f64>) -> String {
    let (first, second) = if (p.x, p.y) <= (q.x, q.y) {
        (p, q)
    } else {
        (q, p)
    };
    format!(
        "{:016x}{:016x}{:016x}{:016x}",
        first.x.to_bits(),
        first.y.to_bits(),
        second.x.to_bits(),
        second.y.to_bits()
    )
}

/// Pin the exact output of every timed workload before comparing its cost.
fn write_gate(directory: &Path) {
    fs::create_dir_all(directory).expect("gate directory");
    [1_000usize, 10_000].into_iter().for_each(|count| {
        let triangulation =
            Plain::bulk_load(random_points(0x9e37_79b9_7f4a_7c15, count)).expect("bulk load");
        fs::write(
            directory.join(format!("bulk-load-{count}-edges.txt")),
            format!("{}\n", canonical_edges(&triangulation).join("\n")),
        )
        .expect("bulk-load gate file");
    });
    [1_000usize, 10_000].into_iter().for_each(|count| {
        let points = random_points(0x9e37_79b9_7f4a_7c15, count);
        let triangulation = insert_all_incrementally(&points);
        fs::write(
            directory.join(format!("incremental-{count}-edges.txt")),
            format!("{}\n", canonical_edges(&triangulation).join("\n")),
        )
        .expect("incremental gate file");
    });

    let hierarchy: HierarchyTriangulation =
        HierarchyTriangulation::bulk_load(random_points(0x0123_4567_89ab_cdef, 20_000))
            .expect("bulk load");
    fs::write(
        directory.join("nearest-20000-5000.txt"),
        hierarchy_nearest_lines(&hierarchy, &random_points(0x3141_5926_5358_9793, 5_000)),
    )
    .expect("nearest gate file");

    let points = random_points(0x94d0_49bb_1331_11eb, 8_000);
    let base: Cdt = Cdt::bulk_load(points.clone()).expect("bulk load");
    let handles = locate_vertex_handles(&base, &points);
    let pairs = constraint_pairs(8_000, 800);
    let (recovered, accepted) = recover_constraint_program(base, &handles, &pairs);
    let accepted_body = format!(
        "{}\n",
        accepted
            .iter()
            .map(usize::to_string)
            .collect::<Vec<_>>()
            .join("\n")
    );
    fs::write(directory.join("cdt-accepted-8000-800.txt"), accepted_body).expect("gate file");
    let constraints_body = format!("{}\n", canonical_constraint_edges(&recovered).join("\n"));
    fs::write(
        directory.join("cdt-constraints-8000-800.txt"),
        constraints_body,
    )
    .expect("gate file");

    [625usize, 2_500]
        .into_iter()
        .for_each(|budget| write_refinement_gate(directory, budget));

    [(1_000usize, 250usize), (10_000, 2_500)]
        .into_iter()
        .for_each(|(point_count, removal_count)| {
            write_removal_gate(directory, point_count, removal_count)
        });
    [(10_000usize, 1_000usize), (100_000, 2_000)]
        .into_iter()
        .for_each(|(point_count, query_count)| {
            write_interpolation_gate(directory, point_count, query_count)
        });
    write_voronoi_gate(directory, 1_000);
    write_dcel_walk_gate(directory, 2_000);
    write_intersection_gate(directory, 10_000, 500);
    write_intersection_vertex_gate(directory, 2_000);

    // Persistent artifacts validate final-state agreement only; they are not
    // Spade timing ratios. Their canonical egress is retained for the hard gate.
    write_persistent_gate(directory, 1_000);
    write_persistent_removal_gate(directory, 10_000, 2_500);
    // The remaining diagnostic gates. Timing is meaningful only after both
    // sides have been shown to compute the same canonical result.
    write_constraint_incremental_gate(directory, 8_000, 800);
    write_constraint_split_gate(directory, 1_000);
    write_sweep_angle_gate(directory, 2_000);
    write_degenerate_line_gate(directory, 2_000);
    write_hierarchy_incremental_gate(directory, 1_000);
    write_hierarchy_duplicate_gate(directory, 10_000, 500);
    write_hierarchy_removal_gate(directory, 10_000, 250);
    write_intersection_outside_gate(directory, 2_000, 100);
}

const HIERARCHY_GATE_QUERIES: usize = 1_000;

/// The hierarchy lanes' gate, in two parts. The base mesh alone would not see a
/// hierarchy whose nesting law has drifted, because a search started from a bad
/// hint still walks to the right answer; a hint naming a vertex that does not
/// exist, or one the search cannot walk out of, moves the answers. Both are
/// pinned.
fn write_hierarchy_gate(directory: &Path, label: &str, triangulation: &HierarchyTriangulation) {
    fs::write(
        directory.join(format!("{label}-edges.txt")),
        format!("{}\n", canonical_edges(triangulation).join("\n")),
    )
    .expect("hierarchy edge gate file");
    fs::write(
        directory.join(format!("{label}-nearest.txt")),
        hierarchy_nearest_lines(
            triangulation,
            &random_points(0x3141_5926_5358_9793, HIERARCHY_GATE_QUERIES),
        ),
    )
    .expect("hierarchy nearest gate file");
}

fn write_hierarchy_incremental_gate(directory: &Path, point_count: usize) {
    let triangulation =
        insert_all_into_hierarchy(&random_points(0x9e37_79b9_7f4a_7c15, point_count));
    write_hierarchy_gate(
        directory,
        &format!("hierarchy-incremental-{point_count}"),
        &triangulation,
    );
}

fn write_hierarchy_duplicate_gate(directory: &Path, point_count: usize, duplicate_count: usize) {
    let base: HierarchyTriangulation =
        HierarchyTriangulation::bulk_load(random_points(0x9e37_79b9_7f4a_7c15, point_count))
            .expect("bulk load");
    let duplicates = random_points(0x9e37_79b9_7f4a_7c15, duplicate_count);
    let triangulation = reinsert_all_into_hierarchy(base, &duplicates);
    write_hierarchy_gate(
        directory,
        &format!("hierarchy-duplicate-{point_count}-{duplicate_count}"),
        &triangulation,
    );
}

fn write_hierarchy_removal_gate(directory: &Path, point_count: usize, removal_count: usize) {
    let points = random_points(0x9e37_79b9_7f4a_7c15, point_count);
    let base: HierarchyTriangulation =
        HierarchyTriangulation::bulk_load(points.clone()).expect("bulk load");
    let triangulation = remove_all_from_hierarchy(base, &points[..removal_count]);
    write_hierarchy_gate(
        directory,
        &format!("hierarchy-removal-{point_count}-{removal_count}"),
        &triangulation,
    );
}

/// Arrival-order insertion, which is what the persistent snapshot witness
/// checks. The survivor must be the incremental lane's mesh on both sides.
fn write_persistent_gate(directory: &Path, point_count: usize) {
    let triangulation =
        insert_all_incrementally(&random_points(0x9e37_79b9_7f4a_7c15, point_count));
    fs::write(
        directory.join(format!("persistent-{point_count}-edges.txt")),
        format!("{}\n", canonical_edges(&triangulation).join("\n")),
    )
    .expect("persistent gate file");
}

fn write_persistent_removal_gate(directory: &Path, point_count: usize, removal_count: usize) {
    let points = random_points(0x9e37_79b9_7f4a_7c15, point_count);
    let base: Plain = Plain::bulk_load(points.clone()).expect("bulk load");
    let surviving = remove_all_by_coordinate(base, &points[..removal_count]);
    fs::write(
        directory.join(format!(
            "persistent-removal-{point_count}-{removal_count}-edges.txt"
        )),
        format!("{}\n", canonical_edges(&surviving).join("\n")),
    )
    .expect("persistent removal gate file");
}

/// The singleton constraint program's accepted request indices and final
/// constrained edge set. A blocked request must be refused at the same index on
/// both sides, or the two are not running the same program.
fn write_constraint_incremental_gate(
    directory: &Path,
    point_count: usize,
    constraint_count: usize,
) {
    let points = random_points(0x94d0_49bb_1331_11eb, point_count);
    let base: Cdt = Cdt::bulk_load(points.clone()).expect("bulk load");
    let handles = locate_vertex_handles(&base, &points);
    let pairs = constraint_pairs(point_count, constraint_count);
    let (recovered, accepted) = add_constraint_edge_program(base, &points, &handles, &pairs);
    fs::write(
        directory.join(format!(
            "constraint-incremental-{point_count}-{constraint_count}-accepted.txt"
        )),
        format!(
            "{}\n",
            accepted
                .iter()
                .map(usize::to_string)
                .collect::<Vec<_>>()
                .join("\n")
        ),
    )
    .expect("gate file");
    fs::write(
        directory.join(format!(
            "constraint-incremental-{point_count}-{constraint_count}-constraints.txt"
        )),
        format!("{}\n", canonical_constraint_edges(&recovered).join("\n")),
    )
    .expect("gate file");
}

/// Both the split vertices and the constrained edges they carve. The edge set
/// names every vertex by coordinate, so a split point that landed one ulp away
/// on one side shows up here rather than hiding behind a matching edge count.
fn write_constraint_split_gate(directory: &Path, band_count: usize) {
    let (vertices, edges, crossings) = split_bands(band_count);
    let base: Cdt = Cdt::bulk_load_cdt(vertices.clone(), edges).expect("cdt bulk load");
    let handles = locate_vertex_handles(&base, &vertices);
    let split = split_constraint_program(base, &handles, &crossings);
    fs::write(
        directory.join(format!("constraint-split-{band_count}-edges.txt")),
        format!("{}\n", canonical_edges(&split).join("\n")),
    )
    .expect("gate file");
    fs::write(
        directory.join(format!("constraint-split-{band_count}-constraints.txt")),
        format!("{}\n", canonical_constraint_edges(&split).join("\n")),
    )
    .expect("gate file");
}

fn write_sweep_angle_gate(directory: &Path, point_count: usize) {
    let triangulation = Plain::bulk_load(near_collinear_points(0x9e37_79b9_7f4a_7c15, point_count))
        .expect("bulk load");
    fs::write(
        directory.join(format!("sweep-angle-collapse-{point_count}-edges.txt")),
        format!("{}\n", canonical_edges(&triangulation).join("\n")),
    )
    .expect("sweep angle gate file");
}

/// The degenerate chain, pinned by the only thing it has: its edges.
///
/// There are no faces to compare and no circumcentres to argue about, so unlike
/// every other gate here this one has no per-side half and no exclusion. The
/// answer is `n - 1` consecutive segments and the two sides either both produce
/// it or one of them is wrong.
fn write_degenerate_line_gate(directory: &Path, point_count: usize) {
    let triangulation = Plain::bulk_load(exactly_collinear_points(point_count)).expect("bulk load");
    fs::write(
        directory.join(format!("degenerate-line-{point_count}-edges.txt")),
        format!("{}\n", canonical_edges(&triangulation).join("\n")),
    )
    .expect("degenerate line gate file");
}

/// Remove vertices by coordinate. The caller owns the mesh being emptied, so
/// the reset every mutable run needs — building or cloning that mesh — happens
/// before the clock starts, and the removals alone are the measured work.
fn remove_all_by_coordinate(mut triangulation: Plain, removals: &[Point2<f64>]) -> Plain {
    for point in removals {
        triangulation.locate_and_remove(*point);
    }
    triangulation
}

/// The removal gate: a deterministic prefix of the input, removed by
/// coordinate, and the surviving canonical edge set. Delaunay triangulations
/// of point sets in general position are unique, so the survivor is forced
/// and both implementations must agree bit-for-bit.
fn write_removal_gate(directory: &Path, point_count: usize, removal_count: usize) {
    let points = random_points(0x9e37_79b9_7f4a_7c15, point_count);
    let base: Plain = Plain::bulk_load(points.clone()).expect("bulk load");
    let surviving = remove_all_by_coordinate(base, &points[..removal_count]);
    fs::write(
        directory.join(format!("removal-{point_count}-{removal_count}-edges.txt")),
        format!("{}\n", canonical_edges(&surviving).join("\n")),
    )
    .expect("removal gate file");
}

/// The interpolation gate, two artifacts per query. The neighbour set is
/// exact-predicate determined over triangulations that already agree
/// bit-for-bit, so it crosses implementations. The weights cannot: each
/// neighbour's stolen area uses the same operation sequence on both sides,
/// but the normalization total accumulates those areas in each side's own
/// neighbour order, and floating-point addition is not associative. The
/// weights are pinned per side in the driver instead.
fn write_interpolation_gate(directory: &Path, point_count: usize, query_count: usize) {
    let triangulation =
        Plain::bulk_load(random_points(0x0123_4567_89ab_cdef, point_count)).expect("bulk load");
    let natural_neighbor = triangulation.natural_neighbor();
    let mut buffer: Vec<(FixedVertexHandle, f64)> = Vec::new();
    let mut neighbor_lines: Vec<String> = Vec::new();
    let mut weight_lines: Vec<String> = Vec::new();
    for query in random_points(0x2718_2818_2845_9045, query_count) {
        natural_neighbor.get_weights(query, &mut buffer);
        let mut entries: Vec<(f64, f64, f64)> = buffer
            .iter()
            .map(|(handle, weight)| {
                let position = triangulation.vertex(*handle).position();
                (position.x, position.y, *weight)
            })
            .collect();
        entries.sort_by(|left, right| {
            (left.0, left.1)
                .partial_cmp(&(right.0, right.1))
                .expect("finite coordinates")
        });
        let query_hex = format!("{:016x}{:016x}", query.x.to_bits(), query.y.to_bits());
        neighbor_lines.push(format!(
            "{}{}",
            query_hex,
            entries
                .iter()
                .map(|(x, y, _)| format!("{:016x}{:016x}", x.to_bits(), y.to_bits()))
                .collect::<String>()
        ));
        weight_lines.push(format!(
            "{}{}",
            query_hex,
            entries
                .iter()
                .map(|(x, y, weight)| {
                    format!(
                        "{:016x}{:016x}{:016x}",
                        x.to_bits(),
                        y.to_bits(),
                        weight.to_bits()
                    )
                })
                .collect::<String>()
        ));
    }
    fs::write(
        directory.join(format!(
            "interpolation-{point_count}-{query_count}-neighbors.txt"
        )),
        format!("{}\n", neighbor_lines.join("\n")),
    )
    .expect("interpolation neighbors gate file");
    fs::write(
        directory.join(format!(
            "interpolation-{point_count}-{query_count}-weights.txt"
        )),
        format!("{}\n", weight_lines.join("\n")),
    )
    .expect("interpolation weights gate file");
}

/// The dual, pinned by its combinatorics and by nothing else.
///
/// Every verb the dual offers resolves a primal element, so its answers are
/// exact over a mesh the bulk-load gate already pins bit for bit. Voronoi vertex
/// POSITIONS are the exception and are deliberately absent from every artifact
/// here: a position is a circumcentre, and the two implementations do not form
/// one the same way. spade takes the reciprocal of the determinant and
/// multiplies by it; moonlight first rescales both difference vectors by their
/// largest component and only then divides. Each rounds where the other does
/// not, so the positions cannot be bit-identical, and a gate over them would be
/// grading the arithmetic rather than the dual. What IS determined about a
/// position is whether it exists at all, and the vertex artifact pins that.
///
/// Direction vectors are pinned in full: each component is one subtraction of
/// two stored coordinates on both sides, so there is nothing left to round.
fn write_voronoi_gate(directory: &Path, count: usize) {
    let triangulation: Plain =
        Plain::bulk_load(random_points(0x9e37_79b9_7f4a_7c15, count)).expect("bulk load");
    let mut dual: Vec<String> = triangulation
        .directed_voronoi_edges()
        .map(dual_edge_record)
        .collect();
    dual.sort_unstable();
    fs::write(
        directory.join(format!("voronoi-{count}-dual.txt")),
        format!("{}\n", dual.join("\n")),
    )
    .expect("voronoi dual gate file");
    fs::write(
        directory.join(format!("voronoi-{count}-cells.txt")),
        format!("{}\n", voronoi_cell_lines(&triangulation).join("\n")),
    )
    .expect("voronoi cell gate file");
    fs::write(
        directory.join(format!("voronoi-{count}-vertices.txt")),
        format!("{}\n", voronoi_vertex_lines(&triangulation).join("\n")),
    )
    .expect("voronoi vertex gate file");
}

/// One directed dual edge: the primal edge it names, the site of the dual face
/// to its left, the dual vertex at each end, the destinations its dual `next`
/// and `prev` reach, and its direction vector. `next` and `prev` rotate about
/// the primal origin, so naming them by destination alone loses nothing.
fn dual_edge_record(edge: DualEdge<'_>) -> String {
    let primal = edge.as_delaunay_edge();
    format!(
        "{}{}{}{}{}{}{}{}",
        coord_hex(primal.from().position()),
        coord_hex(primal.to().position()),
        coord_hex(edge.face().as_delaunay_vertex().position()),
        dual_vertex_name(primal, edge.from()),
        dual_vertex_name(primal, edge.to()),
        coord_hex(edge.next().as_delaunay_edge().to().position()),
        coord_hex(edge.prev().as_delaunay_edge().to().position()),
        coord_hex(edge.direction_vector()),
    )
}

/// An inner dual vertex named by the one corner of its dual face that is not an
/// endpoint of the edge the question was asked about — the record already
/// carries the other two, so the apex completes the face. An outer dual vertex
/// is the edge itself and needs no name.
fn dual_vertex_name<'a>(primal: PrimalEdge<'a>, vertex: DualVertex<'a>) -> String {
    match vertex.as_delaunay_face() {
        None => "O".to_string(),
        Some(face) => {
            let (from, to) = (primal.from(), primal.to());
            let apex = face
                .vertices()
                .into_iter()
                .find(|corner| *corner != from && *corner != to)
                .expect("an inner dual face carries a corner off its own dual edge");
            format!("I{}", coord_hex(apex.position()))
        }
    }
}

/// The dual faces, one line each, plus the three enumerator lengths. The
/// undirected dual carries no artifact of its own: it is the primal undirected
/// edge set relabelled, which every existing edge gate already pins, so its
/// length is the only claim left to make about it.
fn voronoi_cell_lines(triangulation: &Plain) -> Vec<String> {
    let mut lines = vec![
        format!("faces {}", triangulation.voronoi_faces().count()),
        format!(
            "directed {}",
            triangulation.directed_voronoi_edges().count()
        ),
        format!(
            "undirected {}",
            triangulation.undirected_voronoi_edges().count()
        ),
    ];
    let mut cells: Vec<String> = triangulation
        .voronoi_faces()
        .map(|face| {
            let ring: Vec<String> = face
                .adjacent_edges()
                .map(|edge| coord_hex(edge.as_delaunay_edge().to().position()))
                .collect();
            format!(
                "{}{}",
                coord_hex(face.as_delaunay_vertex().position()),
                rotate_to_smallest(ring).concat()
            )
        })
        .collect();
    cells.sort_unstable();
    lines.extend(cells);
    lines
}

/// The inner dual vertices, keyed by their dual face's three corners. Each line
/// carries whether the vertex has a position — the only exactly determined thing
/// about a circumcentre — and the outgoing triple the dual reports.
fn voronoi_vertex_lines(triangulation: &Plain) -> Vec<String> {
    let mut lines: Vec<String> = triangulation
        .inner_faces()
        .map(|face| {
            let mut corners: Vec<String> = face.positions().into_iter().map(coord_hex).collect();
            corners.sort_unstable();
            let dual = VoronoiVertex::Inner(face);
            let existence = if dual.position().is_some() { "J" } else { "N" };
            let ring: Vec<String> = dual
                .out_edges()
                .map(|edges| edges.into_iter().map(directed_hex_of_dual).collect())
                .unwrap_or_default();
            format!(
                "{}{}{}",
                corners.concat(),
                existence,
                rotate_to_smallest(ring).concat()
            )
        })
        .collect();
    lines.sort_unstable();
    lines
}

fn directed_hex_of_dual(edge: DualEdge<'_>) -> String {
    directed_hex(edge.as_delaunay_edge())
}

fn directed_hex(edge: PrimalEdge<'_>) -> String {
    format!(
        "{}{}",
        coord_hex(edge.from().position()),
        coord_hex(edge.to().position())
    )
}

/// Every iterator over the mesh that moonlight also offers. The hull cycle and
/// each vertex's counterclockwise link are cycles with no determined starting
/// point and are rotated; the inner faces are a set and are sorted. The
/// enumerator lengths are stated because they are the only cross-comparable
/// claim the fixed and dynamic iterators make: their elements are handles, and
/// handles do not cross implementations.
fn write_dcel_walk_gate(directory: &Path, count: usize) {
    let triangulation: Plain =
        Plain::bulk_load(random_points(0x9e37_79b9_7f4a_7c15, count)).expect("bulk load");
    fs::write(
        directory.join(format!("dcel-hull-{count}.txt")),
        format!("{}\n", hull_gate_lines(&triangulation).join("\n")),
    )
    .expect("hull gate file");
    fs::write(
        directory.join(format!("dcel-circular-{count}.txt")),
        format!("{}\n", circular_gate_lines(&triangulation).join("\n")),
    )
    .expect("circular gate file");
    fs::write(
        directory.join(format!("dcel-faces-{count}.txt")),
        format!("{}\n", inner_face_gate_lines(&triangulation).join("\n")),
    )
    .expect("inner face gate file");
}

fn hull_gate_lines(triangulation: &Plain) -> Vec<String> {
    let ring: Vec<String> = triangulation.convex_hull().map(directed_hex).collect();
    let mut lines = vec![format!("hull-size {}", ring.len())];
    lines.extend(rotate_to_smallest(ring));
    lines
}

fn circular_gate_lines(triangulation: &Plain) -> Vec<String> {
    let mut lines = vec![
        format!("vertices {}", triangulation.fixed_vertices().count()),
        format!(
            "directed-edges {}",
            triangulation.fixed_directed_edges().count()
        ),
        format!(
            "undirected-edges {}",
            triangulation.fixed_undirected_edges().count()
        ),
        format!("all-faces {}", triangulation.fixed_all_faces().count()),
        format!("inner-faces {}", triangulation.fixed_inner_faces().count()),
    ];
    let mut links: Vec<String> = triangulation
        .vertices()
        .map(|vertex| {
            let ring: Vec<String> = vertex
                .out_edges()
                .map(|edge| coord_hex(edge.to().position()))
                .collect();
            format!(
                "{}{}",
                coord_hex(vertex.position()),
                rotate_to_smallest(ring).concat()
            )
        })
        .collect();
    links.sort_unstable();
    lines.extend(links);
    lines
}

fn inner_face_gate_lines(triangulation: &Plain) -> Vec<String> {
    let mut lines: Vec<String> = triangulation
        .inner_faces()
        .map(|face| {
            let mut corners: Vec<String> = face.positions().into_iter().map(coord_hex).collect();
            corners.sort_unstable();
            corners.concat()
        })
        .collect();
    lines.sort_unstable();
    lines
}

/// Query endpoints pulled inward. The generator fills [-1, 1]^2, so a point with
/// both coordinates inside [-0.5, 0.5] is strictly inside the hull of a few
/// thousand of them and locating it lands on a face. A chord that starts OUTSIDE
/// the hull makes endpoint choice and local hull entry observable, so those
/// chords remain the separate "intersection-outside" cliff lane.
fn interior_query_points(seed: u64, count: usize) -> Vec<Point2<f64>> {
    random_points(seed, count)
        .into_iter()
        .map(|point| Point2::new(0.5 * point.x, 0.5 * point.y))
        .collect()
}

/// Query origins pushed away from the dense centre so the corpus repeatedly
/// exercises outside-hull entry rather than only face-to-face corridors.
fn exterior_query_points(seed: u64, count: usize) -> Vec<Point2<f64>> {
    random_points(seed, count)
        .into_iter()
        .map(|point| Point2::new(4.0 * point.x, 4.0 * point.y))
        .collect()
}

fn intersection_chords(query_count: usize) -> Vec<(Point2<f64>, Point2<f64>)> {
    interior_query_points(0x3141_5926_5358_9793, query_count)
        .into_iter()
        .zip(interior_query_points(0x2718_2818_2845_9045, query_count))
        .collect()
}

fn outside_chords(query_count: usize) -> Vec<(Point2<f64>, Point2<f64>)> {
    exterior_query_points(0x243f_6a88_85a3_08d3, query_count)
        .into_iter()
        .zip(interior_query_points(0x2718_2818_2845_9045, query_count))
        .collect()
}

/// One line per query: the line's own endpoints, then its intersection sequence
/// in the order the walk reports it. Every branch of the walk turns on an
/// orientation predicate over a mesh that already agrees bit for bit, so the
/// whole sequence is exactly determined and crosses implementations — order
/// included, which is why nothing here is sorted.
fn write_intersection_gate(directory: &Path, point_count: usize, query_count: usize) {
    let triangulation: Plain =
        Plain::bulk_load(random_points(0x9e37_79b9_7f4a_7c15, point_count)).expect("bulk load");
    fs::write(
        directory.join(format!(
            "intersection-{point_count}-{query_count}-chords.txt"
        )),
        intersection_body(&triangulation, &intersection_chords(query_count)),
    )
    .expect("intersection chord gate file");
}

fn write_intersection_outside_gate(directory: &Path, point_count: usize, query_count: usize) {
    let triangulation: Plain =
        Plain::bulk_load(random_points(0x9e37_79b9_7f4a_7c15, point_count)).expect("bulk load");
    fs::write(
        directory.join(format!(
            "intersection-outside-{point_count}-{query_count}.txt"
        )),
        intersection_body(&triangulation, &outside_chords(query_count)),
    )
    .expect("intersection outside gate file");
}

const INTERSECTION_VERTEX_LINES: usize = 100;

/// Lines whose endpoints are mesh vertices, which is the only way to reach the
/// vertex and overlap branches of the walk at all: over generated doubles no
/// chord lands exactly on a site, so a gate built from chords alone exercises
/// one branch of three. The first family joins vertices the constraint
/// generator's index pairs select, which starts and ends the walk on a vertex;
/// the second runs along an existing edge, which is the only shape that produces
/// an overlap. Both name their endpoints by coordinate, the second from a list
/// both sides sort by the same encoding, because vertex handles do not cross.
fn write_intersection_vertex_gate(directory: &Path, point_count: usize) {
    let sites = random_points(0x9e37_79b9_7f4a_7c15, point_count);
    let triangulation: Plain = Plain::bulk_load(sites.clone()).expect("bulk load");
    let mut lines: Vec<(Point2<f64>, Point2<f64>)> =
        constraint_pairs(point_count, INTERSECTION_VERTEX_LINES)
            .into_iter()
            .map(|(from_index, to_index)| (sites[from_index], sites[to_index]))
            .collect();
    lines.extend(
        sorted_edge_endpoints(&triangulation)
            .into_iter()
            .take(INTERSECTION_VERTEX_LINES),
    );
    fs::write(
        directory.join(format!("intersection-vertexlines-{point_count}.txt")),
        intersection_body(&triangulation, &lines),
    )
    .expect("intersection vertex line gate file");
}

fn intersection_body(triangulation: &Plain, lines: &[(Point2<f64>, Point2<f64>)]) -> String {
    let body: Vec<String> = lines
        .iter()
        .map(|(from, to)| intersection_line(triangulation, *from, *to))
        .collect();
    format!("{}\n", body.join("\n"))
}

fn intersection_line(triangulation: &Plain, from: Point2<f64>, to: Point2<f64>) -> String {
    let events: String = LineIntersectionIterator::new(triangulation, from, to)
        .map(|event| encode_intersection(&event))
        .collect();
    format!("{}{}{}", coord_hex(from), coord_hex(to), events)
}

fn encode_intersection(event: &Intersection<'_, Point2<f64>, (), (), ()>) -> String {
    match event {
        Intersection::VertexIntersection(vertex) => format!("V{}", coord_hex(vertex.position())),
        Intersection::EdgeIntersection(edge) => format!("E{}", directed_hex(*edge)),
        Intersection::EdgeOverlap(edge) => format!("O{}", directed_hex(*edge)),
    }
}

fn sorted_edge_endpoints(triangulation: &Plain) -> Vec<(Point2<f64>, Point2<f64>)> {
    let mut keyed: Vec<(String, (Point2<f64>, Point2<f64>))> = triangulation
        .undirected_edges()
        .map(|edge| {
            let [a, b] = edge.positions();
            let (first, second) = if (a.x, a.y) <= (b.x, b.y) {
                (a, b)
            } else {
                (b, a)
            };
            (
                format!("{}{}", coord_hex(first), coord_hex(second)),
                (first, second),
            )
        })
        .collect();
    keyed.sort_by(|left, right| left.0.cmp(&right.0));
    keyed.into_iter().map(|(_, pair)| pair).collect()
}

/// The whole dual, walked the way spade's own documentation walks it: every dual
/// face, its adjacent dual edges, and the dual vertex each of those runs to.
fn voronoi_sweep(triangulation: &Plain) -> (f64, usize) {
    triangulation
        .voronoi_faces()
        .fold((0.0, 0), |accumulator, face| {
            face.adjacent_edges()
                .fold(accumulator, |(total, unbounded), edge| {
                    match edge.to().position() {
                        None => (total, unbounded + 1),
                        Some(position) => (total + position.x + position.y, unbounded),
                    }
                })
        })
}

/// Every traversal the DCEL offers, in one pass: the hull cycle, each vertex's
/// counterclockwise link, and each inner face's vertex triple.
fn dcel_walk(triangulation: &Plain) -> (f64, usize) {
    let hull = triangulation
        .convex_hull()
        .fold((0.0, 0usize), |accumulator, edge| {
            charge(accumulator, edge.from().position())
        });
    let links = triangulation.vertices().fold(hull, |accumulator, vertex| {
        vertex
            .out_edges()
            .fold(accumulator, |acc, edge| charge(acc, edge.to().position()))
    });
    triangulation
        .inner_faces()
        .fold(links, |accumulator, face| {
            face.positions().into_iter().fold(accumulator, charge)
        })
}

fn charge((total, count): (f64, usize), position: Point2<f64>) -> (f64, usize) {
    (total + position.x + position.y, count + 1)
}

fn intersection_walk(triangulation: &Plain, chords: &[(Point2<f64>, Point2<f64>)]) -> usize {
    chords
        .iter()
        .map(|(from, to)| LineIntersectionIterator::new(triangulation, *from, *to).count())
        .sum()
}

fn insert_all_incrementally(points: &[Point2<f64>]) -> Plain {
    points
        .iter()
        .copied()
        .fold(Plain::new(), |mut triangulation, point| {
            triangulation.insert(point).expect("insert");
            triangulation
        })
}

fn insert_all_into_hierarchy(points: &[Point2<f64>]) -> HierarchyTriangulation {
    points
        .iter()
        .copied()
        .fold(HierarchyTriangulation::new(), |mut triangulation, point| {
            triangulation.insert(point).expect("insert");
            triangulation
        })
}

/// Points already in the mesh, offered again. Neither side gains a vertex; the
/// lane is what each pays to find that out.
fn reinsert_all_into_hierarchy(
    mut triangulation: HierarchyTriangulation,
    points: &[Point2<f64>],
) -> HierarchyTriangulation {
    for point in points {
        triangulation.insert(*point).expect("insert");
    }
    triangulation
}

fn remove_all_from_hierarchy(
    mut triangulation: HierarchyTriangulation,
    removals: &[Point2<f64>],
) -> HierarchyTriangulation {
    for point in removals {
        triangulation.locate_and_remove(*point);
    }
    triangulation
}

/// One line per query: the coordinates of the nearest vertex the hierarchy
/// routed the search to, or `none`.
fn hierarchy_nearest_lines(
    triangulation: &HierarchyTriangulation,
    queries: &[Point2<f64>],
) -> String {
    let mut body: String = queries
        .iter()
        .map(|query| match triangulation.nearest_neighbor(*query) {
            Some(vertex) => {
                let found = vertex.position();
                format!("{:016x}{:016x}\n", found.x.to_bits(), found.y.to_bits())
            }
            None => "none\n".to_string(),
        })
        .collect();
    body.pop();
    body.push('\n');
    body
}

fn write_refinement_gate(directory: &Path, steiner_budget: usize) {
    let (vertices, edges) = refinement_input();
    let mut triangulation = Cdt::bulk_load_cdt(vertices, edges).expect("cdt bulk load");
    let original_vertex_count = triangulation.num_vertices();
    let outcome = triangulation.refine(
        RefinementParameters::<f64>::new()
            .with_max_additional_vertices(steiner_budget)
            .with_max_allowed_area(0.5)
            .with_angle_limit(AngleLimit::from_radius_to_shortest_edge_ratio(1.0))
            .exclude_outer_faces(true),
    );
    let prefix = format!("refine-{steiner_budget}");
    let completion = if outcome.refinement_complete { 1 } else { 0 };
    let summary = format!(
        "added {}\nvertices {}\nedges {}\ninner-faces {}\nexcluded {}\ncomplete {}\n",
        triangulation.num_vertices() - original_vertex_count,
        triangulation.num_vertices(),
        triangulation.num_undirected_edges(),
        triangulation.num_inner_faces(),
        outcome.excluded_faces.len(),
        completion,
    );
    fs::write(directory.join(format!("{prefix}-summary.txt")), summary)
        .expect("refinement summary gate file");
    fs::write(
        directory.join(format!("{prefix}-edges.txt")),
        format!("{}\n", canonical_edges(&triangulation).join("\n")),
    )
    .expect("refinement edge gate file");
    fs::write(
        directory.join(format!("{prefix}-constraints.txt")),
        format!(
            "{}\n",
            canonical_constraint_edges(&triangulation).join("\n")
        ),
    )
    .expect("refinement constraint gate file");
}

/// The shapes on which the two implementations classify the constrained domain.
/// They do not agree, so this is a characterization rather than a gate: the
/// committed baselines lock in the known divergence and fail if either side
/// moves. See `README.md` for the compatibility rule.
fn divergence_shapes() -> Vec<(&'static str, Vec<Point2<f64>>, Vec<[usize; 2]>)> {
    let square = |scale: f64| {
        vec![
            Point2::new(0.0, 0.0),
            Point2::new(scale, 0.0),
            Point2::new(scale, scale),
            Point2::new(0.0, scale),
        ]
    };
    let loop4 = vec![[0, 1], [1, 2], [2, 3], [3, 0]];
    vec![
        (
            "flush",
            {
                let mut points = square(8.0);
                points.push(Point2::new(4.0, 4.0));
                points
            },
            loop4.clone(),
        ),
        (
            "notched",
            {
                let mut points = square(8.0);
                points.push(Point2::new(13.0, 4.0));
                points.push(Point2::new(4.0, 4.0));
                points
            },
            loop4.clone(),
        ),
        (
            "flush-plus-dangling-segment",
            {
                let mut points = square(8.0);
                points.push(Point2::new(2.0, 2.0));
                points.push(Point2::new(6.0, 6.0));
                points
            },
            {
                let mut edges = loop4.clone();
                edges.push([4, 5]);
                edges
            },
        ),
        (
            "notched-plus-dangling-segment",
            {
                let mut points = square(8.0);
                points.push(Point2::new(13.0, 4.0));
                points.push(Point2::new(2.0, 2.0));
                points.push(Point2::new(6.0, 6.0));
                points
            },
            {
                let mut edges = loop4.clone();
                edges.push([5, 6]);
                edges
            },
        ),
        (
            "annulus",
            {
                let mut points = square(12.0);
                points.push(Point2::new(4.0, 4.0));
                points.push(Point2::new(8.0, 4.0));
                points.push(Point2::new(8.0, 8.0));
                points.push(Point2::new(4.0, 8.0));
                points
            },
            {
                let mut edges = loop4.clone();
                edges.extend_from_slice(&[[4, 5], [5, 6], [6, 7], [7, 4]]);
                edges
            },
        ),
    ]
}

fn write_divergence(path: &Path) {
    let mut lines = Vec::new();
    for (label, vertices, edges) in divergence_shapes() {
        let mut cdt = Cdt::bulk_load_cdt(vertices, edges).expect("cdt bulk load");
        let inner = cdt.inner_faces().count();
        // Budget zero breaks the refinement loop on entry, so `excluded_faces`
        // is the classification of the unrefined triangulation — the thing the
        // other implementation can be asked for directly.
        let excluded = cdt
            .refine(
                RefinementParameters::<f64>::new()
                    .with_max_additional_vertices(0)
                    .exclude_outer_faces(true),
            )
            .excluded_faces
            .len();
        lines.push(format!("{label}: inner {inner}, excluded {excluded}"));
    }
    lines.push(String::new());
    fs::write(path, lines.join("\n")).expect("divergence file");
}

fn constraint_pairs(point_count: usize, constraint_count: usize) -> Vec<(usize, usize)> {
    (0..)
        .map(|index: usize| {
            (
                index % point_count,
                (index * 6151 + point_count / 2) % point_count,
            )
        })
        .filter(|(a, b)| a != b)
        .take(constraint_count)
        .collect()
}

fn locate_vertex_handles(cdt: &Cdt, points: &[Point2<f64>]) -> Vec<FixedVertexHandle> {
    points
        .iter()
        .map(|point| match cdt.locate(*point) {
            PositionInTriangulation::OnVertex(handle) => handle,
            other => panic!("input point is not a vertex: {other:?}"),
        })
        .collect()
}

/// The caller owns the CDT the program edits, so the build that resets it is
/// the caller's to place; nothing here is charged for it.
fn recover_constraint_program(
    base: Cdt,
    handles: &[FixedVertexHandle],
    pairs: &[(usize, usize)],
) -> (Cdt, Vec<usize>) {
    pairs.iter().enumerate().fold(
        (base, Vec::new()),
        |(mut cdt, mut accepted), (index, (from_index, to_index))| {
            let (from, to) = (handles[*from_index], handles[*to_index]);
            if cdt.can_add_constraint(from, to) {
                cdt.add_constraint(from, to);
                accepted.push(index);
            }
            (cdt, accepted)
        },
    )
}

/// The same program the batch recovery runs, one request at a time through the
/// singleton entry point. `add_constraint_edge` inserts both endpoints before
/// recovering, matching the Haskell side's entry point, which does the same;
/// the guard keeps a blocked request a refusal rather than a panic, matching
/// the Haskell side's refusal.
fn add_constraint_edge_program(
    base: Cdt,
    points: &[Point2<f64>],
    handles: &[FixedVertexHandle],
    pairs: &[(usize, usize)],
) -> (Cdt, Vec<usize>) {
    pairs.iter().enumerate().fold(
        (base, Vec::new()),
        |(mut cdt, mut accepted), (index, (from_index, to_index))| {
            if cdt.can_add_constraint(handles[*from_index], handles[*to_index]) {
                cdt.add_constraint_edge(points[*from_index], points[*to_index])
                    .expect("constraint edge");
                accepted.push(index);
            }
            (cdt, accepted)
        },
    )
}

/// Splitting requests, one per band. Every crossed constraint is split at the
/// intersection rather than refused, so no guard is needed and every request is
/// accepted. Splitting appends vertices and never removes one, so the handles
/// resolved before the first request stay valid through the last.
fn split_constraint_program(
    base: Cdt,
    handles: &[FixedVertexHandle],
    crossings: &[(usize, usize)],
) -> Cdt {
    crossings
        .iter()
        .fold(base, |mut cdt, (from_index, to_index)| {
            cdt.add_constraint_and_split(handles[*from_index], handles[*to_index], |vertex| vertex);
            cdt
        })
}

/// A closed square with no dangling constraint. The interior segment used by the
/// package's own benchmark is outside spade's documented "closed shape"
/// contract and makes it refuse the whole domain — not a workload the two can be
/// timed on.
fn refinement_input() -> (Vec<Point2<f64>>, Vec<[usize; 2]>) {
    (
        vec![
            Point2::new(0.0, 0.0),
            Point2::new(64.0, 0.0),
            Point2::new(64.0, 64.0),
            Point2::new(0.0, 64.0),
        ],
        vec![[0, 1], [1, 2], [2, 3], [3, 0]],
    )
}

/// Everything each lane needs before the clock starts. Handle resolution and
/// input generation are setup, not work.
fn run_lane(lane: &str, first: usize, second: usize) {
    match lane {
        "bulk-load" => {
            let points = random_points(0x9e37_79b9_7f4a_7c15, first);
            measure(lane, first, second, move || {
                Plain::bulk_load(points).expect("bulk load").num_vertices()
            });
        }
        "incremental" => {
            let points = random_points(0x9e37_79b9_7f4a_7c15, first);
            measure(lane, first, second, move || {
                insert_all_incrementally(&points).num_vertices()
            });
        }
        // The angular index both bulk loaders open with, given one angle to
        // work with. The input stays in general position, so the load runs the
        // ordinary face-building path from the third point on and every
        // insertion locates by walking; what degrades is the hull lookup that
        // walk starts from, whose buckets all collapse into one. This lane is
        // that degradation and nothing else — the face-less path it does NOT
        // reach is "degenerate-line" below.
        "sweep-angle-collapse" => {
            let points = near_collinear_points(0x9e37_79b9_7f4a_7c15, first);
            measure(lane, first, second, move || {
                Plain::bulk_load(points).expect("bulk load").num_vertices()
            });
        }
        // Exactly collinear input never builds a face. Spade collects and sorts
        // all vertices per insertion (`locate_when_all_vertices_on_line`), while
        // Moonlight now glues the complete admitted line section once.
        "degenerate-line" => {
            let points = exactly_collinear_points(first);
            measure(lane, first, second, move || {
                Plain::bulk_load(points).expect("bulk load").num_vertices()
            });
        }
        "nearest" => {
            let triangulation: HierarchyTriangulation =
                HierarchyTriangulation::bulk_load(random_points(0x0123_4567_89ab_cdef, first))
                    .expect("bulk load");
            let queries = random_points(0x3141_5926_5358_9793, second);
            // Summed rather than counted, to match what the Moonlight side is forced
            // to do to be honest: `is_some` there can be satisfied by a thunk.
            measure(lane, first, second, || {
                queries
                    .iter()
                    .map(|query| match triangulation.nearest_neighbor(*query) {
                        Some(vertex) => vertex.fix().index() as u64,
                        None => 0,
                    })
                    .sum::<u64>()
            });
        }
        "cdt-recovery" => {
            let points = random_points(0x94d0_49bb_1331_11eb, first);
            let pairs = constraint_pairs(first, second);
            let base: Cdt = Cdt::bulk_load(points.clone()).expect("bulk load");
            let handles = locate_vertex_handles(&base, &points);
            // The Haskell side uses immutable state, while this side uses a
            // mutable reset; the base it consumes is built here, before the
            // clock, so both sides time the recovery program and nothing else.
            measure(lane, first, second, move || {
                recover_constraint_program(base, &handles, &pairs).1.len()
            });
        }
        // The same constraint program, requested one edge at a time through the
        // singleton entry point instead of as a batch. Guarded so a blocked
        // request is refused rather than fatal, which is what the Haskell
        // side's refusal does.
        "constraint-incremental" => {
            let points = random_points(0x94d0_49bb_1331_11eb, first);
            let pairs = constraint_pairs(first, second);
            let base: Cdt = Cdt::bulk_load(points.clone()).expect("bulk load");
            let handles = locate_vertex_handles(&base, &points);
            measure(lane, first, second, move || {
                add_constraint_edge_program(base, &points, &handles, &pairs)
                    .1
                    .len()
            });
        }
        "constraint-split" => {
            let (vertices, edges, crossings) = split_bands(first);
            let base: Cdt = Cdt::bulk_load_cdt(vertices.clone(), edges).expect("cdt bulk load");
            let handles = locate_vertex_handles(&base, &vertices);
            measure(lane, first, second, move || {
                split_constraint_program(base, &handles, &crossings).num_vertices()
            });
        }
        // In-place removal, by coordinate, against the same input prefix on
        // both sides. The build is setup on both sides; the measured region is
        // the removals alone.
        "removal" => {
            let points = random_points(0x9e37_79b9_7f4a_7c15, first);
            let base: Plain = Plain::bulk_load(points.clone()).expect("bulk load");
            let removals = points[..second].to_vec();
            measure(lane, first, second, move || {
                remove_all_by_coordinate(base, &removals).num_vertices()
            });
        }
        // The hierarchy lanes. spade's hint generator maintains itself from
        // inside the triangulation through `notify_vertex_inserted` and
        // `notify_vertex_removed`, so an insert or a removal here is the whole
        // maintenance cost and nothing is hoisted out of it.
        "hierarchy-incremental" => {
            let points = random_points(0x9e37_79b9_7f4a_7c15, first);
            measure(lane, first, second, move || {
                insert_all_into_hierarchy(&points).num_vertices()
            });
        }
        "hierarchy-duplicate" => {
            let base: HierarchyTriangulation =
                HierarchyTriangulation::bulk_load(random_points(0x9e37_79b9_7f4a_7c15, first))
                    .expect("bulk load");
            let duplicates = random_points(0x9e37_79b9_7f4a_7c15, second);
            measure(lane, first, second, move || {
                reinsert_all_into_hierarchy(base, &duplicates).num_vertices()
            });
        }
        "hierarchy-removal" => {
            let points = random_points(0x9e37_79b9_7f4a_7c15, first);
            let base: HierarchyTriangulation =
                HierarchyTriangulation::bulk_load(points.clone()).expect("bulk load");
            let removals = points[..second].to_vec();
            measure(lane, first, second, move || {
                remove_all_from_hierarchy(base, &removals).num_vertices()
            });
        }
        // Sibson weights through the NaturalNeighbor handle's reusable buffers;
        // the Haskell side holds the same buffers in an explicit workspace.
        // The sum forces every weight, the way the nearest lane's index sum
        // forces every search.
        "interpolation" => {
            let triangulation =
                Plain::bulk_load(random_points(0x0123_4567_89ab_cdef, first)).expect("bulk load");
            let natural_neighbor = triangulation.natural_neighbor();
            let queries = random_points(0x2718_2818_2845_9045, second);
            let mut buffer: Vec<(FixedVertexHandle, f64)> = Vec::new();
            measure(lane, first, second, || {
                queries
                    .iter()
                    .map(|query| {
                        natural_neighbor.get_weights(*query, &mut buffer);
                        buffer.iter().map(|entry| entry.1).sum::<f64>()
                    })
                    .sum::<f64>()
                    .to_bits()
            });
        }
        // The dual. Both sides hold it as a view of the primal mesh rather than
        // as a second structure, so the mesh is setup on both and the sweep
        // alone is timed.
        "voronoi-sweep" => {
            let triangulation: Plain =
                Plain::bulk_load(random_points(0x9e37_79b9_7f4a_7c15, first)).expect("bulk load");
            measure(lane, first, second, move || voronoi_sweep(&triangulation));
        }
        "dcel-walk" => {
            let triangulation: Plain =
                Plain::bulk_load(random_points(0x9e37_79b9_7f4a_7c15, first)).expect("bulk load");
            measure(lane, first, second, move || dcel_walk(&triangulation));
        }
        // Full Spade ecosystem referent: alphalpha constructs the Delaunay
        // filtration, this harness lowers it in lawful global birth order,
        // LoPHAT reduces once, and every critical-birth Betti profile is read.
        #[cfg(feature = "alpha-persistence")]
        "alpha-persistence" => alpha_persistence_support::bench_one(first, second),
        // Corridor walks between interior endpoints. Both sides locate the
        // start on a face and step edge to edge from there.
        "intersection" => {
            let triangulation: Plain =
                Plain::bulk_load(random_points(0x9e37_79b9_7f4a_7c15, first)).expect("bulk load");
            let chords = intersection_chords(second);
            measure(lane, first, second, move || {
                intersection_walk(&triangulation, &chords)
            });
        }
        // The same walk from outside the hull, retaining endpoint selection and
        // local hull entry as an explicit cliff lane on both sides.
        "intersection-outside" => {
            let triangulation: Plain =
                Plain::bulk_load(random_points(0x9e37_79b9_7f4a_7c15, first)).expect("bulk load");
            let chords = outside_chords(second);
            measure(lane, first, second, move || {
                intersection_walk(&triangulation, &chords)
            });
        }
        // The constrained domain is setup, exactly as it is on the Haskell
        // side; only Ruppert's algorithm runs against the clock.
        "refine" => {
            let (vertices, edges) = refinement_input();
            let mut cdt = Cdt::bulk_load_cdt(vertices, edges).expect("cdt bulk load");
            measure(lane, first, second, move || {
                let before = cdt.num_vertices();
                cdt.refine(
                    RefinementParameters::<f64>::new()
                        .with_max_additional_vertices(first)
                        .with_max_allowed_area(0.5)
                        .with_angle_limit(AngleLimit::from_radius_to_shortest_edge_ratio(1.0))
                        .exclude_outer_faces(true),
                );
                cdt.num_vertices() - before
            });
        }
        other => panic!("unknown lane: {other}"),
    }
}

/// Exactly one timed iteration, matching the Haskell half. This side could
/// repeat safely — a closure re-runs where a thunk does not — but a lane timed
/// a hundred times here against once there is not the same measurement, and the
/// asymmetry would land silently in every ratio. The samples come from the
/// driver's rounds instead, and the scorecard reports their median.
///
/// `FnOnce` rather than `FnMut` for that same one-iteration reason, and because
/// it is what lets a lane hand over the mesh it consumes. A mutable structure
/// needs a fresh copy per run; taking the closure by value means that copy is
/// built before the clock and moved in, rather than cloned inside the timed
/// region where it would be charged as algorithm cost.
fn measure<T>(lane: &str, first: usize, second: usize, work: impl FnOnce() -> T) {
    let start = Instant::now();
    let value = work();
    core::hint::black_box(&value);
    println!("{lane}-{first}-{second},{}", start.elapsed().as_nanos());
}

fn main() {
    let arguments: Vec<String> = std::env::args().collect();
    let tail: Vec<&str> = arguments.iter().skip(1).map(String::as_str).collect();
    match tail.as_slice() {
        ["gate", directory] => write_gate(Path::new(directory)),
        ["divergence", path] => write_divergence(Path::new(path)),
        #[cfg(feature = "alpha-persistence")]
        ["alpha-receipt", count] => alpha_persistence_support::report(count),
        ["bench-one", lane, first, second] => run_lane(
            lane,
            first.parse().expect("first argument"),
            second.parse().expect("second argument"),
        ),
        _ => {
            eprintln!("usage: spade-referent-rs (gate DIR | divergence FILE | alpha-receipt COUNT | bench-one LANE A B)");
            std::process::exit(2);
        }
    }
}
