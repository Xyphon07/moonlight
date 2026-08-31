//! Rust referent boundary for Moonlight's Delaunay construction comparison.
//!
//! Fixture choice, benchmark grouping, agreement, timing, and reporting belong
//! to Haskell. This library retains only the operations that cannot move there:
//! Spade's exact seeded generator and direct calls into the Rust crates under
//! comparison.

use core::ffi::c_void;
use core::hint::black_box;
use core::ptr;
use rand::distr::{Distribution, Uniform};
use rand::rngs::StdRng;
use rand::SeedableRng;
use spade::{HierarchyHintGenerator, Point2, Triangulation};
use std::panic::{catch_unwind, AssertUnwindSafe};

const UPSTREAM_SEED: [u8; 32] = [
    0xfb, 0xdc, 0x4e, 0xa0, 0x30, 0xde, 0x82, 0xba, 0x69, 0x97, 0x3c, 0x52, 0x49, 0x4d, 0x00, 0xca,
    0x0a, 0x5c, 0x21, 0xa3, 0x8d, 0x5c, 0xf2, 0x34, 0x4e, 0x58, 0x7d, 0x80, 0x16, 0x66, 0x23, 0x30,
];

const UNIFORM_RANGE: f64 = 1.0e9;
const LOCAL_STEP_RANGE: f64 = 1.0;

type PlainSpade = spade::DelaunayTriangulation<Point2<f64>>;
type HierarchySpade =
    spade::DelaunayTriangulation<Point2<f64>, (), (), (), HierarchyHintGenerator<f64>>;

#[repr(i32)]
#[derive(Clone, Copy, Debug)]
enum BoundaryStatus {
    Success = 0,
    NullPointer = 1,
    UnknownTag = 2,
    CoordinateCountMismatch = 3,
    DistributionConstructionFailed = 4,
    TriangulationFailed = 5,
    Panicked = 6,
}

impl BoundaryStatus {
    const fn code(self) -> i32 {
        self as i32
    }
}

#[derive(Clone, Copy)]
enum PointDistribution {
    LocalInsertion,
    Uniform,
}

impl TryFrom<i32> for PointDistribution {
    type Error = BoundaryStatus;

    fn try_from(tag: i32) -> Result<Self, Self::Error> {
        match tag {
            0 => Ok(Self::LocalInsertion),
            1 => Ok(Self::Uniform),
            _ => Err(BoundaryStatus::UnknownTag),
        }
    }
}

#[derive(Clone, Copy)]
enum NativeImplementation {
    Spade,
    SpadeHierarchy,
    Cdt,
    Delaunator,
}

impl TryFrom<i32> for NativeImplementation {
    type Error = BoundaryStatus;

    fn try_from(tag: i32) -> Result<Self, Self::Error> {
        match tag {
            0 => Ok(Self::Spade),
            1 => Ok(Self::SpadeHierarchy),
            2 => Ok(Self::Cdt),
            3 => Ok(Self::Delaunator),
            _ => Err(BoundaryStatus::UnknownTag),
        }
    }
}

enum PreparedImplementation {
    Spade(Vec<Point2<f64>>),
    SpadeHierarchy(Vec<Point2<f64>>),
    Cdt(Vec<(f64, f64)>),
    Delaunator(Vec<delaunator::Point>),
}

#[derive(Clone, Copy)]
struct ConstructionSummary {
    vertices: usize,
    triangles: usize,
}

impl PreparedImplementation {
    fn from_coordinates(implementation: NativeImplementation, coordinates: &[[f64; 2]]) -> Self {
        match implementation {
            NativeImplementation::Spade => {
                Self::Spade(coordinates.iter().copied().map(Point2::from).collect())
            }
            NativeImplementation::SpadeHierarchy => {
                Self::SpadeHierarchy(coordinates.iter().copied().map(Point2::from).collect())
            }
            NativeImplementation::Cdt => {
                Self::Cdt(coordinates.iter().map(|[x, y]| (*x, *y)).collect())
            }
            NativeImplementation::Delaunator => Self::Delaunator(
                coordinates
                    .iter()
                    .map(|[x, y]| delaunator::Point { x: *x, y: *y })
                    .collect(),
            ),
        }
    }

    fn run(&self) -> Result<ConstructionSummary, BoundaryStatus> {
        match self {
            Self::Spade(vertices) => {
                let triangulation = PlainSpade::bulk_load(vertices.clone())
                    .map_err(|_| BoundaryStatus::TriangulationFailed)?;
                let summary = ConstructionSummary {
                    vertices: triangulation.num_vertices(),
                    triangles: triangulation.num_inner_faces(),
                };
                black_box(triangulation);
                Ok(summary)
            }
            Self::SpadeHierarchy(vertices) => {
                let triangulation = HierarchySpade::bulk_load(vertices.clone())
                    .map_err(|_| BoundaryStatus::TriangulationFailed)?;
                let summary = ConstructionSummary {
                    vertices: triangulation.num_vertices(),
                    triangles: triangulation.num_inner_faces(),
                };
                black_box(triangulation);
                Ok(summary)
            }
            Self::Cdt(vertices) => {
                let triangles = cdt::triangulate_points(vertices)
                    .map_err(|_| BoundaryStatus::TriangulationFailed)?;
                let summary = ConstructionSummary {
                    vertices: vertices.len(),
                    triangles: triangles.len(),
                };
                black_box(triangles);
                Ok(summary)
            }
            Self::Delaunator(vertices) => {
                let triangulation = delaunator::triangulate(vertices);
                let summary = ConstructionSummary {
                    vertices: vertices.len(),
                    triangles: triangulation.triangles.len() / 3,
                };
                black_box(triangulation);
                Ok(summary)
            }
        }
    }
}

fn boundary_status(action: impl FnOnce() -> Result<(), BoundaryStatus>) -> i32 {
    match catch_unwind(AssertUnwindSafe(action)) {
        Ok(Ok(())) => BoundaryStatus::Success.code(),
        Ok(Err(status)) => status.code(),
        Err(_) => BoundaryStatus::Panicked.code(),
    }
}

fn coordinate_count(point_count: usize) -> Result<usize, BoundaryStatus> {
    point_count
        .checked_mul(2)
        .ok_or(BoundaryStatus::CoordinateCountMismatch)
}

fn uniform_samples(point_count: usize) -> Result<impl Iterator<Item = [f64; 2]>, BoundaryStatus> {
    let distribution = Uniform::new_inclusive(-UNIFORM_RANGE, UNIFORM_RANGE)
        .map_err(|_| BoundaryStatus::DistributionConstructionFailed)?;
    let mut generator = StdRng::from_seed(UPSTREAM_SEED);
    Ok(core::iter::from_fn(move || {
        Some([
            distribution.sample(&mut generator),
            distribution.sample(&mut generator),
        ])
    })
    .take(point_count))
}

fn local_insertion_samples(
    point_count: usize,
) -> Result<impl Iterator<Item = [f64; 2]>, BoundaryStatus> {
    let distribution = Uniform::new_inclusive(-LOCAL_STEP_RANGE, LOCAL_STEP_RANGE)
        .map_err(|_| BoundaryStatus::DistributionConstructionFailed)?;
    let mut generator = StdRng::from_seed(UPSTREAM_SEED);
    let mut previous = [0.0, 1.0];
    Ok(core::iter::from_fn(move || {
        previous = [
            previous[0] + distribution.sample(&mut generator),
            previous[1] + distribution.sample(&mut generator),
        ];
        Some(previous)
    })
    .take(point_count))
}

fn write_coordinates(
    distribution: PointDistribution,
    point_count: usize,
    output: &mut [f64],
) -> Result<(), BoundaryStatus> {
    let expected_count = coordinate_count(point_count)?;
    if output.len() != expected_count {
        return Err(BoundaryStatus::CoordinateCountMismatch);
    }

    let samples: Box<dyn Iterator<Item = [f64; 2]>> = match distribution {
        PointDistribution::LocalInsertion => Box::new(local_insertion_samples(point_count)?),
        PointDistribution::Uniform => Box::new(uniform_samples(point_count)?),
    };

    output
        .chunks_exact_mut(2)
        .zip(samples)
        .for_each(|(coordinate_pair, [x, y])| {
            coordinate_pair.copy_from_slice(&[x, y]);
        });
    Ok(())
}

/// Fill `coordinates` with Spade's exact seeded benchmark distribution.
///
/// # Safety
///
/// A non-null `coordinates` pointer must be writable for `coordinate_count`
/// consecutive `f64` values.
#[no_mangle]
pub unsafe extern "C" fn delaunay_compare_generate(
    distribution_tag: i32,
    point_count: usize,
    coordinates: *mut f64,
    supplied_coordinate_count: usize,
) -> i32 {
    boundary_status(|| {
        let distribution = PointDistribution::try_from(distribution_tag)?;
        let expected_count = coordinate_count(point_count)?;
        if supplied_coordinate_count != expected_count {
            return Err(BoundaryStatus::CoordinateCountMismatch);
        }
        if coordinates.is_null() && supplied_coordinate_count != 0 {
            return Err(BoundaryStatus::NullPointer);
        }
        let output = if supplied_coordinate_count == 0 {
            &mut []
        } else {
            // SAFETY: the caller contract and checks above establish a non-null
            // writable region of exactly `supplied_coordinate_count` values.
            unsafe { core::slice::from_raw_parts_mut(coordinates, supplied_coordinate_count) }
        };
        write_coordinates(distribution, point_count, output)
    })
}

/// Prepare the implementation-specific native input outside the timed region.
///
/// # Safety
///
/// A non-null `coordinates` pointer must be readable for `2 * point_count`
/// consecutive `f64` values. `prepared_output` must be writable for one pointer.
#[no_mangle]
pub unsafe extern "C" fn delaunay_compare_prepare(
    implementation_tag: i32,
    coordinates: *const f64,
    point_count: usize,
    prepared_output: *mut *mut c_void,
) -> i32 {
    boundary_status(|| {
        let implementation = NativeImplementation::try_from(implementation_tag)?;
        if prepared_output.is_null() {
            return Err(BoundaryStatus::NullPointer);
        }
        let supplied_coordinate_count = coordinate_count(point_count)?;
        if coordinates.is_null() && supplied_coordinate_count != 0 {
            return Err(BoundaryStatus::NullPointer);
        }
        let flat_coordinates = if supplied_coordinate_count == 0 {
            &[]
        } else {
            // SAFETY: the caller contract and checks above establish a non-null
            // readable region of exactly `supplied_coordinate_count` values.
            unsafe { core::slice::from_raw_parts(coordinates, supplied_coordinate_count) }
        };
        let coordinate_pairs = flat_coordinates.as_chunks::<2>().0;
        let prepared = PreparedImplementation::from_coordinates(implementation, coordinate_pairs);
        // SAFETY: `prepared_output` was checked non-null and points to writable
        // storage for one opaque pointer by the caller contract.
        unsafe {
            ptr::write(prepared_output, Box::into_raw(Box::new(prepared)).cast());
        }
        Ok(())
    })
}

/// Construct one triangulation from a prepared native input.
///
/// # Safety
///
/// `prepared` must be a live pointer returned by `delaunay_compare_prepare`.
/// Both output pointers must be writable for one `usize`.
#[no_mangle]
pub unsafe extern "C" fn delaunay_compare_run(
    prepared: *const c_void,
    vertex_count_output: *mut usize,
    triangle_count_output: *mut usize,
) -> i32 {
    boundary_status(|| {
        if prepared.is_null() || vertex_count_output.is_null() || triangle_count_output.is_null() {
            return Err(BoundaryStatus::NullPointer);
        }
        // SAFETY: the caller contract requires a live pointer produced by the
        // matching prepare function and keeps it alive for this call.
        let prepared_implementation = unsafe { &*prepared.cast::<PreparedImplementation>() };
        let summary = prepared_implementation.run()?;
        // SAFETY: both output pointers were checked non-null and the caller
        // contract gives writable storage for one value at each address.
        unsafe {
            ptr::write(vertex_count_output, summary.vertices);
            ptr::write(triangle_count_output, summary.triangles);
        }
        Ok(())
    })
}

/// Release one prepared native input. A null pointer is a no-op.
///
/// # Safety
///
/// A non-null pointer must have been returned by `delaunay_compare_prepare`
/// and must not have been released before.
#[no_mangle]
pub unsafe extern "C" fn delaunay_compare_release(prepared: *mut c_void) {
    if !prepared.is_null() {
        // SAFETY: the caller contract transfers the one remaining ownership of
        // the allocation produced by `Box::into_raw` in the prepare function.
        unsafe {
            drop(Box::from_raw(prepared.cast::<PreparedImplementation>()));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{local_insertion_samples, uniform_samples};

    #[test]
    fn local_insertion_prefix_matches_upstream_fixture_bits() {
        let observed = local_insertion_samples(3)
            .expect("the fixed local-insertion distribution is valid")
            .flatten()
            .map(f64::to_bits)
            .collect::<Vec<_>>();
        assert_eq!(
            observed,
            [
                0xbfe28b04856ac6db,
                0x3ff8c718b563a19a,
                0x3fc014ab09d22a14,
                0x4003a1ee45dc4712,
                0xbfd097c41d82adc4,
                0x4008d420538d5c3d,
            ]
        );
    }

    #[test]
    fn uniform_prefix_matches_upstream_fixture_bits() {
        let observed = uniform_samples(3)
            .expect("the fixed uniform distribution is valid")
            .flatten()
            .map(f64::to_bits)
            .collect::<Vec<_>>();
        assert_eq!(
            observed,
            [
                0xc1c1450134a5bd5a,
                0x41c0598b1e249a72,
                0x41c5037dbf1c0b38,
                0x41cafc1cf53845c8,
                0xc1b6f1036f0b1386,
                0x41c35b5d4df8b356,
            ]
        );
    }
}
