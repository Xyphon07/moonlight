use std::marker::PhantomData;
use std::ptr::NonNull;
use std::rc::Rc;

use crate::error::{
    MoonlightError, WireVocabulary, invalid_native_result, projection_shape_error, status_result,
    success_without_handle_error,
};
use crate::raw_generated::{
    ABI_VERSION, ML_MINKOWSKI_ADDITION, ML_MINKOWSKI_CLOSING, ML_MINKOWSKI_EROSION,
    ML_MINKOWSKI_OPENING, ML_OBSTRUCTION_BUFFER_TOO_SMALL, ML_REGION_BOUNDARY, ML_REGION_EXTERIOR,
    ML_REGION_INTERIOR, ML_STATUS_BUFFER_TOO_SMALL, ML_STATUS_OK, NativeMesh,
    NativeMinkowskiReceipt, NativeObstruction, NativeRegion, NativeStructuringElement,
    ml_abi_version, ml_delaunay_f64, ml_mesh_copy_triangles_u32, ml_mesh_copy_vertices_f64,
    ml_mesh_free, ml_mesh_insert_many_f64, ml_mesh_site_difference, ml_mesh_site_intersection,
    ml_mesh_site_symmetric_difference, ml_mesh_site_union, ml_mesh_triangle_count,
    ml_mesh_vertex_count, ml_region_close, ml_region_copy_f64, ml_region_counts,
    ml_region_create_f64, ml_region_difference, ml_region_free, ml_region_inset,
    ml_region_intersection, ml_region_locate_point_f64, ml_region_measure, ml_region_minkowski_sum,
    ml_region_offset, ml_region_open, ml_region_symmetric_difference, ml_region_union,
    ml_runtime_initialize, ml_structuring_element_create_f64, ml_structuring_element_free,
};
use crate::values::{
    ExactRational, MinkowskiOperation, MinkowskiReceipt, PolygonComponent, RegionLocation,
    RegionValuations,
};

pub struct Moonlight;

impl Moonlight {
    pub fn initialize() -> Result<Self, MoonlightError> {
        let status = unsafe { ml_runtime_initialize() };
        if status != ML_STATUS_OK {
            return Err(MoonlightError::RuntimeInitializationRefused { status });
        }
        let version = unsafe { ml_abi_version() };
        if version != ABI_VERSION {
            return Err(MoonlightError::AbiVersionMismatch {
                expected: ABI_VERSION,
                observed: version,
            });
        }
        Ok(Self)
    }

    pub fn delaunay(&self, points: &[[f64; 2]]) -> Result<Mesh, MoonlightError> {
        let coordinates = flatten_points(points);
        create_handle(|output, obstruction| unsafe {
            ml_delaunay_f64(coordinates.as_ptr(), points.len(), output, obstruction)
        })
        .map(Mesh::from_handle)
    }

    pub fn region(&self, components: &[PolygonComponent]) -> Result<Region, MoonlightError> {
        let loop_point_counts = components
            .iter()
            .flat_map(|component| std::iter::once(&component.outer).chain(component.holes.iter()))
            .map(Vec::len)
            .collect::<Vec<_>>();
        let component_loop_counts = components
            .iter()
            .map(|component| component.holes.len() + 1)
            .collect::<Vec<_>>();
        let coordinates = components
            .iter()
            .flat_map(|component| std::iter::once(&component.outer).chain(component.holes.iter()))
            .flat_map(|loop_points| loop_points.iter())
            .flat_map(|[x, y]| [*x, *y])
            .collect::<Vec<_>>();
        create_handle(|output, obstruction| unsafe {
            ml_region_create_f64(
                coordinates.as_ptr(),
                coordinates.len() / 2,
                loop_point_counts.as_ptr(),
                loop_point_counts.len(),
                component_loop_counts.as_ptr(),
                components.len(),
                output,
                obstruction,
            )
        })
        .map(Region::from_handle)
    }

    pub fn structuring_element(
        &self,
        points: &[[f64; 2]],
    ) -> Result<StructuringElement, MoonlightError> {
        let coordinates = flatten_points(points);
        create_handle(|output, obstruction| unsafe {
            ml_structuring_element_create_f64(
                coordinates.as_ptr(),
                points.len(),
                output,
                obstruction,
            )
        })
        .map(StructuringElement::from_handle)
    }
}

pub struct Mesh {
    handle: NonNull<NativeMesh>,
    _thread_affinity: PhantomData<Rc<()>>,
}

impl Mesh {
    fn from_handle(handle: NonNull<NativeMesh>) -> Self {
        Self {
            handle,
            _thread_affinity: PhantomData,
        }
    }

    pub fn vertex_count(&self) -> Result<usize, MoonlightError> {
        self.count(|count, obstruction| unsafe {
            ml_mesh_vertex_count(self.handle.as_ptr(), count, obstruction)
        })
    }

    pub fn triangle_count(&self) -> Result<usize, MoonlightError> {
        self.count(|count, obstruction| unsafe {
            ml_mesh_triangle_count(self.handle.as_ptr(), count, obstruction)
        })
    }

    pub fn vertices(&self) -> Result<Vec<[f64; 2]>, MoonlightError> {
        let count = self.vertex_count()?;
        let mut coordinates = vec![0.0; count * 2];
        let mut written = 0;
        let mut obstruction = NativeObstruction::default();
        let status = unsafe {
            ml_mesh_copy_vertices_f64(
                self.handle.as_ptr(),
                coordinates.as_mut_ptr(),
                count,
                &mut written,
                &mut obstruction,
            )
        };
        status_result(status, obstruction)?;
        coordinates.truncate(written * 2);
        coordinate_pairs(&coordinates)
    }

    pub fn triangles(&self) -> Result<Vec<[u32; 3]>, MoonlightError> {
        let count = self.triangle_count()?;
        let mut triangles = vec![0; count * 3];
        let mut written = 0;
        let mut obstruction = NativeObstruction::default();
        let status = unsafe {
            ml_mesh_copy_triangles_u32(
                self.handle.as_ptr(),
                triangles.as_mut_ptr(),
                count,
                &mut written,
                &mut obstruction,
            )
        };
        status_result(status, obstruction)?;
        triangles.truncate(written * 3);
        triangles
            .chunks_exact(3)
            .map(|triangle| {
                Ok([
                    *triangle.first().ok_or_else(projection_shape_error)?,
                    *triangle.get(1).ok_or_else(projection_shape_error)?,
                    *triangle.get(2).ok_or_else(projection_shape_error)?,
                ])
            })
            .collect()
    }

    pub fn insert_many(&self, points: &[[f64; 2]]) -> Result<Self, MoonlightError> {
        let coordinates = flatten_points(points);
        create_handle(|output, obstruction| unsafe {
            ml_mesh_insert_many_f64(
                self.handle.as_ptr(),
                coordinates.as_ptr(),
                points.len(),
                output,
                obstruction,
            )
        })
        .map(Self::from_handle)
    }

    pub fn site_union(&self, other: &Self) -> Result<Self, MoonlightError> {
        self.binary(|output, obstruction| unsafe {
            ml_mesh_site_union(
                self.handle.as_ptr(),
                other.handle.as_ptr(),
                output,
                obstruction,
            )
        })
    }

    pub fn site_intersection(&self, other: &Self) -> Result<Self, MoonlightError> {
        self.binary(|output, obstruction| unsafe {
            ml_mesh_site_intersection(
                self.handle.as_ptr(),
                other.handle.as_ptr(),
                output,
                obstruction,
            )
        })
    }

    pub fn site_difference(&self, other: &Self) -> Result<Self, MoonlightError> {
        self.binary(|output, obstruction| unsafe {
            ml_mesh_site_difference(
                self.handle.as_ptr(),
                other.handle.as_ptr(),
                output,
                obstruction,
            )
        })
    }

    pub fn site_symmetric_difference(&self, other: &Self) -> Result<Self, MoonlightError> {
        self.binary(|output, obstruction| unsafe {
            ml_mesh_site_symmetric_difference(
                self.handle.as_ptr(),
                other.handle.as_ptr(),
                output,
                obstruction,
            )
        })
    }

    fn binary(
        &self,
        operation: impl FnOnce(*mut *mut NativeMesh, *mut NativeObstruction) -> u32,
    ) -> Result<Self, MoonlightError> {
        create_handle(operation).map(Self::from_handle)
    }

    fn count(
        &self,
        operation: impl FnOnce(*mut usize, *mut NativeObstruction) -> u32,
    ) -> Result<usize, MoonlightError> {
        let mut count = 0;
        let mut obstruction = NativeObstruction::default();
        let status = operation(&mut count, &mut obstruction);
        status_result(status, obstruction)?;
        Ok(count)
    }
}

impl Drop for Mesh {
    fn drop(&mut self) {
        unsafe { ml_mesh_free(self.handle.as_ptr()) }
    }
}

pub struct Region {
    handle: NonNull<NativeRegion>,
    _thread_affinity: PhantomData<Rc<()>>,
}

impl Region {
    fn from_handle(handle: NonNull<NativeRegion>) -> Self {
        Self {
            handle,
            _thread_affinity: PhantomData,
        }
    }

    pub fn components(&self) -> Result<Vec<PolygonComponent>, MoonlightError> {
        let (component_count, loop_count, point_count) = self.counts()?;
        let mut coordinates = vec![0.0; point_count * 2];
        let mut loop_point_offsets = vec![0; loop_count + 1];
        let mut component_loop_offsets = vec![0; component_count + 1];
        let mut obstruction = NativeObstruction::default();
        let status = unsafe {
            ml_region_copy_f64(
                self.handle.as_ptr(),
                coordinates.as_mut_ptr(),
                point_count,
                loop_point_offsets.as_mut_ptr(),
                loop_point_offsets.len(),
                component_loop_offsets.as_mut_ptr(),
                component_loop_offsets.len(),
                &mut obstruction,
            )
        };
        status_result(status, obstruction)?;
        let points = coordinate_pairs(&coordinates)?;
        component_loop_offsets
            .windows(2)
            .map(|window| {
                let (start, end) = offset_window(window)?;
                if start >= end {
                    return Err(projection_shape_error());
                }
                let outer = projected_loop(&points, &loop_point_offsets, start)?;
                let holes = (start + 1..end)
                    .map(|index| projected_loop(&points, &loop_point_offsets, index))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(PolygonComponent { outer, holes })
            })
            .collect()
    }

    pub fn valuations(&self) -> Result<RegionValuations, MoonlightError> {
        self.measure_with_capacity(128)
    }

    pub fn locate(&self, [x, y]: [f64; 2]) -> Result<RegionLocation, MoonlightError> {
        let mut location = 0;
        let mut obstruction = NativeObstruction::default();
        let status = unsafe {
            ml_region_locate_point_f64(self.handle.as_ptr(), x, y, &mut location, &mut obstruction)
        };
        status_result(status, obstruction)?;
        region_location(location)
    }

    pub fn union(&self, other: &Self) -> Result<Self, MoonlightError> {
        self.binary(|output, obstruction| unsafe {
            ml_region_union(
                self.handle.as_ptr(),
                other.handle.as_ptr(),
                output,
                obstruction,
            )
        })
    }

    pub fn intersection(&self, other: &Self) -> Result<Self, MoonlightError> {
        self.binary(|output, obstruction| unsafe {
            ml_region_intersection(
                self.handle.as_ptr(),
                other.handle.as_ptr(),
                output,
                obstruction,
            )
        })
    }

    pub fn difference(&self, other: &Self) -> Result<Self, MoonlightError> {
        self.binary(|output, obstruction| unsafe {
            ml_region_difference(
                self.handle.as_ptr(),
                other.handle.as_ptr(),
                output,
                obstruction,
            )
        })
    }

    pub fn symmetric_difference(&self, other: &Self) -> Result<Self, MoonlightError> {
        self.binary(|output, obstruction| unsafe {
            ml_region_symmetric_difference(
                self.handle.as_ptr(),
                other.handle.as_ptr(),
                output,
                obstruction,
            )
        })
    }

    pub fn minkowski_sum(&self, other: &Self) -> Result<(Self, MinkowskiReceipt), MoonlightError> {
        produce_morphology(|output, receipt, obstruction| unsafe {
            ml_region_minkowski_sum(
                self.handle.as_ptr(),
                other.handle.as_ptr(),
                output,
                receipt,
                obstruction,
            )
        })
    }

    pub fn offset(
        &self,
        element: &StructuringElement,
    ) -> Result<(Self, MinkowskiReceipt), MoonlightError> {
        self.with_element(
            element,
            |element, region, output, receipt, obstruction| unsafe {
                ml_region_offset(element, region, output, receipt, obstruction)
            },
        )
    }

    pub fn inset(
        &self,
        element: &StructuringElement,
    ) -> Result<(Self, MinkowskiReceipt), MoonlightError> {
        self.with_element(
            element,
            |element, region, output, receipt, obstruction| unsafe {
                ml_region_inset(element, region, output, receipt, obstruction)
            },
        )
    }

    pub fn open(
        &self,
        element: &StructuringElement,
    ) -> Result<(Self, MinkowskiReceipt), MoonlightError> {
        self.with_element(
            element,
            |element, region, output, receipt, obstruction| unsafe {
                ml_region_open(element, region, output, receipt, obstruction)
            },
        )
    }

    pub fn close(
        &self,
        element: &StructuringElement,
    ) -> Result<(Self, MinkowskiReceipt), MoonlightError> {
        self.with_element(
            element,
            |element, region, output, receipt, obstruction| unsafe {
                ml_region_close(element, region, output, receipt, obstruction)
            },
        )
    }

    fn binary(
        &self,
        operation: impl FnOnce(*mut *mut NativeRegion, *mut NativeObstruction) -> u32,
    ) -> Result<Self, MoonlightError> {
        create_handle(operation).map(Self::from_handle)
    }

    fn with_element(
        &self,
        element: &StructuringElement,
        operation: impl FnOnce(
            *const NativeStructuringElement,
            *const NativeRegion,
            *mut *mut NativeRegion,
            *mut NativeMinkowskiReceipt,
            *mut NativeObstruction,
        ) -> u32,
    ) -> Result<(Self, MinkowskiReceipt), MoonlightError> {
        produce_morphology(|output, receipt, obstruction| {
            operation(
                element.handle.as_ptr(),
                self.handle.as_ptr(),
                output,
                receipt,
                obstruction,
            )
        })
    }

    fn counts(&self) -> Result<(usize, usize, usize), MoonlightError> {
        let mut component_count = 0;
        let mut loop_count = 0;
        let mut point_count = 0;
        let mut obstruction = NativeObstruction::default();
        let status = unsafe {
            ml_region_counts(
                self.handle.as_ptr(),
                &mut component_count,
                &mut loop_count,
                &mut point_count,
                &mut obstruction,
            )
        };
        status_result(status, obstruction)?;
        Ok((component_count, loop_count, point_count))
    }

    fn measure_with_capacity(&self, capacity: usize) -> Result<RegionValuations, MoonlightError> {
        let mut euler_characteristic = 0;
        let mut area_ratio = vec![0; capacity];
        let mut area_bytes_written = 0;
        let mut perimeter_lower = 0.0;
        let mut perimeter_upper = 0.0;
        let mut obstruction = NativeObstruction::default();
        let status = unsafe {
            ml_region_measure(
                self.handle.as_ptr(),
                &mut euler_characteristic,
                area_ratio.as_mut_ptr(),
                capacity,
                &mut area_bytes_written,
                &mut perimeter_lower,
                &mut perimeter_upper,
                &mut obstruction,
            )
        };
        if status == ML_STATUS_BUFFER_TOO_SMALL
            && obstruction.code == ML_OBSTRUCTION_BUFFER_TOO_SMALL
        {
            return self.measure_with_capacity(area_bytes_written + 1);
        }
        status_result(status, obstruction)?;
        let area_bytes = area_ratio
            .get(..area_bytes_written)
            .ok_or_else(projection_shape_error)?
            .iter()
            .map(|byte| *byte as u8)
            .collect::<Vec<_>>();
        let area_text = String::from_utf8(area_bytes).map_err(|failure| {
            invalid_native_result(format!(
                "Moonlight returned a non-UTF-8 exact-area ratio: {failure}"
            ))
        })?;
        let (numerator, denominator) = area_text.split_once('/').ok_or_else(|| {
            invalid_native_result("Moonlight returned a malformed exact-area ratio")
        })?;
        Ok(RegionValuations {
            euler_characteristic,
            area: ExactRational {
                numerator: numerator.to_owned(),
                denominator: denominator.to_owned(),
            },
            perimeter_bounds: [perimeter_lower, perimeter_upper],
        })
    }
}

impl Drop for Region {
    fn drop(&mut self) {
        unsafe { ml_region_free(self.handle.as_ptr()) }
    }
}

pub struct StructuringElement {
    handle: NonNull<NativeStructuringElement>,
    _thread_affinity: PhantomData<Rc<()>>,
}

impl StructuringElement {
    fn from_handle(handle: NonNull<NativeStructuringElement>) -> Self {
        Self {
            handle,
            _thread_affinity: PhantomData,
        }
    }
}

impl Drop for StructuringElement {
    fn drop(&mut self) {
        unsafe { ml_structuring_element_free(self.handle.as_ptr()) }
    }
}

fn flatten_points(points: &[[f64; 2]]) -> Vec<f64> {
    points.iter().flat_map(|[x, y]| [*x, *y]).collect()
}

fn coordinate_pairs(coordinates: &[f64]) -> Result<Vec<[f64; 2]>, MoonlightError> {
    coordinates
        .chunks_exact(2)
        .map(|point| {
            Ok([
                *point.first().ok_or_else(projection_shape_error)?,
                *point.get(1).ok_or_else(projection_shape_error)?,
            ])
        })
        .collect()
}

fn offset_window(window: &[usize]) -> Result<(usize, usize), MoonlightError> {
    Ok((
        *window.first().ok_or_else(projection_shape_error)?,
        *window.get(1).ok_or_else(projection_shape_error)?,
    ))
}

fn projected_loop(
    points: &[[f64; 2]],
    offsets: &[usize],
    index: usize,
) -> Result<Vec<[f64; 2]>, MoonlightError> {
    let start = *offsets.get(index).ok_or_else(projection_shape_error)?;
    let end = *offsets.get(index + 1).ok_or_else(projection_shape_error)?;
    points
        .get(start..end)
        .map(<[_]>::to_vec)
        .ok_or_else(projection_shape_error)
}

fn create_handle<Native>(
    operation: impl FnOnce(*mut *mut Native, *mut NativeObstruction) -> u32,
) -> Result<NonNull<Native>, MoonlightError> {
    let mut output = std::ptr::null_mut();
    let mut obstruction = NativeObstruction::default();
    let status = operation(&mut output, &mut obstruction);
    status_result(status, obstruction)?;
    NonNull::new(output).ok_or_else(success_without_handle_error)
}

fn produce_morphology(
    operation: impl FnOnce(
        *mut *mut NativeRegion,
        *mut NativeMinkowskiReceipt,
        *mut NativeObstruction,
    ) -> u32,
) -> Result<(Region, MinkowskiReceipt), MoonlightError> {
    let mut output = std::ptr::null_mut();
    let mut receipt = NativeMinkowskiReceipt::default();
    let mut obstruction = NativeObstruction::default();
    let status = operation(&mut output, &mut receipt, &mut obstruction);
    status_result(status, obstruction)?;
    let handle = NonNull::new(output).ok_or_else(success_without_handle_error)?;
    Ok((Region::from_handle(handle), minkowski_receipt(receipt)?))
}

fn minkowski_receipt(receipt: NativeMinkowskiReceipt) -> Result<MinkowskiReceipt, MoonlightError> {
    Ok(MinkowskiReceipt {
        operation: minkowski_operation(receipt.operation)?,
        input_components: receipt.input_components,
        convex_pieces: receipt.convex_pieces,
        generated_pieces: receipt.generated_pieces,
        generated_convolution_edges: receipt.generated_convolution_edges,
        overlay_passes: receipt.overlay_passes,
        exact_crossings: receipt.exact_crossings,
        output_cells: receipt.output_cells,
        exact_coordinate_bit_growth: receipt.exact_coordinate_bit_growth,
    })
}

fn minkowski_operation(code: u32) -> Result<MinkowskiOperation, MoonlightError> {
    match code {
        ML_MINKOWSKI_ADDITION => Ok(MinkowskiOperation::Addition),
        ML_MINKOWSKI_EROSION => Ok(MinkowskiOperation::Erosion),
        ML_MINKOWSKI_OPENING => Ok(MinkowskiOperation::Opening),
        ML_MINKOWSKI_CLOSING => Ok(MinkowskiOperation::Closing),
        _ => Err(MoonlightError::UnknownWireValue {
            vocabulary: WireVocabulary::MinkowskiOperation,
            value: code,
        }),
    }
}

fn region_location(code: u32) -> Result<RegionLocation, MoonlightError> {
    match code {
        ML_REGION_EXTERIOR => Ok(RegionLocation::Exterior),
        ML_REGION_BOUNDARY => Ok(RegionLocation::Boundary),
        ML_REGION_INTERIOR => Ok(RegionLocation::Interior),
        _ => Err(MoonlightError::UnknownWireValue {
            vocabulary: WireVocabulary::RegionLocation,
            value: code,
        }),
    }
}
