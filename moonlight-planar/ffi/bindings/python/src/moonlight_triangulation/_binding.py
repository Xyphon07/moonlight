from __future__ import annotations

import ctypes
import os
import weakref
from collections.abc import Callable, Iterable, Sequence
from fractions import Fraction
from pathlib import Path
from threading import get_ident
from typing import Self

from ._native_generated import (
    ABI_VERSION,
    ML_OBSTRUCTION_BUFFER_TOO_SMALL,
    ML_STATUS_BUFFER_TOO_SMALL,
    ML_STATUS_OK,
    _NativeApi,
    _NativeMinkowskiReceipt,
    _Obstruction,
)
from .errors import MoonlightError
from .values import (
    MinkowskiOperation,
    MinkowskiReceipt,
    Point,
    PolygonComponent,
    RegionLocation,
    RegionValuations,
    Triangle,
)

_BinaryNativeOperation = Callable[[ctypes.c_void_p, ctypes.c_void_p, object, object], int]
_MorphologyNativeOperation = Callable[[ctypes.c_void_p, ctypes.c_void_p, object, object, object], int]

class Moonlight:
    def __init__(self, library_path: str | os.PathLike[str] | None = None) -> None:
        configured_path = (
            os.environ.get("MOONLIGHT_TRIANGULATION_LIBRARY")
            if library_path is None
            else library_path
        )
        if configured_path is None:
            raise ValueError("set MOONLIGHT_TRIANGULATION_LIBRARY or pass library_path")
        if os.fspath(configured_path) == "":
            raise ValueError("Moonlight native library path must not be empty")
        self._native = _NativeApi(Path(configured_path))
        initialization_status = int(self._native.library.ml_runtime_initialize())
        if initialization_status != ML_STATUS_OK:
            raise RuntimeError(
                f"Moonlight runtime initialization failed with status {initialization_status}"
            )
        observed_version = int(self._native.library.ml_abi_version())
        if observed_version != ABI_VERSION:
            raise RuntimeError(f"unsupported Moonlight ABI version {observed_version}")

    def delaunay(self, points: Sequence[Point]) -> Mesh:
        coordinates, pointer = _coordinate_buffer(points)
        handle = _produce_handle(
            lambda output, obstruction: self._native.library.ml_delaunay_f64(
                pointer, len(coordinates) // 2, output, obstruction
            ),
        )
        return Mesh(self._native, handle)

    def region(self, components: Sequence[PolygonComponent]) -> Region:
        loops, loop_counts, component_counts = _component_layout(components)
        coordinates, coordinate_pointer = _coordinate_buffer(
            point for loop in loops for point in loop
        )
        loop_buffer = _size_buffer(loop_counts)
        component_buffer = _size_buffer(component_counts)
        handle = _produce_handle(
            lambda output, obstruction: self._native.library.ml_region_create_f64(
                coordinate_pointer,
                len(coordinates) // 2,
                loop_buffer,
                len(loop_counts),
                component_buffer,
                len(component_counts),
                output,
                obstruction,
            ),
        )
        return Region(self._native, handle)

    def structuring_element(self, points: Sequence[Point]) -> StructuringElement:
        coordinates, pointer = _coordinate_buffer(points)
        handle = _produce_handle(
            lambda output, obstruction: self._native.library.ml_structuring_element_create_f64(
                pointer, len(coordinates) // 2, output, obstruction
            ),
        )
        return StructuringElement(self._native, handle)


class _OwnedHandle:
    __slots__ = ("_native", "_handle", "_finalizer", "_kind", "_owner_thread", "__weakref__")

    def __init__(self, native: _NativeApi, handle: ctypes.c_void_p, free: Callable[[ctypes.c_void_p], None], kind: str) -> None:
        self._native = native
        self._handle = handle
        self._kind = kind
        self._owner_thread = get_ident()
        self._finalizer = weakref.finalize(self, free, handle)

    def dispose(self) -> None:
        self._require_owner_thread()
        self._finalizer()

    def __enter__(self) -> Self:
        return self

    def __exit__(self, _type: object, _value: object, _traceback: object) -> None:
        self.dispose()

    def _live_handle(self) -> ctypes.c_void_p:
        self._require_owner_thread()
        if not self._finalizer.alive:
            raise RuntimeError(f"{self._kind} is disposed")
        return self._handle

    def _require_owner_thread(self) -> None:
        if get_ident() != self._owner_thread:
            raise RuntimeError(f"{self._kind} belongs to another thread")

    def _require_same_runtime(self, other: _OwnedHandle) -> None:
        if self._native is not other._native:
            raise ValueError("both values must belong to the same Moonlight runtime")


class Mesh(_OwnedHandle):
    __slots__ = ()

    def __init__(self, native: _NativeApi, handle: ctypes.c_void_p) -> None:
        super().__init__(native, handle, native.library.ml_mesh_free, "mesh")

    @property
    def vertex_count(self) -> int:
        return self._count(self._native.library.ml_mesh_vertex_count)

    @property
    def triangle_count(self) -> int:
        return self._count(self._native.library.ml_mesh_triangle_count)

    @property
    def vertices(self) -> tuple[Point, ...]:
        count = self.vertex_count
        output = (ctypes.c_double * (count * 2))()
        written = ctypes.c_size_t()
        obstruction = _Obstruction()
        status = int(
            self._native.library.ml_mesh_copy_vertices_f64(
                self._live_handle(), output, count, ctypes.byref(written), ctypes.byref(obstruction)
            )
        )
        _check_status(status, obstruction)
        return tuple((float(output[index * 2]), float(output[index * 2 + 1])) for index in range(written.value))

    @property
    def triangles(self) -> tuple[Triangle, ...]:
        count = self.triangle_count
        output = (ctypes.c_uint32 * (count * 3))()
        written = ctypes.c_size_t()
        obstruction = _Obstruction()
        status = int(
            self._native.library.ml_mesh_copy_triangles_u32(
                self._live_handle(), output, count, ctypes.byref(written), ctypes.byref(obstruction)
            )
        )
        _check_status(status, obstruction)
        return tuple(
            (int(output[index * 3]), int(output[index * 3 + 1]), int(output[index * 3 + 2]))
            for index in range(written.value)
        )

    def insert_many(self, points: Sequence[Point]) -> Mesh:
        coordinates, pointer = _coordinate_buffer(points)
        handle = _produce_handle(
            lambda output, obstruction: self._native.library.ml_mesh_insert_many_f64(
                self._live_handle(), pointer, len(coordinates) // 2, output, obstruction
            ),
        )
        return Mesh(self._native, handle)

    def site_union(self, other: Mesh) -> Mesh:
        return self._binary(other, self._native.library.ml_mesh_site_union)

    def site_intersection(self, other: Mesh) -> Mesh:
        return self._binary(other, self._native.library.ml_mesh_site_intersection)

    def site_difference(self, other: Mesh) -> Mesh:
        return self._binary(other, self._native.library.ml_mesh_site_difference)

    def site_symmetric_difference(self, other: Mesh) -> Mesh:
        return self._binary(other, self._native.library.ml_mesh_site_symmetric_difference)

    def _binary(self, other: Mesh, operation: _BinaryNativeOperation) -> Mesh:
        self._require_same_runtime(other)
        handle = _produce_handle(
            lambda output, obstruction: operation(
                self._live_handle(), other._live_handle(), output, obstruction
            ),
        )
        return Mesh(self._native, handle)

    def _count(self, operation: Callable[[ctypes.c_void_p, object, object], int]) -> int:
        output = ctypes.c_size_t()
        obstruction = _Obstruction()
        status = int(operation(self._live_handle(), ctypes.byref(output), ctypes.byref(obstruction)))
        _check_status(status, obstruction)
        return int(output.value)


class Region(_OwnedHandle):
    __slots__ = ()

    def __init__(self, native: _NativeApi, handle: ctypes.c_void_p) -> None:
        super().__init__(native, handle, native.library.ml_region_free, "region")

    @property
    def components(self) -> tuple[PolygonComponent, ...]:
        component_count, loop_count, point_count = self._counts()
        coordinates = (ctypes.c_double * (point_count * 2))()
        loop_offsets = (ctypes.c_size_t * (loop_count + 1))()
        component_offsets = (ctypes.c_size_t * (component_count + 1))()
        obstruction = _Obstruction()
        status = int(
            self._native.library.ml_region_copy_f64(
                self._live_handle(),
                coordinates,
                point_count,
                loop_offsets,
                loop_count + 1,
                component_offsets,
                component_count + 1,
                ctypes.byref(obstruction),
            )
        )
        _check_status(status, obstruction)
        points = tuple((float(coordinates[index * 2]), float(coordinates[index * 2 + 1])) for index in range(point_count))
        loops = tuple(
            points[int(loop_offsets[index]) : int(loop_offsets[index + 1])]
            for index in range(loop_count)
        )
        component_ranges = tuple(
            (int(component_offsets[index]), int(component_offsets[index + 1]))
            for index in range(component_count)
        )
        if any(start >= end for start, end in component_ranges):
            raise RuntimeError("Moonlight returned a component without an outer loop")
        return tuple(
            PolygonComponent(loops[start], loops[start + 1 : end])
            for start, end in component_ranges
        )

    @property
    def valuations(self) -> RegionValuations:
        return self._measure_with_capacity(128)

    def locate(self, point: Point) -> RegionLocation:
        output = ctypes.c_uint32()
        obstruction = _Obstruction()
        status = int(
            self._native.library.ml_region_locate_point_f64(
                self._live_handle(), point[0], point[1], ctypes.byref(output), ctypes.byref(obstruction)
            )
        )
        _check_status(status, obstruction)
        return RegionLocation(output.value)

    def union(self, other: Region) -> Region:
        return self._binary(other, self._native.library.ml_region_union)

    def intersection(self, other: Region) -> Region:
        return self._binary(other, self._native.library.ml_region_intersection)

    def difference(self, other: Region) -> Region:
        return self._binary(other, self._native.library.ml_region_difference)

    def symmetric_difference(self, other: Region) -> Region:
        return self._binary(other, self._native.library.ml_region_symmetric_difference)

    def minkowski_sum(self, other: Region) -> tuple[Region, MinkowskiReceipt]:
        self._require_same_runtime(other)
        return self._morph(
            lambda output, receipt, obstruction: self._native.library.ml_region_minkowski_sum(
                self._live_handle(), other._live_handle(), output, receipt, obstruction
            )
        )

    def offset(self, element: StructuringElement) -> tuple[Region, MinkowskiReceipt]:
        return self._with_element(element, self._native.library.ml_region_offset)

    def inset(self, element: StructuringElement) -> tuple[Region, MinkowskiReceipt]:
        return self._with_element(element, self._native.library.ml_region_inset)

    def open(self, element: StructuringElement) -> tuple[Region, MinkowskiReceipt]:
        return self._with_element(element, self._native.library.ml_region_open)

    def close(self, element: StructuringElement) -> tuple[Region, MinkowskiReceipt]:
        return self._with_element(element, self._native.library.ml_region_close)

    def _binary(self, other: Region, operation: _BinaryNativeOperation) -> Region:
        self._require_same_runtime(other)
        handle = _produce_handle(
            lambda output, obstruction: operation(
                self._live_handle(), other._live_handle(), output, obstruction
            ),
        )
        return Region(self._native, handle)

    def _with_element(
        self, element: StructuringElement, operation: _MorphologyNativeOperation
    ) -> tuple[Region, MinkowskiReceipt]:
        self._require_same_runtime(element)
        return self._morph(
            lambda output, receipt, obstruction: operation(
                element._live_handle(), self._live_handle(), output, receipt, obstruction
            )
        )

    def _morph(
        self,
        operation: Callable[[object, object, object], int],
    ) -> tuple[Region, MinkowskiReceipt]:
        handle, native_receipt = _produce_morphology(operation)
        return Region(self._native, handle), _receipt(native_receipt)

    def _counts(self) -> tuple[int, int, int]:
        component_count = ctypes.c_size_t()
        loop_count = ctypes.c_size_t()
        point_count = ctypes.c_size_t()
        obstruction = _Obstruction()
        status = int(
            self._native.library.ml_region_counts(
                self._live_handle(),
                ctypes.byref(component_count),
                ctypes.byref(loop_count),
                ctypes.byref(point_count),
                ctypes.byref(obstruction),
            )
        )
        _check_status(status, obstruction)
        return int(component_count.value), int(loop_count.value), int(point_count.value)

    def _measure_with_capacity(self, capacity: int) -> RegionValuations:
        euler = ctypes.c_int64()
        area = ctypes.create_string_buffer(capacity)
        area_bytes = ctypes.c_size_t()
        lower = ctypes.c_double()
        upper = ctypes.c_double()
        obstruction = _Obstruction()
        status = int(
            self._native.library.ml_region_measure(
                self._live_handle(),
                ctypes.byref(euler),
                area,
                capacity,
                ctypes.byref(area_bytes),
                ctypes.byref(lower),
                ctypes.byref(upper),
                ctypes.byref(obstruction),
            )
        )
        if (
            status == ML_STATUS_BUFFER_TOO_SMALL
            and obstruction.code == ML_OBSTRUCTION_BUFFER_TOO_SMALL
        ):
            return self._measure_with_capacity(int(area_bytes.value) + 1)
        _check_status(status, obstruction)
        numerator, separator, denominator = area.value.decode("ascii").partition("/")
        if separator != "/":
            raise RuntimeError("Moonlight returned a malformed exact-area ratio")
        return RegionValuations(int(euler.value), Fraction(int(numerator), int(denominator)), (lower.value, upper.value))


class StructuringElement(_OwnedHandle):
    __slots__ = ()

    def __init__(self, native: _NativeApi, handle: ctypes.c_void_p) -> None:
        super().__init__(native, handle, native.library.ml_structuring_element_free, "structuring element")


def _component_layout(
    components: Sequence[PolygonComponent],
) -> tuple[tuple[Sequence[Point], ...], tuple[int, ...], tuple[int, ...]]:
    loops = tuple(
        loop
        for component in components
        for loop in (component.outer, *component.holes)
    )
    loop_counts = tuple(len(loop) for loop in loops)
    component_counts = tuple(len(component.holes) + 1 for component in components)
    return loops, loop_counts, component_counts


def _coordinate_buffer(points: Iterable[Point]) -> tuple[tuple[float, ...], object]:
    coordinates = tuple(component for x, y in points for component in (x, y))
    pointer = None if not coordinates else (ctypes.c_double * len(coordinates))(*coordinates)
    return coordinates, pointer


def _size_buffer(values: Sequence[int]) -> object:
    return (ctypes.c_size_t * len(values))(*values)


def _produce_handle(
    operation: Callable[[object, object], int],
) -> ctypes.c_void_p:
    output = ctypes.c_void_p()
    obstruction = _Obstruction()
    status = int(operation(ctypes.byref(output), ctypes.byref(obstruction)))
    _check_status(status, obstruction)
    return _required_handle(output)


def _produce_morphology(
    operation: Callable[[object, object, object], int],
) -> tuple[ctypes.c_void_p, _NativeMinkowskiReceipt]:
    output = ctypes.c_void_p()
    receipt = _NativeMinkowskiReceipt()
    obstruction = _Obstruction()
    status = int(operation(ctypes.byref(output), ctypes.byref(receipt), ctypes.byref(obstruction)))
    _check_status(status, obstruction)
    return _required_handle(output), receipt


def _receipt(native: _NativeMinkowskiReceipt) -> MinkowskiReceipt:
    return MinkowskiReceipt(
        MinkowskiOperation(native.operation),
        int(native.input_components),
        int(native.convex_pieces),
        int(native.generated_pieces),
        int(native.generated_convolution_edges),
        int(native.overlay_passes),
        int(native.exact_crossings),
        int(native.output_cells),
        int(native.exact_coordinate_bit_growth),
    )


def _check_status(status: int, obstruction: _Obstruction) -> None:
    if status != ML_STATUS_OK:
        raise MoonlightError(status, obstruction)


def _required_handle(handle: ctypes.c_void_p) -> ctypes.c_void_p:
    if not handle.value:
        raise RuntimeError("Moonlight returned success without a handle")
    return handle
