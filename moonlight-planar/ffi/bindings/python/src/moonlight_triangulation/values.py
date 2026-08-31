from __future__ import annotations

from dataclasses import dataclass
from enum import IntEnum
from fractions import Fraction

from ._native_generated import (
    ML_MINKOWSKI_ADDITION,
    ML_MINKOWSKI_CLOSING,
    ML_MINKOWSKI_EROSION,
    ML_MINKOWSKI_OPENING,
    ML_REGION_BOUNDARY,
    ML_REGION_EXTERIOR,
    ML_REGION_INTERIOR,
)

Point = tuple[float, float]
Triangle = tuple[int, int, int]


@dataclass(frozen=True)
class PolygonComponent:
    outer: tuple[Point, ...]
    holes: tuple[tuple[Point, ...], ...] = ()


class RegionLocation(IntEnum):
    EXTERIOR = ML_REGION_EXTERIOR
    BOUNDARY = ML_REGION_BOUNDARY
    INTERIOR = ML_REGION_INTERIOR


class MinkowskiOperation(IntEnum):
    ADDITION = ML_MINKOWSKI_ADDITION
    EROSION = ML_MINKOWSKI_EROSION
    OPENING = ML_MINKOWSKI_OPENING
    CLOSING = ML_MINKOWSKI_CLOSING


@dataclass(frozen=True)
class RegionValuations:
    euler_characteristic: int
    area: Fraction
    perimeter_bounds: tuple[float, float]


@dataclass(frozen=True)
class MinkowskiReceipt:
    operation: MinkowskiOperation
    input_components: int
    convex_pieces: int
    generated_pieces: int
    generated_convolution_edges: int
    overlay_passes: int
    exact_crossings: int
    output_cells: int
    exact_coordinate_bit_growth: int
