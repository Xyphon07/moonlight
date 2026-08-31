#[derive(Debug, Clone, PartialEq)]
pub struct PolygonComponent {
    pub outer: Vec<[f64; 2]>,
    pub holes: Vec<Vec<[f64; 2]>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RegionLocation {
    Exterior,
    Boundary,
    Interior,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExactRational {
    pub numerator: String,
    pub denominator: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RegionValuations {
    pub euler_characteristic: i64,
    pub area: ExactRational,
    pub perimeter_bounds: [f64; 2],
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MinkowskiOperation {
    Addition,
    Erosion,
    Opening,
    Closing,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MinkowskiReceipt {
    pub operation: MinkowskiOperation,
    pub input_components: u64,
    pub convex_pieces: u64,
    pub generated_pieces: u64,
    pub generated_convolution_edges: u64,
    pub overlay_passes: u64,
    pub exact_crossings: u64,
    pub output_cells: u64,
    pub exact_coordinate_bit_growth: u64,
}
