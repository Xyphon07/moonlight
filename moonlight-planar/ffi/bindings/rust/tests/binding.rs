use moonlight_triangulation::{
    ExactRational, MinkowskiOperation, Moonlight, MoonlightError, PolygonComponent, RegionLocation,
};

#[test]
fn immutable_site_set_algebra_and_dense_projection() {
    let engine = Moonlight::initialize().expect("runtime");
    let left = engine
        .delaunay(&[[0.0, 0.0], [2.0, 0.0], [0.0, 2.0], [2.0, 2.0]])
        .expect("left mesh");
    let right = engine
        .delaunay(&[[2.0, 0.0], [4.0, 0.0], [2.0, 2.0], [4.0, 2.0]])
        .expect("right mesh");
    let union = left.site_union(&right).expect("site union");
    let intersection = left.site_intersection(&right).expect("site intersection");
    let difference = left.site_difference(&right).expect("site difference");
    let symmetric = left
        .site_symmetric_difference(&right)
        .expect("site symmetric difference");
    let extended = left
        .insert_many(&[[1.0, 1.0], [3.0, 1.0]])
        .expect("batch insertion");

    assert_eq!(left.vertex_count().expect("left count"), 4);
    assert_eq!(union.vertex_count().expect("union count"), 6);
    assert_eq!(intersection.vertex_count().expect("intersection count"), 2);
    assert_eq!(difference.vertex_count().expect("difference count"), 2);
    assert_eq!(symmetric.vertex_count().expect("symmetric count"), 4);
    assert_eq!(extended.vertex_count().expect("extended count"), 6);
    assert_eq!(
        left.vertices().expect("vertices").len(),
        left.vertex_count().expect("vertex count")
    );
    assert_eq!(
        left.triangles().expect("triangles").len(),
        left.triangle_count().expect("triangle count")
    );
}

#[test]
fn exact_region_boolean_valuation_and_morphology() {
    let engine = Moonlight::initialize().expect("runtime");
    let left = engine
        .region(&[PolygonComponent {
            outer: vec![[0.0, 0.0], [2.0, 0.0], [2.0, 2.0], [0.0, 2.0]],
            holes: vec![],
        }])
        .expect("left region");
    let right = engine
        .region(&[PolygonComponent {
            outer: vec![[1.0, 0.0], [3.0, 0.0], [3.0, 2.0], [1.0, 2.0]],
            holes: vec![],
        }])
        .expect("right region");
    let intersection = left.intersection(&right).expect("intersection");
    let symmetric = left
        .symmetric_difference(&right)
        .expect("symmetric difference");
    let kernel = engine
        .structuring_element(&[[-0.5, -0.5], [0.5, -0.5], [0.5, 0.5], [-0.5, 0.5]])
        .expect("structuring element");
    let (offset, receipt) = left.offset(&kernel).expect("offset");

    assert_eq!(left.components().expect("components").len(), 1);
    assert_eq!(
        intersection
            .valuations()
            .expect("intersection valuation")
            .area,
        ExactRational {
            numerator: "2".to_owned(),
            denominator: "1".to_owned(),
        }
    );
    assert_eq!(
        symmetric
            .valuations()
            .expect("symmetric valuation")
            .euler_characteristic,
        2
    );
    assert_eq!(
        left.locate([1.0, 1.0]).expect("interior"),
        RegionLocation::Interior
    );
    assert_eq!(
        left.locate([0.0, 1.0]).expect("boundary"),
        RegionLocation::Boundary
    );
    assert_eq!(
        left.locate([3.0, 1.0]).expect("exterior"),
        RegionLocation::Exterior
    );
    assert_eq!(
        offset.valuations().expect("offset valuation").area,
        ExactRational {
            numerator: "9".to_owned(),
            denominator: "1".to_owned(),
        }
    );
    assert_eq!(receipt.operation, MinkowskiOperation::Addition);
    assert!(receipt.generated_pieces >= 1);
}

#[test]
fn invalid_coordinate_preserves_typed_witness() {
    let engine = Moonlight::initialize().expect("runtime");
    let failure = engine
        .delaunay(&[[0.0, 0.0], [f64::NAN, 1.0], [1.0, 0.0]])
        .err()
        .expect("invalid coordinate refusal");
    let MoonlightError::AbiObstruction(obstruction) = failure else {
        panic!("expected an ABI obstruction");
    };
    assert_eq!(obstruction.status, 4);
    assert_eq!(obstruction.code, 1);
    assert_eq!(obstruction.coordinate_error, 1);
    assert_eq!(obstruction.input_index, 1);
}
