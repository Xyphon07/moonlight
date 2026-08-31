import assert from "node:assert/strict";
import test from "node:test";

import {
  AbiStatus,
  CoordinateError,
  MinkowskiOperation,
  ObstructionCode,
  RegionLocation,
  formatMoonlightFailure,
  loadMoonlight,
  type Moonlight,
  type Result,
} from "../src/index.js";

function expectSuccess<Value>(result: Result<Value>): Value {
  if (!result.ok) {
    assert.fail(formatMoonlightFailure(result.error));
  }
  return result.value;
}

function libraryPath(): string {
  const path = process.env.MOONLIGHT_TRIANGULATION_LIBRARY;
  assert.notEqual(path, undefined, "MOONLIGHT_TRIANGULATION_LIBRARY is required");
  return path ?? "";
}

function runtime(): Moonlight {
  return expectSuccess(loadMoonlight({ kind: "path", path: libraryPath() }));
}

test("immutable site-set algebra and dense projection", () => {
  const engine = runtime();
  using left = expectSuccess(engine.delaunay([[0, 0], [2, 0], [0, 2], [2, 2]]));
  using right = expectSuccess(engine.delaunay([[2, 0], [4, 0], [2, 2], [4, 2]]));
  using union = expectSuccess(left.siteUnion(right));
  using intersection = expectSuccess(left.siteIntersection(right));
  using difference = expectSuccess(left.siteDifference(right));
  using symmetric = expectSuccess(left.siteSymmetricDifference(right));
  using extended = expectSuccess(left.insertMany([[1, 1], [3, 1]]));

  assert.equal(expectSuccess(left.vertexCount()), 4);
  assert.equal(expectSuccess(union.vertexCount()), 6);
  assert.equal(expectSuccess(intersection.vertexCount()), 2);
  assert.equal(expectSuccess(difference.vertexCount()), 2);
  assert.equal(expectSuccess(symmetric.vertexCount()), 4);
  assert.equal(expectSuccess(extended.vertexCount()), 6);
  assert.equal(expectSuccess(left.vertices()).length, expectSuccess(left.vertexCount()));
  assert.equal(expectSuccess(left.triangles()).length, expectSuccess(left.triangleCount()));

});

test("exact regions expose Boolean, valuation, location, and all morphology operations", () => {
  const engine = runtime();
  using left = expectSuccess(engine.region([{ outer: [[0, 0], [2, 0], [2, 2], [0, 2]] }]));
  using right = expectSuccess(engine.region([{ outer: [[1, 0], [3, 0], [3, 2], [1, 2]] }]));
  using intersection = expectSuccess(left.intersection(right));
  using symmetric = expectSuccess(left.symmetricDifference(right));
  using kernel = expectSuccess(
    engine.structuringElement([[-0.25, -0.25], [0.25, -0.25], [0.25, 0.25], [-0.25, 0.25]]),
  );
  const sum = expectSuccess(left.minkowskiSum(right));
  const offset = expectSuccess(left.offset(kernel));
  const inset = expectSuccess(left.inset(kernel));
  const opened = expectSuccess(left.open(kernel));
  const closed = expectSuccess(left.close(kernel));
  using sumRegion = sum.region;
  using offsetRegion = offset.region;
  using insetRegion = inset.region;
  using openedRegion = opened.region;
  using closedRegion = closed.region;

  assert.equal(expectSuccess(left.components()).length, 1);
  assert.equal(expectSuccess(left.components())[0]?.outer.length, 4);
  assert.deepEqual(expectSuccess(intersection.valuations()).area, { numerator: 2n, denominator: 1n });
  assert.equal(expectSuccess(symmetric.valuations()).eulerCharacteristic, 2n);
  assert.equal(expectSuccess(left.locate([1, 1])), RegionLocation.Interior);
  assert.equal(expectSuccess(left.locate([0, 1])), RegionLocation.Boundary);
  assert.equal(expectSuccess(left.locate([3, 1])), RegionLocation.Exterior);
  assert.equal(offset.receipt.operation, MinkowskiOperation.Addition);
  assert.equal(inset.receipt.operation, MinkowskiOperation.Erosion);
  assert.equal(opened.receipt.operation, MinkowskiOperation.Opening);
  assert.equal(closed.receipt.operation, MinkowskiOperation.Closing);
  assert(sum.receipt.generatedPieces >= 1n);

});

test("invalid coordinates return a typed obstruction", () => {
  const result = runtime().delaunay([[0, 0], [Number.NaN, 1], [1, 0]]);
  assert.equal(result.ok, false);
  if (result.ok || result.error.kind !== "abi-obstruction") {
    throw new Error("expected an ABI obstruction");
  }
  assert.equal(result.error.obstruction.status, AbiStatus.GeometryObstruction);
  assert.equal(result.error.obstruction.code, ObstructionCode.InvalidCoordinate);
  assert.equal(result.error.obstruction.coordinateError, CoordinateError.Nan);
  assert.equal(result.error.obstruction.inputIndex, 1n);
});

test("materialized coordinates own native counts", () => {
  const points = new Proxy<readonly (readonly [number, number])[]>([[0, 0]], {
    get(target, property, receiver) {
      return property === "length" ? 1_000 : Reflect.get(target, property, receiver);
    },
  });
  using mesh = expectSuccess(runtime().delaunay(points));
  assert.equal(expectSuccess(mesh.vertexCount()), 1);
});

test("loading, runtime descent, and disposal are explicit", () => {
  const unavailable = loadMoonlight({ kind: "path", path: "" });
  assert.equal(unavailable.ok, false);
  if (!unavailable.ok) {
    assert.equal(unavailable.error.kind, "native-library-unavailable");
  }

  const firstRuntime = runtime();
  const secondRuntime = runtime();
  const first = expectSuccess(firstRuntime.delaunay([[0, 0]]));
  const second = expectSuccess(secondRuntime.delaunay([[1, 1]]));
  const mixed = first.siteUnion(second);
  assert.equal(mixed.ok, false);
  if (!mixed.ok) {
    assert.equal(mixed.error.kind, "foreign-runtime");
  }

  first[Symbol.dispose]();
  assert.equal(first.disposed, true);
  const afterDispose = first.vertexCount();
  assert.equal(afterDispose.ok, false);
  if (!afterDispose.ok) {
    assert.equal(afterDispose.error.kind, "disposed-resource");
  }
  second.dispose();
});
