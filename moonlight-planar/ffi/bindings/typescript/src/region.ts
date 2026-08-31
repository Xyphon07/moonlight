import { Buffer } from "node:buffer";

import { collectResults, failure, success, type Result } from "./failure.js";
import { captureNativeCall, callStatus, produceHandle, produceMorphology } from "./internal/call.js";
import { decodeLocation, decodeStatus, pointsFromCoordinates, toBigInt, toSafeNumber } from "./internal/decode.js";
import {
  ML_OBSTRUCTION_BUFFER_TOO_SMALL,
  ML_STATUS_BUFFER_TOO_SMALL,
  type NativeApi,
  type NativeHandle,
  type NativeInteger,
  type NativeObstruction,
} from "./internal/native.generated.js";
import { borrowResource, OwnedNativeResource, type RuntimeContext } from "./internal/resource.js";
import type { StructuringElement } from "./morphology.js";
import type { OwnedResource } from "./resource.js";
import type { MinkowskiReceipt, Point, PolygonComponent, RegionValuations } from "./values.js";
import type { RegionLocation } from "./wire.generated.js";

export interface MorphologyResult {
  readonly region: Region;
  readonly receipt: MinkowskiReceipt;
}

export interface Region extends OwnedResource {
  components(): Result<readonly PolygonComponent[]>;
  valuations(): Result<RegionValuations>;
  locate(point: Point): Result<RegionLocation>;
  union(other: Region): Result<Region>;
  intersection(other: Region): Result<Region>;
  difference(other: Region): Result<Region>;
  symmetricDifference(other: Region): Result<Region>;
  minkowskiSum(other: Region): Result<MorphologyResult>;
  offset(element: StructuringElement): Result<MorphologyResult>;
  inset(element: StructuringElement): Result<MorphologyResult>;
  open(element: StructuringElement): Result<MorphologyResult>;
  close(element: StructuringElement): Result<MorphologyResult>;
}

/** @internal */
export function createRegion(context: RuntimeContext, handle: NativeHandle): Region {
  return new RegionResource(context, handle);
}

class RegionResource extends OwnedNativeResource implements Region {
  constructor(context: RuntimeContext, handle: NativeHandle) {
    super(context, handle, context.native.ml_region_free, "region");
  }

  components(): Result<readonly PolygonComponent[]> {
    const handle = this.handle();
    if (!handle.ok) {
      return handle;
    }
    const counts = this.counts(handle.value);
    if (!counts.ok) {
      return counts;
    }
    const [componentCount, loopCount, pointCount] = counts.value;
    const coordinates = new Float64Array(pointCount * 2);
    const loopPointOffsets = new BigUint64Array(loopCount + 1);
    const componentLoopOffsets = new BigUint64Array(componentCount + 1);
    const copied = callStatus((obstruction) =>
      this.context().native.ml_region_copy_f64(
        handle.value,
        coordinates,
        pointCount,
        loopPointOffsets,
        loopCount + 1,
        componentLoopOffsets,
        componentCount + 1,
        obstruction,
      ),
    );
    if (!copied.ok) {
      return copied;
    }
    const points = pointsFromCoordinates(coordinates, pointCount);
    const loopOffsets = collectResults(
      Array.from({ length: loopCount + 1 }, (_unused, index) => toSafeNumber(loopPointOffsets[index])),
    );
    if (!loopOffsets.ok) {
      return loopOffsets;
    }
    const componentOffsets = collectResults(
      Array.from({ length: componentCount + 1 }, (_unused, index) => toSafeNumber(componentLoopOffsets[index])),
    );
    if (!componentOffsets.ok) {
      return componentOffsets;
    }
    const loops = Array.from({ length: loopCount }, (_unused, index) =>
      points.slice(loopOffsets.value[index] ?? 0, loopOffsets.value[index + 1] ?? 0),
    );
    return collectResults(
      Array.from({ length: componentCount }, (_unused, index): Result<PolygonComponent> => {
        const start = componentOffsets.value[index] ?? 0;
        const end = componentOffsets.value[index + 1] ?? 0;
        const outer = loops[start];
        return outer === undefined || start >= end
          ? failure({ kind: "invalid-native-result", message: "Moonlight returned a component without an outer loop" })
          : success({ outer, holes: loops.slice(start + 1, end) });
      }),
    );
  }

  valuations(): Result<RegionValuations> {
    const handle = this.handle();
    return handle.ok ? this.measureWithCapacity(handle.value, 128) : handle;
  }

  locate([x, y]: Point): Result<RegionLocation> {
    const handle = this.handle();
    if (!handle.ok) {
      return handle;
    }
    const location = [0];
    const located = callStatus((obstruction) =>
      this.context().native.ml_region_locate_point_f64(handle.value, x, y, location, obstruction),
    );
    return located.ok ? decodeLocation(location[0]) : located;
  }

  union(other: Region): Result<Region> {
    return this.binary(other, this.context().native.ml_region_union);
  }

  intersection(other: Region): Result<Region> {
    return this.binary(other, this.context().native.ml_region_intersection);
  }

  difference(other: Region): Result<Region> {
    return this.binary(other, this.context().native.ml_region_difference);
  }

  symmetricDifference(other: Region): Result<Region> {
    return this.binary(other, this.context().native.ml_region_symmetric_difference);
  }

  minkowskiSum(other: Region): Result<MorphologyResult> {
    const left = this.handle();
    if (!left.ok) {
      return left;
    }
    const right = borrowResource(other, this.context(), "region");
    return right.ok
      ? this.morphology(this.context().native.ml_region_minkowski_sum, left.value, right.value)
      : right;
  }

  offset(element: StructuringElement): Result<MorphologyResult> {
    return this.withElement(element, this.context().native.ml_region_offset);
  }

  inset(element: StructuringElement): Result<MorphologyResult> {
    return this.withElement(element, this.context().native.ml_region_inset);
  }

  open(element: StructuringElement): Result<MorphologyResult> {
    return this.withElement(element, this.context().native.ml_region_open);
  }

  close(element: StructuringElement): Result<MorphologyResult> {
    return this.withElement(element, this.context().native.ml_region_close);
  }

  private binary(other: Region, operation: NativeApi["ml_region_union"]): Result<Region> {
    const left = this.handle();
    if (!left.ok) {
      return left;
    }
    const right = borrowResource(other, this.context(), "region");
    if (!right.ok) {
      return right;
    }
    const produced = produceHandle((output, obstruction) =>
      operation(left.value, right.value, output, obstruction),
    );
    return produced.ok ? success(createRegion(this.context(), produced.value)) : produced;
  }

  private withElement(
    element: StructuringElement,
    operation: NativeApi["ml_region_offset"],
  ): Result<MorphologyResult> {
    const region = this.handle();
    if (!region.ok) {
      return region;
    }
    const structuringElement = borrowResource(element, this.context(), "structuring-element");
    return structuringElement.ok
      ? this.morphology(operation, structuringElement.value, region.value)
      : structuringElement;
  }

  private morphology(
    operation: NativeApi["ml_region_minkowski_sum"],
    first: NativeHandle,
    second: NativeHandle,
  ): Result<MorphologyResult> {
    const produced = produceMorphology((output, receipt, obstruction) =>
      operation(first, second, output, receipt, obstruction),
    );
    return produced.ok
      ? success({ region: createRegion(this.context(), produced.value.handle), receipt: produced.value.receipt })
      : produced;
  }

  private counts(handle: NativeHandle): Result<readonly [number, number, number]> {
    const componentCount: NativeInteger[] = [0];
    const loopCount: NativeInteger[] = [0];
    const pointCount: NativeInteger[] = [0];
    const counted = callStatus((obstruction) =>
      this.context().native.ml_region_counts(
        handle,
        componentCount,
        loopCount,
        pointCount,
        obstruction,
      ),
    );
    if (!counted.ok) {
      return counted;
    }
    const components = toSafeNumber(componentCount[0]);
    const loops = toSafeNumber(loopCount[0]);
    const points = toSafeNumber(pointCount[0]);
    if (!components.ok) {
      return failure(components.error);
    }
    if (!loops.ok) {
      return failure(loops.error);
    }
    if (!points.ok) {
      return failure(points.error);
    }
    return success([components.value, loops.value, points.value]);
  }

  private measureWithCapacity(handle: NativeHandle, capacity: number): Result<RegionValuations> {
    return captureNativeCall(() => {
      const euler: NativeInteger[] = [0];
      const areaRatio = Buffer.alloc(capacity);
      const areaBytes: NativeInteger[] = [0];
      const perimeterLower = [0];
      const perimeterUpper = [0];
      const obstruction: NativeObstruction = {};
      const status = this.context().native.ml_region_measure(
        handle,
        euler,
        areaRatio,
        capacity,
        areaBytes,
        perimeterLower,
        perimeterUpper,
        obstruction,
      );
      if (status === ML_STATUS_BUFFER_TOO_SMALL && obstruction.code === ML_OBSTRUCTION_BUFFER_TOO_SMALL) {
        const required = toSafeNumber(areaBytes[0]);
        return required.ok ? this.measureWithCapacity(handle, required.value + 1) : required;
      }
      const measured = decodeStatus(status, obstruction);
      if (!measured.ok) {
        return measured;
      }
      const byteCount = toSafeNumber(areaBytes[0]);
      if (!byteCount.ok) {
        return byteCount;
      }
      const ratio = areaRatio.subarray(0, byteCount.value).toString("ascii");
      const [numerator, denominator, remainder] = ratio.split("/");
      if (numerator === undefined || denominator === undefined || remainder !== undefined) {
        return failure({ kind: "invalid-native-result", message: "Moonlight returned a malformed exact-area ratio" });
      }
      return success({
        eulerCharacteristic: toBigInt(euler[0]),
        area: { numerator: BigInt(numerator), denominator: BigInt(denominator) },
        perimeterBounds: [perimeterLower[0] ?? 0, perimeterUpper[0] ?? 0],
      });
    });
  }
}
