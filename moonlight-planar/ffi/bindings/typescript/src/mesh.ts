import { success, type Result } from "./failure.js";
import { callStatus, flattenPoints, produceHandle } from "./internal/call.js";
import { pointsFromCoordinates, toSafeNumber } from "./internal/decode.js";
import type { NativeApi, NativeHandle, NativeInteger } from "./internal/native.generated.js";
import { borrowResource, OwnedNativeResource, type RuntimeContext } from "./internal/resource.js";
import type { OwnedResource } from "./resource.js";
import type { Point, Triangle } from "./values.js";

export interface Mesh extends OwnedResource {
  vertexCount(): Result<number>;
  triangleCount(): Result<number>;
  vertices(): Result<readonly Point[]>;
  triangles(): Result<readonly Triangle[]>;
  insertMany(points: readonly Point[]): Result<Mesh>;
  siteUnion(other: Mesh): Result<Mesh>;
  siteIntersection(other: Mesh): Result<Mesh>;
  siteDifference(other: Mesh): Result<Mesh>;
  siteSymmetricDifference(other: Mesh): Result<Mesh>;
}

/** @internal */
export function createMesh(context: RuntimeContext, handle: NativeHandle): Mesh {
  return new MeshResource(context, handle);
}

class MeshResource extends OwnedNativeResource implements Mesh {
  constructor(context: RuntimeContext, handle: NativeHandle) {
    super(context, handle, context.native.ml_mesh_free, "mesh");
  }

  vertexCount(): Result<number> {
    return this.count(this.context().native.ml_mesh_vertex_count);
  }

  triangleCount(): Result<number> {
    return this.count(this.context().native.ml_mesh_triangle_count);
  }

  vertices(): Result<readonly Point[]> {
    const handle = this.handle();
    if (!handle.ok) {
      return handle;
    }
    const count = this.vertexCount();
    if (!count.ok) {
      return count;
    }
    const coordinates = new Float64Array(count.value * 2);
    const written: NativeInteger[] = [0];
    const copied = callStatus((obstruction) =>
      this.context().native.ml_mesh_copy_vertices_f64(
        handle.value,
        coordinates,
        count.value,
        written,
        obstruction,
      ),
    );
    if (!copied.ok) {
      return copied;
    }
    const writtenCount = toSafeNumber(written[0]);
    return writtenCount.ok
      ? success(pointsFromCoordinates(coordinates, writtenCount.value))
      : writtenCount;
  }

  triangles(): Result<readonly Triangle[]> {
    const handle = this.handle();
    if (!handle.ok) {
      return handle;
    }
    const count = this.triangleCount();
    if (!count.ok) {
      return count;
    }
    const triangles = new Uint32Array(count.value * 3);
    const written: NativeInteger[] = [0];
    const copied = callStatus((obstruction) =>
      this.context().native.ml_mesh_copy_triangles_u32(
        handle.value,
        triangles,
        count.value,
        written,
        obstruction,
      ),
    );
    if (!copied.ok) {
      return copied;
    }
    const writtenCount = toSafeNumber(written[0]);
    return writtenCount.ok
      ? success(
          Array.from({ length: writtenCount.value }, (_unused, index): Triangle => [
            triangles[index * 3] ?? 0,
            triangles[index * 3 + 1] ?? 0,
            triangles[index * 3 + 2] ?? 0,
          ]),
        )
      : writtenCount;
  }

  insertMany(points: readonly Point[]): Result<Mesh> {
    const handle = this.handle();
    if (!handle.ok) {
      return handle;
    }
    const coordinates = flattenPoints(points);
    const produced = produceHandle((output, obstruction) =>
      this.context().native.ml_mesh_insert_many_f64(
        handle.value,
        coordinates,
        coordinates.length / 2,
        output,
        obstruction,
      ),
    );
    return produced.ok ? success(createMesh(this.context(), produced.value)) : produced;
  }

  siteUnion(other: Mesh): Result<Mesh> {
    return this.binary(other, this.context().native.ml_mesh_site_union);
  }

  siteIntersection(other: Mesh): Result<Mesh> {
    return this.binary(other, this.context().native.ml_mesh_site_intersection);
  }

  siteDifference(other: Mesh): Result<Mesh> {
    return this.binary(other, this.context().native.ml_mesh_site_difference);
  }

  siteSymmetricDifference(other: Mesh): Result<Mesh> {
    return this.binary(other, this.context().native.ml_mesh_site_symmetric_difference);
  }

  private binary(other: Mesh, operation: NativeApi["ml_mesh_site_union"]): Result<Mesh> {
    const left = this.handle();
    if (!left.ok) {
      return left;
    }
    const right = borrowResource(other, this.context(), "mesh");
    if (!right.ok) {
      return right;
    }
    const produced = produceHandle((output, obstruction) =>
      operation(left.value, right.value, output, obstruction),
    );
    return produced.ok ? success(createMesh(this.context(), produced.value)) : produced;
  }

  private count(operation: NativeApi["ml_mesh_vertex_count"]): Result<number> {
    const handle = this.handle();
    if (!handle.ok) {
      return handle;
    }
    const output: NativeInteger[] = [0];
    const counted = callStatus((obstruction) => operation(handle.value, output, obstruction));
    return counted.ok ? toSafeNumber(output[0]) : counted;
  }
}
