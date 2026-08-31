import { failure, success, type Result } from "./failure.js";
import { flattenLoops, flattenPoints, produceHandle } from "./internal/call.js";
import {
  ABI_VERSION,
  ML_STATUS_OK,
  loadNativeApi,
} from "./internal/native.generated.js";
import type { RuntimeContext } from "./internal/resource.js";
import { createMesh, type Mesh } from "./mesh.js";
import { createStructuringElement, type StructuringElement } from "./morphology.js";
import { createRegion, type Region } from "./region.js";
import type { Point, PolygonComponent } from "./values.js";
import { decodeAbiStatus } from "./wire.generated.js";

export type NativeLibrarySource = {
  readonly kind: "path";
  readonly path: string;
};

export interface Moonlight {
  readonly abiVersion: number;
  delaunay(points: readonly Point[]): Result<Mesh>;
  region(components: readonly PolygonComponent[]): Result<Region>;
  structuringElement(points: readonly Point[]): Result<StructuringElement>;
}

export function loadMoonlight(source: NativeLibrarySource): Result<Moonlight> {
  if (source.path.length === 0) {
    return failure({
      kind: "native-library-unavailable",
      path: source.path,
      message: "native library path must not be empty",
    });
  }
  try {
    const native = loadNativeApi(source.path);
    const initializationStatus = native.ml_runtime_initialize();
    if (initializationStatus !== ML_STATUS_OK) {
      const status = decodeAbiStatus(initializationStatus);
      return status === undefined
        ? failure({ kind: "unknown-wire-value", vocabulary: "status", value: initializationStatus })
        : failure({ kind: "runtime-initialization-refused", status });
    }
    const observedVersion = native.ml_abi_version();
    if (observedVersion !== ABI_VERSION) {
      return failure({
        kind: "abi-version-mismatch",
        expected: ABI_VERSION,
        observed: observedVersion,
      });
    }
    const context: RuntimeContext = { native, identity: Symbol("moonlight runtime") };
    return success(new MoonlightRuntime(context));
  } catch (cause: unknown) {
    return failure({
      kind: "native-library-unavailable",
      path: source.path,
      message: cause instanceof Error ? cause.message : String(cause),
    });
  }
}

class MoonlightRuntime implements Moonlight {
  readonly abiVersion = ABI_VERSION;
  readonly #context: RuntimeContext;

  constructor(context: RuntimeContext) {
    this.#context = context;
  }

  delaunay(points: readonly Point[]): Result<Mesh> {
    const coordinates = flattenPoints(points);
    const produced = produceHandle((output, obstruction) =>
      this.#context.native.ml_delaunay_f64(
        coordinates,
        coordinates.length / 2,
        output,
        obstruction,
      ),
    );
    return produced.ok ? success(createMesh(this.#context, produced.value)) : produced;
  }

  region(components: readonly PolygonComponent[]): Result<Region> {
    const loops = components.flatMap(({ outer, holes = [] }) => [outer, ...holes]);
    const loopPointCounts = BigUint64Array.from(loops.map((loop) => BigInt(loop.length)));
    const componentLoopCounts = BigUint64Array.from(
      components.map(({ holes = [] }) => BigInt(holes.length + 1)),
    );
    const coordinates = flattenLoops(loops);
    const produced = produceHandle((output, obstruction) =>
      this.#context.native.ml_region_create_f64(
        coordinates,
        coordinates.length / 2,
        loopPointCounts,
        loopPointCounts.length,
        componentLoopCounts,
        componentLoopCounts.length,
        output,
        obstruction,
      ),
    );
    return produced.ok ? success(createRegion(this.#context, produced.value)) : produced;
  }

  structuringElement(points: readonly Point[]): Result<StructuringElement> {
    const coordinates = flattenPoints(points);
    const produced = produceHandle((output, obstruction) =>
      this.#context.native.ml_structuring_element_create_f64(
        coordinates,
        coordinates.length / 2,
        output,
        obstruction,
      ),
    );
    return produced.ok
      ? success(createStructuringElement(this.#context, produced.value))
      : produced;
  }
}
