import { failure, success, type Result } from "../failure.js";
import type { MinkowskiReceipt, Point } from "../values.js";
import { decodeMinkowskiReceipt, decodeStatus, nativeCallFailure } from "./decode.js";
import type {
  HandleOutput,
  NativeHandle,
  NativeMinkowskiReceipt,
  NativeObstruction,
} from "./native.generated.js";

type HandleOperation = (output: HandleOutput, obstruction: NativeObstruction) => number;
type MorphologyOperation = (
  output: HandleOutput,
  receipt: NativeMinkowskiReceipt,
  obstruction: NativeObstruction,
) => number;

export interface NativeMorphologyResult {
  readonly handle: NativeHandle;
  readonly receipt: MinkowskiReceipt;
}

export function produceHandle(operation: HandleOperation): Result<NativeHandle> {
  return captureNativeCall(() => {
    const output: HandleOutput = [null];
    const obstruction: NativeObstruction = {};
    const status = decodeStatus(operation(output, obstruction), obstruction);
    if (!status.ok) {
      return status;
    }
    return requireHandle(output, "handle");
  });
}

export function produceMorphology(operation: MorphologyOperation): Result<NativeMorphologyResult> {
  return captureNativeCall(() => {
    const output: HandleOutput = [null];
    const receipt: NativeMinkowskiReceipt = {};
    const obstruction: NativeObstruction = {};
    const status = decodeStatus(operation(output, receipt, obstruction), obstruction);
    if (!status.ok) {
      return status;
    }
    const handle = requireHandle(output, "region handle");
    if (!handle.ok) {
      return handle;
    }
    const decodedReceipt = decodeMinkowskiReceipt(receipt);
    return decodedReceipt.ok
      ? success({ handle: handle.value, receipt: decodedReceipt.value })
      : decodedReceipt;
  });
}

export function callStatus(operation: (obstruction: NativeObstruction) => number): Result<void> {
  return captureNativeCall(() => {
    const obstruction: NativeObstruction = {};
    return decodeStatus(operation(obstruction), obstruction);
  });
}

export function captureNativeCall<Value>(operation: () => Result<Value>): Result<Value> {
  try {
    return operation();
  } catch (cause: unknown) {
    return failure(nativeCallFailure(cause));
  }
}

export function flattenPoints(points: readonly Point[]): Float64Array {
  return Float64Array.from(points.flatMap(([x, y]) => [x, y]));
}

export function flattenLoops(loops: readonly (readonly Point[])[]): Float64Array {
  return Float64Array.from(loops.flatMap((loop) => loop.flatMap(([x, y]) => [x, y])));
}

function requireHandle(output: HandleOutput, description: string): Result<NativeHandle> {
  const handle = output[0];
  return handle === null || handle === undefined
    ? failure({ kind: "invalid-native-result", message: `Moonlight returned success without a ${description}` })
    : success(handle);
}
