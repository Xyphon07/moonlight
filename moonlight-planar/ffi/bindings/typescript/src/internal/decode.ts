import { failure, success, type MoonlightFailure, type Obstruction, type Result } from "../failure.js";
import type { MinkowskiReceipt, Point } from "../values.js";
import {
  decodeAbiStatus,
  decodeCoordinateError,
  decodeMinkowskiOperation,
  decodeObstructionCode,
  decodeRegionLocation,
  type RegionLocation,
} from "../wire.generated.js";
import {
  ML_COORDINATE_ERROR_NONE,
  ML_OBSTRUCTION_NONE,
  ML_STATUS_OK,
  type NativeInteger,
  type NativeMinkowskiReceipt,
  type NativeObstruction,
} from "./native.generated.js";

export function decodeStatus(status: number, obstruction: NativeObstruction): Result<void> {
  if (status === ML_STATUS_OK) {
    return success(undefined);
  }
  const decodedStatus = decodeAbiStatus(status);
  if (decodedStatus === undefined) {
    return failure({ kind: "unknown-wire-value", vocabulary: "status", value: status });
  }
  const codeValue = obstruction.code ?? ML_OBSTRUCTION_NONE;
  const code = decodeObstructionCode(codeValue);
  if (code === undefined) {
    return failure({ kind: "unknown-wire-value", vocabulary: "obstruction", value: codeValue });
  }
  const coordinateErrorValue = obstruction.coordinate_error ?? ML_COORDINATE_ERROR_NONE;
  const coordinateError = decodeCoordinateError(coordinateErrorValue);
  if (coordinateError === undefined) {
    return failure({ kind: "unknown-wire-value", vocabulary: "coordinate-error", value: coordinateErrorValue });
  }
  const decoded: Obstruction = {
    status: decodedStatus,
    code,
    coordinateError,
    inputIndex: toBigInt(obstruction.input_index),
    firstIndex: toBigInt(obstruction.first_index),
    secondIndex: toBigInt(obstruction.second_index),
    firstValue: obstruction.first_value ?? 0,
    secondValue: obstruction.second_value ?? 0,
    point: [obstruction.point_x ?? 0, obstruction.point_y ?? 0],
    message: obstructionMessage(obstruction),
  };
  return failure({ kind: "abi-obstruction", obstruction: decoded });
}

export function decodeLocation(value: number | undefined): Result<RegionLocation> {
  const wireValue = value ?? -1;
  const location = decodeRegionLocation(wireValue);
  return location === undefined
    ? failure({ kind: "unknown-wire-value", vocabulary: "region-location", value: wireValue })
    : success(location);
}

export function decodeMinkowskiReceipt(receipt: NativeMinkowskiReceipt): Result<MinkowskiReceipt> {
  const operationValue = receipt.operation ?? -1;
  const operation = decodeMinkowskiOperation(operationValue);
  if (operation === undefined) {
    return failure({ kind: "unknown-wire-value", vocabulary: "minkowski-operation", value: operationValue });
  }
  return success({
    operation,
    inputComponents: toBigInt(receipt.input_components),
    convexPieces: toBigInt(receipt.convex_pieces),
    generatedPieces: toBigInt(receipt.generated_pieces),
    generatedConvolutionEdges: toBigInt(receipt.generated_convolution_edges),
    overlayPasses: toBigInt(receipt.overlay_passes),
    exactCrossings: toBigInt(receipt.exact_crossings),
    outputCells: toBigInt(receipt.output_cells),
    exactCoordinateBitGrowth: toBigInt(receipt.exact_coordinate_bit_growth),
  });
}

export function toBigInt(value: NativeInteger | undefined): bigint {
  return value === undefined ? 0n : BigInt(value);
}

export function toSafeNumber(value: NativeInteger | undefined): Result<number> {
  const exact = toBigInt(value);
  const projected = Number(exact);
  return Number.isSafeInteger(projected)
    ? success(projected)
    : failure({ kind: "unsafe-integer", value: exact });
}

export function nativeCallFailure(cause: unknown): MoonlightFailure {
  return {
    kind: "native-call-failed",
    message: cause instanceof Error ? cause.message : String(cause),
  };
}

export function pointsFromCoordinates(coordinates: Float64Array, count: number): readonly Point[] {
  return Array.from({ length: count }, (_unused, index): Point => [
    coordinates[index * 2] ?? 0,
    coordinates[index * 2 + 1] ?? 0,
  ]);
}

function obstructionMessage(obstruction: NativeObstruction): string {
  const message = obstruction.message;
  if (message === undefined) {
    return "Moonlight ABI obstruction";
  }
  if (typeof message === "string") {
    return message.split("\0", 1)[0] ?? "";
  }
  const terminator = message.indexOf(0);
  const length = terminator === -1 ? message.length : terminator;
  return new TextDecoder().decode(Uint8Array.from(message.slice(0, length)));
}
