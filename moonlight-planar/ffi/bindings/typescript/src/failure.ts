import type {
  AbiStatus,
  CoordinateError,
  ObstructionCode,
} from "./wire.generated.js";

import type { Point } from "./values.js";

export type Result<Value, Failure = MoonlightFailure> =
  | { readonly ok: true; readonly value: Value }
  | { readonly ok: false; readonly error: Failure };

export interface Obstruction {
  readonly status: AbiStatus;
  readonly code: ObstructionCode;
  readonly coordinateError: CoordinateError;
  readonly inputIndex: bigint;
  readonly firstIndex: bigint;
  readonly secondIndex: bigint;
  readonly firstValue: number;
  readonly secondValue: number;
  readonly point: Point;
  readonly message: string;
}

export type ResourceKind = "mesh" | "region" | "structuring-element";

export type MoonlightFailure =
  | {
      readonly kind: "native-library-unavailable";
      readonly path: string;
      readonly message: string;
    }
  | {
      readonly kind: "runtime-initialization-refused";
      readonly status: AbiStatus;
    }
  | {
      readonly kind: "abi-version-mismatch";
      readonly expected: number;
      readonly observed: number;
    }
  | {
      readonly kind: "abi-obstruction";
      readonly obstruction: Obstruction;
    }
  | {
      readonly kind: "disposed-resource";
      readonly resource: ResourceKind;
    }
  | {
      readonly kind: "foreign-runtime";
      readonly resource: ResourceKind;
    }
  | {
      readonly kind: "unsafe-integer";
      readonly value: bigint;
    }
  | {
      readonly kind: "unknown-wire-value";
      readonly vocabulary: "status" | "obstruction" | "coordinate-error" | "region-location" | "minkowski-operation";
      readonly value: number;
    }
  | {
      readonly kind: "invalid-native-result";
      readonly message: string;
    }
  | {
      readonly kind: "native-call-failed";
      readonly message: string;
    };

export function success<Value>(value: Value): Result<Value> {
  return { ok: true, value };
}

export function failure<Value = never>(error: MoonlightFailure): Result<Value> {
  return { ok: false, error };
}

export function collectResults<Value>(results: readonly Result<Value>[]): Result<readonly Value[]> {
  const refused = results.find((result) => !result.ok);
  return refused?.ok === false
    ? failure(refused.error)
    : success(results.flatMap((result) => (result.ok ? [result.value] : [])));
}

export function formatMoonlightFailure(error: MoonlightFailure): string {
  switch (error.kind) {
    case "native-library-unavailable":
      return `cannot load Moonlight native library at ${error.path}: ${error.message}`;
    case "runtime-initialization-refused":
      return `Moonlight runtime initialization refused with ${error.status}`;
    case "abi-version-mismatch":
      return `Moonlight ABI ${error.observed} is incompatible with expected ABI ${error.expected}`;
    case "abi-obstruction":
      return error.obstruction.message || `${error.obstruction.status}: ${error.obstruction.code}`;
    case "disposed-resource":
      return `${error.resource} is disposed`;
    case "foreign-runtime":
      return `${error.resource} belongs to another Moonlight runtime`;
    case "unsafe-integer":
      return `Moonlight count ${error.value} exceeds JavaScript's safe integer range`;
    case "unknown-wire-value":
      return `Moonlight returned unknown ${error.vocabulary} value ${error.value}`;
    case "invalid-native-result":
      return error.message;
    case "native-call-failed":
      return error.message;
  }
}
