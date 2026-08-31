import type { MinkowskiOperation } from "./wire.generated.js";

export type Point = readonly [x: number, y: number];
export type Triangle = readonly [first: number, second: number, third: number];

export interface PolygonComponent {
  readonly outer: readonly Point[];
  readonly holes?: readonly (readonly Point[])[];
}

export interface ExactRational {
  readonly numerator: bigint;
  readonly denominator: bigint;
}

export interface RegionValuations {
  readonly eulerCharacteristic: bigint;
  readonly area: ExactRational;
  readonly perimeterBounds: readonly [lower: number, upper: number];
}

export interface MinkowskiReceipt {
  readonly operation: MinkowskiOperation;
  readonly inputComponents: bigint;
  readonly convexPieces: bigint;
  readonly generatedPieces: bigint;
  readonly generatedConvolutionEdges: bigint;
  readonly overlayPasses: bigint;
  readonly exactCrossings: bigint;
  readonly outputCells: bigint;
  readonly exactCoordinateBitGrowth: bigint;
}
