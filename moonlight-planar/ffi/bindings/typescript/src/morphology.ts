import type { NativeHandle } from "./internal/native.generated.js";
import { OwnedNativeResource, type RuntimeContext } from "./internal/resource.js";
import type { OwnedResource } from "./resource.js";

declare const structuringElementBrand: unique symbol;

export interface StructuringElement extends OwnedResource {
  readonly [structuringElementBrand]: "structuring-element";
}

/** @internal */
export function createStructuringElement(
  context: RuntimeContext,
  handle: NativeHandle,
): StructuringElement {
  return new StructuringElementResource(context, handle);
}

class StructuringElementResource extends OwnedNativeResource implements StructuringElement {
  declare readonly [structuringElementBrand]: "structuring-element";

  constructor(context: RuntimeContext, handle: NativeHandle) {
    super(context, handle, context.native.ml_structuring_element_free, "structuring-element");
  }
}
