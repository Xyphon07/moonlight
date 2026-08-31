import { failure, success, type ResourceKind, type Result } from "../failure.js";
import type { OwnedResource } from "../resource.js";
import type { NativeApi, NativeHandle } from "./native.generated.js";

export interface RuntimeContext {
  readonly native: NativeApi;
  readonly identity: symbol;
}

interface FinalizerState {
  readonly free: (handle: NativeHandle) => void;
  readonly handle: NativeHandle;
}

const handleFinalizer = new FinalizationRegistry<FinalizerState>(({ free, handle }) => free(handle));
export const resourceAccess: unique symbol = Symbol("moonlight resource access");

export interface InternalResource {
  readonly [resourceAccess]: () => ResourceProjection;
}

interface OpenResource {
  readonly kind: "open";
  readonly handle: NativeHandle;
}

interface DisposedResource {
  readonly kind: "disposed";
}

type ResourceState = OpenResource | DisposedResource;

export interface ResourceProjection {
  readonly context: RuntimeContext;
  readonly resource: ResourceKind;
  readonly state: ResourceState;
}

export abstract class OwnedNativeResource implements InternalResource, OwnedResource {
  readonly #context: RuntimeContext;
  readonly #free: (handle: NativeHandle) => void;
  readonly #resource: ResourceKind;
  #state: ResourceState;

  protected constructor(
    context: RuntimeContext,
    handle: NativeHandle,
    free: (handle: NativeHandle) => void,
    resource: ResourceKind,
  ) {
    this.#context = context;
    this.#free = free;
    this.#resource = resource;
    this.#state = { kind: "open", handle };
    handleFinalizer.register(this, { free, handle }, this);
  }

  get disposed(): boolean {
    return this.#state.kind === "disposed";
  }

  dispose(): void {
    const state = this.#state;
    if (state.kind === "open") {
      handleFinalizer.unregister(this);
      this.#free(state.handle);
      this.#state = { kind: "disposed" };
    }
  }

  [Symbol.dispose](): void {
    this.dispose();
  }

  [resourceAccess](): ResourceProjection {
    return {
      context: this.#context,
      resource: this.#resource,
      state: this.#state,
    };
  }

  protected context(): RuntimeContext {
    return this.#context;
  }

  protected handle(): Result<NativeHandle> {
    return this.#state.kind === "open"
      ? success(this.#state.handle)
      : failure({ kind: "disposed-resource", resource: this.#resource });
  }
}

export function borrowResource(
  value: unknown,
  expectedContext: RuntimeContext,
  expectedResource: ResourceKind,
): Result<NativeHandle> {
  if (!isInternalResource(value)) {
    return failure({ kind: "foreign-runtime", resource: expectedResource });
  }
  const projection = value[resourceAccess]();
  if (projection.context.identity !== expectedContext.identity || projection.resource !== expectedResource) {
    return failure({ kind: "foreign-runtime", resource: expectedResource });
  }
  return projection.state.kind === "open"
    ? success(projection.state.handle)
    : failure({ kind: "disposed-resource", resource: expectedResource });
}

function isInternalResource(value: unknown): value is InternalResource {
  return typeof value === "object" && value !== null && resourceAccess in value;
}
