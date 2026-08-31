export interface OwnedResource extends Disposable {
  readonly disposed: boolean;
  dispose(): void;
}
