# Construction

Moonlight constructors validate once and return the owning carriers directly.
There is no deferred linear-algebra expression tree.

## Dense matrices

```haskell
matrixValue <-
  matrixRows @2 @2
    [ [1.0, 2.0],
      [3.0, 4.0]
    ]
```

`matrixRows` is row-major. Type-level dimensions are checked against the nested
rows, including zero-row matrices whose column count cannot be inferred from the
value alone. Treat `DenseRows` and nested-list construction as validated
authoring/projection surfaces; they preserve shape failures, not dense hot-path
performance.

For dynamic dimensions:

```haskell
matrixValue <-
  dynMatrixFromRows
    [ [1.0, 2.0],
      [3.0, 4.0]
    ]
```

## Structured sparse matrices

```haskell
sparseOperator <-
  tridiagonalCSR
    [2.0, 2.0, 2.0]
    [-1.0, -1.0]
```

```haskell
pathOperator <-
  pathLaplacianCSR 8
```

```haskell
graphOperator <-
  graphLaplacianCSR
    ["a", "b", "c"]
    [ GraphEdge "a" "b" 1.0,
      GraphEdge "b" "c" 2.0
    ]
```

The graph vertex list defines matrix row and column order. Edges are undirected;
parallel and reversed edges are combined. Weights must be finite and
non-negative.

## Matrix-free eigen requests

```haskell
count <- mkPositiveCount 4
operator <- declaredSelfAdjointVectorLinearOperator dimension applyA
let config =
      withEigenFallbackInitialVector seed defaultEigenSolveConfig
eigenvalues <- solveEigenRequest config operator (EigenvaluesRequest SmallestEigenvalues count)
```

```haskell
count <- mkPositiveCount 4
operator <- selfAdjointCSRLinearOperator csr
eigenpairs <- solveEigenRequest defaultEigenSolveConfig operator (EigenpairsRequest SmallestEigenvalues count)
```

The request lives above Krylov: values-only requests can avoid ambient vector
lifting, while eigenpair requests return a contiguous `Eigenpairs` payload.

## Selected structured spectra

```haskell
count <- mkPositiveCount 4
operator <- pathLaplacianLinearOperator 10000
eigenvalues <-
  solveEigenRequest
    defaultEigenSolveConfig
    operator
    (EigenvaluesRequest SmallestEigenvalues count)
```

This is the package's flagship hot path: Moonlight recognizes certified operator structure and computes only the requested spectral data. Use this route when downstream code needs a small spectral slice; do not build dense rows just to throw most of the spectrum away.

## Force networks

```haskell
forceNetwork <-
  network
    [ support "a" (Vec3 0.0 0.0 0.0),
      load
        "b"
        (Vec3 0.0 1.0 0.0)
        (Vec3 0.0 (-10.0) 0.0),
      member "a" "b"
    ]
```

Declarations may occur in any order. Repeated loads add, repeated members are
idempotent, and repeated node declarations must agree on position. `network`
returns a validated `ForceNetwork` directly.
