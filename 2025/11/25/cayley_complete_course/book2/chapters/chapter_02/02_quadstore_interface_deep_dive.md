
# Chapter 2: The QuadStore Interface Deep Dive

As we discussed in the previous chapter, the `QuadStore` interface is the single most important abstraction in Cayley. It is the contract that separates the query engine from the storage layer. By understanding this interface in detail, you will unlock the ability to create your own custom backends and to reason about the performance of existing ones.

This chapter will provide a line-by-line deep dive into the `QuadStore` interface, explaining the purpose and requirements of each method.

## The `QuadStore` Interface Definition

Let's start by looking at the full definition of the `QuadStore` interface, as found in `graph/quadstore.go`:

```go
type QuadStore interface {
    refs.Namer
    QuadIndexer

    ApplyDeltas(in []Delta, opts IgnoreOpts) error
    NewQuadWriter() (quad.WriteCloser, error)

    NodesAllIterator() iterator.Shape
    QuadsAllIterator() iterator.Shape

    Close() error
}
```

The interface is composed of two other interfaces, `refs.Namer` and `QuadIndexer`, and several of its own methods. Let's break these down.

## `refs.Namer`: Translating Between Values and Refs

The `refs.Namer` interface is responsible for translating between the high-level `quad.Value` types that you use in your application and the internal `graph.Ref` types that the storage backend uses.

```go
type Namer interface {
    ValueOf(ctx context.Context, v quad.Value) (Ref, error)
    NameOf(ctx context.Context, id Ref) (quad.Value, error)
}
```

*   `ValueOf(v quad.Value) (Ref, error)`: This method takes a `quad.Value` (like a string or an IRI) and returns the backend's internal reference for it. If the value does not yet exist in the database, the backend should create a new internal reference for it.

*   `NameOf(id Ref) (quad.Value, error)`: This is the reverse operation. It takes an internal `Ref` and returns the corresponding `quad.Value`.

This abstraction is critical. It allows the backend to use any internal representation it wants for its data (e.g., 64-bit integers, pointers, byte slices), while still presenting a consistent view to the query engine.

## `QuadIndexer`: The Heart of Querying

The `QuadIndexer` interface defines the methods for querying quads based on their components.

```go
type QuadIndexer interface {
    Quad(Ref) (quad.Quad, error)
    QuadIterator(quad.Direction, Ref) iterator.Shape
    QuadIteratorSize(ctx context.Context, d quad.Direction, v Ref) (refs.Size, error)
    QuadDirection(id Ref, d quad.Direction) (Ref, error)
    Stats(ctx context.Context, exact bool) (Stats, error)
}
```

*   `Quad(Ref) (quad.Quad, error)`: This method takes a `Ref` that represents a quad and returns the full `quad.Quad` object.

*   `QuadIterator(quad.Direction, Ref) iterator.Shape`: This is the most important query method. It takes a direction (Subject, Predicate, Object, or Label) and a `Ref` for a value, and it returns an iterator that will scan over all quads that have that value in that direction.

*   `QuadIteratorSize(...) (refs.Size, error)`: This method provides an estimated size for a `QuadIterator`. This is crucial for the query optimizer to be able to reorder iterators and create an efficient query plan.

*   `QuadDirection(id Ref, d quad.Direction) (Ref, error)`: This is a convenience method for quickly getting a single component of a quad without having to load the entire quad object.

*   `Stats(...) (Stats, error)`: This method returns basic statistics about the graph, such as the number of nodes and quads.

## Write Operations: `ApplyDeltas` and `NewQuadWriter`

All write operations in Cayley go through one of two methods:

*   `ApplyDeltas(in []Delta, opts IgnoreOpts) error`: This method is used for transactional updates. It takes a slice of `Delta` objects, where each `Delta` represents a quad to be added or deleted. The backend must ensure that all of these operations are applied atomically.

*   `NewQuadWriter() (quad.WriteCloser, error)`: This method is used for efficient bulk loading of data. It returns a `quad.WriteCloser` that can be used to stream large numbers of quads into the database.

## Full Graph Iteration: `NodesAllIterator` and `QuadsAllIterator`

These two methods provide a way to iterate over the entire graph:

*   `NodesAllIterator() iterator.Shape`: Returns an iterator that scans over every unique node in the graph.

*   `QuadsAllIterator() iterator.Shape`: Returns an iterator that scans over every quad in the graph.

These are the building blocks for queries that need to consider the entire graph, such as `g.V().All()`.

## Lifecycle: `Close`

Finally, the `Close()` method is responsible for cleanly shutting down the database, flushing any pending writes to disk, and releasing any resources.

By implementing these methods, you can create a fully functional Cayley backend. In the exercises for this chapter, you will get hands-on experience with this interface by creating a simple wrapper around an existing `QuadStore` that logs all of the calls to its methods. This will give you a clear picture of how the query engine interacts with the storage layer.

---

### References

[1] Cayley GitHub Repository. `graph/quadstore.go`. https://github.com/cayleygraph/cayley/blob/master/graph/quadstore.go

[2] Cayley GitHub Repository. `graph/refs/refs.go`. https://github.com/cayleygraph/cayley/blob/master/graph/refs/refs.go
