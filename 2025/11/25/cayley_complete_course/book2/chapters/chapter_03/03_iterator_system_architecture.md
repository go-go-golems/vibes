
# Chapter 3: Iterator System Architecture

If the `QuadStore` interface is the foundation of Cayley, then the iterator system is the engine that drives it. Every query, no matter how complex, is executed as a tree of iterators. Understanding this system is the key to understanding Cayley's performance and to creating your own custom query operations.

This chapter will explore the architecture of the iterator system, from the base interfaces to the composition patterns that make it so powerful.

## The Iterator Interfaces

At the core of the iterator system are two fundamental interfaces, `Scanner` and `Index`, which are both built on top of a `Base` interface.

### The `Base` Interface

```go
type Base interface {
    String() string
    TagResults(map[string]refs.Ref)
    Result() refs.Ref
    NextPath(ctx context.Context) bool
    Err() error
    Close() error
}
```

*   `Result()`: Returns the current value that the iterator is pointing to.
*   `NextPath()`: Advances the iterator to the next valid path. A path is a complete solution to a sub-query.
*   `TagResults()`: Associates tags with the current result.
*   `Err()` and `Close()`: Standard error handling and resource cleanup.

### The `Scanner` Interface

```go
type Scanner interface {
    Base
    Next(ctx context.Context) bool
}
```

A `Scanner` is an iterator that can be advanced one step at a time using the `Next()` method. This is used for iterators that perform a sequential scan, such as `AllIterator`.

### The `Index` Interface

```go
type Index interface {
    Base
    Contains(ctx context.Context, v refs.Ref) bool
}
```

An `Index` is an iterator that can efficiently check for the existence of a specific value using the `Contains()` method. This is used for iterators that represent a fixed set of values or that can perform fast lookups, such as `FixedIterator` or `HasIterator`.

## Iterator Composition

The real power of the iterator system comes from composition. Iterators can be combined to create more complex query operations. The two most important composition patterns are `And` and `Or`.

### The `And` Iterator

The `And` iterator takes two sub-iterators and returns their intersection. It works by iterating through one of its sub-iterators and, for each value, checking if that value is contained in the other sub-iterator.

This is where the distinction between `Scanner` and `Index` becomes critical. The `And` iterator is most efficient when its right-hand side is an `Index`, as this allows for fast lookups. The query optimizer will try to arrange the iterator tree to take advantage of this.

### The `Or` Iterator

The `Or` iterator takes two sub-iterators and returns their union. It simply iterates through both sub-iterators and returns all of their unique values.

## The Query Optimizer

When you write a query, Cayley's query optimizer analyzes it and creates an execution plan in the form of an iterator tree. The optimizer's goal is to create the most efficient tree possible.

It does this by applying a series of transformation rules, such as:

*   **Reordering `And` iterators**: As mentioned above, the optimizer will try to place the most restrictive (i.e., smallest) iterator on the left-hand side of an `And` and an `Index` on the right-hand side.

*   **Pushing down filters**: The optimizer will try to apply filtering operations (like `Has`) as early as possible in the query plan to reduce the amount of data that needs to be processed by later stages.

*   **Choosing the best join strategy**: For complex queries with multiple joins, the optimizer will try to choose the most efficient join order.

The optimizer relies on the `QuadIteratorSize` method of the `QuadStore` to get estimates of the size of different iterators. Accurate size estimates are crucial for the optimizer to make good decisions.

## Iterator Lifecycle

It is essential to manage the lifecycle of your iterators correctly to avoid resource leaks.

1.  **Creation**: Iterators are created by the `QuadStore` or by other iterators.
2.  **Execution**: The query engine calls `Next()` or `Contains()` to advance the iterators and get results.
3.  **Cleanup**: The `Close()` method must be called on every iterator to release any resources it holds (such as file handles or network connections).

This is typically handled automatically by the query engine, but if you are creating your own iterators, you must be careful to implement the `Close()` method correctly and to call it on your sub-iterators.

By understanding the iterator system, you can reason about the performance of your queries, identify bottlenecks, and even create your own custom query operations. In the exercises for this chapter, you will implement your own custom filter iterator, giving you a practical understanding of how these powerful components work.

---

### References

[1] Cayley GitHub Repository. `graph/iterator/iterator.go`. https://github.com/cayleygraph/cayley/blob/master/graph/iterator/iterator.go

[2] Cayley Documentation. "Query Optimization." https://cayley.gitbook.io/cayley/advanced-topics/query-optimization
