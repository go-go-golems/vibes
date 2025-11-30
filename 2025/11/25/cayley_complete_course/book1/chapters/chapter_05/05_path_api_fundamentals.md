
# Chapter 5: Path API Fundamentals

Now that you have a solid understanding of the quad data model and how to persist your graph, it is time to master the art of querying. Cayley's **Path API** is a powerful and expressive tool for traversing your graph and extracting meaningful information. This chapter will introduce you to the fundamental building blocks of the Path API, from starting your queries to traversing relationships and filtering results.

## The Path Object

The central concept in the Path API is the **path object**. A path represents a traversal through the graph, starting from a set of nodes and following a sequence of predicates. The Path API provides a fluent, chainable interface for building up these paths step by step.

Every query starts by creating a path object. The most common way to do this is with `cayley.StartPath`, which takes the graph store and an optional set of starting nodes as arguments.

```go
// Start a path from all nodes in the graph
p := cayley.StartPath(store)

// Start a path from a specific node
p := cayley.StartPath(store, quad.String("Alice"))
```

Once you have a path object, you can chain methods to it to define your traversal.

## Traversal: Out(), In(), and Both()

The core of any graph query is traversing relationships. The Path API provides three primary methods for this:

*   `Out(predicates ...quad.Value)`: Follows predicates in the forward direction, from subject to object.
*   `In(predicates ...quad.Value)`: Follows predicates in the reverse direction, from object to subject.
*   `Both(predicates ...quad.Value)`: Follows predicates in both directions.

Let's consider a simple social network graph:

```nquads
<Alice> <knows> <Bob> .
<Charlie> <knows> <Alice> .
```

To find everyone whom Alice knows, we would use `Out()`:

```go
p := cayley.StartPath(store, quad.IRI("Alice")).Out(quad.IRI("knows"))
// This path now points to Bob
```

To find everyone who knows Alice, we would use `In()`:

```go
p := cayley.StartPath(store, quad.IRI("Alice")).In(quad.IRI("knows"))
// This path now points to Charlie
```

`Both()` is useful for symmetric relationships, like `sibling_of` or `married_to`. It allows you to find all related nodes regardless of the direction of the quad.

## Filtering: Has() and HasR()

Often, you will want to filter the nodes in your path based on their properties. The `Has()` and `HasR()` methods allow you to do this.

*   `Has(predicate quad.Value, object quad.Value)`: Filters the nodes in the current path, keeping only those that have the given predicate and object.
*   `HasR(predicate quad.Value, subject quad.Value)`: The reverse of `Has()`. It filters nodes that are the object of a quad with the given predicate and subject.

Imagine we have a graph of people with their ages:

```nquads
<Alice> <age> 25 .
<Bob> <age> 30 .
<Charlie> <age> 25 .
```

To find everyone who is 25 years old, we can start from all nodes and then filter with `Has()`:

```go
p := cayley.StartPath(store).Has(quad.IRI("age"), quad.Int(25))
// This path now points to Alice and Charlie
```

`Has()` is a powerful tool for selecting nodes based on their properties before you continue your traversal.

## Collecting Results: All(), GetLimit(), and Iterators

Once you have constructed your path, you need to retrieve the results. The Path API provides several ways to do this.

*   `All()`: Executes the query and returns all results as a slice of `quad.Value`. This is convenient for small result sets, but it can consume a lot of memory for large queries.

    ```go
    results, err := p.All()
    ```

*   `GetLimit(n int)`: Similar to `All()`, but it limits the number of results returned to `n`.

*   **Iterators**: For large result sets, using an iterator is the most efficient approach. An iterator allows you to process the results one by one, without loading them all into memory at once. The `Iterate()` method returns an iterator object, which you can then use with methods like `EachValue()`.

    ```go
    err := p.Iterate(nil).EachValue(nil, func(value quad.Value) error {
        fmt.Println(quad.NativeOf(value))
        return nil
    })
    ```

Using iterators is the recommended way to process query results in production applications, as it provides better performance and memory management.

By combining these fundamental building blocks—starting paths, traversing relationships, filtering nodes, and collecting results—you can construct a wide variety of powerful queries to explore and analyze your graph data. The exercises for this chapter will give you hands-on practice with each of these concepts.

---

### References

[1] Cayley Documentation. "Path API." https://cayley.gitbook.io/cayley/query-languages/path-api
