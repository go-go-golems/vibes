
# Chapter 7: Embedding Cayley as a Library

While Cayley can be run as a standalone server, one of its most powerful features is the ability to be embedded as a library directly into your Go applications. This gives you the full power of a graph database without the overhead of managing a separate service.

This chapter will walk you through the process of embedding Cayley, from initialization to querying and lifecycle management.

## Why Embed Cayley?

Embedding Cayley offers several advantages:

*   **Simplicity**: No need to manage a separate database server, which simplifies deployment and operations.
*   **Performance**: Queries are executed in-process, avoiding network latency.
*   **Flexibility**: You can programmatically configure and control every aspect of the database.
*   **Integration**: Tightly integrate the graph database with your application logic.

## Initializing Cayley

The first step is to create a new Cayley graph instance. You can do this by calling `cayley.NewGraph` with the name of the backend you want to use and the path to the database file (for persistent stores).

```go
import "github.com/cayleygraph/cayley"

// For a persistent BoltDB store
handle, err := cayley.NewGraph("bolt", "/path/to/my.db", nil)
if err != nil {
    log.Fatal(err)
}
defer handle.Close()

// For an in-memory store
memHandle, err := cayley.NewGraph("memstore", "", nil)
```

The third argument to `NewGraph` is an `Options` map that can be used to pass backend-specific configuration.

## Basic Operations

Once you have a `graph.Handle`, you can perform all of the standard Cayley operations:

*   **Adding Quads**: Use the `AddQuad` or `AddQuads` methods to add data to the graph.

    ```go
    err = handle.AddQuad(quad.Make("alice", "knows", "bob", nil))
    ```

*   **Querying**: Create a new query by calling `cayley.StartPath` with the graph handle.

    ```go
    p := cayley.StartPath(handle, quad.String("alice")).Out(quad.String("knows"))
    ```

*   **Iterating Results**: Use an iterator to get the results of your query.

    ```go
    it, _ := p.Iterate(context.Background()).All()
    for _, val := range it {
        fmt.Println(handle.NameOf(val))
    }
    ```

## Lifecycle Management

When you embed Cayley, your application is responsible for managing its lifecycle. This means:

*   **Initialization**: Creating the graph handle when your application starts.
*   **Shutdown**: Calling `handle.Close()` to cleanly shut down the database when your application exits. This is crucial for persistent stores to ensure that all data is flushed to disk.

A common pattern is to create the graph handle in your `main` function and use a `defer` statement to ensure that `Close()` is called.

## Programmatic Configuration

Embedding Cayley gives you full programmatic control over its configuration. You can use the `Options` map to tune backend-specific settings.

```go
opts := make(graph.Options)
opts["read_only"] = true

handle, err := cayley.NewGraph("bolt", "/path/to/my.db", opts)
```

This allows you to create dynamic, environment-aware configurations without having to rely on static config files.

In the exercises for this chapter, you will build a simple REST API that embeds Cayley to provide a graph-based data service. This will give you hands-on experience with all aspects of embedding Cayley, from initialization to querying and lifecycle management.

---

### References

[1] Cayley GitHub Repository. `cayley.go`. https://github.com/cayleygraph/cayley/blob/master/cayley.go
