
# Chapter 7: Gizmo Query Language

While the Path API provides a powerful and idiomatic way to query your graph from within your Go programs, Cayley also offers another query language: **Gizmo**. Gizmo is a JavaScript-based query language that provides a more dynamic and interactive way to explore your data. This chapter will introduce you to the fundamentals of Gizmo, its relationship to the Path API, and how to use it both from the command line and within your Go applications.

## Gizmo vs. Path API

Gizmo and the Path API are two different ways of expressing the same underlying query concepts. In fact, Gizmo is essentially a JavaScript wrapper around the same Path API methods you have already learned. The key difference is the environment in which you use them.

| Feature | Path API | Gizmo |
| :--- | :--- | :--- |
| **Language** | Go | JavaScript |
| **Environment** | Compiled Go programs | Interactive REPL, HTTP API |
| **Use Case** | Programmatic, static queries | Interactive exploration, dynamic queries |
| **Type Safety** | Compile-time type checking | Dynamic typing |

The Path API is ideal for building robust, production applications where queries are known at compile time. Gizmo, on the other hand, shines when you need to interactively explore your data, prototype queries, or build applications that require dynamic query generation.

## Using the Gizmo REPL

The easiest way to get started with Gizmo is through its interactive **REPL** (Read-Eval-Print Loop). You can launch the REPL from the command line by running the `cayley` executable with the `repl` subcommand.

```bash
cayley repl --dbpath=/path/to/mygraph.db
```

This will open a Gizmo prompt where you can type queries and see the results immediately. The Gizmo query syntax closely mirrors the Path API.

```javascript
// Find all nodes in the graph
g.V().All()

// Find everyone whom Alice knows
g.V("Alice").Out("knows").All()

// Find everyone who is 25 years old
g.V().Has("age", 25).All()
```

Notice the similarities to the Path API: `g.V()` is equivalent to `cayley.StartPath()`, and the `Out()`, `Has()`, and `All()` methods work just as you would expect. The REPL is an excellent tool for learning Gizmo and for quickly testing out query ideas.

## Set Operations: Intersect, Union, and Except

Gizmo provides a rich set of methods for performing set operations on your query results. These methods allow you to combine the results of multiple paths in powerful ways.

*   `Intersect(path)`: Returns the nodes that are present in both the current path and the given path.
*   `Union(path)`: Returns all nodes from both the current path and the given path.
*   `Except(path)`: Returns the nodes from the current path that are not present in the given path.

For example, to find all people who are friends with both Alice and Bob:

```javascript
var aliceFriends = g.V("Alice").Out("friendsWith");
var bobFriends = g.V("Bob").Out("friendsWith");

aliceFriends.Intersect(bobFriends).All();
```

To find all people who are friends with either Alice or Bob:

```javascript
aliceFriends.Union(bobFriends).All();
```

And to find all of Alice's friends who are not also friends with Bob:

```javascript
aliceFriends.Except(bobFriends).All();
```

These set operations are fundamental to building complex queries that combine information from different parts of your graph.

## Executing Gizmo from Go

While the REPL is great for interactive use, you can also execute Gizmo queries from within your Go applications. The `gizmo.Execute` function takes a Gizmo query as a string and returns the results.

```go
import "github.com/cayleygraph/cayley/gizmo"

query := `g.V("Alice").Out("knows").All()`

results, err := gizmo.Execute(ctx, store, query, gizmo.Options{})
if err != nil {
    log.Fatalf("Gizmo query failed: %v", err)
}

for _, result := range results {
    fmt.Println(quad.NativeOf(result.Value))
}
```

This allows you to store your queries in configuration files, build them dynamically based on user input, or even expose a Gizmo endpoint in your own application's API. It provides a powerful bridge between the dynamic world of JavaScript and the robust, compiled world of Go.

In the exercises for this chapter, you will practice using the Gizmo REPL to explore a dataset and then write a Go program that executes a dynamic Gizmo query.

---

### References

[1] Cayley Documentation. "Gizmo." https://cayley.gitbook.io/cayley/query-languages/gizmo
