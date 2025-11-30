# Chapter 2: Your First Cayley Program

In the previous chapter, we introduced the core concepts of graph databases and got our hands dirty with the Cayley command-line interface. Now, it's time to take the next step and interact with Cayley programmatically using Go. This chapter will guide you through setting up a Go project, creating your first in-memory graph, adding data, and running basic queries using Cayley's Path API.

## Setting Up a Go Project with Cayley

To use Cayley as a library in your Go project, you first need to set up a Go module and add Cayley as a dependency. A Go module is a collection of Go packages that are released together. It defines the dependencies for your project, making builds reproducible.

First, create a new directory for your project and initialize a Go module. Let's call our project `cayley-learning`.

```bash
mkdir cayley-learning
cd cayley-learning
go mod init github.com/your-username/cayley-learning
```

Next, you need to add the Cayley library to your project's dependencies. You can do this by creating a `main.go` file and importing the necessary Cayley packages. When you build or run your project, Go will automatically download the required dependencies.

Here is a minimal `main.go` file to start with:

```go
package main

import (
	"fmt"
	"log"

	"github.com/cayleygraph/cayley"
	"github.com/cayleygraph/quad"
)

func main() {
	fmt.Println("Cayley project setup complete!")
}
```

Run `go mod tidy` to download and verify the dependencies. This command ensures that your `go.mod` file matches the source code in your module.

```bash
go mod tidy
```

## Creating an In-Memory Graph

For rapid prototyping and testing, Cayley provides an in-memory graph store. This store is ephemeral, meaning all data is lost when the program terminates, but it's incredibly fast and requires no external setup. To create a new in-memory graph, you use the `cayley.NewMemoryGraph()` function.

```go
// Create a brand new in-memory graph store.
store, err := cayley.NewMemoryGraph()
if err != nil {
	log.Fatalf("Failed to create graph: %v", err)
}
```

The `store` variable is a handle to your graph, which you will use for all subsequent operations like adding and querying data.

## Adding Quads to the Graph

With your graph store created, you can now add data to it. As we learned in Chapter 1, the basic unit of data in Cayley is the quad. The `quad` package provides the tools to create quads. The `quad.Make()` function is the most common way to construct a quad.

Let's add a simple quad representing the statement "The sky is blue."

```go
// Add a quad to the store.
store.AddQuad(quad.Make("sky", "is", "blue", nil))
```

The `quad.Make` function takes four arguments: subject, predicate, object, and label. In this case, we are using simple string literals for the subject, predicate, and object. We pass `nil` for the label, indicating that this quad does not belong to a specific named graph.

## Basic Queries with the Path API

Once you have data in your graph, you'll want to retrieve it. Cayley's **Path API** provides a fluent, chainable interface for building and executing queries. A query starts from a specific node or a set of nodes and traverses the graph by following edges.

To start a query, you use `cayley.StartPath()`, providing the graph store and the starting node(s).

Let's build a query to find out what color the sky is:

```go
// Start a path from the "sky" node.
p := cayley.StartPath(store, quad.String("sky"))
```

Next, we traverse the graph by following the `is` predicate. The `Out()` method follows predicates in the forward direction (from subject to object).

```go
// Follow the "is" predicate.
p = p.Out(quad.String("is"))
```

Finally, to get the results of our query, we can use an iterator. The `Iterate()` method returns an iterator that you can loop over. The `EachValue()` function provides a convenient way to process each result.

```go
// Iterate over the results and print them.
err = p.Iterate(nil).EachValue(nil, func(value quad.Value) error {
	nativeValue := quad.NativeOf(value) // Convert the RDF value to a Go type.
	fmt.Println(nativeValue)
	return nil
})
if err != nil {
	log.Fatalf("Failed to iterate results: %v", err)
}
```

When you run this code, it will print `blue` to the console. The `quad.NativeOf()` function is a handy utility that converts Cayley's internal `quad.Value` representation into a standard Go type (like `string`, `int`, etc.).

This chapter has given you the essential tools to start building applications with Cayley. In the following exercises, you will solidify this knowledge by building a simple social network and querying it.

---

### References

[1] The Go Programming Language. "Go Modules Reference." https://go.dev/ref/mod

[2] Cayley. "Quickstart as Library." https://cayley.gitbook.io/cayley/usage/quickstart-as-lib
