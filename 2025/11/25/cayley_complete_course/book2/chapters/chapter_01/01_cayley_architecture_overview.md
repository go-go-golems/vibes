
# Chapter 1: Cayley Architecture Overview

Welcome to the advanced course on Cayley! In Book 1, you mastered the art of using Cayley to build powerful knowledge bases and AI agent systems. Now, we will go deeper. This book is for the developer who wants to understand not just how to use Cayley, but how it works, how to extend it, and how to embed it into production applications.

This chapter will provide a high-level overview of Cayley's internal architecture. We will dissect the system into its core components, understand how they interact, and set the stage for the deep dives in the chapters to come.

## The Layered Architecture

Cayley is designed as a modular, layered system. This architecture makes it highly flexible and extensible. At a high level, the layers are:

1.  **Query Language Layer**: This is the user-facing layer, where you write queries in languages like Gizmo or use the Path API.
2.  **Query Planner/Optimizer**: This layer takes a query and translates it into an efficient execution plan. The plan is represented as a tree of iterators.
3.  **Iterator System**: This is the heart of the query engine. Each iterator is a small, specialized component that performs a single operation (e.g., scanning, filtering, joining).
4.  **QuadStore Interface**: This is the core abstraction that separates the query engine from the storage backend. All interactions with the underlying database go through this interface.
5.  **Storage Backend**: This is the actual database that stores the quads, such as BoltDB, PostgreSQL, or a custom backend that you create.

This layered design means you can swap out components at any layer without affecting the others. For example, you can write a new storage backend, and the entire query engine will work on top of it without modification.

## The QuadStore Interface: The Great Abstraction

The most important concept in Cayley's architecture is the `QuadStore` interface. This Go interface defines a contract that any storage backend must fulfill. As long as a database can implement this interface, it can be used as a backend for Cayley.

We will explore this interface in detail in the next chapter, but at a high level, it provides methods for:

*   Adding and deleting quads.
*   Querying for quads based on their subject, predicate, object, or label.
*   Retrieving statistics about the graph.
*   Iterating over all nodes and quads.

This abstraction is what makes Cayley so powerful. It allows you to choose the storage technology that best fits your needs, from a simple in-memory store to a distributed, fault-tolerant database.

## The Iterator System: A Pipeline of Operations

Every query in Cayley is executed as a pipeline of **iterators**. An iterator is a small, focused object that performs a single, well-defined task. For example:

*   `AllIterator`: Scans through all nodes or quads in the graph.
*   `FixedIterator`: Represents a fixed set of nodes.
*   `AndIterator`: Takes two iterators and returns their intersection.
*   `HasIterator`: Filters an iterator based on a property.

When you write a query, Cayley's query planner translates it into a tree of these iterators. For example, the query `g.V("alice").Out("knows")` might be translated into a tree like this:

```
    OutIterator("knows")
        |
    FixedIterator("alice")
```

The query engine then executes this tree, with the results of one iterator flowing into the next. This pipeline model is highly efficient and allows for a great deal of optimization.

## The Registry and Plugin System

Cayley is designed to be extensible from the ground up. It uses a **registry** pattern to discover and load components at runtime. When Cayley starts up, it scans for registered storage backends, query languages, and other plugins.

This is accomplished through Go's `init()` function mechanism. Any package that wants to register a component simply adds an `init()` function that calls the appropriate registration function, such as `graph.RegisterQuadStore`.

This plugin system is what allows you to add your own custom backends, iterators, and even query languages to Cayley without having to modify the core codebase.

## Refs vs. Values: The Abstraction Boundary

One final concept that is crucial to understanding Cayley's internals is the distinction between **Values** and **Refs**.

*   **`quad.Value`**: This is the high-level representation of a node or literal, such as `quad.String("alice")` or `quad.Int(30)`. This is what you work with in your application code.

*   **`graph.Ref`**: This is an opaque, internal reference that the `QuadStore` uses to identify a value. It could be an integer ID, a pointer, or any other internal representation.

The `QuadStore` interface includes a `Namer` component that is responsible for translating between these two representations. This abstraction allows the storage backend to use whatever internal representation is most efficient, while still presenting a clean, consistent API to the rest of the system.

In the chapters to come, we will tear down these components one by one, explore their source code, and learn how to build our own. By the end of this book, you will not just be a user of Cayley; you will be a contributor and an expert.

---

### References

[1] Cayley GitHub Repository. `graph/quadstore.go`. https://github.com/cayleygraph/cayley/blob/master/graph/quadstore.go

[2] Cayley GitHub Repository. `graph/iterator/iterator.go`. https://github.com/cayleygraph/cayley/blob/master/graph/iterator/iterator.go
