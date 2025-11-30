# Chapter 4: Storage Backends

Up to this point, we have been working exclusively with Cayley's in-memory graph store. While excellent for prototyping, testing, and small-scale applications, real-world systems require **persistence**—the ability to store data on disk so that it survives program termination and can be accessed later. This chapter explores Cayley's flexible storage architecture, focusing on its support for multiple backends and diving deep into the use of BoltDB for single-node, persistent graph storage.

## Memory Store vs. Persistent Stores

Cayley's default storage backend is the **memory store**. As its name implies, all quads are stored in the computer's main memory (RAM). This provides exceptional speed for both read and write operations, as there is no need to access slower disk-based storage. However, this speed comes at a significant cost: the data is ephemeral. When your program exits, the entire graph is lost.

**Persistent stores**, on the other hand, write data to disk, ensuring its durability. This is essential for any production application that needs to maintain state over time. Cayley supports a wide variety of persistent backends, each with its own trade-offs in terms of performance, scalability, and operational complexity.

| Backend | Type | Description |
| :--- | :--- | :--- |
| **Memory** | In-Memory | Extremely fast, but not persistent. Ideal for testing. |
| **BoltDB** | Embedded KV | A fast, single-file key-value store written in Go. Great for embedded applications. |
| **PostgreSQL** | Relational DB | A powerful, open-source relational database. Good for integrating with existing SQL infrastructure. |
| **MongoDB** | Document DB | A popular NoSQL document store. Suitable for large, horizontally-scalable deployments. |
| **CockroachDB**| Distributed SQL| A distributed SQL database that provides scalability and resilience. |

## BoltDB for Single-Node Persistence

For many applications, a simple, embedded, single-file database is all that is needed. **BoltDB** is a pure Go key-value store that is designed for exactly this purpose. It is fast, reliable, and because it is embedded within your application, it requires no separate server process or complex setup.

To use BoltDB with Cayley, you first need to import the BoltDB driver. This is typically done with a blank import, which registers the driver with Cayley's storage registry.

```go
import _ "github.com/cayleygraph/cayley/graph/kv/bolt"
```

### Initializing and Opening a BoltDB Store

Before you can use a BoltDB store, you must initialize it. This is a one-time operation that creates the database file on disk. The `graph.InitQuadStore` function is used for this purpose.

```go
dbPath := "/path/to/mygraph.db"
err := graph.InitQuadStore("bolt", dbPath, nil)
if err != nil {
    log.Fatalf("Failed to initialize database: %v", err)
}
```

Once the database is initialized, you can open it using `cayley.NewGraph`. This function returns a handle to the graph store, which you can then use to add and query quads, just as you did with the memory store.

```go
store, err := cayley.NewGraph("bolt", dbPath, nil)
if err != nil {
    log.Fatalf("Failed to open database: %v", err)
}
defer store.Close()
```

It is crucial to call `store.Close()` when you are finished with the database. This ensures that all data is flushed to disk and the database file is closed cleanly.

## Transactions for Atomic Operations

When working with a persistent database, ensuring data integrity is paramount. It is often necessary to perform multiple write operations as a single, atomic unit. This is where **transactions** come in. A transaction is a sequence of operations that are guaranteed to either all succeed or all fail. This prevents the database from being left in an inconsistent state.

Cayley provides a simple transaction API. You create a new transaction, add or remove quads from it, and then apply the transaction to the store.

```go
// Create a new transaction
t := cayley.NewTransaction()

// Add quads to the transaction
t.AddQuad(quad.Make("user:alice", "status", "active", nil))
t.AddQuad(quad.Make("user:alice", "last_login", time.Now(), nil))

// Apply the transaction to the store
err := store.ApplyTransaction(t)
if err != nil {
    log.Fatalf("Transaction failed: %v", err)
}
```

In this example, both the status and last login time for Alice are updated together. If an error were to occur during the `ApplyTransaction` call, neither quad would be written to the database, ensuring that the user's profile remains consistent.

Transactions are not just for adding data. You can also use them to remove quads atomically:

```go
t.RemoveQuad(quad.Make("user:alice", "status", "inactive", nil))
```

By combining a persistent backend like BoltDB with transactions, you can build robust, reliable applications that safely manage your graph data. In the exercises, you will practice converting an in-memory graph to a persistent BoltDB store and using transactions to perform atomic updates.

---

### References

[1] BoltDB. "An embedded key/value database for Go." https://github.com/boltdb/bolt

[2] Cayley Documentation. "KV Backends." https://cayley.gitbook.io/cayley/database-backends/kv-backends
