
# Chapter 12: Knowledge Base Operations

Building a knowledge base is not just about modeling and querying; it is also about managing the lifecycle of your data. This chapter will cover the essential operations for maintaining a healthy and efficient knowledge base, from performing CRUD operations at scale to managing bulk data imports and ensuring data quality.

## CRUD Operations at Scale

**CRUD** (Create, Read, Update, Delete) are the four fundamental operations of data management. In Cayley, these operations are performed on quads.

*   **Create**: Adding new quads to the graph. This is done using `store.AddQuad()` or, for atomic operations, `store.ApplyTransaction()` with `t.AddQuad()`.

*   **Read**: Querying for quads. This is the focus of the Path API and Gizmo, as we have covered in previous chapters.

*   **Update**: Updating existing information. In an RDF graph, updates are typically performed as a combination of a delete and a create. To change the value of a property, you first remove the old quad and then add the new one. This should almost always be done within a transaction to ensure atomicity.

    ```go
    t := cayley.NewTransaction()
    t.RemoveQuad(quad.Make("Alice", "age", 29, nil))
    t.AddQuad(quad.Make("Alice", "age", 30, nil))
    store.ApplyTransaction(t)
    ```

*   **Delete**: Removing quads from the graph. This is done using `store.RemoveQuad()` or `t.RemoveQuad()` within a transaction.

When performing these operations at scale, it is important to batch them together in transactions to reduce overhead and improve performance.

## Bulk Imports

Populating a knowledge base often involves importing large amounts of data from external sources. Cayley provides several mechanisms for efficient bulk loading.

*   **Quad Files**: You can load data from quad files in formats like N-Quads or Turtle using the `cayley load` command.

    ```bash
    cayley load --dbpath=/path/to/mygraph.db --quads=mydata.nq
    ```

*   **`quad.Writer`**: For programmatic bulk loading, the `quad.Writer` interface provides a high-performance way to stream quads into the database. You can get a `quad.Writer` from `store.NewQuadWriter()`.

    ```go
    writer, err := store.NewQuadWriter()
    if err != nil {
        log.Fatal(err)
    }
    defer writer.Close()

    for _, q := range myQuads {
        _, err := writer.WriteQuad(q)
        if err != nil {
            log.Printf("Failed to write quad: %v", err)
        }
    }
    ```

Using `quad.Writer` is significantly more efficient for large imports than adding quads one by one.

## Graph Statistics

Understanding the size and shape of your graph is essential for performance tuning and capacity planning. The `store.Stats()` method provides basic statistics about your graph, such as the number of nodes and quads.

```go
stats, err := store.Stats(ctx, true) // true for exact stats
if err != nil {
    log.Fatal(err)
}

fmt.Printf("Nodes: %d, Quads: %d\n", stats.Nodes.Value, stats.Quads.Value)
```

For more detailed analysis, you can write queries to count the number of instances of a certain type, find the average number of properties per node, or identify the most highly connected nodes in your graph.

## Validation and Constraints

Ensuring the quality and consistency of your data is a critical aspect of knowledge base management. While RDF itself is very flexible, you often want to enforce certain rules or constraints on your data.

*   **Schema Validation**: You can use a schema language like SHACL (Shapes Constraint Language) to define a set of rules that your data must conform to. You can then validate your data against this schema before importing it into Cayley.

*   **Application-Level Validation**: You can also implement validation logic in your application code. Before adding a new quad, you can check if it violates any of your business rules, such as "a person must have exactly one age" or "a product must have a price."

*   **Data Cleaning**: It is often necessary to clean and normalize your data before importing it. This can involve tasks like resolving duplicate entities, standardizing date formats, or correcting spelling errors.

By implementing a robust data validation and cleaning pipeline, you can ensure that your knowledge base remains a reliable and trustworthy source of information.

Managing a knowledge base is an ongoing process that requires a combination of efficient data loading, careful data quality management, and a solid understanding of your data's lifecycle. The exercises for this chapter will give you hands-on experience with bulk loading data into Cayley and implementing a simple data validation pipeline.

---

### References

[1] W3C. "Shapes Constraint Language (SHACL)." https://www.w3.org/TR/shacl/

[2] Cayley Documentation. "Loading Data." https://cayley.gitbook.io/cayley/getting-started/loading-data
