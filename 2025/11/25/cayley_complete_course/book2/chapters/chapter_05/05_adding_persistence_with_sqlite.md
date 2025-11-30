
# Chapter 5: Adding Persistence with SQLite

Our simple in-memory backend is a great learning tool, but for most real-world applications, you need your data to persist. In this chapter, you will take the concepts from the previous chapter and apply them to build a persistent `QuadStore` backend using SQLite.

SQLite is a lightweight, file-based SQL database that is an excellent choice for a simple, single-node persistent backend. It will allow us to focus on the logic of the `QuadStore` implementation without the complexity of managing a separate database server.

## Schema Design

Our first task is to design a SQL schema to store our quads and values. A good schema is essential for performance. We will need two main tables:

1.  **`nodes`**: This table will store the mapping from our internal integer IDs to the actual `quad.Value` data.

    ```sql
    CREATE TABLE nodes (
        id INTEGER PRIMARY KEY,
        hash BLOB UNIQUE,
        value BLOB,
        type INTEGER
    );
    ```

    We will use a hash of the value to quickly check for existence.

2.  **`quads`**: This table will store the quads themselves, using the integer IDs from the `nodes` table.

    ```sql
    CREATE TABLE quads (
        subject_id INTEGER,
        predicate_id INTEGER,
        object_id INTEGER,
        label_id INTEGER,
        PRIMARY KEY (subject_id, predicate_id, object_id, label_id)
    );
    ```

To ensure fast queries in all four directions, we will also need to create indexes on each of the `_id` columns in the `quads` table.

## `Init` vs. `New`

With a persistent store, we need to distinguish between creating a new database and opening an existing one. We will implement two functions for this:

*   `InitFunc`: This function will be called when a new database is being created. It will be responsible for creating the database file and running the `CREATE TABLE` statements.

*   `NewFunc`: This function will be called to open an existing database. It will connect to the database file and prepare the necessary SQL statements.

We will register both of these functions with Cayley's registry.

## Implementing the `QuadStore` Methods with SQL

Now, we will re-implement the `QuadStore` methods using SQL queries against our SQLite database.

*   **`ValueOf`**: This will first query the `nodes` table by hash to see if a value already exists. If it does, it will return the existing ID. If not, it will insert a new row into the `nodes` table and return the new ID.

*   **`QuadIterator`**: This will execute a `SELECT` query against the `quads` table, using a `WHERE` clause on the appropriate `_id` column. For example, to find all quads with a given subject, we would run `SELECT * FROM quads WHERE subject_id = ?`.

*   **`ApplyDeltas`**: This method will be implemented using SQL transactions to ensure atomicity. For each `Add` delta, it will insert a row into the `quads` table. For each `Delete` delta, it will delete a row.

## Performance and Optimization

Using a SQL database gives us access to a powerful query optimizer. We can leverage this by:

*   **Creating the right indexes**: As mentioned above, creating indexes on all four `_id` columns in the `quads` table is crucial for performance.

*   **Using prepared statements**: To avoid the overhead of parsing the same SQL queries over and over, we will use prepared statements for all of our common queries.

*   **Batching writes**: When adding or deleting large numbers of quads, we will batch them together in a single transaction to reduce the overhead of transaction management.

By the end of this chapter, you will have a fully functional, persistent Cayley backend. You will have learned how to map the `QuadStore` interface to a relational database schema and how to optimize your implementation for performance. The exercises will guide you through the process of designing the schema, implementing the `QuadStore` methods with SQL, and benchmarking the performance of your new backend.

---

### References

[1] SQLite Documentation. https://www.sqlite.org/docs.html

[2] Go `database/sql` Package Documentation. https://golang.org/pkg/database/sql/
