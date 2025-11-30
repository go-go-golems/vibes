# Chapter 11: Performance Optimization

Performance is a critical aspect of any database system. While Cayley is designed to be performant out of the box, there are many ways to tune and optimize it for your specific workload. This chapter will explore various techniques for profiling, optimizing, and scaling your Cayley applications.

## Profiling with `pprof`

Before you can optimize, you must first identify the bottlenecks. Go has excellent built-in support for profiling via the `net/http/pprof` package. By importing this package, you can expose a set of HTTP endpoints that provide detailed profiling data about your application.

```go
import (
	_ "net/http/pprof"
	"net/http"
)

func main() {
	// ... your application logic ...

	// Expose the pprof endpoints
	go func() {
		log.Println(http.ListenAndServe("localhost:6060", nil))
	}()

	// ...
}
```

Once your application is running, you can use the `go tool pprof` command to analyze the profiling data and identify the parts of your code that are consuming the most CPU or memory.

## Iterator Optimization

As we discussed in Chapter 3, the iterator system is the heart of Cayley's query engine. Optimizing the iterator tree is one of the most effective ways to improve query performance.

*   **Size Estimation**: Ensure that your custom iterators provide accurate size estimates. The query optimizer relies on these estimates to build an efficient query plan.

*   **Join Order**: For `And` iterators, always try to place the most restrictive (smallest) iterator on the left-hand side and an `Index` on the right-hand side.

*   **Custom Iterators**: If you have a common query pattern that is not well-supported by the built-in iterators, consider writing a custom iterator that is specifically designed for that pattern.

## Indexing Strategy

For persistent backends, the indexing strategy is the single most important factor for query performance. As we saw in Chapter 5, you must create indexes for all four quad directions. For more advanced workloads, you may also want to create combined indexes for common query patterns.

When designing your indexing strategy, you need to consider the trade-off between read performance and write performance. Every index you add will improve the performance of certain read queries, but it will also add overhead to every write operation.

## Memory Optimization

For in-memory or partially in-memory backends, memory usage can be a major concern. Techniques for optimizing memory usage include:

*   **Using more compact data structures**: Choose data structures that have a low memory footprint.
*   **Reference counting**: To ensure that unused objects are garbage collected.
*   **Lazy loading**: To avoid loading the entire graph into memory at once.
*   **Caching**: Use a cache with a clear eviction policy to limit the amount of data that is kept in memory.

## Batch Operations

When adding or deleting large numbers of quads, it is much more efficient to batch them together in a single operation. The `AddQuads` and `ApplyDeltas` methods are designed for this purpose. Batching reduces the overhead of transaction management and allows the backend to perform bulk updates, which is often much faster than performing many small updates.

By applying these optimization techniques, you can build a Cayley application that is not only correct and reliable but also highly performant and scalable. The exercises for this chapter will guide you through the process of profiling a slow query, optimizing it by improving the iterator plan, and tuning the indexing strategy of your SQLite backend.

---

### References

[1] Go `net/http/pprof` Package Documentation. https://golang.org/pkg/net/http/pprof/

[2] The Go Blog. "Profiling Go Programs." https://go.dev/blog/pprof
