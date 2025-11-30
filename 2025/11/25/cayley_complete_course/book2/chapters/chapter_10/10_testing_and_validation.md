# Chapter 10: Testing and Validation

Building custom backends and extensions for Cayley is a powerful capability, but with great power comes great responsibility. Thorough testing and validation are essential to ensure that your custom components are correct, performant, and reliable. This chapter will cover the tools and techniques for testing your Cayley extensions, with a focus on the `graphtest` package.

## The `graphtest` Package

Cayley includes a powerful testing suite in the `graph/graphtest` package. This suite is designed to test the correctness and compliance of any `QuadStore` implementation. It includes a comprehensive set of tests that cover all aspects of the `QuadStore` interface, from basic CRUD operations to complex queries and transactional semantics.

### Using `graphtest.TestAll`

The main entry point to the test suite is the `graphtest.TestAll` function. This function takes a factory function that can create a new instance of your `QuadStore` and a configuration object.

```go
import (
	"testing"
	"github.com/cayleygraph/cayley/graph"
	"github.com/cayleygraph/cayley/graph/graphtest"
)

func TestMyCustomStore(t *testing.T) {
	// Factory function to create a new instance of your store
	factory := func(t testing.TB) (graph.QuadStore, func()) {
		qs, err := NewMyCustomStore("/path/to/test.db")
		if err != nil {
			t.Fatalf("Failed to create store: %v", err)
		}
		// Return the store and a cleanup function
		return qs, func() {
			qs.Close()
			os.Remove("/path/to/test.db")
		}
	}

	// Run the test suite
	graphtest.TestAll(t, factory, &graphtest.Config{})
}
```

By simply providing this factory function, you can run dozens of tests against your backend, ensuring that it behaves as expected.

### `graphtest` Configuration

The `graphtest.Config` object allows you to customize the behavior of the test suite. For example, you can:

*   Skip certain tests that are not applicable to your backend.
*   Provide a seed for the random number generator to make tests reproducible.
*   Enable or disable specific features, such as support for transactions.

## Writing Your Own Tests

While `graphtest` is excellent for ensuring compliance, you will also need to write your own tests for the specific features and optimizations of your backend. These tests should cover:

*   **Edge Cases**: Test how your backend handles empty values, large values, and unusual quad structures.
*   **Performance**: Write benchmarks to measure the performance of your backend for different workloads (read-heavy, write-heavy, etc.).
*   **Concurrency**: Write tests that access the backend from multiple goroutines to ensure that your locking and concurrency control mechanisms are correct.

### Benchmarking

Go's built-in `testing` package provides excellent support for benchmarking. You can write benchmark functions that will be run by the `go test` command with the `-bench` flag.

```go
func BenchmarkMyStore_AddQuads(b *testing.B) {
	qs, cleanup := createMyStore(b)
	defer cleanup()

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		// Add a batch of quads
		qs.AddQuads(...)
	}
}
```

## Property-Based Testing

For more advanced testing, you can use property-based testing libraries like `gopter` to automatically generate a wide range of inputs and test that your backend satisfies certain properties. For example, you could write a property-based test that verifies that any quad that is added to the store can be retrieved.

By combining the `graphtest` suite with your own unit tests, benchmarks, and property-based tests, you can build a high degree of confidence in the correctness and performance of your custom Cayley components. The exercises in this chapter will guide you through the process of creating a comprehensive test suite for the SQLite backend that we built in Chapter 5, including compliance tests, benchmarks, and a simple concurrency test.

---

### References

[1] Cayley GitHub Repository. `graph/graphtest/graphtest.go`. https://github.com/cayleygraph/cayley/blob/master/graph/graphtest/graphtest.go

[2] Go `testing` Package Documentation. https://golang.org/pkg/testing/

[3] Gopter GitHub Repository. https://github.com/leanovate/gopter
