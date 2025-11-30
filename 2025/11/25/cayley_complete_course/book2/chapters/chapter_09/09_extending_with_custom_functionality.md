# Chapter 9: Extending with Custom Functionality

Cayley's modular architecture not only allows for custom backends but also for extending its functionality in various other ways. This chapter explores how you can add custom features to your embedded Cayley instance, from new API endpoints to domain-specific query functions.

## Adding Custom HTTP Endpoints

When running Cayley as a standalone server, it exposes a set of HTTP APIs for querying and managing the graph. When you embed Cayley, you can extend this API with your own custom endpoints. This is particularly useful for creating domain-specific APIs that are tailored to your application's needs.

```go
import (
	"net/http"
	"github.com/cayleygraph/cayley/graph"
	"github.com/julienschmidt/httprouter"
)

func createCustomAPI(handle *graph.Handle) http.Handler {
	router := httprouter.New()

	// Add a custom endpoint
	router.GET("/api/v1/my-custom-query", func(w http.ResponseWriter, r *http.Request, _ httprouter.Params) {
		// Your custom query logic here
		// You can use the 'handle' to query the graph
		w.Write([]byte("Hello from custom API!"))
	})

	return router
}
```

You can then run this router as part of your application's web server, providing a unified API for both standard Cayley operations and your custom extensions.

## Implementing Custom Gizmo Functions

Gizmo, Cayley's JavaScript-based query language, is also extensible. You can register your own custom functions that can be called from within Gizmo queries. This allows you to encapsulate complex, domain-specific logic into reusable functions.

To create a custom Gizmo function, you need to implement the `gizmo.Function` interface and register it with the Gizmo environment.

```go
import (
	"github.com/cayleygraph/cayley/query/gizmo"
)

// A custom function that, for example, calculates the 'popularity' of a node
func popularityFunc(ctx *gizmo.Context, args []interface{}) (interface{}, error) {
	// Your logic to calculate popularity
	return 100.0, nil // Return a popularity score
}

// Register the function
gizmo.RegisterFunction("popularity", popularityFunc)
```

Once registered, you can use this function in your Gizmo queries:

```javascript
g.V().Has("type", "post").Filter(function(d) { return popularity(d) > 50 })
```

## Value Type Extensions

Cayley's `quad.Value` system can be extended to support custom data types. This is useful when you need to store domain-specific data that doesn't fit into the standard types (string, int, bool, etc.).

To create a custom value type, you need to:

1.  Define a Go type for your custom data.
2.  Implement the `quad.Value` interface for your type.
3.  Register your type with the `quad` package so that it can be serialized and deserialized.

This is an advanced feature that requires careful implementation, but it can be very powerful for applications that need to store rich, structured data in the graph.

## Middleware Patterns

Another way to extend Cayley's functionality is by using middleware. You can wrap the `QuadStore` interface with your own implementation that adds extra functionality before or after calls to the underlying store. This is a great way to implement features like:

*   **Logging**: Log all queries and updates to the graph.
*   **Auditing**: Keep a detailed audit trail of all changes to the data.
*   **Metrics**: Collect detailed metrics on query performance and data access patterns.

```go
type LoggingQuadStore struct {
	graph.QuadStore
	logger *log.Logger
}

func (qs *LoggingQuadStore) ApplyDeltas(deltas []graph.Delta, opts graph.IgnoreOpts) error {
	qs.logger.Printf("Applying %d deltas...", len(deltas))
	return qs.QuadStore.ApplyDeltas(deltas, opts)
}
```

By extending Cayley with custom functionality, you can create a graph database that is perfectly tailored to your application's needs. The exercises in this chapter will guide you through the process of adding a custom HTTP endpoint to your embedded Cayley application and creating a simple custom Gizmo function.


mo function.


---

### References

[1] Cayley GitHub Repository. `query/gizmo/gizmo.go`. https://github.com/cayleygraph/cayley/blob/master/query/gizmo/gizmo.go

[2] Cayley GitHub Repository. `quad/quad.go`. https://github.com/cayleygraph/cayley/blob/master/quad/quad.go
