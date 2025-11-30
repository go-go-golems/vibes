# Chapter 8: Configuration and Customization

Effective configuration is crucial for any production-ready application, and Cayley is no exception. When embedding Cayley as a library, you gain fine-grained control over its behavior. This chapter will delve into the various ways to configure and customize your embedded Cayley instance, from backend options to query settings.

## Understanding Cayley's Configuration Model

Cayley's configuration is primarily driven by the `graph.Options` map, which is passed during the `cayley.NewGraph` call. This map allows you to specify parameters that are specific to the chosen backend or to the overall graph behavior.

```go
type Options map[string]interface{}
```

This generic map allows for flexible key-value pairs, where the interpretation of the values depends on the backend or component being configured.

## Backend-Specific Options

Each `QuadStore` backend can define and consume its own set of options. For example:

*   **BoltDB**: Might have options for `read_only`, `no_sync`, or `timeout`.
*   **PostgreSQL**: Could include `host`, `port`, `user`, `password`, `database`, or `sslmode`.
*   **Custom Backends**: Your own custom backends can define any options relevant to their internal workings.

It's important to consult the documentation or source code of the specific backend you are using to understand its available options.

## Programmatic Configuration

When embedding Cayley, you typically configure it programmatically, rather than relying on external configuration files. This allows for dynamic configuration based on your application's environment or runtime conditions.

```go
import (
	"github.com/cayleygraph/cayley/graph"
	"github.com/cayleygraph/cayley"
)

func setupCayley(dbPath string, readOnly bool) (*cayley.Graph, error) {
	opts := make(graph.Options)
	opts["read_only"] = readOnly
	opts["timeout"] = "30s" // Example for BoltDB

	h, err := cayley.NewGraph("bolt", dbPath, opts)
	if err != nil {
		return nil, fmt.Errorf("failed to create graph: %w", err)
	}
	return h, nil
}
```

This approach ensures that your Cayley instance is configured precisely as your application requires.

## Customizing Query Behavior

Beyond backend configuration, you can also influence Cayley's query behavior. While the core query optimization is handled internally, you can sometimes provide hints or adjust parameters.

### Custom Namespaces and Vocabularies

Cayley supports RDF-style namespaces, which can be configured to simplify your quad data. You can define custom prefixes for IRIs, making your data more readable and manageable.

```go
// Example of defining a custom namespace
quad.AddNamespace("ex", "http://example.org/schema/")

// Now you can use it like:
quad.Make("ex:person1", "ex:hasName", "Alice", nil)
```

This is typically done at application startup, before any quads are added or queried.

## Environment-Based Configuration

For deployment flexibility, it's common to configure applications using environment variables. You can easily integrate environment variables into your programmatic Cayley configuration.

```go
import (
	"os"
	"strconv"
)

func getCayleyOptions() graph.Options {
	opts := make(graph.Options)

	if val := os.Getenv("CAYLEY_READ_ONLY"); val != "" {
		if b, err := strconv.ParseBool(val); err == nil {
			opts["read_only"] = b
		}
	}

	if val := os.Getenv("CAYLEY_BOLT_TIMEOUT"); val != "" {
		opts["timeout"] = val
	}
	return opts
}

// Usage:
// handle, err := cayley.NewGraph("bolt", dbPath, getCayleyOptions())
```

This pattern allows you to easily adjust Cayley's behavior across different deployment environments (development, staging, production) without recompiling your application.

## Overriding Default Behaviors

In some advanced scenarios, you might want to override Cayley's default behaviors. This could involve providing a custom `Namer` implementation or even replacing parts of the query optimizer. These are advanced topics that require a deep understanding of Cayley's internals, but the modular design makes them possible.

By mastering configuration and customization, you can tailor your embedded Cayley instance to perfectly fit the needs of your application, ensuring optimal performance and maintainability. The exercises in this chapter will guide you through creating a flexible configuration system for your embedded Cayley application, allowing you to experiment with different backend options and query settings.

---

### References

[1] Cayley GitHub Repository. `graph/graph.go`. https://github.com/cayleygraph/cayley/blob/master/graph/graph.go

[2] Cayley GitHub Repository. `quad/quad.go`. https://github.com/cayleygraph/cayley/blob/master/quad/quad.go
