
# Chapter 4: Building a Simple In-Memory Backend

Now that you have a solid theoretical understanding of the `QuadStore` and iterator interfaces, it is time to put that knowledge into practice. In this chapter, you will build your first custom Cayley backend: a simple, non-persistent, in-memory quad store.

This exercise will teach you the fundamentals of implementing the `QuadStore` interface and will serve as the foundation for the more advanced backends we will build in later chapters.

## Design and Data Structures

Before we start writing code, let's think about the data structures we will need. Our in-memory store needs to be able to:

1.  Store quads.
2.  Assign unique IDs to values and quads.
3.  Look up quads efficiently in all four directions (Subject, Predicate, Object, Label).

A simple and effective way to achieve this is with a combination of maps and slices:

*   `quads []quad.Quad`: A slice to store all of our quads. The index in this slice can serve as the quad's ID.
*   `values map[quad.Value]int64`: A map to store our unique values and their corresponding IDs.
*   `nextValueID int64`: A counter to generate new value IDs.
*   `indexes [4]map[int64][]int64`: An array of four maps, one for each direction. Each map will store a mapping from a value ID to a slice of quad IDs.

This design is straightforward and will allow us to implement the `QuadStore` interface with relative ease.

## Implementing the `QuadStore` Interface

Let's walk through the implementation of the key `QuadStore` methods.

### `ValueOf` and `NameOf`

These methods will manage the mapping between `quad.Value` and our internal `int64` IDs. `ValueOf` will check if a value already exists in our `values` map. If it does, it will return the existing ID. If not, it will generate a new ID, store the mapping, and return the new ID.

`NameOf` will do the reverse, looking up an ID in a reverse mapping to find the corresponding `quad.Value`.

### `QuadIterator`

This is our primary query method. It will take a direction and a value ID, look up the corresponding list of quad IDs in our `indexes`, and return an iterator that can scan over those IDs.

For this simple implementation, we can create a basic iterator that simply holds a slice of quad IDs and a current position.

### `ApplyDeltas`

This method will handle adding and deleting quads. For an `Add` delta, it will:

1.  Get or create IDs for the subject, predicate, object, and label of the quad.
2.  Add the quad to our `quads` slice.
3.  Update the four indexes with the new quad's ID.

For a `Delete` delta, it will find the quad to be deleted and remove it from the `quads` slice and the indexes. (For simplicity, we might choose to mark quads as deleted rather than actually removing them from the slice.)

## Registration

Finally, we need to register our new backend with Cayley so that it can be discovered and used. We will do this by creating an `init()` function in our package that calls `graph.RegisterQuadStore`.

```go
func init() {
    graph.RegisterQuadStore("simplemem", graph.QuadStoreRegistration{
        NewFunc: func(path string, opts graph.Options) (graph.QuadStore, error) {
            return NewSimpleMemStore(), nil
        },
        IsPersistent: false,
    })
}
```

## Testing with `graphtest`

Cayley provides a comprehensive test suite in the `graphtest` package that can be used to verify the correctness of any `QuadStore` implementation. Once we have our simple in-memory store, we will write a test that runs this suite against it.

```go
import "github.com/cayleygraph/cayley/graph/graphtest"

func TestSimpleMemStore(t *testing.T) {
    graphtest.TestAll(t, func(t testing.TB) (graph.QuadStore, func()) {
        qs := NewSimpleMemStore()
        return qs, func() { qs.Close() }
    }, &graphtest.Config{})
}
```

Passing this test suite will give us confidence that our backend is a correct and compliant implementation of the `QuadStore` interface.

By the end of this chapter, you will have a fully functional, albeit simple, Cayley backend. You will have gained a deep, practical understanding of the `QuadStore` interface and will be ready to tackle the challenge of building a persistent backend in the next chapter. The exercises will guide you step-by-step through the process of building and testing your in-memory store.

---

### References

[1] Cayley GitHub Repository. `graph/graphtest/graphtest.go`. https://github.com/cayleygraph/cayley/blob/master/graph/graphtest/graphtest.go
