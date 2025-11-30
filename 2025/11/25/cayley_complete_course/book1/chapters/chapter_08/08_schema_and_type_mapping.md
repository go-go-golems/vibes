
# Chapter 8: Schema Package and Type Mapping

So far, we have been working with quads and `quad.Value` primitives directly. While this provides a great deal of flexibility, it can also be cumbersome and error-prone. In many applications, it is more convenient to work with native Go structs and let the system handle the conversion to and from quads. This is where Cayley's **schema package** comes in. The schema package provides a powerful and convenient way to map your Go types to your graph schema, enabling you to work with your data in a more natural and type-safe way.

## From Go Structs to RDF

The core idea behind the schema package is to use Go structs to define the shape of your data. You can then use the `schema.NewEncoder` to convert instances of these structs into quads, and `schema.NewDecoder` to do the reverse.

Let's define a simple `Person` struct:

```go
type Person struct {
    ID   quad.IRI `json:"@id"`
    Name string   `json:"name"`
    Age  int      `json:"age"`
}
```

We can now create an instance of this struct and encode it into quads:

```go
alice := Person{
    ID:   quad.IRI("person:alice"),
    Name: "Alice Smith",
    Age:  30,
}

quads, err := schema.NewEncoder(nil).Encode(alice)
// quads now contains:
// <person:alice> <name> "Alice Smith" .
// <person:alice> <age> 30 .
```

This automatic conversion from structs to quads simplifies your code and reduces the amount of boilerplate you need to write.

## Type Annotations and Field Mappings

The schema package uses struct tags to control the mapping between Go fields and RDF properties. The `json` tag is used to specify the predicate IRI for each field.

*   `json:"@id"`: This special tag identifies the field that should be used as the subject of the quads.
*   `json:"name"`: This maps the `Name` field to the predicate `<name>`.
*   `json:"age,omitempty"`: The `omitempty` option tells the encoder to skip this field if it has a zero value.
*   `json:"-"`: This tells the encoder to ignore this field completely.

You can also use the `quad` tag for more advanced mapping options, such as specifying the direction of a relationship or marking a field as a reverse link.

```go
type Person struct {
    ID      quad.IRI `json:"@id"`
    Friends []Person `json:"friends_with" quad:"@list"`
}
```

The `quad:"@list"` tag tells the encoder to treat the `Friends` slice as an RDF list, creating a linked list structure in the graph.

## IRI Generation Strategies

When encoding new data, you often need to generate new IRIs for your entities. The schema package provides several strategies for doing this.

You can provide a custom `IRIGenerator` to the encoder. This allows you to implement your own logic for generating IRIs, such as using a UUID or a hash of the entity's properties.

```go
enc := schema.NewEncoder(nil)
enc.IRIGenerator = func(v interface{}) (quad.IRI, error) {
    p := v.(Person)
    return quad.IRI(fmt.Sprintf("person:%s", strings.ToLower(p.Name))),
        nil
}
```

This gives you complete control over how your entities are identified in the graph.

## Loading Data into Structs

The schema package is not just for writing data; it is also a powerful tool for reading data from the graph and loading it into your Go structs. The `schema.NewDecoder` takes a graph store and can load an entity by its IRI.

```go
dec := schema.NewDecoder(store)

var alice Person
err := dec.Load(&alice, quad.IRI("person:alice"))
if err != nil {
    log.Fatalf("Failed to load person: %v", err)
}

fmt.Println(alice.Name) // Prints "Alice Smith"
```

The decoder will automatically traverse the graph, find the relevant quads, and populate the fields of your struct. It can even handle complex nested structures and relationships.

By using the schema package, you can bridge the gap between the graph-based world of RDF and the type-safe, object-oriented world of Go. This allows you to write cleaner, more maintainable code while still leveraging the full power of the Cayley graph database. The exercises for this chapter will guide you through the process of defining a schema for a real-world dataset and using the encoder and decoder to manage your data.

---

### References

[1] Cayley Documentation. "Schema Package." https://cayley.gitbook.io/cayley/advanced-topics/schema-package
