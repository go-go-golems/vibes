
# Chapter 6: Advanced Path Operations

Having mastered the fundamentals of the Path API, you are now ready to explore its more advanced features. These features allow you to build complex, reusable query patterns, perform deep traversals, and manage intricate query results. This chapter will introduce you to morphisms, recursive following, backtracking, and tagging, transforming you from a basic query writer to a sophisticated graph navigator.

## Morphisms for Reusable Patterns

As your queries become more complex, you will often find yourself repeating the same sequence of traversal and filtering steps. The Path API provides a powerful abstraction for encapsulating these reusable patterns: the **morphism**. A morphism is essentially a pre-defined path segment that you can apply to any path object.

You can create a morphism using `cayley.Morphism()`, and then chain the same Path API methods you are already familiar with.

```go
// Create a morphism to find the parents of a node
var isParentOf = cayley.Morphism().In(quad.IRI("parent_of"))

// Now, apply this morphism to a path
p := cayley.StartPath(store, quad.String("Alice")).Follow(isParentOf)
```

In this example, `isParentOf` is a reusable morphism that can be used to find the parents of any node in the graph. Morphisms make your code cleaner, more modular, and easier to read.

## Follow() and FollowR() for Complex Traversals

While `Out()` and `In()` are great for simple traversals, sometimes you need to follow a more complex path defined by a morphism. This is where `Follow()` and `FollowR()` (reverse follow) come in.

`Follow()` takes a morphism and applies it to the current path. This allows you to compose complex queries from smaller, reusable parts.

Let's define a morphism to find a person's grandparents:

```go
var isGrandparentOf = cayley.Morphism().In(quad.IRI("parent_of")).In(quad.IRI("parent_of"))

// Find Alice's grandparents
p := cayley.StartPath(store, quad.String("Alice")).Follow(isGrandparentOf)
```

This is much cleaner than writing `p.In(...).In(...)` every time you need to find grandparents.

## FollowRecursive() for Deep Traversal

Many real-world graphs contain hierarchical or recursive structures, such as organizational charts, file systems, or taxonomies. Querying these structures often requires traversing an unknown number of levels deep. The `FollowRecursive()` method is designed for exactly this purpose.

`FollowRecursive()` takes a morphism and repeatedly applies it until no new nodes can be found. For example, to find all ancestors of Alice in a family tree:

```go
var isParentOf = cayley.Morphism().In(quad.IRI("parent_of"))

p := cayley.StartPath(store, quad.String("Alice")).FollowRecursive(isParentOf)
```

This single line of code will traverse the `parent_of` relationship upwards, finding Alice's parents, grandparents, great-grandparents, and so on, until it reaches the top of the hierarchy.

## Back() for Backtracking

As you build up a path, you are essentially moving forward through the graph. Sometimes, however, you need to backtrack to a previous point in your path. The `Back()` method allows you to do this.

`Back()` takes a tag name (which we will cover next) and returns the path to the nodes that were tagged with that name. This is useful for queries where you need to explore a side path and then return to your main traversal.

For example, to find all people who have a child over the age of 30:

```go
p := cayley.StartPath(store).Tag("person").
    Out(quad.IRI("parent_of")).
    Has(quad.IRI("age"), quad.Int(30)).
    Back("person")
```

In this query, we first tag all nodes as `person`. Then, we find their children and filter for those who are 30. Finally, we use `Back("person")` to return to the original set of people who met the criteria.

## Tags for Result Collection

In complex queries, you often need to collect results from different points in your traversal. The `Tag()` and `And()` methods allow you to do this.

`Tag(tagName string)` assigns a tag to the nodes at the current point in the path. You can then use `Back()` to return to these tagged nodes, or you can use the tags to collect results.

When you use an iterator, you can pass a `map[string]refs.Ref` to the `TagResults()` method. This map will be filled with the tagged nodes for each result.

```go
p := cayley.StartPath(store, quad.String("Alice")).
    Out(quad.IRI("knows")).Tag("friend").
    Out(quad.IRI("age")).Tag("friend_age")

err := p.Iterate(nil).Each(func(result map[string]refs.Ref) error {
    friend := quad.NativeOf(result["friend"])
    age := quad.NativeOf(result["friend_age"])
    fmt.Printf("Alice's friend %v is %v years old\n", friend, age)
    return nil
})
```

This query finds Alice's friends and their ages, collecting both the friend and their age in the same result map. Tags are an indispensable tool for building queries that return rich, structured results.

By mastering these advanced path operations, you can write concise, powerful, and reusable queries to unlock the full potential of your graph data. The exercises for this chapter will challenge you to apply these patterns to solve complex graph problems.

---

### References

[1] Cayley Documentation. "Morphisms." https://cayley.gitbook.io/cayley/query-languages/path-api#morphisms
