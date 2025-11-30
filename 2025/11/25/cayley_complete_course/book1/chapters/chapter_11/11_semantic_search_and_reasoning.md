
# Chapter 11: Semantic Search and Reasoning

By now, you are proficient at querying your graph based on its explicit structure. However, the true power of a knowledge base lies not just in what is explicitly stated, but in what can be inferred. This chapter will introduce you to the concepts of semantic search and reasoning, showing you how to unlock the hidden knowledge in your graph and perform more intelligent queries.

## Full-Text Search Integration

While graph traversals are excellent for navigating relationships, they are not well-suited for searching within the content of your literals. For this, you need **full-text search**. Cayley can be integrated with full-text search engines like Elasticsearch or Bleve to provide powerful text search capabilities.

When you index your literals in a full-text search engine, you can perform complex text queries (such as keyword searches, phrase searches, and fuzzy searches) and then use the results of those queries as starting points for your graph traversals.

For example, you could find all documents that contain the phrase "machine learning" and then traverse the graph to find their authors, publication dates, and related topics.

## Pattern-Based Reasoning

**Reasoning** (or inference) is the process of deriving new facts from existing ones based on a set of rules. One of the simplest forms of reasoning is pattern-based reasoning, where you define rules that match specific patterns in your graph.

For example, you could define a rule that says "If a person is the parent of another person, then they are also an ancestor of that person."

```
parent_of(X, Y) -> ancestor_of(X, Y)
```

Cayley does not have a built-in rule engine, but you can implement this kind of reasoning in your application logic. You can write queries that find all `parent_of` relationships and then add the corresponding `ancestor_of` quads to your graph.

## Recursive Rules

More complex reasoning often involves recursive rules. For example, the full definition of an ancestor is recursive:

```
parent_of(X, Y) -> ancestor_of(X, Y)
parent_of(X, Y) AND ancestor_of(Y, Z) -> ancestor_of(X, Z)
```

This rule states that if X is a parent of Y, and Y is an ancestor of Z, then X is also an ancestor of Z. By repeatedly applying this rule, you can infer the entire ancestor hierarchy.

As we saw in Chapter 6, you can perform this kind of recursive traversal in Cayley using `FollowRecursive()`.

```go
var isAncestorOf = cayley.Morphism().Out(quad.IRI("parent_of"))
p := cayley.StartPath(store, quad.String("Alice")).FollowRecursive(isAncestorOf)
```

This allows you to query for inferred relationships without having to explicitly store all of them in your graph.

## Query Optimization

As your graph and your queries become more complex, performance becomes a critical concern. Cayley includes a sophisticated **query optimizer** that analyzes your queries and attempts to find the most efficient way to execute them.

The optimizer can perform a variety of transformations, such as reordering iterators, pushing down filters, and choosing the most efficient join strategy. For example, when performing an intersection between two paths, the optimizer will try to place the smaller result set on the left-hand side to reduce the number of comparisons.

While the optimizer is largely automatic, you can help it by:

*   **Providing accurate statistics**: The optimizer relies on statistics about your data (such as the number of quads and the size of your iterators) to make good decisions. Make sure your storage backend is configured to provide these statistics.

*   **Structuring your queries logically**: While the optimizer can reorder things, it is still good practice to structure your queries in a way that is as restrictive as possible, as early as possible. This helps to prune the search space and reduce the amount of work the optimizer has to do.

By combining graph traversals with full-text search and reasoning, you can build truly intelligent applications that go beyond simple data retrieval and provide deep insights into your knowledge base. The exercises for this chapter will guide you through the process of integrating a full-text search engine and implementing a simple reasoning engine on top of Cayley.

---

### References

[1] Russell, Stuart J., and Peter Norvig. "Artificial Intelligence: A Modern Approach." Pearson, 2020.

[2] Cayley Documentation. "Query Optimization." https://cayley.gitbook.io/cayley/advanced-topics/query-optimization
