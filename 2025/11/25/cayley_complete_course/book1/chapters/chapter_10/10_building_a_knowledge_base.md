
# Chapter 10: Building a Knowledge Base

Having mastered the fundamentals of Cayley, from basic queries to advanced data modeling, you are now ready to tackle one of the most powerful applications of graph databases: building a **knowledge base**. A knowledge base (KB) is a centralized repository of information, a machine-readable representation of facts, rules, and relationships about a particular domain. In this chapter, you will learn how to represent knowledge with quads, import external datasets, and implement simple inference rules to make your knowledge base more intelligent.

## Knowledge Representation with Quads

At the heart of any knowledge base is a robust model for representing information. Cayley's quad-based model is exceptionally well-suited for this task. As we've discussed, a quad `{subject, predicate, object, label}` can represent a single factual statement. By combining millions of these quads, we can construct a rich, interconnected web of knowledge.

Let's consider how to model a simple piece of knowledge: "Marie Curie was a physicist who was born in Warsaw."

We can break this down into several atomic facts:

*   Marie Curie is a person.
*   Marie Curie's occupation is physicist.
*   Marie Curie was born in Warsaw.
*   Warsaw is a city in Poland.

Using IRIs to uniquely identify our entities, we can represent this knowledge in Cayley as follows:

```nquads
<kb:marie_curie> <rdf:type> <kb:person> .
<kb:marie_curie> <kb:occupation> <kb:physicist> .
<kb:marie_curie> <kb:born_in> <kb:warsaw> .
<kb:warsaw> <kb:is_in> <kb:poland> .
```

This simple example illustrates the power of representing knowledge as a graph. We can now ask complex questions like "Which physicists were born in Poland?" by traversing the graph, even though we never explicitly stated that Marie Curie was born in Poland.

## Importing External Data

A knowledge base is only as good as the data it contains. Fortunately, there are vast amounts of structured data available on the web in formats like RDF, N-Triples, and JSON-LD. Cayley provides tools to easily import this data into your graph.

The `cayley load` command is your primary tool for bulk-loading data. It supports various formats and can load data from local files or remote URLs.

For example, to load a dataset of information about famous scientists from a remote N-Triples file, you would run:

```bash
./cayley load -i https://example.com/scientists.nt
```

This command will parse the file and add all the triples (or quads) to your Cayley database. This is an incredibly powerful way to bootstrap your knowledge base with vast amounts of publicly available information from sources like DBpedia (a machine-readable version of Wikipedia) or Wikidata.

## Implementing Simple Inference

One of the hallmarks of an intelligent system is the ability to **infer** new knowledge from existing facts. While Cayley is not a full-fledged reasoning engine, its query capabilities are powerful enough to implement simple forms of inference.

Let's consider a classic example: transitivity. If we know that "Warsaw is in Poland" and "Poland is in Europe," we should be able to infer that "Warsaw is in Europe." We can achieve this with a recursive query.

First, let's define a morphism for the `is_in` relationship:

```go
var isIn = cayley.Morphism().Out(quad.IRI("kb:is_in"))
```

Now, we can use `FollowRecursive()` to find all locations that Warsaw is in, directly or indirectly:

```go
p := cayley.StartPath(store, quad.IRI("kb:warsaw")).FollowRecursive(isIn)
```

This query will return both `kb:poland` and `kb:europe`, effectively performing a transitive inference. This pattern can be used to implement various forms of reasoning, such as:

*   **Symmetry**: If A is a sibling of B, then B is a sibling of A.
*   **Inverse relationships**: If A is a parent of B, then B is a child of A.
*   **Property chains**: If A lives in B and B is part of C, then A lives in C.

By combining data loading with inference queries, you can transform a static collection of facts into a dynamic, intelligent knowledge base that can answer questions and reveal insights that are not explicitly stated in the data.

In the exercises for this chapter, you will get hands-on experience with importing a real-world dataset and implementing your own inference rules to enrich your knowledge base.

---

### References

[1] DBpedia. https://www.dbpedia.org/

[2] Wikidata. https://www.wikidata.org/

[3] Cayley Documentation. "Gizmo API." https://cayley.gitbook.io/cayley/query-languages/gizmoapi
