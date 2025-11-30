# Chapter 3: Understanding Quads and the Quad Model

Now that you have written your first Cayley program and experienced the power of graph queries, it is time to dive deeper into the fundamental data structure that makes it all possible: the **quad**. Understanding quads thoroughly is essential for designing effective graph schemas and writing efficient queries. In this chapter, we will dissect the anatomy of a quad, explore the different types of values that can appear in each position, and learn about the semantic web standards that Cayley builds upon.

## Anatomy of a Quad

A quad in Cayley is a four-tuple consisting of a **subject**, a **predicate**, an **object**, and a **label**. This structure extends the traditional RDF triple by adding the fourth component, which provides crucial context and metadata capabilities.

The **subject** is the entity that the statement is about. It answers the question "Who or what are we talking about?" In the quad `{"Alice", "knows", "Bob", nil}`, Alice is the subject.

The **predicate** defines the relationship or property being described. It answers "What aspect or relationship?" In our example, `knows` is the predicate, indicating a relationship of acquaintance.

The **object** is the value or target of the relationship. It answers "What is the value or who is related?" Here, Bob is the object, the person whom Alice knows.

The **label** (sometimes called the graph name or context) provides metadata about the quad itself. It can indicate the source of the information, the time period when it was valid, the confidence level, or any other contextual information. When set to `nil`, it indicates that the quad belongs to the default graph.

This four-part structure gives Cayley tremendous flexibility. You can use labels to implement versioning, track provenance, manage access control, or partition your data into logical subgraphs.

## IRIs and Literals

In the semantic web world, and by extension in Cayley, we distinguish between two primary types of values: **IRIs** (Internationalized Resource Identifiers) and **literals**.

An **IRI** is a globally unique identifier for a resource. It is similar to a URL but more general. IRIs are used to unambiguously identify entities, properties, and concepts. For example, instead of using the string `"Alice"`, we might use the IRI `<http://example.org/people/alice>` to ensure there is no confusion with other entities named Alice.

In Cayley, you can create an IRI using the `quad.IRI` function:

```go
alice := quad.IRI("http://example.org/people/alice")
knows := quad.IRI("http://example.org/vocab/knows")
```

A **literal** is a concrete data value, such as a string, number, or date. Literals represent actual data rather than references to other entities. For instance, `"Alice"` as a string literal, or `25` as an integer literal representing an age.

Cayley provides several functions to create typed literals:

```go
name := quad.String("Alice")
age := quad.Int(25)
height := quad.Float(1.68)
birthdate := quad.Time(time.Date(1998, 3, 15, 0, 0, 0, 0, time.UTC))
```

The distinction between IRIs and literals is crucial. IRIs can appear as subjects, predicates, or objects in a quad, while literals typically appear only as objects. This reflects the semantic web principle that we make statements about resources (identified by IRIs), and those statements can have literal values.

## Blank Nodes

Sometimes, we need to describe an entity that does not have a natural global identifier, or we want to create an anonymous intermediate node in our graph. For these cases, RDF provides **blank nodes** (also called anonymous nodes or bnodes).

A blank node is a node that exists in the graph but does not have a globally unique IRI. It is identified only within the scope of a particular graph or dataset. Blank nodes are useful for representing complex structures, such as a person's address, without needing to mint a unique IRI for every address.

In Cayley, you can create a blank node using `quad.BNode`:

```go
address := quad.BNode("addr1")
store.AddQuad(quad.Make("Alice", "hasAddress", address, nil))
store.AddQuad(quad.Make(address, "street", "123 Main St", nil))
store.AddQuad(quad.Make(address, "city", "Springfield", nil))
```

In this example, `addr1` is a blank node representing Alice's address. It does not have a global IRI, but it serves as a connection point for the street and city information.

## Namespaces and Vocabularies

As your knowledge base grows, you will quickly find that using full IRIs everywhere becomes cumbersome. This is where **namespaces** and **vocabularies** come into play. A namespace is a prefix that represents a common base IRI, allowing you to write shorter, more readable identifiers.

For example, instead of writing `<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>` every time you want to specify the type of an entity, you can define a namespace prefix:

```go
import "github.com/cayleygraph/quad/voc"

func init() {
    voc.RegisterPrefix("rdf:", "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
    voc.RegisterPrefix("foaf:", "http://xmlns.com/foaf/0.1/")
    voc.RegisterPrefix("ex:", "http://example.org/")
}
```

Now you can use the short form `rdf:type`, `foaf:name`, or `ex:Person` in your code, and Cayley will expand them to their full IRIs.

A **vocabulary** is a collection of terms (IRIs) that define concepts and relationships for a particular domain. Common vocabularies include:

| Vocabulary | Prefix | Purpose |
|------------|--------|---------|
| RDF | rdf: | Core RDF concepts like `type`, `Property` |
| RDFS | rdfs: | RDF Schema for defining classes and properties |
| OWL | owl: | Web Ontology Language for complex semantics |
| FOAF | foaf: | Friend of a Friend - describing people and relationships |
| Dublin Core | dc: | Metadata for documents and resources |

By using established vocabularies, your data becomes interoperable with other systems and datasets that use the same vocabularies. This is a key principle of the Linked Data movement.

In the exercises for this chapter, you will practice creating a domain-specific vocabulary, using IRIs and literals appropriately, and leveraging labels for versioning and provenance tracking.

---

### References

[1] W3C. "RDF 1.1 Concepts and Abstract Syntax." https://www.w3.org/TR/rdf11-concepts/

[2] W3C. "RDF Schema 1.1." https://www.w3.org/TR/rdf-schema/

[3] Cayley. "Quad Package Documentation." https://pkg.go.dev/github.com/cayleygraph/quad
