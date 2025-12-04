---
Title: Graph Database Integration Analysis
Ticket: ANALYZE-FACT-EXTRACTION
Status: active
Topics:
    - analysis
    - go
    - fact-extraction
DocType: analysis
Intent: long-term
Owners: []
RelatedFiles:
    - Path: 2025/11/25/fact-extraction-go/DIARY_CAYLEY.md
      Note: |-
        Implementation diary for Cayley integration
        Implementation diary documenting Cayley integration
    - Path: 2025/11/25/fact-extraction-go/DIARY_CAYLEY_EMBEDDINGS.md
      Note: |-
        Cayley embeddings diary
        Diary documenting embeddings integration with Cayley graph database
    - Path: 2025/11/25/fact-extraction-go/cayley-embeddings/facts.nq
      Note: |-
        N-Quads format data file
        N-Quads format data file with triples and metadata
    - Path: 2025/11/25/fact-extraction-go/cayley-embeddings/load_facts_to_cayley.py
      Note: |-
        Python script to load facts into Cayley
        Python script to load facts from SQLite into Cayley and generate N-Quads format
    - Path: 2025/11/25/fact-extraction-go/graph-query/gizmo_queries.js
      Note: |-
        Gizmo query patterns
        15 Gizmo query patterns including morphisms
    - Path: 2025/11/25/fact-extraction-go/graph-query/gizmo_runner.go
      Note: |-
        Gizmo API query runner
        Gizmo query runner implementation for advanced graph queries
    - Path: 2025/11/25/fact-extraction-go/graph-query/main.go
      Note: |-
        Main graph query implementation
        Main graph query CLI implementation with load
    - Path: 2025/11/25/fact-extraction-go/graph-query/queries_output.txt
      Note: |-
        Query output examples
        Example query output showing relationships for key entities
ExternalSources: []
Summary: 'Comprehensive analysis of Cayley graph database integration: quad model, data loading from SQLite, path-based query API, Gizmo query language, N-Quads format, and comparison with SQL for relationship queries'
LastUpdated: 2025-12-03T11:30:22.936400092-05:00
---





# Graph Database Integration Analysis

## Research Objective

Analyze the Cayley graph database integration, including N-Quads format, query patterns, graph traversal, and how it enables relationship discovery.

## Research Instructions

### Phase 1: Understand Cayley

1. **Research Cayley graph database**:
   - What is Cayley?
   - What is the N-Quads format?
   - What is the Gizmo query language?
   - How does it compare to other graph databases?

### Phase 2: Data Loading Analysis

**Files to analyze:**
- `vibes/2025/11/25/fact-extraction-go/graph-query/main.go`
- `vibes/2025/11/25/fact-extraction-go/cayley-embeddings/load_facts_to_cayley.py`
- `vibes/2025/11/25/fact-extraction-go/cayley-embeddings/facts.nq`

**Tasks:**
1. **Document data loading process**:
   - How are RDF triples converted to N-Quads?
   - What metadata is included?
   - How is the graph database initialized?
   - What is the loading performance?

2. **Analyze N-Quads format**:
   - Extract sample quads
   - Document the structure
   - Understand the graph component
   - Analyze metadata quads

3. **Code analysis**:
   - Trace the loading code
   - Document conversion logic
   - Analyze error handling

### Phase 3: Query Patterns Analysis

**Files to analyze:**
- `vibes/2025/11/25/fact-extraction-go/graph-query/gizmo_queries.js`
- `vibes/2025/11/25/fact-extraction-go/graph-query/gizmo_runner.go`
- `vibes/2025/11/25/fact-extraction-go/graph-query/queries_output.txt`

**Tasks:**
1. **Document query patterns**:
   - List all 15 query patterns
   - Document what each query does
   - Provide examples
   - Analyze query complexity

2. **Test queries**:
   - Run each query pattern
   - Document results
   - Analyze performance
   - Identify use cases

3. **Gizmo language analysis**:
   - Document Gizmo syntax
   - Understand traversal patterns
   - Analyze query optimization

### Phase 4: Graph Traversal Analysis

**Tasks:**
1. **Document traversal patterns**:
   - 1-hop neighbors
   - Multi-hop paths
   - Mutual connections
   - Network analysis

2. **Analyze relationship discovery**:
   - What relationships can be discovered?
   - What insights does graph traversal provide?
   - How does it compare to SQL queries?

3. **Performance analysis**:
   - Query execution time
   - Scalability
   - Index usage

### Phase 5: Integration with Extraction Pipeline

**Files to analyze:**
- `vibes/2025/11/25/fact-extraction-go/DIARY_CAYLEY.md`
- `vibes/2025/11/25/fact-extraction-go/DIARY_CAYLEY_EMBEDDINGS.md`

**Tasks:**
1. **Document integration points**:
   - How does extraction feed into graph?
   - What is the data flow?
   - How is consistency maintained?

2. **Analyze use cases**:
   - What problems does graph solve?
   - What queries are enabled?
   - What insights are discovered?

### Phase 6: Comparison with SQL

**Tasks:**
1. **Compare query approaches**:
   - Graph queries vs SQL queries
   - Performance comparison
   - Expressiveness comparison
   - Use case fit

2. **Document advantages**:
   - When is graph better?
   - When is SQL better?
   - Hybrid approaches

### Phase 7: Recommendations

**Deliverables:**
1. **Graph Database Architecture Documentation**
2. **Query Pattern Catalog**
3. **Performance Analysis Report**
4. **Integration Guide**
5. **Recommendations**

## Key Questions to Answer

1. **How effective is graph database for this use case?**
2. **What query patterns are most useful?**
3. **How does it compare to SQL?**
4. **What improvements are needed?**

## Related Files

- `vibes/2025/11/25/fact-extraction-go/graph-query/`
- `vibes/2025/11/25/fact-extraction-go/cayley-embeddings/`
- `vibes/2025/11/25/fact-extraction-go/DIARY_CAYLEY.md`

## Expected Timeline: 15-20 hours

---

## Analysis: Cayley Graph Database Integration

### Introduction: Why Graph Databases for Fact Extraction

The integration of Cayley graph database into the fact extraction pipeline represents a fundamental shift from relational to graph-based data modeling. While SQLite excels at storing structured facts and supporting simple queries, graph databases unlock powerful capabilities for relationship discovery, path finding, and network analysis. The RDF triple structure (actor-action-target) maps naturally to graph edges, making graph databases an ideal fit for exploring connections between entities extracted from documents.

Cayley was chosen as the graph database solution for several strategic reasons: it's written in Go (enabling native integration with the Go fact extractor), supports embedded operation with BoltDB (no server required), uses a quad-based data model that extends RDF triples with labels for metadata, and provides a Gremlin-like query API that makes graph traversal intuitive. The integration demonstrates how graph databases can transform extracted facts from isolated statements into an interconnected knowledge network that reveals patterns, relationships, and insights that would be difficult or impossible to discover with SQL alone.

### Understanding Cayley: Quad-Based Graph Model

Cayley uses a quad-based data model, which extends the familiar RDF triple (subject-predicate-object) with a fourth component: the label. This label enables Cayley to attach metadata to relationships, support named graphs, and create more expressive data structures than pure triples allow. Understanding this quad model is essential to understanding how the fact extraction data is represented and queried in Cayley.

**Quad Structure**:

A quad consists of four components:
- **Subject**: The source node (e.g., an actor like "Jeffrey Epstein")
- **Predicate**: The relationship type (e.g., an action like "met with")
- **Object**: The target node (e.g., another entity like "Donald Trump")
- **Label**: An optional identifier for the quad itself (used for metadata)

**Quad Model in Practice** (`graph-query/main.go` lines 189-220):

```189:220:vibes/2025/11/25/fact-extraction-go/graph-query/main.go
		// Create main relationship quad
		quads = append(quads, quad.Make(
			t.Actor,
			t.Action,
			t.Target,
			fmt.Sprintf("triple:%d", t.ID),
		))

		// Add metadata as additional quads
		tripleNode := quad.IRI(fmt.Sprintf("triple:%d", t.ID))

		quads = append(quads, quad.Make(tripleNode, "doc_id", t.DocID, nil))
		quads = append(quads, quad.Make(tripleNode, "explicit_topic", t.ExplicitTopic, nil))
		quads = append(quads, quad.Make(tripleNode, "implicit_topic", t.ImplicitTopic, nil))

		if t.Timestamp.Valid {
			quads = append(quads, quad.Make(tripleNode, "timestamp", t.Timestamp.String, nil))
		}

		if t.Location.Valid {
			quads = append(quads, quad.Make(tripleNode, "location", t.Location.String, nil))
		}

		// Parse and add tags
		if t.Tags != "" {
			var tags []string
			if err := json.Unmarshal([]byte(t.Tags), &tags); err == nil {
				for _, tag := range tags {
					quads = append(quads, quad.Make(tripleNode, "tag", tag, nil))
				}
			}
		}
```

The implementation converts each RDF triple into multiple quads: one main relationship quad (actor-action-target) with the triple ID as the label, and additional metadata quads that attach information like document ID, topics, timestamps, locations, and tags to the triple. This design allows queries to filter relationships by metadata while maintaining the core graph structure.

**Why Quads Instead of Triples?**

The quad model provides several advantages over pure triples. First, the label enables attaching arbitrary metadata to relationships without polluting the graph structure—metadata quads reference the triple by its label, keeping the main graph clean. Second, labels enable named graphs, allowing different datasets or versions to coexist in the same database. Third, the label provides a way to uniquely identify and reference specific relationships, which is essential for provenance tracking and metadata queries.

### Data Loading: From SQLite to Graph

The data loading process transforms structured SQLite records into graph quads, creating an interconnected network from what were previously isolated facts. This transformation is not just a format conversion—it's a semantic shift that enables graph-based reasoning and traversal.

**Loading Process** (`graph-query/main.go` lines 147-241):

```147:241:vibes/2025/11/25/fact-extraction-go/graph-query/main.go
func loadDataIntoGraph() error {
	// Open SQLite database
	db, err := sql.Open("sqlite3", dbPath)
	if err != nil {
		return fmt.Errorf("failed to open SQLite: %w", err)
	}
	defer db.Close()

	// Initialize Cayley graph
	if err := graph.InitQuadStore("bolt", graphPath, nil); err != nil && err != graph.ErrDatabaseExists {
		return fmt.Errorf("failed to init graph: %w", err)
	}

	store, err := cayley.NewGraph("bolt", graphPath, nil)
	if err != nil {
		return fmt.Errorf("failed to open graph: %w", err)
	}
	defer store.Close()

	// Query all triples
	rows, err := db.Query(`
		SELECT id, doc_id, timestamp, actor, action, target, location, 
		       actor_likely_type, triple_tags, explicit_topic, implicit_topic, sequence_order
		FROM rdf_triples
	`)
	if err != nil {
		return fmt.Errorf("failed to query triples: %w", err)
	}
	defer rows.Close()

	// Load into graph
	count := 0
	var quads []quad.Quad

	for rows.Next() {
		var t RDFTriple
		if err := rows.Scan(&t.ID, &t.DocID, &t.Timestamp, &t.Actor, &t.Action,
			&t.Target, &t.Location, &t.ActorType, &t.Tags, &t.ExplicitTopic,
			&t.ImplicitTopic, &t.SequenceOrder); err != nil {
			return fmt.Errorf("failed to scan row: %w", err)
		}

		// Create main relationship quad
		quads = append(quads, quad.Make(
			t.Actor,
			t.Action,
			t.Target,
			fmt.Sprintf("triple:%d", t.ID),
		))

		// Add metadata as additional quads
		tripleNode := quad.IRI(fmt.Sprintf("triple:%d", t.ID))

		quads = append(quads, quad.Make(tripleNode, "doc_id", t.DocID, nil))
		quads = append(quads, quad.Make(tripleNode, "explicit_topic", t.ExplicitTopic, nil))
		quads = append(quads, quad.Make(tripleNode, "implicit_topic", t.ImplicitTopic, nil))

		if t.Timestamp.Valid {
			quads = append(quads, quad.Make(tripleNode, "timestamp", t.Timestamp.String, nil))
		}

		if t.Location.Valid {
			quads = append(quads, quad.Make(tripleNode, "location", t.Location.String, nil))
		}

		// Parse and add tags
		if t.Tags != "" {
			var tags []string
			if err := json.Unmarshal([]byte(t.Tags), &tags); err == nil {
				for _, tag := range tags {
					quads = append(quads, quad.Make(tripleNode, "tag", tag, nil))
				}
			}
		}

		count++
		if count%100 == 0 {
			if err := store.AddQuadSet(quads); err != nil {
				return fmt.Errorf("failed to add quads: %w", err)
			}
			quads = quads[:0]
			fmt.Printf("Loaded %d triples...\n", count)
		}
	}

	// Add remaining quads
	if len(quads) > 0 {
		if err := store.AddQuadSet(quads); err != nil {
			return fmt.Errorf("failed to add final quads: %w", err)
		}
	}

	fmt.Printf("✓ Loaded %d triples into graph\n", count)
	return nil
}
```

The loading process follows a batch pattern: quads are accumulated in memory and written to the graph database every 100 triples. This batching improves performance by reducing the number of database operations while keeping memory usage reasonable. The process handles optional fields (like timestamps and locations) by checking their validity before creating metadata quads, ensuring that only present data is stored in the graph.

**BoltDB Backend**:

Cayley uses BoltDB as the embedded storage backend, which means the entire graph database is stored in a single file with no server process required. This design choice simplifies deployment and makes the graph database portable—the entire knowledge graph can be copied, backed up, or shared as a single file. BoltDB provides ACID transactions and efficient key-value storage optimized for read-heavy workloads, which aligns well with the query patterns expected in graph exploration.

### Graph Query Patterns: Traversing Relationships

Cayley provides a path-based query API that makes graph traversal intuitive. Instead of writing complex SQL joins to follow relationships, queries express traversal patterns directly: "start at this node, follow outgoing edges, filter by predicate, continue to neighbors." This declarative approach makes relationship queries natural and readable.

**Basic Traversal: Finding Relationships** (`graph-query/main.go` lines 249-285):

```249:285:vibes/2025/11/25/fact-extraction-go/graph-query/main.go
func queryActorRelationships(actor string) {
	ctx := context.Background()
	p := cayley.StartPath(store, quad.String(actor)).Out()

	fmt.Printf("\n🔍 Relationships for: %s\n", actor)
	fmt.Println(strings.Repeat("=", 80))

	count := 0
	err := p.Iterate(ctx).EachValue(nil, func(value quad.Value) {
		// Get the predicate (action) and object (target)
		it := store.QuadIterator(quad.Subject, store.ValueOf(quad.String(actor)))
		defer it.Close()

		for it.Next(ctx) {
			q := store.Quad(it.Result())
			if q.Object.String() == value.String() {
				count++
				fmt.Printf("%d. %s → [%s] → %s\n", count, actor, q.Predicate, q.Object)

				// Get metadata
				label := q.Label
				if label != nil {
					printTripleMetadata(ctx, label)
				}
				fmt.Println()
			}
		}
	})

	if err != nil {
		log.Printf("Error iterating: %v", err)
	}

	if count == 0 {
		fmt.Println("No relationships found")
	}
}
```

This query demonstrates the basic pattern: start at a specific actor node, follow all outgoing edges (`.Out()`), and iterate over the results. The implementation then queries back to find the specific quads that connect the actor to each target, allowing it to display both the relationship type (predicate) and retrieve metadata via the quad label.

**Neighbor Discovery** (`graph-query/main.go` lines 287-310):

```287:310:vibes/2025/11/25/fact-extraction-go/graph-query/main.go
func findNeighbors(person string) {
	ctx := context.Background()

	fmt.Printf("\n👥 Direct connections to: %s\n", person)
	fmt.Println(strings.Repeat("=", 80))

	// Find outgoing relationships
	fmt.Println("\n📤 Outgoing relationships:")
	outPath := cayley.StartPath(store, quad.String(person)).Out()
	count := 0
	outPath.Iterate(ctx).EachValue(nil, func(value quad.Value) {
		count++
		fmt.Printf("  %d. %s\n", count, value)
	})

	// Find incoming relationships
	fmt.Println("\n📥 Incoming relationships:")
	inPath := cayley.StartPath(store, quad.String(person)).In()
	count = 0
	inPath.Iterate(ctx).EachValue(nil, func(value quad.Value) {
		count++
		fmt.Printf("  %d. %s\n", count, value)
	})
}
```

The neighbor discovery query demonstrates bidirectional traversal: `.Out()` follows edges where the person is the subject (outgoing relationships), while `.In()` follows edges where the person is the object (incoming relationships). This distinction is crucial for understanding network structure—outgoing relationships show who the person connects to, while incoming relationships show who connects to them.

**Path Finding** (`graph-query/main.go` lines 312-338):

```312:338:vibes/2025/11/25/fact-extraction-go/graph-query/main.go
func findPaths(from, to string) {
	ctx := context.Background()

	fmt.Printf("\n🛤️  Finding paths from '%s' to '%s'\n", from, to)
	fmt.Println(strings.Repeat("=", 80))

	// Try to find paths up to 3 hops
	for hops := 1; hops <= 3; hops++ {
		fmt.Printf("\nSearching %d-hop paths...\n", hops)

		p := cayley.StartPath(store, quad.String(from))
		for i := 0; i < hops; i++ {
			p = p.Out()
		}
		p = p.Has(quad.IRI("id"), quad.String(to))

		found := false
			p.Iterate(ctx).EachValue(nil, func(value quad.Value) {
				found = true
				fmt.Printf("  Found: %s\n", value)
			})

		if found {
			break
		}
	}
}
```

The path-finding implementation demonstrates multi-hop traversal: it chains multiple `.Out()` calls to traverse multiple relationship hops, then filters to check if the target node is reached. While this implementation is simplified (it checks reachability rather than returning the actual path), it demonstrates how graph databases make multi-hop queries straightforward compared to SQL, which would require recursive CTEs or multiple self-joins.

### Gizmo Query Language: Advanced Graph Patterns

Gizmo is Cayley's JavaScript-based query language that provides a more expressive syntax for complex graph queries. While the Go path API is powerful, Gizmo enables declarative queries that express graph patterns concisely, making it easier to write and understand complex relationship queries.

**Morphisms: Reusable Path Patterns** (`graph-query/gizmo_queries.js` lines 11-26):

```11:26:vibes/2025/11/25/fact-extraction-go/graph-query/gizmo_queries.js
// Define a morphism for "acted upon" relationships
var actedUpon = g.Morphism()
  .out("action")
  .out("target");

// Define a morphism for reverse relationships (who acted on X)
var actedUponBy = g.Morphism()
  .in("target")
  .in("action");

// Define a morphism for 2-hop relationships
var twoHopRelationship = g.Morphism()
  .out("action")
  .out("target")
  .out("action")
  .out("target");
```

Morphisms are reusable path patterns that can be applied to different starting nodes. The `actedUpon` morphism defines a two-step traversal: follow an "action" edge, then follow a "target" edge. This pattern can be reused across multiple queries, making complex traversals readable and maintainable.

**Query Patterns** (`graph-query/gizmo_queries.js` lines 32-40):

```32:40:vibes/2025/11/25/fact-extraction-go/graph-query/gizmo_queries.js
function findAllRelationships(personName) {
  return g.V(personName)
    .tag("person")
    .out("action")
    .tag("action")
    .out("target")
    .tag("target")
    .all();
}
```

This query demonstrates Gizmo's fluent API: start at a vertex (`.V()`), tag intermediate results for later reference (`.tag()`), traverse edges (`.out()`), and collect all results (`.all()`). The `.tag()` method is particularly powerful—it allows queries to reference intermediate nodes, enabling complex patterns like "find all people connected to both A and B."

**Mutual Connections Query** (`graph-query/gizmo_queries.js` lines 48-53):

```48:53:vibes/2025/11/25/fact-extraction-go/graph-query/gizmo_queries.js
function findMutualConnections(personA, personB) {
  var aConnections = g.V(personA).follow(actedUpon);
  var bConnections = g.V(personB).follow(actedUpon);
  
  return aConnections.intersect(bConnections).all();
}
```

This query demonstrates set operations: it finds all connections from person A, finds all connections from person B, then intersects the two sets to find mutual connections. This pattern would be extremely difficult to express in SQL but is natural in Gizmo.

**Network Neighborhood Query** (`graph-query/gizmo_queries.js` lines 164-172):

```164:172:vibes/2025/11/25/fact-extraction-go/graph-query/gizmo_queries.js
function findNetworkNeighborhood(personName, hops) {
  var connection = g.Morphism().out("action").out("target");
  
  return g.V(personName)
    .tag("center")
    .followRecursive(connection)
    .tag("neighbor")
    .all();
}
```

The recursive traversal pattern (`.followRecursive()`) enables queries of unknown depth—finding all nodes reachable from a starting point regardless of how many hops away they are. This is essential for network analysis and discovering indirect connections.

### N-Quads Format: Standardized Graph Serialization

N-Quads is a standardized format for serializing graph data, extending N-Triples with a fourth field for labels. The fact extraction pipeline generates N-Quads files that can be loaded into Cayley or other graph databases, providing interoperability and enabling data exchange.

**N-Quads Structure** (`cayley-embeddings/facts.nq` sample):

```1:22:vibes/2025/11/25/fact-extraction-go/cayley-embeddings/facts.nq
<Jeffrey_Epstein> <owned> <New_York_City_townhouse> .
<Jeffrey_Epstein_owned_New_York_City_townhouse> <confidence> "0.95" .
<Jeffrey_Epstein_owned_New_York_City_townhouse> <reasoning> "The document describes Epstein's New York townhouse extensively and mentions it as a significant residence." .
<Jeffrey_Epstein> <owned> <Palm_Beach_property_at_358_El_Brillo_Way> .
<Jeffrey_Epstein_owned_Palm_Beach_property_at_358_El_Brillo_Way> <confidence> "0.98" .
<Jeffrey_Epstein_owned_Palm_Beach_property_at_358_El_Brillo_Way> <reasoning> "The text references Epstein's Palm Beach residence and police police activity related to that property." .
<Jeffrey_Epstein> <pleaded_guilty> <state_solicitation_charges> .
<Jeffrey_Epstein_pleaded_guilty_state_solicitation_charges> <confidence> "1.0" .
<Jeffrey_Epstein_pleaded_guilty_state_solicitation_charges> <reasoning> "Text indicates Epstein pleaded guilty to solicitation charges in 2008 and served jail time." .
<Jeffrey_Epstein> <served> <thirteen_months_of_eighteen-month_sentence> .
<Jeffrey_Epstein_served_thirteen_months_of_eighteen-month_sentence> <confidence> "0.98" .
<Jeffrey_Epstein_served_thirteen_months_of_eighteen-month_sentence> <reasoning> "Document states Epstein served a jail sentence with work-release privileges after pleading guilty." .
<Leslie_Wexner> <acquired> <Herbert_N._Straus_Mansion_on_East_71st_Street> .
<Leslie_Wexner_acquired_Herbert_N._Straus_Mansion_on_East_71st_Street> <confidence> "0.95" .
<Leslie_Wexner_acquired_Herbert_N._Straus_Mansion_on_East_71st_Street> <reasoning> "The document mentions that Leslie Wexner acquired the property that later served as Epstein's residence." .
<Jeffrey_Epstein> <was_accompanied_by> <Ghislaine_Maxwell_and_Tony_Randall> .
<Jeffrey_Epstein_was_accompanied_by_Ghislaine_Maxwell_and_Tony_Randall> <confidence> "0.9" .
<Jeffrey_Epstein_was_accompanied_by_Ghislaine_Maxwell_and_Tony_Randall> <reasoning> "Describes a photograph featuring Epstein with Ghislaine Maxwell and Tony Randall at a public event." .
<Jeffrey_Epstein> <was_accompanied_by> <Donald_Trump> .
<Jeffrey_Epstein_was_accompanied_by_Donald_Trump> <confidence> "0.9" .
<Jeffrey_Epstein_was_accompanied_by_Donald_Trump> <reasoning> "The document lists a photo showing Epstein with Donald Trump attending an event." .
```

Each line represents a quad: subject, predicate, object, and optionally a label (the fourth field, omitted here but implied). IRIs are enclosed in angle brackets, literals in quotes, and the line ends with a period. This format is human-readable, machine-parseable, and standardized, making it ideal for data exchange and archival.

**N-Quads Generation** (`cayley-embeddings/load_facts_to_cayley.py` lines 49-88):

```49:88:vibes/2025/11/25/fact-extraction-go/cayley-embeddings/load_facts_to_cayley.py
def generate_nquads(triples, entities, relations, output_path):
    """Generate N-Quads file for Cayley"""
    with open(output_path, 'w') as f:
        # Write triples
        for triple in triples:
            actor = iri_encode(triple['actor'])
            action = iri_encode(triple['action'])
            target = iri_encode(triple['target'])
            
            # Main triple
            f.write(f'<{actor}> <{action}> <{target}> .\n')
            
            # Add metadata as separate triples
            if triple.get('confidence'):
                f.write(f'<{actor}_{action}_{target}> <confidence> "{triple["confidence"]}" .\n')
            
            if triple.get('reasoning'):
                reasoning = triple['reasoning'].replace('"', '\\"').replace('\n', ' ')[:500]
                f.write(f'<{actor}_{action}_{target}> <reasoning> "{reasoning}" .\n')
        
        # Write entity types and descriptions
        for name, entity in entities.items():
            name_iri = iri_encode(name)
            if entity.get('entity_type'):
                entity_type_iri = iri_encode(entity['entity_type'])
                f.write(f'<{name_iri}> <rdf:type> <{entity_type_iri}> .\n')
            if entity.get('description'):
                desc = entity['description'].replace('"', '\\"').replace('\n', ' ')[:500]
                f.write(f'<{name_iri}> <description> "{desc}" .\n')
        
        # Write relation types and descriptions
        for name, relation in relations.items():
            name_iri = iri_encode(name)
            if relation.get('relation_type'):
                rel_type_iri = iri_encode(relation['relation_type'])
                f.write(f'<relation:{name_iri}> <rdf:type> <RelationType:{rel_type_iri}> .\n')
            if relation.get('description'):
                desc = relation['description'].replace('"', '\\"').replace('\n', ' ')[:500]
                f.write(f'<relation:{name_iri}> <description> "{desc}" .\n')
    
    print(f"Generated {output_path} with {len(triples)} triples")
```

The N-Quads generator converts Python data structures into the standardized format, handling IRI encoding (replacing spaces with underscores, removing special characters) and escaping string literals. The generator creates separate quads for metadata (confidence, reasoning) and entity/relation descriptions, maintaining the separation between graph structure and metadata.

### Graph Statistics and Analysis

The graph database enables statistical analysis that reveals patterns in the extracted knowledge. These statistics provide insights into network structure, relationship types, and entity connectivity that would be difficult to compute with SQL.

**Statistics Implementation** (`graph-query/main.go` lines 357-419):

```357:419:vibes/2025/11/25/fact-extraction-go/graph-query/main.go
func showStats() {
	ctx := context.Background()

	fmt.Println("\n📊 Graph Database Statistics")
	fmt.Println(strings.Repeat("=", 80))

	// Count total quads
	it := store.QuadsAllIterator()
	defer it.Close()

	quadCount := 0
	for it.Next(ctx) {
		quadCount++
	}

	fmt.Printf("Total quads: %d\n", quadCount)

	// Count unique subjects (actors)
	subjects := make(map[string]bool)
	it = store.QuadsAllIterator()
	for it.Next(ctx) {
		q := store.Quad(it.Result())
		subjects[q.Subject.String()] = true
	}
	it.Close()

	fmt.Printf("Unique subjects (actors): %d\n", len(subjects))

	// Count unique predicates (actions)
	predicates := make(map[string]int)
	it = store.QuadsAllIterator()
	for it.Next(ctx) {
		q := store.Quad(it.Result())
		predicates[q.Predicate.String()]++
	}
	it.Close()

	fmt.Printf("Unique predicates (actions): %d\n", len(predicates))

	fmt.Println("\nTop 10 predicates:")
	// Sort and display top predicates
	type kv struct {
		Key   string
		Value int
	}
	var sorted []kv
	for k, v := range predicates {
		sorted = append(sorted, kv{k, v})
	}

	// Simple bubble sort
	for i := 0; i < len(sorted); i++ {
		for j := i + 1; j < len(sorted); j++ {
			if sorted[j].Value > sorted[i].Value {
				sorted[i], sorted[j] = sorted[j], sorted[i]
			}
		}
	}

	for i := 0; i < 10 && i < len(sorted); i++ {
		fmt.Printf("  %d. %s: %d\n", i+1, sorted[i].Key, sorted[i].Value)
	}
}
```

The statistics function iterates through all quads to compute aggregate metrics: total quad count, unique subjects (entities), unique predicates (relationship types), and frequency distribution of predicates. This analysis reveals which relationship types are most common in the extracted data, providing insights into the nature of the knowledge graph.

**Query Results** (`graph-query/queries_output.txt`):

The query output demonstrates the graph database in action, showing relationships for key entities like Jeffrey Epstein, Alan Dershowitz, Donald Trump, Prince Andrew, and Ghislaine Maxwell. The results reveal network structure: who connects to whom, what types of relationships exist, and how entities are positioned in the network. For example, the output shows that Jeffrey Epstein has many outgoing relationships (actions he performed), while Prince Andrew has both incoming and outgoing relationships, indicating a more central position in the network.

### Integration with Extraction Pipeline

The graph database integration completes the fact extraction pipeline by providing a query interface for the extracted knowledge. The integration follows a two-stage process: extraction stores facts in SQLite, then a loading step converts SQLite records into graph quads.

**Data Flow**:

```
Documents → Fact Extraction → SQLite (rdf_triples) → Graph Loader → Cayley Graph → Query Interface
```

This design separates concerns: SQLite provides reliable storage and simple queries, while Cayley enables advanced graph analysis. The loading step can be run incrementally as new facts are extracted, keeping the graph database synchronized with the extraction results.

**Python Loader** (`cayley-embeddings/load_facts_to_cayley.py`):

The Python loader demonstrates an alternative approach to graph loading, generating N-Quads files that can be loaded into Cayley using standard tools. This approach provides flexibility—the N-Quads file can be loaded into different graph databases, shared with other systems, or archived for long-term storage.

### Comparison: Graph vs SQL for Relationship Queries

Graph databases excel at relationship queries that are difficult or inefficient in SQL. Understanding these differences helps explain why Cayley was integrated into the fact extraction pipeline.

**SQL Approach**:

Finding all relationships for an actor in SQL requires a simple query:
```sql
SELECT actor, action, target 
FROM rdf_triples 
WHERE actor = 'Jeffrey Epstein';
```

However, finding multi-hop paths becomes exponentially more complex:
```sql
-- 2-hop path (A → B → C)
SELECT t1.actor, t1.target as intermediate, t2.target
FROM rdf_triples t1
JOIN rdf_triples t2 ON t1.target = t2.actor
WHERE t1.actor = 'Jeffrey Epstein';
```

Each additional hop requires another join, making deep traversals impractical.

**Graph Approach**:

The same queries in Cayley are straightforward:
```go
// 1-hop
cayley.StartPath(store, "Jeffrey Epstein").Out()

// 2-hop
cayley.StartPath(store, "Jeffrey Epstein").Out().Out()

// N-hop (recursive)
cayley.StartPath(store, "Jeffrey Epstein").FollowRecursive(...)
```

Graph databases are optimized for traversal, making multi-hop queries efficient and natural.

**When to Use Each**:

SQL excels at:
- Aggregations and statistics
- Filtering by exact values
- Joining across multiple tables
- Transactional updates

Graph databases excel at:
- Relationship traversal
- Path finding
- Network analysis
- Pattern matching in graph structure

The fact extraction pipeline uses both: SQLite for storage and simple queries, Cayley for relationship exploration and network analysis.

### Embeddings Integration: Semantic Search in Graphs

The Cayley embeddings integration extends the graph database with semantic search capabilities, enabling queries that combine graph structure with vector similarity. This hybrid approach allows finding entities similar to a query text, then exploring their relationships in the graph.

**Architecture** (`DIARY_CAYLEY_EMBEDDINGS.md`):

The embeddings integration uses a service layer that combines an embedding index (for vector similarity) with the Cayley graph store (for structural queries). Queries can start with semantic similarity (finding entities similar to query text) and then traverse the graph to discover relationships, or start with graph constraints and rerank results using embedding similarity.

**Hybrid Search Pattern**:

```go
// Semantic seed set via embeddings
seeds := embeddingIndex.Search("Jeffrey Epstein connections", k=50)

// Graph constraints
persons := graph.HasType("Person")

// Combine: semantic + structural
results := graph.Intersect(seeds, persons)

// Rerank by combined scores
reranked := reranker.Rerank(results, 
    embeddingWeight=0.6,
    graphWeight=0.2,
    typeWeight=0.2)
```

This pattern demonstrates how embeddings and graphs complement each other: embeddings provide semantic understanding (finding conceptually similar entities), while graphs provide structural understanding (exploring relationships and connections).

### Lessons Learned: Graph Database Integration

The Cayley integration provides several important lessons about graph databases, data modeling, and query design.

**Lesson 1: Quad Model Enables Rich Metadata**

The quad model's label component enables attaching metadata to relationships without polluting the graph structure. This separation is crucial for maintaining clean graph queries while supporting rich metadata queries. The implementation uses triple IDs as labels, creating a bridge between the graph structure and metadata.

**Lesson 2: Graph Traversal is Natural for Relationships**

Queries that traverse relationships are much more natural in graph databases than in SQL. The path-based API makes multi-hop queries straightforward, enabling network analysis that would be impractical with relational queries.

**Lesson 3: Embedded Databases Simplify Deployment**

Using BoltDB as an embedded backend eliminates the need for a separate database server, making deployment simpler and enabling portable knowledge graphs. The entire graph database is a single file that can be copied, backed up, or shared easily.

**Lesson 4: Gizmo Provides Expressive Query Language**

Gizmo's declarative syntax makes complex graph patterns readable and maintainable. Morphisms enable reusable path patterns, and set operations (intersect, union) enable sophisticated relationship queries that would be difficult to express in imperative code.

**Lesson 5: Graph and SQL Complement Each Other**

The hybrid approach—using SQLite for storage and Cayley for queries—demonstrates that graph and relational databases are complementary, not competing. Each excels at different types of queries, and using both provides the best of both worlds.

### Current State and Future Directions

The Cayley graph database integration successfully provides a query interface for extracted facts, enabling relationship discovery and network analysis. The implementation demonstrates core graph database concepts and provides a foundation for advanced features.

**What Works Well**:

- Quad-based data model naturally represents RDF triples with metadata
- Path-based query API makes relationship traversal intuitive
- BoltDB embedded backend simplifies deployment
- Gizmo query language enables expressive graph patterns
- Integration with fact extraction pipeline is clean and maintainable

**Areas for Enhancement**:

- Path finding could return actual paths, not just reachability
- Metadata queries could be optimized with better indexing
- Gizmo queries require HTTP server for full execution (could be improved)
- Embeddings integration could be extended with real vector search
- Visualization integration would enable interactive graph exploration

**Design Philosophy**:

The graph database integration follows a pragmatic approach: start with core functionality (loading and basic queries), then extend with advanced features (Gizmo, embeddings) as needed. This incremental development ensures that each component is well-understood and tested before adding complexity.
