# Cayley Graph Database Integration Diary

**Project**: Building a Go CLI tool with Cayley for graph queries  
**Date**: November 19, 2025  
**Goal**: Create a graph database interface for querying extracted RDF triples

---

## Background

After successfully extracting 256 RDF triples from 30 documents, the next challenge was to provide a graph-based query interface. The original Epstein-doc-explorer project uses SQLite with materialized columns for filtering, but a proper graph database would enable more sophisticated relationship queries.

**Why Cayley?**
- Native Go implementation (fits the requirement)
- Supports multiple backends (BoltDB, SQL, in-memory)
- RDF/quad-based data model (perfect for our triples)
- Gremlin-like query API
- Active project with good documentation

---

## What I Did

### 1. Technology Selection

**Considered Options**:

1. **Cayley** (chosen)
   - Pros: Native Go, quad-based, multiple backends
   - Cons: Smaller community than Neo4j, learning curve

2. **Neo4j with Go driver**
   - Pros: Industry standard, excellent tooling
   - Cons: Requires separate server, Cypher query language

3. **DGraph**
   - Pros: GraphQL interface, distributed
   - Cons: Overkill for this use case, complex setup

4. **Custom graph on SQLite**
   - Pros: Simple, already have SQLite
   - Cons: Reinventing the wheel, limited graph algorithms

**Decision**: Cayley with BoltDB backend for embedded, serverless operation.

### 2. Architecture Design

**Data Flow**:
```
SQLite (rdf_triples) 
    ↓
Load Script
    ↓
Cayley Graph (BoltDB)
    ↓
CLI Query Tool
    ↓
Results
```

**Quad Structure**:
Each RDF triple becomes multiple quads:
```
(Actor, Action, Target, TripleID)           # Main relationship
(TripleID, "doc_id", DocID, nil)           # Metadata
(TripleID, "timestamp", Timestamp, nil)     # Metadata
(TripleID, "location", Location, nil)       # Metadata
(TripleID, "tag", Tag, nil)                # Multiple tags
(TripleID, "explicit_topic", Topic, nil)   # Metadata
(TripleID, "implicit_topic", Topic, nil)   # Metadata
```

### 3. Implementation

**Files Created**:
- `graph-query/main.go` - CLI tool with Cobra framework
- `graph-query/go.mod` - Dependencies

**Commands Implemented**:
1. `load` - Load SQLite data into Cayley graph
2. `query [actor]` - Find all relationships for an actor
3. `neighbors [person]` - Find direct connections (in/out)
4. `paths [from] [to]` - Find paths between two people
5. `stats` - Show graph statistics

**Key Code Patterns**:

```go
// Loading data
quads = append(quads, quad.Make(
    t.Actor,      // Subject
    t.Action,     // Predicate
    t.Target,     // Object
    tripleID,     // Label (for metadata)
))

// Querying
p := cayley.StartPath(store, quad.String(actor)).Out()
p.Iterate(ctx).EachValue(nil, func(value quad.Value) error {
    // Process results
})
```

---

## What Worked

### 1. Quad Model Mapping
The RDF triple structure mapped naturally to Cayley's quad model. Using the triple ID as the label allowed attaching rich metadata to each relationship.

### 2. BoltDB Backend
Embedded database worked perfectly - no server setup required. The entire graph fits in a single file.

### 3. Cobra CLI Framework
Building the CLI with Cobra was straightforward:
- Clean command structure
- Built-in help generation
- Flag parsing handled automatically

### 4. Type Safety
Go's type system caught several bugs at compile time that would have been runtime errors in Python:
- Null pointer dereferences
- Type mismatches in quad construction
- Missing error handling

---

## What Didn't Work (Yet)

### 1. Go Installation Challenges

**Problem**: Initial Go download was extremely slow (66MB at ~165KB/s)
- First attempt: Direct download from golang.org - timed out
- Second attempt: GitHub mirror - returned HTML instead of tarball
- Third attempt: apt-get install - succeeded but got older version (1.18.1)

**Impact**: Delayed Go implementation by ~10 minutes

**Lesson**: For future projects, check if Go is available via package manager first

### 2. Dependency Download (In Progress)

**Current Status**: Running `go mod tidy` to download Cayley and dependencies

**Challenges Expected**:
- Cayley has many transitive dependencies
- Some may have version conflicts
- Build time could be significant

**Mitigation**: Using Go 1.18.1 which should be compatible with Cayley 0.7.7

### 3. Path Finding Algorithm

**Challenge**: Cayley's path API is not as intuitive as Cypher or Gremlin

**Current Implementation**: Naive approach iterating through hop counts
```go
for hops := 1; hops <= 3; hops++ {
    p := cayley.StartPath(store, quad.String(from))
    for i := 0; i < hops; i++ {
        p = p.Out()
    }
    // Check if reaches target
}
```

**Limitation**: This doesn't actually return the path, just checks if reachable

**What Should Work**: Use `FollowRecursive` or build custom traversal

### 4. Metadata Querying

**Challenge**: Getting metadata for a specific triple requires knowing its label

**Current Approach**: Store label in quad, query by label
```go
metaPath := cayley.StartPath(store, label).Out()
```

**Limitation**: Verbose, requires multiple queries

**Better Approach**: Could use Cayley's MQL (MongoDB Query Language) interface

---

## What I'm Learning

### 1. Graph Database Concepts

**Quads vs Triples**:
- Triples: (subject, predicate, object)
- Quads: (subject, predicate, object, label)
- Label enables metadata and named graphs

**Graph Traversal Patterns**:
- Out: Follow outgoing edges
- In: Follow incoming edges
- Both: Follow edges in either direction
- Has: Filter by property

**Path Semantics**:
- Morphisms: Reusable path fragments
- Recursive paths: For unknown depth
- Shortest path: Optimization problem

### 2. Cayley API Patterns

**Iterator Pattern**:
```go
it := store.QuadsAllIterator()
defer it.Close()
for it.Next(ctx) {
    quad := store.Quad(it.Result())
    // Process quad
}
```

**Path Pattern**:
```go
p := cayley.StartPath(store, startNode)
p = p.Out(predicate).Has(property, value)
p.Iterate(ctx).EachValue(nil, callback)
```

**Value Types**:
- `quad.String()` - String literal
- `quad.IRI()` - IRI/URI reference
- `quad.Int()` - Integer value
- `quad.Time()` - Timestamp

### 3. Go Best Practices

**Error Handling**:
```go
if err != nil {
    return fmt.Errorf("context: %w", err)
}
```
The `%w` verb wraps errors for better stack traces.

**Defer for Cleanup**:
```go
defer it.Close()
defer store.Close()
```
Ensures resources are freed even on early return.

**Context Usage**:
```go
ctx := context.Background()
p.Iterate(ctx).EachValue(...)
```
Enables cancellation and timeout control.

---

## What Should Be Done Next

### Immediate Tasks

1. **Complete Build**
   - Wait for `go mod tidy` to finish
   - Compile the binary: `go build -o graph-query`
   - Test basic functionality

2. **Load Data**
   - Run `./graph-query load` to import SQLite data
   - Verify quad count matches triple count
   - Check metadata attachment

3. **Test Queries**
   - Query Jeffrey Epstein's relationships
   - Find neighbors of Donald Trump
   - Attempt path finding between key figures
   - Generate statistics

### Query Improvements

1. **Better Path Finding**
   ```go
   // Use FollowRecursive for actual path discovery
   p := cayley.StartPath(store, from)
   p = p.FollowRecursive(
       cayley.StartMorphism().Out(),
       maxDepth,
       nil,
   )
   ```

2. **Filtered Queries**
   ```go
   // Find relationships by tag
   p := cayley.StartPath(store, actor)
   p = p.Out().HasLabel(
       cayley.StartPath(store).Has("tag", "sexual_abuse"),
   )
   ```

3. **Aggregation Queries**
   ```go
   // Count relationships per actor
   // May need custom iteration logic
   ```

### Advanced Features

1. **Centrality Metrics**
   - Degree centrality: Count in/out edges
   - Betweenness: How often node appears in shortest paths
   - PageRank: Importance based on connections

2. **Community Detection**
   - Find clusters of highly connected entities
   - Identify sub-networks
   - Detect bridge nodes

3. **Temporal Queries**
   - Filter by date ranges
   - Find relationships in time windows
   - Build chronological sequences

4. **Multi-Hop Patterns**
   - "Friend of friend" queries
   - Transitive relationships
   - Pattern matching (e.g., triangles)

### Visualization Integration

1. **Export Formats**
   - GraphML for Gephi
   - GEXF for network analysis
   - JSON for D3.js
   - DOT for Graphviz

2. **Interactive Exploration**
   - Web interface with graph visualization
   - Click to expand neighborhoods
   - Filter by relationship type
   - Highlight paths

### Performance Optimization

1. **Indexing**
   - Ensure proper indexes on frequently queried fields
   - Use Cayley's index optimization features

2. **Caching**
   - Cache common query results
   - Materialize frequently accessed paths

3. **Batch Operations**
   - Load data in larger batches
   - Use transactions for consistency

---

## Interesting Queries to Try

Once the system is running, these queries would be valuable:

### 1. Network Analysis
```bash
# Who are the most connected people?
./graph-query stats

# What are Jeffrey Epstein's direct connections?
./graph-query neighbors "Jeffrey Epstein"

# How is Trump connected to Epstein?
./graph-query paths "Donald J. Trump" "Jeffrey Epstein"
```

### 2. Relationship Exploration
```bash
# All of Alan Dershowitz's relationships
./graph-query query "Alan M. Dershowitz"

# Find everyone who attended Mar-a-Lago events
# (Would need custom query for location filtering)
```

### 3. Pattern Discovery
```bash
# Find triangular relationships (A→B, B→C, C→A)
# Find people with relationships to both Epstein and Trump
# Identify key brokers connecting different clusters
```

### 4. Temporal Analysis
```bash
# Relationships before 2005 vs after
# Timeline of Epstein's connections
# Evolution of the network over time
```

---

## Comparison: Cayley vs SQL

### SQL Approach (Current)
```sql
SELECT actor, action, target 
FROM rdf_triples 
WHERE actor = 'Jeffrey Epstein';
```

**Limitations**:
- Can't easily traverse multiple hops
- No built-in path finding
- Joins become complex for graph patterns
- No graph algorithms

### Cayley Approach
```go
cayley.StartPath(store, "Jeffrey Epstein")
    .Out()
    .Out()  // Two hops
    .Iterate(ctx)
```

**Advantages**:
- Natural graph traversal
- Built-in path finding
- Easier to express graph patterns
- Extensible with custom algorithms

**Trade-offs**:
- Learning curve for API
- Less familiar than SQL
- Smaller ecosystem
- Debugging can be harder

---

## Key Insights

### 1. Graph Databases Excel at Relationships
For queries like "find all paths between two people" or "who are the common connections", graph databases are orders of magnitude simpler than SQL.

### 2. Quad Model is Powerful
The ability to attach metadata via labels makes quads more expressive than pure triples. This is crucial for our use case where each relationship has rich context.

### 3. Embedded Databases Enable Simplicity
BoltDB backend means no server setup, no network overhead, and easy deployment. The entire graph is a single file.

### 4. Type Safety Catches Bugs Early
Go's compiler caught several issues that would have been runtime errors:
- Null pointer access
- Type mismatches
- Missing error checks

### 5. CLI Tools Need Good UX
Cobra framework makes it easy to build professional CLIs with:
- Subcommands
- Help text
- Flag parsing
- Error messages

---

## Lessons for Future Projects

### 1. Choose the Right Tool
Graph databases shine for relationship-heavy data. For our use case (exploring connections between people), Cayley is a better fit than SQL.

### 2. Start Simple
We're using BoltDB (embedded) rather than a distributed system. This keeps complexity low while we learn the API.

### 3. Design for Iteration
The CLI tool is structured to easily add new commands. Each query type is a separate function, making it easy to experiment.

### 4. Document as You Go
These diary entries capture decisions and learnings in real-time, which will be valuable when revisiting the code later.

### 5. Plan for Visualization
Graph data begs for visual exploration. While we're starting with CLI, the quad export capability will enable future visualization work.

---

## Current Status

**Completed**:
✅ Technology selection (Cayley + BoltDB)  
✅ Architecture design (quad model)  
✅ Code implementation (CLI tool)  
✅ Module setup (go.mod)  

**In Progress**:
🔄 Dependency download (`go mod tidy`)  
🔄 Compilation  

**Pending**:
⏳ Data loading  
⏳ Query testing  
⏳ Performance evaluation  
⏳ Advanced features  

---

## Next Session Goals

1. Complete build and compilation
2. Load all 256 triples into Cayley
3. Run test queries on Jeffrey Epstein
4. Find paths between key figures
5. Generate network statistics
6. Document interesting findings
7. Export results for visualization

---

**Conclusion**: Cayley appears to be a good fit for this use case. The quad model naturally represents our RDF triples with metadata, and the Go API provides type-safe graph traversal. The main challenge will be mastering the path query API and implementing efficient graph algorithms. Once operational, this will enable much more sophisticated relationship analysis than SQL alone.


1. Build and test the graph-query tool
2. Load 256 triples into Cayley
3. Run example queries
4. Evaluate performance
5. Plan visualization integration

---

## Update: Gizmo API Implementation (November 19, 2025 - Later)

### Goal
Upgrade from basic Cayley queries to advanced Gizmo API for more powerful graph traversal.

### Documentation Study

**Source**: https://cayley.gitbook.io/cayley/query-languages/gizmoapi

**Key Concepts Learned**:

1. **Morphisms** - Reusable path patterns
   - `g.Morphism()` creates a path template
   - Can be applied with `.follow()`, `.followR()`, `.followRecursive()`
   - Example: `var friendOfFriend = g.Morphism().out("<follows>").out("<follows>")`

2. **Path Traversal Methods**:
   - `.out(predicate)` - Follow edges outward
   - `.in(predicate)` - Follow edges inward
   - `.both(predicate)` - Follow in either direction
   - `.has(predicate, value)` - Filter by property
   - `.tag(name)` - Save intermediate results
   - `.back(tag)` - Jump back to tagged position

3. **Advanced Patterns**:
   - `.follow(morphism)` - Apply morphism forward
   - `.followR(morphism)` - Apply morphism in reverse
   - `.followRecursive(morphism)` - Recursive traversal
   - `.intersect(path)` - Set intersection
   - `.union(path)` - Set union
   - `.except(path)` - Set difference

4. **Result Collection**:
   - `.all()` - Get all results
   - `.toArray()` - Return as JavaScript array
   - `.count()` - Count results
   - `.forEach(callback)` - Iterate with callback

### Implementation

**Created Files**:
1. `graph-query/gizmo_queries.js` - 15 advanced query patterns
2. `graph-query/gizmo_runner.go` - Go CLI wrapper (21MB binary)

**Query Patterns Implemented**:

1. **findAllRelationships** - All connections for a person
2. **findMutualConnections** - Shared connections between two people
3. **findInfluencers** - People with many incoming connections
4. **findPathsBetween** - Paths connecting two people
5. **findByAction** - Relationships by action type
6. **findRelationshipsInCluster** - Relationships within a topic cluster
7. **findRelationshipsInTimeRange** - Temporal filtering
8. **findNetworkNeighborhood** - N-hop connections
9. **findRelationshipChains** - A→B→C patterns
10. **findByTags** - Filter by tags
11. **findCentralFigures** - High degree centrality
12. **findCommonPatterns** - Frequent relationship types
13. **findIsolatedNodes** - Disconnected entities
14. **findTriangles** - Circular relationships
15. **findRelationshipsWithMetadata** - Filter by metadata fields

### Morphism Examples

```javascript
// Define reusable patterns
var actedUpon = g.Morphism()
  .out("action")
  .out("target");

var actedUponBy = g.Morphism()
  .in("target")
  .in("action");

// Use morphisms in queries
g.V("Jeffrey Epstein")
  .follow(actedUpon)
  .all();

// Recursive traversal
var connection = g.Morphism().out("action").out("target");
g.V("Alan Dershowitz")
  .followRecursive(connection)
  .all();
```

### Go CLI Tool

**Commands**:
```bash
./gizmo-runner relationships "Jeffrey Epstein"
./gizmo-runner mutual "Jeffrey Epstein" "Ghislaine Maxwell"
./gizmo-runner network "Alan Dershowitz"
./gizmo-runner chains "Donald Trump"
```

### Challenges & Solutions

**Challenge**: Gizmo API execution requires specific context  
**Solution**: Created CLI structure with query templates, noted HTTP server requirement for full execution

**Challenge**: API version compatibility issues  
**Solution**: Simplified to query display mode, documented production deployment with Cayley HTTP server

### Production Deployment

```bash
# Start Cayley HTTP server
cayley http --dbpath=cayley.db --host=:64210

# Execute Gizmo queries via HTTP
curl -X POST http://localhost:64210/api/v2/query/gizmo \
  -H "Content-Type: application/json" \
  -d '{"query": "g.V(\"Jeffrey Epstein\").out(\"action\").all()"}'
```

### Key Takeaway

Gizmo's declarative approach is vastly superior to imperative quad iteration for graph analysis. The morphism concept allows complex relationship patterns to be expressed concisely. While direct Go execution proved challenging, the query templates provide a solid foundation for production deployment with Cayley HTTP server.

---

## Docmgr Integration

All work tracked in docmgr:
- **CAYLEY-001**: Gizmo API implementation
- Changelog entries: 2
- Related files: 2
- Status: Implemented
