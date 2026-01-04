# Gizmo API Notes

## The Graph Object
- Name: `graph`, Alias: `g`
- This is the main entry point for queries
- Generates query objects that compile to Go iterator tree when executed

## Key Methods

### graph.V(nodeId)
- `V` is shorthand for Vertex
- Starts a query path at given vertex/vertices
- `nodeId` (Optional): String or list of strings representing starting vertices
- No ids means "all vertices"
- Returns: Path object

### graph.Morphism()
- Creates a morphism path object (reusable path pattern)
- Unqueryable on its own, defines one end of the path
- Common pattern: save to variables for reuse
- Example:
```javascript
var shorterPath = graph
  .Morphism()
  .out("foo")
  .out("bar");
```

### Path Object Methods

#### path.out([predicatePath], [tags])
- Follow quads in forward direction (subject to object)
- `predicatePath` options:
  - null/undefined: All predicates pointing out
  - string: Specific predicate name
  - list of strings: Multiple predicates
  - query path object: Set of predicates
- `tags`: Optional tagging for results

#### path.in([predicatePath], [tags])
- Inverse of out() - follow quads backward (object to subject)
- Same argument structure as out()

#### path.has(predicate, object)
- Filter nodes that have specific predicate-object relationship
- Used for filtering based on properties

#### path.all()
- Executes query and returns all results
- Returns string-to-string map (tag to node) for each path

#### path.follow(morphism)
- Apply a morphism (reusable path pattern) to current path
- Starts as if at g.M() and follows through morphism path

#### path.tag(tags)
- Save nodes to given tag for result tracking
- Helps understand how path reached the end

## Query Pattern
Typical Gizmo query pattern:
1. Start somewhere in the graph (g.V())
2. Follow a path (.out(), .in(), .has(), etc.)
3. Execute the query (.all(), .getLimit())

## Example Queries
```javascript
// Find all vertices (limit 5)
g.V().getLimit(5);

// Find vertex with specific property
g.V().has("<name>", "Humphrey Bogart").all();

// Complex path traversal
g.V()
  .has("<name>", "Casablanca")
  .out("</film/film/starring>")
  .out("</film/performance/actor>")
  .out("<name>")
  .all();

// Using morphisms for reusable patterns
var filmToActor = g
  .Morphism()
  .out("</film/film/starring>")
  .out("</film/performance/actor>");

g.V()
  .has("<name>", "Casablanca")
  .follow(filmToActor)
  .out("<name>")
  .all();
```

## Key Concepts
- Quads: (subject, predicate, object, label) - basic graph storage unit
- Morphisms: Reusable path patterns
- Tags: Way to capture intermediate results
- Path traversal: Chain methods to navigate graph relationships

