---
Title: Database Schema Design Analysis
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
    - Path: 2025/11/25/fact-extraction-go/extract_facts_full_enhanced.py
      Note: Database schema creation in Python with enhanced tables
    - Path: 2025/11/25/fact-extraction-go/fact_extraction.db
      Note: Basic database schema
    - Path: 2025/11/25/fact-extraction-go/fact_extraction_full.db
      Note: Full enhanced database schema
    - Path: 2025/11/25/fact-extraction-go/go-extractor/pkg/storage/sqlite.go
      Note: Go storage implementation with schema
    - Path: 2025/11/25/fact-extraction-go/main.go
      Note: Database schema creation in Go - initDatabase function
    - Path: 2025/11/25/fact-extraction-go/sample_queries.sql
      Note: Sample SQL queries demonstrating schema usage
ExternalSources: []
Summary: 'Analysis of database schema design: table structures, indexes, data relationships, normalization, and query patterns'
LastUpdated: 2025-12-03T09:42:22.055210663-05:00
---



# Database Schema Design Analysis

## Research Objective

Analyze the database schema design, including table structures, indexes, data relationships, normalization, and query patterns.

## Research Instructions

### Phase 1: Schema Documentation

**Files to analyze:**
- `vibes/2025/11/25/fact-extraction-go/main.go` (initDatabase)
- `vibes/2025/11/25/fact-extraction-go/extract_facts_full_enhanced.py` (database creation)
- `vibes/2025/11/25/fact-extraction-go/go-extractor/pkg/storage/sqlite.go`
- Actual databases: `fact_extraction.db`, `fact_extraction_full.db`

**Tasks:**
1. **Document all tables**:
   - `documents` table
   - `rdf_triples` table
   - `rdf_triples_full` table
   - `entity_descriptions` table
   - `relation_descriptions` table

2. **Document schema evolution**:
   - Basic schema
   - Enhanced schema
   - Full enhanced schema
   - What changed and why

### Phase 2: Schema Analysis

**Tasks:**
1. **Normalization analysis**:
   - Is the schema normalized?
   - What redundancy exists?
   - What trade-offs were made?

2. **Data types**:
   - Are data types appropriate?
   - Are there type mismatches?
   - Are there size constraints?

3. **Constraints**:
   - What constraints exist?
   - Are foreign keys used?
   - What validation exists?

### Phase 3: Index Analysis

**Tasks:**
1. **Document indexes**:
   - What indexes exist?
   - What columns are indexed?
   - What indexes are missing?

2. **Query performance**:
   - How do indexes affect queries?
   - What queries are slow?
   - What indexes would help?

### Phase 4: Query Pattern Analysis

**Files to analyze:**
- `sample_queries.sql`
- Query code in extraction scripts
- Graph query code

**Tasks:**
1. **Document query patterns**:
   - Common queries
   - Complex queries
   - Join patterns
   - Aggregation patterns

2. **Analyze query performance**:
   - Which queries are fast?
   - Which queries are slow?
   - What optimizations exist?

### Phase 5: Data Relationships

**Tasks:**
1. **Document relationships**:
   - Documents → Triples
   - Triples → Entities
   - Entities → Descriptions
   - Relations → Descriptions

2. **Analyze referential integrity**:
   - Are relationships enforced?
   - What happens on delete?
   - Are there orphaned records?

### Phase 6: Schema Improvements

**Deliverables:**
1. **Complete Schema Documentation**
2. **Index Analysis Report**
3. **Query Pattern Analysis**
4. **Normalization Analysis**
5. **Improvement Recommendations**

## Key Questions to Answer

1. **Is the schema well-designed?**
2. **What are the strengths?**
3. **What are the weaknesses?**
4. **What improvements are needed?**

## Related Files

- `vibes/2025/11/25/fact-extraction-go/main.go`
- `vibes/2025/11/25/fact-extraction-go/extract_facts_full_enhanced.py`
- `vibes/2025/11/25/fact-extraction-go/go-extractor/pkg/storage/sqlite.go`
- `vibes/2025/11/25/fact-extraction-go/sample_queries.sql`

## Expected Timeline: 12-15 hours

---

## Analysis: Database Schema Design for Fact Extraction

### Introduction: Designing for Fact Storage and Query

The database schema design for the fact extraction pipeline reflects a pragmatic approach to storing structured facts extracted from unstructured documents. Unlike traditional relational databases designed for transactional applications, this schema must support fact storage, relationship queries, temporal analysis, and integration with downstream processes like entity resolution and graph databases. The schema evolves through multiple versions, each adding capabilities while maintaining backward compatibility and query performance.

The design choices reveal trade-offs between normalization and denormalization, between comprehensive metadata storage and query simplicity, and between flexibility and performance. Understanding these trade-offs helps explain why certain design decisions were made and how the schema supports the fact extraction pipeline's goals of storing facts reliably, enabling efficient queries, and facilitating downstream analysis.

### Schema Evolution: From Basic to Enhanced

The database schema evolved through three main versions, each addressing limitations discovered in practice and adding capabilities needed for advanced features like entity resolution and provenance tracking.

**Basic Schema** (`main.go` lines 128-183):

The foundational schema establishes two core tables: documents and rdf_triples:

```128:183:vibes/2025/11/25/fact-extraction-go/main.go
	// Create documents table
	_, err = db.Exec(`
		CREATE TABLE IF NOT EXISTS documents (
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			doc_id TEXT UNIQUE NOT NULL,
			file_path TEXT NOT NULL,
			one_sentence_summary TEXT NOT NULL,
			paragraph_summary TEXT NOT NULL,
			date_range_earliest TEXT,
			date_range_latest TEXT,
			category TEXT NOT NULL,
			content_tags TEXT NOT NULL,
			full_text TEXT,
			analysis_timestamp TEXT NOT NULL,
			input_tokens INTEGER,
			output_tokens INTEGER,
			cost_usd REAL,
			error TEXT,
			created_at DATETIME DEFAULT CURRENT_TIMESTAMP
		);
	`)

	// Create RDF triples table
	_, err = db.Exec(`
		CREATE TABLE IF NOT EXISTS rdf_triples (
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			doc_id TEXT NOT NULL,
			timestamp TEXT,
			actor TEXT NOT NULL,
			action TEXT NOT NULL,
			target TEXT NOT NULL,
			location TEXT,
			actor_likely_type TEXT,
			triple_tags TEXT,
			explicit_topic TEXT,
			implicit_topic TEXT,
			sequence_order INTEGER NOT NULL,
			created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
			FOREIGN KEY (doc_id) REFERENCES documents(doc_id) ON DELETE CASCADE
		);
	`)

	// Create indexes
	_, err = db.Exec(`
		CREATE INDEX IF NOT EXISTS idx_documents_doc_id ON documents(doc_id);
		CREATE INDEX IF NOT EXISTS idx_documents_category ON documents(category);
		CREATE INDEX IF NOT EXISTS idx_rdf_triples_doc_id ON rdf_triples(doc_id);
		CREATE INDEX IF NOT EXISTS idx_rdf_triples_actor ON rdf_triples(actor);
		CREATE INDEX IF NOT EXISTS idx_rdf_triples_timestamp ON rdf_triples(timestamp);
	`)
```

This basic schema establishes the core structure: documents store metadata about processed documents, and rdf_triples store the extracted facts. The foreign key relationship ensures referential integrity, and the `ON DELETE CASCADE` clause ensures that deleting a document automatically removes its associated triples. Indexes on `doc_id`, `actor`, and `timestamp` support common query patterns.

**Enhanced Schema** (`extract_facts_enhanced.py` lines 68-104):

The enhanced version adds cluster assignments and processing tracking:

```68:104:vibes/2025/11/25/fact-extraction-go/extract_facts_enhanced.py
        # Documents table
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS documents (
                doc_id TEXT PRIMARY KEY,
                one_sentence_summary TEXT,
                paragraph_summary TEXT,
                category TEXT,
                content_tags TEXT,
                date_range_earliest TEXT,
                date_range_latest TEXT,
                full_text TEXT,
                input_tokens INTEGER,
                output_tokens INTEGER,
                cost_usd REAL,
                processed_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            )
        """)
        
        # RDF triples table with cluster columns
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS rdf_triples (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                doc_id TEXT,
                timestamp TEXT,
                actor TEXT,
                action TEXT,
                target TEXT,
                location TEXT,
                actor_likely_type TEXT,
                triple_tags TEXT,
                explicit_topic TEXT,
                implicit_topic TEXT,
                sequence_order INTEGER,
                cluster_ids TEXT,
                cluster_themes TEXT,
                FOREIGN KEY (doc_id) REFERENCES documents(doc_id) ON DELETE CASCADE
            )
        """)
```

The enhanced schema adds `cluster_ids` and `cluster_themes` columns to the `rdf_triples` table, storing JSON arrays of cluster assignments. This denormalization allows filtering triples by cluster without joins, improving query performance for cluster-based analysis.

**Full Enhanced Schema** (`extract_facts_full_enhanced.py` lines 300-349):

The full enhanced schema adds provenance and description tables:

```300:349:vibes/2025/11/25/fact-extraction-go/extract_facts_full_enhanced.py
        # Create tables
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS rdf_triples_full (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                doc_id TEXT NOT NULL,
                timestamp TEXT,
                actor TEXT NOT NULL,
                action TEXT NOT NULL,
                target TEXT NOT NULL,
                location TEXT,
                triple_tags TEXT,
                explicit_topic TEXT,
                implicit_topic TEXT,
                reasoning TEXT,
                citations TEXT,
                confidence REAL,
                actor_description TEXT,
                actor_type TEXT,
                target_description TEXT,
                target_type TEXT,
                relation_description TEXT,
                relation_type TEXT,
                created_at DATETIME DEFAULT CURRENT_TIMESTAMP
            )
        """)
        
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS entity_descriptions (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                entity_name TEXT NOT NULL UNIQUE,
                description TEXT,
                entity_type TEXT,
                first_seen_doc TEXT,
                mention_count INTEGER DEFAULT 1,
                created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
                updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
            )
        """)
        
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS relation_descriptions (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                relation_name TEXT NOT NULL UNIQUE,
                description TEXT,
                relation_type TEXT,
                first_seen_doc TEXT,
                usage_count INTEGER DEFAULT 1,
                created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
                updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
            )
        """)
```

The full enhanced schema introduces three new tables: `rdf_triples_full` (with provenance fields), `entity_descriptions` (normalized entity metadata), and `relation_descriptions` (normalized relation metadata). This design separates core facts from descriptions, enabling entity and relation deduplication while maintaining the ability to query facts with their provenance.

### Documents Table: Document Metadata Storage

The `documents` table stores metadata about processed documents, serving as both a processing log and a document catalog. This table enables tracking which documents have been processed, understanding document characteristics, and analyzing extraction costs.

**Basic Documents Schema** (`main.go` lines 129-147):

```129:147:vibes/2025/11/25/fact-extraction-go/main.go
		CREATE TABLE IF NOT EXISTS documents (
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			doc_id TEXT UNIQUE NOT NULL,
			file_path TEXT NOT NULL,
			one_sentence_summary TEXT NOT NULL,
			paragraph_summary TEXT NOT NULL,
			date_range_earliest TEXT,
			date_range_latest TEXT,
			category TEXT NOT NULL,
			content_tags TEXT NOT NULL,
			full_text TEXT,
			analysis_timestamp TEXT NOT NULL,
			input_tokens INTEGER,
			output_tokens INTEGER,
			cost_usd REAL,
			error TEXT,
			created_at DATETIME DEFAULT CURRENT_TIMESTAMP
		);
```

**Design Decisions**:

1. **Primary Key Choice**: Uses `id INTEGER PRIMARY KEY AUTOINCREMENT` for the primary key but also has `doc_id TEXT UNIQUE NOT NULL`. This dual-key design allows efficient integer-based joins while maintaining human-readable document identifiers.

2. **Full Text Storage**: The `full_text` field stores the complete document content, enabling re-analysis without re-reading files. This denormalization trades storage space for convenience and enables queries that search document content.

3. **Cost Tracking**: Fields like `input_tokens`, `output_tokens`, and `cost_usd` enable cost analysis and budgeting. This metadata helps understand extraction economics and optimize prompt design.

4. **Error Handling**: The `error` field stores error messages, enabling analysis of failure patterns and debugging extraction issues.

5. **Temporal Fields**: Both `analysis_timestamp` and `created_at` are stored, allowing tracking of when documents were analyzed versus when records were created (useful for reprocessing scenarios).

**Simplified Documents Schema** (`go-extractor/pkg/storage/sqlite.go` lines 41-47):

The Go implementation uses a simplified documents table:

```41:47:vibes/2025/11/25/fact-extraction-go/go-extractor/pkg/storage/sqlite.go
	CREATE TABLE IF NOT EXISTS documents (
		doc_id TEXT PRIMARY KEY,
		processed_at TIMESTAMP,
		tokens_in INTEGER,
		tokens_out INTEGER,
		cost_usd REAL
	);
```

This simplified version omits document summaries, categories, and full text, focusing only on processing metadata. This design reflects the Go implementation's goal of being a focused extraction tool rather than a comprehensive document analysis system.

### RDF Triples Table: Core Fact Storage

The `rdf_triples` table stores the extracted facts, representing relationships between entities. This table is the core of the fact extraction database, and its design directly impacts query performance and data quality.

**Basic Triples Schema** (`main.go` lines 154-170):

```154:170:vibes/2025/11/25/fact-extraction-go/main.go
		CREATE TABLE IF NOT EXISTS rdf_triples (
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			doc_id TEXT NOT NULL,
			timestamp TEXT,
			actor TEXT NOT NULL,
			action TEXT NOT NULL,
			target TEXT NOT NULL,
			location TEXT,
			actor_likely_type TEXT,
			triple_tags TEXT,
			explicit_topic TEXT,
			implicit_topic TEXT,
			sequence_order INTEGER NOT NULL,
			created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
			FOREIGN KEY (doc_id) REFERENCES documents(doc_id) ON DELETE CASCADE
		);
```

**Design Decisions**:

1. **Core Triple Fields**: The `actor`, `action`, and `target` fields represent the core RDF triple structure. These fields are `NOT NULL`, ensuring every triple has complete relationship information.

2. **Optional Metadata**: Fields like `timestamp`, `location`, and `actor_likely_type` are nullable, allowing triples to omit metadata when not available. This design balances completeness with flexibility.

3. **JSON Storage**: Fields like `triple_tags` store JSON arrays as TEXT, enabling flexible tag storage without requiring a separate tags table. This denormalization simplifies queries but requires JSON parsing in application code.

4. **Sequence Order**: The `sequence_order` field preserves the order of triples within a document, enabling chronological analysis and maintaining document structure.

5. **Foreign Key Constraint**: The foreign key to `documents(doc_id)` with `ON DELETE CASCADE` ensures referential integrity and automatic cleanup when documents are deleted.

**Enhanced Triples Schema** (`extract_facts_full_enhanced.py` lines 301-323):

The full enhanced version adds provenance and description fields:

```301:323:vibes/2025/11/25/fact-extraction-go/extract_facts_full_enhanced.py
            CREATE TABLE IF NOT EXISTS rdf_triples_full (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                doc_id TEXT NOT NULL,
                timestamp TEXT,
                actor TEXT NOT NULL,
                action TEXT NOT NULL,
                target TEXT NOT NULL,
                location TEXT,
                triple_tags TEXT,
                explicit_topic TEXT,
                implicit_topic TEXT,
                reasoning TEXT,
                citations TEXT,
                confidence REAL,
                actor_description TEXT,
                actor_type TEXT,
                target_description TEXT,
                target_type TEXT,
                relation_description TEXT,
                relation_type TEXT,
                created_at DATETIME DEFAULT CURRENT_TIMESTAMP
            )
```

**Provenance Fields**:

The `reasoning`, `citations`, and `confidence` fields enable provenance tracking—understanding why facts were extracted, what evidence supports them, and how confident the extraction is. These fields support downstream processes like fact verification and quality assessment.

**Description Fields**:

The description fields (`actor_description`, `target_description`, `relation_description`) store entity and relation descriptions directly in the triple table. This denormalization enables efficient queries that include descriptions without joins, but it creates redundancy when the same entity appears in multiple triples.

**Trade-offs**:

Storing descriptions in the triple table trades normalization for query performance. A normalized design would store descriptions in separate tables and join when needed, but the denormalized design enables faster queries and simpler application code at the cost of storage space and potential inconsistency.

### Entity and Relation Descriptions: Normalized Metadata

The full enhanced schema introduces separate tables for entity and relation descriptions, providing a normalized approach to storing metadata that appears across multiple triples.

**Entity Descriptions Table** (`extract_facts_full_enhanced.py` lines 326-336):

```326:336:vibes/2025/11/25/fact-extraction-go/extract_facts_full_enhanced.py
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS entity_descriptions (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                entity_name TEXT NOT NULL UNIQUE,
                description TEXT,
                entity_type TEXT,
                first_seen_doc TEXT,
                mention_count INTEGER DEFAULT 1,
                created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
                updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
            )
        """)
```

**Design Decisions**:

1. **Unique Constraint**: The `entity_name UNIQUE` constraint ensures each entity has a single canonical description, preventing duplicates and enabling efficient lookups.

2. **Update Strategy** (`extract_facts_full_enhanced.py` lines 376-388):

```376:388:vibes/2025/11/25/fact-extraction-go/extract_facts_full_enhanced.py
        # Insert/update entity descriptions
        for name, desc in result.entity_descriptions.items():
            cursor.execute("""
                INSERT INTO entity_descriptions (entity_name, description, entity_type, first_seen_doc)
                VALUES (?, ?, ?, ?)
                ON CONFLICT(entity_name) DO UPDATE SET
                    description = CASE 
                        WHEN length(excluded.description) > length(description) 
                        THEN excluded.description 
                        ELSE description 
                    END,
                    mention_count = mention_count + 1,
                    updated_at = CURRENT_TIMESTAMP
            """, (name, desc.description, desc.entity_type, doc_id))
```

The update strategy uses `ON CONFLICT` to handle entities that appear in multiple documents. When conflicts occur, it keeps the longer description (assuming more detail is better) and increments the mention count. This approach aggregates entity information across documents while maintaining a single canonical description per entity.

3. **Mention Tracking**: The `mention_count` field tracks how many times an entity appears, enabling analysis of entity importance and frequency.

**Relation Descriptions Table** (`extract_facts_full_enhanced.py` lines 339-349):

```339:349:vibes/2025/11/25/fact-extraction-go/extract_facts_full_enhanced.py
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS relation_descriptions (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                relation_name TEXT NOT NULL UNIQUE,
                description TEXT,
                relation_type TEXT,
                first_seen_doc TEXT,
                usage_count INTEGER DEFAULT 1,
                created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
                updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
            )
        """)
```

The relation descriptions table follows the same pattern as entity descriptions, storing normalized metadata about relations (actions) that appear across multiple triples. This normalization enables relation deduplication and semantic analysis.

**Hybrid Approach**:

The schema uses a hybrid approach: descriptions are stored both in the `rdf_triples_full` table (for query convenience) and in normalized tables (for deduplication and consistency). This design provides the benefits of both approaches: fast queries with descriptions included, and normalized metadata for entity/relation resolution.

### Index Design: Supporting Query Patterns

Indexes are crucial for query performance, and the fact extraction schema includes indexes that support common query patterns identified during development.

**Basic Indexes** (`main.go` lines 177-182):

```177:182:vibes/2025/11/25/fact-extraction-go/main.go
	// Create indexes
	_, err = db.Exec(`
		CREATE INDEX IF NOT EXISTS idx_documents_doc_id ON documents(doc_id);
		CREATE INDEX IF NOT EXISTS idx_documents_category ON documents(category);
		CREATE INDEX IF NOT EXISTS idx_rdf_triples_doc_id ON rdf_triples(doc_id);
		CREATE INDEX IF NOT EXISTS idx_rdf_triples_actor ON rdf_triples(actor);
		CREATE INDEX IF NOT EXISTS idx_rdf_triples_timestamp ON rdf_triples(timestamp);
	`)
```

**Index Rationale**:

1. **doc_id Indexes**: Indexes on `doc_id` in both tables support joins between documents and triples, enabling queries like "show all triples for a specific document."

2. **Category Index**: The category index on documents enables filtering documents by type (e.g., "show all emails" or "show all court filings"), supporting document-level analysis.

3. **Actor Index**: The actor index on triples supports the most common query pattern: "find all relationships for a specific person." This is essential for entity-centric analysis.

4. **Timestamp Index**: The timestamp index enables temporal queries like "show relationships in chronological order" or "find relationships in a date range."

**Go Implementation Indexes** (`go-extractor/pkg/storage/sqlite.go` lines 71-73):

```71:73:vibes/2025/11/25/fact-extraction-go/go-extractor/pkg/storage/sqlite.go
	CREATE INDEX IF NOT EXISTS idx_actor ON rdf_triples(actor);
	CREATE INDEX IF NOT EXISTS idx_action ON rdf_triples(action);
	CREATE INDEX IF NOT EXISTS idx_target ON rdf_triples(target);
```

The Go implementation adds indexes on `action` and `target`, supporting queries that filter by relationship type or find relationships targeting specific entities. These indexes reflect query patterns discovered during implementation.

**Missing Indexes**:

Several potentially useful indexes are missing:
- Composite indexes (e.g., `(actor, action)` for common filter combinations)
- Indexes on JSON fields (e.g., `triple_tags` for tag-based queries)
- Full-text indexes on description fields for semantic search

These omissions reflect a pragmatic approach: indexes are added when query performance becomes an issue, rather than preemptively.

### Query Patterns: How the Schema Supports Analysis

The database schema supports several common query patterns that enable fact analysis and relationship discovery.

**Pattern 1: Entity-Centric Queries** (`sample_queries.sql` lines 3-13):

```3:13:vibes/2025/11/25/fact-extraction-go/sample_queries.sql
-- 1. All Jeffrey Epstein relationships
SELECT 
    actor, 
    action, 
    target, 
    location,
    explicit_topic,
    implicit_topic
FROM rdf_triples 
WHERE actor LIKE '%Epstein%' OR target LIKE '%Epstein%'
ORDER BY sequence_order;
```

This query pattern finds all relationships involving a specific entity, whether as actor or target. The `LIKE` pattern matching handles name variations, and the `sequence_order` preserves document order. The actor and target indexes support this query efficiently.

**Pattern 2: Temporal Queries** (`sample_queries.sql` lines 60-69):

```60:69:vibes/2025/11/25/fact-extraction-go/sample_queries.sql
-- 7. Relationships with timestamps
SELECT 
    timestamp,
    actor,
    action,
    target,
    location
FROM rdf_triples
WHERE timestamp IS NOT NULL
ORDER BY timestamp;
```

Temporal queries enable chronological analysis of relationships. The timestamp index supports efficient ordering, and the `IS NOT NULL` filter excludes triples without temporal information.

**Pattern 3: Tag-Based Filtering** (`sample_queries.sql` lines 25-32):

```25:32:vibes/2025/11/25/fact-extraction-go/sample_queries.sql
-- 3. Sexual abuse related triples
SELECT 
    actor, 
    action, 
    target,
    explicit_topic
FROM rdf_triples 
WHERE triple_tags LIKE '%sexual_abuse%' OR triple_tags LIKE '%sexual abuse%';
```

Tag-based queries use `LIKE` pattern matching on JSON-stored tags. This approach works but is inefficient—SQLite must scan all rows and parse JSON. A better approach would use full-text search or a separate tags table, but the current design prioritizes simplicity.

**Pattern 4: Aggregation Queries** (`sample_queries.sql` lines 42-49):

```42:49:vibes/2025/11/25/fact-extraction-go/sample_queries.sql
-- 5. Most connected people (as actors)
SELECT 
    actor,
    COUNT(*) as relationship_count
FROM rdf_triples
GROUP BY actor
ORDER BY relationship_count DESC
LIMIT 20;
```

Aggregation queries identify patterns like most-connected entities or most-common relationship types. The actor index supports efficient grouping, and these queries enable network analysis and relationship discovery.

**Pattern 5: Location-Based Queries** (`sample_queries.sql` lines 71-78):

```71:78:vibes/2025/11/25/fact-extraction-go/sample_queries.sql
-- 8. Relationships at Mar-a-Lago
SELECT 
    actor,
    action,
    target,
    timestamp
FROM rdf_triples
WHERE location LIKE '%Mar-a-Lago%';
```

Location-based queries enable spatial analysis of relationships. Without a location index, these queries require full table scans, but for the current dataset size, this is acceptable.

### Normalization Analysis: Trade-offs and Decisions

The database schema makes deliberate trade-offs between normalization and denormalization, balancing query performance, storage efficiency, and data consistency.

**Denormalization Decisions**:

1. **Tags in Triples**: Tags are stored as JSON in the `triple_tags` field rather than in a separate tags table. This denormalization enables fast tag-based queries without joins but makes tag management and consistency more difficult.

2. **Descriptions in Triples**: The `rdf_triples_full` table stores entity and relation descriptions directly, creating redundancy when the same entity appears in multiple triples. This design enables queries that include descriptions without joins but requires careful update logic to maintain consistency.

3. **Full Text Storage**: The `documents` table stores `full_text`, duplicating file content in the database. This denormalization enables re-analysis without file access but increases storage requirements.

**Normalization Decisions**:

1. **Separate Descriptions Tables**: The `entity_descriptions` and `relation_descriptions` tables normalize entity and relation metadata, ensuring single canonical descriptions per entity/relation. This normalization supports deduplication and consistency.

2. **Foreign Key Relationships**: The foreign key between `rdf_triples` and `documents` enforces referential integrity, ensuring triples always reference valid documents.

3. **Processing Log Separation**: The `processing_log` table (in some schemas) separates processing status from document metadata, enabling tracking of processing state independently.

**Why These Trade-offs**:

The denormalization decisions prioritize query performance and application simplicity. For fact extraction, queries that include tags or descriptions are common, and avoiding joins improves performance. The normalization decisions prioritize data consistency and deduplication, which are essential for entity resolution and quality analysis.

### Data Types: Choosing Appropriate Storage

The schema uses SQLite's flexible type system, choosing types that balance storage efficiency, query performance, and application simplicity.

**TEXT for Most Fields**:

Most fields use `TEXT` type, including entity names, actions, locations, and timestamps. This choice reflects SQLite's type affinity system, where `TEXT` can store strings of any length and SQLite handles type conversion automatically.

**TEXT for Timestamps**:

Timestamps are stored as `TEXT` rather than `DATETIME` or `TIMESTAMP` types. This design allows flexible timestamp formats (ISO 8601, date-only, etc.) without requiring strict format validation. The trade-off is that temporal queries require string comparison rather than native date arithmetic.

**REAL for Costs**:

Cost fields use `REAL` (floating-point) type, enabling precise decimal calculations for cost tracking. This is appropriate for financial calculations where precision matters.

**INTEGER for Counts**:

Count fields (tokens, mention counts) use `INTEGER` type, providing efficient storage and arithmetic operations for aggregation queries.

**JSON Storage as TEXT**:

JSON fields (tags, citations) are stored as `TEXT`, requiring application-level JSON parsing. SQLite doesn't have native JSON support in older versions, so this approach is necessary. Newer SQLite versions support JSON functions, but the current implementation uses application-level parsing for compatibility.

### Foreign Keys and Referential Integrity

The schema uses foreign keys to enforce referential integrity between tables, ensuring data consistency and enabling automatic cleanup.

**Foreign Key Definition** (`main.go` line 169):

```169:169:vibes/2025/11/25/fact-extraction-go/main.go
			FOREIGN KEY (doc_id) REFERENCES documents(doc_id) ON DELETE CASCADE
```

**CASCADE Delete Behavior**:

The `ON DELETE CASCADE` clause ensures that deleting a document automatically deletes all associated triples. This behavior maintains referential integrity and prevents orphaned triples, but it requires careful consideration when deleting documents—accidental deletions could remove large amounts of data.

**Foreign Key Enforcement**:

SQLite requires foreign keys to be explicitly enabled (via `PRAGMA foreign_keys = ON`). The fact extraction code doesn't explicitly enable this pragma, which means foreign key constraints may not be enforced unless SQLite is configured to enforce them by default. This is a potential issue that could lead to referential integrity violations.

**Missing Foreign Keys**:

The schema doesn't define foreign keys from `rdf_triples_full` to `entity_descriptions` or `relation_descriptions`, even though these relationships exist conceptually. This omission reflects the denormalized design where descriptions are stored in both places, making foreign keys less meaningful.

### Processing Log: Tracking Extraction State

Some schema versions include a `processing_log` table to track document processing state, enabling checkpointing and resumable processing.

**Processing Log Schema** (`go-extractor/pkg/storage/sqlite.go` lines 64-69):

```64:69:vibes/2025/11/25/fact-extraction-go/go-extractor/pkg/storage/sqlite.go
	CREATE TABLE IF NOT EXISTS processing_log (
		doc_id TEXT PRIMARY KEY,
		status TEXT,
		timestamp TIMESTAMP,
		error_message TEXT
	);
```

**Design Purpose**:

The processing log enables tracking which documents have been processed successfully, which failed, and when processing occurred. This information supports resumable processing (skipping already-processed documents) and error analysis (identifying patterns in failures).

**Status Field**:

The `status` field stores processing state (e.g., "success", "error"), enabling queries that filter by processing status. This design supports operational monitoring and debugging.

**Error Tracking**:

The `error_message` field stores error details, enabling analysis of failure patterns and debugging extraction issues. This metadata helps identify systematic problems (e.g., JSON parsing failures, API errors).

### Query Performance: Index Impact

The indexes defined in the schema directly impact query performance for common patterns.

**Index Effectiveness** (`sample_queries.sql`):

The sample queries demonstrate how indexes support efficient queries:

- **Entity queries** (lines 3-13): The actor index enables fast lookups of relationships for specific entities
- **Temporal queries** (lines 60-69): The timestamp index enables efficient chronological ordering
- **Aggregation queries** (lines 42-49): Indexes support efficient grouping and counting

**Index Limitations**:

Some query patterns don't benefit from existing indexes:

- **Tag queries** (lines 25-32): Tag-based queries use `LIKE` on JSON fields, requiring full table scans
- **Location queries** (lines 71-78): Location queries lack indexes, requiring full table scans
- **Topic queries** (lines 80-89): Topic-based queries on `explicit_topic` or `implicit_topic` lack indexes

These limitations reflect a pragmatic approach: indexes are added when performance becomes an issue, not preemptively. For the current dataset size, full table scans are acceptable, but larger datasets would benefit from additional indexes.

### Schema Comparison: Python vs Go Implementations

The Python and Go implementations use different schema designs, reflecting different priorities and use cases.

**Python Schema** (`main.go`, `extract_facts.py`):

- Comprehensive document metadata (summaries, categories, tags)
- Full text storage
- Rich triple metadata (topics, tags, locations)
- Multiple indexes for various query patterns

**Go Schema** (`go-extractor/pkg/storage/sqlite.go`):

- Minimal document metadata (processing info only)
- No full text storage
- Core triple fields only
- Focused indexes (actor, action, target)

**Design Philosophy Differences**:

The Python schema prioritizes comprehensive document analysis, storing rich metadata that supports document-level queries and analysis. The Go schema prioritizes fact extraction efficiency, storing only what's necessary for fact storage and basic queries. These differences reflect the different goals: Python for comprehensive analysis, Go for efficient extraction.

### Schema Strengths and Weaknesses

**Strengths**:

1. **Clear Structure**: The two-table design (documents + triples) is intuitive and easy to understand
2. **Foreign Key Integrity**: Foreign keys ensure referential integrity and automatic cleanup
3. **Flexible Metadata**: JSON fields enable flexible tag and citation storage without schema changes
4. **Query Support**: Indexes support common query patterns efficiently
5. **Evolution Support**: Schema evolution through multiple versions maintains backward compatibility

**Weaknesses**:

1. **JSON Query Limitations**: JSON fields require application-level parsing and don't benefit from indexes
2. **Denormalization Trade-offs**: Storing descriptions in multiple places creates consistency challenges
3. **Missing Indexes**: Some query patterns (tags, locations, topics) lack indexes
4. **Type Flexibility**: Using TEXT for timestamps prevents native date arithmetic
5. **Foreign Key Enforcement**: Foreign keys may not be enforced if not explicitly enabled

### Recommendations for Improvement

**Recommendation 1: Add Composite Indexes**

Composite indexes on common filter combinations (e.g., `(actor, action)`, `(doc_id, timestamp)`) would improve query performance for filtered queries.

**Recommendation 2: Consider Full-Text Search**

For tag and topic queries, full-text search indexes would enable efficient semantic queries without full table scans.

**Recommendation 3: Normalize Tags**

A separate tags table with a many-to-many relationship to triples would enable efficient tag-based queries while maintaining flexibility.

**Recommendation 4: Use Native Timestamps**

Storing timestamps as SQLite's native datetime type would enable efficient temporal queries and date arithmetic.

**Recommendation 5: Explicit Foreign Key Enforcement**

Explicitly enabling foreign key enforcement (`PRAGMA foreign_keys = ON`) would ensure referential integrity is maintained.

**Recommendation 6: Add Missing Indexes**

Indexes on `location`, `explicit_topic`, and `implicit_topic` would improve query performance for location and topic-based analysis.

### Current State and Future Directions

The database schema successfully supports fact extraction, storage, and basic querying. The evolution from basic to enhanced schemas demonstrates iterative improvement based on real-world needs.

**What Works Well**:

- Clear two-table structure supports intuitive queries
- Foreign keys maintain referential integrity
- Indexes support common query patterns
- Flexible JSON fields enable metadata without schema changes
- Schema evolution maintains backward compatibility

**Areas for Enhancement**:

- Better JSON query support (full-text search, JSON functions)
- More comprehensive indexing strategy
- Normalized tag storage for efficient queries
- Native timestamp types for temporal analysis
- Explicit foreign key enforcement

**Design Philosophy**:

The schema design prioritizes pragmatism over theoretical perfection. Denormalization is used where it improves query performance, normalization is used where it improves consistency, and indexes are added based on observed query patterns rather than theoretical optimization. This approach produces a schema that works well in practice while remaining maintainable and understandable.
