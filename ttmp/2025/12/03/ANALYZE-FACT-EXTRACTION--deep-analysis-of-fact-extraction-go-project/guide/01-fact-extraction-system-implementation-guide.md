---
Title: Fact Extraction System Implementation Guide
Ticket: ANALYZE-FACT-EXTRACTION
Status: active
Topics:
    - analysis
    - go
    - fact-extraction
    - implementation
DocType: guide
Intent: long-term
Owners: []
RelatedFiles:
    - Path: ttmp/2025/12/03/ANALYZE-FACT-EXTRACTION--deep-analysis-of-fact-extraction-go-project/analysis/01-deep-analysis-of-fact-extraction-go-project.md
      Note: |-
        Overview analysis of fact extraction architecture
        Overall architecture and project evolution
    - Path: ttmp/2025/12/03/ANALYZE-FACT-EXTRACTION--deep-analysis-of-fact-extraction-go-project/analysis/02-provenance-first-extraction-analysis.md
      Note: |-
        Provenance-first extraction patterns
        Provenance tracking patterns
    - Path: ttmp/2025/12/03/ANALYZE-FACT-EXTRACTION--deep-analysis-of-fact-extraction-go-project/analysis/03-multi-stage-entity-resolution-analysis.md
      Note: |-
        Multi-stage entity resolution pipeline
        Entity resolution pipeline design
    - Path: ttmp/2025/12/03/ANALYZE-FACT-EXTRACTION--deep-analysis-of-fact-extraction-go-project/analysis/04-description-enhanced-deduplication-analysis.md
      Note: |-
        Description-enhanced deduplication techniques
        Description-enhanced embedding strategy
    - Path: ttmp/2025/12/03/ANALYZE-FACT-EXTRACTION--deep-analysis-of-fact-extraction-go-project/analysis/05-go-implementation-analysis.md
      Note: Go implementation patterns
    - Path: ttmp/2025/12/03/ANALYZE-FACT-EXTRACTION--deep-analysis-of-fact-extraction-go-project/analysis/06-graph-database-integration-analysis.md
      Note: |-
        Graph database integration with Cayley
        Cayley graph database integration
    - Path: ttmp/2025/12/03/ANALYZE-FACT-EXTRACTION--deep-analysis-of-fact-extraction-go-project/analysis/10-database-schema-design-analysis.md
      Note: |-
        Database schema design patterns
        SQL schema design patterns
ExternalSources: []
Summary: Comprehensive guide for implementing a fact extraction system with entity resolution, provenance tracking, and dual-query capabilities (SQL + graph). Covers architecture, data flow, key algorithms, and implementation strategies.
LastUpdated: 2025-12-03T12:00:00-05:00
---


# Fact Extraction System Implementation Guide

## Executive Summary

This guide provides a conceptual blueprint for implementing a **fact extraction system** that:
1. Extracts structured RDF triples (subject-predicate-object) from unstructured documents using LLMs
2. Tracks provenance through reasoning chains and citations
3. Resolves duplicate entities through a multi-stage pipeline
4. Stores facts in both relational (SQL) and graph databases for flexible querying

The system achieves **95%+ deduplication accuracy** at **$0.01 per 100 entities** while extracting facts at approximately **$0.005 per document**. The dual storage approach enables both traditional SQL queries and graph-based relationship exploration.

---

## Source Documents Consulted

This guide synthesizes findings from 11 analysis documents. Below is each document and the key insights extracted from it:

### 01 - Deep Analysis of Fact Extraction Go Project
**Path:** `analysis/01-deep-analysis-of-fact-extraction-go-project.md`

- **Overall system architecture** — Understood the complete pipeline from document loading through graph storage
- **Project evolution** — Traced how the system grew from basic extraction to enhanced provenance to full descriptions
- **Key metrics** — Extracted performance benchmarks (667 triples, 525 entities, 29% deduplication, $0.17 total cost)
- **Component breakdown** — Identified the six core components (loader, extractor, parser, writer, resolver, graph)
- **Technology stack** — Documented Python/Go implementations, gpt-4.1-mini, Sentence Transformers, FAISS, SQLite, Cayley

### 02 - Provenance-First Extraction Analysis
**Path:** `analysis/02-provenance-first-extraction-analysis.md`

- **Provenance concept** — Learned why asking LLMs to "reason first, then extract" reduces hallucinations
- **Citation extraction** — Understood how exact quotes from documents ground extracted facts
- **Confidence scoring** — Learned how evidence quality maps to numerical confidence (0.0-1.0)
- **Prompt structure** — Documented the multi-part prompt requesting reasoning, citations, and structured output
- **Quality impact** — Confirmed 98% citation accuracy and 95% reasoning relevance in manual review

### 03 - Multi-Stage Entity Resolution Analysis
**Path:** `analysis/03-multi-stage-entity-resolution-analysis.md`

- **Three-stage architecture** — Learned the embedding → profiles → LLM merge pipeline
- **Why multi-stage works** — Understood the cost/accuracy tradeoff (676x cheaper than naive LLM pairwise)
- **FAISS usage** — Documented the IndexFlatIP index with L2 normalization for cosine similarity
- **Union-find algorithm** — Learned how candidate pairs are grouped into connected components for batching
- **Accuracy metrics** — Extracted 95%+ accuracy from manual review of 20 sample merge groups
- **Specific merge examples** — Documented successful merges like "Alan Dershowitz" + "Alan M. Dershowitz"

### 04 - Description-Enhanced Deduplication Analysis
**Path:** `analysis/04-description-enhanced-deduplication-analysis.md`

- **Description extraction** — Learned how entity descriptions are requested in the extraction prompt
- **Embedding strategy** — Understood why `name + description + actions` outperforms name-only (0.85 vs 0.60 similarity)
- **"Longest wins" aggregation** — Documented the strategy for merging descriptions across documents
- **Edge case handling** — Learned how descriptions enable matching "first Latino director to win Oscar" → "Alfonso Cuarón"
- **Impact quantification** — Descriptions improve similarity scores by 20-30% for genuine duplicates

### 05 - Go Implementation Analysis  
**Path:** `analysis/05-go-implementation-analysis.md`

- **Go architecture** — Documented the cmd/pkg structure with separate extractor, storage, and types packages
- **Direct OpenAI usage** — Learned that geppetto streaming limitations led to direct API client usage
- **Type safety patterns** — Understood use of pointers for optional fields in Go structs
- **CLI design** — Documented Cobra usage for command-line interface
- **Implementation status** — Noted which features are complete vs partial (concurrency not yet implemented)

### 06 - Graph Database Integration Analysis
**Path:** `analysis/06-graph-database-integration-analysis.md`

- **Quad model** — Learned subject-predicate-object-label structure extending RDF triples
- **Cayley architecture** — Understood BoltDB embedded backend and Gizmo query language
- **N-Quads format** — Documented the standardized serialization format for graph data
- **Query patterns** — Extracted 15 Gizmo query patterns including morphisms for reusable traversals
- **Metadata separation** — Learned how labels enable attaching metadata without polluting graph structure
- **SQL vs Graph comparison** — Documented when each approach is more appropriate

### 07 - Tag Clustering Analysis
**Path:** `analysis/07-tag-clustering-analysis.md`

- **Clustering approach** — Learned LLM-based semantic grouping of 357 tags into 25 clusters
- **Cluster assignment** — Understood how triples are mapped to clusters via their tags
- **Alternative methods** — Documented K-means on embeddings as alternative approach
- **Use cases** — Learned how clusters enable theme-based filtering and corpus-level analysis

### 08 - Cost Efficiency Analysis
**Path:** `analysis/08-cost-efficiency-analysis.md`

- **Per-document costs** — Extracted cost breakdown by extraction type (basic $0.001, enhanced $0.004, full $0.005)
- **Batch estimates** — Documented scaling costs (100 docs = $0.50, 1000 docs = $5.00 for full extraction)
- **Deduplication economics** — Confirmed $0.0089 total for entity + relation merging (306 entities)
- **Cost-quality tradeoff** — Understood that full extraction at 3.6x cost is worth it for downstream deduplication

### 09 - LLM Prompt Engineering Analysis
**Path:** `analysis/09-llm-prompt-engineering-analysis.md`

- **Prompt components** — Documented 5-part structure (role, rules, instructions, format, guidelines)
- **Domain-specific rules** — Learned how Jeffrey Epstein variant handling prevents entity fragmentation
- **JSON extraction patterns** — Understood markdown code block handling in response parsing
- **Prompt evolution** — Traced changes from basic to enhanced prompts and their motivations
- **Temperature settings** — Learned 0.0 temperature for deterministic merge decisions, 0.3 for extraction

### 10 - Database Schema Design Analysis
**Path:** `analysis/10-database-schema-design-analysis.md`

- **Schema evolution** — Traced basic → enhanced → full schema progression
- **Normalization decisions** — Understood tradeoffs between normalized descriptions table and denormalized triple columns
- **Index strategy** — Documented key indexes (doc_id, actor, target, timestamp)
- **Foreign key design** — Learned CASCADE delete pattern for document → triples relationship
- **JSON storage pattern** — Understood storing arrays as TEXT JSON for flexibility

### 11 - Python vs Go Implementation Comparison
**Path:** `analysis/11-python-vs-go-implementation-comparison.md`

- **Feature parity** — Understood which features exist in Python but not yet in Go
- **Performance comparison** — Learned that Python baseline is ~6 docs/min, Go target is 30 docs/min with concurrency
- **Code structure differences** — Documented Python dataclasses vs Go structs with JSON tags

---

## Part 1: System Architecture

### 1.1 High-Level Architecture

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                           FACT EXTRACTION SYSTEM                             │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│  ┌──────────────┐    ┌──────────────┐    ┌──────────────┐    ┌────────────┐ │
│  │   Document   │───▶│   LLM        │───▶│   Entity     │───▶│   Storage  │ │
│  │   Loader     │    │   Extractor  │    │   Resolution │    │   Layer    │ │
│  └──────────────┘    └──────────────┘    └──────────────┘    └────────────┘ │
│                                                                    │         │
│                                                              ┌─────┴─────┐   │
│                                                              │           │   │
│                                                           ┌──▼──┐   ┌───▼──┐│
│                                                           │SQLite│   │Cayley││
│                                                           │  DB  │   │Graph ││
│                                                           └─────┘   └──────┘│
└─────────────────────────────────────────────────────────────────────────────┘
```

### 1.2 Core Components

| Component | Purpose | Key Responsibilities |
|-----------|---------|---------------------|
| **Document Loader** | Ingest documents | Read files, extract IDs, batch processing |
| **LLM Extractor** | Extract structured facts | Prompt management, response parsing, provenance |
| **Entity Resolver** | Deduplicate entities | Embedding generation, candidate finding, LLM merging |
| **Storage Layer** | Persist and query facts | SQL storage, graph database, dual-write |
| **Query Engine** | Retrieve facts | SQL queries, graph traversal, semantic search |

### 1.3 Data Flow Overview

```
Documents (TXT/PDF)
       │
       ▼
┌──────────────────────────────────────────────────────────────┐
│                    EXTRACTION PHASE                          │
│  ┌─────────────┐    ┌─────────────┐    ┌─────────────┐      │
│  │ Read Doc    │───▶│ LLM Extract │───▶│ Parse JSON  │      │
│  │ + Metadata  │    │ + Provenance│    │ + Validate  │      │
│  └─────────────┘    └─────────────┘    └─────────────┘      │
└──────────────────────────────────────────────────────────────┘
       │
       ▼ (RDF Triples + Entity Descriptions + Provenance)
┌──────────────────────────────────────────────────────────────┐
│                    RESOLUTION PHASE                          │
│  ┌─────────────┐    ┌─────────────┐    ┌─────────────┐      │
│  │ Build       │───▶│ Find        │───▶│ LLM Batch   │      │
│  │ Profiles    │    │ Candidates  │    │ Merge       │      │
│  └─────────────┘    └─────────────┘    └─────────────┘      │
└──────────────────────────────────────────────────────────────┘
       │
       ▼ (Deduplicated Entities + Merge Groups)
┌──────────────────────────────────────────────────────────────┐
│                    STORAGE PHASE                             │
│  ┌─────────────┐                    ┌─────────────┐         │
│  │ Write to    │                    │ Write to    │         │
│  │ SQLite      │                    │ Cayley      │         │
│  │ (Relational)│                    │ (Graph)     │         │
│  └─────────────┘                    └─────────────┘         │
└──────────────────────────────────────────────────────────────┘
```

---

## Part 2: Data Structures

### 2.1 Core Domain Model

The system operates on a small set of core data structures that flow through the pipeline:

#### Document
```
Document:
  id: string            # Unique document identifier
  file_path: string     # Path to source file
  content: string       # Full text content
  metadata:
    category: enum      # court_filing | email | report | ...
    date_range: (date, date)?  # Optional temporal bounds
    tags: list<string>  # Document-level tags
```

#### RDF Triple (Fact)
```
RDFTriple:
  # Core triple
  actor: string         # Subject - who/what is acting
  action: string        # Predicate - what they did
  target: string        # Object - who/what they acted upon
  
  # Metadata
  timestamp: datetime?  # When (if known)
  location: string?     # Where (if known)
  tags: list<string>    # Fact-level tags
  topics:
    explicit: string?   # Main topic directly evidenced
    implicit: string?   # Inferred underlying topic
  
  # Provenance (for enhanced extraction)
  reasoning: string?    # Why this fact was extracted
  citations: list<Citation>?  # Supporting quotes
  confidence: float?    # 0.0-1.0 confidence score
  
  # Entity descriptions (for deduplication)
  actor_description: string?
  actor_type: enum      # person | organization | location | ...
  target_description: string?
  target_type: enum
  relation_description: string?
  relation_type: enum   # action | state | attribute | ...
```

#### Citation
```
Citation:
  text: string          # Exact quote from document
  relevance: string     # Why this quote supports the fact
```

#### Entity Profile (for resolution)
```
EntityProfile:
  name: string                    # Entity name
  description: string             # Aggregated description
  entity_type: enum               # person | organization | ...
  mention_count: int              # How many times seen
  
  # Contextual information
  actions_as_actor: list<string>  # What they do (verbs)
  actions_as_target: list<string> # What happens to them
  co_occurring_entities: set<string>  # Who they appear with
  
  # Evidence (for LLM merging)
  sample_reasoning: list<string>  # Top reasoning chains
  sample_citations: list<string>  # Top citations
  doc_ids: set<string>            # Documents they appear in
  avg_confidence: float           # Average confidence of their facts
```

#### Merge Decision
```
MergeGroup:
  canonical_name: string          # The unified name to use
  aliases: list<string>           # All names that map to this entity
  confidence: float               # Merge confidence
  reason: string                  # Why these are the same entity
```

### 2.2 Storage Models

#### SQL Schema (Relational)

The SQL schema provides structured storage for querying and analysis:

```
┌───────────────────────────────────────────────────────────────┐
│                         documents                              │
├───────────────────────────────────────────────────────────────┤
│ doc_id (PK)       │ TEXT UNIQUE NOT NULL                      │
│ file_path         │ TEXT NOT NULL                              │
│ summary           │ TEXT                                       │
│ category          │ TEXT                                       │
│ content_tags      │ TEXT (JSON array)                         │
│ date_range_*      │ TEXT (ISO dates)                          │
│ full_text         │ TEXT                                       │
│ cost_usd          │ REAL                                       │
│ processed_at      │ TIMESTAMP                                  │
└───────────────────────────────────────────────────────────────┘
           │
           │ 1:N
           ▼
┌───────────────────────────────────────────────────────────────┐
│                         rdf_triples                            │
├───────────────────────────────────────────────────────────────┤
│ id (PK)           │ INTEGER AUTOINCREMENT                     │
│ doc_id (FK)       │ TEXT → documents(doc_id)                  │
│ actor             │ TEXT NOT NULL                              │
│ action            │ TEXT NOT NULL                              │
│ target            │ TEXT NOT NULL                              │
│ timestamp         │ TEXT                                       │
│ location          │ TEXT                                       │
│ tags              │ TEXT (JSON array)                         │
│ reasoning         │ TEXT                                       │
│ citations         │ TEXT (JSON array)                         │
│ confidence        │ REAL                                       │
│ actor_description │ TEXT                                       │
│ target_description│ TEXT                                       │
│ relation_description│ TEXT                                     │
│ sequence_order    │ INTEGER                                    │
└───────────────────────────────────────────────────────────────┘

┌───────────────────────────────────────────────────────────────┐
│                    entity_descriptions                         │
├───────────────────────────────────────────────────────────────┤
│ entity_name (PK)  │ TEXT UNIQUE NOT NULL                      │
│ description       │ TEXT                                       │
│ entity_type       │ TEXT                                       │
│ mention_count     │ INTEGER                                    │
│ first_seen_doc    │ TEXT                                       │
│ updated_at        │ TIMESTAMP                                  │
└───────────────────────────────────────────────────────────────┘

┌───────────────────────────────────────────────────────────────┐
│                   relation_descriptions                        │
├───────────────────────────────────────────────────────────────┤
│ relation_name (PK)│ TEXT UNIQUE NOT NULL                      │
│ description       │ TEXT                                       │
│ relation_type     │ TEXT                                       │
│ usage_count       │ INTEGER                                    │
└───────────────────────────────────────────────────────────────┘
```

**Key indexes:**
- `idx_rdf_triples_doc_id` - Query triples by document
- `idx_rdf_triples_actor` - Query triples by actor
- `idx_rdf_triples_target` - Query triples by target
- `idx_rdf_triples_timestamp` - Temporal queries

#### Graph Schema (Quad-based)

The graph database uses a quad model (subject-predicate-object-label):

```
Main Relationship Quad:
┌──────────────────────────────────────────────────────────────┐
│ Subject    │ Predicate  │ Object     │ Label                 │
├────────────┼────────────┼────────────┼───────────────────────┤
│ "Person A" │ "met with" │ "Person B" │ "triple:123"          │
└──────────────────────────────────────────────────────────────┘

Metadata Quads (referencing the label):
┌──────────────────────────────────────────────────────────────┐
│ Subject      │ Predicate  │ Object           │ Label         │
├──────────────┼────────────┼──────────────────┼───────────────┤
│ triple:123   │ doc_id     │ "document_001"   │ null          │
│ triple:123   │ timestamp  │ "2020-01-15"     │ null          │
│ triple:123   │ location   │ "New York"       │ null          │
│ triple:123   │ tag        │ "business"       │ null          │
│ triple:123   │ tag        │ "meeting"        │ null          │
│ triple:123   │ confidence │ "0.95"           │ null          │
└──────────────────────────────────────────────────────────────┘
```

This design allows:
- Direct graph traversal on the main relationship quads
- Metadata filtering via label-based joins
- Separation of structure from metadata

---

## Part 3: Extraction Pipeline

### 3.1 Document Loading

The document loader handles ingestion and prepares documents for extraction:

```
PROCEDURE load_documents(directory, limit):
  documents = []
  FOR each file IN directory:
    doc = Document(
      id = extract_id_from_filename(file),
      file_path = file.path,
      content = read_file(file)
    )
    documents.append(doc)
    IF len(documents) >= limit:
      BREAK
  RETURN documents
```

**Key considerations:**
- Extract document ID from filename (e.g., `doc_001.txt` → `doc_001`)
- Support batch limits for incremental processing
- Handle encoding issues gracefully
- Store full text for later re-analysis

### 3.2 LLM Extraction

The LLM extraction phase uses carefully crafted prompts to extract structured facts with provenance:

#### Prompt Structure

```
SYSTEM_PROMPT:
┌────────────────────────────────────────────────────────────────┐
│ 1. ROLE DEFINITION                                             │
│    "You are an expert fact extraction system..."               │
├────────────────────────────────────────────────────────────────┤
│ 2. DOMAIN-SPECIFIC RULES                                       │
│    "Identify entity X under these aliases: [a, b, c]..."      │
├────────────────────────────────────────────────────────────────┤
│ 3. EXTRACTION INSTRUCTIONS                                     │
│    "For EACH FACT, provide:"                                   │
│    - Core triple (actor, action, target)                       │
│    - Reasoning (why you extracted this)                        │
│    - Citations (exact quotes)                                  │
│    - Confidence (0.0-1.0)                                      │
│    - Entity descriptions                                        │
├────────────────────────────────────────────────────────────────┤
│ 4. OUTPUT FORMAT                                               │
│    JSON schema with examples                                   │
├────────────────────────────────────────────────────────────────┤
│ 5. GUIDELINES                                                  │
│    - Use consistent entity naming                              │
│    - Actions should be verb phrases                            │
│    - Focus on person-to-person relationships                   │
└────────────────────────────────────────────────────────────────┘
```

#### Extraction Flow

```
PROCEDURE extract_facts(document, llm_client):
  prompt = build_prompt(document)
  
  response = llm_client.chat(
    model = "gpt-4.1-mini",  # Or equivalent cost-effective model
    messages = [system_prompt, user_prompt + document.content],
    response_format = JSON
  )
  
  # Parse and validate response
  parsed = parse_llm_response(response)
  validated = validate_triples(parsed.triples)
  
  # Track costs
  cost = calculate_cost(response.tokens_in, response.tokens_out)
  
  RETURN ExtractionResult(
    document_id = document.id,
    triples = validated,
    entity_descriptions = parsed.entity_descriptions,
    relation_descriptions = parsed.relation_descriptions,
    cost = cost
  )
```

### 3.3 Response Parsing

LLM responses need careful parsing to handle edge cases:

```
PROCEDURE parse_llm_response(response):
  content = response.text
  
  # Handle markdown code blocks
  IF "```json" IN content:
    content = extract_between("```json", "```", content)
  ELSE IF "```" IN content:
    content = extract_between("```", "```", content)
  
  # Parse JSON
  TRY:
    data = json.parse(content)
  CATCH:
    RETURN empty_result()
  
  # Validate structure
  IF "triples" NOT IN data:
    RETURN empty_result()
  
  # Parse individual components
  triples = parse_triples(data["triples"])
  entities = parse_entity_descriptions(data.get("entity_descriptions", {}))
  relations = parse_relation_descriptions(data.get("relation_descriptions", {}))
  
  RETURN ParsedResult(triples, entities, relations)
```

### 3.4 Provenance Tracking

The provenance-first approach asks the LLM to reason before extracting:

**Why Provenance Matters:**
1. **Reduces hallucinations** - Grounding in citations constrains extraction
2. **Enables verification** - Can trace back to source quotes
3. **Improves quality** - Reasoning forces careful analysis
4. **Supports confidence** - Evidence quality informs confidence scores

**Extraction with Provenance:**
```
FOR each fact TO extract:
  1. Identify relationship in document
  2. Generate reasoning explaining why this is a fact
  3. Extract exact quotes as citations
  4. Assign confidence based on evidence quality
  5. Describe entities to support deduplication
  6. Output structured triple with all metadata
```

---

## Part 4: Entity Resolution

### 4.1 Why Entity Resolution Matters

Without entity resolution, the same real-world entity appears under multiple names:
- "Jeffrey Epstein" vs "Epstein" vs "jee"
- "Virginia Roberts" vs "Virginia Roberts Giuffre" vs "Jane Doe #3"
- "Bill Clinton" vs "Former President Bill Clinton"

This fragments the knowledge graph and prevents accurate relationship analysis.

### 4.2 Three-Stage Pipeline

The entity resolution pipeline uses three stages that balance speed, accuracy, and cost:

```
┌─────────────────────────────────────────────────────────────────┐
│                    ENTITY RESOLUTION PIPELINE                   │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  STAGE 1: Embedding-Based Candidate Generation                  │
│  ────────────────────────────────────────────                   │
│  • Generate embeddings: name + description + actions            │
│  • Build FAISS index for fast similarity search                 │
│  • Find top-K candidates per entity (threshold: 0.7)            │
│  • Cost: $0 (local computation)                                 │
│  • Speed: ~2 seconds for 300 entities                           │
│                                                                  │
│                           ▼                                      │
│                                                                  │
│  STAGE 2: Context Profile Construction                          │
│  ─────────────────────────────────────                          │
│  • Build rich profiles from database                            │
│  • Aggregate: actions, co-occurrences, evidence                 │
│  • Provides context for LLM decision-making                     │
│  • Cost: $0 (database queries)                                  │
│  • Speed: ~1 second for 300 entities                            │
│                                                                  │
│                           ▼                                      │
│                                                                  │
│  STAGE 3: LLM Batch Merging                                     │
│  ─────────────────────────────                                  │
│  • Group candidates into connected components                   │
│  • Send batches of related entities to LLM                      │
│  • LLM decides which entities are the same                      │
│  • Cost: ~$0.01 per 100 entities                                │
│  • Speed: ~20 seconds for 300 entities                          │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### 4.3 Stage 1: Embedding Generation

```
PROCEDURE find_candidates(profiles, threshold=0.7):
  # Build rich text for each entity
  texts = {}
  FOR name, profile IN profiles:
    text = name
    IF profile.description:
      text += ". " + profile.description
    IF profile.actions_as_actor:
      text += " Actions: " + join(profile.actions_as_actor[:3])
    texts[name] = text
  
  # Generate embeddings
  embeddings = embedding_model.encode(list(texts.values()))
  
  # Normalize for cosine similarity
  normalize_L2(embeddings)
  
  # Build FAISS index
  index = FAISSIndexFlatIP(embeddings.dimension)
  index.add(embeddings)
  
  # Find candidates for each entity
  candidates = {}
  FOR i, name IN enumerate(texts.keys()):
    query = embeddings[i]
    similarities, indices = index.search(query, k=10)
    
    entity_candidates = []
    FOR sim, idx IN zip(similarities, indices):
      IF idx != i AND sim >= threshold:
        entity_candidates.append((names[idx], sim))
    
    IF entity_candidates:
      candidates[name] = entity_candidates
  
  RETURN candidates
```

### 4.4 Stage 2: Profile Construction

```
PROCEDURE build_entity_profiles(database):
  profiles = {}
  
  # Get all unique entities (actors and targets)
  entities = query_unique_entities(database)
  
  FOR entity IN entities:
    profile = EntityProfile(name=entity)
    
    # Get description from entity_descriptions table
    profile.description = query_entity_description(entity)
    
    # Collect actions where entity is actor
    profile.actions_as_actor = query_actions_as_actor(entity, limit=10)
    
    # Collect actions where entity is target
    profile.actions_as_target = query_actions_as_target(entity, limit=10)
    
    # Find co-occurring entities
    profile.co_occurring_entities = query_co_occurring(entity, limit=20)
    
    # Collect sample evidence
    top_triples = query_top_confidence_triples(entity, limit=5)
    profile.sample_reasoning = [t.reasoning FOR t IN top_triples]
    profile.sample_citations = [t.citations FOR t IN top_triples]
    
    profiles[entity] = profile
  
  RETURN profiles
```

### 4.5 Stage 3: LLM Batch Merging

```
PROCEDURE batch_merge(candidates, profiles, llm_client):
  # Build connected components from candidates
  components = union_find(candidates)
  
  merge_results = []
  
  FOR component IN components:
    IF len(component) < 2:
      CONTINUE
    
    # Build prompt with full context
    prompt = build_merge_prompt(component, profiles)
    
    response = llm_client.chat(
      model = "gpt-4.1-mini",
      messages = [system_prompt, prompt],
      temperature = 0.0,  # Deterministic
      response_format = JSON
    )
    
    # Parse merge decisions
    merge_groups = parse_merge_response(response)
    merge_results.extend(merge_groups)
  
  RETURN merge_results
```

**Merge Prompt Structure:**
```
You are an expert entity resolution system.

Given these entities with their profiles, identify groups that refer 
to the same real-world entity:

Entity 1: "Alan Dershowitz"
  - Description: Attorney who represented Jeffrey Epstein
  - Actions: defended, represented, filed motion for
  - Co-occurs with: Jeffrey Epstein, Virginia Roberts
  - Sample reasoning: "Dershowitz was named as part of Epstein's defense team"

Entity 2: "Alan M. Dershowitz"  
  - Description: Harvard Law professor and defense attorney
  - Actions: denied allegations, issued statement, filed lawsuit
  - Co-occurs with: Jeffrey Epstein, Jane Doe #3
  - Sample reasoning: "The text references Dershowitz's role in the legal case"

[... more entities ...]

For each merge group, provide:
- canonical_name: The best name to use
- aliases: All names that map to this entity
- confidence: How confident (0.0-1.0)
- reason: Why these are the same entity
```

### 4.6 Description-Enhanced Deduplication

The key insight is that **entity descriptions dramatically improve matching accuracy**:

| Approach | Similarity Score |
|----------|-----------------|
| Name only | ~0.60 |
| Name + description | ~0.80 |
| Name + description + actions | ~0.85 |

**Why descriptions help:**
- Capture semantic meaning beyond lexical similarity
- Handle variants like "first Latino director to win an Oscar" → "Alfonso Cuarón"
- Distinguish same-name entities by their context
- Enable LLM to make informed merge decisions

---

## Part 5: Storage Layer

### 5.1 Dual Storage Strategy

The system writes to both SQL and graph databases:

```
PROCEDURE store_facts(extraction_result, merge_results):
  # Write to SQLite
  write_to_sqlite(extraction_result)
  
  # Convert to quads and write to Cayley
  quads = convert_to_quads(extraction_result, merge_results)
  write_to_cayley(quads)
```

### 5.2 SQL Storage

```
PROCEDURE write_to_sqlite(result):
  # Insert/update document
  upsert_document(result.document_id, result.metadata)
  
  # Insert triples
  FOR triple IN result.triples:
    insert_triple(
      doc_id = result.document_id,
      actor = triple.actor,
      action = triple.action,
      target = triple.target,
      # ... all fields
    )
  
  # Update entity descriptions (longest wins)
  FOR name, desc IN result.entity_descriptions:
    upsert_entity_description(
      name,
      desc,
      strategy = "KEEP_LONGER"  # length(new) > length(existing)
    )
  
  # Update relation descriptions
  FOR rel, desc IN result.relation_descriptions:
    upsert_relation_description(rel, desc)
```

### 5.3 Graph Storage

```
PROCEDURE convert_to_quads(result, merge_results):
  quads = []
  
  # Apply entity resolution
  canonical_map = build_canonical_map(merge_results)
  
  FOR triple IN result.triples:
    # Resolve to canonical names
    actor = canonical_map.get(triple.actor, triple.actor)
    target = canonical_map.get(triple.target, triple.target)
    
    # Main relationship quad
    label = "triple:" + triple.id
    quads.append(Quad(actor, triple.action, target, label))
    
    # Metadata quads
    quads.append(Quad(label, "doc_id", result.document_id))
    quads.append(Quad(label, "confidence", triple.confidence))
    
    IF triple.timestamp:
      quads.append(Quad(label, "timestamp", triple.timestamp))
    IF triple.location:
      quads.append(Quad(label, "location", triple.location))
    
    FOR tag IN triple.tags:
      quads.append(Quad(label, "tag", tag))
  
  RETURN quads
```

---

## Part 6: Query Capabilities

### 6.1 SQL Query Patterns

**Find all facts about an entity:**
```sql
SELECT * FROM rdf_triples 
WHERE actor = 'Jeffrey Epstein' OR target = 'Jeffrey Epstein'
ORDER BY timestamp;
```

**Find entities by description:**
```sql
SELECT entity_name, description 
FROM entity_descriptions 
WHERE description LIKE '%attorney%'
ORDER BY mention_count DESC;
```

**Find high-confidence facts:**
```sql
SELECT actor, action, target, confidence, reasoning
FROM rdf_triples
WHERE confidence > 0.9
ORDER BY confidence DESC;
```

**Co-occurrence analysis:**
```sql
SELECT t1.actor, t2.target, COUNT(*) as co_occurrences
FROM rdf_triples t1
JOIN rdf_triples t2 ON t1.doc_id = t2.doc_id AND t1.id != t2.id
WHERE t1.actor = 'Person A'
GROUP BY t1.actor, t2.target
ORDER BY co_occurrences DESC;
```

### 6.2 Graph Query Patterns

**Find all relationships for an entity:**
```javascript
g.V("Jeffrey Epstein").Out().All()
```

**Find neighbors (1-hop):**
```javascript
// Outgoing relationships
g.V("Person A").Out().All()

// Incoming relationships
g.V("Person A").In().All()
```

**Find mutual connections:**
```javascript
var aConnections = g.V("Person A").Out()
var bConnections = g.V("Person B").Out()
aConnections.Intersect(bConnections).All()
```

**Multi-hop traversal:**
```javascript
// 2-hop paths
g.V("Person A").Out().Out().All()

// Path with relationship types
g.V("Person A")
  .Out("met with")
  .Out("testified about")
  .All()
```

**Network neighborhood (recursive):**
```javascript
var connection = g.Morphism().Out()
g.V("Person A").FollowRecursive(connection).All()
```

### 6.3 When to Use Which

| Use Case | SQL | Graph |
|----------|-----|-------|
| Simple lookups by ID | ✓ | |
| Full-text search | ✓ | |
| Aggregations (counts, averages) | ✓ | |
| Temporal analysis | ✓ | |
| Multi-hop relationships | | ✓ |
| Path finding | | ✓ |
| Network analysis | | ✓ |
| Mutual connections | | ✓ |
| Recursive traversal | | ✓ |

---

## Part 7: Implementation Recommendations

### 7.1 Technology Choices

| Component | Recommended Technology | Alternatives |
|-----------|----------------------|--------------|
| Language | Go or Python | Both work well |
| LLM | GPT-4.1-mini | Claude, Gemini |
| Embeddings | all-MiniLM-L6-v2 | OpenAI embeddings |
| Vector Search | FAISS | Annoy, HNSWlib |
| SQL Database | SQLite | PostgreSQL for scale |
| Graph Database | Cayley | Neo4j, ArangoDB |

### 7.2 Cost Optimization

**Extraction costs by approach:**
| Approach | Cost/Doc | Features |
|----------|----------|----------|
| Basic | $0.001 | Triples only |
| Enhanced | $0.004 | + reasoning + citations |
| Full | $0.005 | + descriptions |

**Entity resolution costs:**
| Approach | Cost/100 entities |
|----------|-------------------|
| Naive LLM (pairwise) | $4.67 |
| Multi-stage (recommended) | $0.01 |
| Savings | **676x** |

### 7.3 Accuracy Targets

| Component | Target | Achievable |
|-----------|--------|------------|
| Extraction precision | 90%+ | 95%+ |
| Entity resolution accuracy | 90%+ | 95%+ |
| Citation accuracy | 95%+ | 98%+ |

### 7.4 Scalability Considerations

**For small scale (100-1000 docs):**
- SQLite + embedded Cayley (BoltDB)
- Sequential processing
- Single machine

**For medium scale (1000-100K docs):**
- PostgreSQL + Cayley with PostgreSQL backend
- Parallel extraction (worker pool)
- Batch processing

**For large scale (100K+ docs):**
- Distributed SQL (CockroachDB, TiDB)
- Neo4j or ArangoDB cluster
- Streaming extraction pipeline
- Incremental entity resolution

---

## Part 8: Implementation Checklist

### Phase 1: Core Extraction
- [ ] Implement document loader
- [ ] Design extraction prompt
- [ ] Implement LLM client wrapper
- [ ] Implement JSON response parser
- [ ] Create SQL schema
- [ ] Implement SQL storage writer
- [ ] Add cost tracking

### Phase 2: Enhanced Extraction
- [ ] Add provenance fields to prompt
- [ ] Add citation extraction
- [ ] Add entity description extraction
- [ ] Add confidence scoring
- [ ] Update schema for provenance

### Phase 3: Entity Resolution
- [ ] Implement embedding generation
- [ ] Implement FAISS candidate finding
- [ ] Implement profile construction
- [ ] Implement LLM batch merging
- [ ] Implement merge application

### Phase 4: Graph Integration
- [ ] Design quad structure
- [ ] Implement SQLite to quad conversion
- [ ] Implement Cayley writer
- [ ] Implement basic graph queries
- [ ] Add relationship traversal queries

### Phase 5: Query Layer
- [ ] Implement SQL query helpers
- [ ] Implement graph query patterns
- [ ] Add semantic search (optional)
- [ ] Build query API

---

## Appendix A: Sample Data Flow

**Input Document:**
```
Email from: jeeitunes@gmail.com
To: ghislaine@maxwell.com
Date: 2008-03-15

Hi G,

Met with Alan yesterday about the legal situation. He thinks we should 
file the motion next week. Don't worry about the Palm Beach property - 
everything is handled.

- J
```

**Extracted Facts (after LLM processing):**
```json
{
  "triples": [
    {
      "actor": "Jeffrey Epstein",
      "action": "met with",
      "target": "Alan Dershowitz",
      "timestamp": "2008-03-14",
      "location": null,
      "reasoning": "Email references a meeting 'yesterday' about legal matters",
      "citations": [{"text": "Met with Alan yesterday about the legal situation"}],
      "confidence": 0.95,
      "actor_description": "Financier and subject of criminal investigation",
      "target_description": "Attorney providing legal counsel"
    },
    {
      "actor": "Jeffrey Epstein",
      "action": "sent email to",
      "target": "Ghislaine Maxwell",
      "timestamp": "2008-03-15",
      "reasoning": "Direct email communication",
      "citations": [{"text": "Email from: jeeitunes@gmail.com To: ghislaine@maxwell.com"}],
      "confidence": 1.0
    }
  ],
  "entity_descriptions": {
    "Jeffrey Epstein": {
      "description": "Financier and subject of criminal investigations",
      "entity_type": "person"
    },
    "Alan Dershowitz": {
      "description": "Attorney providing legal counsel in the case",
      "entity_type": "person"
    }
  }
}
```

**Stored in SQLite (rdf_triples):**
```
| id | doc_id | actor           | action       | target          | confidence |
|----|--------|-----------------|--------------|-----------------|------------|
| 1  | doc_42 | Jeffrey Epstein | met with     | Alan Dershowitz | 0.95       |
| 2  | doc_42 | Jeffrey Epstein | sent email to| Ghislaine Maxwell| 1.00      |
```

**Stored in Cayley (quads):**
```
<Jeffrey_Epstein> <met_with> <Alan_Dershowitz> <triple:1> .
<triple:1> <doc_id> "doc_42" .
<triple:1> <timestamp> "2008-03-14" .
<triple:1> <confidence> "0.95" .

<Jeffrey_Epstein> <sent_email_to> <Ghislaine_Maxwell> <triple:2> .
<triple:2> <doc_id> "doc_42" .
<triple:2> <timestamp> "2008-03-15" .
<triple:2> <confidence> "1.00" .
```

---

## Appendix B: Key Metrics

**Performance achieved in reference implementation:**
- 68 documents processed
- 667 RDF triples extracted
- 525 unique entities
- 216 entities after deduplication (29% reduction)
- 95%+ deduplication accuracy
- Total cost: $0.17
- Average: $0.0025/document

**Processing times (for 300 entities):**
- Embedding generation: ~1 second
- FAISS search: <1 second
- Profile construction: ~1 second
- LLM batch merging: ~20 seconds
- Total: ~23 seconds

---

## Conclusion

This guide provides a comprehensive blueprint for implementing a fact extraction system. The key innovations are:

1. **Provenance-first extraction** - Reasoning and citations reduce hallucinations
2. **Description-enhanced embeddings** - Rich text improves semantic matching
3. **Multi-stage entity resolution** - Combines speed of embeddings with accuracy of LLMs
4. **Dual storage strategy** - SQL for structured queries, graphs for relationship exploration

The system achieves high accuracy (95%+) at low cost ($0.01/100 entities for deduplication, $0.005/document for extraction), making it practical for real-world document analysis.

For implementation questions or to see the reference implementation, consult the analysis documents in this ticket.
