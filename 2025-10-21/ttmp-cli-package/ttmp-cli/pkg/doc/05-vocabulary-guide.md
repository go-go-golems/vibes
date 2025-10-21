---
Title: Vocabulary Management Guide
Slug: vocabulary-guide
Short: Guide to managing controlled vocabulary for consistent metadata
Topics:
- vocabulary
- metadata
- reference
Commands:
- vocab
Flags: []
IsTopLevel: true
IsTemplate: false
ShowPerDefault: true
SectionType: GeneralTopic
---

The ttmp system uses controlled vocabulary to ensure consistent metadata across all documents. This guide explains how to manage and extend the vocabulary.

## Why Controlled Vocabulary?

Controlled vocabulary provides several benefits:

1. **Consistency**: Everyone uses the same terms for the same concepts
2. **Discoverability**: Documents can be reliably filtered and searched
3. **Validation**: Tools can detect typos and suggest corrections
4. **LLM Context**: LLMs can understand and use standardized tags

Without controlled vocabulary, you might see variations like:
- `websocket`, `web-socket`, `WebSocket`, `ws`
- `frontend`, `front-end`, `ui`, `client`

With controlled vocabulary, there's one canonical slug for each concept.

## Vocabulary File Structure

The vocabulary is stored in `doc/vocabulary.yaml`:

```yaml
topics:
  - slug: chat
    description: Chat backend and frontend surfaces
  - slug: observability
    description: Logging, metrics, tracing, and alerting work
  - slug: llm-workflow
    description: Tooling and processes for collaborating with LLMs

docTypes:
  - slug: index
    description: Canonical entry point for the ticket
  - slug: design-doc
    description: Structured rationale and architecture notes
  - slug: reference
    description: Prompt cheat-sheets, API contracts, or context summaries

intent:
  - slug: short-term
    description: Expected to live only for the ticket's duration
  - slug: long-term
    description: Candidate for promotion into docs
  - slug: throwaway
    description: Ephemeral scratchpad or spike
```

## Managing Vocabulary

### List Entries

View all entries in a category:

```bash
ttmp vocab list topics
ttmp vocab list docTypes
ttmp vocab list intent
```

Output as JSON for programmatic access:

```bash
ttmp vocab list topics --output json
```

### Add New Entries

Add a new topic:

```bash
ttmp vocab add topics --slug frontend --description "Frontend development and UI work"
```

Add a new doc type:

```bash
ttmp vocab add docTypes --slug spike --description "Exploratory spike or investigation"
```

Add a new intent:

```bash
ttmp vocab add intent --slug permanent --description "Should be moved to permanent documentation"
```

### Validation

The `ttmp doctor` command validates all metadata against the vocabulary:

```bash
ttmp doctor --ticket MEN-3475
```

Issues reported:
- Unknown topics not in vocabulary
- Unknown doc types not in vocabulary
- Typos with suggestions for corrections

## Vocabulary Categories

### Topics

Topics categorize the subject matter of documents. They should be:

- **Broad enough** to group related work
- **Specific enough** to be meaningful
- **Stable over time** (avoid project-specific names)

**Good topics:**
- `chat`, `observability`, `authentication`, `database`, `frontend`, `backend`

**Avoid:**
- `project-x`, `sprint-5`, `q4-work` (too project-specific)
- `code`, `docs`, `work` (too generic)

### DocTypes

DocTypes categorize the structure and purpose of documents:

- **index**: One per ticket, canonical landing page
- **working-note**: Free-form exploration and meeting notes
- **design-doc**: Structured architecture and decision documents
- **reference**: Reusable prompts, API contracts, quick-look tables
- **tutorial**: Step-by-step guides
- **playbook**: Command sequences and operational procedures
- **task-list**: Checkbox-based task tracking
- **log**: Chronological decision log or changelog
- **script**: Documentation for temporary code

### Intent

Intent indicates the expected lifespan:

- **short-term**: Lives for the ticket's duration
- **long-term**: Candidate for promotion to permanent docs
- **throwaway**: Ephemeral scratchpad or spike

## Assigning Vocabulary

### During Initialization

Specify topics when creating a ticket:

```bash
ttmp init MEN-3475 --title "Chat refactoring" --topics chat,backend,refactoring
```

### During Document Creation

Specify topics when adding documents:

```bash
ttmp add design-doc "api-architecture" --topics chat,api,backend
```

Or inherit from ticket (default behavior):

```bash
ttmp add working-note "initial context"
```

### After Creation

Assign topics to existing documents:

```bash
ttmp vocab assign --ticket MEN-3475 --doc index.md --topics chat,backend,websocket
```

Update intent:

```bash
ttmp meta update --doc reference/01-prompt-pack.md --field Intent --value long-term
```

## Best Practices

### Choosing Topics

1. **Use 1-3 topics per document**: More than 3 dilutes meaning
2. **Choose most specific applicable topics**: If both `backend` and `chat` apply, use both
3. **Avoid redundancy**: Don't use both `frontend` and `ui` if they mean the same thing
4. **Think about filtering**: Would you want to find this document by this topic?

### Extending Vocabulary

1. **Check existing entries first**: Use `ttmp vocab list topics` before adding new ones
2. **Discuss with team**: Vocabulary changes affect everyone
3. **Use clear descriptions**: Help others understand when to use each term
4. **Keep slugs lowercase**: Use hyphens for multi-word terms (`llm-workflow`, not `LLM_Workflow`)

### Vocabulary Evolution

As your project grows, vocabulary will evolve:

1. **Add new topics** for emerging areas of work
2. **Deprecate unused topics** (but keep them for historical documents)
3. **Merge similar topics** when distinctions become meaningless
4. **Split overly broad topics** when they cover too much ground

Document vocabulary changes in a changelog or migration guide.

## Validation Workflow

Integrate vocabulary validation into your workflow:

```bash
# Before committing
ttmp doctor

# Check specific ticket
ttmp doctor --ticket MEN-3475

# Get JSON output for CI integration
ttmp doctor --output json
```

## LLM Integration

Controlled vocabulary helps LLMs:

1. **Understand context**: "This document is about chat and backend work"
2. **Filter relevant docs**: "Show me all design-docs about observability"
3. **Suggest related work**: "Other tickets tagged with websocket"
4. **Maintain consistency**: "Use the topic 'llm-workflow' not 'ai-workflow'"

When prompting LLMs, include vocabulary context:

```
Available topics: chat, observability, llm-workflow, frontend, backend, database
Available doc types: index, design-doc, reference, working-note, playbook

Create a design-doc about the chat WebSocket migration.
Use topics: chat, backend, websocket
```

## Next Steps

Learn about the metadata schema:

```
ttmp help metadata-schema
```

See vocabulary in action:

```
ttmp help tutorial-basic-workflow
```

Explore all commands:

```
ttmp help commands-reference
```

