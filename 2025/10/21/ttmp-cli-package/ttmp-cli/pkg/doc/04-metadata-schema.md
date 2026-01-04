---
Title: Metadata Schema Reference
Slug: metadata-schema
Short: Complete reference for YAML frontmatter metadata fields
Topics:
- reference
- metadata
- schema
Commands: []
Flags: []
IsTopLevel: true
IsTemplate: false
ShowPerDefault: true
SectionType: GeneralTopic
---

Every markdown document in ttmp starts with YAML frontmatter that provides structured metadata. This reference documents all metadata fields, their valid values, and usage guidelines.

## Required Fields

### Status

**Type:** String  
**Valid values:** `draft` | `active` | `review` | `archived`  
**Default:** `draft`

Indicates the lifecycle stage of the document.

- **draft**: Initial creation, work in progress
- **active**: Currently being worked on
- **review**: Ready for review or feedback
- **archived**: Completed or no longer relevant

**Example:**
```yaml
Status: active
```

### Topics

**Type:** Array of strings  
**Valid values:** Must match entries in `doc/vocabulary.yaml`  
**Required:** Yes

Tags that categorize the document for filtering and discovery. Use lowercase slugs from the controlled vocabulary.

**Example:**
```yaml
Topics:
- chat
- backend
- refactoring
```

**Best practices:**
- Use 1-3 topics per document
- Choose the most specific applicable topics
- Validate against vocabulary with `ttmp vocab list topics`

## Optional but Recommended Fields

### Title

**Type:** String  
**Purpose:** Human-readable title for the document

**Example:**
```yaml
Title: Chat API Architecture
```

### Ticket

**Type:** String  
**Format:** `MEN-XXXX`  
**Purpose:** Links document to a specific ticket

**Example:**
```yaml
Ticket: MEN-3475
```

### DocType

**Type:** String  
**Valid values:** Must match entries in `doc/vocabulary.yaml`  
**Common values:** `index` | `design-doc` | `reference` | `working-note` | `tutorial` | `playbook` | `task-list` | `log` | `script`

Categorizes the document's purpose and structure.

**Example:**
```yaml
DocType: design-doc
```

**Guidelines:**
- **index**: One per ticket, canonical entry point
- **working-note**: Free-form exploration and notes
- **design-doc**: Structured architecture and decisions
- **reference**: Reusable prompts and API contracts
- **tutorial**: Step-by-step guides
- **playbook**: Operational procedures
- **task-list**: Checkbox-based task tracking
- **log**: Chronological decision log
- **script**: Documentation for temporary code

### Intent

**Type:** String  
**Valid values:** `short-term` | `long-term` | `throwaway`  
**Default:** `short-term`

Indicates the expected lifespan of the document.

- **short-term**: Lives for the ticket's duration
- **long-term**: Candidate for promotion to permanent docs
- **throwaway**: Ephemeral scratchpad or spike

**Example:**
```yaml
Intent: long-term
```

### Owners

**Type:** Array of strings  
**Purpose:** Lists people responsible for the document

**Example:**
```yaml
Owners:
- alice
- bob
```

### RelatedFiles

**Type:** Array of strings  
**Purpose:** Links to relevant source files in the codebase

**Example:**
```yaml
RelatedFiles:
- go/pkg/chat/handler.go
- go/pkg/chat/websocket.go
- web/src/api/chat.ts
```

**Best practices:**
- Use repository-relative paths
- Update when code structure changes
- Use `ttmp relate` command to manage this field

### Summary

**Type:** String (multiline)  
**Purpose:** Brief description for quick scanning

**Example:**
```yaml
Summary: >
  Tracks the API remapping work to align backend and frontend chat endpoints.
  Includes WebSocket migration and authentication flow updates.
```

### LastUpdated

**Type:** String  
**Format:** `YYYY-MM-DD`  
**Purpose:** Tracks document freshness

**Example:**
```yaml
LastUpdated: 2025-10-21
```

**Automation:**
- Automatically set by `ttmp init` and `ttmp add`
- Update manually or with `ttmp meta update`
- Used by `ttmp doctor` to detect stale documents (>14 days)

## Complete Example

```yaml
---
Title: Chat WebSocket Architecture
Ticket: MEN-3475
Status: active
Topics:
- chat
- backend
- websocket
DocType: design-doc
Intent: long-term
Owners:
- alice
- bob
RelatedFiles:
- go/pkg/chat/handler.go
- go/pkg/chat/websocket.go
- web/src/store/api/chatApi.ts
Summary: >
  Proposes migrating chat from HTTP polling to WebSocket connections
  for real-time message delivery. Includes authentication flow and
  reconnection strategy.
LastUpdated: 2025-10-21
---

# Chat WebSocket Architecture

[Document content follows...]
```

## Validation

The `ttmp doctor` command validates metadata:

```bash
ttmp doctor --ticket MEN-3475
```

Common issues detected:
- Missing required fields (Status, Topics)
- Unknown topics or doc types
- Stale LastUpdated dates
- Invalid field values

## Programmatic Access

Update metadata with `ttmp meta update`:

```bash
ttmp meta update --doc index.md --field Status --value active
ttmp meta update --doc design/01-arch.md --field Intent --value long-term
```

Add topics with `ttmp vocab assign`:

```bash
ttmp vocab assign --ticket MEN-3475 --doc index.md --topics chat,backend
```

Update related files with `ttmp relate`:

```bash
ttmp relate --ticket MEN-3475 --files go/pkg/chat/handler.go,web/src/api/chat.ts
```

## Metadata and LLM Context

Well-structured metadata enables LLMs to:

1. **Filter relevant documents**: Query by topic, status, or doc type
2. **Understand document purpose**: DocType indicates structure and intent
3. **Navigate relationships**: RelatedFiles links documentation to code
4. **Assess freshness**: LastUpdated indicates if context is current
5. **Identify owners**: Owners field suggests who to ask for clarification

## Next Steps

Learn about vocabulary management:

```
ttmp help vocabulary-guide
```

See metadata in action:

```
ttmp help tutorial-basic-workflow
```

Explore all commands:

```
ttmp help commands-reference
```

