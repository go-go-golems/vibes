---
Title: Commands Reference
Slug: commands-reference
Short: Complete reference for all ttmp CLI commands
Topics:
- reference
- commands
Commands:
- init
- add
- relate
- meta
- vocab
- list
- doctor
- search
Flags: []
IsTopLevel: true
IsTemplate: false
ShowPerDefault: true
SectionType: GeneralTopic
---

This reference documents all ttmp CLI commands, their parameters, and usage patterns.

## ttmp init

Initialize a new ticket directory with standard structure.

**Usage:**
```bash
ttmp init [ticket] --title <title> --topics <topics> [options]
```

**Parameters:**
- `ticket` (positional, optional): Ticket identifier (e.g., MEN-3475). If omitted, attempts to derive from git branch name
- `--title` (required): Human-readable title for the ticket
- `--topics` (required): Comma-separated list of topics
- `--owners` (optional): Comma-separated list of owners
- `--intent` (optional): short-term | long-term | throwaway (default: short-term)
- `--root` (optional): Root directory for ttmp (default: ./ttmp)

**Examples:**
```bash
ttmp init MEN-3475 --title "Chat API cleanup" --topics chat,backend
ttmp init MEN-3476 --title "Performance spike" --topics observability --intent throwaway
ttmp init MEN-3477 --title "Frontend redesign" --topics frontend --owners alice,bob
```

**Output:**
Creates directory structure with index.md, tasks.md, changelog.md, and subdirectories.

## ttmp add

Create a new document from a template.

**Usage:**
```bash
ttmp add <doc-type> <name> [options]
```

**Parameters:**
- `doc-type` (positional, required): working-note | design-doc | reference | playbook | script
- `name` (positional, required): Name/slug for the document
- `--ticket` (optional): Ticket identifier (default: infer from current directory)
- `--topics` (optional): Comma-separated topics (default: inherit from ticket)

**Examples:**
```bash
ttmp add working-note "initial context"
ttmp add design-doc "api-architecture" --ticket MEN-3475
ttmp add reference "websocket-triage" --topics chat,debugging
ttmp add playbook "smoke-test" --ticket MEN-3475
```

**Output:**
Creates numbered document in appropriate subdirectory with metadata template.

## ttmp relate

Update RelatedFiles metadata for documents.

**Usage:**
```bash
ttmp relate --ticket <ticket> --files <files> [options]
```

**Parameters:**
- `--ticket` (required): Ticket identifier
- `--files` (required): Comma-separated list of file paths
- `--doc` (optional): Specific document to update (default: index.md)

**Examples:**
```bash
ttmp relate --ticket MEN-3475 --files go/pkg/chat/handler.go,web/src/api/chat.ts
ttmp relate --ticket MEN-3475 --doc design/01-architecture.md --files pkg/server.go
```

**Output:**
Updates RelatedFiles field in document frontmatter.

## ttmp meta update

Edit metadata fields programmatically.

**Usage:**
```bash
ttmp meta update --doc <doc> --field <field> --value <value>
```

**Parameters:**
- `--doc` (required): Document path
- `--field` (required): Metadata field name (Status, Intent, Title, Ticket, DocType, Summary, LastUpdated)
- `--value` (required): New value for the field

**Examples:**
```bash
ttmp meta update --doc index.md --field Status --value active
ttmp meta update --doc design/01-arch.md --field Intent --value long-term
ttmp meta update --doc index.md --field LastUpdated --value 2025-10-21
```

**Output:**
Updates specified field in document metadata.

## ttmp vocab list

List vocabulary entries.

**Usage:**
```bash
ttmp vocab list <category> [options]
```

**Parameters:**
- `category` (positional, required): topics | docTypes | intent
- `--vocab-file` (optional): Path to vocabulary.yaml (default: ./doc/vocabulary.yaml)
- `--output` (optional): Output format (table | json | yaml | csv)

**Examples:**
```bash
ttmp vocab list topics
ttmp vocab list docTypes --output json
ttmp vocab list intent --output yaml
```

**Output:**
Structured list of vocabulary entries with slug and description.

## ttmp vocab add

Add a new vocabulary entry.

**Usage:**
```bash
ttmp vocab add <category> --slug <slug> --description <description>
```

**Parameters:**
- `category` (positional, required): topics | docTypes | intent
- `--slug` (required): Vocabulary slug
- `--description` (required): Human-readable description
- `--vocab-file` (optional): Path to vocabulary.yaml (default: ./doc/vocabulary.yaml)

**Examples:**
```bash
ttmp vocab add topics --slug frontend --description "Frontend development"
ttmp vocab add docTypes --slug spike --description "Exploratory spike"
```

**Output:**
Updates vocabulary.yaml with new entry.

## ttmp vocab assign

Assign topics to documents.

**Usage:**
```bash
ttmp vocab assign --ticket <ticket> --topics <topics> [options]
```

**Parameters:**
- `--ticket` (required): Ticket identifier
- `--doc` (optional): Specific document (default: all docs in ticket)
- `--topics` (required): Comma-separated topics to assign

**Examples:**
```bash
ttmp vocab assign --ticket MEN-3475 --doc index.md --topics chat,backend
ttmp vocab assign --ticket MEN-3475 --topics observability
```

**Output:**
Updates Topics field in document metadata.

## ttmp list tickets

List all tickets with metadata.

**Usage:**
```bash
ttmp list tickets [options]
```

**Parameters:**
- `--root` (optional): Root directory for ttmp (default: ./ttmp)
- `--output` (optional): Output format (table | json | yaml | csv)
- `--fields` (optional): Comma-separated list of fields to display

**Examples:**
```bash
ttmp list tickets
ttmp list tickets --output json
ttmp list tickets --fields ticket,status,topics
```

**Output:**
Structured list with ticket, slug, status, has_index, doc_count, topics, owners.

## ttmp list docs

List documents in a ticket.

**Usage:**
```bash
ttmp list docs [options]
```

**Parameters:**
- `--ticket` (optional): Ticket identifier (default: all tickets)
- `--root` (optional): Root directory for ttmp (default: ./ttmp)
- `--output` (optional): Output format (table | json | yaml | csv)

**Examples:**
```bash
ttmp list docs --ticket MEN-3475
ttmp list docs --ticket MEN-3475 --output json
ttmp list docs --fields ticket,file,doc_type,status
```

**Output:**
Structured list with ticket, file, doc_type, status, title, topics.

## ttmp doctor

Run health checks on ticket documentation.

**Usage:**
```bash
ttmp doctor [options]
```

**Parameters:**
- `--ticket` (optional): Ticket identifier (default: all tickets)
- `--root` (optional): Root directory for ttmp (default: ./ttmp)
- `--output` (optional): Output format (table | json | yaml | csv)

**Examples:**
```bash
ttmp doctor
ttmp doctor --ticket MEN-3475
ttmp doctor --output json
```

**Output:**
List of issues with severity, ticket, file, and message.

**Checks:**
- Missing index.md
- Documents without required metadata
- Unknown topics or doc types
- Stale documents (LastUpdated > 14 days)
- Missing Status field

## ttmp search

Search for related files using git history and patterns.

**Usage:**
```bash
ttmp search --ticket <ticket> [options]
```

**Parameters:**
- `--ticket` (required): Ticket identifier
- `--topics` (optional): Filter by topics
- `--root` (optional): Root directory for ttmp (default: ./ttmp)
- `--output` (optional): Output format (table | json | yaml | csv)

**Examples:**
```bash
ttmp search --ticket MEN-3475 --topics chat
ttmp search --ticket MEN-3475 --output json
```

**Output:**
Suggested related files with file, relevance, and reason.

## Global Flags

All commands support these global flags:

- `--help, -h`: Show help for the command
- `--version`: Show version information

Commands using GlazeCommand (list, doctor, search, vocab list) also support:

- `--output`: Output format (table, json, yaml, csv)
- `--fields`: Comma-separated list of fields to display
- `--sort-columns`: Columns to sort by
- `--filter`: Filter rows by expression
- `--flatten`: Flatten nested structures
- `--template`: Go template for custom formatting

## Next Steps

Learn about the metadata schema:

```
ttmp help metadata-schema
```

Understand vocabulary management:

```
ttmp help vocabulary-guide
```

See practical examples:

```
ttmp help tutorial-basic-workflow
```

