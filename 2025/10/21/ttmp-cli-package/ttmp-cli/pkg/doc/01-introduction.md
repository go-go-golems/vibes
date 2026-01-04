---
Title: Introduction to TTMP CLI
Slug: introduction
Short: Learn about the ttmp CLI tool for managing structured LLM workflow documentation
Topics:
- getting-started
- overview
Commands: []
Flags: []
IsTopLevel: true
IsTemplate: false
ShowPerDefault: true
SectionType: GeneralTopic
---

The **ttmp** CLI tool manages structured documentation for LLM-assisted development workflows. It provides commands for creating, organizing, and maintaining ticket-focused documentation in the `ttmp/` directory, following a consistent structure and metadata model that makes collaboration with LLMs more reliable and efficient.

## Why TTMP?

When working with LLMs on software projects, context management becomes critical. The ttmp tool solves common problems:

- **No entry point per ticket**: Without structure, contributors must skim multiple files to understand scope and decisions
- **Inconsistent metadata**: Missing YAML frontmatter makes filtering and automation impossible
- **Context drift**: Repeated discovery work that could be cached once
- **Search burden**: Rich references exist but have no metadata for discoverability

## Core Concepts

The ttmp system organizes documentation around **tickets** (work items like MEN-3475) with a predictable structure:

- **index.md**: Canonical landing page summarizing ticket intent, status, and key links
- **Subdirectories**: Organized spaces for different document types (design/, reference/, playbooks/, etc.)
- **Metadata**: YAML frontmatter on every document with topics, status, owners, and related files
- **Vocabulary**: Controlled vocabulary in `doc/vocabulary.yaml` for consistent tagging

## Quick Start

Initialize a new ticket:

```bash
ttmp init MEN-3475 --title "Chat API cleanup" --topics chat,backend
```

Add documentation:

```bash
cd ttmp/MEN-3475-chat-api-cleanup
ttmp add working-note "initial context"
ttmp add design-doc "api-architecture"
```

List and inspect:

```bash
ttmp list tickets
ttmp list docs --ticket MEN-3475
ttmp doctor --ticket MEN-3475
```

## Next Steps

Learn more about specific commands and workflows:

```
ttmp help tutorial-basic-workflow
ttmp help commands-reference
ttmp help metadata-schema
```

