---
Title: Basic TTMP Workflow Tutorial
Slug: tutorial-basic-workflow
Short: Step-by-step guide to creating and managing a ticket with ttmp
Topics:
- tutorial
- workflow
- getting-started
Commands:
- init
- add
- relate
- list
Flags: []
IsTopLevel: true
IsTemplate: false
ShowPerDefault: true
SectionType: Tutorial
---

This tutorial walks through a complete workflow for managing ticket documentation with ttmp, from initialization through to completion and health checks.

## Step 1: Initialize a New Ticket

Start by creating a new ticket directory with the `init` command. This scaffolds the standard structure and creates initial files.

```bash
ttmp init MEN-3475 --title "Chat backend refactoring" --topics chat,backend,refactoring --owners alice
```

This creates:

```
ttmp/MEN-3475-chat-backend-refactoring/
  index.md           # Landing page
  tasks.md           # Task list
  changelog.md       # Decision log
  various/           # Working notes
  design/            # Design documents
  reference/         # References and prompts
  playbooks/         # Operational procedures
  scripts/           # Temporary code
```

## Step 2: Add Working Notes

As you explore the codebase and gather context, capture it in working notes:

```bash
cd ttmp/MEN-3475-chat-backend-refactoring
ttmp add working-note "initial codebase survey"
```

Edit the generated file to add your findings. The template includes sections for summary, notes, action items, and references.

## Step 3: Link Related Files

Connect your documentation to the actual code being modified:

```bash
ttmp relate --ticket MEN-3475 --files go/pkg/chat/handler.go,go/pkg/chat/websocket.go,web/src/api/chat.ts
```

This updates the `RelatedFiles` field in `index.md`, making it easy for LLMs to understand which files are relevant.

## Step 4: Create Design Documentation

When you've gathered enough context, document your design decisions:

```bash
ttmp add design-doc "websocket-architecture"
```

The design-doc template includes sections for executive summary, context, proposed solution, alternatives considered, and implementation notes.

## Step 5: Add Reference Material

Create reusable prompt packs or API references that can be fed to LLMs:

```bash
ttmp add reference "chat-api-contract"
```

Reference documents include structured sections for goal, context chunks, usage prompts, and expected output.

## Step 6: Create Operational Playbooks

Document manual procedures or command sequences:

```bash
ttmp add playbook "smoke-test-websocket"
```

Playbooks include prerequisites, step-by-step commands with expected output, verification steps, and troubleshooting guidance.

## Step 7: List and Inspect

View all tickets and their status:

```bash
ttmp list tickets
ttmp list tickets --output json
```

View documents within a ticket:

```bash
ttmp list docs --ticket MEN-3475
ttmp list docs --ticket MEN-3475 --fields file,doc_type,status
```

## Step 8: Run Health Checks

Before wrapping up, ensure documentation quality:

```bash
ttmp doctor --ticket MEN-3475
```

The doctor command checks for:
- Missing index.md
- Documents without required metadata
- Unknown topics or doc types
- Stale documents (LastUpdated > 14 days)
- Missing Status field

## Step 9: Update Metadata

As work progresses, update document status:

```bash
ttmp meta update --doc index.md --field Status --value active
ttmp meta update --doc design/01-websocket-architecture.md --field Intent --value long-term
```

## Step 10: Archive Completed Work

When the ticket is complete, mark it as archived:

```bash
ttmp meta update --doc index.md --field Status --value archived
```

## Best Practices

1. **Initialize early**: Create the ticket structure as soon as work begins
2. **Capture context continuously**: Add working notes throughout the day
3. **Link files proactively**: Use `relate` to connect documentation to code
4. **Document decisions**: Use design-docs for rationale that LLMs should remember
5. **Create reusable references**: Extract stable prompts into reference documents
6. **Run doctor regularly**: Check documentation health before major milestones
7. **Update status**: Keep Status field current (draft → active → review → archived)

## Common Patterns

### Spike Investigation

For exploratory work:

```bash
ttmp init MEN-3476 --title "Database performance spike" --topics observability,database --intent throwaway
ttmp add working-note "query analysis"
ttmp add playbook "reproduce-slow-query"
```

### Long-term Reference

For documentation that should graduate to permanent docs:

```bash
ttmp add reference "llm-context-best-practices" --intent long-term
ttmp meta update --doc reference/01-llm-context-best-practices.md --field Intent --value long-term
```

### Multi-owner Collaboration

For tickets with multiple contributors:

```bash
ttmp init MEN-3477 --title "Frontend redesign" --topics frontend,design --owners alice,bob,charlie
```

## Next Steps

Learn about the complete command reference:

```
ttmp help commands-reference
```

Understand the metadata schema:

```
ttmp help metadata-schema
```

Explore vocabulary management:

```
ttmp help vocabulary-guide
```

