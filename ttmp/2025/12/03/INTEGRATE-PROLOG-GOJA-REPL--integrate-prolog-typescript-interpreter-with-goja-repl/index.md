---
Title: Integrate Prolog TypeScript Interpreter with Goja REPL
Ticket: INTEGRATE-PROLOG-GOJA-REPL
Status: active
Topics:
    - go
    - typescript
    - goja
    - prolog
    - repl
DocType: index
Intent: long-term
Owners: []
RelatedFiles:
    - Path: 2025-06-22/goja-ts-integration
      Note: |-
        Reference implementation for Goja TypeScript integration patterns
        Reference implementation for TypeScript-Go integration pattern
    - Path: 2025/12/03/prolog-webapp/server/prolog-ts.ts
      Note: |-
        Source Prolog TypeScript interpreter to be integrated
        TypeScript Prolog interpreter - source implementation
    - Path: 2025/12/05/goja-prolog
      Note: New Go project implementing Prolog interpreter with Goja
    - Path: 2025/12/05/goja-prolog/internal/prolog/evaluator.go
      Note: PrologEvaluator implementation with Goja integration
    - Path: 2025/12/05/goja-prolog/pkg/doc/topics/building-typescript-goja-applications.md
      Note: Complete playbook for TypeScript + Goja integration
    - Path: ttmp/2025/12/03/INTEGRATE-PROLOG-GOJA-REPL--integrate-prolog-typescript-interpreter-with-goja-repl/analysis/05-bug-report-query-bindings-not-displaying-variable-values.md
      Note: Bug report for bindings display issue
ExternalSources: []
Summary: Integrate TypeScript Prolog interpreter into Go binary using Goja VM, creating standalone REPL tool
LastUpdated: 2025-12-03T19:16:15.432803496-05:00
---








# Integrate Prolog TypeScript Interpreter with Goja REPL

## Overview

<!-- Provide a brief overview of the ticket, its goals, and current status -->

## Key Links

- **Related Files**: See frontmatter RelatedFiles field
- **External Sources**: See frontmatter ExternalSources field

## Status

Current status: **active**

## Topics

- go
- typescript
- goja
- prolog
- repl

## Tasks

See [tasks.md](./tasks.md) for the current task list.

## Changelog

See [changelog.md](./changelog.md) for recent changes and decisions.

## Structure

- design/ - Architecture and design documents
- reference/ - Prompt packs, API contracts, context summaries
- playbooks/ - Command sequences and test procedures
- scripts/ - Temporary code and tooling
- various/ - Working notes and research
- archive/ - Deprecated or reference-only artifacts
