---
Title: Sequencing Page Implementation
Ticket: SEQUENCING-PAGE-IMPLEMENTATION
Status: active
Topics:
    - sequencing
    - frontend
    - backend
    - implementation
DocType: index
Intent: long-term
Owners: []
RelatedFiles:
    - Path: ../../../../../../../../../zine-layout/pkg/docs/topics/frontend-developer-guide.md
      Note: React developer onboarding + patterns + playbooks
    - Path: ../../../../../../../../../zine-layout/web/src/api.ts
      Note: RTK Query hooks with optimistic updates
    - Path: ../../../../../../../../../zine-layout/web/src/views/ProjectDetail.tsx
      Note: Updated to use SequencesTabWrapper for integrated sequencing UI
    - Path: ../../../../../../../../../zine-layout/web/src/views/tabs/SequencesTabWrapper.tsx
      Note: Wrapper component that allows switching between v2 and legacy sequencing UI
    - Path: ../../../../../../../../../zine-layout/web/src/views/v2/SequencingPage.tsx
      Note: Main sequencing page component for v2 implementation
    - Path: ../../../../../../../../../zine-layout/web/src/views/v2/components/AssetPicker.tsx
      Note: Modal component for selecting multiple assets to add to sequences
    - Path: ../../../../../../../../../zine-layout/web/src/views/v2/components/SequenceEditor.tsx
      Note: Component for editing sequence items
    - Path: ../../../../../../../../../zine-layout/web/src/views/v2/components/SequenceItem.tsx
      Note: Loads asset previews via rel_path/url and shows fallback messaging
    - Path: ../../../../../../../../../zine-layout/web/src/views/v2/components/SequenceList.tsx
      Note: Component for displaying and creating sequences
    - Path: ../../../../../../../../../zine-layout/web/src/views/v2/components/SequenceSlideshow.tsx
      Note: Implements single + book spread preview per sequencing UX walkthrough
    - Path: 2025/11/29/photobook-app/2025/11/30/SEQUENCING-PAGE-IMPLEMENTATION-sequencing-page-implementation/reference/playbook-draggable-sequence-editor.md
      Note: How-to implement optimistic
ExternalSources: []
Summary: ""
LastUpdated: 2025-11-30T19:04:19.77905811-05:00
---









# Sequencing Page Implementation

## Overview

<!-- Provide a brief overview of the ticket, its goals, and current status -->

## Key Links

- **Related Files**: See frontmatter RelatedFiles field
- **External Sources**: See frontmatter ExternalSources field

## Status

Current status: **active**

## Topics

- sequencing
- frontend
- backend
- implementation

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
