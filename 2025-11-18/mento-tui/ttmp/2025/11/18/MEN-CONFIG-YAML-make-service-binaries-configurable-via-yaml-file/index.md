---
Title: Make service binaries configurable via YAML file
Ticket: MEN-CONFIG-YAML
Status: active
Topics:
    - configuration
    - yaml
    - services
DocType: index
Intent: long-term
Owners: []
RelatedFiles:
    - Path: /home/manuel/code/wesen/corporate-headquarters/vibes/2025-11-18/mento-tui/README.md
      Note: Update documentation for YAML configuration and usage
    - Path: /home/manuel/code/wesen/corporate-headquarters/vibes/2025-11-18/mento-tui/cmd/main.go
      Note: |-
        Contains hardcoded working directory that should be configurable
        CLI --config flag; remove hardcoded chdir; load YAML config
    - Path: /home/manuel/code/wesen/corporate-headquarters/vibes/2025-11-18/mento-tui/go.mod
      Note: Add YAML dependency (gopkg.in/yaml.v3)
    - Path: /home/manuel/code/wesen/corporate-headquarters/vibes/2025-11-18/mento-tui/internal/models/models.go
      Note: |-
        Defines Service struct that will be populated from YAML config
        Service struct; add WorkingDirectory
    - Path: /home/manuel/code/wesen/corporate-headquarters/vibes/2025-11-18/mento-tui/internal/services/manager.go
      Note: |-
        Contains hardcoded service definitions that need to be made configurable
        Service lifecycle
    - Path: /home/manuel/code/wesen/corporate-headquarters/vibes/2025-11-18/mento-tui/internal/ui/app.go
      Note: UI wiring; pass config to manager and screens
    - Path: /home/manuel/code/wesen/corporate-headquarters/vibes/2025-11-18/mento-tui/internal/ui/config.go
      Note: Config viewer reference for environment/config display
    - Path: /home/manuel/code/wesen/corporate-headquarters/vibes/2025-11-18/mento-tui/internal/ui/dashboard.go
      Note: Dashboard rendering; display multiple ports per service
    - Path: /home/manuel/code/wesen/corporate-headquarters/vibes/2025-11-18/mento-tui/ttmp/2025/11/18/MEN-CONFIG-YAML-make-service-binaries-configurable-via-yaml-file/various/02-additional-configuration-options.md
      Note: Comprehensive list of additional configuration options
ExternalSources: []
Summary: ""
LastUpdated: 2025-11-18T09:11:14.737571111-05:00
---














# Make service binaries configurable via YAML file

## Overview

This ticket aims to make the mento-tui service manager configurable via a YAML file instead of hardcoding service definitions in Go code.

**Current State**: Services (Identity Server, Frontend/Vite, Mento Worker) are hardcoded in `internal/services/manager.go` with fixed ports, binary paths, and environment variables.

**Goal**: Allow users to configure services, ports, binary paths, and environment variables through a YAML configuration file.

**Analysis**: See `various/01-analysis-repository-configuration.md` for detailed analysis of current implementation and proposed changes.

## Key Links

- **Related Files**: See frontmatter RelatedFiles field
- **External Sources**: See frontmatter ExternalSources field

## Status

Current status: **active**

## Topics

- configuration
- yaml
- services

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
