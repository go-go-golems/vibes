# Changelog

## 2025-11-30

- Initial workspace created

## 2025-12-01

- Added modern `LayoutRequest`-aligned TypeScript types (Frame/Crop/Presentation) alongside legacy settings to begin frontend migration toward the new API shapes.
- Updated ImageLayoutsTab to build/send `LayoutRequest` (frame/crop/presentation) and load both legacy and new shapes; began UI copy updates toward the new model.
- Added CLI smoke-test playbook (`playbook/02-imagelayout-cli-smoke-test.md`) documenting compute/frame/crop/presentation commands for LayoutRequest specs.
- Ran imagelayout CLI smoke test with sample LayoutRequest (/tmp/layout.yaml, 4000×3000 source) across `compute`, `layout frame`, `layout crop`, and `layout presentation` commands; all returned expected geometry and modes.


## 2025-11-30

Defined LayoutRequest data model and refactored engine helpers

### Related Files

- /home/manuel/workspaces/2025-11-30/photobook-app-go/zine-layout/pkg/imagelayout/defaults.go — Added DefaultLayoutRequest baseline
- /home/manuel/workspaces/2025-11-30/photobook-app-go/zine-layout/pkg/imagelayout/engine/engine.go — Split ComputeViewport into buildFrame/resolveCrop/composeTarget
- /home/manuel/workspaces/2025-11-30/photobook-app-go/zine-layout/pkg/imagelayout/types.go — Introduced LayoutRequest + supporting specs


## 2025-11-30

Added LayoutRequest→Inputs conversion and ratio/page unit tests

### Related Files

- /home/manuel/workspaces/2025-11-30/photobook-app-go/zine-layout/pkg/imagelayout/engine/inputs.go — New InputsFromRequest builder
- /home/manuel/workspaces/2025-11-30/photobook-app-go/zine-layout/pkg/imagelayout/engine/inputs_test.go — Unit tests for ratio/page frames


## 2025-11-30

Expanded LayoutRequest->Inputs for viewport frames and added coverage

### Related Files

- /home/manuel/workspaces/2025-11-30/photobook-app-go/zine-layout/pkg/imagelayout/engine/inputs.go — Derived viewport dimensions helper
- /home/manuel/workspaces/2025-11-30/photobook-app-go/zine-layout/pkg/imagelayout/engine/inputs_test.go — Viewport test case


## 2025-11-30

Implemented crop zoom/presentation offsets in LayoutRequest inputs and added tests

### Related Files

- /home/manuel/workspaces/2025-11-30/photobook-app-go/zine-layout/pkg/imagelayout/engine/engine.go — Crop scale + presentation offsets
- /home/manuel/workspaces/2025-11-30/photobook-app-go/zine-layout/pkg/imagelayout/engine/engine_test.go — Zoom + presentation test
- /home/manuel/workspaces/2025-11-30/photobook-app-go/zine-layout/pkg/imagelayout/engine/inputs.go — Mapped zoom/extent/presentation
- /home/manuel/workspaces/2025-11-30/photobook-app-go/zine-layout/pkg/imagelayout/engine/inputs_test.go — Zoom/presentation builder tests


## 2025-11-30

Added imagelayout layout frame/crop/presentation verbs plus engine analysis helpers

### Related Files

- /home/manuel/workspaces/2025-11-30/photobook-app-go/zine-layout/cmd/zine-layout/cmds/imagelayout/layout_commands.go — New CLI verbs
- /home/manuel/workspaces/2025-11-30/photobook-app-go/zine-layout/cmd/zine-layout/cmds/imagelayout/layout_commands_test.go — Coverage for verbs
- /home/manuel/workspaces/2025-11-30/photobook-app-go/zine-layout/pkg/imagelayout/engine/analysis.go — Frame/Crop/Presentation analysis helpers
- /home/manuel/workspaces/2025-11-30/photobook-app-go/zine-layout/scripts/imagelayout_validation/main.go — Validation harness shifted to LayoutRequest specs


## 2025-11-30

Added flag-based imagelayout CLI verbs + smoke playbook

### Related Files

- /home/manuel/workspaces/2025-11-30/photobook-app-go/zine-layout/cmd/zine-layout/cmds/imagelayout/layout_commands.go — Frame/Crop/Presentation/Compute verbs
- /home/manuel/workspaces/2025-11-30/photobook-app-go/zine-layout/cmd/zine-layout/cmds/imagelayout/layout_commands_test.go — CLI tests
- /home/manuel/workspaces/2025-11-30/photobook-app-go/zine-layout/cmd/zine-layout/cmds/imagelayout/layout_flags.go — Flag parsing + overrides
- /home/manuel/workspaces/2025-11-30/photobook-app-go/zine-layout/scripts/imagelayout_cli_playbook.sh — Smoke script covering verbs


## 2025-11-30

Scoped imagelayout CLI verbs to relevant flags, added analysis helpers/playbook

### Related Files

- /home/manuel/workspaces/2025-11-30/photobook-app-go/scripts/playbooks/imagelayout_cli_smoke.sh — New playbook exercising verbs
- /home/manuel/workspaces/2025-11-30/photobook-app-go/zine-layout/cmd/zine-layout/cmds/imagelayout/compute.go — compute command now reuses flag builder
- /home/manuel/workspaces/2025-11-30/photobook-app-go/zine-layout/cmd/zine-layout/cmds/imagelayout/layout_commands.go — frame/crop/presentation verbs with appropriate flags
- /home/manuel/workspaces/2025-11-30/photobook-app-go/zine-layout/cmd/zine-layout/cmds/imagelayout/layout_commands_test.go — Updated tests to drive both spec+flag flows
- /home/manuel/workspaces/2025-11-30/photobook-app-go/zine-layout/cmd/zine-layout/cmds/imagelayout/layout_params.go — Per-mode flag definitions
- /home/manuel/workspaces/2025-11-30/photobook-app-go/zine-layout/pkg/imagelayout/engine/analysis.go — Frame/Crop/Presentation analyzers used by CLI

## 2025-11-30

Frontend: ImageLayoutsTab now builds/sends LayoutRequest (frame/crop/presentation) and loads both legacy/new shapes; added fill/crop strategy controls, pan/offset inputs. Added CLI smoke-test playbook for imagelayout compute/frame/crop/presentation commands.


## 2025-11-30

CLI+Frontend migration: ImageLayoutsTab now builds/sends LayoutRequest (frame/crop/presentation) with fill mode, crop strategy, pan, offsets; template cards display LayoutRequest page info; updated LayoutTemplateManager copy to reference LayoutRequest. API types accept LayoutRequest for templates/overrides; LaidOutImage overrides typed for new shape. Added imagelayout CLI smoke-test playbook covering compute/frame/crop/presentation commands with sample spec. CLI verbs already scoped per mode via layout frame/crop/presentation commands.

## 2025-11-30

Migrated CLI logging off Viper: root command now uses InitLoggerFromCobra (no deprecation warnings). Verified imagelayout CLI commands run without logging warnings (compute/layout frame/crop/presentation against /tmp/layout.yaml).


## 2025-11-30

Removed legacy ViewportSettings and InputsFromSettings; engine and tests now rely solely on LayoutRequest. Updated TypeScript API to drop legacy settings interfaces; templates/overrides are LayoutRequest-only. Ran gofmt and go test ./... to validate.


## 2025-11-30

Docs cleanup: removed legacy ViewportSettings references from usage examples, analysis overview, crop revamp analysis now notes legacy removal, and Zine App design overview now points to LayoutRequest.

