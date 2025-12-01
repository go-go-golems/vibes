# Changelog

## 2025-11-30

- Initial workspace created


## 2025-11-30

Documented internal inputs refactor plan

### Related Files

- /home/manuel/workspaces/2025-11-30/photobook-app-go/vibes/2025/11/29/photobook-app/2025/11/30/ENGINE-INPUTS-STRUCT-group-imagelayout-engine-inputs/design/01-engine-inputs-grouping-analysis.md — Analysis + plan
- /home/manuel/workspaces/2025-11-30/photobook-app-go/vibes/2025/11/29/photobook-app/2025/11/30/ENGINE-INPUTS-STRUCT-group-imagelayout-engine-inputs/tasks.md — Tasks derived from plan


## 2025-11-30

Introduced NormalizedInputs (Frame/Crop/Presentation) and refactored engine helpers to use per-phase structs; CLI/validation updated accordingly

### Related Files

- /home/manuel/workspaces/2025-11-30/photobook-app-go/zine-layout/cmd/zine-layout/cmds/imagelayout/layout_commands.go — CLI verbs updated for new analyzer signatures
- /home/manuel/workspaces/2025-11-30/photobook-app-go/zine-layout/pkg/imagelayout/engine/analysis.go — Analyzers now consume narrow inputs
- /home/manuel/workspaces/2025-11-30/photobook-app-go/zine-layout/pkg/imagelayout/engine/engine.go — ComputeViewport + normalization emit grouped structs
- /home/manuel/workspaces/2025-11-30/photobook-app-go/zine-layout/pkg/imagelayout/engine/normalized_inputs.go — Defines grouped inputs
- /home/manuel/workspaces/2025-11-30/photobook-app-go/zine-layout/scripts/imagelayout_validation/main.go — Validation harness uses new frame/crop structs

