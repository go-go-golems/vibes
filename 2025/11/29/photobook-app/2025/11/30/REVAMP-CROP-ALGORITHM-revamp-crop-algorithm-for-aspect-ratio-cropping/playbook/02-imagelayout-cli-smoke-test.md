---
Title: Imagelayout CLI Smoke Test
Ticket: REVAMP-CROP-ALGORITHM
Status: active
Topics:
    - imagelayout
    - cli
    - testing
DocType: playbook
Intent: short-term
Owners: []
RelatedFiles:
    - Path: ../../../../../../../../../../zine-layout/cmd/zine-layout/cmds/imagelayout/compute.go
      Note: |-
        imagelayout compute command
        CLI smoke test targets LayoutRequest compute/analysis verbs
    - Path: ../../../../../../../../../../zine-layout/cmd/zine-layout/cmds/imagelayout/layout_commands.go
      Note: frame/crop/presentation inspection verbs
ExternalSources: []
Summary: Quick smoke test for imagelayout CLI using LayoutRequest inputs
LastUpdated: 2025-12-01T00:00:00-05:00
---


# Imagelayout CLI Smoke Test

Run this after changes to the imagelayout engine or CLI wiring to verify the modern LayoutRequest path works end-to-end.

## Prerequisites
- Go toolchain available
- Sample image dimensions: use 4000×3000 for these checks
- LayoutRequest spec saved at `/tmp/layout.yaml`

Example `/tmp/layout.yaml`:
```yaml
frame:
  mode: ratio
  ratio: 1.3333333333   # 4:3
  fill: cover
crop:
  strategy: anchor
  anchor: top-left
  zoom: 1.0
presentation:
  user_scale: 1.0
  offset_px: { x: 0, y: 0 }
export:
  format: png
  quality: 90
  background: white
  filename_template: test-{panel}.{ext}
  out_dir: ./out
```

## Commands

1) Compute full layout:
```bash
go run ./cmd/zine-layout imagelayout compute \
  --spec /tmp/layout.yaml \
  --source-width 4000 --source-height 3000
```
- Expect JSON with `layout`, `result`, and `trace` fields; `result.mode` should be `"cover"`.

2) Inspect frame stage:
```bash
go run ./cmd/zine-layout imagelayout layout frame \
  --spec /tmp/layout.yaml \
  --source-width 4000 --source-height 3000
```
- Expect `canvas_rect` and `target_ratio` reflecting 4:3.

3) Inspect crop stage:
```bash
go run ./cmd/zine-layout imagelayout layout crop \
  --spec /tmp/layout.yaml \
  --source-width 4000 --source-height 3000
```
- Expect `source_rect` trimmed to the requested ratio and anchored top-left.

4) Inspect presentation stage:
```bash
go run ./cmd/zine-layout imagelayout layout presentation \
  --spec /tmp/layout.yaml \
  --source-width 4000 --source-height 3000
```
- Expect `target_rect` placement with any offsets/scales applied.

## Quick checklist
- All commands complete without errors.
- `result.mode` matches `frame.fill`.
- `source_rect` ratio matches requested crop/frame ratio.
- Offsets and user scale propagate into `target_rect` when provided.
