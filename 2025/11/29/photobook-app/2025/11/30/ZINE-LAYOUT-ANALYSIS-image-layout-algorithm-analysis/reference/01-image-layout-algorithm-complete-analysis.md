---
Title: 'Image Layout Algorithm: Orientation Guide'
Ticket: ZINE-LAYOUT-ANALYSIS
Status: active
Topics:
    - imagelayout
    - analysis
    - reference
DocType: reference
Intent: long-term
Owners: []
RelatedFiles:
    - Path: ../../../../../../../../../../zine-layout/cmd/zine-layout/cmds/imagelayout/compute.go
      Note: CLI command entrypoint
    - Path: ../../../../../../../../../../zine-layout/pkg/imagelayout/defaults.go
      Note: Defaults for modern and legacy settings
    - Path: ../../../../../../../../../../zine-layout/pkg/imagelayout/engine/engine.go
      Note: ComputeViewport core math
    - Path: ../../../../../../../../../../zine-layout/pkg/imagelayout/engine/inputs.go
      Note: LayoutRequest normalization
    - Path: ../../../../../../../../../../zine-layout/pkg/imagelayout/engine/engine_test.go
      Note: Behavior and validation coverage
    - Path: 2025/11/29/photobook-app/2025/11/30/ZINE-LAYOUT-ANALYSIS-image-layout-algorithm-analysis/reference/03-image-layout-api-usage-examples.md
      Note: Companion examples doc
ExternalSources: []
Summary: 'Orientation guide to the imagelayout engine: mental model, algorithm snapshot, validation pointers'
LastUpdated: 2025-11-30T22:45:20-05:00
---

# Image Layout Algorithm: Orientation Guide

This trims the previous long-form analysis down to the essentials: the mental model, how the engine flows, and where to find details in code or examples.

## What this covers
- How to think about LayoutRequest → NormalizedInputs → ViewportResult
- The minimal algorithm walk-through (ratio choice, crop, scale, place)
- Quick cheats for frame modes, crop strategies, and scaling
- Pointers to examples, CLI helpers, and validation tests

## Mental model: LayoutRequest → NormalizedInputs
- **FrameSpec (output box)**: ratio/page/viewport decide the target rectangle and fill mode (contain vs cover).
- **CropSpec (what survives)**: ratio override, zoom/extent, and positioning strategy (auto, anchor, focus, manual pan).
- **PresentationSpec (after the cut)**: user scale and pixel nudges applied after crop/scale math.
- `InputsFromRequest` groups these into `FrameInputs`, `CropInputs`, `PresentationInputs`, and `SourceMeta`, matching the internal flow (frame first, crop second, presentation last).

## Package map
- `pkg/imagelayout/types.go`: public structs (`LayoutRequest`, `ViewportResult`, `Rect`, `ImageMeta`, legacy `ViewportSettings`).
- `pkg/imagelayout/engine/inputs.go`: `InputsFromRequest` normalization (modern API) plus legacy adapter.
- `pkg/imagelayout/engine/engine.go`: `ComputeViewport` with `buildFrame`, `resolveCrop`, `composeTarget`.
- `pkg/imagelayout/defaults.go`: defaults for modern and legacy shapes.
- `pkg/imagelayout/engine/engine_test.go`: end-to-end coverage of modes, ratios, offsets, validation.
- CLI front door: `cmd/zine-layout/cmds/imagelayout/compute.go`.

## Algorithm snapshot
1. **Normalize inputs** (`InputsFromRequest`): validate source dims and requested frame/crop/presentation; derive canvas (ratio/page/viewport), resolve margins, pick crop ratio, clamp units.
2. **Compute viewport** (`ComputeViewport`):
   - Frame: build canvas/content rects.
   - Ratio pick: prefer `Crop.Ratio`; else if `Frame.Fill == cover` use target ratio; else fall back to source ratio.
   - Crop: compare source vs requested ratio, trim width or height, apply zoom/extent.
   - Position: strategy selects pan (auto center, anchor presets, focus point alignment, or manual pan).
   - Scale: contain uses `min(scaleX, scaleY)`; cover uses `max`.
   - Presentation: apply `UserScale` and `OffsetPx`; produce `SourceRect`, `TargetRect`, `CanvasRect`, and `Scale`.
3. **Trace**: optional step-by-step trace mirrors the phases above for debugging.

## Frame modes (cheat sheet)
| Mode      | Canvas math                                   | Typical use                   |
|-----------|-----------------------------------------------|-------------------------------|
| ratio     | `W = Ratio * sourceH`, `H = sourceH`          | Web/editor aspect previews    |
| page      | `Canvas = page in * DPI`, `Content = Canvas - margins` | Print/PDF with margins |
| viewport  | `Canvas = given px (derive missing via Ratio)`| Fixed or responsive canvases  |

Example (ratio):
```go
req := imagelayout.LayoutRequest{
    Frame: imagelayout.FrameSpec{Mode: "ratio", Ratio: floatPtr(16.0 / 9.0), Fill: "contain"},
}
```

## Crop strategies and scaling
- **auto**: center crop.
- **anchor**: named presets map to normalized pan (-1..1).
- **focus**: align a source pixel to a target percentage.
- **manual**: explicit pan (normalized or px).
- **Scaling**: contain keeps full crop visible (`min` scale); cover fills the box (`max` scale). `Presentation.UserScale` multiplies afterward; `Presentation.OffsetPx` nudges placement without changing crop math.

## Examples and CLI helpers
- See `vibes/.../03-image-layout-api-usage-examples.md` for runnable LayoutRequest and CLI cases (ratio/page/viewport, focus points, offsets, zoom).
- CLI: `zine-layout imagelayout compute --spec <file>` for full results; `layout frame|crop|presentation` to inspect each stage.

## Validation and edge checks
- Source dimensions > 0; page mode requires DPI > 0 and margins within canvas (tests in `engine_test.go`).
- Viewport mode: at least one dimension or ratio present.
- Crop ratio > 0 when provided; crop units limited to `normalized` or `px`.
- Safe division and clamping prevent zero/NaN scale values (`engine.go` helpers).

## Related
- Deep dive on the crop revamp rationale: `vibes/2025/11/29/photobook-app/2025/11/30/REVAMP-CROP-ALGORITHM-revamp-crop-algorithm-for-aspect-ratio-cropping/reference/01-crop-algorithm-revamp-analysis.md`.
- Companion examples: `vibes/2025/11/29/photobook-app/2025/11/30/ZINE-LAYOUT-ANALYSIS-image-layout-algorithm-analysis/reference/03-image-layout-api-usage-examples.md`.
