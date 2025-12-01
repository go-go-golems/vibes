---
Title: 'Image Layout Algorithm: Complete Analysis'
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
      Note: CLI command implementation using imagelayout engine
    - Path: ../../../../../../../../../../zine-layout/pkg/imagelayout/defaults.go
      Note: Default settings factory function
    - Path: ../../../../../../../../../../zine-layout/pkg/imagelayout/engine/engine.go
      Note: Core computation algorithms (InputsFromSettings)
    - Path: ../../../../../../../../../../zine-layout/pkg/imagelayout/engine/inputs.go
      Note: Modern LayoutRequest normalization
    - Path: ../../../../../../../../../../zine-layout/pkg/imagelayout/engine/normalized_inputs.go
      Note: Grouped internal structs (frame/crop/presentation/source)
    - Path: ../../../../../../../../../../zine-layout/pkg/imagelayout/engine/analysis.go
      Note: Stage inspection helpers
    - Path: ../../../../../../../../../../zine-layout/pkg/imagelayout/engine/engine_test.go
      Note: Comprehensive test suite for all algorithms
    - Path: ../../../../../../../../../../zine-layout/pkg/imagelayout/types.go
      Note: Core type definitions (Rect, LayoutRequest, ViewportSettings legacy)
    - Path: ../../../../../../../../../../zine-layout/pkg/imagelayout/engine/inputs.go
      Note: Modern LayoutRequest normalization
    - Path: ../../../../../../../../../../zine-layout/pkg/pagelayout/renderer/renderer.go
      Note: Page renderer that uses ViewportResult for cropping and placement
    - Path: ../../../../../../../../../../zine-layout/pkg/services/layout.go
      Note: Service layer integration (CreateLaidOutImage)
    - Path: ../../../../../../../../../../zine-layout/web/src/api.ts
      Note: TypeScript type definitions matching ViewportSettings
    - Path: 2025/11/29/photobook-app/2025/11/30/ZINE-LAYOUT-ANALYSIS-image-layout-algorithm-analysis/reference/03-image-layout-api-usage-examples.md
      Note: Companion examples doc
ExternalSources: []
Summary: 'Comprehensive analysis of the imagelayout package: algorithms, API, types, cropping, scaling, ratio calculations, trace reading, and worked examples'
LastUpdated: 2025-11-30T22:55:20-05:00
---

# Image Layout Algorithm: Complete Analysis

Restores the full walkthrough (mental model, algorithms, examples) while cutting only redundant prose from the 1.5k-line draft.

## Goal

This document is the long-form reference for the `imagelayout` package. It explains the algorithms, types, and API contracts that govern cropping, scaling, ratio handling, and placement so engineers can reason about behavior without digging through code.

## Context

`imagelayout` decides how a source image is cropped and placed into a viewport or page. The engine supports three frame modes (ratio, page, viewport), two scaling behaviors (contain vs cover), and several positioning strategies (auto, anchor presets, focus points, manual pan). It is consumed by:
- CLI commands (`cmd/zine-layout/cmds/imagelayout/compute.go`)
- Service layer (`pkg/services/layout.go`)
- Page renderer (`pkg/pagelayout/renderer/renderer.go`)
- Web API (`web/src/api.ts` type shapes)

The `REVAMP-CROP-ALGORITHM` work introduced the modern `LayoutRequest` (frame/crop/presentation) and grouped `NormalizedInputs`, replacing the flat legacy `ViewportSettings`.

## Modern API: LayoutRequest (REVAMP-CROP-ALGORITHM)

The modern request model splits decisions into three ordered phases: first pick the frame shape, then decide which part of the source survives, and finally apply presentation tweaks. This mirrors how photographers and designers think about framing and helps the code keep concerns isolated.

- **FrameSpec (output box)** describes the target rectangle via `Mode` (`ratio | page | viewport`), `Ratio`, `Fill` (`contain | cover`), `Page`, `Viewport`, and `FitAxis`.
- **CropSpec (source selection)** captures how to carve the source with `Strategy` (`auto | anchor | focus | manual`), optional `Ratio` override, `Zoom`, `Extent`, `Focus`, `Anchor`, `Pan`, and `Units`.
- **PresentationSpec (post-crop tweaks)** holds `UserScale`, `OffsetPx`, and `ClampToCanvas` to nudge after the crop math is done.

`InputsFromRequest` converts this to `NormalizedInputs` (`FrameInputs`, `CropInputs`, `PresentationInputs`, `SourceMeta`) which mirrors the internal order of operations.

## Package Structure

Each file in the package has a clear role. Use this as a map when chasing behavior:
- `types.go` defines the public structs (Rect, ImageMeta, legacy ViewportSettings, LayoutRequest, ViewportResult, Trace).
- `defaults.go` supplies sane defaults for both modern and legacy inputs.
- `engine/inputs.go` normalizes requests (`InputsFromRequest`) and adapts legacy settings.
- `engine/normalized_inputs.go` groups internal structs for frame, crop, presentation, and source.
- `engine/engine.go` runs `ComputeViewport` and its helpers (`buildFrame`, `resolveCrop`, `composeTarget`).
- `engine/analysis.go` offers inspection helpers (`AnalyzeFrame`, `AnalyzeCrop`, `AnalyzePresentation`).
- Tests live in `engine_test.go` and `inputs_test.go`.
- Integration points are the CLI command, services, renderer, and TypeScript API shapes.

## Core Data Structures (selected)

The core types are intentionally small so they can be reasoned about without a long mental stack.

`Rect` is the primitive geometry:
```4:9:zine-layout/pkg/imagelayout/types.go
type Rect struct {
	X float64 `json:"x"`
	Y float64 `json:"y"`
	W float64 `json:"w"`
	H float64 `json:"h"`
}
```

`LayoutRequest` is the modern recipe:
```60:90:zine-layout/pkg/imagelayout/types.go
type LayoutRequest struct {
	Frame        FrameSpec        `json:"frame"`
	Crop         CropSpec         `json:"crop"`
	Presentation PresentationSpec `json:"presentation"`
	Export       ExportOptions    `json:"export"`
}
```

Legacy `ViewportSettings` persists for backward compatibility and CLI flags. Prefer `LayoutRequest` for new code.

## Algorithm Overview

The engine runs in two deliberate phases. First, it validates and reshapes inputs into normalized structs. Second, it performs the math that crops, scales, and places the image. Keeping these steps separate makes it easier to debug and to extend.

1. **Normalization** (`InputsFromRequest`): Validate image dims and request; build grouped inputs (Frame/Crop/Presentation + SourceMeta).
2. **Viewport computation** (`ComputeViewport`): Build canvas, choose crop ratio, crop source, position, scale, apply presentation offsets, emit `ViewportResult` and optional `Trace`.

### Flow Diagram (modern path)

```mermaid
flowchart TD
    Start([Start: LayoutRequest + ImageMeta]) --> Validate{Validate Inputs}
    Validate -->|Invalid| Error([Error])
    Validate -->|Valid| Normalize[InputsFromRequest]

    Normalize --> BuildFrame[Build FrameInputs]
    BuildFrame --> FrameMode{Frame.Mode?}
    FrameMode -->|page| PageMode[Canvas = Page × DPI<br/>Content = Canvas - Margins]
    FrameMode -->|ratio| RatioMode[Canvas = Ratio × SourceH<br/>No Margins]
    FrameMode -->|viewport| ViewportMode[Canvas = Viewport dims<br/>No Margins]
    PageMode --> Orientation{Orientation?}
    Orientation -->|landscape| SwapDims[Swap W/H]
    Orientation -->|portrait| BuildCrop
    SwapDims --> BuildCrop
    RatioMode --> BuildCrop
    ViewportMode --> BuildCrop

    BuildCrop[Build CropInputs] --> BuildPresentation[Build PresentationInputs]
    BuildPresentation --> Compute[ComputeViewport]

    Compute --> FrameHelper[buildFrame:<br/>Canvas/Content Rect]
    FrameHelper --> RatioPick{Requested Ratio}
    RatioPick -->|Crop.Ratio| UseCropRatio[Use Crop.Ratio]
    RatioPick -->|Fill=cover| UseTargetRatio[Use Target Ratio]
    RatioPick -->|else| UseSourceRatio[Use Source Ratio]
    UseCropRatio --> CropCalc[resolveCrop]
    UseTargetRatio --> CropCalc
    UseSourceRatio --> CropCalc

    CropCalc --> Compare{Source vs Requested}
    Compare -->|Source > Requested| CropWidth[Trim width]
    Compare -->|Source < Requested| CropHeight[Trim height]
    Compare -->|=| NoCrop[Keep full]
    CropWidth --> Zoom[Apply Zoom/Extent]
    CropHeight --> Zoom
    NoCrop --> Zoom

    Zoom --> Position{Strategy}
    Position -->|focus| FocusCalc[Align source→target point]
    Position -->|anchor| AnchorCalc[Preset pan -1..1]
    Position -->|manual| ManualCalc[Direct pan]
    Position -->|auto| AutoCalc[Center]

    FocusCalc --> ScaleCalc[composeTarget]
    AnchorCalc --> ScaleCalc
    ManualCalc --> ScaleCalc
    AutoCalc --> ScaleCalc

    ScaleCalc --> FillMode{Fill?}
    FillMode -->|contain| MinScale[scale = min(x,y)]
    FillMode -->|cover| MaxScale[scale = max(x,y)]
    MinScale --> Present[Apply UserScale + OffsetPx]
    MaxScale --> Present
    Present --> Result([ViewportResult:<br/>SourceRect, TargetRect,<br/>CanvasRect, Scale, Mode])

    style Start fill:#e1f5ff
    style Result fill:#c8e6c9
    style Error fill:#ffcdd2
    style BuildFrame fill:#fff3e0
    style BuildCrop fill:#fff3e0
    style BuildPresentation fill:#fff3e0
    style Compute fill:#fff9c4
    style FrameHelper fill:#fff9c4
    style CropCalc fill:#fff9c4
    style ScaleCalc fill:#fff9c4
```

## Input Normalization Details

Normalization is where bad inputs are rejected and ambiguous intent is resolved. The output is a set of grouped structs that downstream helpers can trust.

`InputsFromRequest` responsibilities:
- Validate source dimensions > 0.
- **Frame**: derive canvas/content based on mode:
  - `ratio`: `Canvas.W = Ratio * sourceH`, `Canvas.H = sourceH`.
  - `page`: convert inches × DPI, handle orientation swap, compute margins in px.
  - `viewport`: use provided px; derive missing dimension from ratio when needed.
- **Crop**: pick requested ratio (crop override > target ratio > source ratio), clamp zoom/extent, normalize units (px vs normalized).
- **Presentation**: default user scale to 1, offsets to 0, clamp flags.
- Emit grouped structs (`FrameInputs`, `CropInputs`, `PresentationInputs`, `SourceMeta`) to reduce illegal combos.

Legacy adapter `InputsFromSettings` mirrors the above for `ViewportSettings`; keep for older CLI/specs.

## Viewport Computation

Computation takes the trusted normalized inputs and turns them into concrete rectangles and a scale factor. Each helper does one job so the trace stays readable.

`ComputeViewport(inp NormalizedInputs) (ViewportResult, *Trace)` orchestrates:
1. `buildFrame`: produce `canvasRect` and `contentRect` (page mode keeps margins separately).
2. `resolveCrop`: compute requested ratio, compare with source ratio, trim width/height, apply zoom/extent, apply positioning strategy to set pan.
3. `composeTarget`: compute scale factors (`scaleX = targetW/srcW`, `scaleY = targetH/srcH`), pick min/max based on fill, apply `Presentation.UserScale`, place `TargetRect` with `OffsetPx`, return `Mode` (`cover|contain`).

Helpers in `engine.go` include `safeDiv` (avoid div-by-zero) and `clampFloat`.

### Trace interpretation

`Trace` steps (from `engine.go`) capture intermediate values for debugging:
- `"crop"`: requested ratio, source ratio, trimmed source rect, zoom/extent, chosen pan (strategy, anchor/focus/manual values).
- `"scale"`: pre/post user scale factors, target dimensions before/after offsets, fill mode selection.
- `"result"`: final `SourceRect`, `TargetRect`, `CanvasRect`, `Scale`, and `Mode`.

When triaging a bug, check `"crop"` first (ratio and pan), then `"scale"` (fill vs contain), then `"result"` (final placement).

## Frame Modes

Each frame mode answers “how big is the box we are filling?” The choice drives canvas math and whether margins exist.

### Ratio
- **Purpose**: aspect-ratio driven (web/editor).
- **Canvas**: `W = Ratio × sourceH`, `H = sourceH`; no margins.
- **Use**: previews, responsive boxes.
- **Example**:
```go
req := imagelayout.LayoutRequest{
    Frame: imagelayout.FrameSpec{
        Mode:  "ratio",
        Ratio: floatPtr(16.0 / 9.0),
        Fill:  "contain",
    },
}
```

### Page
- **Purpose**: print/PDF with margins and DPI.
- **Canvas**: `(widthIn, heightIn) × DPI`; swap dims for landscape if set.
- **Content**: canvas minus margins (pixels).
- **Use**: printable pages.
- **Example**:
```go
req := imagelayout.LayoutRequest{
    Frame: imagelayout.FrameSpec{
        Mode: "page",
        Page: &imagelayout.PageFrame{
            WidthIn: 8.5, HeightIn: 11, DPI: 300, Orientation: "portrait",
            MarginsIn: imagelayout.BoxSpacing{Top: 0.25, Right: 0.25, Bottom: 0.25, Left: 0.25},
        },
    },
}
```

### Viewport
- **Purpose**: explicit pixel canvas (web/editor).
- **Canvas**: px provided; derive missing dimension if ratio present.
- **Use**: fixed-size or responsive canvases.
- **Example**:
```go
req := imagelayout.LayoutRequest{
    Frame: imagelayout.FrameSpec{
        Mode: "viewport",
        Viewport: &imagelayout.ViewportFrame{Width: 1920, Height: 0},
        Ratio: floatPtr(3.0 / 2.0),
    },
}
```

## Scaling Modes

Scaling decides whether the entire crop stays visible or the target is fully filled.
- **Contain** (`Fill=contain`): scale = `min(scaleX, scaleY)`; letterboxes if needed; result `Mode="contain"`.
- **Cover** (`Fill=cover`): scale = `max(scaleX, scaleY)`; ensures full cover; result `Mode="cover"`.
- `Presentation.UserScale` multiplies the chosen scale; `OffsetPx` shifts final placement.

## Positioning Strategies

Positioning chooses where the crop sits within the frame. Use presets for quick alignment or focus/manual modes for precision.
- **auto**: center pan (0,0).
- **anchor**: preset pans (`center`, `top-left`, `top`, `top-right`, `left`, `right`, `bottom-left`, `bottom`, `bottom-right`).
- **focus**: align `(SourceX, SourceY)` in source to `(TargetX, TargetY)` normalized in target.
- **manual**: direct pan via `Pan` (normalized or px depending on `Units`).

Anchor presets map to normalized pans:

| Preset         | Pan (X, Y) |
|----------------|------------|
| center         | ( 0, 0 )   |
| top-left       | (-1,-1)    |
| top            | ( 0,-1)    |
| top-right      | ( 1,-1)    |
| left           | (-1, 0)    |
| right          | ( 1, 0)    |
| bottom-left    | (-1, 1)    |
| bottom         | ( 0, 1)    |
| bottom-right   | ( 1, 1)    |

Focus-point example (align a face):
```go
// 4000x3000 source, target wants the face around 30% from top, centered horizontally
req.Crop = imagelayout.CropSpec{
    Strategy: "focus",
    Focus: &imagelayout.FocusPoint{
        SourceX: 2100, SourceY: 900, // pixel in source
        TargetX: 0.5,  TargetY: 0.3, // normalized target position
    },
}
```
The crop offset is adjusted so `(2100,900)` in the source lands at `(50%,30%)` of the target box before scaling.

## API Surface (summary)

Most consumers touch only a handful of entry points. Defaults exist for both the modern and legacy paths, and analysis helpers surface internals when debugging.
- `DefaultLayoutRequest() LayoutRequest`: modern defaults.
- `DefaultSettings() ViewportSettings`: legacy defaults.
- `InputsFromRequest(req LayoutRequest, meta ImageMeta)`: normalize modern inputs.
- `InputsFromSettings(settings ViewportSettings, meta ImageMeta)`: legacy adapter.
- `ComputeViewport(inp NormalizedInputs) (ViewportResult, *Trace)`: core math.
- `AnalyzeFrame/Crop/Presentation`: introspection helpers for debugging stages.

## Usage Examples

Examples are kept in the companion doc so they can be run and updated independently. Use them as templates for CLI specs or service payloads.
- Ratio/page/viewport requests, zoom, offsets, anchor presets, focus points.
- CLI usage: `zine-layout imagelayout compute --spec <file>` plus `layout frame|crop|presentation` stage inspection.

### Worked examples (quick reference)

Assume source 4000×3000.

- **Ratio mode, 16:9, cover, auto**  
  - Canvas: 5333×3000 (ratio × sourceH).  
  - Requested ratio: 16:9 (1.78) vs source 4:3 (1.33) → trim height.  
  - SourceRect: X=0, Y=375, W=4000, H=2250.  
  - Scale: cover uses max(scaleX=5333/4000=1.33, scaleY=3000/2250=1.33) → 1.33.  
  - TargetRect: X=0, Y=0, W=5333, H=3000.

- **Page mode, 8.5×11in @300dpi, 0.25in margins, contain**  
  - Canvas: 2550×3300; Content: 2400×3150.  
  - Requested ratio: target (2400/3150=0.76) vs source 1.33 → trim width.  
  - SourceRect: X=666, Y=0, W=2666, H=3000 (auto center).  
  - Scale (contain): min(2400/2666=0.90, 3150/3000=1.05) → 0.90.  
  - TargetRect (within content): X=0, Y=150, W=2400, H=2700 (plus margins if needed).

- **Viewport mode, 1920×1080 derived via 16:9, cover, anchor top**  
  - Canvas: 1920×1080.  
  - Requested ratio 16:9 vs source 4:3 → trim height.  
  - SourceRect: X=0, Y=0 (anchor top), W=4000, H=2250.  
  - Scale (cover): max(1920/4000=0.48, 1080/2250=0.48) → 0.48.  
  - TargetRect: X=0, Y=0, W=1920, H=1080.

## Integration Points

The same shapes and results flow through the stack: CLI for debugging, services for persistence, renderer for placement, and TS types for the web editor.
- **CLI**: `cmd/zine-layout/cmds/imagelayout/compute.go` and subcommands.
- **Service layer**: `pkg/services/layout.go` uses `InputsFromRequest`, persists `LayoutComputation` (Layout + Result + Trace).
- **Renderer**: `pkg/pagelayout/renderer/renderer.go` consumes `ViewportResult` (`SourceRect`, `TargetRect`, `CanvasRect`, `Scale`).
- **Web API**: TS types (`web/src/api.ts`) mirror `LayoutRequest`/`ViewportResult` for client/editor parity.

## Edge Cases and Validation

Validation guards against impossible layouts before math begins. Errors are intentionally specific to aid user-facing messages.
- Source dimensions must be > 0.
- Page mode: DPI > 0; margins cannot exceed canvas; orientation swap handled.
- Viewport mode: at least one of width/height/ratio provided.
- Crop ratio > 0; crop units limited to `normalized` or `px`.
- Safe division and clamping prevent NaN scale values (`engine.go`).
- Optional `ClampToCanvas` limits presentation offsets to canvas bounds.

Common validation errors (from tests):
- `dpi must be > 0`
- `content area must be positive after margins`
- `at least one of viewport width/height/ratio must be set`
- `crop ratio must be > 0`
- `invalid crop units` (anything outside `normalized|px`)

## Testing

The test suite exercises both the normalization and compute layers, covering mode combinations, scaling choices, positioning strategies, and validation failures.
- `engine_test.go`: cover ratio/page/viewport, cover vs contain, anchors, focus, offsets, margins, validation errors.
- `inputs_test.go`: normalization correctness for LayoutRequest.
- Example specs in `03-image-layout-api-usage-examples.md` can be executed via CLI to sanity-check behaviors.

## Performance Considerations

The algorithm is arithmetic-heavy and allocation-light. Traces add overhead; skip them in production paths unless you are diagnosing a layout.
- Pure arithmetic; per-call allocations minimal.
- Traces are optional; skip when not debugging.
- Margin/ratio math uses float64 for sub-pixel fidelity; downstream renderers may quantize to ints.

## Related

These documents provide adjacent context: the crop revamp rationale and the separate zine imposition engine.
- Crop revamp design + migration: `vibes/2025/11/29/photobook-app/2025/11/30/REVAMP-CROP-ALGORITHM-revamp-crop-algorithm-for-aspect-ratio-cropping/reference/01-crop-algorithm-revamp-analysis.md`.
- Zine layout engine (imposition, not single image): `02-zine-layout-algorithm-complete-analysis.md`.

## Glossary (UI ↔ API)

Use this table when mapping editor controls to request fields.
- Zoom slider → `Crop.Zoom` (>1 zooms in, <1 zooms out).
- Aspect lock → `Crop.Ratio` (overrides frame ratio when set).
- Fit/Fill toggle → `Frame.Fill` (`contain` vs `cover`).
- Anchor buttons (grid) → `Crop.Strategy="anchor"` + `Anchor`.
- Face/subject picker → `Crop.Strategy="focus"` + `FocusPoint`.
- Nudge arrows → `Presentation.OffsetPx` (pixels).
- User scale knob → `Presentation.UserScale`.
- Canvas size dropdown → `Frame.Mode` + `Frame.Page|Viewport|Ratio`.

## Next Steps

The remaining work is practical polish: expand runnable examples and include a visual trace walkthrough.
- Add more worked examples to `03-image-layout-api-usage-examples.md` for focus and mixed-units pans.
- Add a short trace-reading cheatsheet screenshot from CLI output.
