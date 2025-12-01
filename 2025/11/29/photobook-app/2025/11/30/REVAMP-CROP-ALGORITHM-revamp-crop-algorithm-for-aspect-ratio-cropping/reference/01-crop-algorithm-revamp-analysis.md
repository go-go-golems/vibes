---
Title: Crop Algorithm Revamp Analysis
Ticket: REVAMP-CROP-ALGORITHM
Status: active
Topics:
    - imagelayout
    - layout
    - ux
DocType: reference
Intent: long-term
Owners: []
RelatedFiles:
    - Path: ../../../../../../../../../../zine-layout/pkg/imagelayout/engine/engine.go
      Note: Current InputsFromSettings + ComputeViewport implementation
    - Path: ../../../../../../../../../../zine-layout/web/src/api.ts
      Note: TS layout types now include LayoutRequest; frontend migration in progress
    - Path: ../../../../../../../../../../zine-layout/web/src/views/v2/components/SequenceSlideshow.tsx
      Note: UI currently exposing the complex set of layout knobs
ExternalSources: []
Summary: Explains why the existing ViewportSettings inputs are confusing and proposes a two-layer FrameSpec + CropSpec design to simplify both UI and engine.
LastUpdated: 2025-11-30T20:30:41.037405182-05:00
---


# Crop Algorithm Revamp Analysis

## Goal

- Document why the current `ViewportSettings` structure is difficult to reason about for both UI authors and engine consumers.
- Capture a clear separation between **frame selection** (the output shape we are cutting to) and **crop selection** (how we cut that shape out of the source image).
- Provide a concrete design proposal (`FrameSpec`, `CropSpec`, `PresentationSpec`) that the Go engine and the UI can adopt without duplicating math.

## Context

- The existing reference doc (`ZINE-LAYOUT-ANALYSIS/01-image-layout-algorithm-complete-analysis.md`) thoroughly explains the current engine, but the input knobs remain overwhelming because everything is mixed into `ViewportSettings`.
- UX feedback (see `ZINE-APP-DESIGN/debate-round-17-image-layout-ux-api.md`) shows photographers only think about two things:
  1. **What shape should the final image occupy?** (pure ratio, page-with-margins, fit-to-box)
  2. **Which part of the original image should survive that crop?** (zoom, pan, anchoring, focus point)
- Implementation reality (`zine-layout/pkg/imagelayout/engine/engine.go`) already performs two distinct phases (target frame computation, source crop computation) yet the API exposes every knob at once, confusing UI flows like `SequenceSlideshow`.

## Quick Reference

### Observations from Current Implementation

| Pain Point | Evidence | Impact |
| --- | --- | --- |
| Frame + crop + presentation fields are interleaved in `ViewportSettings`. | `InputsFromSettings` juggles `Paper*`, `Margin*`, `CropRatio`, `CropWidthPx`, `FitWidthPx`, `UserScale`, `Position*`, `Focus` in one struct. | UI has to show 15+ controls simultaneously even though only 2-3 are meaningful per workflow. |
| Users expect "pick an aspect ratio, then move/zoom the image inside that shape". | Debate Round 17, and designers prototyping SequenceSlideshow. | We force them to understand modes (`page`, `crop`, `fit`) even when they just want "square crop anchored on a face". |
| Engine already computes canvas separately from crop. | `ComputeViewport` lines 265-381 clearly separate `canvasRect` (`Frame`) from `sourceRect` (`Crop`). | Renaming the inputs to mirror the two phases removes most confusion. |

### Proposed Data Model

```
type LayoutRequest struct {
    Frame        FrameSpec        // chooses the target aspect ratio / box
    Crop         CropSpec         // chooses how to cut the source to match the frame
    Presentation PresentationSpec // optional: scaling + user zoom after frame crop
}
```

| Layer | Purpose | Key Fields | Maps to legacy fields |
| --- | --- | --- | --- |
| `FrameSpec` | Defines the output box/aspect ratio before any source crop. | `Mode` (`ratio | page | viewport`), `Ratio` (numeric), `Page` (`width_in`, `height_in`, `margin_in`, `dpi`), `ViewportPx` (`width_px`, `height_px`). | `Mode`, `Paper*`, `Margin*`, `CropToFill` (frame fill), `Fit*`. |
| `CropSpec` | Defines which portion of the source image is retained. | `Strategy` (`focus | anchor | manual | auto`), `Zoom` (scale factor), `Pan` (`x`, `y` normalized), `Focus` (`source`, `target`), `Extent` (percentage of source to keep). | `CropRatio`, `Position*`, `Units`, `Focus`, (implicit from `CropWidthPx`,`CropHeightPx`). |
| `PresentationSpec` | Optional adjustments applied after `FrameSpec`/`CropSpec`. | `UserScale`, `OffsetPx`, `ClampToCanvas` flags. | `UserScale`, `Position*` when `Units == px`. |

**Design Principle:** UI only surfaces either `FrameSpec` *or* `CropSpec` controls at a time. For example, choosing "Square" locks `FrameSpec.Ratio=1.0` and reveals `CropSpec` sliders (pan/zoom). Choosing "Page center" highlights `FrameSpec.Page` inputs (paper size + margins) and allows the engine to compute the exact aspect ratio automatically.

### Algorithm Outline (Pseudo-code)

```
frame := BuildFrame(FrameSpec)         // returns canvas rect + requested ratio
crop  := ResolveCrop(frame.Ratio, CropSpec, SourceMeta)
dst   := Compose(frame.CanvasRect, crop.SourceRect, PresentationSpec)
return ViewportResult{SourceRect: crop.SourceRect, TargetRect: dst.TargetRect, CanvasRect: frame.CanvasRect}
```

1. **BuildFrame** – decides the target ratio without touching the source:
   - `ratio` mode → use `FrameSpec.Ratio`.
   - `page` mode → compute `contentRect` from paper size/margins, ratio = `contentW / contentH`.
   - `viewport` mode → take exact pixel dims (web canvas).
2. **ResolveCrop** – takes the requested ratio and finds a source rectangle with the same aspect:
   - Start with full image.
   - Apply `Zoom` (smaller `Extent` ⇒ tighter crop).
   - Apply `Pan` or `Focus` to position the crop window.
3. **Compose** – scale to the canvas, apply optional user scale/offset, emit `TargetRect`.

> This mirrors the existing engine math but exposes human-friendly groupings. No new math is required initially; we only reorganize inputs and rename UI controls.

### Migration Strategy

1. Introduce `FrameSpec`, `CropSpec`, `PresentationSpec` Go structs and conversion helpers from the legacy `ViewportSettings`.
2. Update frontend API types (`web/src/api.ts`) to match the new structs.
3. Gradually deprecate legacy fields by generating them from the structured request (e.g., `FrameSpec.Page` → existing `Paper*` fields) to keep backwards compatibility.

## Usage Examples

### 1. Square crop anchored on a face

```json
{
  "frame": { "mode": "ratio", "ratio": 1.0 },
  "crop": {
    "strategy": "focus",
    "focus": { "source_x": 1820, "source_y": 980, "target_x": 0.5, "target_y": 0.4 },
    "zoom": 1.15
  },
  "presentation": { "user_scale": 1.0 }
}
```

- UI shows a simple "Square" toggle and a draggable focus reticle.
- Engine derives `requestedRatio=1.0`; `zoom` shrinks the crop window before aligning to the focus point.

### 2. Full-bleed page layout with margins

```json
{
  "frame": {
    "mode": "page",
    "page": { "width_in": 8.5, "height_in": 11, "dpi": 300 },
    "margins_in": { "top": 0.25, "right": 0.25, "bottom": 0.25, "left": 0.25 },
    "fill": "cover"
  },
  "crop": { "strategy": "anchor", "anchor": "top-left" },
  "presentation": { "user_scale": 1.0 }
}
```

- Designers specify physical media; the frame builder outputs the content rectangle ratio automatically.
- Crop strategy simply states "align top-left" instead of juggling `PositionX/Y` + `AnchorPreset`.

### 3. Fit-to-width preview in the web editor

```json
{
  "frame": { "mode": "viewport", "viewport_px": { "width": 1600, "height": 0 }, "fit_axis": "width" },
  "crop": { "strategy": "auto" },
  "presentation": { "user_scale": 0.9, "offset_px": { "x": 0, "y": -40 } }
}
```

- Equivalent to today’s `fit` mode (width). Height is derived from the source ratio.
- `presentation.offset_px` replaces the confusing normalized `PositionX/Y` when designers simply need to nudge the rendered preview.

## Related

- `vibes/2025/11/29/photobook-app/2025/11/30/ZINE-LAYOUT-ANALYSIS-image-layout-algorithm-analysis/reference/01-image-layout-algorithm-complete-analysis.md` — canonical algorithm deep dive (must be updated to reference the Frame/Crop split).
- `vibes/2025/11/29/photobook-app/2025/11/30/ZINE-APP-DESIGN-zine-app-design-sequencing-and-layout-application/reference/debate-round-17-image-layout-ux-api.md` — UX debate that surfaced the problem.
