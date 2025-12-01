---
Title: FrameCrop Refactor Plan
Ticket: REVAMP-CROP-ALGORITHM
Status: active
Topics:
    - imagelayout
    - layout
    - ux
DocType: design
Intent: long-term
Owners: []
RelatedFiles: []
ExternalSources: []
Summary: ""
LastUpdated: 2025-11-30T20:45:05.992365766-05:00
---

# FrameCrop Refactor Plan

## Audience

This plan is written for a new engineer (or intern) joining the project. It assumes you can build the Go backend and React front-end, but you may not know the existing layout engine. Everything you need—context, file paths, sequencing, and testing—is documented below so you can execute the refactor independently.

---

## 1. Why We Are Doing This

| Problem | Evidence (files / UX research) | Consequence |
| --- | --- | --- |
| `ViewportSettings` mixes frame, crop, and presentation knobs in one flat structure. | `zine-layout/pkg/imagelayout/engine/engine.go`, `SequenceSlideshow.tsx`, Debate Round 17. | UI exposes 15+ controls at once and the engine has tangled branching. |
| Users reason about two things only: “what shape should the output have?” and “which part of the image should be kept?” | `.../reference/01-crop-algorithm-revamp-analysis.md` | Current API fights the mental model, causing UX confusion. |
| Engine implementation already behaves in two phases (frame ➜ crop), but inputs do not reflect that. | `ComputeViewport`: canvas rect vs source rect steps. | Hard to maintain and communicate. |

**Goal:** Replace the legacy inputs with a clean `LayoutRequest` that explicitly separates `FrameSpec`, `CropSpec`, and `PresentationSpec`. No backward-compatibility layer is required.

---

## 2. High-Level Refactor Sequence

1. **Define new structs** in Go (`LayoutRequest`, `FrameSpec`, `CropSpec`, `PresentationSpec`).
2. **Update engine normalization** to consume those structs (`buildFrame`, `resolveCrop`, `composeTarget` helpers).
3. **Adapt services + CLI** to send/receive the new JSON shape.
4. **Regenerate API types** and refactor frontend state + components around the new model.
5. **Update templates, docs, and tests** to ensure the new workflow is the only one.

You will work mostly under `zine-layout/` (Go) and `zine-layout/web/` (React).

---

## 3. Detailed Work Breakdown

### 3.1 Go Data Model & Engine

| File | Change |
| --- | --- |
| `zine-layout/pkg/imagelayout/types.go` | Remove `ViewportSettings`. Add `LayoutRequest`, `FrameSpec`, `CropSpec`, `PresentationSpec`, helpers (`PageFrame`, `ViewportFrame`, `Vec2`, `Vec2Px`). |
| `zine-layout/pkg/imagelayout/defaults.go` | Export `DefaultLayoutRequest()` returning safe defaults (ratio 4:3, crop strategy auto, presentation scale 1). |
| `zine-layout/pkg/imagelayout/engine/inputs.go` *(new)* | `InputsFromRequest(req LayoutRequest, meta ImageMeta) (Inputs, error)` that orchestrates validation and conversion. |
| `zine-layout/pkg/imagelayout/engine/engine.go` | Update `ComputeViewport` to use new `Inputs`. Remove legacy flags (`Mode`, `Units`, `AnchorPreset`). Implement helper functions: `buildFrame`, `resolveCrop`, `composeTarget`. |
| `zine-layout/pkg/imagelayout/engine/engine_test.go` | Replace fixtures with table-driven tests that cover ratio/page/viewport frames, anchor vs focus crops, manual pan/zoom, and presentation offsets. |

**Pseudocode Sketch**

```go
func buildFrame(spec FrameSpec, meta ImageMeta) (FrameInputs, error) {
    switch spec.Mode {
    case "ratio":
        ratio := coalesce(spec.Ratio, float64(meta.Width)/float64(meta.Height))
        return FrameInputs{CanvasRect: Rect{0,0,1,1}, Ratio: ratio, ScaleUnit: "ratio"}, nil
    case "page":
        canvas := pageToPixels(spec.Page)
        content := subtractMargins(canvas, spec.Page.Margins)
        return FrameInputs{CanvasRect: content, Ratio: content.W / content.H, ScaleUnit: "px"}, nil
    case "viewport":
        dims := resolveViewport(spec.Viewport)
        return FrameInputs{CanvasRect: Rect{0,0,dims.W,dims.H}, Ratio: dims.W / dims.H, ScaleUnit: "px"}, nil
    default:
        return FrameInputs{}, fmt.Errorf("unsupported frame mode")
    }
}

func resolveCrop(spec CropSpec, targetRatio float64, meta ImageMeta) CropInputs {
    window := applyZoom(meta.Rect(), spec.Zoom, spec.Extent)
    window = enforceRatio(window, targetRatio)
    switch spec.Strategy {
    case "focus":
        window = alignFocus(window, spec.Focus, meta)
    case "anchor":
        window = alignAnchor(window, spec.Anchor)
    case "manual":
        window = panWindow(window, spec.Pan)
    default: // auto
        window = centerWindow(window)
    }
    return CropInputs{SourceRect: window}
}
```

### 3.2 Services, CLI, Templates

| File | Change |
| --- | --- |
| `pkg/services/layout.go` | Update `CreateLaidOutImage` & `RecomputeLaidOutImage` to accept `LayoutRequest`. Remove legacy merging logic. |
| `pkg/services/templates/*.json` | Store `layout` objects directly (no `viewport_settings`). |
| `cmd/zine-layout/cmds/imagelayout/compute.go` | Rename CLI flags to `--frame-*`, `--crop-*`, `--presentation-*`. Add `layout_request` YAML parsing. |
| `pkg/serve/laid_out_images_routes.go` | Request bodies now include `layout LayoutRequest`. Responses echo the same. |

### 3.3 Frontend Types & Store

| File | Change |
| --- | --- |
| `web/src/api.ts` | Define `LayoutRequest`, `FrameSpec`, etc. Remove legacy interfaces. |
| `web/src/store/*` | Update slices/selectors to handle the new shape (e.g., `layoutRequest.frame.mode`). |
| `web/src/api/generated.ts` (if applicable) | Regenerate from API schema. |

### 3.4 UI: Sequence Slideshow + Editor Panels

1. **Frame Panel (`SequenceSlideshow.tsx`)**
   - Controls: mode toggle (ratio/page/viewport), ratio presets, page dimension inputs, viewport pixel fields, fill mode.
2. **Crop Panel**
   - Strategy select (auto, anchor, focus, manual).
   - Per-strategy inputs (anchor dropdown, focus reticle, pan XY sliders, zoom slider).
3. **Presentation Panel**
   - User scale slider, offset X/Y pixel inputs, “reset adjustments” button.

**Component Structure Sketch**

```tsx
const FrameControls = ({ value, onChange }: FrameSpecProps) => { ... }
const CropControls = ({ value, onChange }: CropSpecProps) => { ... }
const PresentationControls = ({ value, onChange }: PresentationSpecProps) => { ... }

export const SequenceSlideshow = () => {
  const [frame, setFrame] = useState<FrameSpec>(defaultFrame);
  const [crop, setCrop] = useState<CropSpec>(defaultCrop);
  const [presentation, setPresentation] = useState<PresentationSpec>(defaultPresentation);

  const apply = () => {
    dispatch(applyLayout({ frame, crop, presentation }));
  };

  return (
    <>
      <FrameControls value={frame} onChange={setFrame} />
      <CropControls value={crop} onChange={setCrop} />
      <PresentationControls value={presentation} onChange={setPresentation} />
      <button onClick={apply}>Apply Layout</button>
    </>
  );
};
```

### 3.5 Documentation & Samples

- Update `ZINE-LAYOUT-ANALYSIS/.../01-image-layout-algorithm-complete-analysis.md` with new struct definitions (already partially done—finish remaining sections).
- Keep `reference/01-crop-algorithm-revamp-analysis.md` in sync when code lands.
- Update CLI help text and README snippets to reference `LayoutRequest`.

---

## 4. Testing Strategy

| Layer | Tests to Add / Update |
| --- | --- |
| Engine | Unit tests for each frame mode + crop strategy; golden tests verifying `SourceRect`, `TargetRect`, and `CanvasRect`. |
| Services | Tests ensuring `CreateLaidOutImage` persists the new layout payload and returns the computed result. |
| CLI | Integration test running `go run ... imagelayout compute --frame-mode ratio --frame-ratio 1 --crop-strategy focus ...` verifying JSON output. |
| API | HTTP handler tests for POST `/projects/:id/laid-out-images` ensuring request validation + response payload shape. |
| Frontend | Jest tests for reducers/selectors, React Testing Library for control panels, Cypress (or Playwright) smoke tests covering the new workflow (toggle frame modes, move focus reticle, submit). |

**Manual QA Checklist**
1. Create laid-out image via CLI using ratio frame + focus crop.
2. Create via UI with page mode + anchor crop, confirm backend receives correct JSON.
3. Preview handles fit-to-viewport scenario without errors.

---

## 5. Risk Mitigation & Coordination

- **Templates / Data migration:** Because we are not maintaining backward compatibility, all existing template JSON must be migrated in the same PR (command-line script or manual edits). Verify by loading templates in UI and ensuring they render.
- **Frontend ↔ Backend sync:** Merge backend changes first, deploy API, then update frontend pointing to the new schema (or use feature branch where both land together).
- **Developer tooling:** Update TypeScript types and regenerate RTK Query hooks immediately after backend change to avoid drift.
- **Documentation:** Keep this ticket’s docs updated. Any new insight should go into `reference/01-crop-algorithm-revamp-analysis.md`.

---

## 6. Milestones & Suggested Order

1. **Milestone A – Engine Ready**
   - Structs defined, engine + tests green (`go test ./zine-layout/...`).
2. **Milestone B – Services + CLI**
   - REST endpoints, CLI command, templates updated. End-to-end Go tests pass.
3. **Milestone C – Frontend & UI**
   - TypeScript types updated, UI refactored, unit + integration tests pass.
4. **Milestone D – Docs & Verification**
   - Docs refreshed, manual QA run through checklist, ticket changelog updated.

Mark each milestone in `tasks.md` / `changelog.md` as you go so reviewers can follow progress.

---

## 7. Resources & Quick Links

- **Existing analysis:** `reference/01-crop-algorithm-revamp-analysis.md`
- **Engine code:** `zine-layout/pkg/imagelayout/engine/engine.go`
- **Service entrypoint:** `pkg/services/layout.go`
- **Frontend component:** `zine-layout/web/src/views/v2/components/SequenceSlideshow.tsx`
- **API routes:** `pkg/serve/laid_out_images_routes.go`

When unsure, trace through these files in this order: engine ➜ services ➜ API ➜ UI. Update docs and changelog after each meaningful chunk.
