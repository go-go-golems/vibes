---
Title: Image layout preview & render UI spec
Ticket: IMAGELAYOUT-PREVIEW
Status: active
Topics:
    - imagelayout
    - frontend
    - ux
DocType: design
Intent: short-term
Owners: []
RelatedFiles:
    - Path: ../../../../../../../../../../zine-layout/pkg/serve/layout_preview_routes.go
      Note: Preview endpoint
    - Path: ../../../../../../../../../../zine-layout/pkg/services/layout.go
      Note: PreviewLayout helper
    - Path: ../../../../../../../../../../zine-layout/web/src/api.ts
      Note: RTK preview mutation + new render endpoint to add
    - Path: ../../../../../../../../../../zine-layout/web/src/views/tabs/ImageLayoutsTab.tsx
      Note: Template + laid-out image UI wiring
ExternalSources: []
Summary: "UI/UX spec for shared crop widget, preview geometry, backend render, and compare modal across template editor and laid-out image creation."
LastUpdated: 2025-11-30T23:53:47-05:00
---


# Image layout preview & render UI spec

## Goal
Provide a unified UI for template editing and laid-out image creation that:
- Uses backend geometry for live preview, with the same crop widget across contexts.
- Adds a backend render view to validate actual output.
- Adds a side-by-side compare modal (preview geometry vs rendered image) to catch render/placement drift.

## Screens (ASCII)

### Template Editor (Frame + Preview/Crop + Render)
```
+-------------------------------------------------------------------+
| Edit Template: 8x10 Portrait         [Save Template] [Cancel]     |
+----------------- Frame (defines target box) ----------------------+
| Mode: (• Page ○ Ratio ○ Viewport)                                 |
|   Page: [Width 8.0 in] [Height 10.0 in] [DPI 300]                 |
|   Orientation: (• Portrait ○ Landscape)                           |
| Margins: (Uniform □) Top [0.5] Right [0.5] Bottom [0.5] Left [0.5]
| Fill intent default: (• Cover ○ Contain)                          |
| Target box (after margins): 7.0 x 9.0 in  | Aspect: 0.78          |
+--------------- Preview & Crop (per-image sandbox) ----------------+
| Reference Asset: [portrait_001.jpg ▼]  Source: 4000 x 6000        |
| Target: 7.0 x 9.0 in @300 DPI (from frame)                        |
|---------------------------------------------------------------    |
| [ Preview canvas showing target box; draggable crop ]             |
| Crop widget (same as laid-out image):                             |
|   Strategy: (• Auto center  ○ Anchor preset  ○ Manual pan)        |
|   Anchor: [Middle-Center ▼] (if Anchor)                           |
|   Pan X: [-0.20]  Pan Y: [0.05] (if Manual)                       |
|   Zoom: [1.10]                                                    |
|   Offset X: [0 px]  Offset Y: [0 px]                              |
|   Clamp: [x] Keep inside target box                               |
| [Reset to auto] [Apply to preview]                                |
+------------------- Render & Compare -----------------------------+
| [Render backend image]  (uses current frame + crop as if laid-out)|
| Render status: [ Ready / Rendering... / Error: ... ]             |
| Rendered thumbnail: [ small backend-rendered image box ]          |
| [Open Compare Modal]                                             |
+-------------------------------------------------------------------+
```

### Create / Edit Laid-Out Image (same crop widget + render/compare)
```
+-------------------------------------------------------------+
| Create Laid-Out Image                                       |
+-------------------------------------------------------------+
| Template: [8x10 Portrait ▼]  Target: 7.0 x 9.0 in (0.78)    |
| Asset:    [portrait_001.jpg ▼] Source: 4000 x 6000          |
+---------------- Crop to Template ---------------------------+
| [ Same crop widget as template preview; persisted here ]    |
|   Strategy / Anchor / Manual pan / Zoom / Offsets / Clamp   |
| [Reset to template default]    [Save Placement]             |
+---------------- Render & Compare ---------------------------+
| [Render backend image]   Render status: [Ready/Rendering…]  |
| Rendered thumbnail: [backend image]                         |
| [Open Compare Modal]                                        |
+-------------------------------------------------------------+
```

### Compare Modal (side-by-side)
```
+-------------------------------------------------------------+
| Compare Preview vs Render        [Close]                    |
+-------------------------------------------------------------+
| Left: Preview (frontend overlay from geometry)              |
| [ target box with image positioned per backend geometry ]   |
|-------------------------------------------------------------|
| Right: Backend Render                                       |
| [ actual rendered image returned by backend ]               |
+-------------------------------------------------------------+
| Notes:                                                      |
| - Verify alignment, scaling, and clipping match.            |
| - If mismatch, check crop settings, DPI, and margins.       |
+-------------------------------------------------------------+
```

## Widgets and props
- Frame form (template editor only):
  - Mode: `ratio | page | viewport`
  - Page: `width_in`, `height_in`, `dpi`, `orientation`
  - Margins: `top/right/bottom/left`, uniform toggle
  - Fill intent default: `cover | contain`
  - Derived target box/aspect: computed, displayed read-only
- Crop widget (shared):
  - Strategy: `auto | anchor | manual`
  - Anchor preset (when anchor): named anchor key (e.g., `middle-center`)
  - Pan X/Y (manual): floats (-1..1 normalized)
  - Zoom: float (>0)
  - Offsets: `offset_px.x/y` (presentation)
  - Clamp: boolean (clamp to canvas)
  - Reset: restore to template defaults (in laid-out flow) or auto (in template sandbox)
- Preview panel:
  - Renders backend geometry: `ViewportResult` (canvas_rect, target_rect, source_rect, scale, mode)
  - Uses reference asset (template sandbox) or current asset (laid-out flow)
- Render panel:
  - Triggers backend render endpoint; shows status + thumbnail
  - Opens compare modal
- Compare modal:
  - Left: preview overlay using `ViewportResult`
  - Right: rendered bitmap returned by backend

## API calls (symbols/files)
- Geometry preview (existing):
  - Frontend hook: `usePreviewLayoutRequestMutation` in `zine-layout/web/src/api.ts`
  - Backend: `POST /api/projects/{projectId}/image-layout/preview` in `zine-layout/pkg/serve/layout_preview_routes.go` → `LayoutService.PreviewLayout` (`zine-layout/pkg/services/layout.go`)
- Render (new, to add):
  - Suggested route: `POST /api/projects/{projectId}/image-layout/render` with `layout`, `asset_id|image`, returns rendered image (png/jpeg) + maybe geometry.
  - Handlers should reuse layout pipeline + renderer (see `zine-layout/pkg/imagelayout/engine`, `pkg/export` if available) and stream bytes.
- Compare modal data flow:
  - Preview side: reuse `ViewportResult` from preview call.
  - Render side: use response from render endpoint (URL/blob).

## Implementation notes
- Keep template saves limited to frame + default fill intent; crop widget in template editor is a sandbox/seed, not persisted per asset.
- In laid-out image flow, persist crop/pan/zoom/offsets per asset.
- Debounce preview calls; allow render calls on demand (button).
- Surface validation errors inline (DPI, dimensions, invalid ratios).
- Highlight target aspect and derived box whenever margins/size change.

## Related files to update
- Template UI: `zine-layout/web/src/views/tabs/ImageLayoutsTab.tsx`
- API layer: `zine-layout/web/src/api.ts`
- Backend preview: `zine-layout/pkg/serve/layout_preview_routes.go`, `zine-layout/pkg/services/layout.go`
- Backend render (new): add route + handler under `zine-layout/pkg/serve/` and service function
- Compare modal component (new): `zine-layout/web/src/components/` (to create)
