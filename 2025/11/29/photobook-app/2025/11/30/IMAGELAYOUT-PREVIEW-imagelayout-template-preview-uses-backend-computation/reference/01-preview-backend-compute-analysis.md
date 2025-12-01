---
Title: Imagelayout Preview Backend Compute Analysis
Ticket: IMAGELAYOUT-PREVIEW
Status: active
Topics:
    - imagelayout
    - frontend
    - ux
DocType: reference
Intent: short-term
Owners: []
RelatedFiles:
    - Path: ../../../../../../../../../../zine-layout/pkg/imagelayout/engine/engine.go
      Note: ComputeViewport / trace
    - Path: ../../../../../../../../../../zine-layout/pkg/imagelayout/engine/inputs.go
      Note: InputsFromRequest normalization
    - Path: ../../../../../../../../../../zine-layout/pkg/serve/layout_preview_routes.go
      Note: Project-scoped preview endpoint (POST /image-layout/preview)
    - Path: ../../../../../../../../../../zine-layout/pkg/services/layout.go
      Note: Service layer using InputsFromRequest + PreviewLayout helper
    - Path: ../../../../../../../../../../zine-layout/web/src/api.ts
      Note: RTK Query preview mutation wiring (previewLayoutRequest)
    - Path: ../../../../../../../../../../zine-layout/web/src/views/tabs/ImageLayoutsTab.tsx
      Note: Debounced preview UI that renders backend canvas/target rects
    - Path: 2025/11/29/photobook-app/2025/11/30/ZINE-LAYOUT-ANALYSIS-image-layout-algorithm-analysis/reference/01-image-layout-algorithm-complete-analysis.md
      Note: Algorithm reference
ExternalSources: []
Summary: How to shift imagelayout template preview to backend-computed geometry while keeping client-side rendering lightweight
LastUpdated: 2025-11-30T23:43:27-05:00
---


# Imagelayout Preview Backend Compute Analysis

## Goal
Move the template editor preview to use backend-computed geometry (LayoutRequest → ComputeViewport) so that every UI change is validated and returns canonical rectangles/scale without rendering pixels on the server.

## Current Behavior
- `ImageLayoutsTab` builds a `LayoutRequest` client-side and renders a static preview via CSS transforms.
- No backend call is made when toggling aspect ratio/fill/crop; errors aren’t surfaced until save/apply.
- Backend already exposes ComputeViewport via services; laid-out images persist LayoutRequest + result.

## Desired Behavior
- On each form change, send the current `LayoutRequest` + asset dimensions to a lightweight preview endpoint.
- Receive `ViewportResult` + `Trace` and use it to render the preview overlay (rectangles/scale) client-side.
- Validate inputs server-side; surface errors inline (e.g., invalid ratio/DPI).
- Avoid server image rendering; only compute geometry.

## Key Concepts
- **LayoutRequest**: frame/crop/presentation split; see algorithm reference doc.
- **InputsFromRequest → ComputeViewport**: canonical pipeline to derive `ViewportResult` + `Trace`.
- **Preview endpoint**: thin handler that accepts LayoutRequest + ImageMeta and returns the computation without persisting.

## Frontend touchpoints
- `web/src/views/tabs/ImageLayoutsTab.tsx`: form state + preview pane. Needs to call the preview endpoint on change and render returned rectangles.
- `web/src/api.ts`: add RTK Query mutation/query for preview (e.g., `previewLayoutRequest`), expecting `ViewportResult` + `Trace`.

## Backend touchpoints
- `pkg/services/layout.go` (or new handler): expose non-persisting preview route that reuses `InputsFromRequest` + `ComputeViewport`.
- `cmd/zine-layout/cmds/imagelayout`: CLI already exposes compute/inspect; can drive the same code path in a handler.

## Risks / considerations
- Avoid flooding backend: debounce client calls; maybe on blur/idle.
- Ensure consistent validation messages between UI and CLI.
- Keep payload small: send asset width/height, not the image.

## Next steps
- Update docs/playbook; add tests to ensure preview endpoint validates inputs.
- Wire validation errors into UI copy and onboarding docs.
- Consider trace logging/inspection toggle in the UI to help debug layouts.

## Implementation snapshot
- Backend: `POST /api/projects/{projectId}/image-layout/preview` calls `LayoutService.PreviewLayout` (LayoutRequest + asset_id or inline image meta) and returns `ViewportResult` + `Trace` without persisting.
- Frontend API: `usePreviewLayoutRequestMutation` posts the current `LayoutRequest` with the selected preview asset id.
- UI: `ImageLayoutsTab` debounces form changes, surfaces server validation errors, and renders the backend `canvas_rect/target_rect` overlay instead of a client-only transform.
