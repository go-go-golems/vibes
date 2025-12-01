# Changelog

## 2025-11-30

- Initial workspace created


## 2025-11-30

Captured bug: ImageLayoutsTab preview stays client-only; needs backend compute. Added reference analysis doc pointing to engine (ComputeViewport), services, frontend files, and the main imagelayout algorithm doc. Created tasks for backend preview endpoint, RTK Query hook, UI wiring with validation, debounce, and playbook.


## 2025-11-30

Added POST /api/projects/{id}/image-layout/preview using LayoutService.PreviewLayout; accepts LayoutRequest + asset_id/image meta and returns ViewportResult + Trace without persisting.


## 2025-11-30

Hooked ImageLayoutsTab preview to backend RTK mutation with debounce + inline errors; overlay now renders backend canvas/target rectangles; documented curl/UI steps in playbook/01-preview-backend-geometry.md and refreshed analysis doc.

