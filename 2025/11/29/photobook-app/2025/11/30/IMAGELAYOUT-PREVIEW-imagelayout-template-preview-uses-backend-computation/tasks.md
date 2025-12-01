# Tasks

## TODO

- [x] Wire ImageLayoutsTab preview to backend computation: send current LayoutRequest + asset meta to a preview endpoint and display returned geometry.
- [x] Add API route/client hook for previewing LayoutRequest without persisting (returns ViewportResult + trace).
- [x] Validate form state before preview call; surface errors inline (e.g., missing ratio, invalid DPI).
- [x] Update UI to show backend-computed rectangles/scale in the preview overlay, not just CSS transforms.
- [x] Add playbook entry to exercise preview API from CLI and UI.
- [x] Update docs and changelog after implementation.

## Done

- [x] Initial ticket created
- [x] Add backend preview endpoint: accept LayoutRequest + image dims; return ViewportResult + Trace without persisting.
- [x] Add RTK Query endpoint/hook to call preview API from ImageLayoutsTab (debounced).
- [x] Wire ImageLayoutsTab preview pane to backend result: show backend-computed SourceRect/TargetRect/Scale; surface validation errors inline.
- [x] Debounce preview requests and add form validation before sending (ratio, DPI, required fields).
- [x] Add playbook entry: CLI curl/go run preview endpoint with sample LayoutRequest to sanity-check responses.
- [x] Update docs/changelog after implementation; include backend endpoint, UI wiring, and preview behavior.
- [x] Implement render endpoint: POST /api/projects/{id}/image-layout/render returning rendered image bytes for given layout + asset/meta.
- [x] Refactor shared crop widget component so template preview and laid-out image creation use the same controls + state mapping.
- [x] Wire render action + compare modal in ImageLayoutsTab: trigger render, show thumbnail, open side-by-side compare (preview vs render).
- [x] Update docs/playbook to cover render/compare flow and validation steps.
