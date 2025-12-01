---
Title: Preview backend geometry
Ticket: IMAGELAYOUT-PREVIEW
Status: active
Topics:
    - imagelayout
    - frontend
    - ux
DocType: playbook
Intent: short-term
Owners: []
RelatedFiles: []
ExternalSources: []
Summary: "Quick smoke test for the image layout preview endpoint and UI wiring"
LastUpdated: 2025-11-30T23:42:44.140037847-05:00
---

# Preview backend geometry

## Purpose

Exercise the new backend preview path that computes layout geometry without persisting and confirm the ImageLayoutsTab uses the backend response for its live preview.

## Environment Assumptions

- `zine-layout` server running locally (default base URL `http://localhost:3030`)
- At least one project with an uploaded asset to reference
- API reachable at `/api`
- Commands run from repo root

## Commands

```bash
# 1) Identify a project and asset to preview (replace IDs below)
PROJECT_ID="<project-id>"
ASSET_ID="<asset-id>"

# 2) Craft a LayoutRequest payload (saves to a temp file for reuse)
cat > /tmp/imagelayout-preview.json <<'EOF'
{
  "layout": {
    "frame": {
      "mode": "page",
      "fill": "cover",
      "page": {
        "width_in": 8.0,
        "height_in": 10.0,
        "dpi": 300,
        "orientation": "portrait",
        "margins_in": { "top": 0.5, "right": 0.5, "bottom": 0.5, "left": 0.5 }
      }
    },
    "crop": { "strategy": "auto", "ratio": null, "zoom": 1, "pan": { "x": 0, "y": 0 }, "units": "normalized" },
    "presentation": { "user_scale": 1, "offset_px": { "x": 0, "y": 0 }, "clamp_to_canvas": false },
    "export": { "format": "png", "quality": 90, "background": "white", "filename_template": "{name}-{panel}.{ext}", "out_dir": "./out" }
  },
  "asset_id": "'${ASSET_ID}'"
}
EOF

# 3) Call the backend preview endpoint (returns geometry + trace, no persistence)
curl -s -X POST "http://localhost:3030/api/projects/${PROJECT_ID}/image-layout/preview" \
  -H "Content-Type: application/json" \
  --data @/tmp/imagelayout-preview.json | jq .

# 4) Verify the UI uses the same endpoint:
#    - Open the web app, navigate to Image Layouts, start creating/editing a template.
#    - Pick the same asset for preview.
#    - Tweak frame/crop settings and confirm network calls hit /image-layout/preview.
#    - The overlay should match the target_rect/canvas_rect from the curl response.
```

## Exit Criteria

- `POST /api/projects/{projectId}/image-layout/preview` returns HTTP 200 with `result.target_rect`, `result.source_rect`, and `result.scale`.
- Errors for invalid input (bad DPI, missing asset) surface as JSON `error` messages.
- In the UI, changing frame/crop values triggers the preview request and updates the overlay; no client-side only transforms remain.

## Notes

- The endpoint accepts either `asset_id` or explicit `image.width`/`image.height` in the body; asset lookup enforces project ownership.
- No layout or preview image is persisted—this path only returns geometry.
