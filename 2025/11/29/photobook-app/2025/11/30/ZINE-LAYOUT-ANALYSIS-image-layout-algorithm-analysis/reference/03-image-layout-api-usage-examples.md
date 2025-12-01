---
Title: Image Layout API Usage Examples
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
      Note: CLI compute command
    - Path: ../../../../../../../../../../zine-layout/pkg/imagelayout/defaults.go
      Note: Default factory functions
    - Path: ../../../../../../../../../../zine-layout/pkg/imagelayout/engine/inputs.go
      Note: InputsFromRequest implementation
    - Path: ../../../../../../../../../../zine-layout/pkg/imagelayout/types.go
      Note: LayoutRequest and ViewportSettings definitions
    - Path: 2025/11/29/photobook-app/2025/11/30/ZINE-LAYOUT-ANALYSIS-image-layout-algorithm-analysis/reference/01-image-layout-algorithm-complete-analysis.md
      Note: Algorithm reference
ExternalSources: []
Summary: Copy/paste-ready examples for using the imagelayout API with both modern LayoutRequest and legacy ViewportSettings
LastUpdated: 2025-12-01T02:00:00-05:00
---


# Image Layout API Usage Examples

## Goal

Provide copy/paste-ready examples demonstrating how to use the imagelayout API for common scenarios. Covers both the modern `LayoutRequest` API and legacy `ViewportSettings` for backward compatibility.

## Context

The imagelayout package computes how images should be cropped, scaled, and positioned within viewports. It supports:
- **Modern API**: `LayoutRequest` with separated Frame/Crop/Presentation specs
- **Legacy API**: `ViewportSettings` (flat structure, still supported)

All examples show:
1. Input configuration
2. Expected output
3. CLI command equivalent
4. Explanation of the calculation

## Modern API Examples (LayoutRequest)

### Example 1: Ratio Mode - 16:9 Widescreen

**Scenario**: Display a 4000×3000px image in a 16:9 aspect ratio frame.

**Code**:
```go
req := imagelayout.LayoutRequest{
    Frame: imagelayout.FrameSpec{
        Mode:  "ratio",
        Ratio: floatPtr(16.0 / 9.0),
        Fill:  "contain",
    },
    Crop: imagelayout.CropSpec{
        Strategy: "auto",
        Zoom:     1.0,
        Extent:   1.0,
        Units:    "normalized",
    },
    Presentation: imagelayout.PresentationSpec{
        UserScale:     1.0,
        ClampToCanvas: true,
    },
}

meta := imagelayout.ImageMeta{Width: 4000, Height: 3000}
inputs, err := engine.InputsFromRequest(req, meta)
if err != nil {
    return err
}

result, trace := engine.ComputeViewport(inputs)
```

**CLI Command**:
```bash
go run ./zine-layout/cmd/zine-layout imagelayout compute \
  --source-width 4000 --source-height 3000 \
  --frame-mode ratio \
  --frame-ratio 1.777778 \
  --frame-fill contain \
  --crop-strategy auto
```

**Expected Result**:
- Canvas: 5333×3000px (16:9 ratio, height matches source)
- Source Rect: Full image (0, 0, 4000, 3000)
- Target Rect: Scaled to fit within canvas
- Mode: "contain"

### Example 2: Page Mode - 8.5×11" with Margins

**Scenario**: Place a 4000×3000px image on letter-size paper with 0.25" margins at 300 DPI.

**Code**:
```go
req := imagelayout.LayoutRequest{
    Frame: imagelayout.FrameSpec{
        Mode: "page",
        Fill: "contain",
        Page: &imagelayout.PageFrame{
            WidthIn:     8.5,
            HeightIn:    11.0,
            DPI:         300,
            Orientation: "portrait",
            MarginsIn: imagelayout.BoxSpacing{
                Top: 0.25, Right: 0.25, Bottom: 0.25, Left: 0.25,
            },
        },
    },
    Crop: imagelayout.CropSpec{
        Strategy: "auto",
    },
    Presentation: imagelayout.PresentationSpec{
        UserScale: 1.0,
    },
}

meta := imagelayout.ImageMeta{Width: 4000, Height: 3000}
inputs, err := engine.InputsFromRequest(req, meta)
result, trace := engine.ComputeViewport(inputs)
```

**CLI Command**:
```bash
go run ./zine-layout/cmd/zine-layout imagelayout compute \
  --source-width 4000 --source-height 3000 \
  --frame-mode page \
  --frame-page-width-in 8.5 \
  --frame-page-height-in 11.0 \
  --frame-page-dpi 300 \
  --frame-margin-top-in 0.25 \
  --frame-margin-right-in 0.25 \
  --frame-margin-bottom-in 0.25 \
  --frame-margin-left-in 0.25 \
  --crop-strategy auto
```

**Expected Result**:
- Canvas: 2550×3300px (8.5×11" at 300 DPI)
- Content: 2400×3150px (after 75px margins)
- Source Rect: Full image (0, 0, 4000, 3000)
- Target Rect: (75, 75, 2400, 1800) - scaled to fit content area
- Scale: 0.6
- Mode: "contain"

### Example 3: Square Crop with Focus Point

**Scenario**: Extract a 1:1 square crop centered on a person's face at (2000, 1500) in the source.

**Code**:
```go
req := imagelayout.LayoutRequest{
    Frame: imagelayout.FrameSpec{
        Mode:  "ratio",
        Ratio: floatPtr(1.0), // Square
        Fill:  "contain",
    },
    Crop: imagelayout.CropSpec{
        Strategy: "focus",
        Ratio:    floatPtr(1.0), // Override frame ratio
        Focus: &imagelayout.FocusPoint{
            SourceX: 2000,
            SourceY: 1500,
            TargetX: 0.5, // Center horizontally
            TargetY: 0.5, // Center vertically
        },
    },
}

meta := imagelayout.ImageMeta{Width: 4000, Height: 3000}
inputs, err := engine.InputsFromRequest(req, meta)
result, trace := engine.ComputeViewport(inputs)
```

**CLI Command**:
```bash
go run ./zine-layout/cmd/zine-layout imagelayout compute \
  --source-width 4000 --source-height 3000 \
  --frame-mode ratio \
  --frame-ratio 1.0 \
  --crop-strategy focus \
  --crop-ratio 1.0 \
  --crop-focus-source-x 2000 \
  --crop-focus-source-y 1500 \
  --crop-focus-target-x 0.5 \
  --crop-focus-target-y 0.5
```

**Expected Result**:
- Source Rect: (500, 0, 3000, 3000) - square crop centered on focus point
- Crop offset calculated to align (2000, 1500) with center of 3000×3000 crop
- Target Rect: Scaled square placed on canvas

### Example 4: Viewport Mode with Zoom

**Scenario**: Display image in a 1920×1080 viewport with 2× zoom.

**Code**:
```go
req := imagelayout.LayoutRequest{
    Frame: imagelayout.FrameSpec{
        Mode: "viewport",
        Viewport: &imagelayout.ViewportFrame{
            Width:  1920,
            Height: 1080,
        },
        Fill: "cover",
    },
    Crop: imagelayout.CropSpec{
        Strategy: "manual",
        Zoom:     2.0,
        Pan:      imagelayout.Vec2{X: 0, Y: 0}, // Center
        Units:    "normalized",
    },
}

meta := imagelayout.ImageMeta{Width: 4000, Height: 3000}
inputs, err := engine.InputsFromRequest(req, meta)
result, trace := engine.ComputeViewport(inputs)
```

**CLI Command**:
```bash
go run ./zine-layout/cmd/zine-layout imagelayout compute \
  --source-width 4000 --source-height 3000 \
  --frame-mode viewport \
  --frame-viewport-width 1920 \
  --frame-viewport-height 1080 \
  --frame-fill cover \
  --crop-strategy manual \
  --crop-zoom 2.0 \
  --crop-pan-x 0 \
  --crop-pan-y 0
```

**Expected Result**:
- Canvas: 1920×1080px
- Source Rect: 2000×1500px (half of original due to 2× zoom), centered
- Target Rect: Scaled to fill viewport
- Mode: "cover"

### Example 5: Presentation Offsets

**Scenario**: Place image with a 10px right, 20px down offset after normal layout.

**Code**:
```go
req := imagelayout.LayoutRequest{
    Frame: imagelayout.FrameSpec{
        Mode:  "ratio",
        Ratio: floatPtr(4.0 / 3.0),
        Fill:  "contain",
    },
    Crop: imagelayout.CropSpec{
        Strategy: "auto",
    },
    Presentation: imagelayout.PresentationSpec{
        UserScale: 1.0,
        OffsetPx:  imagelayout.Vec2Px{X: 10, Y: 20},
    },
}

meta := imagelayout.ImageMeta{Width: 4000, Height: 3000}
inputs, err := engine.InputsFromRequest(req, meta)
result, trace := engine.ComputeViewport(inputs)
```

**CLI Command**:
```bash
go run ./zine-layout/cmd/zine-layout imagelayout compute \
  --source-width 4000 --source-height 3000 \
  --frame-mode ratio \
  --frame-ratio 1.333333 \
  --crop-strategy auto \
  --presentation-offset-x 10 \
  --presentation-offset-y 20
```

**Expected Result**:
- Target Rect: Normal placement plus (10, 20) pixel offset
- Presentation offsets applied after scale calculation

### Example 6: Anchor Preset - Top Left

**Scenario**: Crop image and align to top-left corner.

**Code**:
```go
req := imagelayout.LayoutRequest{
    Frame: imagelayout.FrameSpec{
        Mode:  "ratio",
        Ratio: floatPtr(1.0),
        Fill:  "cover",
    },
    Crop: imagelayout.CropSpec{
        Strategy: "anchor",
        Anchor:   "top-left",
        Ratio:    floatPtr(1.0),
    },
}

meta := imagelayout.ImageMeta{Width: 4000, Height: 3000}
inputs, err := engine.InputsFromRequest(req, meta)
result, trace := engine.ComputeViewport(inputs)
```

**CLI Command**:
```bash
go run ./zine-layout/cmd/zine-layout imagelayout compute \
  --source-width 4000 --source-height 3000 \
  --frame-mode ratio \
  --frame-ratio 1.0 \
  --frame-fill cover \
  --crop-strategy anchor \
  --crop-anchor top-left \
  --crop-ratio 1.0
```

**Expected Result**:
- Source Rect: 3000×3000px square crop from top-left (0, 0, 3000, 3000)
- Target fills canvas completely
- Mode: "cover"

## Legacy API Examples (ViewportSettings)

### Example 1: Basic Page Layout

**Code**:
```go
settings := imagelayout.DefaultSettings()
meta := imagelayout.ImageMeta{Width: 4000, Height: 3000}

inputs, err := engine.InputsFromSettings(settings, meta)
if err != nil {
    return err
}

result, trace := engine.ComputeViewport(inputs)
// result.SourceRect: crop region in source
// result.TargetRect: placement on canvas
// result.CanvasRect: content area
// result.Scale: applied scale factor
// result.Mode: "contain" or "cover"
```

**Result**:
- Canvas: 2550×3300px (8×10" at 300 DPI)
- Content: 2400×3000px (after 0.25" margins)
- Source Rect: Full image
- Target Rect: Scaled to fit content area
- Scale: 0.6
- Mode: "contain"

### Example 2: Square Crop with Focus Point

**Code**:
```go
settings := imagelayout.ViewportSettings{
    Mode: "page",
    CropRatio: floatPtr(1.0),  // Square
    Focus: &imagelayout.FocusPoint{
        SourceX: 2000,
        SourceY: 1500,
        TargetX: 0.5,  // Center
        TargetY: 0.5,
    },
    PaperWidthIn:  8.0,
    PaperHeightIn: 10.0,
    DPI:           300,
    MarginTopIn:   0.25,
    MarginRightIn: 0.25,
    MarginBottomIn: 0.25,
    MarginLeftIn:  0.25,
}
meta := imagelayout.ImageMeta{Width: 4000, Height: 3000}

inputs, _ := engine.InputsFromSettings(settings, meta)
result, _ := engine.ComputeViewport(inputs)
// Crop region centered on (2000, 1500) in source
```

**Result**:
- Source Rect: (500, 0, 3000, 3000) - square crop aligned to focus point
- Scale: 0.75

### Example 3: Cover Mode with Anchor

**Code**:
```go
settings := imagelayout.DefaultSettings()
settings.CropToFill = true
settings.AnchorPreset = "top-left"
meta := imagelayout.ImageMeta{Width: 4000, Height: 3000}

inputs, _ := engine.InputsFromSettings(settings, meta)
result, _ := engine.ComputeViewport(inputs)
// result.Mode == "cover"
// Image fills canvas, aligned to top-left
```

**Result**:
- Mode: "cover"
- Image fills entire content area
- Aligned to top-left corner

### Example 4: Fit to Width

**Code**:
```go
settings := imagelayout.ViewportSettings{
    Mode: "fit",
    FitMode: "width",
    FitWidthPx: floatPtr(1600),
    PaperWidthIn: 8.0,
    PaperHeightIn: 10.0,
    DPI: 300,
}
meta := imagelayout.ImageMeta{Width: 4000, Height: 3000}

inputs, _ := engine.InputsFromSettings(settings, meta)
result, _ := engine.ComputeViewport(inputs)
// Canvas: 1600×1200px (maintains 4:3 aspect ratio)
```

**Result**:
- Canvas: 1600×1200px
- Height derived from width and source aspect ratio
- Scale: 0.4

## CLI Usage Examples

### Basic Compute with Spec File

**Create spec.yaml**:
```yaml
layout:
  frame:
    mode: page
    fill: contain
    page:
      width_in: 8.5
      height_in: 11.0
      dpi: 300
      orientation: portrait
      margins_in:
        top: 0.25
        right: 0.25
        bottom: 0.25
        left: 0.25
  crop:
    strategy: auto
    zoom: 1.0
    extent: 1.0
  presentation:
    user_scale: 1.0
    clamp_to_canvas: true
image:
  width: 4000
  height: 3000
```

**Run**:
```bash
go run ./zine-layout/cmd/zine-layout imagelayout compute --spec spec.yaml
```

### Compute with Flags Only

```bash
go run ./zine-layout/cmd/zine-layout imagelayout compute \
  --source-width 4000 \
  --source-height 3000 \
  --frame-mode page \
  --frame-page-width-in 8.5 \
  --frame-page-height-in 11.0 \
  --frame-page-dpi 300 \
  --frame-page-orientation portrait \
  --frame-margin-top-in 0.25 \
  --frame-margin-right-in 0.25 \
  --frame-margin-bottom-in 0.25 \
  --frame-margin-left-in 0.25 \
  --crop-strategy auto \
  --presentation-user-scale 1.0
```

### Inspect Frame Stage Only

```bash
go run ./zine-layout/cmd/zine-layout imagelayout layout frame \
  --source-width 4000 \
  --source-height 3000 \
  --frame-mode ratio \
  --frame-ratio 1.5
```

**Output**:
```json
{
  "mode": "ratio",
  "canvas_rect": {
    "x": 0,
    "y": 0,
    "w": 4500,
    "h": 3000
  },
  "content_rect": {
    "x": 0,
    "y": 0,
    "w": 4500,
    "h": 3000
  },
  "target_ratio": 1.5,
  "margins_px": {
    "top": 0,
    "right": 0,
    "bottom": 0,
    "left": 0
  },
  "clamp_to_canvas": true
}
```

### Inspect Crop Stage

```bash
go run ./zine-layout/cmd/zine-layout imagelayout layout crop \
  --source-width 4000 \
  --source-height 3000 \
  --frame-mode ratio \
  --frame-ratio 1.0 \
  --crop-strategy manual \
  --crop-zoom 2.0 \
  --crop-pan-x 0.25 \
  --crop-pan-y -0.25
```

**Output**:
```json
{
  "source_rect": {
    "x": 625,
    "y": 125,
    "w": 1500,
    "h": 1500
  },
  "diagnostics": {
    "sx": 625,
    "sy": 125,
    "sw": 1500,
    "sh": 1500,
    "source_ratio": 1.333,
    "requested_ratio": 1.0,
    "range_x": 2500,
    "range_y": 1500,
    "focus_applied": false,
    "crop_zoom": 2.0,
    "crop_extent": 1.0
  }
}
```

### Inspect Presentation Stage

```bash
go run ./zine-layout/cmd/zine-layout imagelayout layout presentation \
  --source-width 4000 \
  --source-height 3000 \
  --frame-mode ratio \
  --frame-ratio 1.0 \
  --crop-strategy auto \
  --presentation-user-scale 1.2 \
  --presentation-offset-x 15 \
  --presentation-offset-y -10
```

**Output**:
```json
{
  "target_rect": {
    "x": 15,
    "y": -10,
    "w": 3600,
    "h": 3600
  },
  "scale": 1.2,
  "mode": "contain",
  "diagnostics": {
    "scale_x": 1.0,
    "scale_y": 1.0,
    "final": 1.2,
    "mode": "contain",
    "dst_w": 3600,
    "dst_h": 3600,
    "tx": 15,
    "ty": -10,
    "presentation_units": "px"
  }
}
```

## Service Layer Examples

### Create Laid Out Image

```go
import (
    "github.com/go-go-golems/zine-layout/pkg/services"
    "github.com/go-go-golems/zine-layout/pkg/repo"
)

// Assuming you have a LayoutService instance
layoutService := &services.LayoutService{
    repos: repos, // Your repository collection
}

// Create a laid-out image with template and optional overrides
overridesJSON := `{
    "presentation": {
        "user_scale": 1.1,
        "offset_px": {"x": 5, "y": -3}
    }
}`

laidOut, err := layoutService.CreateLaidOutImage(
    projectID,
    assetID,
    templateID,
    &overridesJSON,
)
if err != nil {
    return err
}

// laidOut.ResultJSON contains the Computation (Layout + Result + Trace)
```

### Recompute Existing Layout

```go
// Fetch existing laid-out image
laidOut, err := repos.LaidOutImages.Get(laidOutID)
if err != nil {
    return err
}

// Recompute (useful after template changes)
err = layoutService.RecomputeLaidOutImage(laidOut)
if err != nil {
    return err
}

// laidOut.ResultJSON now contains updated computation
```

## Common Patterns

### Pattern 1: Default Layout with Overrides

```go
// Start with defaults
req := imagelayout.DefaultLayoutRequest()

// Override specific fields
req.Frame.Mode = "viewport"
req.Frame.Viewport = &imagelayout.ViewportFrame{
    Width:  1920,
    Height: 1080,
}
req.Crop.Zoom = 1.5
req.Presentation.UserScale = 1.1

meta := imagelayout.ImageMeta{Width: 4000, Height: 3000}
inputs, _ := engine.InputsFromRequest(req, meta)
result, _ := engine.ComputeViewport(inputs)
```

### Pattern 2: Programmatic Ratio Calculation

```go
// Calculate ratio from dimensions
aspectRatio := 16.0 / 9.0

req := imagelayout.LayoutRequest{
    Frame: imagelayout.FrameSpec{
        Mode:  "ratio",
        Ratio: &aspectRatio,
        Fill:  "contain",
    },
    Crop: imagelayout.CropSpec{
        Strategy: "auto",
    },
}
```

### Pattern 3: Merging Template with Overrides

```go
// Load template settings
var templateReq imagelayout.LayoutRequest
json.Unmarshal(templateJSON, &templateReq)

// Parse overrides
var overrides imagelayout.LayoutRequest
json.Unmarshal(overridesJSON, &overrides)

// Merge (overrides take precedence)
merged := templateReq
if overrides.Frame.Mode != "" {
    merged.Frame.Mode = overrides.Frame.Mode
}
if overrides.Crop.Zoom != 0 {
    merged.Crop.Zoom = overrides.Crop.Zoom
}
if overrides.Presentation.UserScale != 0 {
    merged.Presentation.UserScale = overrides.Presentation.UserScale
}

inputs, _ := engine.InputsFromRequest(merged, meta)
result, _ := engine.ComputeViewport(inputs)
```

## Testing Examples

### Unit Test: Ratio Frame

```go
func TestRatioFrameLayout(t *testing.T) {
    req := imagelayout.DefaultLayoutRequest()
    ratio := 16.0 / 9.0
    req.Frame.Mode = "ratio"
    req.Frame.Ratio = &ratio

    meta := imagelayout.ImageMeta{Width: 4000, Height: 3000}
    inputs, err := engine.InputsFromRequest(req, meta)
    if err != nil {
        t.Fatalf("InputsFromRequest: %v", err)
    }

    if inputs.Frame.Mode != engine.FrameModeRatio {
        t.Fatalf("expected ratio mode")
    }

    expectedW := ratio * float64(meta.Height)
    if !almostEqual(inputs.Frame.CanvasRect.W, expectedW) {
        t.Fatalf("unexpected canvas width: %f", inputs.Frame.CanvasRect.W)
    }
}
```

### Unit Test: Crop Zoom

```go
func TestCropZoom(t *testing.T) {
    req := imagelayout.DefaultLayoutRequest()
    req.Frame.Mode = "ratio"
    ratio := 1.0
    req.Frame.Ratio = &ratio
    req.Crop.Strategy = "manual"
    req.Crop.Zoom = 2.0
    req.Crop.Pan = imagelayout.Vec2{X: 0, Y: 0}

    meta := imagelayout.ImageMeta{Width: 4000, Height: 4000}
    inputs, err := engine.InputsFromRequest(req, meta)
    if err != nil {
        t.Fatalf("InputsFromRequest: %v", err)
    }

    result, _ := engine.ComputeViewport(inputs)
    
    // 2× zoom means crop is half the size
    expectedCropW := 2000.0
    if !almostEqual(result.SourceRect.W, expectedCropW) {
        t.Fatalf("expected zoomed width %f, got %f", expectedCropW, result.SourceRect.W)
    }
}
```

## Related

- Algorithm analysis: `01-image-layout-algorithm-complete-analysis.md`
- Refactor plan: `REVAMP-CROP-ALGORITHM/.../01-framecrop-refactor-plan.md`
- CLI commands: `zine-layout/cmd/zine-layout/cmds/imagelayout/`
- Engine implementation: `zine-layout/pkg/imagelayout/engine/`
