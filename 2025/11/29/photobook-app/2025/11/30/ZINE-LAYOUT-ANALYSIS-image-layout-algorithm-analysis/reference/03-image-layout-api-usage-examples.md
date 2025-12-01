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
      Note: LayoutRequest definitions
    - Path: 2025/11/29/photobook-app/2025/11/30/ZINE-LAYOUT-ANALYSIS-image-layout-algorithm-analysis/reference/01-image-layout-algorithm-complete-analysis.md
      Note: Algorithm reference
ExternalSources: []
Summary: Copy/paste-ready examples for using the imagelayout API with LayoutRequest (frame/crop/presentation)
LastUpdated: 2025-12-01T02:00:00-05:00
---


# Image Layout API Usage Examples

## Goal

Provide copy/paste-ready examples demonstrating how to use the imagelayout API for common scenarios. Covers the modern `LayoutRequest` API.

## Context

The imagelayout package computes how images should be cropped, scaled, and positioned within viewports. It supports:
- **API**: `LayoutRequest` with separated Frame/Crop/Presentation specs

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

## Legacy API Examples

Legacy `ViewportSettings` has been removed. Use `LayoutRequest` for all specs and requests.
