---
Title: 'Layout Algorithms: Compact Overview'
Ticket: ZINE-APP-DESIGN
Status: draft
Topics:
    - reference
    - algorithms
    - technical-overview
DocType: reference
Intent: long-term
Owners: []
RelatedFiles:
    - Path: ../../../../../../../../../zine-layout/pkg/imagelayout/engine/engine.go
      Note: Core imagelayout algorithms - InputsFromSettings and ComputeViewport
    - Path: ../../../../../../../../../zine-layout/pkg/imagelayout/types.go
      Note: Type definitions for LayoutRequest
    - Path: ../../../../../../../../../zine-layout/pkg/pagelayout/renderer/renderer.go
      Note: Page rendering algorithm - RenderPage
    - Path: ../../../../../../../../../zine-layout/pkg/pagelayout/settings.go
      Note: PageLayoutSettings and helper methods for content area calculation
    - Path: ../../../../../../../../../zine-layout/pkg/zinelayout/layout.go
      Note: ZineLayout imposition algorithm - CreateOutputImage for arranging pages on print sheets
    - Path: ../../../../../../../../../zine-layout/pkg/zinelayout/margin.go
      Note: Margin type definitions and pixel computation from expressions
    - Path: ../../../../../../../../../zine-layout/pkg/zinelayout/rotation.go
      Note: Image rotation algorithms (0/90/180/270 degrees)
    - Path: 2025/11/29/photobook-app/2025/11/30/PAGE-LAYOUT-ANALYSIS-page-layout-algorithm-analysis/reference/01-page-layout-algorithm-complete-analysis.md
      Note: Detailed analysis of pagelayout package
    - Path: 2025/11/29/photobook-app/2025/11/30/ZINE-LAYOUT-ANALYSIS-image-layout-algorithm-analysis/reference/01-image-layout-algorithm-complete-analysis.md
      Note: Detailed analysis of imagelayout package
    - Path: 2025/11/29/photobook-app/2025/11/30/ZINE-LAYOUT-ANALYSIS-image-layout-algorithm-analysis/reference/02-zine-layout-algorithm-complete-analysis.md
      Note: Detailed analysis of zinelayout package
ExternalSources: []
Summary: Compact technical overview of imagelayout, pagelayout, and zinelayout algorithms with code references for debate participants
LastUpdated: 2025-11-30T15:00:00-05:00
---


# Layout Algorithms: Compact Overview

**Purpose:** Quick reference for debate participants. Links to detailed analysis docs and specific code locations.

## Three-Layer Architecture

```
┌─────────────────────────────────────────┐
│  imagelayout  │  Crop/scale single image│
│               │  within viewport        │
└───────────────┼─────────────────────────┘
                │
┌───────────────┼─────────────────────────┐
│  pagelayout   │  Render image onto     │
│               │  physical page         │
└───────────────┼─────────────────────────┘
                │
┌───────────────┼─────────────────────────┐
│  zinelayout   │  Arrange multiple pages │
│               │  on print sheets        │
└───────────────┴─────────────────────────┘
```

**Workflow:** `imagelayout` → `pagelayout` → `zinelayout`

---

## 1. imagelayout — Image Cropping & Scaling

**Purpose:** Computes how to crop and scale a single image to fit a viewport (canvas/content area).

**Key Question:** "What part of the source image should I use, and how big should it be?"

### Core Algorithm: Two-Phase Computation

**Phase 1: Input Normalization** (`InputsFromSettings`)
- Converts inches, pixels, ratios → normalized pixel values
- Determines canvas size based on mode (page/crop/fit)
- Resolves anchor presets → normalized coordinates

**Phase 2: Viewport Computation** (`ComputeViewport`)
- Calculates crop region (`SourceRect`) — which part of image to use
- Calculates scale factor — how big to make it
- Calculates placement (`TargetRect`) — where it goes on canvas

### Key Functions

| Function | Purpose | Location |
|----------|---------|----------|
| `InputsFromSettings()` | Normalize settings + image meta → `Inputs` | ```59:214:zine-layout/pkg/imagelayout/engine/engine.go``` |
| `ComputeViewport()` | Calculate crop region, scale, placement | ```242:399:zine-layout/pkg/imagelayout/engine/engine.go``` |
| `resolveAnchor()` | Map preset names → normalized coords | ```223:239:zine-layout/pkg/imagelayout/engine/engine.go``` |
| `computeOffset()` | Convert position values → pixel offsets | ```408:418:zine-layout/pkg/imagelayout/engine/engine.go``` |

### Key Types

| Type | Purpose | Location |
|------|---------|----------|
| `LayoutRequest` | Input configuration (mode, paper size, margins, crop ratio, positioning) | ```27:58:zine-layout/pkg/imagelayout/types.go``` |
| `ViewportResult` | Output (SourceRect, TargetRect, CanvasRect, Scale, Mode) | ```69:75:zine-layout/pkg/imagelayout/types.go``` |
| `Rect` | Rectangle with float coords (X, Y, W, H) | ```4:9:zine-layout/pkg/imagelayout/types.go``` |
| `FocusPoint` | Align source point to target position | ```61:66:zine-layout/pkg/imagelayout/types.go``` |

### Modes

- **`page`**: Canvas = Paper size × DPI, Content = Canvas - margins
- **`crop`**: Canvas = CropWidth × CropHeight (no margins)
- **`fit`**: Canvas = FitWidth × FitHeight (calculated from FitMode)

### Scaling Modes

- **`contain`** (`CropToFill=false`): Uses `min(scaleX, scaleY)` — entire crop fits, may leave empty space
- **`cover`** (`CropToFill=true`): Uses `max(scaleX, scaleY)` — fills target, may crop beyond SourceRect

### Positioning Strategies

1. **Focus Point** (highest priority): Aligns specific source point to target position
2. **Anchor Preset**: Maps names (`center`, `top-left`, etc.) to normalized coords (-1..1)
3. **Manual**: Direct `PositionX`/`PositionY` values (normalized or pixels)

**Anchor Presets:** ```11:21:zine-layout/pkg/imagelayout/engine/engine.go```

### Detailed Analysis

📚 **Full documentation:** `vibes/2025/11/29/photobook-app/2025/11/30/ZINE-LAYOUT-ANALYSIS-image-layout-algorithm-analysis/reference/01-image-layout-algorithm-complete-analysis.md`

---

## 2. pagelayout — Page Rendering

**Purpose:** Renders a cropped/scaled image onto a physical page canvas with margins, spreads, and variants.

**Key Question:** "How do I place this image on a page?"

### Core Algorithm: RenderPage

**Steps:**
1. Validate settings, create canvas (Paper size × DPI)
2. Calculate content area (Canvas - margins)
3. Optionally crop source using `LayoutResult.SourceRect` (from imagelayout)
4. Place image based on `PositioningMode`:
   - **`fill`**: Scale-cover into content area, preserve aspect ratio, center
   - **`absolute`**: Place at exact coordinates with exact size
5. Draw optional border
6. Generate variants (thumbnail, left/right spreads)

### Key Functions

| Function | Purpose | Location |
|----------|---------|----------|
| `RenderPage()` | Main rendering function | ```43:122:zine-layout/pkg/pagelayout/renderer/renderer.go``` |
| `drawIntoTargetCover()` | Scale-cover algorithm (fill mode) | ```124:142:zine-layout/pkg/pagelayout/renderer/renderer.go``` |
| `cropSourceToRect()` | Crop source image to SourceRect | ```144:168:zine-layout/pkg/pagelayout/renderer/renderer.go``` |
| `splitSpread()` | Split spread into left/right pages | ```189:206:zine-layout/pkg/pagelayout/renderer/renderer.go``` |
| `makeThumbnail()` | Generate thumbnail variant | ```170:187:zine-layout/pkg/pagelayout/renderer/renderer.go``` |
| `ContentRectPx()` | Calculate content area (canvas - margins) | ```120:136:zine-layout/pkg/pagelayout/settings.go``` |
| `Canonicalize()` | Validate and normalize settings | ```59:101:zine-layout/pkg/pagelayout/settings.go``` |

### Key Types

| Type | Purpose | Location |
|------|---------|----------|
| `PageLayoutSettings` | Page configuration (size, margins, DPI, spread, positioning, border) | ```30:57:zine-layout/pkg/pagelayout/settings.go``` |
| `RenderContext` | Inputs for rendering (settings, source image, LayoutResult, background) | ```25:35:zine-layout/pkg/pagelayout/renderer/renderer.go``` |
| `PageRenderResult` | Output (Full page + variants map) | ```37:40:zine-layout/pkg/pagelayout/renderer/renderer.go``` |

### Positioning Modes

- **`fill`**: Scale-cover into content area (uses `max(scaleX, scaleY)`, centers result)
- **`absolute`**: Exact coordinates (`ImageXIn`, `ImageYIn`) and size (`ImageWidthIn`, `ImageHeightIn`)
- **`snap`**: Currently alias for `fill`

### Variants Generated

- **`full`**: Complete rendered page
- **`combined`**: Same as full (alias)
- **`thumbnail`**: Scaled to max side (default 512px)
- **`left`**: Left half of spread (if `IsSpread=true`)
- **`right`**: Right half of spread (if `IsSpread=true`)

### Spread Handling

- **Gutter**: Binding area between left/right pages
- **Split calculation**: `leftEnd = center - gutter/2`, `rightStart = center + gutter/2`
- **Gutter markers**: Dashed vertical lines at inner edges

**Spread split:** ```189:206:zine-layout/pkg/pagelayout/renderer/renderer.go```

### Detailed Analysis

📚 **Full documentation:** `vibes/2025/11/29/photobook-app/2025/11/30/PAGE-LAYOUT-ANALYSIS-page-layout-algorithm-analysis/reference/01-page-layout-algorithm-complete-analysis.md`

---

## 3. zinelayout — Print Sheet Imposition

**Purpose:** Arranges multiple rendered pages onto print sheets in a grid pattern so that when folded, pages appear in correct reading order.

**Key Question:** "How do I arrange pages on a sheet so folding produces the right order?"

### Core Algorithm: CreateOutputImage

**Steps:**
1. Determine grid size (from `PageSetup` or infer from layout positions)
2. Compute all margins (convert expressions → pixels)
3. Calculate cell sizes (image size + layout margins)
4. Calculate cell positions (row/column layout)
5. Create canvas, fill white
6. For each layout entry:
   - Validate input index
   - Rotate image if needed (0°, 90°, 180°, 270°)
   - Place at cell position + layout margins
7. Draw borders (layout borders, inner borders)
8. Apply margins (PageSetup + OutputPage margins)
9. Draw page border and global border

### Key Functions

| Function | Purpose | Location |
|----------|---------|----------|
| `CreateOutputImage()` | Main imposition algorithm | ```60:260:zine-layout/pkg/zinelayout/layout.go``` |
| `ComputeAllMargins()` | Convert margin expressions → pixels | ```300:337:zine-layout/pkg/zinelayout/layout.go``` |
| `rotateImage()` | Rotate image (0/90/180/270 degrees) | ```8:21:zine-layout/pkg/zinelayout/rotation.go``` |
| `AllImagesSameSize()` | Validate all input images same size | ```263:274:zine-layout/pkg/zinelayout/layout.go``` |

### Key Types

| Type | Purpose | Location |
|------|---------|----------|
| `ZineLayout` | Root structure (PageSetup, OutputPages, Global) | ```13:17:zine-layout/pkg/zinelayout/layout.go``` |
| `PageSetup` | Grid configuration and global margins | ```24:31:zine-layout/pkg/zinelayout/layout.go``` |
| `OutputPage` | Single print sheet (ID, Layout array, margins) | ```33:38:zine-layout/pkg/zinelayout/layout.go``` |
| `Layout` | Single page placement (InputIndex, Position, Rotation, Margin) | ```40:46:zine-layout/pkg/zinelayout/layout.go``` |
| `Position` | Grid coordinates (Row, Column) | ```54:58:zine-layout/pkg/zinelayout/layout.go``` |
| `Margin` | Margin values (Top, Bottom, Left, Right) with unit expressions | ```12:18:zine-layout/pkg/zinelayout/margin.go``` |

### Grid System

- **Grid**: Rows × Columns (e.g., 2×4 = 8 pages)
- **Position**: 0-indexed (Row 0 = top, Column 0 = left)
- **Cell size**: Image size + Layout margins
- **Cell position**: Calculated left-to-right, top-to-bottom

**Grid calculation:** ```78:89:zine-layout/pkg/zinelayout/layout.go```

### Rotation

- **0°**: Normal orientation
- **90°**: Clockwise rotation
- **180°**: Upside down (common for top row in zines)
- **270°**: Counter-clockwise rotation

**Rotation functions:** ```8:21:zine-layout/pkg/zinelayout/rotation.go```

### Margin System

Three levels (additive):
1. **PageSetup.Margin**: Global margins for entire sheet
2. **OutputPage.Margin**: Per-output-page margins
3. **Layout.Margin**: Per-layout (per-page) margins

**Margin computation:** ```71:109:zine-layout/pkg/zinelayout/margin.go```

### Border Types

- **`plain`**: Solid line
- **`dotted`**: Dotted border
- **`dashed`**: Dashed border
- **`corner`**: Corner marks only

**Border drawing:** `zine-layout/pkg/zinelayout/border.go`

### Detailed Analysis

📚 **Full documentation:** `vibes/2025/11/29/photobook-app/2025/11/30/ZINE-LAYOUT-ANALYSIS-image-layout-algorithm-analysis/reference/02-zine-layout-algorithm-complete-analysis.md`

---

## Integration Points

### Service Layer

| Service | Purpose | Location |
|---------|---------|----------|
| `LayoutService.CreateLaidOutImage()` | Uses imagelayout to compute crop/scale | `zine-layout/pkg/services/layout.go` |
| `PagesService.RenderPage()` | Uses pagelayout to render pages | `zine-layout/pkg/services/pages.go` |
| `ImpositionService.ImposeZine()` | Uses zinelayout to create print sheets | `zine-layout/pkg/services/imposition.go` |

### CLI Commands

| Command | Purpose | Location |
|---------|---------|----------|
| `imagelayout compute` | Compute ViewportResult from settings | `zine-layout/cmd/zine-layout/cmds/imagelayout/compute.go` |
| `pagelayout render` | Render page with image | `zine-layout/cmd/zine-layout/cmds/pagelayout/render.go` |
| `render` | Render zine layout from YAML | `zine-layout/cmd/zine-layout/cmds/render.go` |

---

## Quick Reference: Algorithm Flow

### Complete Workflow

```
1. User selects image + template
   ↓
2. imagelayout.InputsFromSettings()
   → Normalize settings → Inputs
   ↓
3. imagelayout.ComputeViewport()
   → Calculate SourceRect, TargetRect, Scale
   → Returns ViewportResult
   ↓
4. pagelayout.RenderPage()
   → Crop source to SourceRect (if provided)
   → Scale and place on page canvas
   → Generate variants (thumbnail, spreads)
   → Returns PageRenderResult
   ↓
5. zinelayout.CreateOutputImage()
   → Arrange multiple pages on grid
   → Rotate pages as needed
   → Apply margins and borders
   → Returns print-ready sheet image
   ↓
6. Export to PDF
   → Multiple sheets → PDF pages
```

### Data Flow

```
LayoutRequest (imagelayout input)
    ↓
ViewportResult (imagelayout output)
    ↓
LayoutResult (stored in DB, passed to pagelayout)
    ↓
PageRenderResult (pagelayout output)
    ↓
Rendered page images (input to zinelayout)
    ↓
ZineLayout YAML spec (zinelayout input)
    ↓
Print sheet images (zinelayout output)
    ↓
PDF export
```

---

## Key Constraints & Assumptions

### imagelayout
- All calculations use floating-point (sub-pixel precision)
- Source dimensions must be > 0
- DPI must be > 0
- Margins must not exceed canvas size
- Focus point overrides PositionX/Y when provided

### pagelayout
- All input images must be same size (for zinelayout)
- Canvas dimensions = Paper size × DPI (rounded)
- Content area = Canvas - margins
- Spread gutter accounts for binding area
- Variants generated automatically (can't request single variant)

### zinelayout
- All input page images must be same size
- Grid size inferred from layout positions if not specified
- Rotation currently validated to 0° or 180° (though 90°/270° exist)
- Margins stack: PageSetup + OutputPage + Layout
- Cell size = Image size + Layout margins

---

## Common Use Cases

### Use Case 1: Basic Page Layout
```
1. imagelayout: Compute crop/scale for image on 8.5×11" page
2. pagelayout: Render image onto page with margins
3. Result: Single rendered page
```

### Use Case 2: Zine with Multiple Pages
```
1. For each page:
   a. imagelayout: Compute crop/scale
   b. pagelayout: Render page
2. zinelayout: Arrange all pages on print sheet(s)
3. Result: Print-ready sheet(s) for folding
```

### Use Case 3: Spread Page
```
1. pagelayout: Render spread (IsSpread=true, wider canvas)
2. pagelayout: Split into left/right variants
3. Result: Two separate page images for binding
```

---

## Related Files

### Analysis Documents
- **Image Layout Analysis:** `vibes/2025/11/29/photobook-app/2025/11/30/ZINE-LAYOUT-ANALYSIS-image-layout-algorithm-analysis/reference/01-image-layout-algorithm-complete-analysis.md`
- **Zine Layout Analysis:** `vibes/2025/11/29/photobook-app/2025/11/30/ZINE-LAYOUT-ANALYSIS-image-layout-algorithm-analysis/reference/02-zine-layout-algorithm-complete-analysis.md`
- **Page Layout Analysis:** `vibes/2025/11/29/photobook-app/2025/11/30/PAGE-LAYOUT-ANALYSIS-page-layout-algorithm-analysis/reference/01-page-layout-algorithm-complete-analysis.md`

### Core Packages
- **imagelayout:** `zine-layout/pkg/imagelayout/`
- **pagelayout:** `zine-layout/pkg/pagelayout/`
- **zinelayout:** `zine-layout/pkg/zinelayout/`

### Service Layer
- **Layout Service:** `zine-layout/pkg/services/layout.go`
- **Pages Service:** `zine-layout/pkg/services/pages.go`
- **Imposition Service:** `zine-layout/pkg/services/imposition.go`

---

## For Debate Participants

**When discussing sequencing:**
- Focus on how images flow through the system
- Consider how `imagelayout` cropping affects sequence feel
- Think about how `pagelayout` templates constrain sequencing

**When discussing cropping:**
- Reference `imagelayout` algorithms (crop region calculation, scale factors)
- Consider when cropping happens (before sequencing? during? after?)
- Think about smart defaults vs. user control

**When discussing page layouts:**
- Reference `pagelayout` templates and positioning modes
- Consider how templates affect sequencing workflow
- Think about spread handling and gutter calculations

**When discussing print/export:**
- Reference `zinelayout` imposition algorithm
- Consider how page order maps to print sheets
- Think about rotation and grid arrangements

**Code references:** All code references use format ````startLine:endLine:filepath``` for easy lookup.

