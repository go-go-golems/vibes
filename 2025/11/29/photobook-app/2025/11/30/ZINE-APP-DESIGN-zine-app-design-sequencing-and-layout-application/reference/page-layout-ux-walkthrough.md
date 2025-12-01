---
Title: 'Page Layout UX Walkthrough'
Ticket: ZINE-APP-DESIGN
Status: draft
Topics:
    - ux-design
    - page-layout
    - template-assignment
    - walkthrough
DocType: reference
Intent: long-term
Owners: []
RelatedFiles:
    - debate-round-18-page-layout-ux-api.md
Summary: UX walkthrough for page template creation and assignment workflow with ASCII diagrams showing template creation, selection, preview, assignment, batch operations, asynchronous rendering, and error handling
LastUpdated: 2025-12-01T00:00:00-05:00
---

# Page Layout UX Walkthrough

**Based on:** Debate Round 18 consensus on page layout UX+API patterns

**Key Principles:**
- Visual template creation (wizard or form with actual `PageLayoutSettings`: page dimensions, margins, spread mode, positioning mode, borders)
- Visual template selection (thumbnails/preview cards showing image requirements)
- Preview before assigning (compute page layout without creating)
- Batch operations (assign template to multiple pages)
- Optimistic updates (fast UI with rollback on error)
- Asynchronous rendering (render pages in background)
- Template reuse (create once, use many times)
- Image requirements (page templates implicitly define image orientation and aspect ratio compatibility)

**Page Template Settings (`PageLayoutSettings`):**
- **Page dimensions:** `PageWidthIn`, `PageHeightIn`, `DPI` (physical page size in inches, DPI for pixel conversion)
- **Margins:** `MarginTopIn`, `MarginRightIn`, `MarginBottomIn`, `MarginLeftIn` (margins in inches)
- **Spread configuration:** `IsSpread` (boolean), `GutterWidthIn`, `GutterOverlapIn` (for two-page spreads)
- **Positioning mode:** `PositioningMode` (`"fill"`, `"absolute"`, `"snap"`)
  - `"fill"`: Scale-cover into content area, preserving aspect ratio (works with any aspect ratio, crops to fit)
  - `"absolute"`: Place at exact coordinates with exact size (requires `ImageXIn`, `ImageYIn`, `ImageWidthIn`, `ImageHeightIn`)
  - `"snap"`: Currently treated as alias for `"fill"`
- **Border settings:** `BorderEnabled`, `BorderColor`, `BorderType` (`"plain"`, `"dotted"`, `"dashed"`, `"corner"`)

**Image Requirements (implicit from template):**
- **Page dimensions determine orientation:** Portrait (8.5×11in), landscape (11×8.5in), square (8×8in)
- **Content area (page minus margins) determines usable space:** Affects which images work well
- **Positioning mode affects image selection:**
  - `"fill"` mode: Works with any aspect ratio (crops to fit content area)
  - `"absolute"` mode: Allows exact placement, but requires specific image dimensions
- **Spread configuration:** Two-page spreads require wider images or different composition

---

## UI Layout

### Initial State: Page Layout View

```
┌─────────────────────────────────────────────────────────────────────────┐
│ Page Layouts                                    [+ New Template]      │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                           │
│ Templates:                                                                │
│ ┌──────────────────────┐  ┌──────────────────────┐                      │
│ │ 📄 Single Page       │  │ 📄 Spread (2-page)   │                      │
│ │ 8.5x11in portrait    │  │ 11x17in landscape    │                      │
│ │ Portrait images      │  │ Wide images           │                      │
│ │ Fill mode            │  │ Fill mode             │                      │
│ └──────────────────────┘  └──────────────────────┘                      │
│ ┌──────────────────────┐  ┌──────────────────────┐                      │
│ │ 📄 Square Page        │  │ 📄 Custom Template   │                      │
│ │ 8x8in square          │  │ Custom settings      │                      │
│ │ Square images         │  │ See details           │                      │
│ │ Fill mode             │  │                       │                      │
│ └──────────────────────┘  └──────────────────────┘                      │
│                                                                           │
│ Available Laid-Out Images:                                               │
│ ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐             │
│ │ IMG-001 │ │ IMG-002 │ │ IMG-003 │ │ IMG-004 │ │ IMG-005 │             │
│ │(cropped)│ │(cropped)│ │(cropped)│ │(cropped)│ │(cropped)│             │
│ └─────────┘ └─────────┘ └─────────┘ └─────────┘ └─────────┘             │
│                                                                           │
│ Laid-Out Pages:                                                           │
│ ┌─────────────────────────────────────────────────────────────────────┐ │
│ │ [No pages yet]                                                      │ │
│ │                                                                     │ │
│ │ Select a template and laid-out image to create a page.              │ │
│ └─────────────────────────────────────────────────────────────────────┘ │
│                                                                           │
└─────────────────────────────────────────────────────────────────────────┘
```

**API Calls:**
- `GET /page-templates` — List global templates
  - **Params:** None
  - **Response:** `{ page_templates: PageTemplate[] }`
- `GET /projects/{projectId}/page-templates` — List project templates
  - **Params:** `projectId` (path parameter)
  - **Response:** `{ page_templates: PageTemplate[] }`
- `GET /projects/{projectId}/laid-out-images` — List laid-out images for project
  - **Params:** `projectId` (path parameter)
  - **Response:** `{ laid_out_images: LaidOutImage[] }`
- `GET /projects/{projectId}/laid-out-pages` — List laid-out pages for project
  - **Params:** `projectId` (path parameter)
  - **Response:** `{ laid_out_pages: LaidOutPage[] }`

---

## Template Creation Workflow

### Step 1: User Creates Template (Wizard or Form)

**User Action:** Click "[+ New Template]" button

**UI State (Wizard Option - Step 1):**
```
┌─────────────────────────────────────────────────────────────────────────┐
│ Create Page Template                                                    │
├─────────────────────────────────────────────────────────────────────────┤ │
│                                                                           │ │
│ Step 1 of 4: Page Size                                                   │ │
│ ┌─────────────────────────────────────────────────────────────────────┐ │ │
│ │ ┌───────────────────────────────┬─────────────────────────────────┐ │ │
│ │ │ Settings                       │ Preview                        │ │ │
│ │ ├───────────────────────────────┼─────────────────────────────────┤ │ │
│ │ │ Paper Size: [8.5x11in Portrait]│ ┌─────────────────────────┐   │ │ │
│ │ │                                │ │                         │   │ │ │
│ │ │ Or Custom:                      │ │    Page Preview         │   │ │ │
│ │ │ Width:  [8.5] inches           │ │                         │   │ │ │
│ │ │ Height: [11] inches            │ │  [Content Area]          │   │ │ │
│ │ │ DPI:    [300]                 │ │                         │   │ │ │
│ │ │                                │ │                         │   │ │ │
│ │ │                                │ └─────────────────────────┘   │ │ │
│ │ │                                │ 8.5×11in portrait              │ │ │
│ │ └───────────────────────────────┴─────────────────────────────────┘ │ │
│ │                                                                     │ │ │
│ │ [Cancel] [Next →]                                                   │ │ │
│ └─────────────────────────────────────────────────────────────────────┘ │ │
│                                                                           │ │
└─────────────────────────────────────────────────────────────────────────┘ │
```

**UI State (Wizard Option - Step 2: Margins):**
```
┌─────────────────────────────────────────────────────────────────────────┐
│ Create Page Template                                                    │
├─────────────────────────────────────────────────────────────────────────┤ │
│                                                                           │ │
│ Step 2 of 4: Margins                                                     │ │
│ ┌─────────────────────────────────────────────────────────────────────┐ │ │
│ │ ┌───────────────────────────────┬─────────────────────────────────┐ │ │
│ │ │ Settings                       │ Preview                        │ │ │
│ │ ├───────────────────────────────┼─────────────────────────────────┤ │ │
│ │ │ Margins:                       │ ┌─────────────────────────┐   │ │ │
│ │ │ • Top:    [0.5] inches         │ │ ┌─────────────────────┐ │   │ │ │
│ │ │ • Right:  [0.5] inches         │ │ │                     │ │   │ │ │
│ │ │ • Bottom: [0.5] inches         │ │ │  Content Area        │ │   │ │ │
│ │ │ • Left:   [0.5] inches         │ │ │  (after margins)     │ │   │ │ │
│ │ │                                │ │ │                     │ │   │ │ │
│ │ │ [Uniform Margins]              │ │ └─────────────────────┘ │   │ │ │
│ │ │                                │ └─────────────────────────┘   │ │ │
│ │ │                                │ Margins shown in preview      │ │ │
│ │ └───────────────────────────────┴─────────────────────────────────┘ │ │
│ │                                                                     │ │ │
│ │ [← Back] [Next →]                                                  │ │ │
│ └─────────────────────────────────────────────────────────────────────┘ │ │
│                                                                           │ │
└─────────────────────────────────────────────────────────────────────────┘ │
```

**UI State (Wizard Option - Step 3: Spread Configuration):**
```
┌─────────────────────────────────────────────────────────────────────────┐
│ Create Page Template                                                    │
├─────────────────────────────────────────────────────────────────────────┤ │
│                                                                           │ │
│ Step 3 of 4: Spread Configuration                                        │ │
│ ┌─────────────────────────────────────────────────────────────────────┐ │ │
│ │ ┌───────────────────────────────┬─────────────────────────────────┐ │ │
│ │ │ Settings                       │ Preview                        │ │ │
│ │ ├───────────────────────────────┼─────────────────────────────────┤ │ │
│ │ │ • [✓] Single Page              │ ┌─────────────────────────┐   │ │ │
│ │ │ • [ ] Spread (2-page)          │ │                         │   │ │ │
│ │ │                                │ │    Single Page          │   │ │ │
│ │ │ Gutter Width: [0.5] inches     │ │                         │   │ │ │
│ │ │ (only if spread)               │ │  [Content Area]         │   │ │ │
│ │ │                                │ │                         │   │ │ │
│ │ │                                │ └─────────────────────────┘   │ │ │
│ │ │                                │                                │ │ │
│ │ │ When "Spread" selected:        │ ┌─────────────────────────┐   │ │ │
│ │ │                                │ │ Left Page │ Right Page   │   │ │ │
│ │ │                                │ │          │              │   │ │ │
│ │ │                                │ │ [Content]│ [Content]    │   │ │ │
│ │ │                                │ │          │              │   │ │ │
│ │ │                                │ └─────────────────────────┘   │ │ │
│ │ │                                │ Gutter shown between pages    │ │ │
│ │ └───────────────────────────────┴─────────────────────────────────┘ │ │
│ │                                                                     │ │ │
│ │ [← Back] [Next →]                                                  │ │ │
│ └─────────────────────────────────────────────────────────────────────┘ │ │
│                                                                           │ │
└─────────────────────────────────────────────────────────────────────────┘ │
```

**UI State (Form Option):**
```
┌─────────────────────────────────────────────────────────────────────────┐
│ Create Page Template                                                    │
├─────────────────────────────────────────────────────────────────────────┤ │
│                                                                           │ │
│ ┌─────────────────────────────────────────────────────────────────────┐ │ │
│ │ Template Name: [Single Page Portrait]                                │ │ │
│ │ Description:  [Standard 8.5x11in portrait page]                     │ │ │
│ │                                                                     │ │ │
│ │ ┌───────────────────────────────┬─────────────────────────────────┐ │ │
│ │ │ Settings                       │ Preview                        │ │ │
│ │ ├───────────────────────────────┼─────────────────────────────────┤ │ │
│ │ │ Page Dimensions:               │ ┌─────────────────────────┐   │ │ │
│ │ │ • Width:  [8.5] inches         │ │                         │   │ │ │
│ │ │ • Height: [11] inches          │ │                         │   │ │ │
│ │ │ • DPI:    [300]                │ │    Page Template        │   │ │ │
│ │ │                                │ │    Preview               │   │ │ │
│ │ │ Margins:                       │ │                         │   │ │ │
│ │ │ • Top:    [0.5] inches         │ │  [Content Area]          │   │ │ │
│ │ │ • Right:  [0.5] inches         │ │                         │   │ │ │
│ │ │ • Bottom: [0.5] inches         │ │                         │   │ │ │
│ │ │ • Left:   [0.5] inches         │ │                         │   │ │ │
│ │ │                                │ └─────────────────────────┘   │ │ │
│ │ │ Spread Configuration:          │                                │ │ │
│ │ │ • [✓] Single Page              │ Page: 8.5×11in portrait       │ │ │
│ │ │ • [ ] Spread (2-page)          │ Content: 7.5×10in             │ │ │
│ │ │ • Gutter: [0.5] inches          │ Mode: Fill                    │ │ │
│ │ │                                │                                │ │ │
│ │ │ Positioning Mode:               │ Updates in real-time as       │ │ │
│ │ │ • [✓] Fill (scale-cover)        │ settings change               │ │ │
│ │ │ • [ ] Absolute                  │                                │ │ │
│ │ │ • [ ] Snap                      │                                │ │ │
│ │ │                                │                                │ │ │
│ │ │ Border (Optional):               │                                │ │ │
│ │ │ • [ ] Enable Border             │                                │ │ │
│ │ │ • Color: [#000000]              │                                │ │ │
│ │ │ • Type: [Plain ▼]               │                                │ │ │
│ │ │                                │                                │ │ │
│ │ │ Image Requirements:             │                                │ │ │
│ │ │ • Orientation: Portrait          │                                │ │ │
│ │ │ • Content Area: 7.5×10in        │                                │ │ │
│ │ │ • Works with: Any aspect ratio   │                                │ │ │
│ │ └───────────────────────────────┴─────────────────────────────────┘ │ │
│ │                                                                     │ │ │
│ │ [Cancel] [Create Template]                                          │ │ │
│ └─────────────────────────────────────────────────────────────────────┘ │ │
│                                                                           │ │
└─────────────────────────────────────────────────────────────────────────┘ │
```

**UI State (Spread Template Preview):**
```
┌─────────────────────────────────────────────────────────────────────────┐
│ Create Page Template                                                    │
├─────────────────────────────────────────────────────────────────────────┤ │
│                                                                           │ │
│ ┌─────────────────────────────────────────────────────────────────────┐ │ │
│ │ Template Name: [Two-Page Spread]                                    │ │ │
│ │ Description:  [11x17in landscape spread]                            │ │ │
│ │                                                                     │ │ │
│ │ ┌───────────────────────────────┬─────────────────────────────────┐ │ │
│ │ │ Settings                       │ Preview                        │ │ │
│ │ ├───────────────────────────────┼─────────────────────────────────┤ │ │
│ │ │ Page Dimensions:               │ ┌─────────────────────────┐   │ │ │
│ │ │ • Width:  [17] inches           │ │                         │   │ │ │
│ │ │ • Height: [11] inches           │ │    Left Page │ Right    │   │ │ │
│ │ │ • DPI:    [300]                 │ │             │ Page      │   │ │ │
│ │ │                                │ │  [Content] │ [Content] │   │ │ │
│ │ │ Margins:                       │ │             │           │   │ │ │
│ │ │ • Top:    [0.5] inches          │ │             │           │   │ │ │
│ │ │ • Right:  [0.5] inches         │ │             │           │   │ │ │
│ │ │ • Bottom: [0.5] inches         │ │             │           │   │ │ │
│ │ │ • Left:   [0.5] inches         │ │             │           │   │ │ │
│ │ │                                │ └─────────────────────────┘   │ │ │
│ │ │ Spread Configuration:          │                                │ │ │
│ │ │ • [ ] Single Page              │ Spread: 17×11in landscape     │ │ │
│ │ │ • [✓] Spread (2-page)          │ Gutter: 0.5in (150px)          │ │ │
│ │ │ • Gutter: [0.5] inches          │ Left: 8.25in, Right: 8.25in    │ │ │
│ │ │                                │                                │ │ │
│ │ │ Positioning Mode:               │ Updates in real-time as       │ │ │
│ │ │ • [✓] Fill (scale-cover)        │ settings change               │ │ │
│ │ │ • [ ] Absolute                  │                                │ │ │
│ │ │ • [ ] Snap                      │                                │ │ │
│ │ │                                │                                │ │ │
│ │ │ Border (Optional):               │                                │ │ │
│ │ │ • [ ] Enable Border             │                                │ │ │
│ │ │ • Color: [#000000]              │                                │ │ │
│ │ │ • Type: [Plain ▼]               │                                │ │ │
│ │ │                                │                                │ │ │
│ │ │ Image Requirements:             │                                │ │ │
│ │ │ • Orientation: Landscape       │                                │ │ │
│ │ │ • Content Area: 16×10in         │                                │ │ │
│ │ │ • Works with: Wide images        │                                │ │ │
│ │ └───────────────────────────────┴─────────────────────────────────┘ │ │
│ │                                                                     │ │ │
│ │ [Cancel] [Create Template]                                          │ │ │
│ └─────────────────────────────────────────────────────────────────────┘ │ │
│                                                                           │ │
└─────────────────────────────────────────────────────────────────────────┘ │
```

**API Calls:**
- **Preview computation (client-side):** As user changes settings, preview updates in real-time
  - Computes content area from page dimensions and margins
  - Shows visual representation of page layout
  - For spreads: Shows left/right page split with gutter
  - Updates immediately as settings change (no API call needed)
- **Template creation:** No API call until user submits form

---

### Step 2: User Submits Template Creation

**User Action:** Click "[Create Template]" button

**UI State (Immediate - Optimistic Update):**
```
┌─────────────────────────────────────────────────────────────────────────┐
│ Templates:                                                                │
│ ┌──────────────────────┐  ┌──────────────────────┐                      │
│ │ 📄 Single Page       │  │ 📄 Spread (2-page)   │                      │
│ │ 8.5x11in portrait    │  │ 11x17in landscape    │                      │
│ └──────────────────────┘  └──────────────────────┘                      │
│ ┌──────────────────────┐  ┌──────────────────────┐                      │
│ │ 📄 Square Page        │  │ 📄 Custom Template ✓ │                      │
│ │ 8x8in square          │  │ Custom settings      │                      │
│ └──────────────────────┘  └──────────────────────┘                      │
│                                                                           │
│ ✓ Template created (syncing...)  ← Toast notification                   │
│                                                                           │
└─────────────────────────────────────────────────────────────────────────┘
```

**API Calls:**
- `POST /projects/{projectId}/page-templates` — Create project template
  - **Params:**
    - `projectId` (path parameter)
    - `name` (body parameter): `string` (required)
    - `description` (body parameter): `string` (optional)
    - `template` (body parameter): `object` (required) - `PageLayoutSettings` JSON:
      ```json
      {
        "pageWidthIn": 8.5,
        "pageHeightIn": 11.0,
        "dpi": 300,
        "marginTopIn": 0.5,
        "marginRightIn": 0.5,
        "marginBottomIn": 0.5,
        "marginLeftIn": 0.5,
        "isSpread": false,
        "gutterWidthIn": 0.0,
        "gutterOverlapIn": 0.0,
        "positioningMode": "fill",
        "anchorPreset": "",
        "imageXIn": 0.0,
        "imageYIn": 0.0,
        "imageWidthIn": 0.0,
        "imageHeightIn": 0.0,
        "borderEnabled": false,
        "borderColor": "",
        "borderType": "plain"
      }
      ```
  - **Response:** `{ page_template: PageTemplate }`
  - **Note:** Optimistic update appears immediately, API call happens in background
- **Alternative:** `POST /page-templates` — Create global template
  - **Params:** Same as above (no `projectId` in path)
  - **Response:** `{ page_template: PageTemplate }`

---

## How Page Templates Relate to Image Requirements

**Key Insight:** Page templates implicitly define image requirements through their settings. Understanding this relationship helps users select compatible images.

### Page Dimensions → Image Orientation

- **Portrait pages (8.5×11in):** Work best with portrait-oriented images (3:4, 2:3 aspect ratios)
- **Landscape pages (11×8.5in):** Work best with landscape-oriented images (4:3, 3:2 aspect ratios)
- **Square pages (8×8in):** Work best with square images (1:1 aspect ratio)
- **Spread pages (17×11in):** Work best with wide images or panoramic compositions

### Content Area → Usable Space

- **Content area = Page dimensions - Margins**
- Example: 8.5×11in page with 0.5in margins = 7.5×10in content area
- Larger margins = smaller content area = images need to be cropped more
- Smaller margins = larger content area = images can be displayed larger

### Positioning Mode → Image Compatibility

- **`"fill"` mode (default):**
  - Works with any aspect ratio
  - Scales image to cover entire content area
  - Preserves aspect ratio (crops to fit)
  - Best for: Flexible image selection, automatic cropping
  
- **`"absolute"` mode:**
  - Requires exact image dimensions (`ImageWidthIn`, `ImageHeightIn`)
  - Places image at exact coordinates (`ImageXIn`, `ImageYIn`)
  - Best for: Precise control, specific image sizes

- **`"snap"` mode:**
  - Currently treated as alias for `"fill"`
  - Reserved for future anchor presets (center, top-left, etc.)

### Spread Configuration → Image Composition

- **Single page:** Standard image placement
- **Spread (2-page):** Requires wider images or different composition
- **Gutter:** Binding area between pages (affects how images are split)

### Visual Indicators in UI

Templates should show:
- **Orientation indicator:** Portrait/Landscape/Square icon
- **Content area preview:** Visual representation of usable space
- **Image compatibility:** "Works with: Any aspect ratio" or "Requires: Portrait images"
- **Positioning mode:** "Fill mode (auto-crop)" or "Absolute placement"

---

## Template Selection and Preview

### Step 3: User Selects Template

**User Action:** Click on "Single Page" template

**UI State:**
```
┌─────────────────────────────────────────────────────────────────────────┐
│ Templates:                                                                │
│ ┌──────────────────────┐  ┌──────────────────────┐                      │
│ │ 📄 Single Page  ✓     │  │ 📄 Spread (2-page)   │                      │
│ │ 8.5x11in portrait     │  │ 11x17in landscape    │                      │
│ └──────────────────────┘  └──────────────────────┘                      │
│                                                                           │
│ Selected Template: Single Page                                           │
│ ┌─────────────────────────────────────────────────────────────────────┐ │
│ │ Template Preview:                                                   │ │
│ │ ┌─────────────┐                                                     │ │
│ │ │             │                                                     │ │
│ │ │  Page       │  ← Visual preview of template                      │ │
│ │ │  Layout     │                                                     │ │
│ │ │             │                                                     │ │
│ │ └─────────────┘                                                     │ │
│ │                                                                     │ │
│ │ Settings (PageLayoutSettings):                                      │ │
│ │ • Size: 8.5×11in portrait (2550×3300px @ 300 DPI)                 │ │
│ │ • Margins: 0.5in all sides (150px)                                  │ │
│ │ • Content Area: 7.5×10in (2250×3000px)                              │ │
│ │ • Spread Mode: Single page                                         │ │
│ │ • Positioning: Fill (scale-cover, preserves aspect ratio)          │ │
│ │                                                                     │ │
│ │ Image Requirements:                                                 │ │
│ │ • Orientation: Portrait (works best with portrait images)           │ │
│ │ • Aspect Ratio: Any (fill mode crops to fit content area)           │ │
│ │ • Recommended: Portrait-oriented images (3:4, 2:3, etc.)            │ │
│ └─────────────────────────────────────────────────────────────────────┘ │
│                                                                           │
└─────────────────────────────────────────────────────────────────────────┘
```

**API Calls:**
- `GET /page-templates/{templateId}` — Get template details (optional, if not already cached)
  - **Params:** `templateId` (path parameter)
  - **Response:** `{ page_template: PageTemplate }`
  - **Note:** May be cached from initial template list query

---

### Step 4: User Selects Laid-Out Image

**User Action:** Click on IMG-001 (laid-out image)

**UI State:**
```
┌─────────────────────────────────────────────────────────────────────────┐
│ Available Laid-Out Images:                                              │
│ ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐           │
│ │ IMG-001 │ │ IMG-002 │ │ IMG-003 │ │ IMG-004 │ │ IMG-005 │           │
│ │(cropped)│ │(cropped)│ │(cropped)│ │(cropped)│ │(cropped)│           │
│ │   ✓     │ │         │ │         │ │         │ │         │           │
│ └─────────┘ └─────────┘ └─────────┘ └─────────┘ └─────────┘           │
│                                                                           │
│ Preview:                                                                  │
│ ┌─────────────────────────────────────────────────────────────────────┐ │
│ │ Template: Single Page                                               │ │
│ │ Image: IMG-001                                                      │ │
│ ├─────────────────────────────────────────────────────────────────────┤ │
│ │                                                                     │ │
│ │ ┌─────────────┐                                                     │ │
│ │ │             │                                                     │ │
│ │ │  [Preview]  │  ← Preview of how template will look with image   │ │
│ │ │             │                                                     │ │
│ │ └─────────────┘                                                     │ │
│ │                                                                     │ │
│ │ Template Settings:                                                   │ │
│ │ • Page: 8.5×11in portrait                                          │ │
│ │ • Content Area: 7.5×10in (after 0.5in margins)                     │ │
│ │ • Positioning: Fill mode (scale-cover, preserves aspect ratio)      │ │
│ │                                                                     │ │
│ │ Image Compatibility:                                                │ │
│ │ • IMG-001: Portrait (3:4 ratio) ✓ Compatible                       │ │
│ │ • Fill mode will scale image to cover content area                  │ │
│ │ • Image may be cropped to fit (expected behavior)                   │ │
│ │                                                                     │ │
│ │ [Cancel] [Create Page]                                              │ │
│ └─────────────────────────────────────────────────────────────────────┘ │
│                                                                           │
└─────────────────────────────────────────────────────────────────────────┘
```

**API Calls:**
- `GET /laid-out-pages/preview` — Preview page layout without creating (optional)
  - **Params:**
    - `page_template_id` (query parameter): `string` (required)
    - `laid_out_image_id` (query parameter): `string` (required)
  - **Response:** `{ preview: { layout: PageLayoutPreview, preview_image_url: string } }`
  - **Note:** Can be computed client-side instead of API call for faster preview
  - **Alternative:** Compute client-side using template settings and laid-out image crop zones (no API call)

---

## Assigning Template to Page (Optimistic Update)

### Step 5: User Creates Page

**User Action:** Click "[Create Page]" button

**UI State (Immediate - Optimistic Update):**
```
┌─────────────────────────────────────────────────────────────────────────┐
│ Laid-Out Pages:                                                          │
├─────────────────────────────────────────────────────────────────────────┤ │
│                                                                           │ │
│ ┌─────────────────────────────────────────────────────────────────────┐ │ │
│ │ ┌─────────────┐                                                     │ │ │
│ │ │             │                                                     │ │ │
│ │ │  [Page]     │  ← Optimistic page (placeholder)                  │ │ │
│ │ │  Preview    │                                                     │ │ │
│ │ │             │                                                     │ │ │
│ │ └─────────────┘                                                     │ │ │
│ │ Single Page • IMG-001                                                │ │ │
│ │ ⏳ Rendering...  ← Loading indicator                                 │ │ │
│ └─────────────────────────────────────────────────────────────────────┘ │ │
│                                                                           │ │
│ ✓ Page created (rendering...)  ← Toast notification                     │ │
│                                                                           │ │
└─────────────────────────────────────────────────────────────────────────┘ │
```

**API Calls:**
- `POST /projects/{projectId}/laid-out-pages` — Create laid-out page
  - **Params:**
    - `projectId` (path parameter)
    - `page_template_id` (body parameter): `string` (required)
    - `laid_out_image_id` (body parameter): `string` (required)
  - **Response:** `{ laid_out_page: LaidOutPage }`
    - **Response includes:** Page ID, template ID, laid-out image ID
    - **Note:** Page rendering happens asynchronously (doesn't block API call)
  - **Note:** Optimistic update appears immediately, API call happens in background

---

### Step 6: Page Rendering (Asynchronous)

**UI State (Rendering in Progress):**
```
┌─────────────────────────────────────────────────────────────────────────┐
│ Laid-Out Pages:                                                          │
├─────────────────────────────────────────────────────────────────────────┤ │
│                                                                           │ │
│ ┌─────────────────────────────────────────────────────────────────────┐ │ │
│ │ ┌─────────────┐                                                     │ │ │
│ │ │             │                                                     │ │ │
│ │ │  [Page]     │  ← Placeholder (rendering in background)           │ │ │
│ │ │  Preview    │                                                     │ │ │
│ │ │             │                                                     │ │ │
│ │ └─────────────┘                                                     │ │ │
│ │ Single Page • IMG-001                                                │ │ │
│ │ ⏳ Rendering... (this may take a moment)                             │ │ │
│ └─────────────────────────────────────────────────────────────────────┘ │ │
│                                                                           │ │
│ ✓ Page created (rendering...)  ← Toast notification                     │ │
│                                                                           │ │
└─────────────────────────────────────────────────────────────────────────┘ │
```

**API Calls:**
- Same as previous operation (API call completed, rendering in progress)
  - **Status:** `201 Created`
  - **Response:** `{ laid_out_page: LaidOutPage }`
  - **Note:** Page rendering happens asynchronously in background
  - **Note:** Frontend polls preview endpoint or uses websocket to get rendered image

---

### Step 7: Page Preview Ready

**UI State (After Rendering Complete):**
```
┌─────────────────────────────────────────────────────────────────────────┐
│ Laid-Out Pages:                                                          │
├─────────────────────────────────────────────────────────────────────────┤ │
│                                                                           │ │
│ ┌─────────────────────────────────────────────────────────────────────┐ │ │
│ │ ┌─────────────┐                                                     │ │ │
│ │ │             │                                                     │ │ │
│ │ │ [Rendered]  │  ← Backend-rendered page (high quality)            │ │ │
│ │ │   Page      │                                                     │ │ │
│ │ │             │                                                     │ │ │
│ │ └─────────────┘                                                     │ │ │
│ │ Single Page • IMG-001                                                │ │ │
│ │ ✓ Complete                                                           │ │ │
│ └─────────────────────────────────────────────────────────────────────┘ │ │
│                                                                           │ │
│ ✓ Page created successfully  ← Toast notification (success)             │ │
│                                                                           │ │
└─────────────────────────────────────────────────────────────────────────┘ │
```

**API Calls:**
- `GET /laid-out-pages/{pageId}/preview` — Get rendered page preview
  - **Params:** 
    - `pageId` (path parameter)
    - `variant` (query parameter): `string` (optional, default: "thumbnail")
      - Options: `thumbnail`, `combined`, `left`, `right`, `full`
  - **Response:** Image file (PNG/JPEG) or `{ image_url: string, status: 'ready' }`
  - **Note:** Polled after page creation, or use websocket for real-time updates
  - **Alternative:** Websocket event `laid-out-page-rendered` with `{ id: string, image_url: string, variants: { thumbnail: string, combined: string, ... } }`

---

## Batch Operations

### Assigning Template to Multiple Pages

**User Action:** Select multiple laid-out images (IMG-001, IMG-002, IMG-003), then click "[Create Pages with Template]"

**UI State (Immediate - Optimistic Updates):**
```
┌─────────────────────────────────────────────────────────────────────────┐
│ Available Laid-Out Images:                                              │
│ ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐           │
│ │ IMG-001 │ │ IMG-002 │ │ IMG-003 │ │ IMG-004 │ │ IMG-005 │           │
│ │(cropped)│ │(cropped)│ │(cropped)│ │(cropped)│ │(cropped)│           │
│ │   ✓     │ │   ✓     │ │   ✓     │ │         │ │         │           │
│ └─────────┘ └─────────┘ └─────────┘ └─────────┘ └─────────┘           │
│                                                                           │
│ Laid-Out Pages:                                                          │
│ ┌─────────────────────────────────────────────────────────────────────┐ │
│ │ ┌─────────┐ ┌─────────┐ ┌─────────┐                                │ │
│ │ │ [Page-1]│ │ [Page-2]│ │ [Page-3]│  ← Optimistic pages (instant) │ │
│ │ └─────────┘ └─────────┘ └─────────┘                                │ │
│ │ Single Page Single Page Single Page                                 │ │
│ │ ⏳ Rendering...                                                      │ │
│ └─────────────────────────────────────────────────────────────────────┘ │
│                                                                           │
│ ✓ Creating 3 pages (rendering...)  ← Toast notification                │
│                                                                           │
└─────────────────────────────────────────────────────────────────────────┘
```

**API Calls:**
- `POST /projects/{projectId}/laid-out-pages/batch` — Create multiple laid-out pages
  - **Params:**
    - `projectId` (path parameter)
    - `page_template_id` (body parameter): `string` (required)
    - `laid_out_image_ids` (body parameter): `Array<string>` (required)
  - **Response:** `{ laid_out_pages: LaidOutPage[] }`
    - **Response includes:** Array of page IDs, template ID, laid-out image IDs
    - **Note:** Page rendering happens asynchronously for all pages (doesn't block API call)
  - **Note:** Optimistic updates appear immediately for all pages, single API call for batch

**After Server Response (Pages Created, Rendering in Progress):**
```
┌─────────────────────────────────────────────────────────────────────────┐
│ Laid-Out Pages:                                                          │
├─────────────────────────────────────────────────────────────────────────┤ │
│                                                                           │ │
│ ┌─────────────────────────────────────────────────────────────────────┐ │ │
│ │ ┌─────────┐ ┌─────────┐ ┌─────────┐                                │ │ │
│ │ │ [Page-1]│ │ [Page-2]│ │ [Page-3]│  ← Placeholders (rendering)    │ │ │
│ │ └─────────┘ └─────────┘ └─────────┘                                │ │ │
│ │ Single Page Single Page Single Page                                 │ │ │
│ │ ⏳ Rendering... (this may take a moment)                             │ │ │
│ └─────────────────────────────────────────────────────────────────────┘ │ │
│                                                                           │ │
│ ✓ Created 3 pages (rendering...)  ← Toast notification                  │ │
│                                                                           │ │
└─────────────────────────────────────────────────────────────────────────┘ │
```

**After Pages Rendered (All Preview URLs Available):**
```
┌─────────────────────────────────────────────────────────────────────────┐
│ Laid-Out Pages:                                                          │
├─────────────────────────────────────────────────────────────────────────┤ │
│                                                                           │ │
│ ┌─────────────────────────────────────────────────────────────────────┐ │ │
│ │ ┌─────────┐ ┌─────────┐ ┌─────────┐                                │ │ │
│ │ │[Rendered]│ │[Rendered]│ │[Rendered]│  ← Backend-rendered pages   │ │ │
│ │ └─────────┘ └─────────┘ └─────────┘                                │ │ │
│ │ Single Page Single Page Single Page                                 │ │ │
│ │ ✓ Complete                                                           │ │ │
│ └─────────────────────────────────────────────────────────────────────┘ │ │
│                                                                           │ │
│ ✓ Created 3 pages successfully  ← Toast notification                   │ │
│                                                                           │ │
└─────────────────────────────────────────────────────────────────────────┘ │
```

**API Calls:**
- `GET /laid-out-pages/{pageId}/preview` — Get rendered page previews (multiple calls, one per page)
  - **Params:** 
    - `pageId` (path parameter) - called for each page in batch
    - `variant` (query parameter): `string` (optional, default: "thumbnail")
  - **Response:** Image file (PNG/JPEG) or `{ image_url: string, status: 'ready' }`
  - **Note:** Polled for each page after batch creation, or use websocket for real-time updates
  - **Alternative:** Websocket events `laid-out-page-rendered` with `{ id: string, image_url: string }` for each page

---

## Error Handling

### Error During Page Creation

**UI State (On Error - Rollback):**
```
┌─────────────────────────────────────────────────────────────────────────┐
│ Laid-Out Pages:                                                          │
├─────────────────────────────────────────────────────────────────────────┤ │
│                                                                           │ │
│ ┌─────────────────────────────────────────────────────────────────────┐ │ │
│ │ [No pages]  ← Rolled back to previous state                         │ │ │
│ └─────────────────────────────────────────────────────────────────────┘ │ │
│                                                                           │ │
│ ┌─────────────────────────────────────────────────────────────────────┐ │ │
│ │ ✗ Failed to create page. Please try again.  ← Error toast (red)  │ │ │
│ │ [Retry]                                                              │ │ │
│ └─────────────────────────────────────────────────────────────────────┘ │ │
│                                                                           │ │
└─────────────────────────────────────────────────────────────────────────┘ │
```

**API Calls:**
- `POST /projects/{projectId}/laid-out-pages` — Error response
  - **Status:** `500 Internal Server Error` or network error
  - **Error Response:** `{ error: string }`
  - **Note:** RTK Query automatically rolls back optimistic update via `patchResult.undo()`

---

### Error During Page Rendering

**UI State (Rendering Fails, Keep Placeholder):**
```
┌─────────────────────────────────────────────────────────────────────────┐
│ Laid-Out Pages:                                                          │
├─────────────────────────────────────────────────────────────────────────┤ │
│                                                                           │ │
│ ┌─────────────────────────────────────────────────────────────────────┐ │ │
│ │ ┌─────────────┐                                                     │ │ │
│ │ │             │                                                     │ │ │
│ │ │  [Page]     │  ← Placeholder kept (fallback)                     │ │ │
│ │ │  Preview    │                                                     │ │ │
│ │ │             │                                                     │ │ │
│ │ └─────────────┘                                                     │ │ │
│ │ Single Page • IMG-001                                                │ │ │
│ │ ⚠ Rendering failed (page created, preview unavailable)                │ │ │
│ └─────────────────────────────────────────────────────────────────────┘ │ │
│                                                                           │ │
│ ⚠ Page rendering failed, page created but preview unavailable  ← Warning │ │
│                                                                           │ │
└─────────────────────────────────────────────────────────────────────────┘ │
```

**API Calls:**
- `GET /laid-out-pages/{pageId}/preview` — Error or timeout
  - **Status:** `500 Internal Server Error`, `404 Not Found`, or timeout
  - **Error Response:** `{ error: string }`
  - **Note:** Fallback to placeholder if rendering fails (page still exists, can retry rendering)

---

## Template Preview (Before Assigning)

### Preview Workflow

**User Action:** Select template and laid-out image, preview appears automatically

**UI State:**
```
┌─────────────────────────────────────────────────────────────────────────┐
│ Template: Single Page                                                    │
│ Image: IMG-001                                                           │
├─────────────────────────────────────────────────────────────────────────┤ │
│                                                                           │ │
│ ┌─────────────────────────────────────────────────────────────────────┐ │ │
│ │ Preview (Before Creating):                                          │ │ │
│ │ ┌─────────────┐                                                     │ │ │
│ │ │             │                                                     │ │ │
│ │ │  [Preview]  │  ← Computed page layout preview                    │ │ │
│ │ │             │                                                     │ │ │
│ │ └─────────────┘                                                     │ │ │
│ │                                                                     │ │ │
│ │ Layout:                                                              │ │ │
│ │ • Page Size: 8.5x11in portrait                                      │ │ │
│ │ • Margins: 0.5in all sides                                          │ │ │
│ │ • Image Position: Fill (centered)                                    │ │ │
│ │                                                                     │ │ │
│ │ [Cancel] [Create Page]                                              │ │ │
│ └─────────────────────────────────────────────────────────────────────┘ │ │
│                                                                           │ │
└─────────────────────────────────────────────────────────────────────────┘ │
```

**API Calls:**
- `GET /laid-out-pages/preview` — Preview page layout without creating (optional)
  - **Params:**
    - `page_template_id` (query parameter): `string` (required)
    - `laid_out_image_id` (query parameter): `string` (required)
  - **Response:** `{ preview: { layout: PageLayoutPreview, preview_image_url: string } }`
  - **Alternative:** Compute client-side using template settings and laid-out image crop zones (no API call)
  - **Note:** Preview does not create `LaidOutPage` record, just computes layout

---

## Template Reuse

### Using Existing Templates

**UI State:**
```
┌─────────────────────────────────────────────────────────────────────────┐
│ Templates:                                                                │
│ ┌──────────────────────┐  ┌──────────────────────┐                      │
│ │ 📄 Single Page       │  │ 📄 Spread (2-page)    │                      │
│ │ 8.5x11in portrait    │  │ 11x17in landscape    │                      │
│ │ Used 12 times        │  │ Used 8 times          │                      │
│ └──────────────────────┘  └──────────────────────┘                      │
│ ┌──────────────────────┐  ┌──────────────────────┐                      │
│ │ 📄 Square Page        │  │ 📄 Project Template  │                      │
│ │ 8x8in square          │  │ Project-specific      │                      │
│ │ Used 5 times          │  │ Used 3 times          │                      │
│ └──────────────────────┘  └──────────────────────┘                      │
│                                                                           │
│ Global Templates:                                                        │
│ ┌──────────────────────┐  ┌──────────────────────┐                      │
│ │ 📄 Standard Single   │  │ 📄 Standard Spread    │                      │
│ │ Available to all     │  │ Available to all     │                      │
│ └──────────────────────┘  └──────────────────────┘                      │
│                                                                           │
└─────────────────────────────────────────────────────────────────────────┘
```

**API Calls:**
- `GET /page-templates` — List global templates (already called on initial load)
  - **Params:** None
  - **Response:** `{ page_templates: PageTemplate[] }`
- `GET /projects/{projectId}/page-templates` — List project templates (already called on initial load)
  - **Params:** `projectId` (path parameter)
  - **Response:** `{ page_templates: PageTemplate[] }`
- **Note:** Templates are cached, no additional API calls needed for reuse

---

## Updating Page Assignment

### Changing Image on Existing Page

**User Action:** Click on existing page, click "[Change Image]" button

**UI State:**
```
┌─────────────────────────────────────────────────────────────────────────┐
│ Laid-Out Pages:                                                          │
├─────────────────────────────────────────────────────────────────────────┤ │
│                                                                           │ │
│ ┌─────────────────────────────────────────────────────────────────────┐ │ │
│ │ ┌─────────────┐                                                     │ │ │
│ │ │             │                                                     │ │ │
│ │ │ [Rendered]  │  ← Selected page                                   │ │ │
│ │ │   Page      │                                                     │ │ │
│ │ │             │                                                     │ │ │
│ │ └─────────────┘                                                     │ │ │
│ │ Single Page • IMG-001                                                │ │ │
│ │                                                                     │ │ │
│ │ [Change Image] [Delete Page]                                        │ │ │
│ └─────────────────────────────────────────────────────────────────────┘ │ │
│                                                                           │ │
│ ┌─────────────────────────────────────────────────────────────────────┐ │ │
│ │ Select New Image:                                                    │ │ │
│ │ ┌─────────┐ ┌─────────┐ ┌─────────┐                              │ │ │
│ │ │ IMG-002 │ │ IMG-003 │ │ IMG-004 │                              │ │ │
│ │ └─────────┘ └─────────┘ └─────────┘                              │ │ │
│ │                                                                     │ │ │
│ │ [Cancel] [Update Page]                                              │ │ │
│ └─────────────────────────────────────────────────────────────────────┘ │ │
│                                                                           │ │
└─────────────────────────────────────────────────────────────────────────┘ │
```

**After Selection (Optimistic Update):**
```
┌─────────────────────────────────────────────────────────────────────────┐
│ Laid-Out Pages:                                                          │
├─────────────────────────────────────────────────────────────────────────┤ │
│                                                                           │ │
│ ┌─────────────────────────────────────────────────────────────────────┐ │ │
│ │ ┌─────────────┐                                                     │ │ │
│ │ │             │                                                     │ │ │
│ │ │  [Page]     │  ← Updated with new image (optimistic)             │ │ │
│ │ │  Preview    │                                                     │ │ │
│ │ │             │                                                     │ │ │
│ │ └─────────────┘                                                     │ │ │
│ │ Single Page • IMG-002                                                │ │ │
│ │ ⏳ Rendering...                                                      │ │ │
│ └─────────────────────────────────────────────────────────────────────┘ │ │
│                                                                           │ │
│ ✓ Page updated (rendering...)  ← Toast notification                     │ │
│                                                                           │ │
└─────────────────────────────────────────────────────────────────────────┘ │
```

**API Calls:**
- `PATCH /laid-out-pages/{pageId}` — Update laid-out page image
  - **Params:**
    - `pageId` (path parameter)
    - `laid_out_image_id` (body parameter): `string` (required)
  - **Response:** `{ laid_out_page: LaidOutPage }`
    - **Note:** Page re-rendering happens asynchronously (doesn't block API call)
  - **Note:** Optimistic update appears immediately, API call happens in background

---

## Summary: Key UX Patterns

### 1. Visual Template Creation with Live Preview
- **What:** Create templates visually (wizard or form with presets) with live preview
- **Why:** Clear, confident creation, supports experimentation, see results immediately
- **How:** Visual wizard or form, live preview updates as settings change, presets for quick start
- **Preview shows:** Page layout, content area, margins, spread split (if spread), border (if enabled)

### 2. Visual Template Selection
- **What:** Show templates as thumbnails/preview cards, not just dropdowns
- **Why:** Clear, confident selection, supports experimentation
- **How:** Visual template cards with preview and settings

### 3. Preview Before Assigning
- **What:** Show how template will look on page before creating
- **Why:** Reduces errors, builds confidence, supports experimentation
- **How:** Compute page layout without creating `LaidOutPage` record

### 4. Optimistic Updates
- **What:** Page appears immediately, rendering happens in background
- **Why:** Fast UI, feels instant, better UX
- **How:** RTK Query `onQueryStarted` updates cache before API call

### 5. Asynchronous Rendering
- **What:** Page rendering happens in background, preview available when ready
- **Why:** Fast UI, doesn't block API call, better UX
- **How:** Render pages asynchronously, poll preview endpoint or use websocket

### 6. Batch Operations
- **What:** Assign template to multiple pages at once
- **Why:** Efficient, professional workflow, fewer API calls
- **How:** Batch API endpoint, optimistic updates for all pages

### 7. Template Reuse
- **What:** Create templates once, use many times
- **Why:** Efficient, consistent, professional workflow
- **How:** Template management, global and project-specific templates

---

## Technical Implementation Notes

### RTK Query Mutation Pattern (Page Creation)
```typescript
createLaidOutPage: builder.mutation({
  query: ({ projectId, pageTemplateId, laidOutImageId }) => ({
    url: `/projects/${encodeURIComponent(projectId)}/laid-out-pages`,
    method: 'POST',
    body: { page_template_id: pageTemplateId, laid_out_image_id: laidOutImageId },
  }),
  async onQueryStarted({ projectId, pageTemplateId, laidOutImageId }, { dispatch, queryFulfilled }) {
    // Optimistic update - update cache immediately
    const patchResult = dispatch(
      api.util.updateQueryData('getLaidOutPages', { projectId }, (draft) => {
        draft.push({
          id: `temp-${Date.now()}`,
          project_id: projectId,
          page_template_id: pageTemplateId,
          laid_out_image_id: laidOutImageId,
          result: null, // Will be replaced by server response
          rendering: true, // Flag for rendering state
        });
      })
    );
    try {
      await queryFulfilled;
      // Server response automatically replaces optimistic update
      // Start polling for rendered preview
      startPollingPreview(queryFulfilled.data.laid_out_page.id);
    } catch (error) {
      // Rollback on error
      patchResult.undo();
      dispatch(uiSlice.actions.addToast({
        id: Date.now().toString(),
        text: 'Failed to create page. Please try again.',
        type: 'error',
      }));
    }
  },
  invalidatesTags: (_result, _error, { projectId }) => [
    { type: 'LaidOutPage', id: `LIST-${projectId}` },
  ],
}),
```

### Preview Polling Pattern
```typescript
async function startPollingPreview(pageId: string) {
  const maxAttempts = 10;
  let attempts = 0;
  
  const poll = async () => {
    try {
      const response = await fetch(`/api/laid-out-pages/${pageId}/preview?variant=thumbnail`);
      if (response.ok) {
        // Preview ready, update cache
        dispatch(
          api.util.updateQueryData('getLaidOutPages', { projectId }, (draft) => {
            const page = draft.find((p) => p.id === pageId);
            if (page) {
              page.preview_url = response.url;
              page.rendering = false;
            }
          })
        );
      } else if (response.status === 202) {
        // Still rendering, poll again
        attempts++;
        if (attempts < maxAttempts) {
          setTimeout(poll, 1000); // Poll every second
        }
      }
    } catch (error) {
      // Keep placeholder, show warning
      console.warn('Failed to fetch page preview, keeping placeholder');
    }
  };
  
  // Start polling after 1 second delay
  setTimeout(poll, 1000);
}
```

### Key Workflow Steps
1. **User creates template:** Visual wizard or form (optional, can use presets)
2. **User selects template:** Show templates visually, preview template
3. **User selects laid-out image:** Show available images, preview page layout
4. **User creates page:** Optimistic update appears immediately
5. **API call:** `POST /laid-out-pages` returns page ID immediately
6. **Page rendering:** Happens asynchronously in background
7. **Preview polling:** Frontend polls preview endpoint or uses websocket
8. **Preview ready:** Update cache with preview URL, show rendered page

---

**End of Page Layout UX Walkthrough**

