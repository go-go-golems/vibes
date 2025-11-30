---
Title: 'CLI Tools Design: pagelayout and imagelayout'
Ticket: CLI-TOOLS-DESIGN
Status: active
Topics:
    - cli
    - design
    - pagelayout
    - imagelayout
DocType: design-doc
Intent: long-term
Owners: []
RelatedFiles: []
ExternalSources: []
Summary: 'Design standalone CLI tools for pagelayout (similar to zinelayout render) and enhance imagelayout CLI tools'
LastUpdated: 2025-11-30T12:52:22.946490075-05:00
---

# CLI Tools Design: pagelayout and imagelayout

## Executive Summary

This document designs standalone CLI tools for exercising the `pagelayout` and `imagelayout` packages directly, similar to how `zine-layout render` exercises the `zinelayout` package. Currently:

- **imagelayout**: Has `zine-layout imagelayout compute` command ✅
- **pagelayout**: Only accessible via `zine-layout workflow laid-out-pages render` which requires database setup ❌

The goal is to provide direct CLI access to pagelayout rendering without database dependencies, enabling easier testing, validation, and standalone usage.

## Problem Statement

### Current State

1. **pagelayout package**: 
   - Core algorithm: `pkg/pagelayout/renderer/renderer.go` → `RenderPage()`
   - Only accessible via: `zine-layout workflow laid-out-pages render --page-id <id>`
   - Requires: Database setup, existing project/page/template/asset records
   - Problem: Cannot test pagelayout algorithm directly without full database setup

2. **imagelayout package**:
   - Core algorithm: `pkg/imagelayout/engine/engine.go` → `ComputeViewport()`
   - Has CLI: `zine-layout imagelayout compute` ✅
   - Status: Already has standalone CLI tool

3. **zinelayout package** (reference):
   - Core algorithm: `pkg/zinelayout` → `CreateOutputImage()`
   - Has CLI: `zine-layout render --spec layout.yaml [images...]` ✅
   - Can work standalone with YAML spec and image files

### Problem

During analysis of the pagelayout algorithm, we needed to test the rendering behavior but had to create a custom test program because there's no standalone CLI tool. The `workflow laid-out-pages render` command requires:
- SQLite database initialization
- Project creation
- Asset upload
- Page template creation
- Laid-out image computation
- Page creation

This is too complex for simple algorithm validation and testing.

### Use Cases

1. **Algorithm Testing**: Test pagelayout rendering with different settings without database
2. **Validation**: Validate rendering behavior matches expectations
3. **Documentation Examples**: Generate example outputs for documentation
4. **Debugging**: Debug rendering issues with specific configurations
5. **Integration Testing**: Test pagelayout independently before integration

## Proposed Solution

### 1. Add `pagelayout` Command Group

Create a new top-level command group `zine-layout pagelayout` with a `render` subcommand:

```bash
zine-layout pagelayout render \
  --source-image image.png \
  --settings settings.yaml \
  --output-dir ./output \
  [--variant thumbnail|full|combined|left|right]
```

**Features:**
- Accept source image file path
- Accept page settings via YAML/JSON file or command-line flags
- Optionally accept `imagelayout.ViewportResult` for crop geometry
- Generate all variants (full, thumbnail, combined, left, right)
- Save variants to output directory
- Support test mode (generate test images)

### 2. Enhance `imagelayout` Command Group

The `imagelayout compute` command already exists and works well. Consider adding:

- **`imagelayout render`**: Render the computed result to an actual image (optional enhancement)
- Better integration with pagelayout (pass results between commands)

### Command Structure

```
zine-layout
├── render              # zinelayout (imposition) - EXISTS
├── imagelayout         # imagelayout - EXISTS
│   └── compute         # Compute viewport - EXISTS
│   └── render          # Render viewport result (NEW - optional)
└── pagelayout          # pagelayout - NEW
    └── render          # Render page - NEW
```

## Design Decisions

### Decision 1: Standalone Command vs. Workflow Subcommand

**Decision**: Create standalone `pagelayout` command group, not a subcommand under `workflow`.

**Rationale**:
- `workflow` commands operate on database repositories
- `pagelayout render` should work without database (like `render` and `imagelayout compute`)
- Consistent with existing pattern: standalone algorithms get top-level commands

### Decision 2: YAML/JSON Spec Format

**Decision**: Support both YAML and JSON spec files, plus command-line flags.

**Rationale**:
- Matches `imagelayout compute` pattern (accepts `--spec` file)
- Matches `render` pattern (accepts YAML layout spec)
- Command-line flags for quick testing
- YAML/JSON for complex configurations

### Decision 3: Integration with imagelayout

**Decision**: Support optional `--layout-result` parameter to pass `imagelayout.ViewportResult`.

**Rationale**:
- Enables workflow: `imagelayout compute` → `pagelayout render`
- Matches how service layer uses pagelayout (with LayoutResult)
- Optional: can render without crop geometry

### Decision 4: Variant Output

**Decision**: Generate all variants by default, allow filtering with `--variant`.

**Rationale**:
- Matches current `RenderPage()` behavior (generates all variants)
- `--variant` flag allows requesting specific variant only
- Useful for testing specific variants

## Detailed Design

### Command: `zine-layout pagelayout render`

#### Flags

**Required:**
- `--source-image` (string): Path to source image file
- `--settings` (string): Path to YAML/JSON page settings file (or use flags)

**Optional:**
- `--output-dir` (string, default: `./output`): Output directory for variants
- `--variant` (string): Specific variant to generate (thumbnail|full|combined|left|right). If not specified, generates all.
- `--layout-result` (string): Path to JSON file containing `imagelayout.ViewportResult` (for crop geometry)
- `--background-color` (string): Background color (default: white)
- `--thumbnail-max-px` (int, default: 512): Maximum side length for thumbnail

**Page Settings Flags** (alternative to `--settings` file):
- `--page-width-in` (float): Page width in inches
- `--page-height-in` (float): Page height in inches
- `--dpi` (float): Dots per inch
- `--margin-top-in`, `--margin-right-in`, `--margin-bottom-in`, `--margin-left-in` (float): Margins
- `--is-spread` (bool): Enable spread mode
- `--gutter-width-in` (float): Gutter width for spreads
- `--positioning-mode` (string): fill|absolute|snap
- `--border-enabled` (bool): Enable border
- `--border-color` (string): Border color
- `--border-type` (string): Border type (plain|dotted|dashed|corner)
- `--image-x-in`, `--image-y-in`, `--image-width-in`, `--image-height-in` (float): Absolute positioning

**Test Mode:**
- `--test` (bool): Generate test image instead of reading from file
- `--test-width` (int): Test image width
- `--test-height` (int): Test image height

#### Input Format

**Settings File (YAML):**
```yaml
pageWidthIn: 8.5
pageHeightIn: 11.0
dpi: 300
marginTopIn: 0.25
marginRightIn: 0.25
marginBottomIn: 0.25
marginLeftIn: 0.25
isSpread: false
positioningMode: fill
borderEnabled: true
borderColor: "#000000"
borderType: plain
```

**Settings File (JSON):**
```json
{
  "pageWidthIn": 8.5,
  "pageHeightIn": 11.0,
  "dpi": 300,
  "marginTopIn": 0.25,
  "marginRightIn": 0.25,
  "marginBottomIn": 0.25,
  "marginLeftIn": 0.25,
  "isSpread": false,
  "positioningMode": "fill",
  "borderEnabled": true,
  "borderColor": "#000000",
  "borderType": "plain"
}
```

**Layout Result File (JSON):**
```json
{
  "source_rect": {
    "x": 500,
    "y": 0,
    "w": 3000,
    "h": 3000
  },
  "target_rect": {
    "x": 75,
    "y": 75,
    "w": 2400,
    "h": 1800
  },
  "canvas_rect": {
    "x": 75,
    "y": 75,
    "w": 2400,
    "h": 3150
  },
  "scale": 0.6,
  "mode": "contain"
}
```

#### Output

- Saves PNG files to output directory:
  - `full.png` - Full rendered page
  - `combined.png` - Same as full (alias)
  - `thumbnail.png` - Scaled-down version
  - `left.png` - Left half of spread (if `isSpread: true`)
  - `right.png` - Right half of spread (if `isSpread: true`)
- Prints summary to stdout:
  ```
  Rendered page variants:
    full: output/full.png (2550x3300)
    thumbnail: output/thumbnail.png (396x512)
    combined: output/combined.png (2550x3300)
  ```

#### Example Usage

```bash
# Basic rendering with YAML settings
zine-layout pagelayout render \
  --source-image photo.jpg \
  --settings page.yaml \
  --output-dir ./rendered

# With imagelayout crop result
zine-layout imagelayout compute \
  --source-width 4000 --source-height 3000 \
  --mode page --crop-ratio 1.0 \
  > layout.json

zine-layout pagelayout render \
  --source-image photo.jpg \
  --settings page.yaml \
  --layout-result layout.json \
  --output-dir ./rendered

# Spread rendering
zine-layout pagelayout render \
  --source-image photo.jpg \
  --page-width-in 17.0 \
  --page-height-in 11.0 \
  --dpi 300 \
  --is-spread \
  --gutter-width-in 0.5 \
  --positioning-mode fill \
  --output-dir ./spread

# Test mode
zine-layout pagelayout render \
  --test \
  --test-width 4000 --test-height 3000 \
  --settings page.yaml \
  --output-dir ./test-output
```

### Command: `zine-layout imagelayout render` (Optional Enhancement)

**Purpose**: Render the computed viewport result to an actual image file.

**Flags:**
- `--source-image` (string): Source image file
- `--result` (string): Path to JSON file with `ViewportResult` (or pipe from `compute`)
- `--output` (string): Output image path
- `--background-color` (string): Background color

**Use Case**: Visualize the computed crop/placement without full page rendering.

**Status**: Lower priority - can be added later if needed.

## Implementation Plan

### Phase 1: Core pagelayout render Command

1. **Create command structure**
   - Add `cmd/zine-layout/cmds/pagelayout/command.go`
   - Add `cmd/zine-layout/cmds/pagelayout/render.go`
   - Wire into main command in `cmd/zine-layout/main.go`

2. **Implement settings parsing**
   - Support YAML/JSON file parsing
   - Support command-line flags
   - Merge flags with file settings (flags override file)

3. **Implement image loading**
   - Load source image from file
   - Support test mode (generate test images)
   - Handle image decoding errors

4. **Implement layout result loading** (optional)
   - Parse JSON `ViewportResult` if provided
   - Pass to `RenderContext.LayoutResult`

5. **Implement rendering**
   - Call `renderer.RenderPage()`
   - Handle errors appropriately

6. **Implement output**
   - Save variants to output directory
   - Print summary to stdout
   - Handle file write errors

### Phase 2: Testing and Validation

1. **Create test cases**
   - Basic fill mode rendering
   - Spread rendering
   - Absolute positioning
   - With layout result crop
   - Border rendering

2. **Validate output**
   - Check file generation
   - Verify image dimensions
   - Compare with expected results

3. **Update documentation**
   - Add usage examples
   - Document spec file format
   - Add to CLI help

### Phase 3: Integration Examples

1. **Create example workflows**
   - `imagelayout compute` → `pagelayout render` pipeline
   - Example YAML settings files
   - Example scripts

2. **Update analysis documentation**
   - Add CLI examples to pagelayout analysis
   - Show how to use CLI for validation

## Alternatives Considered

### Alternative 1: Extend `workflow laid-out-pages render`

**Approach**: Add flags to `workflow laid-out-pages render` to work without database.

**Rejected Because**:
- Breaks the pattern: `workflow` commands operate on repositories
- Would require complex conditional logic
- Less discoverable (buried in workflow subcommands)

### Alternative 2: Add to `imagelayout` Command Group

**Approach**: Add `imagelayout render` that does pagelayout rendering.

**Rejected Because**:
- Confusing: imagelayout computes geometry, pagelayout renders pages
- Different packages, different purposes
- Better to have separate command groups

### Alternative 3: Single `render` Command with Mode Flag

**Approach**: Extend `zine-layout render` with `--mode pagelayout|zinelayout`.

**Rejected Because**:
- `render` is specifically for zinelayout (imposition)
- Would make command more complex
- Less clear separation of concerns

## Open Questions

1. **Should we support piping between commands?**
   - `imagelayout compute | pagelayout render`?
   - Would require JSON parsing from stdin

2. **Should we add `imagelayout render` command?**
   - Lower priority
   - Could be useful for visualizing viewport results

3. **Output format options?**
   - Currently PNG only
   - Should we support other formats (JPEG, PDF)?

4. **Verbose/debug mode?**
   - Should we add `--verbose` flag to show algorithm steps?
   - Useful for debugging and documentation

## Related

- **pagelayout analysis**: `PAGE-LAYOUT-ANALYSIS` ticket - Documents the algorithm
- **imagelayout analysis**: `ZINE-LAYOUT-ANALYSIS` ticket - Documents the algorithm
- **Existing CLI**: `zine-layout render` - Reference implementation for zinelayout
- **Existing CLI**: `zine-layout imagelayout compute` - Reference implementation for imagelayout

## Success Criteria

1. ✅ Can render a page without database setup
2. ✅ Can test pagelayout algorithm with various settings
3. ✅ Can integrate with imagelayout compute results
4. ✅ Generates all variants correctly
5. ✅ Supports both YAML and JSON settings
6. ✅ Command-line flags work for quick testing
7. ✅ Documentation includes usage examples
