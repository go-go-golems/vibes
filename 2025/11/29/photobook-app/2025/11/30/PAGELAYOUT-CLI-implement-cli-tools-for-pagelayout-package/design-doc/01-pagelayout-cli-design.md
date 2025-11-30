---
Title: 'CLI Tools Design: pagelayout Package'
Ticket: PAGELAYOUT-CLI
Status: active
Topics:
    - cli
    - design
    - pagelayout
    - implementation
DocType: design-doc
Intent: long-term
Owners: []
RelatedFiles: []
ExternalSources: []
Summary: 'Design and implement standalone CLI tools for pagelayout package: compute and render commands'
LastUpdated: 2025-11-30T12:52:22.946490075-05:00
---

# CLI Tools Design: pagelayout Package

## Executive Summary

This document designs standalone CLI tools for exercising the `pagelayout` package directly, similar to how `zine-layout render` exercises the `zinelayout` package and `zine-layout imagelayout compute` exercises the `imagelayout` package.

Currently, `pagelayout` is only accessible via `zine-layout workflow laid-out-pages render` which requires database setup. The goal is to provide direct CLI access to pagelayout functionality without database dependencies, enabling easier testing, validation, and standalone usage.

## Problem Statement

### Current State

**pagelayout package**: 
- Core algorithm: `pkg/pagelayout/renderer/renderer.go` → `RenderPage()`
- Settings helpers: `pkg/pagelayout/settings.go` → `PageLayoutSettings`, `ContentRectPx()`, etc.
- Only accessible via: `zine-layout workflow laid-out-pages render --page-id <id>`
- Requires: Database setup, existing project/page/template/asset records
- Problem: Cannot test pagelayout algorithm directly without full database setup

**Reference implementations:**
- `zine-layout render` - Standalone CLI for `zinelayout` package ✅
- `zine-layout imagelayout compute` - Standalone CLI for `imagelayout` package ✅

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
2. **Settings Validation**: Validate page settings and compute content areas
3. **Documentation Examples**: Generate example outputs for documentation
4. **Debugging**: Debug rendering issues with specific configurations
5. **Integration Testing**: Test pagelayout independently before integration

## Proposed Solution

### Command Structure

Create a new top-level command group `zine-layout pagelayout` with two subcommands:

1. **`compute`**: Compute page metrics and content areas (similar to `imagelayout compute`)
2. **`render`**: Render a page with an image (similar to `zine-layout render`)

```
zine-layout
├── render              # zinelayout (imposition) - EXISTS
├── imagelayout         # imagelayout - EXISTS
│   └── compute         # Compute viewport - EXISTS
└── pagelayout          # pagelayout - NEW
    ├── compute         # Compute page metrics - NEW
    └── render          # Render page - NEW
```

## Design Decisions

### Decision 1: Standalone Command vs. Workflow Subcommand

**Decision**: Create standalone `pagelayout` command group, not a subcommand under `workflow`.

**Rationale**:
- `workflow` commands operate on database repositories
- `pagelayout` commands should work without database (like `render` and `imagelayout compute`)
- Consistent with existing pattern: standalone algorithms get top-level commands

### Decision 2: Two Commands (compute + render)

**Decision**: Provide both `compute` and `render` commands.

**Rationale**:
- `compute`: Validates settings and computes metrics (useful for debugging)
- `render`: Actually renders the page (main use case)
- Matches pattern: `imagelayout` has `compute`, `render` has full rendering
- Allows testing settings without rendering

### Decision 3: YAML/JSON Spec Format

**Decision**: Support both YAML and JSON spec files, plus command-line flags.

**Rationale**:
- Matches `imagelayout compute` pattern (accepts `--spec` file)
- Matches `render` pattern (accepts YAML layout spec)
- Command-line flags for quick testing
- YAML/JSON for complex configurations

### Decision 4: Variant Output

**Decision**: Generate all variants by default, allow filtering with `--variant`.

**Rationale**:
- Matches current `RenderPage()` behavior (generates all variants)
- `--variant` flag allows requesting specific variant only
- Useful for testing specific variants

## Detailed Design

### Command 1: `zine-layout pagelayout compute`

**Purpose**: Compute page metrics, validate settings, and show content area calculations.

**Use Cases**:
- Validate page settings before rendering
- Debug content area calculations
- Understand how settings affect page dimensions

#### Flags

**Required:**
- `--settings` (string): Path to YAML/JSON page settings file (or use flags)

**Optional (Page Settings Flags - alternative to `--settings` file):**
- `--page-width-in` (float): Page width in inches
- `--page-height-in` (float): Page height in inches
- `--dpi` (float): Dots per inch
- `--margin-top-in`, `--margin-right-in`, `--margin-bottom-in`, `--margin-left-in` (float): Margins
- `--is-spread` (bool): Enable spread mode
- `--gutter-width-in` (float): Gutter width for spreads

#### Output

Prints JSON with computed metrics:
```json
{
  "settings": {
    "pageWidthIn": 8.5,
    "pageHeightIn": 11.0,
    "dpi": 300,
    ...
  },
  "metrics": {
    "pixelWidth": 2550,
    "pixelHeight": 3300,
    "contentRect": {
      "x": 75,
      "y": 75,
      "width": 2400,
      "height": 3150
    },
    "spreadSplitX": -1
  },
  "valid": true
}
```

#### Example Usage

```bash
# Compute from YAML file
zine-layout pagelayout compute --settings page.yaml

# Compute from flags
zine-layout pagelayout compute \
  --page-width-in 8.5 \
  --page-height-in 11.0 \
  --dpi 300 \
  --margin-top-in 0.25 \
  --margin-right-in 0.25 \
  --margin-bottom-in 0.25 \
  --margin-left-in 0.25

# Spread computation
zine-layout pagelayout compute \
  --page-width-in 17.0 \
  --page-height-in 11.0 \
  --dpi 300 \
  --is-spread \
  --gutter-width-in 0.5
```

### Command 2: `zine-layout pagelayout render`

**Purpose**: Render a source image onto a page using page settings.

#### Flags

**Required:**
- `--source-image` (string): Path to source image file
- `--settings` (string): Path to YAML/JSON page settings file (or use flags)

**Optional:**
- `--output-dir` (string, default: `./output`): Output directory for variants
- `--variant` (string): Specific variant to generate (thumbnail|full|combined|left|right). If not specified, generates all.
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

# Specific variant only
zine-layout pagelayout render \
  --source-image photo.jpg \
  --settings page.yaml \
  --variant thumbnail \
  --output-dir ./thumbnails
```

## Implementation Guide for New Developers

### Prerequisites

Before starting, familiarize yourself with:

1. **Existing CLI Commands** (reference implementations):
   - `zine-layout imagelayout compute` - See `cmd/zine-layout/cmds/imagelayout/compute.go`
   - `zine-layout render` - See `cmd/zine-layout/cmds/render/command.go`
   - Study how they structure commands, parse flags, and handle I/O

2. **pagelayout Package**:
   - Read `pkg/pagelayout/settings.go` - Understand `PageLayoutSettings` and helper methods
   - Read `pkg/pagelayout/renderer/renderer.go` - Understand `RenderPage()` function
   - Read `pkg/pagelayout/renderer/renderer_test.go` - See test examples

3. **Glazed Framework**:
   - Commands use Glazed framework for CLI structure
   - See existing commands for patterns
   - Key types: `cmds.CommandDescription`, `cmds.BareCommand`, `parameters.NewParameterDefinition`

### File Structure

Create the following files:

```
cmd/zine-layout/cmds/pagelayout/
├── command.go          # Command group root (like imagelayout/command.go)
├── compute.go          # Compute command implementation
└── render.go           # Render command implementation
```

### Step-by-Step Implementation

#### Step 1: Create Command Group Root

**File**: `cmd/zine-layout/cmds/pagelayout/command.go`

**Reference**: `cmd/zine-layout/cmds/imagelayout/command.go`

```go
package pagelayoutcmd

import "github.com/spf13/cobra"

// NewCommand builds the pagelayout verb-group root and wires subcommands.
func NewCommand() (*cobra.Command, error) {
	root := &cobra.Command{
		Use:   "pagelayout",
		Short: "Page layout helpers",
	}

	computeCmd, err := NewComputeCommand()
	if err != nil {
		return nil, err
	}
	root.AddCommand(computeCmd)

	renderCmd, err := NewRenderCommand()
	if err != nil {
		return nil, err
	}
	root.AddCommand(renderCmd)

	return root, nil
}
```

**Wire into main**: Add to `cmd/zine-layout/main.go`:
```go
pagelayoutCmd, err := pagelayoutcmd.NewCommand()
cobra.CheckErr(err)
rootCmd.AddCommand(pagelayoutCmd)
```

#### Step 2: Implement Compute Command

**File**: `cmd/zine-layout/cmds/pagelayout/compute.go`

**Reference**: `cmd/zine-layout/cmds/imagelayout/compute.go`

**Key Functions**:
- Parse settings from YAML/JSON file or flags
- Call `settings.Canonicalize()` to validate
- Call `settings.PixelWidth()`, `settings.PixelHeight()`, `settings.ContentRectPx()`, `settings.SpreadSplitX()`
- Output JSON with computed metrics

**Structure**:
```go
type computeCommand struct {
	*cmds.CommandDescription
}

type computeSettings struct {
	Spec           string  `glazed.parameter:"spec"`
	PageWidthIn    float64 `glazed.parameter:"page-width-in"`
	PageHeightIn   float64 `glazed.parameter:"page-height-in"`
	DPI            float64 `glazed.parameter:"dpi"`
	// ... other flags
}

func (c *computeCommand) Run(ctx context.Context, parsedLayers *layers.ParsedLayers) error {
	// 1. Parse settings from file or flags
	// 2. Validate with Canonicalize()
	// 3. Compute metrics
	// 4. Output JSON
}
```

#### Step 3: Implement Render Command

**File**: `cmd/zine-layout/cmds/pagelayout/render.go`

**Reference**: `cmd/zine-layout/cmds/render/command.go` for structure, `pkg/services/pages.go` for rendering logic

**Key Functions**:
- Parse settings (same as compute)
- Load source image (or generate test image)
- Create `renderer.RenderContext`
- Call `renderer.RenderPage()`
- Save variants to output directory

**Structure**:
```go
type renderCommand struct {
	*cmds.CommandDescription
}

type renderSettings struct {
	SourceImage    string  `glazed.parameter:"source-image"`
	Settings       string  `glazed.parameter:"settings"`
	OutputDir      string  `glazed.parameter:"output-dir"`
	Variant        string  `glazed.parameter:"variant"`
	// ... other flags
}

func (c *renderCommand) Run(ctx context.Context, parsedLayers *layers.ParsedLayers) error {
	// 1. Parse settings
	// 2. Load/generate source image
	// 3. Create RenderContext
	// 4. Call renderer.RenderPage()
	// 5. Save variants
	// 6. Print summary
}
```

#### Step 4: Settings Parsing Helper

Create a helper function to parse settings from file or flags:

```go
func parseSettings(parsedLayers *layers.ParsedLayers, specPath string, flags *settingsStruct) (pagelayout.PageLayoutSettings, error) {
	settings := pagelayout.PageLayoutSettings{}
	
	// Load from file if provided
	if specPath != "" {
		data, err := os.ReadFile(specPath)
		if err != nil {
			return settings, fmt.Errorf("read spec: %w", err)
		}
		if err := unmarshalByExt(specPath, data, &settings); err != nil {
			return settings, fmt.Errorf("parse spec: %w", err)
		}
	}
	
	// Override with flags if provided
	if parameterSet(parsedLayers, "page-width-in") {
		settings.PageWidthIn = flags.PageWidthIn
	}
	// ... apply other flags
	
	// Validate
	if err := settings.Canonicalize(); err != nil {
		return settings, fmt.Errorf("invalid settings: %w", err)
	}
	
	return settings, nil
}
```

#### Step 5: Image Loading Helper

```go
func loadSourceImage(sourcePath string, test bool, testWidth, testHeight int) (image.Image, error) {
	if test {
		// Generate test image (see pkg/zinelayout/image.go for example)
		return generateTestImage(testWidth, testHeight), nil
	}
	
	f, err := os.Open(sourcePath)
	if err != nil {
		return nil, fmt.Errorf("open image: %w", err)
	}
	defer f.Close()
	
	img, _, err := image.Decode(f)
	if err != nil {
		return nil, fmt.Errorf("decode image: %w", err)
	}
	
	return img, nil
}
```

#### Step 6: Variant Saving Helper

```go
func saveVariants(result *renderer.PageRenderResult, outputDir string, requestedVariant string) error {
	if err := os.MkdirAll(outputDir, 0o755); err != nil {
		return fmt.Errorf("create output dir: %w", err)
	}
	
	variantsToSave := result.Variants
	if requestedVariant != "" {
		if variant, ok := result.Variants[requestedVariant]; ok {
			variantsToSave = map[string]image.Image{requestedVariant: variant}
		}
	}
	
	for name, img := range variantsToSave {
		path := filepath.Join(outputDir, name+".png")
		if err := savePNG(img, path); err != nil {
			return fmt.Errorf("save %s: %w", name, err)
		}
	}
	
	return nil
}
```

### Testing Strategy

1. **Unit Tests**: Test settings parsing, validation, metric computation
2. **Integration Tests**: Test full render workflow with test images
3. **Manual Testing**: Use CLI commands with various settings

**Test Cases**:
- Basic fill mode rendering
- Spread rendering with gutter
- Absolute positioning
- Border rendering
- Thumbnail generation
- Settings validation (compute command)

### Common Pitfalls

1. **Settings Validation**: Always call `Canonicalize()` before using settings
2. **Image Decoding**: Handle different image formats (PNG, JPEG, etc.)
3. **File Paths**: Use `filepath.Join()` for cross-platform compatibility
4. **Error Handling**: Return descriptive errors with context
5. **Flag Parsing**: Check if flags are set before using (use `parameterSet()` helper)

### Key Files to Reference

**For Command Structure**:
- `cmd/zine-layout/cmds/imagelayout/compute.go` - Compute command pattern
- `cmd/zine-layout/cmds/render/command.go` - Render command pattern
- `cmd/zine-layout/cmds/imagelayout/command.go` - Command group pattern

**For pagelayout Usage**:
- `pkg/services/pages.go` - See how service layer uses pagelayout
- `pkg/pagelayout/renderer/renderer_test.go` - Test examples
- `pkg/pagelayout/settings.go` - Settings API

**For Utilities**:
- `pkg/app/render.go` - Image loading helpers (`ReadInputImages`, `GenerateTestImages`)
- `pkg/zinelayout/image.go` - Test image generation

## Implementation Plan

### Phase 1: Compute Command

1. Create command structure (`command.go`, `compute.go`)
2. Implement settings parsing (file + flags)
3. Implement metric computation
4. Implement JSON output
5. Add tests
6. Wire into main command

### Phase 2: Render Command

1. Create `render.go`
2. Implement settings parsing (reuse from compute)
3. Implement image loading (file + test mode)
4. Implement rendering (`RenderPage()` call)
5. Implement variant saving
6. Add tests
7. Wire into command group

### Phase 3: Testing and Documentation

1. Create comprehensive test cases
2. Test with various settings
3. Update CLI help documentation
4. Create example YAML/JSON files
5. Add usage examples to pagelayout analysis doc

## Alternatives Considered

### Alternative 1: Extend `workflow laid-out-pages render`

**Approach**: Add flags to `workflow laid-out-pages render` to work without database.

**Rejected Because**:
- Breaks the pattern: `workflow` commands operate on repositories
- Would require complex conditional logic
- Less discoverable (buried in workflow subcommands)

### Alternative 2: Single Command

**Approach**: Only implement `render` command, skip `compute`.

**Rejected Because**:
- `compute` is useful for debugging settings
- Matches pattern: `imagelayout` has `compute`
- Low cost to implement (reuses settings parsing)

## Related

- **pagelayout analysis**: `PAGE-LAYOUT-ANALYSIS` ticket - Complete algorithm documentation
- **Existing CLI**: `zine-layout render` - Reference implementation for zinelayout
- **Existing CLI**: `zine-layout imagelayout compute` - Reference implementation for imagelayout
- **Service layer**: `pkg/services/pages.go` - How pagelayout is used in production

## Success Criteria

1. ✅ Can compute page metrics without database setup
2. ✅ Can render a page without database setup
3. ✅ Can test pagelayout algorithm with various settings
4. ✅ Generates all variants correctly
5. ✅ Supports both YAML and JSON settings
6. ✅ Command-line flags work for quick testing
7. ✅ Documentation includes usage examples
8. ✅ Code follows existing patterns and conventions

