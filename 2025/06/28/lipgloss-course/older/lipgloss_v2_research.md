# Lipgloss v2 Research Findings

## Key Features in v2.0.0-beta.2

### Compositing (Main Feature)
The big news in v2.beta.2 is the new compositing API for layers and views:

```go
box := lipgloss.NewStyle().
    Width(10).
    Height(5).
    Border(lipgloss.NormalBorder())

// Make some layers.
a := lipgloss.NewLayer(box.Render("Who wants marmalade?"))
b := lipgloss.NewLayer(box.Render("I do!"))

// Put layers in a canvas.
canvas := lipgloss.NewCanvas(
    a.X(5).Y(10).Z(1),
    b.X(3).Y(7)
)

// Render it all out.
lipgloss.Println(canvas.Render())
```

Key concepts:
- **Layer**: Individual UI elements that can be positioned
- **Canvas**: Container for multiple layers with positioning
- **X/Y positioning**: Absolute positioning on the canvas
- **Z-index**: Layer stacking order
- **Nested layers**: Layers can contain other layers via `Layer.AddLayers`

### Table Enhancements
- Several bug fixes for table rendering
- Enhanced table sizing and content wrapping algorithms
- New border styles: MarkdownBorder and ASCIIBorder
- Better column width calculation
- Content wrapping by default inside cells

### API Changes from Alpha
- Color definition changes - no longer accepts hexadecimal integers
- Must use string format: `lipgloss.Color("#0000cc")` or `lipgloss.Color("204")`

## Repository Information
- GitHub: https://github.com/charmbracelet/lipgloss
- Current stable: v1.1.0
- Beta version: v2.0.0-beta.2 (released 2 hours ago)
- Stars: 9.2k
- Language: Go

## Documentation Links
- Go package docs: https://pkg.go.dev/github.com/charmbracelet/lipgloss/v2
- Compositing example mentioned in release notes
- Layer and Canvas API documentation available

## Perfect for Window Manager Demo
The compositing features with layers, canvas, X/Y positioning, and Z-index make this ideal for creating a window manager demo with overlapping windows.



## VHS with Text Screenshot Support

### Main VHS Repository
- GitHub: https://github.com/charmbracelet/vhs
- Stars: 16.7k
- Latest version: v0.10.0
- Description: CLI home video recorder for terminal GIFs

### Wesen Fork with Text Screenshots
- Fork: https://github.com/wesen/vhs/tree/task/add-text-screenshot
- Branch: task/add-text-screenshot
- Key commits:
  - "Add support for ANSI screenshots" (17301ec)
  - "Add text screenshot"

### Text Screenshot Features
The wesen fork adds:
1. **ANSI Buffer Support**: New `AnsiBuffer()` function that preserves ANSI escape codes
2. **Text Screenshot Storage**: `ansiScreenshots` map for storing ANSI content
3. **File Extensions**: Support for `.ansi` extension alongside `.png`, `.txt`
4. **ANSI Screenshot Function**: `makeAnsiScreenshot()` for creating text-based screenshots
5. **Frame Capture Disable**: Option to disable frame capture for text-only output

### Implementation Details
- Uses JavaScript evaluation to extract terminal buffer with ANSI codes
- Preserves foreground/background colors via ANSI escape sequences
- Stores content as text files with ANSI formatting
- Allows validation of terminal UI without image comparison

### Benefits for Course
- Can create both visual GIFs and text screenshots
- Text screenshots allow programmatic validation of UI output
- Perfect for testing lipgloss v2 layouts and ensuring they work correctly
- Enables iteration and refinement based on actual terminal output

