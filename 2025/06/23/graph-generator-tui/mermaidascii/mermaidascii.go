package mermaidascii

import (
	"fmt"
	"strings"

	"github.com/AlexanderGrooff/mermaid-ascii/cmd"
)

// Options for rendering mermaid diagrams
type Options struct {
	PaddingX      int
	PaddingY      int
	BorderPadding int
	ASCIIOnly     bool
	Coords        bool
}

// DefaultOptions returns sensible default options
func DefaultOptions() Options {
	return Options{
		PaddingX:      5,
		PaddingY:      5,
		BorderPadding: 1,
		ASCIIOnly:     false,
		Coords:        false,
	}
}

// Render takes a mermaid diagram string and returns ASCII art
func Render(src string, opt Options) (string, error) {
	// Save current global state
	oldCoords := cmd.Coords
	oldUseAscii := cmd.UseAscii
	oldPaddingX := cmd.PaddingBetweenX
	oldPaddingY := cmd.PaddingBetweenY
	oldBorderPadding := cmd.BoxBorderPadding

	// Set options
	cmd.Coords = opt.Coords
	cmd.UseAscii = opt.ASCIIOnly
	cmd.PaddingBetweenX = opt.PaddingX
	cmd.PaddingBetweenY = opt.PaddingY
	cmd.BoxBorderPadding = opt.BorderPadding

	// Restore global state when done
	defer func() {
		cmd.Coords = oldCoords
		cmd.UseAscii = oldUseAscii
		cmd.PaddingBetweenX = oldPaddingX
		cmd.PaddingBetweenY = oldPaddingY
		cmd.BoxBorderPadding = oldBorderPadding
	}()

	// Parse the mermaid source
	properties, err := cmd.MermaidFileToMap(src, "lib")
	if err != nil {
		return "", fmt.Errorf("failed to parse mermaid: %w", err)
	}

	// Render to string
	result := cmd.DrawMap(properties)
	return result, nil
}

// RenderToWriter renders a mermaid diagram and writes to the provided writer
func RenderToWriter(src string, opt Options, w *strings.Builder) error {
	result, err := Render(src, opt)
	if err != nil {
		return err
	}
	w.WriteString(result)
	return nil
}

