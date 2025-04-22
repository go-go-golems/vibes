// Package misimports demonstrates the error of using cmds.ParsedLayers and cmds.DefaultSlug
package misimports

import (
	"context"
	"fmt"

	"github.com/go-go-golems/glazed/pkg/cmds"
)

// AddCmd is a command that demonstrates the misimport error
type AddCmd struct {
	cfg *Config
}

// Config is a simple configuration struct
type Config struct {
	IndexPath string
}

// Run executes the command
func (c *AddCmd) Run(ctx context.Context, parsedLayers *cmds.ParsedLayers) error {
	// This should be layers.ParsedLayers, not cmds.ParsedLayers
	s := &struct {
		Path string
	}{}

	// This should be layers.DefaultSlug, not cmds.DefaultSlug
	if err := parsedLayers.InitializeStruct(cmds.DefaultSlug, s); err != nil {
		return fmt.Errorf("failed to initialize struct: %w", err)
	}

	return nil
}
