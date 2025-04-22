// Package interfacemissing demonstrates the error of missing RunIntoGlazeProcessor method
package interfacemissing

import (
	"context"
	"fmt"

	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
)

// InitCmd is a command that demonstrates the missing interface method error
type InitCmd struct {
	cfg *Config
}

// Config is a simple configuration struct
type Config struct {
	IndexPath string
}

// Run executes the command
// This command only implements Run but not RunIntoGlazeProcessor
// which is required by the cmds.GlazeCommand interface
func (c *InitCmd) Run(ctx context.Context, parsedLayers *layers.ParsedLayers) error {
	fmt.Println("Initializing...")
	return nil
}

// The missing method should be:
// func (c *InitCmd) RunIntoGlazeProcessor(ctx context.Context, parsedLayers *layers.ParsedLayers, gp middlewares.Processor) error {
//     return c.Run(ctx, parsedLayers)
// }

// This function demonstrates how the command would be used in a slice of GlazeCommand
func UseCommands() {
	initCmd := &InitCmd{}
	
	// This will cause a compile error because InitCmd doesn't implement GlazeCommand
	commands := []cmds.GlazeCommand{
		initCmd, // Error: InitCmd does not implement cmds.GlazeCommand (missing method RunIntoGlazeProcessor)
	}
	
	fmt.Println(commands) // Just to use the variable
}
