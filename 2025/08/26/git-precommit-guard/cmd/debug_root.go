package cmd

import (
    "github.com/go-go-golems/glazed/pkg/cmds"
)

// DebugRootCommand is a namespace command for all debug subcommands.
type DebugRootCommand struct { *cmds.CommandDescription }

func NewDebugRootCommand() (*DebugRootCommand, error) {
    cd := cmds.NewCommandDescription(
        "debug",
        cmds.WithShort("Debug and diagnostics commands"),
    )
    return &DebugRootCommand{CommandDescription: cd}, nil
}


