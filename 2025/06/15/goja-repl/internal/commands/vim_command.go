package commands

import (
	"fmt"

	"github.com/user/goja-repl/internal/engine"
	"github.com/user/goja-repl/internal/tmux"
)

// VimCommand handles spawning vim in tmux and capturing code
type VimCommand struct {
	tmuxManager *tmux.TmuxManager
}

// NewVimCommand creates a new vim command handler
func NewVimCommand(tm *tmux.TmuxManager) *VimCommand {
	return &VimCommand{
		tmuxManager: tm,
	}
}

// Execute runs the vim command
func (c *VimCommand) Execute(args string, jsEngine *engine.JSEngine) (string, error) {
	if !tmux.IsInsideTmux() {
		return "Must be running inside a tmux session. Use /tmux start first.", nil
	}

	// Determine initial content
	var initialContent string
	if args != "" {
		// Try to get variable content if args is a variable name
		content, err := jsEngine.GetVariable(args)
		if err == nil {
			initialContent = content
		} else {
			// If not a variable, treat as initial content
			initialContent = args
		}
	}

	// Spawn vim and capture the edited code
	code, err := c.tmuxManager.SpawnVimAndCaptureCode(initialContent)
	if err != nil {
		return "", fmt.Errorf("failed to capture code from vim: %w", err)
	}

	// Evaluate the code
	result, err := jsEngine.Eval(code)
	if err != nil {
		return "", fmt.Errorf("error evaluating code: %w", err)
	}

	return fmt.Sprintf("Code executed successfully. Result: %s", result), nil
}

// Help returns help information for the vim command
func (c *VimCommand) Help() string {
	return "Spawn vim in tmux, edit code, and insert it into the REPL on exit"
}
