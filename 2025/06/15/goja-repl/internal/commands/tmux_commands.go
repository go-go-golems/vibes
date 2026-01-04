package commands

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/user/goja-repl/internal/engine"
	"github.com/user/goja-repl/internal/tmux"
)

// TmuxCommand handles tmux session management
type TmuxCommand struct {
	tmuxManager *tmux.TmuxManager
}

func NewTmuxCommand() *TmuxCommand {
	return &TmuxCommand{
		tmuxManager: tmux.New("goja-repl"),
	}
}

func (c *TmuxCommand) Execute(args string, jsEngine *engine.JSEngine) (string, error) {
	if tmux.IsInsideTmux() {
		return "Already running inside a tmux session", nil
	}

	parts := strings.Fields(args)
	if len(parts) == 0 {
		return "Usage: /tmux [start|kill]", nil
	}

	switch parts[0] {
	case "start":
		err := c.tmuxManager.Initialize()
		if err != nil {
			return "", fmt.Errorf("failed to initialize tmux: %w", err)
		}
		err = c.tmuxManager.AttachToSession()
		if err != nil {
			return "", fmt.Errorf("failed to attach to tmux session: %w", err)
		}
		return "Tmux session started", nil
	case "kill":
		err := c.tmuxManager.KillSession()
		if err != nil {
			return "", fmt.Errorf("failed to kill tmux session: %w", err)
		}
		return "Tmux session killed", nil
	default:
		return fmt.Sprintf("Unknown tmux command: %s", parts[0]), nil
	}
}

func (c *TmuxCommand) Help() string {
	return "Manage tmux sessions"
}

// EditorCommand handles spawning an editor in tmux
type EditorCommand struct {
	tmuxManager *tmux.TmuxManager
}

func NewEditorCommand(tm *tmux.TmuxManager) *EditorCommand {
	return &EditorCommand{
		tmuxManager: tm,
	}
}

func (c *EditorCommand) Execute(args string, jsEngine *engine.JSEngine) (string, error) {
	if !tmux.IsInsideTmux() {
		return "Must be running inside a tmux session. Use /tmux start first.", nil
	}

	parts := strings.Fields(args)
	if len(parts) == 0 {
		return "Usage: /edit [variable|file] [name]", nil
	}

	switch parts[0] {
	case "variable":
		if len(parts) < 2 {
			return "Usage: /edit variable [name]", nil
		}
		
		varName := parts[1]
		value, err := jsEngine.GetVariable(varName)
		if err != nil {
			return "", fmt.Errorf("failed to get variable: %w", err)
		}
		
		// Create a temporary file
		tmpDir := os.TempDir()
		filename := filepath.Join(tmpDir, fmt.Sprintf("%s.js", varName))
		
		// Spawn editor with the variable content
		err = c.tmuxManager.SpawnEditor(filename, value)
		if err != nil {
			return "", fmt.Errorf("failed to spawn editor: %w", err)
		}
		
		// The content will be read and updated when the user returns to the REPL
		return fmt.Sprintf("Editing variable '%s' in editor window", varName), nil
		
	case "file":
		if len(parts) < 2 {
			return "Usage: /edit file [path]", nil
		}
		
		filename := parts[1]
		content, err := os.ReadFile(filename)
		if err != nil {
			if os.IsNotExist(err) {
				// Create a new file
				content = []byte("")
			} else {
				return "", fmt.Errorf("failed to read file: %w", err)
			}
		}
		
		// Spawn editor with the file content
		err = c.tmuxManager.SpawnEditor(filename, string(content))
		if err != nil {
			return "", fmt.Errorf("failed to spawn editor: %w", err)
		}
		
		return fmt.Sprintf("Editing file '%s' in editor window", filename), nil
		
	default:
		return fmt.Sprintf("Unknown edit command: %s", parts[0]), nil
	}
}

func (c *EditorCommand) Help() string {
	return "Spawn an editor in tmux"
}

// LogCommand handles console log management
type LogCommand struct {
	tmuxManager *tmux.TmuxManager
}

func NewLogCommand(tm *tmux.TmuxManager) *LogCommand {
	return &LogCommand{
		tmuxManager: tm,
	}
}

func (c *LogCommand) Execute(args string, jsEngine *engine.JSEngine) (string, error) {
	if !tmux.IsInsideTmux() {
		return "Must be running inside a tmux session. Use /tmux start first.", nil
	}

	parts := strings.Fields(args)
	if len(parts) == 0 {
		return "Usage: /log [view|send] [message]", nil
	}

	switch parts[0] {
	case "view":
		err := c.tmuxManager.SwitchToLogWindow()
		if err != nil {
			return "", fmt.Errorf("failed to switch to log window: %w", err)
		}
		return "Switched to log window", nil
		
	case "send":
		if len(parts) < 2 {
			return "Usage: /log send [message]", nil
		}
		
		message := strings.Join(parts[1:], " ")
		err := c.tmuxManager.SendToLogWindow(message)
		if err != nil {
			return "", fmt.Errorf("failed to send message to log window: %w", err)
		}
		return "Message sent to log window", nil
		
	case "return":
		err := c.tmuxManager.SwitchToMainWindow()
		if err != nil {
			return "", fmt.Errorf("failed to switch to main window: %w", err)
		}
		return "Returned to main REPL window", nil
		
	default:
		return fmt.Sprintf("Unknown log command: %s", parts[0]), nil
	}
}

func (c *LogCommand) Help() string {
	return "Manage console logs in tmux"
}
