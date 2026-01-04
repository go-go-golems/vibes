package commands

import (
	"fmt"
	"strings"

	"github.com/user/goja-repl/internal/engine"
	"github.com/user/goja-repl/internal/tmux"
)

// CommandHandler defines the interface for slash command handlers
type CommandHandler interface {
	Execute(args string, jsEngine *engine.JSEngine) (string, error)
	Help() string
}

// CommandRegistry manages all available slash commands
type CommandRegistry struct {
	commands map[string]CommandHandler
}

// NewCommandRegistry creates a new command registry
func NewCommandRegistry() *CommandRegistry {
	registry := &CommandRegistry{
		commands: make(map[string]CommandHandler),
	}
	
	// Create tmux manager
	tmuxManager := tmux.New("goja-repl")
	
	// Register built-in commands
	registry.Register("help", &HelpCommand{registry: registry})
	registry.Register("clear", &ClearCommand{})
	registry.Register("history", &HistoryCommand{})
	registry.Register("quit", &QuitCommand{})
	registry.Register("exit", &QuitCommand{})
	
	// Register tmux-related commands
	registry.Register("tmux", NewTmuxCommand())
	registry.Register("edit", NewEditorCommand(tmuxManager))
	registry.Register("log", NewLogCommand(tmuxManager))
	
	return registry
}

// Register adds a command to the registry
func (r *CommandRegistry) Register(name string, handler CommandHandler) {
	r.commands[name] = handler
}

// Get returns a command handler by name
func (r *CommandRegistry) Get(name string) (CommandHandler, bool) {
	handler, exists := r.commands[name]
	return handler, exists
}

// ListCommands returns a list of all registered command names
func (r *CommandRegistry) ListCommands() []string {
	var commands []string
	for name := range r.commands {
		commands = append(commands, name)
	}
	return commands
}

// HelpCommand shows help information for commands
type HelpCommand struct {
	registry *CommandRegistry
}

func (c *HelpCommand) Execute(args string, jsEngine *engine.JSEngine) (string, error) {
	if args == "" {
		// List all commands
		var help strings.Builder
		help.WriteString("Available commands:\n")
		
		for _, name := range c.registry.ListCommands() {
			cmd, _ := c.registry.Get(name)
			help.WriteString(fmt.Sprintf("  /%s - %s\n", name, cmd.Help()))
		}
		
		return help.String(), nil
	}
	
	// Show help for specific command
	cmd, exists := c.registry.Get(args)
	if !exists {
		return fmt.Sprintf("Unknown command: /%s", args), nil
	}
	
	return fmt.Sprintf("/%s - %s", args, cmd.Help()), nil
}

func (c *HelpCommand) Help() string {
	return "Display help information"
}

// ClearCommand clears the screen
type ClearCommand struct{}

func (c *ClearCommand) Execute(args string, jsEngine *engine.JSEngine) (string, error) {
	// The actual clearing will be handled by the UI
	return "CLEAR_SCREEN", nil
}

func (c *ClearCommand) Help() string {
	return "Clear the screen"
}

// HistoryCommand shows command history
type HistoryCommand struct{}

func (c *HistoryCommand) Execute(args string, jsEngine *engine.JSEngine) (string, error) {
	history := jsEngine.GetHistory()
	if len(history) == 0 {
		return "No history", nil
	}
	
	var result strings.Builder
	for i, cmd := range history {
		result.WriteString(fmt.Sprintf("%d: %s\n", i+1, cmd))
	}
	
	return result.String(), nil
}

func (c *HistoryCommand) Help() string {
	return "Show command history"
}

// QuitCommand exits the application
type QuitCommand struct{}

func (c *QuitCommand) Execute(args string, jsEngine *engine.JSEngine) (string, error) {
	// The actual quitting will be handled by the UI
	return "QUIT", nil
}

func (c *QuitCommand) Help() string {
	return "Exit the REPL"
}
