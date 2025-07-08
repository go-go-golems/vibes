package main

import (
	"strings"
	"time"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/sahilm/fuzzy"
)

// Command represents a command that can be executed
type Command struct {
	Name        string
	Description string
	Action      func() tea.Cmd
}

// CommandPalette represents the command palette state
type CommandPalette struct {
	commands     []Command
	filteredCmds []Command
	query        string
	selected     int
	visible      bool
	width        int
	height       int
}

// Styles for command palette
var (
	paletteStyle = lipgloss.NewStyle().
		Border(lipgloss.RoundedBorder()).
		BorderForeground(lipgloss.Color("62")).
		Background(lipgloss.Color("235")).
		Padding(1).
		Margin(2, 4)

	queryStyle = lipgloss.NewStyle().
		Foreground(lipgloss.Color("86")).
		Background(lipgloss.Color("240")).
		Padding(0, 1).
		Margin(0, 0, 1, 0)

	commandStyle = lipgloss.NewStyle().
		Padding(0, 1)

	selectedCommandStyle = lipgloss.NewStyle().
		Background(lipgloss.Color("62")).
		Foreground(lipgloss.Color("230")).
		Padding(0, 1)

	commandNameStyle = lipgloss.NewStyle().
		Foreground(lipgloss.Color("86")).
		Bold(true)

	commandDescStyle = lipgloss.NewStyle().
		Foreground(lipgloss.Color("243"))

	paletteHeaderStyle = lipgloss.NewStyle().
		Foreground(lipgloss.Color("212")).
		Bold(true).
		Margin(0, 0, 1, 0)
)

// NewCommandPalette creates a new command palette
func NewCommandPalette() *CommandPalette {
	cp := &CommandPalette{
		commands:     []Command{},
		filteredCmds: []Command{},
		query:        "",
		selected:     0,
		visible:      false,
	}
	
	// Register default commands
	cp.RegisterCommand("help", "Show help information", func() tea.Cmd {
		return func() tea.Msg {
			return CommandExecutedMsg{Command: "help"}
		}
	})
	
	cp.RegisterCommand("clear", "Clear chat messages", func() tea.Cmd {
		return func() tea.Msg {
			return CommandExecutedMsg{Command: "clear"}
		}
	})
	
	cp.RegisterCommand("quit", "Exit the application", func() tea.Cmd {
		return tea.Quit
	})
	
	cp.RegisterCommand("echo", "Echo a test message", func() tea.Cmd {
		return func() tea.Msg {
			return CommandExecutedMsg{Command: "echo", Data: "Hello from command palette!"}
		}
	})
	
	cp.RegisterCommand("time", "Show current time", func() tea.Cmd {
		return func() tea.Msg {
			return CommandExecutedMsg{Command: "time", Data: time.Now().Format("15:04:05")}
		}
	})
	
	cp.RegisterCommand("date", "Show current date", func() tea.Cmd {
		return func() tea.Msg {
			return CommandExecutedMsg{Command: "date", Data: time.Now().Format("2006-01-02")}
		}
	})
	
	cp.RegisterCommand("about", "Show application information", func() tea.Cmd {
		return func() tea.Msg {
			return CommandExecutedMsg{Command: "about"}
		}
	})
	
	cp.RegisterCommand("theme", "Change application theme", func() tea.Cmd {
		return func() tea.Msg {
			return CommandExecutedMsg{Command: "theme"}
		}
	})
	
	cp.updateFiltered()
	return cp
}

// RegisterCommand adds a new command to the palette
func (cp *CommandPalette) RegisterCommand(name, description string, action func() tea.Cmd) {
	cp.commands = append(cp.commands, Command{
		Name:        name,
		Description: description,
		Action:      action,
	})
	cp.updateFiltered()
}

// Show makes the command palette visible
func (cp *CommandPalette) Show() {
	cp.visible = true
	cp.query = ""
	cp.selected = 0
	cp.updateFiltered()
}

// Hide makes the command palette invisible
func (cp *CommandPalette) Hide() {
	cp.visible = false
	cp.query = ""
	cp.selected = 0
}

// IsVisible returns whether the command palette is visible
func (cp *CommandPalette) IsVisible() bool {
	return cp.visible
}

// SetSize sets the dimensions for the command palette
func (cp *CommandPalette) SetSize(width, height int) {
	cp.width = width
	cp.height = height
}

// updateFiltered updates the filtered commands based on the current query
func (cp *CommandPalette) updateFiltered() {
	if cp.query == "" {
		cp.filteredCmds = cp.commands
	} else {
		// Use fuzzy matching
		var targets []string
		for _, cmd := range cp.commands {
			targets = append(targets, cmd.Name)
		}
		
		matches := fuzzy.Find(cp.query, targets)
		cp.filteredCmds = []Command{}
		
		for _, match := range matches {
			cp.filteredCmds = append(cp.filteredCmds, cp.commands[match.Index])
		}
	}
	
	// Reset selection if out of bounds
	if cp.selected >= len(cp.filteredCmds) {
		cp.selected = 0
	}
}

// Update handles command palette updates
func (cp *CommandPalette) Update(msg tea.Msg) tea.Cmd {
	if !cp.visible {
		return nil
	}
	
	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch msg.String() {
		case "escape", "ctrl+p":
			cp.Hide()
			return nil
			
		case "enter":
			if len(cp.filteredCmds) > 0 && cp.selected < len(cp.filteredCmds) {
				cmd := cp.filteredCmds[cp.selected].Action()
				cp.Hide()
				return cmd
			}
			return nil
			
		case "up", "ctrl+k":
			if cp.selected > 0 {
				cp.selected--
			}
			return nil
			
		case "down", "ctrl+j":
			if cp.selected < len(cp.filteredCmds)-1 {
				cp.selected++
			}
			return nil
			
		case "backspace":
			if len(cp.query) > 0 {
				cp.query = cp.query[:len(cp.query)-1]
				cp.updateFiltered()
			}
			return nil
			
		default:
			if len(msg.String()) == 1 {
				cp.query += msg.String()
				cp.updateFiltered()
			}
			return nil
		}
	}
	
	return nil
}

// View renders the command palette
func (cp *CommandPalette) View() string {
	if !cp.visible {
		return ""
	}
	
	// Header
	header := paletteHeaderStyle.Render("Command Palette")
	
	// Query input
	queryPrompt := "> " + cp.query
	if cp.query == "" {
		queryPrompt += "Type to search commands..."
	}
	query := queryStyle.Width(cp.width - 12).Render(queryPrompt)
	
	// Commands list
	var commandLines []string
	maxCommands := 8 // Limit visible commands
	
	for i, cmd := range cp.filteredCmds {
		if i >= maxCommands {
			break
		}
		
		name := commandNameStyle.Render(cmd.Name)
		desc := commandDescStyle.Render(" - " + cmd.Description)
		line := name + desc
		
		if i == cp.selected {
			line = selectedCommandStyle.Width(cp.width - 12).Render(line)
		} else {
			line = commandStyle.Width(cp.width - 12).Render(line)
		}
		
		commandLines = append(commandLines, line)
	}
	
	if len(commandLines) == 0 {
		commandLines = append(commandLines, commandStyle.Render("No commands found"))
	}
	
	// Footer with navigation help
	footer := helpStyle.Render("↑↓ navigate • Enter select • Esc close")
	
	content := lipgloss.JoinVertical(lipgloss.Left, 
		header,
		query, 
		strings.Join(commandLines, "\n"),
		"",
		footer,
	)
	
	return paletteStyle.Width(cp.width - 8).Render(content)
}

// CommandExecutedMsg is sent when a command is executed
type CommandExecutedMsg struct {
	Command string
	Data    interface{}
}

