package ui

import (
	"fmt"
	"strings"

	"github.com/charmbracelet/bubbles/textinput"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/user/goja-repl/internal/commands"
	"github.com/user/goja-repl/internal/engine"
	"github.com/user/goja-repl/internal/tmux"
)

// Model represents the UI state for the REPL
type Model struct {
	styles        Styles
	jsEngine      *engine.JSEngine
	cmdRegistry   *commands.CommandRegistry
	tmuxManager   *tmux.TmuxManager
	textInput     textinput.Model
	history       []historyEntry
	multilineMode bool
	multilineText []string
	width         int
	quitting      bool
}

// historyEntry represents a single entry in the REPL history
type historyEntry struct {
	input  string
	output string
	isErr  bool
}

// NewModel creates a new UI model
func NewModel() Model {
	ti := textinput.New()
	ti.Placeholder = "Enter JavaScript or /command"
	ti.Focus()
	ti.Width = 80
	ti.Prompt = "js> "

	jsEngine := engine.New()
	tmuxManager := tmux.New("goja-repl")
	cmdRegistry := commands.NewCommandRegistry()

	// Register tmux-related commands
	tmuxCmd := commands.NewTmuxCommand()
	editorCmd := commands.NewEditorCommand(tmuxManager)
	logCmd := commands.NewLogCommand(tmuxManager)
	vimCmd := commands.NewVimCommand(tmuxManager)

	cmdRegistry.Register("tmux", tmuxCmd)
	cmdRegistry.Register("edit", editorCmd)
	cmdRegistry.Register("log", logCmd)
	cmdRegistry.Register("vim", vimCmd)

	// Set up console.log handler to redirect to tmux log window if available
	jsEngine.SetConsoleLogHandler(func(message string) {
		if tmux.IsInsideTmux() {
			_ = tmuxManager.SendToLogWindow(message)
		} else {
			fmt.Println("[LOG]:", message)
		}
	})

	return Model{
		styles:        DefaultStyles(),
		jsEngine:      jsEngine,
		cmdRegistry:   cmdRegistry,
		tmuxManager:   tmuxManager,
		textInput:     ti,
		history:       []historyEntry{},
		multilineMode: false,
		multilineText: []string{},
		width:         80, // Default width
		quitting:      false,
	}
}

// Init initializes the UI model
func (m Model) Init() tea.Cmd {
	return textinput.Blink
}

// Update handles UI events and updates the model
func (m Model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	var cmd tea.Cmd

	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		// Update the width for proper wrapping
		m.width = msg.Width
		m.textInput.Width = msg.Width - 10 // Account for prompt and padding
		
	case tea.KeyMsg:
		// Check for Ctrl+J as a substitute for Shift+Enter (which isn't directly supported)
		if msg.Type == tea.KeyCtrlJ {
			// Handle Ctrl+J for multiline input
			if !m.multilineMode {
				m.multilineMode = true
				m.multilineText = []string{m.textInput.Value()}
			} else {
				m.multilineText = append(m.multilineText, m.textInput.Value())
			}
			m.textInput.Reset()
			return m, nil
		}

		switch msg.Type {
		case tea.KeyCtrlC:
			m.quitting = true
			return m, tea.Quit

		case tea.KeyEnter:
			input := m.textInput.Value()
			
			// If in multiline mode, check if we should execute or continue
			if m.multilineMode {
				if input == "" {
					// Empty line in multiline mode means execute the code
					fullInput := strings.Join(m.multilineText, "\n")
					m = m.processInput(fullInput)
					m.multilineMode = false
					m.multilineText = []string{}
				} else {
					// Add another line to multiline input
					m.multilineText = append(m.multilineText, input)
					m.textInput.Reset()
					return m, nil
				}
			} else {
				// Normal single-line mode
				if input == "" {
					return m, nil
				}
				m = m.processInput(input)
			}
			
			m.textInput.Reset()

			if m.quitting {
				return m, tea.Quit
			}
		}
	}

	m.textInput, cmd = m.textInput.Update(msg)
	return m, cmd
}

// View renders the UI
func (m Model) View() string {
	var sb strings.Builder

	// Title
	sb.WriteString(m.styles.Title.Render(" Goja JavaScript REPL "))
	sb.WriteString("\n\n")

	// History with wrapping
	for _, entry := range m.history {
		// Input
		sb.WriteString(m.styles.Prompt.Render("js> "))
		sb.WriteString(m.wrapText(entry.input, m.width-5))
		sb.WriteString("\n")

		// Output
		if entry.isErr {
			sb.WriteString(m.wrapText(m.styles.Error.Render(entry.output), m.width))
		} else {
			sb.WriteString(m.wrapText(m.styles.Result.Render(entry.output), m.width))
		}
		sb.WriteString("\n\n")
	}

	// Multiline input display
	if m.multilineMode {
		sb.WriteString(m.styles.Info.Render("Multiline Mode (press Enter on empty line to execute):\n"))
		for _, line := range m.multilineText {
			sb.WriteString(m.styles.Prompt.Render("... "))
			sb.WriteString(m.wrapText(line, m.width-5))
			sb.WriteString("\n")
		}
	}

	// Input field
	sb.WriteString(m.textInput.View())
	sb.WriteString("\n\n")

	// Help text
	helpText := "Type JavaScript code or /help for commands"
	if m.multilineMode {
		helpText = "Multiline mode: Enter empty line to execute, Ctrl+J for more lines"
	} else {
		helpText += " (Ctrl+J for multiline)"
	}
	
	if tmux.IsInsideTmux() {
		helpText += " (tmux mode active)"
	}
	sb.WriteString(m.styles.HelpText.Render(helpText))
	sb.WriteString("\n")

	if m.quitting {
		sb.WriteString("\n")
		sb.WriteString(m.styles.Info.Render("Exiting..."))
		sb.WriteString("\n")
	}

	return sb.String()
}

// wrapText wraps text to fit within the given width
func (m Model) wrapText(text string, width int) string {
	if width <= 0 {
		return text
	}

	var sb strings.Builder
	lines := strings.Split(text, "\n")

	for i, line := range lines {
		if len(line) <= width {
			sb.WriteString(line)
		} else {
			// Wrap the line
			currentWidth := 0
			words := strings.Fields(line)
			for j, word := range words {
				wordLen := len(word)
				if currentWidth+wordLen > width {
					// Start a new line with proper indentation
					sb.WriteString("\n    ")
					currentWidth = 4 // Account for indentation
				} else if j > 0 {
					sb.WriteString(" ")
					currentWidth++
				}
				sb.WriteString(word)
				currentWidth += wordLen
			}
		}

		// Add newline between original lines, but not after the last one
		if i < len(lines)-1 {
			sb.WriteString("\n")
		}
	}

	return sb.String()
}

// processInput handles user input and updates the model
func (m Model) processInput(input string) Model {
	if engine.IsSlashCommand(input) {
		// Handle slash command
		cmd, args := engine.ParseSlashCommand(input)
		handler, exists := m.cmdRegistry.Get(cmd)

		if !exists {
			m.history = append(m.history, historyEntry{
				input:  input,
				output: fmt.Sprintf("Unknown command: /%s", cmd),
				isErr:  true,
			})
			return m
		}

		output, err := handler.Execute(args, m.jsEngine)
		if err != nil {
			m.history = append(m.history, historyEntry{
				input:  input,
				output: err.Error(),
				isErr:  true,
			})
			return m
		}

		if output == "QUIT" {
			m.quitting = true
			return m
		}

		if output == "CLEAR_SCREEN" {
			m.history = []historyEntry{}
			return m
		}

		m.history = append(m.history, historyEntry{
			input:  input,
			output: output,
			isErr:  false,
		})
		return m
	}

	// Handle JavaScript evaluation
	result, err := m.jsEngine.Eval(input)
	if err != nil {
		m.history = append(m.history, historyEntry{
			input:  input,
			output: err.Error(),
			isErr:  true,
		})
		return m
	}

	m.history = append(m.history, historyEntry{
		input:  input,
		output: result,
		isErr:  false,
	})
	return m
}
