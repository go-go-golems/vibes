package main

import (
	"context"
	"fmt"
	"strings"

	"github.com/charmbracelet/bubbles/textarea"
	"github.com/charmbracelet/bubbles/viewport"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"js-uhoh-repl/pkg/evaluator"
)

// REPLModel represents the REPL interface
type REPLModel struct {
	evaluator    *evaluator.JSUhohEvaluator
	input        textarea.Model
	output       viewport.Model
	history      []string
	historyIndex int
	width        int
	height       int
	ready        bool
}

// NewREPLModel creates a new REPL model
func NewREPLModel(eval *evaluator.JSUhohEvaluator) REPLModel {
	// Create input textarea
	input := textarea.New()
	input.Placeholder = "Enter JavaScript code or /load <file> to load a file..."
	input.Focus()
	input.Prompt = "js-uhoh> "
	input.CharLimit = 0
	input.SetWidth(80)
	input.SetHeight(3)

	// Create output viewport
	output := viewport.New(80, 20)
	output.SetContent("JavaScript + Uhoh REPL\nType JavaScript code to execute, or use /load <file> to load JS files.\nUse createUI(formDef) to create uhoh UIs.\n\n")

	return REPLModel{
		evaluator:    eval,
		input:        input,
		output:       output,
		history:      []string{},
		historyIndex: -1,
	}
}

func (m REPLModel) Init() tea.Cmd {
	return textarea.Blink
}

func (m REPLModel) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	var cmds []tea.Cmd

	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		m.width = msg.Width
		m.height = msg.Height

		if !m.ready {
			// Set up dimensions
			inputHeight := 4
			outputHeight := m.height - inputHeight - 2

			m.input.SetWidth(m.width - 2)
			m.input.SetHeight(inputHeight)

			m.output.Width = m.width - 2
			m.output.Height = outputHeight

			m.ready = true
		}

	case tea.KeyMsg:
		switch msg.String() {
		case "ctrl+c":
			return m, tea.Quit

		case "enter":
			// Get the input
			code := strings.TrimSpace(m.input.Value())
			if code == "" {
				break
			}

			// Add to history
			m.history = append(m.history, code)
			m.historyIndex = len(m.history)

			// Clear input
			m.input.SetValue("")

			// Evaluate the code
			result, err := m.evaluator.Evaluate(context.Background(), code)
			
			// Add to output
			outputLine := fmt.Sprintf("js-uhoh> %s\n", code)
			if err != nil {
				outputLine += fmt.Sprintf("Error: %v\n\n", err)
			} else {
				outputLine += fmt.Sprintf("%s\n\n", result)
			}

			currentContent := m.output.View()
			m.output.SetContent(currentContent + outputLine)
			m.output.GotoBottom()

		case "up":
			// Navigate history up
			if len(m.history) > 0 && m.historyIndex > 0 {
				m.historyIndex--
				m.input.SetValue(m.history[m.historyIndex])
				m.input.CursorEnd()
			}

		case "down":
			// Navigate history down
			if len(m.history) > 0 && m.historyIndex < len(m.history)-1 {
				m.historyIndex++
				m.input.SetValue(m.history[m.historyIndex])
				m.input.CursorEnd()
			} else if m.historyIndex == len(m.history)-1 {
				m.historyIndex = len(m.history)
				m.input.SetValue("")
			}

		default:
			// Update input
			var cmd tea.Cmd
			m.input, cmd = m.input.Update(msg)
			cmds = append(cmds, cmd)
		}

	default:
		// Update input
		var cmd tea.Cmd
		m.input, cmd = m.input.Update(msg)
		cmds = append(cmds, cmd)
	}

	return m, tea.Batch(cmds...)
}

func (m REPLModel) View() string {
	if !m.ready {
		return "Loading..."
	}

	// Styles
	titleStyle := lipgloss.NewStyle().
		Bold(true).
		Foreground(lipgloss.Color("205")).
		Padding(0, 1)

	outputStyle := lipgloss.NewStyle().
		Border(lipgloss.RoundedBorder()).
		BorderForeground(lipgloss.Color("238")).
		Padding(1)

	inputStyle := lipgloss.NewStyle().
		Border(lipgloss.RoundedBorder()).
		BorderForeground(lipgloss.Color("205")).
		Padding(0, 1)

	// Build the view
	title := titleStyle.Render("JavaScript + Uhoh REPL")
	output := outputStyle.Width(m.width - 4).Height(m.output.Height).Render(m.output.View())
	input := inputStyle.Width(m.width - 4).Render(m.input.View())

	help := lipgloss.NewStyle().
		Foreground(lipgloss.Color("241")).
		Render("Press Ctrl+C to quit • Use ↑/↓ for history • /load <file> to load JS files")

	return lipgloss.JoinVertical(
		lipgloss.Left,
		title,
		output,
		input,
		help,
	)
}

