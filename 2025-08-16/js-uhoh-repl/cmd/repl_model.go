package main

import (
	"context"
	"encoding/json"
	"fmt"
	"strings"

	"github.com/charmbracelet/bubbles/textarea"
	"github.com/charmbracelet/bubbles/viewport"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	uhoh "github.com/go-go-golems/uhoh/pkg"
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

	// Optional active uhoh form model
	child tea.Model
	// Initial values from uhoh
	childVals map[string]interface{}
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

	// Redirect console.log into the history output
	eval.SetOutputHook(func(line string) {
		current := output.View()
		output.SetContent(current + line + "\n")
		output.GotoBottom()
	})

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

	if m.child != nil {
		// Forward all msgs to the child while active; allow Ctrl+C to quit the REPL
		switch t := msg.(type) {
		case tea.KeyMsg:
			if t.Type == tea.KeyCtrlC {
				return m, tea.Quit
			}
		}
		var cmd tea.Cmd
		m.child, cmd = m.child.Update(msg)
		cmds = append(cmds, cmd)
		return m, tea.Batch(cmds...)
	}

	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		m.width = msg.Width
		m.height = msg.Height

		if !m.ready {
			m.ready = true
		}
		inputHeight := 4
		outputHeight := m.height - inputHeight - 2
		m.input.SetWidth(m.width - 2)
		m.input.SetHeight(inputHeight)
		m.output.Width = m.width - 2
		m.output.Height = outputHeight

	case tea.KeyMsg:
		switch msg.String() {
		case "ctrl+c":
			return m, tea.Quit
		case "enter":
			code := strings.TrimSpace(m.input.Value())
			if code == "" {
				break
			}
			m.history = append(m.history, code)
			m.historyIndex = len(m.history)
			m.input.SetValue("")

			// Evaluate
			result, err := m.evaluator.Evaluate(context.Background(), code)

			// Detect UI signal (JSON object with __uhoh_ui__ true)
			uiSignal := false
			var maybeObj map[string]interface{}
			if err == nil && strings.HasPrefix(strings.TrimSpace(result), "{") {
				_ = json.Unmarshal([]byte(result), &maybeObj)
				if b, ok := maybeObj["__uhoh_ui__"].(bool); ok && b {
					uiSignal = true
				}
			}

			if uiSignal {
				// Build child model from YAML in signal
				yamlStr, _ := maybeObj["form_yaml"].(string)
				form, vals, buildErr := uhoh.BuildBubbleTeaModelFromYAML([]byte(yamlStr))
				if buildErr != nil {
					result = fmt.Sprintf("error: %v", buildErr)
				} else {
					m.child = form
					m.childVals = vals
					// Do not print result now; child view will take over
					return m, nil
				}
			}

			// No UI; render result to output
			outputLine := fmt.Sprintf("js-uhoh> %s\n", code)
			if err != nil {
				outputLine += fmt.Sprintf("Error: %v\n\n", err)
			} else {
				outputLine += fmt.Sprintf("%s\n\n", result)
			}
			currentContent := m.output.View()
			m.output.SetContent(currentContent + outputLine)
			m.output.GotoBottom()
		default:
			var cmd tea.Cmd
			m.input, cmd = m.input.Update(msg)
			cmds = append(cmds, cmd)
		}

	default:
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

	var bottom string
	if m.child != nil {
		bottom = inputStyle.Width(m.width - 4).Render(m.child.View())
	} else {
		bottom = inputStyle.Width(m.width - 4).Render(m.input.View())
	}

	help := lipgloss.NewStyle().
		Foreground(lipgloss.Color("241")).
		Render("Press Ctrl+C to quit • Use ↑/↓ for history • /load <file> to load JS files")

	return lipgloss.JoinVertical(
		lipgloss.Left,
		title,
		output,
		bottom,
		help,
	)
}

