package ui

import (
	"fmt"
	"mento-tui/internal/services"
	"strings"

	"github.com/charmbracelet/bubbles/viewport"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type HelpModel struct {
	manager  *services.Manager
	viewport viewport.Model
	width    int
	height   int
}

func NewHelpModel(manager *services.Manager) HelpModel {
	return HelpModel{
		manager:  manager,
		viewport: viewport.New(80, 20),
	}
}

func (m HelpModel) Init() tea.Cmd {
	return nil
}

func (m HelpModel) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		m.width = msg.Width
		m.height = msg.Height
		m.viewport.Width = msg.Width - 4
		m.viewport.Height = msg.Height - 8
		m.updateViewport()
	}

	var cmd tea.Cmd
	m.viewport, cmd = m.viewport.Update(msg)
	return m, cmd
}

func (m *HelpModel) updateViewport() {
	var content strings.Builder

	sections := []struct {
		title    string
		bindings []struct{ key, desc string }
	}{
		{
			title: "GLOBAL KEYS",
			bindings: []struct{ key, desc string }{
				{"q, ctrl+c", "Quit application"},
				{"h, ?", "Show this help screen"},
				{"esc", "Go back / Return to dashboard"},
			},
		},
		{
			title: "DASHBOARD",
			bindings: []struct{ key, desc string }{
				{"↑/k, ↓/j", "Navigate between services"},
				{"r", "Restart selected service"},
				{"s", "Stop selected service"},
				{"enter", "Start selected service (if stopped)"},
				{"a", "Start all services"},
				{"x", "Stop all services"},
				{"t", "View tail logs"},
				{"c", "View configuration"},
				{"e", "View environment"},
			},
		},
		{
			title: "LOG VIEWER",
			bindings: []struct{ key, desc string }{
				{"tab", "Switch between service tabs"},
				{"1-4", "Jump to specific tab (Identity/Frontend/Worker/All)"},
				{"↑/k, ↓/j", "Scroll logs"},
				{"g", "Go to top"},
				{"G", "Go to bottom"},
				{"/", "Search logs (coming soon)"},
				{"f", "Filter logs (coming soon)"},
			},
		},
		{
			title: "CONFIGURATION",
			bindings: []struct{ key, desc string }{
				{"↑/k, ↓/j", "Scroll configuration"},
				{"e", "Edit configuration (coming soon)"},
			},
		},
	}

	for _, section := range sections {
		content.WriteString(ConfigSectionStyle.Render(section.title))
		content.WriteString("\n\n")

		for _, binding := range section.bindings {
			line := fmt.Sprintf("  %s%s%s",
				HelpKeyStyle.Render(binding.key),
				strings.Repeat(" ", 20-len(binding.key)),
				HelpDescStyle.Render(binding.desc))
			content.WriteString(line)
			content.WriteString("\n")
		}
		content.WriteString("\n")
	}

	// About section
	content.WriteString(ConfigSectionStyle.Render("ABOUT"))
	content.WriteString("\n\n")
	content.WriteString("  Mento Services Manager - TUI Application\n")
	content.WriteString("  Built with Bubble Tea framework\n")
	content.WriteString("  Manages Identity Server, Frontend (Vite), and Mento Worker\n")

	m.viewport.SetContent(content.String())
}

func (m HelpModel) View() string {
	if m.width == 0 {
		return "Loading..."
	}

	m.updateViewport()

	var b strings.Builder

	// Header
	header := lipgloss.NewStyle().
		Width(m.width).
		BorderStyle(lipgloss.NormalBorder()).
		BorderBottom(true).
		BorderForeground(ColorBorder).
		Render(fmt.Sprintf(" HELP%s[ESC] Back",
			strings.Repeat(" ", m.width-20)))

	b.WriteString(header)
	b.WriteString("\n\n")

	// Viewport with help
	b.WriteString(m.viewport.View())

	return b.String()
}
