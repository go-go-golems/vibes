package ui

import (
	"fmt"
	"mento-tui/internal/config"
	"mento-tui/internal/services"
	"strings"

	"github.com/charmbracelet/bubbles/textinput"
	"github.com/charmbracelet/bubbles/viewport"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type ConfigModel struct {
	manager     *services.Manager
	viewport    viewport.Model
	cfg         *config.AppConfig
	width       int
	height      int
	searchMode  bool
	searchInput textinput.Model
	searchQuery string
}

func NewConfigModel(manager *services.Manager, cfg *config.AppConfig) ConfigModel {
	ti := textinput.New()
	ti.Placeholder = "Search env vars..."
	ti.CharLimit = 100
	ti.Width = 50
	ti.Focus()

	return ConfigModel{
		manager:     manager,
		viewport:    viewport.New(80, 20),
		cfg:         cfg,
		searchMode:  false,
		searchInput: ti,
		searchQuery: "",
	}
}

func (m ConfigModel) Init() tea.Cmd {
	return nil
}

func (m ConfigModel) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	var cmd tea.Cmd

	// Handle search mode first
	if m.searchMode {
		switch msg := msg.(type) {
		case tea.KeyMsg:
			switch msg.String() {
			case "esc":
				m.searchMode = false
				m.searchInput.SetValue("")
				m.searchQuery = ""
				m.updateViewport()
				return m, nil
			case "enter":
				m.searchMode = false
				m.searchQuery = m.searchInput.Value()
				m.updateViewport()
				return m, nil
			}
		}
		// Update search input
		m.searchInput, cmd = m.searchInput.Update(msg)
		m.searchQuery = m.searchInput.Value() // Update filter in real-time
		m.updateViewport()
		return m, cmd
	}

	// Normal mode handling
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		m.width = msg.Width
		m.height = msg.Height
		m.viewport.Width = msg.Width - 4
		m.viewport.Height = m.height - 8
		m.searchInput.Width = min(50, m.width-20)
		m.updateViewport()
	case tea.KeyMsg:
		switch msg.String() {
		case "/":
			m.searchMode = true
			m.searchInput.SetValue("")
			m.searchInput.Focus()
			return m, nil
		}
	}

	m.viewport, cmd = m.viewport.Update(msg)
	return m, cmd
}

func (m *ConfigModel) updateViewport() {
	var content strings.Builder
	query := strings.ToLower(m.searchQuery)

	// Environment variables from YAML config (grouped by service)
	content.WriteString(ConfigSectionStyle.Render("ENVIRONMENT (YAML)"))
	content.WriteString("\n\n")
	
	hasMatches := false
	for _, svc := range m.cfg.Services {
		// Filter env vars for this service
		filtered := m.filterEnvVars(svc.EnvVars, query)
		
		// Skip service section if no matches and search is active
		if query != "" && len(filtered) == 0 {
			continue
		}
		
		if len(filtered) > 0 {
			hasMatches = true
		}
		
		// Service header
		content.WriteString(ConfigSectionStyle.Render(strings.ToUpper(svc.Name)))
		content.WriteString("\n")
		// Env vars box
		content.WriteString(m.renderEnvVarsBox(filtered))
		content.WriteString("\n\n")
	}
	
	// Show message if search active but no matches
	if query != "" && !hasMatches {
		content.WriteString(ConfigValueStyle.Render("No matches found for: " + m.searchQuery))
		content.WriteString("\n")
	}

	m.viewport.SetContent(content.String())
}

func (m ConfigModel) filterEnvVars(items []string, query string) []string {
	if query == "" {
		return items
	}
	
	queryLower := strings.ToLower(query)
	filtered := make([]string, 0)
	
	for _, kv := range items {
		// Check if key or value matches (case-insensitive)
		kvLower := strings.ToLower(kv)
		if strings.Contains(kvLower, queryLower) {
			filtered = append(filtered, kv)
		}
	}
	
	return filtered
}

func (m ConfigModel) renderEnvVarsBox(items []string) string {
	var content strings.Builder
	for _, kv := range items {
		// Parse KEY=VALUE for better formatting
		parts := strings.SplitN(kv, "=", 2)
		if len(parts) == 2 {
			key := parts[0]
			value := parts[1]
			line := fmt.Sprintf("%s  %s",
				ConfigKeyStyle.Render(key),
				ConfigValueStyle.Render(value))
			content.WriteString(line)
		} else {
			// Fallback if no = found
			line := fmt.Sprintf("%s",
				ConfigValueStyle.Render(kv))
			content.WriteString(line)
		}
		content.WriteString("\n")
	}

	return ConfigBoxStyle.Width(m.width - 8).Render(content.String())
}

func (m ConfigModel) View() string {
	if m.width == 0 {
		return "Loading..."
	}

	m.updateViewport()

	var b strings.Builder

	// Header with search mode support
	var header string
	if m.searchMode {
		left := " CONFIGURATION"
		searchPrompt := "Search: " + m.searchInput.View()
		right := "[Enter] Apply  [ESC] Cancel"
		rightW := lipgloss.Width(right)
		searchW := max(0, m.width-rightW-lipgloss.Width(left))
		
		header = lipgloss.NewStyle().
			Width(m.width).
			BorderStyle(lipgloss.NormalBorder()).
			BorderBottom(true).
			BorderForeground(ColorBorder).
			Render(lipgloss.JoinHorizontal(lipgloss.Top,
				lipgloss.NewStyle().Width(lipgloss.Width(left)).Render(left),
				lipgloss.NewStyle().Width(searchW).Render(searchPrompt),
				lipgloss.NewStyle().Width(rightW).Align(lipgloss.Right).Render(right),
			))
	} else {
		left := " CONFIGURATION"
		right := "[E] Edit  [/] Search  [ESC] Back"
		rightW := lipgloss.Width(right)
		leftW := max(0, m.width-rightW)
		
		header = lipgloss.NewStyle().
			Width(m.width).
			BorderStyle(lipgloss.NormalBorder()).
			BorderBottom(true).
			BorderForeground(ColorBorder).
			Render(lipgloss.JoinHorizontal(lipgloss.Top,
				lipgloss.NewStyle().Width(leftW).Render(left),
				lipgloss.NewStyle().Width(rightW).Align(lipgloss.Right).Render(right),
			))
	}

	b.WriteString(header)
	b.WriteString("\n\n")

	// Viewport with config
	b.WriteString(m.viewport.View())
	
	// Footer with search status (if search active)
	if m.searchQuery != "" {
		b.WriteString("\n")
		footer := lipgloss.NewStyle().
			Width(m.width).
			BorderStyle(lipgloss.NormalBorder()).
			BorderTop(true).
			BorderForeground(ColorBorder).
			Padding(0, 1).
			Render(" Filter: '" + m.searchQuery + "'")
		b.WriteString(footer)
	}

	return b.String()
}
