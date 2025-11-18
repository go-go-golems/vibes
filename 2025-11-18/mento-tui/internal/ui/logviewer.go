package ui

import (
	"fmt"
	"mento-tui/internal/models"
	"mento-tui/internal/services"
	"strings"

	"github.com/charmbracelet/bubbles/viewport"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type LogViewerModel struct {
	manager     *services.Manager
	viewport    viewport.Model
	selectedTab int // 0=Identity, 1=Frontend, 2=Worker, 3=All
	width       int
	height      int
	autoScroll  bool
}

func NewLogViewerModel(manager *services.Manager) LogViewerModel {
	return LogViewerModel{
		manager:     manager,
		viewport:    viewport.New(80, 20),
		selectedTab: 3, // Start with "All"
		autoScroll:  true,
	}
}

func (m LogViewerModel) Init() tea.Cmd {
	return nil
}

func (m LogViewerModel) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		m.width = msg.Width
		m.height = msg.Height
		m.viewport.Width = msg.Width - 4
		m.viewport.Height = msg.Height - 10
	case tea.KeyMsg:
		switch msg.String() {
		case "tab":
			m.selectedTab = (m.selectedTab + 1) % 4
			m.updateViewport()
		case "1":
			m.selectedTab = 0
			m.updateViewport()
		case "2":
			m.selectedTab = 1
			m.updateViewport()
		case "3":
			m.selectedTab = 2
			m.updateViewport()
		case "4":
			m.selectedTab = 3
			m.updateViewport()
		}
	}

	var cmd tea.Cmd
	m.viewport, cmd = m.viewport.Update(msg)
	return m, cmd
}

func (m *LogViewerModel) updateViewport() {
	var lines []models.LogLine
	switch m.selectedTab {
	case 0:
		lines = m.manager.GlobalLog.GetFilteredLines("Identity Server")
	case 1:
		lines = m.manager.GlobalLog.GetFilteredLines("Frontend (Vite)")
	case 2:
		lines = m.manager.GlobalLog.GetFilteredLines("Mento Worker")
	case 3:
		lines = m.manager.GlobalLog.GetLines()
	}

	var content strings.Builder
	for _, line := range lines {
		timestamp := line.Timestamp.Format("15:04:05")
		content.WriteString(fmt.Sprintf("%s [%s] %s\n",
			LogTimestampStyle.Render(timestamp),
			LogServiceStyle.Render(line.Service),
			line.Message))
	}

	m.viewport.SetContent(content.String())
	if m.autoScroll {
		m.viewport.GotoBottom()
	}
}

func (m LogViewerModel) View() string {
	if m.width == 0 {
		return "Loading..."
	}

	// Minimum width check
	if m.width < 40 {
		return lipgloss.NewStyle().
			Foreground(ColorError).
			Padding(1, 2).
			Render("Terminal too narrow. Please widen the window (minimum 40 characters).")
	}

	// Update viewport content
	m.updateViewport()

	var b strings.Builder

	// Header using Lipgloss JoinHorizontal
	left := " LOG VIEWER"
	right := "[TAB] Switch  [/] Search  [ESC] Back"
	rightW := lipgloss.Width(right)
	leftW := max(0, m.width-rightW)

	header := lipgloss.NewStyle().
		Width(m.width).
		BorderStyle(lipgloss.NormalBorder()).
		BorderBottom(true).
		BorderForeground(ColorBorder).
		Render(lipgloss.JoinHorizontal(lipgloss.Top,
			lipgloss.NewStyle().Width(leftW).Render(left),
			lipgloss.NewStyle().Width(rightW).Align(lipgloss.Right).Render(right),
		))

	b.WriteString(header)
	b.WriteString("\n")

	// Tab bar using Lipgloss JoinHorizontal
	tabs := []string{"Identity", "Frontend", "Worker", "All"}
	var tabsSeg strings.Builder
	tabsSeg.WriteString(" ")
	for i, tab := range tabs {
		if i == m.selectedTab {
			tabsSeg.WriteString(ButtonActiveStyle.Render(tab))
		} else {
			tabsSeg.WriteString(ButtonStyle.Render(tab))
		}
	}
	autoScrollStatus := "OFF"
	if m.autoScroll {
		autoScrollStatus = "ON"
	}
	rightTab := fmt.Sprintf("Auto-scroll: %s", autoScrollStatus)
	rightTabW := lipgloss.Width(rightTab)
	leftTabW := max(0, m.width-rightTabW)

	tabBar := lipgloss.JoinHorizontal(lipgloss.Top,
		lipgloss.NewStyle().Width(leftTabW).Render(tabsSeg.String()),
		lipgloss.NewStyle().Width(rightTabW).Align(lipgloss.Right).Render(rightTab),
	)

	b.WriteString(tabBar)
	b.WriteString("\n")

	// Separator
	b.WriteString(lipgloss.NewStyle().
		Width(m.width).
		BorderStyle(lipgloss.NormalBorder()).
		BorderTop(true).
		BorderForeground(ColorBorder).
		Render(""))
	b.WriteString("\n")

	// Viewport with logs
	b.WriteString(m.viewport.View())
	b.WriteString("\n")

	// Footer using Lipgloss JoinHorizontal
	lines := m.manager.GlobalLog.GetLines()
	leftFooter := " Filter: <none>"
	rightFooter := fmt.Sprintf("Lines: %d / %d", len(lines), len(lines))
	rightFooterW := lipgloss.Width(rightFooter)
	leftFooterW := max(0, m.width-rightFooterW)

	footer := lipgloss.NewStyle().
		Width(m.width).
		BorderStyle(lipgloss.NormalBorder()).
		BorderTop(true).
		BorderForeground(ColorBorder).
		Padding(0, 1).
		Render(lipgloss.JoinHorizontal(lipgloss.Top,
			lipgloss.NewStyle().Width(leftFooterW).Render(leftFooter),
			lipgloss.NewStyle().Width(rightFooterW).Align(lipgloss.Right).Render(rightFooter),
		))

	b.WriteString(footer)

	return b.String()
}
