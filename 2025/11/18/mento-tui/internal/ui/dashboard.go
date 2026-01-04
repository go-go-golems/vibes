package ui

import (
	"fmt"
	"mento-tui/internal/models"
	"mento-tui/internal/services"
	"strings"
	"time"

	"github.com/charmbracelet/bubbles/viewport"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type DashboardModel struct {
	manager    *services.Manager
	viewport   viewport.Model
	width      int
	height     int
	lastUpdate time.Time
}

func NewDashboardModel(manager *services.Manager) DashboardModel {
	return DashboardModel{
		manager:    manager,
		viewport:   viewport.New(80, 20),
		lastUpdate: time.Now(),
	}
}

func (m DashboardModel) Init() tea.Cmd {
	return nil
}

func (m DashboardModel) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		m.width = msg.Width
		m.height = msg.Height
		m.viewport.Width = msg.Width
		m.viewport.Height = msg.Height - 10
	case tea.KeyMsg:
		switch msg.String() {
		case "up", "k":
			if m.manager.SelectedIndex > 0 {
				m.manager.SelectedIndex--
			}
		case "down", "j":
			if m.manager.SelectedIndex < len(m.manager.Services)-1 {
				m.manager.SelectedIndex++
			}
		case "r":
			// Restart selected service
			go m.manager.RestartService(m.manager.SelectedIndex)
		case "s":
			// Stop selected service
			go m.manager.StopService(m.manager.SelectedIndex)
		case "a":
			// Start all services
			go m.manager.StartAll()
		case "x":
			// Stop all services
			go m.manager.StopAll()
		}
	}

	var cmd tea.Cmd
	m.viewport, cmd = m.viewport.Update(msg)
	return m, cmd
}

func (m DashboardModel) View() string {
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

	var b strings.Builder

	// Header using Lipgloss JoinHorizontal
	left := " MENTO SERVICES MANAGER"
	right := "[Q] Quit  [H] Help"
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
	b.WriteString("\n\n")

	// Service Status Title using Lipgloss JoinHorizontal
	uptime := m.manager.GetUptime()
	uptimeStr := fmt.Sprintf("%02d:%02d:%02d",
		int(uptime.Hours()),
		int(uptime.Minutes())%60,
		int(uptime.Seconds())%60)

	leftStatus := " SERVICE STATUS"
	rightStatus := fmt.Sprintf("Uptime: %s", uptimeStr)
	rightStatusW := lipgloss.Width(rightStatus)
	leftStatusW := max(0, m.width-rightStatusW)

	statusLine := lipgloss.JoinHorizontal(lipgloss.Top,
		lipgloss.NewStyle().Width(leftStatusW).Render(leftStatus),
		lipgloss.NewStyle().Width(rightStatusW).Align(lipgloss.Right).Render(rightStatus),
	)
	b.WriteString(statusLine)
	b.WriteString("\n\n")

	// Service Cards
	for i, svc := range m.manager.Services {
		b.WriteString(m.renderServiceCard(svc, i == m.manager.SelectedIndex))
		b.WriteString("\n")
	}

	// Footer with quick actions
	footer := lipgloss.NewStyle().
		Width(m.width).
		BorderStyle(lipgloss.NormalBorder()).
		BorderTop(true).
		BorderForeground(ColorBorder).
		Padding(0, 1).
		Render(" QUICK ACTIONS\n [A] Start All  [X] Stop All  [C] Config  [E] Environment  [T] Tail Logs")

	b.WriteString("\n")
	b.WriteString(footer)

	return b.String()
}

func (m DashboardModel) renderServiceCard(svc *models.Service, selected bool) string {
	style := ServiceCardStyle
	if selected {
		style = ServiceCardSelectedStyle
	}

	var content strings.Builder

	// Service name and port(s) using Lipgloss Width
	portStr := formatPorts(svc.Ports)
	nameCol := lipgloss.NewStyle().Width(50).Render(ServiceNameStyle.Render(svc.Name))
	nameLine := nameCol + fmt.Sprintf("Port: %s", portStr)
	content.WriteString(nameLine)
	content.WriteString("\n")

	// Status and PID using Lipgloss Width
	statusStyle := StatusStyle(svc.Status.String())
	pidInfo := ""
	if svc.PID > 0 {
		pidInfo = fmt.Sprintf("PID: %d", svc.PID)
	}
	statusCol := lipgloss.NewStyle().Width(50).Render(statusStyle.Render(svc.Status.String()))
	statusLine := fmt.Sprintf("%s %s%s", svc.Status.Icon(), statusCol, pidInfo)
	content.WriteString(statusLine)
	content.WriteString("\n")

	// CPU and Memory (only if running)
	if svc.Status == models.StatusRunning {
		statsLine := fmt.Sprintf("CPU: %.1f%%  MEM: %dMB",
			svc.CPUPercent, svc.MemoryMB)
		content.WriteString(statsLine)
		content.WriteString("\n")
	}

	// Actions (only for selected service)
	if selected {
		actions := "[R] Restart  [S] Stop  [L] View Logs"
		if svc.Status == models.StatusStopped {
			actions = "[Enter] Start  [L] View Logs"
		}
		content.WriteString(actions)
	}

	return style.Width(m.width - 8).Render(content.String())
}

// formatPorts formats ports for display
func formatPorts(ports []int) string {
	if len(ports) == 0 {
		return "N/A"
	}
	if len(ports) == 1 {
		return fmt.Sprintf("%d", ports[0])
	}
	// Multiple ports: join with comma
	portStrs := make([]string, len(ports))
	for i, p := range ports {
		portStrs[i] = fmt.Sprintf("%d", p)
	}
	return strings.Join(portStrs, ", ")
}
