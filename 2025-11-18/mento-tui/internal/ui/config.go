package ui

import (
	"fmt"
	"mento-tui/internal/models"
	"mento-tui/internal/services"
	"os"
	"strings"

	"github.com/charmbracelet/bubbles/viewport"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type ConfigModel struct {
	manager  *services.Manager
	viewport viewport.Model
	config   *models.Config
	width    int
	height   int
}

func NewConfigModel(manager *services.Manager) ConfigModel {
	config := loadConfig()
	return ConfigModel{
		manager:  manager,
		viewport: viewport.New(80, 20),
		config:   config,
	}
}

func loadConfig() *models.Config {
	return &models.Config{
		EnvSources: []models.EnvSource{
			{Path: ".envrc", Loaded: true},
			{Path: ".env.local", Loaded: true},
			{Path: "web/sso-app/.env.staging", Loaded: true},
		},
		Database: map[string]string{
			"ONE_ON_ONE_V3_DATABASE_URL": maskSecret(os.Getenv("ONE_ON_ONE_V3_DATABASE_URL")),
			"WORKFLOWS_DATABASE_URL":     maskSecret(os.Getenv("WORKFLOWS_DATABASE_URL")),
			"IDENTITY_SERVICE_DB_DSN":    maskSecret("postgres://postgres:***@localhost:5432"),
			"IDENTITY_SERVICE_DB_DRIVER": "pgx",
		},
		OAuth: map[string]string{
			"STYTCH_PROJECT_ID":     maskSecret(os.Getenv("STYTCH_PROJECT_ID")),
			"STYTCH_SECRET":         maskSecret("secret-test-***"),
			"GOOGLE_CLIENT_ID":      maskSecret(os.Getenv("GOOGLE_CLIENT_ID")),
			"GOOGLE_CLIENT_SECRET":  maskSecret("GOCSPX-***"),
			"SLACK_CLIENT_ID":       maskSecret("123456789.***"),
			"SLACK_CLIENT_SECRET":   maskSecret("***"),
			"GITHUB_CLIENT_ID":      maskSecret("Iv1.***"),
			"LINEAR_CLIENT_ID":      maskSecret("***"),
		},
		ServiceConfig: map[string]string{
			"IDENTITY_SERVICE_PORT":         "8083",
			"VITE_PORT":                     "5173",
			"MENTO_SERVICE_PORT":            "8082",
			"MENTO_SERVICE_PUBLIC_BASE_URL": "http://localhost:8082",
			"LOG_LEVEL":                     "debug",
		},
	}
}

func maskSecret(s string) string {
	if s == "" {
		return "not set"
	}
	if len(s) <= 10 {
		return "***"
	}
	// Show first few chars and mask the rest
	if strings.Contains(s, "@") {
		// For URLs, mask password
		parts := strings.Split(s, "@")
		if len(parts) > 1 {
			userPass := strings.Split(parts[0], ":")
			if len(userPass) > 1 {
				return userPass[0] + ":***@" + parts[1]
			}
		}
	}
	// For other secrets, show prefix
	if len(s) > 20 {
		return s[:15] + "***"
	}
	return s[:5] + "***"
}

func (m ConfigModel) Init() tea.Cmd {
	return nil
}

func (m ConfigModel) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
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

func (m *ConfigModel) updateViewport() {
	var content strings.Builder

	// Environment Sources
	content.WriteString(ConfigSectionStyle.Render("ENVIRONMENT SOURCES"))
	content.WriteString("\n")
	for _, src := range m.config.EnvSources {
		icon := "✅"
		if !src.Loaded {
			icon = "❌"
		}
		content.WriteString(fmt.Sprintf("%s %s\n", icon, src.Path))
	}
	content.WriteString("\n")

	// Database
	content.WriteString(ConfigSectionStyle.Render("DATABASE"))
	content.WriteString("\n")
	content.WriteString(m.renderConfigBox(m.config.Database))
	content.WriteString("\n")

	// OAuth
	content.WriteString(ConfigSectionStyle.Render("OAUTH CREDENTIALS"))
	content.WriteString("\n")
	content.WriteString(m.renderConfigBox(m.config.OAuth))
	content.WriteString("\n")

	// Service Config
	content.WriteString(ConfigSectionStyle.Render("SERVICE CONFIGURATION"))
	content.WriteString("\n")
	content.WriteString(m.renderConfigBox(m.config.ServiceConfig))

	m.viewport.SetContent(content.String())
}

func (m ConfigModel) renderConfigBox(items map[string]string) string {
	var content strings.Builder
	for key, value := range items {
		line := fmt.Sprintf("%s  %s",
			ConfigKeyStyle.Render(key),
			ConfigValueStyle.Render(value))
		content.WriteString(line)
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

	// Header
	header := lipgloss.NewStyle().
		Width(m.width).
		BorderStyle(lipgloss.NormalBorder()).
		BorderBottom(true).
		BorderForeground(ColorBorder).
		Render(fmt.Sprintf(" CONFIGURATION%s[E] Edit  [ESC] Back",
			strings.Repeat(" ", m.width-45)))

	b.WriteString(header)
	b.WriteString("\n\n")

	// Viewport with config
	b.WriteString(m.viewport.View())

	return b.String()
}
