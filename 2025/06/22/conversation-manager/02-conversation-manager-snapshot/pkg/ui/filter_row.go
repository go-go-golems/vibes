package ui

import (
	"strings"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/conversation-manager/pkg/models"
)

// FilterRowModel handles the display and management of filter buttons
type FilterRowModel struct {
	dateFilters   []FilterButton
	tagFilters    []FilterButton
	modelFilters  []FilterButton
	currentCategory int // 0=date, 1=tags, 2=models
	currentButton   int
	width          int
	height         int
	styles         *Styles
	focused        bool
	visible        bool
	activeFilters  models.FilterOptions
}

// FilterButton represents a single filter button
type FilterButton struct {
	Label  string
	Value  string
	Active bool
}

// NewFilterRowModel creates a new filter row model
func NewFilterRowModel(styles *Styles) *FilterRowModel {
	return &FilterRowModel{
		dateFilters: []FilterButton{
			{Label: "Today", Value: "today", Active: false},
			{Label: "Yesterday", Value: "yesterday", Active: false},
			{Label: "This Week", Value: "this_week", Active: false},
			{Label: "Last 30 days", Value: "last_30_days", Active: false},
			{Label: "All time", Value: "all", Active: true},
		},
		tagFilters:      []FilterButton{},
		modelFilters:    []FilterButton{},
		currentCategory: 0,
		currentButton:   0,
		styles:          styles,
		focused:         false,
		visible:         false,
		activeFilters: models.FilterOptions{
			DateRange: "all",
			Tags:      []string{},
			Models:    []string{},
		},
	}
}

// Init initializes the filter row model
func (m *FilterRowModel) Init() tea.Cmd {
	return nil
}

// Update handles messages for the filter row model
func (m *FilterRowModel) Update(msg tea.Msg) (*FilterRowModel, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.KeyMsg:
		if !m.focused || !m.visible {
			return m, nil
		}

		switch msg.String() {
		case "left", "h":
			m.moveToPreviousCategory()
		case "right", "l":
			m.moveToNextCategory()
		case "up", "k":
			m.moveToPreviousButton()
		case "down", "j":
			m.moveToNextButton()
		case " ":
			return m, m.toggleCurrentFilter()
		case "c":
			return m, m.clearCurrentCategory()
		case "enter":
			return m, m.applyFilters()
		}
	}

	return m, nil
}

// SetFocused sets the focus state of the model
func (m *FilterRowModel) SetFocused(focused bool) {
	m.focused = focused
}

// SetVisible sets the visibility of the filter row
func (m *FilterRowModel) SetVisible(visible bool) {
	m.visible = visible
}

// IsVisible returns whether the filter row is visible
func (m *FilterRowModel) IsVisible() bool {
	return m.visible
}

// SetSize sets the dimensions of the model
func (m *FilterRowModel) SetSize(width, height int) {
	m.width = width
	m.height = height
}

// SetAvailableFilters sets the available tag and model filters
func (m *FilterRowModel) SetAvailableFilters(tags, models []string) {
	// Update tag filters
	m.tagFilters = []FilterButton{}
	for _, tag := range tags {
		m.tagFilters = append(m.tagFilters, FilterButton{
			Label:  strings.Title(tag),
			Value:  tag,
			Active: false,
		})
	}

	// Update model filters
	m.modelFilters = []FilterButton{}
	for _, model := range models {
		m.modelFilters = append(m.modelFilters, FilterButton{
			Label:  model,
			Value:  model,
			Active: false,
		})
	}
}

// moveToPreviousCategory moves to the previous filter category
func (m *FilterRowModel) moveToPreviousCategory() {
	if m.currentCategory > 0 {
		m.currentCategory--
		m.currentButton = 0
	}
}

// moveToNextCategory moves to the next filter category
func (m *FilterRowModel) moveToNextCategory() {
	if m.currentCategory < 2 {
		m.currentCategory++
		m.currentButton = 0
	}
}

// moveToPreviousButton moves to the previous button in current category
func (m *FilterRowModel) moveToPreviousButton() {
	buttons := m.getCurrentCategoryButtons()
	if len(buttons) > 0 && m.currentButton > 0 {
		m.currentButton--
	}
}

// moveToNextButton moves to the next button in current category
func (m *FilterRowModel) moveToNextButton() {
	buttons := m.getCurrentCategoryButtons()
	if len(buttons) > 0 && m.currentButton < len(buttons)-1 {
		m.currentButton++
	}
}

// getCurrentCategoryButtons returns the buttons for the current category
func (m *FilterRowModel) getCurrentCategoryButtons() []FilterButton {
	switch m.currentCategory {
	case 0:
		return m.dateFilters
	case 1:
		return m.tagFilters
	case 2:
		return m.modelFilters
	default:
		return []FilterButton{}
	}
}

// toggleCurrentFilter toggles the current filter button
func (m *FilterRowModel) toggleCurrentFilter() tea.Cmd {
	buttons := m.getCurrentCategoryButtons()
	if len(buttons) == 0 || m.currentButton >= len(buttons) {
		return nil
	}

	switch m.currentCategory {
	case 0: // Date filters (exclusive)
		// Clear all date filters first
		for i := range m.dateFilters {
			m.dateFilters[i].Active = false
		}
		// Activate the selected one
		m.dateFilters[m.currentButton].Active = true
		m.activeFilters.DateRange = m.dateFilters[m.currentButton].Value

	case 1: // Tag filters (multiple)
		m.tagFilters[m.currentButton].Active = !m.tagFilters[m.currentButton].Active
		m.updateActiveTagFilters()

	case 2: // Model filters (multiple)
		m.modelFilters[m.currentButton].Active = !m.modelFilters[m.currentButton].Active
		m.updateActiveModelFilters()
	}

	return func() tea.Msg {
		return FilterAppliedMsg{
			Options: m.activeFilters,
			Results: []models.ConversationSummary{}, // Will be populated by main model
		}
	}
}

// clearCurrentCategory clears all filters in the current category
func (m *FilterRowModel) clearCurrentCategory() tea.Cmd {
	switch m.currentCategory {
	case 0: // Date filters - set to "all"
		for i := range m.dateFilters {
			m.dateFilters[i].Active = m.dateFilters[i].Value == "all"
		}
		m.activeFilters.DateRange = "all"

	case 1: // Tag filters
		for i := range m.tagFilters {
			m.tagFilters[i].Active = false
		}
		m.activeFilters.Tags = []string{}

	case 2: // Model filters
		for i := range m.modelFilters {
			m.modelFilters[i].Active = false
		}
		m.activeFilters.Models = []string{}
	}

	return func() tea.Msg {
		return FilterAppliedMsg{
			Options: m.activeFilters,
			Results: []models.ConversationSummary{}, // Will be populated by main model
		}
	}
}

// applyFilters applies the current filter settings
func (m *FilterRowModel) applyFilters() tea.Cmd {
	return func() tea.Msg {
		return FilterAppliedMsg{
			Options: m.activeFilters,
			Results: []models.ConversationSummary{}, // Will be populated by main model
		}
	}
}

// updateActiveTagFilters updates the active tag filters list
func (m *FilterRowModel) updateActiveTagFilters() {
	activeTags := []string{}
	for _, filter := range m.tagFilters {
		if filter.Active {
			activeTags = append(activeTags, filter.Value)
		}
	}
	m.activeFilters.Tags = activeTags
}

// updateActiveModelFilters updates the active model filters list
func (m *FilterRowModel) updateActiveModelFilters() {
	activeModels := []string{}
	for _, filter := range m.modelFilters {
		if filter.Active {
			activeModels = append(activeModels, filter.Value)
		}
	}
	m.activeFilters.Models = activeModels
}

// GetActiveFilters returns the current active filters
func (m *FilterRowModel) GetActiveFilters() models.FilterOptions {
	return m.activeFilters
}

// View renders the filter row model
func (m *FilterRowModel) View() string {
	if !m.visible {
		return ""
	}

	var content strings.Builder

	// Render active filters summary
	content.WriteString(m.renderActiveFiltersSummary())
	content.WriteString("\n")

	// Render filter categories
	content.WriteString(m.renderFilterCategories())

	return m.styles.FilterRowStyle.Width(m.width).Render(content.String())
}

// renderActiveFiltersSummary renders a summary of active filters
func (m *FilterRowModel) renderActiveFiltersSummary() string {
	var parts []string

	// Date filter
	if m.activeFilters.DateRange != "all" {
		for _, filter := range m.dateFilters {
			if filter.Active {
				parts = append(parts, "📅 "+filter.Label)
				break
			}
		}
	}

	// Tag filters
	if len(m.activeFilters.Tags) > 0 {
		tagStr := "🏷️ " + strings.Join(m.activeFilters.Tags, ", ")
		parts = append(parts, tagStr)
	}

	// Model filters
	if len(m.activeFilters.Models) > 0 {
		modelStr := "🤖 " + strings.Join(m.activeFilters.Models, ", ")
		parts = append(parts, modelStr)
	}

	if len(parts) == 0 {
		return ""
	}

	return strings.Join(parts, " • ")
}

// renderFilterCategories renders the filter category buttons
func (m *FilterRowModel) renderFilterCategories() string {
	var content strings.Builder

	// Date filters
	content.WriteString("📅 ")
	content.WriteString(m.renderFilterButtons(m.dateFilters, 0))
	content.WriteString("\n")

	// Tag filters
	if len(m.tagFilters) > 0 {
		content.WriteString("🏷️ ")
		content.WriteString(m.renderFilterButtons(m.tagFilters, 1))
		content.WriteString("\n")
	}

	// Model filters
	if len(m.modelFilters) > 0 {
		content.WriteString("🤖 ")
		content.WriteString(m.renderFilterButtons(m.modelFilters, 2))
		content.WriteString("\n")
	}

	return content.String()
}

// renderFilterButtons renders a row of filter buttons
func (m *FilterRowModel) renderFilterButtons(buttons []FilterButton, categoryIndex int) string {
	var parts []string

	for i, button := range buttons {
		isSelected := m.focused && m.currentCategory == categoryIndex && m.currentButton == i
		
		var style lipgloss.Style
		if button.Active {
			style = m.styles.ActiveFilterButton
		} else {
			style = m.styles.InactiveFilterButton
		}

		// Add selection indicator
		text := button.Label
		if isSelected {
			text = "[" + text + "]"
		}

		parts = append(parts, style.Render(text))
	}

	return strings.Join(parts, " ")
}

