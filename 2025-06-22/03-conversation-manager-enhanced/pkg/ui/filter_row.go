package ui

import (
	"fmt"
	"strings"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/conversation-manager/pkg/models"
)

// FilterRowModel handles the display and management of filter buttons
type FilterRowModel struct {
	dateFilters     []FilterButton
	tagFilters      []FilterButton
	modelFilters    []FilterButton
	currentCategory int // 0=date, 1=tags, 2=models
	currentButton   int
	width           int
	height          int
	styles          *Styles
	focused         bool
	visible         bool
	activeFilters   models.FilterOptions
	
	// Enhanced tag filtering
	tagFilterModel  *TagFilterModel
	showTagDetails  bool
	tagManager      *models.TagManager
}

// FilterButton represents a single filter button
type FilterButton struct {
	Label  string
	Value  string
	Active bool
	Count  int // Number of items matching this filter
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
		tagFilterModel:  NewTagFilterModel(),
		showTagDetails:  false,
		tagManager:      models.NewTagManager(),
		activeFilters: models.FilterOptions{
			DateRange: "all",
			Tags:      []string{},
			Models:    []string{},
		},
	}
}

// Init initializes the filter row model
func (m *FilterRowModel) Init() tea.Cmd {
	return tea.Batch(
		m.tagFilterModel.Init(),
	)
}

// Update handles messages for the filter row model
func (m *FilterRowModel) Update(msg tea.Msg) (*FilterRowModel, tea.Cmd) {
	var cmds []tea.Cmd
	
	// Update tag filter model
	var tagCmd tea.Cmd
	m.tagFilterModel, tagCmd = m.tagFilterModel.Update(msg)
	if tagCmd != nil {
		cmds = append(cmds, tagCmd)
	}
	
	switch msg := msg.(type) {
	case ConversationsLoadedMsg:
		m.updateFiltersFromConversations(msg.Conversations)
		m.tagManager.UpdateFromConversations(msg.Conversations)
		
	case TagFilterChangedMsg:
		m.activeFilters.Tags = msg.SelectedTags
		m.updateTagFilterButtons()
		cmds = append(cmds, func() tea.Msg {
			return FilterChangedMsg{Options: m.activeFilters}
		})
		
	case tea.KeyMsg:
		if !m.focused || !m.visible {
			return m, tea.Batch(cmds...)
		}
		
		// Handle tag detail view
		if m.showTagDetails {
			switch msg.String() {
			case "escape", "t":
				m.showTagDetails = false
				m.tagFilterModel.SetVisible(false)
			default:
				// Let tag filter model handle the input
				return m, tea.Batch(cmds...)
			}
			return m, tea.Batch(cmds...)
		}
		
		switch msg.String() {
		case "left", "h":
			if m.currentCategory > 0 {
				m.currentCategory--
				m.currentButton = 0
			}
			
		case "right", "l":
			maxCategory := 2
			if m.currentCategory < maxCategory {
				m.currentCategory++
				m.currentButton = 0
			}
			
		case "up", "k":
			if m.currentButton > 0 {
				m.currentButton--
			}
			
		case "down", "j":
			maxButton := m.getMaxButtonIndex()
			if m.currentButton < maxButton {
				m.currentButton++
			}
			
		case " ", "enter":
			cmds = append(cmds, m.toggleCurrentFilter())
			
		case "t":
			// Open tag details view
			if m.currentCategory == 1 { // Tags category
				m.showTagDetails = true
				m.tagFilterModel.SetVisible(true)
				m.tagFilterModel.SetFocused(true)
			}
			
		case "c":
			// Clear current category filters
			cmds = append(cmds, m.clearCurrentCategory())
			
		case "C":
			// Clear all filters
			cmds = append(cmds, m.clearAllFilters())
		}
	}
	
	return m, tea.Batch(cmds...)
}

// updateFiltersFromConversations updates filter options based on available conversations
func (m *FilterRowModel) updateFiltersFromConversations(conversations []models.ConversationSummary) {
	// Update tag filters
	tagCounts := make(map[string]int)
	modelCounts := make(map[string]int)
	
	for _, conv := range conversations {
		// Count tags
		for _, tag := range conv.Tags {
			tagCounts[tag]++
		}
		
		// Count models
		if conv.Model != "" {
			modelCounts[conv.Model]++
		}
	}
	
	// Update tag filter buttons
	m.tagFilters = []FilterButton{}
	popularTags := m.tagManager.GetPopularTags(10)
	for _, tag := range popularTags {
		m.tagFilters = append(m.tagFilters, FilterButton{
			Label:  fmt.Sprintf("%s %s", models.GetTagIcon(tag), tag),
			Value:  tag,
			Active: m.isTagActive(tag),
			Count:  tagCounts[tag],
		})
	}
	
	// Update model filter buttons
	m.modelFilters = []FilterButton{}
	for model, count := range modelCounts {
		m.modelFilters = append(m.modelFilters, FilterButton{
			Label:  model,
			Value:  model,
			Active: m.isModelActive(model),
			Count:  count,
		})
	}
}

// updateTagFilterButtons updates tag filter buttons based on selected tags
func (m *FilterRowModel) updateTagFilterButtons() {
	for i := range m.tagFilters {
		m.tagFilters[i].Active = m.isTagActive(m.tagFilters[i].Value)
	}
}

// isTagActive checks if a tag is currently active
func (m *FilterRowModel) isTagActive(tag string) bool {
	for _, activeTag := range m.activeFilters.Tags {
		if activeTag == tag {
			return true
		}
	}
	return false
}

// isModelActive checks if a model is currently active
func (m *FilterRowModel) isModelActive(model string) bool {
	for _, activeModel := range m.activeFilters.Models {
		if activeModel == model {
			return true
		}
	}
	return false
}

// getMaxButtonIndex returns the maximum button index for the current category
func (m *FilterRowModel) getMaxButtonIndex() int {
	switch m.currentCategory {
	case 0: // Date
		return len(m.dateFilters) - 1
	case 1: // Tags
		return len(m.tagFilters) - 1
	case 2: // Models
		return len(m.modelFilters) - 1
	default:
		return 0
	}
}

// toggleCurrentFilter toggles the current filter
func (m *FilterRowModel) toggleCurrentFilter() tea.Cmd {
	switch m.currentCategory {
	case 0: // Date
		if m.currentButton < len(m.dateFilters) {
			// Clear other date filters (only one can be active)
			for i := range m.dateFilters {
				m.dateFilters[i].Active = false
			}
			m.dateFilters[m.currentButton].Active = true
			m.activeFilters.DateRange = m.dateFilters[m.currentButton].Value
		}
		
	case 1: // Tags
		if m.currentButton < len(m.tagFilters) {
			tag := m.tagFilters[m.currentButton].Value
			m.tagFilters[m.currentButton].Active = !m.tagFilters[m.currentButton].Active
			
			if m.tagFilters[m.currentButton].Active {
				// Add tag
				m.activeFilters.Tags = append(m.activeFilters.Tags, tag)
			} else {
				// Remove tag
				for i, activeTag := range m.activeFilters.Tags {
					if activeTag == tag {
						m.activeFilters.Tags = append(m.activeFilters.Tags[:i], m.activeFilters.Tags[i+1:]...)
						break
					}
				}
			}
		}
		
	case 2: // Models
		if m.currentButton < len(m.modelFilters) {
			model := m.modelFilters[m.currentButton].Value
			m.modelFilters[m.currentButton].Active = !m.modelFilters[m.currentButton].Active
			
			if m.modelFilters[m.currentButton].Active {
				// Add model
				m.activeFilters.Models = append(m.activeFilters.Models, model)
			} else {
				// Remove model
				for i, activeModel := range m.activeFilters.Models {
					if activeModel == model {
						m.activeFilters.Models = append(m.activeFilters.Models[:i], m.activeFilters.Models[i+1:]...)
						break
					}
				}
			}
		}
	}
	
	return func() tea.Msg {
		return FilterChangedMsg{Options: m.activeFilters}
	}
}

// clearCurrentCategory clears filters for the current category
func (m *FilterRowModel) clearCurrentCategory() tea.Cmd {
	switch m.currentCategory {
	case 0: // Date
		for i := range m.dateFilters {
			m.dateFilters[i].Active = false
		}
		m.dateFilters[4].Active = true // "All time"
		m.activeFilters.DateRange = "all"
		
	case 1: // Tags
		for i := range m.tagFilters {
			m.tagFilters[i].Active = false
		}
		m.activeFilters.Tags = []string{}
		
	case 2: // Models
		for i := range m.modelFilters {
			m.modelFilters[i].Active = false
		}
		m.activeFilters.Models = []string{}
	}
	
	return func() tea.Msg {
		return FilterChangedMsg{Options: m.activeFilters}
	}
}

// clearAllFilters clears all active filters
func (m *FilterRowModel) clearAllFilters() tea.Cmd {
	// Clear date filters
	for i := range m.dateFilters {
		m.dateFilters[i].Active = false
	}
	m.dateFilters[4].Active = true // "All time"
	
	// Clear tag filters
	for i := range m.tagFilters {
		m.tagFilters[i].Active = false
	}
	
	// Clear model filters
	for i := range m.modelFilters {
		m.modelFilters[i].Active = false
	}
	
	m.activeFilters = models.FilterOptions{
		DateRange: "all",
		Tags:      []string{},
		Models:    []string{},
	}
	
	return func() tea.Msg {
		return FilterChangedMsg{Options: m.activeFilters}
	}
}

// View renders the filter row model
func (m *FilterRowModel) View() string {
	if !m.visible {
		return ""
	}
	
	// Show tag details if active
	if m.showTagDetails {
		return m.tagFilterModel.View()
	}
	
	var sections []string
	
	// Category headers
	categories := []string{"📅 Date", "🏷️ Tags", "🤖 Models"}
	var categoryHeaders []string
	
	for i, category := range categories {
		style := m.styles.FilterButton
		if i == m.currentCategory && m.focused {
			style = m.styles.FilterButtonActive
		}
		
		// Add active filter count
		activeCount := m.getActiveCategoryCount(i)
		if activeCount > 0 {
			category += fmt.Sprintf(" (%d)", activeCount)
		}
		
		categoryHeaders = append(categoryHeaders, style.Render(category))
	}
	
	headerRow := lipgloss.JoinHorizontal(lipgloss.Left, categoryHeaders...)
	sections = append(sections, headerRow)
	
	// Filter buttons for current category
	var filterButtons []string
	
	switch m.currentCategory {
	case 0: // Date
		for i, filter := range m.dateFilters {
			style := m.styles.FilterButton
			if filter.Active {
				style = m.styles.FilterButtonSelected
			}
			if i == m.currentButton && m.focused {
				style = m.styles.FilterButtonActive
			}
			
			filterButtons = append(filterButtons, style.Render(filter.Label))
		}
		
	case 1: // Tags
		for i, filter := range m.tagFilters {
			style := m.styles.FilterButton
			if filter.Active {
				style = m.styles.FilterButtonSelected
			}
			if i == m.currentButton && m.focused {
				style = m.styles.FilterButtonActive
			}
			
			label := filter.Label
			if filter.Count > 0 {
				label += fmt.Sprintf(" (%d)", filter.Count)
			}
			
			filterButtons = append(filterButtons, style.Render(label))
		}
		
		// Add "More tags..." button
		moreStyle := m.styles.FilterButton
		if len(m.tagFilters) == m.currentButton && m.focused {
			moreStyle = m.styles.FilterButtonActive
		}
		filterButtons = append(filterButtons, moreStyle.Render("More tags... (t)"))
		
	case 2: // Models
		for i, filter := range m.modelFilters {
			style := m.styles.FilterButton
			if filter.Active {
				style = m.styles.FilterButtonSelected
			}
			if i == m.currentButton && m.focused {
				style = m.styles.FilterButtonActive
			}
			
			label := filter.Label
			if filter.Count > 0 {
				label += fmt.Sprintf(" (%d)", filter.Count)
			}
			
			filterButtons = append(filterButtons, style.Render(label))
		}
	}
	
	if len(filterButtons) > 0 {
		buttonRow := lipgloss.JoinHorizontal(lipgloss.Left, filterButtons...)
		sections = append(sections, buttonRow)
	}
	
	// Help text
	var helpItems []string
	helpItems = append(helpItems, "←/→: Categories")
	helpItems = append(helpItems, "↑/↓: Options")
	helpItems = append(helpItems, "Space: Toggle")
	if m.currentCategory == 1 {
		helpItems = append(helpItems, "t: Tag details")
	}
	helpItems = append(helpItems, "c: Clear category")
	helpItems = append(helpItems, "C: Clear all")
	
	help := m.styles.Help.Render(strings.Join(helpItems, " • "))
	sections = append(sections, help)
	
	return lipgloss.JoinVertical(lipgloss.Left, sections...)
}

// getActiveCategoryCount returns the number of active filters in a category
func (m *FilterRowModel) getActiveCategoryCount(category int) int {
	switch category {
	case 0: // Date
		for _, filter := range m.dateFilters {
			if filter.Active && filter.Value != "all" {
				return 1
			}
		}
		return 0
	case 1: // Tags
		return len(m.activeFilters.Tags)
	case 2: // Models
		return len(m.activeFilters.Models)
	default:
		return 0
	}
}

// SetFocused sets the focus state
func (m *FilterRowModel) SetFocused(focused bool) {
	m.focused = focused
	if m.showTagDetails {
		m.tagFilterModel.SetFocused(focused)
	}
}

// SetVisible sets the visibility state
func (m *FilterRowModel) SetVisible(visible bool) {
	m.visible = visible
	if !visible {
		m.showTagDetails = false
		m.tagFilterModel.SetVisible(false)
	}
}

// SetSize sets the model dimensions
func (m *FilterRowModel) SetSize(width, height int) {
	m.width = width
	m.height = height
	m.tagFilterModel.SetSize(width, height)
}

// GetActiveFilters returns the current active filters
func (m *FilterRowModel) GetActiveFilters() models.FilterOptions {
	return m.activeFilters
}

