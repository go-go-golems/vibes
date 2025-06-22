package ui

import (
	"fmt"
	"strings"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/conversation-manager/pkg/models"
)

// TagFilterModel handles tag-specific filtering
type TagFilterModel struct {
	width    int
	height   int
	focused  bool
	visible  bool
	
	// Tag management
	tagManager    *models.TagManager
	allTags       []string
	selectedTags  []string
	tagCategories []models.TagCategory
	
	// UI state
	currentCategory int
	currentTag      int
	filterMode      string // "category" or "tags"
	searchQuery     string
	searchActive    bool
	
	// Display options
	showCounts      bool
	maxTagsPerRow   int
	
	styles *Styles
}

// NewTagFilterModel creates a new tag filter model
func NewTagFilterModel() *TagFilterModel {
	return &TagFilterModel{
		tagManager:    models.NewTagManager(),
		selectedTags:  []string{},
		tagCategories: models.GetTagCategories(),
		filterMode:    "category",
		showCounts:    true,
		maxTagsPerRow: 6,
		styles:        NewStyles(),
	}
}

// Init initializes the tag filter model
func (m *TagFilterModel) Init() tea.Cmd {
	return nil
}

// Update handles messages for the tag filter model
func (m *TagFilterModel) Update(msg tea.Msg) (*TagFilterModel, tea.Cmd) {
	switch msg := msg.(type) {
	case ConversationsLoadedMsg:
		m.tagManager.UpdateFromConversations(msg.Conversations)
		m.allTags = m.tagManager.GetAllTags()
		
	case tea.KeyMsg:
		if !m.focused || !m.visible {
			return m, nil
		}
		
		if m.searchActive {
			return m.handleSearchInput(msg)
		}
		
		switch msg.String() {
		case "tab":
			// Switch between category and tag mode
			if m.filterMode == "category" {
				m.filterMode = "tags"
				m.currentTag = 0
			} else {
				m.filterMode = "category"
				m.currentCategory = 0
			}
			
		case "left", "h":
			if m.filterMode == "category" {
				if m.currentCategory > 0 {
					m.currentCategory--
				}
			} else {
				if m.currentTag > 0 {
					m.currentTag--
				}
			}
			
		case "right", "l":
			if m.filterMode == "category" {
				if m.currentCategory < len(m.tagCategories)-1 {
					m.currentCategory++
				}
			} else {
				if m.currentTag < len(m.allTags)-1 {
					m.currentTag++
				}
			}
			
		case "up", "k":
			if m.filterMode == "tags" {
				newPos := m.currentTag - m.maxTagsPerRow
				if newPos >= 0 {
					m.currentTag = newPos
				}
			}
			
		case "down", "j":
			if m.filterMode == "tags" {
				newPos := m.currentTag + m.maxTagsPerRow
				if newPos < len(m.allTags) {
					m.currentTag = newPos
				}
			}
			
		case " ", "enter":
			if m.filterMode == "category" {
				// Toggle all tags in category
				return m, m.toggleCategoryTags()
			} else {
				// Toggle individual tag
				return m, m.toggleTag()
			}
			
		case "/":
			m.searchActive = true
			m.searchQuery = ""
			
		case "c":
			// Clear all filters
			m.selectedTags = []string{}
			return m, func() tea.Msg {
				return TagFilterChangedMsg{SelectedTags: m.selectedTags}
			}
			
		case "escape":
			if m.searchActive {
				m.searchActive = false
				m.searchQuery = ""
				m.allTags = m.tagManager.GetAllTags()
			}
		}
	}
	
	return m, nil
}

// handleSearchInput handles search input when search is active
func (m *TagFilterModel) handleSearchInput(msg tea.KeyMsg) (*TagFilterModel, tea.Cmd) {
	switch msg.String() {
	case "escape":
		m.searchActive = false
		m.searchQuery = ""
		m.allTags = m.tagManager.GetAllTags()
		
	case "enter":
		m.searchActive = false
		if len(m.allTags) > 0 {
			m.currentTag = 0
		}
		
	case "backspace":
		if len(m.searchQuery) > 0 {
			m.searchQuery = m.searchQuery[:len(m.searchQuery)-1]
			m.updateSearchResults()
		}
		
	case "ctrl+u":
		m.searchQuery = ""
		m.updateSearchResults()
		
	default:
		// Handle printable characters
		if len(msg.String()) == 1 && msg.String() >= " " && msg.String() <= "~" {
			m.searchQuery += msg.String()
			m.updateSearchResults()
		}
	}
	
	return m, nil
}

// updateSearchResults updates the tag list based on search query
func (m *TagFilterModel) updateSearchResults() {
	if m.searchQuery == "" {
		m.allTags = m.tagManager.GetAllTags()
	} else {
		m.allTags = m.tagManager.SearchTags(m.searchQuery)
	}
	
	// Reset cursor if out of bounds
	if m.currentTag >= len(m.allTags) {
		m.currentTag = 0
	}
}

// toggleCategoryTags toggles all tags in the current category
func (m *TagFilterModel) toggleCategoryTags() tea.Cmd {
	if m.currentCategory >= len(m.tagCategories) {
		return nil
	}
	
	category := m.tagCategories[m.currentCategory]
	tagsByCategory := m.tagManager.GetTagsByCategory()
	categoryTags := tagsByCategory[category.Name]
	
	// Check if any tags in this category are selected
	hasSelected := false
	for _, tag := range categoryTags {
		if m.isTagSelected(tag) {
			hasSelected = true
			break
		}
	}
	
	if hasSelected {
		// Remove all tags in this category
		for _, tag := range categoryTags {
			m.removeTag(tag)
		}
	} else {
		// Add all tags in this category
		for _, tag := range categoryTags {
			if !m.isTagSelected(tag) {
				m.selectedTags = append(m.selectedTags, tag)
			}
		}
	}
	
	return func() tea.Msg {
		return TagFilterChangedMsg{SelectedTags: m.selectedTags}
	}
}

// toggleTag toggles the current tag
func (m *TagFilterModel) toggleTag() tea.Cmd {
	if m.currentTag >= len(m.allTags) {
		return nil
	}
	
	tag := m.allTags[m.currentTag]
	
	if m.isTagSelected(tag) {
		m.removeTag(tag)
	} else {
		m.selectedTags = append(m.selectedTags, tag)
	}
	
	return func() tea.Msg {
		return TagFilterChangedMsg{SelectedTags: m.selectedTags}
	}
}

// isTagSelected checks if a tag is currently selected
func (m *TagFilterModel) isTagSelected(tag string) bool {
	for _, selected := range m.selectedTags {
		if selected == tag {
			return true
		}
	}
	return false
}

// removeTag removes a tag from the selected list
func (m *TagFilterModel) removeTag(tag string) {
	for i, selected := range m.selectedTags {
		if selected == tag {
			m.selectedTags = append(m.selectedTags[:i], m.selectedTags[i+1:]...)
			break
		}
	}
}

// View renders the tag filter model
func (m *TagFilterModel) View() string {
	if !m.visible {
		return ""
	}
	
	var sections []string
	
	// Header
	header := m.styles.FilterHeader.Render("🏷️  Tag Filters")
	sections = append(sections, header)
	
	// Search bar
	if m.searchActive {
		searchBar := m.renderSearchBar()
		sections = append(sections, searchBar)
	}
	
	// Mode indicator
	modeIndicator := m.renderModeIndicator()
	sections = append(sections, modeIndicator)
	
	// Content based on current mode
	if m.filterMode == "category" {
		content := m.renderCategoryView()
		sections = append(sections, content)
	} else {
		content := m.renderTagView()
		sections = append(sections, content)
	}
	
	// Selected tags summary
	if len(m.selectedTags) > 0 {
		summary := m.renderSelectedTagsSummary()
		sections = append(sections, summary)
	}
	
	// Help text
	help := m.renderHelp()
	sections = append(sections, help)
	
	return lipgloss.JoinVertical(lipgloss.Left, sections...)
}

// renderSearchBar renders the search input bar
func (m *TagFilterModel) renderSearchBar() string {
	prompt := "🔍 Search tags: "
	cursor := "█"
	
	searchText := m.searchQuery
	if m.searchActive {
		searchText += cursor
	}
	
	return m.styles.SearchBox.Render(prompt + searchText)
}

// renderModeIndicator renders the current mode indicator
func (m *TagFilterModel) renderModeIndicator() string {
	categoryStyle := m.styles.FilterButton
	tagStyle := m.styles.FilterButton
	
	if m.filterMode == "category" {
		categoryStyle = m.styles.FilterButtonActive
	} else {
		tagStyle = m.styles.FilterButtonActive
	}
	
	categoryBtn := categoryStyle.Render("Categories")
	tagBtn := tagStyle.Render("Tags")
	
	return lipgloss.JoinHorizontal(lipgloss.Left, categoryBtn, " ", tagBtn)
}

// renderCategoryView renders the category selection view
func (m *TagFilterModel) renderCategoryView() string {
	var items []string
	
	tagsByCategory := m.tagManager.GetTagsByCategory()
	
	for i, category := range m.tagCategories {
		categoryTags := tagsByCategory[category.Name]
		selectedCount := 0
		
		for _, tag := range categoryTags {
			if m.isTagSelected(tag) {
				selectedCount++
			}
		}
		
		style := m.styles.FilterButton
		if i == m.currentCategory && m.focused {
			style = m.styles.FilterButtonActive
		} else if selectedCount > 0 {
			style = m.styles.FilterButtonSelected
		}
		
		label := fmt.Sprintf("%s %s", category.Icon, category.Name)
		if m.showCounts {
			label += fmt.Sprintf(" (%d)", len(categoryTags))
		}
		if selectedCount > 0 {
			label += fmt.Sprintf(" [%d selected]", selectedCount)
		}
		
		items = append(items, style.Render(label))
	}
	
	// Arrange in rows
	var rows []string
	itemsPerRow := 3
	
	for i := 0; i < len(items); i += itemsPerRow {
		end := i + itemsPerRow
		if end > len(items) {
			end = len(items)
		}
		
		row := lipgloss.JoinHorizontal(lipgloss.Left, items[i:end]...)
		rows = append(rows, row)
	}
	
	return lipgloss.JoinVertical(lipgloss.Left, rows...)
}

// renderTagView renders the individual tag selection view
func (m *TagFilterModel) renderTagView() string {
	if len(m.allTags) == 0 {
		return m.styles.FilterButton.Render("No tags found")
	}
	
	var items []string
	
	for i, tag := range m.allTags {
		style := m.styles.FilterButton
		if i == m.currentTag && m.focused {
			style = m.styles.FilterButtonActive
		} else if m.isTagSelected(tag) {
			style = m.styles.FilterButtonSelected
		}
		
		label := fmt.Sprintf("%s %s", models.GetTagIcon(tag), tag)
		if m.showCounts {
			count := m.tagManager.GetTagCount(tag)
			label += fmt.Sprintf(" (%d)", count)
		}
		
		items = append(items, style.Render(label))
	}
	
	// Arrange in grid
	var rows []string
	
	for i := 0; i < len(items); i += m.maxTagsPerRow {
		end := i + m.maxTagsPerRow
		if end > len(items) {
			end = len(items)
		}
		
		row := lipgloss.JoinHorizontal(lipgloss.Left, items[i:end]...)
		rows = append(rows, row)
	}
	
	return lipgloss.JoinVertical(lipgloss.Left, rows...)
}

// renderSelectedTagsSummary renders a summary of selected tags
func (m *TagFilterModel) renderSelectedTagsSummary() string {
	if len(m.selectedTags) == 0 {
		return ""
	}
	
	var tagItems []string
	for _, tag := range m.selectedTags {
		icon := models.GetTagIcon(tag)
		item := m.styles.FilterButtonSelected.Render(fmt.Sprintf("%s %s", icon, tag))
		tagItems = append(tagItems, item)
	}
	
	header := m.styles.FilterHeader.Render(fmt.Sprintf("Selected Tags (%d):", len(m.selectedTags)))
	tags := lipgloss.JoinHorizontal(lipgloss.Left, tagItems...)
	
	return lipgloss.JoinVertical(lipgloss.Left, header, tags)
}

// renderHelp renders help text
func (m *TagFilterModel) renderHelp() string {
	var helpItems []string
	
	if m.searchActive {
		helpItems = append(helpItems, "Type to search • Enter: Apply • Esc: Cancel")
	} else {
		if m.filterMode == "category" {
			helpItems = append(helpItems, "←/→: Navigate • Space: Toggle category • Tab: Switch to tags")
		} else {
			helpItems = append(helpItems, "←/→/↑/↓: Navigate • Space: Toggle tag • Tab: Switch to categories")
		}
		helpItems = append(helpItems, "/: Search • c: Clear all • Esc: Close")
	}
	
	return m.styles.Help.Render(strings.Join(helpItems, " • "))
}

// SetFocused sets the focus state
func (m *TagFilterModel) SetFocused(focused bool) {
	m.focused = focused
}

// SetVisible sets the visibility state
func (m *TagFilterModel) SetVisible(visible bool) {
	m.visible = visible
}

// SetSize sets the model dimensions
func (m *TagFilterModel) SetSize(width, height int) {
	m.width = width
	m.height = height
}

// GetSelectedTags returns the currently selected tags
func (m *TagFilterModel) GetSelectedTags() []string {
	return m.selectedTags
}

// SetSelectedTags sets the selected tags
func (m *TagFilterModel) SetSelectedTags(tags []string) {
	m.selectedTags = tags
}

