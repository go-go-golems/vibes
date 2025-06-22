package ui

import (
	"fmt"
	"path/filepath"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/conversation-manager/pkg/data"
	"github.com/conversation-manager/pkg/models"
)

// MainModel represents the root model that coordinates all submodels
type MainModel struct {
	// Submodels
	conversationList *ConversationListModel
	search           *SearchModel
	filterRow        *FilterRowModel
	preview          *PreviewModel
	status           *StatusModel

	// Data manager
	dataManager *data.Manager

	// State
	currentFocus models.Focus
	currentMode  models.ViewMode
	width        int
	height       int
	styles       *Styles

	// Layout
	previewVisible bool
	filterVisible  bool
}

// NewMainModel creates a new main model
func NewMainModel() MainModel {
	styles := NewStyles()
	
	// Initialize data manager
	dataDir := filepath.Join(".", "data", "conversations")
	dataManager := data.NewManager(dataDir)

	return MainModel{
		conversationList: NewConversationListModel(styles),
		search:           NewSearchModel(styles),
		filterRow:        NewFilterRowModel(styles),
		preview:          NewPreviewModel(styles),
		status:           NewStatusModel(styles),
		dataManager:      dataManager,
		currentFocus:     models.FocusBrowse,
		currentMode:      models.ViewModeBrowse,
		styles:           styles,
		previewVisible:   false,
		filterVisible:    false,
	}
}

// Init initializes the main model
func (m MainModel) Init() tea.Cmd {
	return tea.Batch(
		m.conversationList.Init(),
		m.search.Init(),
		m.filterRow.Init(),
		m.preview.Init(),
		m.status.Init(),
		m.loadConversations(),
	)
}

// loadConversations loads conversations from the data manager
func (m MainModel) loadConversations() tea.Cmd {
	return func() tea.Msg {
		if err := m.dataManager.LoadConversations(); err != nil {
			return StatusUpdateMsg{Message: fmt.Sprintf("Error loading conversations: %v", err)}
		}
		
		conversations := m.dataManager.GetConversations()
		return ConversationsLoadedMsg{Conversations: conversations}
	}
}

// Update handles messages for the main model
func (m MainModel) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	var cmds []tea.Cmd

	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		m.width = msg.Width
		m.height = msg.Height
		m.updateLayout()

	case tea.KeyMsg:
		if m.currentFocus == models.FocusSearch {
			// Let search model handle input when focused
			var searchCmd tea.Cmd
			m.search, searchCmd = m.search.Update(msg)
			if searchCmd != nil {
				cmds = append(cmds, searchCmd)
			}
		} else if m.currentFocus == models.FocusFilter {
			// Let filter model handle input when focused
			var filterCmd tea.Cmd
			m.filterRow, filterCmd = m.filterRow.Update(msg)
			if filterCmd != nil {
				cmds = append(cmds, filterCmd)
			}
		} else {
			// Handle global key bindings
			switch msg.String() {
			case "/":
				// Switch to search mode
				m.currentFocus = models.FocusSearch
				m.currentMode = models.ViewModeSearch
				m.search.SetFocused(true)
				m.conversationList.SetFocused(false)
				m.filterRow.SetFocused(false)
				cmds = append(cmds, func() tea.Msg {
					return FocusChangedMsg{Focus: m.currentFocus}
				})

			case "f":
				// Toggle filter visibility
				m.filterVisible = !m.filterVisible
				m.filterRow.SetVisible(m.filterVisible)
				if m.filterVisible {
					m.currentFocus = models.FocusFilter
					m.currentMode = models.ViewModeFilter
					m.filterRow.SetFocused(true)
					m.conversationList.SetFocused(false)
					m.search.SetFocused(false)
				} else {
					m.currentFocus = models.FocusBrowse
					m.currentMode = models.ViewModeBrowse
					m.conversationList.SetFocused(true)
					m.filterRow.SetFocused(false)
				}
				cmds = append(cmds, func() tea.Msg {
					return FocusChangedMsg{Focus: m.currentFocus}
				})

			case " ":
				// Toggle preview
				m.previewVisible = !m.previewVisible
				m.preview.SetVisible(m.previewVisible)
				m.updateLayout()

			case "escape":
				// Return to browse mode
				m.currentFocus = models.FocusBrowse
				m.currentMode = models.ViewModeBrowse
				m.conversationList.SetFocused(true)
				m.search.SetFocused(false)
				m.filterRow.SetFocused(false)
				m.filterVisible = false
				m.filterRow.SetVisible(false)
				cmds = append(cmds, func() tea.Msg {
					return FocusChangedMsg{Focus: m.currentFocus}
				})

			case "q", "ctrl+c":
				return m, tea.Quit

			default:
				// Let conversation list handle other keys when in browse mode
				if m.currentFocus == models.FocusBrowse {
					var listCmd tea.Cmd
					m.conversationList, listCmd = m.conversationList.Update(msg)
					if listCmd != nil {
						cmds = append(cmds, listCmd)
					}
				}
			}
		}

	case SearchQueryChangedMsg:
		// Perform search
		results := m.dataManager.SearchConversations(msg.Query)
		cmds = append(cmds, func() tea.Msg {
			return SearchResultsMsg{Results: results}
		})

	case FilterChangedMsg:
		// Apply filters
		cmds = append(cmds, func() tea.Msg {
			return FilterAppliedMsg{Options: msg.Options}
		})

	case ConversationSelectedMsg:
		// Load conversation for preview
		if conv, found := m.dataManager.GetConversation(msg.ConversationID); found {
			m.preview.SetConversation(conv)
			if !m.previewVisible {
				m.previewVisible = true
				m.preview.SetVisible(true)
				m.updateLayout()
			}
		}
	}

	// Update all submodels
	var cmd tea.Cmd

	m.conversationList, cmd = m.conversationList.Update(msg)
	if cmd != nil {
		cmds = append(cmds, cmd)
	}

	m.search, cmd = m.search.Update(msg)
	if cmd != nil {
		cmds = append(cmds, cmd)
	}

	m.filterRow, cmd = m.filterRow.Update(msg)
	if cmd != nil {
		cmds = append(cmds, cmd)
	}

	m.preview, cmd = m.preview.Update(msg)
	if cmd != nil {
		cmds = append(cmds, cmd)
	}

	m.status, cmd = m.status.Update(msg)
	if cmd != nil {
		cmds = append(cmds, cmd)
	}

	return m, tea.Batch(cmds...)
}

// updateLayout updates the layout of submodels based on current state
func (m *MainModel) updateLayout() {
	// Calculate available space
	availableHeight := m.height - 3 // Reserve space for status bar

	if m.filterVisible {
		availableHeight -= 4 // Reserve space for filter row
	}

	if m.currentFocus == models.FocusSearch {
		availableHeight -= 3 // Reserve space for search input
	}

	// Set sizes for submodels
	if m.previewVisible {
		// Split view: conversation list on left, preview on right
		listWidth := m.width / 2
		previewWidth := m.width - listWidth

		m.conversationList.SetSize(listWidth, availableHeight)
		m.preview.SetSize(previewWidth, availableHeight)
	} else {
		// Full width for conversation list
		m.conversationList.SetSize(m.width, availableHeight)
	}

	m.search.SetSize(m.width, 3)
	m.filterRow.SetSize(m.width, 4)
	m.status.SetSize(m.width, 1)
}

// View renders the main model
func (m MainModel) View() string {
	var sections []string

	// Search input (if in search mode)
	if m.currentFocus == models.FocusSearch {
		sections = append(sections, m.search.View())
	}

	// Filter row (if visible)
	if m.filterVisible {
		sections = append(sections, m.filterRow.View())
	}

	// Main content area
	if m.previewVisible {
		// Split view
		leftPanel := m.conversationList.View()
		rightPanel := m.preview.View()
		
		mainContent := lipgloss.JoinHorizontal(
			lipgloss.Top,
			leftPanel,
			rightPanel,
		)
		sections = append(sections, mainContent)
	} else {
		// Single panel
		sections = append(sections, m.conversationList.View())
	}

	// Status bar
	sections = append(sections, m.status.View())

	return lipgloss.JoinVertical(lipgloss.Left, sections...)
}

