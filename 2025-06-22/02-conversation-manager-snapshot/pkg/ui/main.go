package ui

import (
	"fmt"
	"path/filepath"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/conversation-manager/pkg/data"
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
	currentFocus FocusState
	currentMode  ViewMode
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
		currentFocus:     FocusBrowse,
		currentMode:      ViewModeBrowse,
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

// Update handles messages for the main model
func (m MainModel) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	var cmds []tea.Cmd

	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		m.width = msg.Width
		m.height = msg.Height
		m.updateLayout()

	case tea.KeyMsg:
		// Handle global keys first
		switch msg.String() {
		case "q", "ctrl+c":
			return m, tea.Quit

		case "/":
			// Switch to search mode - don't pass the "/" key to search model
			m.currentFocus = FocusSearch
			m.currentMode = ViewModeSearch
			m.search.SetFocused(true)
			m.conversationList.SetFocused(false)
			m.filterRow.SetFocused(false)
			m.preview.SetFocused(false)
			
			cmds = append(cmds, func() tea.Msg {
				return FocusChangedMsg{Focus: FocusSearch}
			})
			// Don't pass this key to submodels - return early
			return m, tea.Batch(cmds...)

		case "f":
			// Toggle filter visibility
			m.filterVisible = !m.filterVisible
			m.filterRow.SetVisible(m.filterVisible)
			if m.filterVisible {
				m.currentFocus = FocusFilter
				m.filterRow.SetFocused(true)
				m.conversationList.SetFocused(false)
				m.search.SetFocused(false)
				m.preview.SetFocused(false)
			} else {
				m.currentFocus = FocusBrowse
				m.conversationList.SetFocused(true)
				m.filterRow.SetFocused(false)
			}
			m.updateLayout()

		case " ":
			// Toggle preview visibility
			if m.currentFocus != FocusSearch {
				m.previewVisible = !m.previewVisible
				m.preview.SetVisible(m.previewVisible)
				m.updateLayout()
				
				// If opening preview, load selected conversation
				if m.previewVisible {
					selectedConv := m.conversationList.GetSelectedConversation()
					if selectedConv != nil {
						cmds = append(cmds, m.loadConversationDetail(selectedConv.ID))
					}
				}
			}

		case "esc":
			// Return to browse mode
			m.currentFocus = FocusBrowse
			m.currentMode = ViewModeBrowse
			m.conversationList.SetFocused(true)
			m.search.SetFocused(false)
			m.filterRow.SetFocused(false)
			m.preview.SetFocused(false)
			
			// Clear search if active
			if m.search.GetQuery() != "" {
				m.search.ClearSearch()
				cmds = append(cmds, m.loadConversations())
			}
			
			cmds = append(cmds, func() tea.Msg {
				return FocusChangedMsg{Focus: FocusBrowse}
			})
		}

	case ConversationsLoadedMsg:
		// Update available filters
		tags := m.dataManager.GetAvailableTags()
		models := m.dataManager.GetAvailableModels()
		m.filterRow.SetAvailableFilters(tags, models)

	case SearchQueryChangedMsg:
		// Perform search
		results := m.dataManager.SearchConversations(msg.Query)
		cmds = append(cmds, func() tea.Msg {
			return SearchResultsMsg{Results: results, Query: msg.Query}
		})

	case FilterAppliedMsg:
		// Apply filters
		filtered := m.dataManager.FilterConversations(msg.Options)
		cmds = append(cmds, func() tea.Msg {
			return FilterAppliedMsg{Options: msg.Options, Results: filtered}
		})

	case PreviewRequestMsg:
		// Load conversation detail for preview
		m.previewVisible = true
		m.preview.SetVisible(true)
		m.updateLayout()
		cmds = append(cmds, m.loadConversationDetail(msg.ConversationID))

	case ConversationSelectedMsg:
		// Handle conversation selection (could open in external app)
		m.status.SetStatusMessage("Opening conversation: " + msg.ConversationID)

	case StatusUpdateMsg:
		// Status message updates are handled by the status model
	}

	// Update submodels
	var cmd tea.Cmd

	m.conversationList, cmd = m.conversationList.Update(msg)
	cmds = append(cmds, cmd)

	m.search, cmd = m.search.Update(msg)
	cmds = append(cmds, cmd)

	m.filterRow, cmd = m.filterRow.Update(msg)
	cmds = append(cmds, cmd)

	m.preview, cmd = m.preview.Update(msg)
	cmds = append(cmds, cmd)

	m.status, cmd = m.status.Update(msg)
	cmds = append(cmds, cmd)

	return m, tea.Batch(cmds...)
}

// View renders the main model
func (m MainModel) View() string {
	if m.width == 0 || m.height == 0 {
		return "Loading..."
	}

	var content []string

	// Header
	headerHeight := 1
	header := m.renderHeader()
	content = append(content, header)

	// Filter row (if visible)
	filterHeight := 0
	if m.filterVisible {
		filterHeight = 4 // Approximate height for filter row
		filterContent := m.filterRow.View()
		content = append(content, filterContent)
	}

	// Main content area
	mainHeight := m.height - headerHeight - filterHeight - 2 // Status takes 2 lines

	// Preview splits the main area
	if m.previewVisible {
		listHeight := mainHeight * 2 / 3
		previewHeight := mainHeight - listHeight

		// Conversation list or search results
		if m.currentMode == ViewModeSearch && m.search.GetQuery() != "" {
			m.search.SetSize(m.width, listHeight)
			searchContent := m.search.View()
			content = append(content, searchContent)
		} else {
			m.conversationList.SetSize(m.width, listHeight)
			listContent := m.conversationList.View()
			content = append(content, listContent)
		}

		// Preview panel
		m.preview.SetSize(m.width, previewHeight)
		previewContent := m.preview.View()
		if previewContent != "" {
			content = append(content, m.styles.BorderStyle.Render(""))
			content = append(content, previewContent)
		}
	} else {
		// Full height for main content
		if m.currentMode == ViewModeSearch && m.search.GetQuery() != "" {
			m.search.SetSize(m.width, mainHeight)
			searchContent := m.search.View()
			content = append(content, searchContent)
		} else {
			m.conversationList.SetSize(m.width, mainHeight)
			listContent := m.conversationList.View()
			content = append(content, listContent)
		}
	}

	// Status bar
	m.status.SetSize(m.width, 2)
	statusContent := m.status.View()
	content = append(content, m.styles.BorderStyle.Render(""))
	content = append(content, statusContent)

	return lipgloss.JoinVertical(lipgloss.Left, content...)
}

// renderHeader renders the application header
func (m MainModel) renderHeader() string {
	conversationCount := len(m.dataManager.GetConversationSummaries())
	
	title := "💬 Conversations"
	if conversationCount > 0 {
		title += m.styles.CountStyle.Render(" (" + fmt.Sprintf("%d", conversationCount) + ")")
	}

	return m.styles.HeaderStyle.Width(m.width).Render(title)
}

// updateLayout updates the layout of all submodels
func (m *MainModel) updateLayout() {
	// Update sizes for all submodels based on current layout
	m.conversationList.SetSize(m.width, m.height)
	m.search.SetSize(m.width, m.height)
	m.filterRow.SetSize(m.width, 4)
	m.preview.SetSize(m.width, m.height/3)
	m.status.SetSize(m.width, 2)
}

// loadConversations loads all conversations from the data manager
func (m MainModel) loadConversations() tea.Cmd {
	return func() tea.Msg {
		if err := m.dataManager.LoadConversations(); err != nil {
			return StatusUpdateMsg{Message: "Error loading conversations: " + err.Error()}
		}

		summaries := m.dataManager.GetConversationSummaries()
		return ConversationsLoadedMsg{Conversations: summaries}
	}
}

// loadConversationDetail loads the full conversation for preview
func (m MainModel) loadConversationDetail(conversationID string) tea.Cmd {
	return func() tea.Msg {
		conversation, found := m.dataManager.GetConversationByID(conversationID)
		if !found {
			return StatusUpdateMsg{Message: "Conversation not found: " + conversationID}
		}

		return ConversationDetailLoadedMsg{Conversation: *conversation}
	}
}

