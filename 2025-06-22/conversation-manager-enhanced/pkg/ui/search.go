package ui

import (
	"fmt"
	"strings"
	"time"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/conversation-manager/pkg/models"
)

// SearchMode represents different search modes
type SearchMode int

const (
	SearchModeGeneral SearchMode = iota
	SearchModeTags
	SearchModeContent
	SearchModeTitle
	SearchModeDateRange
)

// SearchModel handles search input and results with enhanced tag and date range support
type SearchModel struct {
	query         string
	cursor        int
	results       []models.SearchResult
	width         int
	height        int
	styles        *Styles
	focused       bool
	resultCursor  int
	
	// Enhanced search features
	searchMode    SearchMode
	tagManager    *models.TagManager
	dateParser    *models.DateRangeParser
	suggestions   []string
	showSuggestions bool
	suggestionCursor int
	
	// Search history
	searchHistory []string
	historyIndex  int
	
	// Tag search specific
	tagQuery      string
	tagResults    []string
	inTagMode     bool
	
	// Date range search specific
	dateQuery     string
	dateResults   []string
	inDateMode    bool
	activeDateRanges []models.DateRange
}

// NewSearchModel creates a new search model
func NewSearchModel(styles *Styles) *SearchModel {
	return &SearchModel{
		query:           "",
		cursor:          0,
		results:         []models.SearchResult{},
		styles:          styles,
		focused:         false,
		resultCursor:    0,
		searchMode:      SearchModeGeneral,
		tagManager:      models.NewTagManager(),
		dateParser:      models.NewDateRangeParser(),
		suggestions:     []string{},
		showSuggestions: false,
		suggestionCursor: 0,
		searchHistory:   []string{},
		historyIndex:    -1,
		tagQuery:        "",
		tagResults:      []string{},
		inTagMode:       false,
		dateQuery:       "",
		dateResults:     []string{},
		inDateMode:      false,
		activeDateRanges: []models.DateRange{},
	}
}

// Init initializes the search model
func (m *SearchModel) Init() tea.Cmd {
	return nil
}

// Update handles messages for the search model
func (m *SearchModel) Update(msg tea.Msg) (*SearchModel, tea.Cmd) {
	switch msg := msg.(type) {
	case SearchResultsMsg:
		m.results = msg.Results
		m.resultCursor = 0
		
	case ConversationsLoadedMsg:
		m.tagManager.UpdateFromConversations(msg.Conversations)
		
	case tea.KeyMsg:
		if !m.focused {
			return m, nil
		}
		
		// Handle suggestions navigation
		if m.showSuggestions {
			return m.handleSuggestionsInput(msg)
		}
		
		// Handle tag mode
		if m.inTagMode {
			return m.handleTagModeInput(msg)
		}
		
		// Handle date mode
		if m.inDateMode {
			return m.handleDateModeInput(msg)
		}
		
		switch msg.String() {
		case "ctrl+u":
			// Clear entire search
			m.query = ""
			m.cursor = 0
			m.clearSuggestions()
			m.activeDateRanges = []models.DateRange{}
			return m, func() tea.Msg {
				return SearchQueryChangedMsg{Query: m.query}
			}
			
		case "ctrl+w":
			// Clear last word
			words := strings.Fields(m.query[:m.cursor])
			if len(words) > 0 {
				words = words[:len(words)-1]
				newQuery := strings.Join(words, " ")
				if len(newQuery) > 0 {
					newQuery += " "
				}
				m.query = newQuery + m.query[m.cursor:]
				m.cursor = len(newQuery)
				m.updateSuggestions()
				m.updateActiveDateRanges()
				return m, func() tea.Msg {
					return SearchQueryChangedMsg{Query: m.query}
				}
			}
			
		case "backspace":
			if len(m.query) > 0 && m.cursor > 0 {
				m.query = m.query[:m.cursor-1] + m.query[m.cursor:]
				m.cursor--
				m.updateSuggestions()
				m.updateActiveDateRanges()
				return m, func() tea.Msg {
					return SearchQueryChangedMsg{Query: m.query}
				}
			}
			
		case "left":
			if m.cursor > 0 {
				m.cursor--
			}
			
		case "right":
			if m.cursor < len(m.query) {
				m.cursor++
			}
			
		case "home":
			m.cursor = 0
			
		case "end":
			m.cursor = len(m.query)
			
		case "up", "k":
			if len(m.results) > 0 && m.resultCursor > 0 {
				m.resultCursor--
			} else if len(m.searchHistory) > 0 {
				// Navigate search history
				if m.historyIndex < len(m.searchHistory)-1 {
					m.historyIndex++
					m.query = m.searchHistory[len(m.searchHistory)-1-m.historyIndex]
					m.cursor = len(m.query)
					m.updateActiveDateRanges()
					return m, func() tea.Msg {
						return SearchQueryChangedMsg{Query: m.query}
					}
				}
			}
			
		case "down", "j":
			if len(m.results) > 0 && m.resultCursor < len(m.results)-1 {
				m.resultCursor++
			} else if m.historyIndex >= 0 {
				// Navigate search history
				if m.historyIndex > 0 {
					m.historyIndex--
					m.query = m.searchHistory[len(m.searchHistory)-1-m.historyIndex]
				} else {
					m.historyIndex = -1
					m.query = ""
				}
				m.cursor = len(m.query)
				m.updateActiveDateRanges()
				return m, func() tea.Msg {
					return SearchQueryChangedMsg{Query: m.query}
				}
			}
			
		case "enter":
			if len(m.results) > 0 && m.resultCursor < len(m.results) {
				selectedResult := m.results[m.resultCursor]
				m.addToHistory(m.query)
				return m, func() tea.Msg {
					return ConversationSelectedMsg{ConversationID: selectedResult.Conversation.ID}
				}
			}
			
		case "tab":
			// Show/hide suggestions
			if len(m.suggestions) > 0 {
				m.showSuggestions = !m.showSuggestions
				m.suggestionCursor = 0
			}
			
		case "ctrl+t":
			// Toggle tag search mode
			m.inTagMode = !m.inTagMode
			if m.inTagMode {
				m.tagQuery = ""
				m.updateTagResults()
			}
			
		case "ctrl+d":
			// Toggle date range search mode
			m.inDateMode = !m.inDateMode
			if m.inDateMode {
				m.dateQuery = ""
				m.updateDateResults()
			}
			
		case "ctrl+m":
			// Cycle search modes
			m.searchMode = SearchMode((int(m.searchMode) + 1) % 5)
			return m, func() tea.Msg {
				return SearchQueryChangedMsg{Query: m.query}
			}
			
		default:
			// Handle printable characters
			if len(msg.String()) == 1 && msg.String() >= " " && msg.String() <= "~" {
				char := msg.String()
				m.query = m.query[:m.cursor] + char + m.query[m.cursor:]
				m.cursor++
				m.updateSuggestions()
				m.updateActiveDateRanges()
				return m, func() tea.Msg {
					return SearchQueryChangedMsg{Query: m.query}
				}
			}
		}
	}
	
	return m, nil
}

// handleDateModeInput handles input when in date range search mode
func (m *SearchModel) handleDateModeInput(msg tea.KeyMsg) (*SearchModel, tea.Cmd) {
	switch msg.String() {
	case "escape", "ctrl+d":
		m.inDateMode = false
		
	case "backspace":
		if len(m.dateQuery) > 0 {
			m.dateQuery = m.dateQuery[:len(m.dateQuery)-1]
			m.updateDateResults()
		}
		
	case "enter":
		if len(m.dateResults) > 0 {
			// Add selected date range to main query
			selectedRange := m.dateResults[0]
			if m.query != "" && !strings.HasSuffix(m.query, " ") {
				m.query += " "
			}
			m.query += "range:" + selectedRange
			m.cursor = len(m.query)
			m.inDateMode = false
			m.updateActiveDateRanges()
			
			return m, func() tea.Msg {
				return SearchQueryChangedMsg{Query: m.query}
			}
		}
		
	default:
		// Handle printable characters
		if len(msg.String()) == 1 && msg.String() >= " " && msg.String() <= "~" {
			m.dateQuery += msg.String()
			m.updateDateResults()
		}
	}
	
	return m, nil
}

// handleSuggestionsInput handles input when suggestions are shown
func (m *SearchModel) handleSuggestionsInput(msg tea.KeyMsg) (*SearchModel, tea.Cmd) {
	switch msg.String() {
	case "escape", "tab":
		m.clearSuggestions()
		
	case "up", "k":
		if m.suggestionCursor > 0 {
			m.suggestionCursor--
		}
		
	case "down", "j":
		if m.suggestionCursor < len(m.suggestions)-1 {
			m.suggestionCursor++
		}
		
	case "enter":
		if m.suggestionCursor < len(m.suggestions) {
			suggestion := m.suggestions[m.suggestionCursor]
			
			// Replace current word with suggestion
			words := strings.Fields(m.query[:m.cursor])
			if len(words) > 0 {
				words[len(words)-1] = suggestion
			} else {
				words = []string{suggestion}
			}
			
			newQuery := strings.Join(words, " ") + " " + m.query[m.cursor:]
			m.query = newQuery
			m.cursor = len(strings.Join(words, " ")) + 1
			m.clearSuggestions()
			m.updateActiveDateRanges()
			
			return m, func() tea.Msg {
				return SearchQueryChangedMsg{Query: m.query}
			}
		}
	}
	
	return m, nil
}

// handleTagModeInput handles input when in tag search mode
func (m *SearchModel) handleTagModeInput(msg tea.KeyMsg) (*SearchModel, tea.Cmd) {
	switch msg.String() {
	case "escape", "ctrl+t":
		m.inTagMode = false
		
	case "backspace":
		if len(m.tagQuery) > 0 {
			m.tagQuery = m.tagQuery[:len(m.tagQuery)-1]
			m.updateTagResults()
		}
		
	case "enter":
		if len(m.tagResults) > 0 {
			// Add selected tag to main query
			selectedTag := m.tagResults[0]
			if m.query != "" && !strings.HasSuffix(m.query, " ") {
				m.query += " "
			}
			m.query += "tag:" + selectedTag
			m.cursor = len(m.query)
			m.inTagMode = false
			
			return m, func() tea.Msg {
				return SearchQueryChangedMsg{Query: m.query}
			}
		}
		
	default:
		// Handle printable characters
		if len(msg.String()) == 1 && msg.String() >= " " && msg.String() <= "~" {
			m.tagQuery += msg.String()
			m.updateTagResults()
		}
	}
	
	return m, nil
}

// updateSuggestions updates search suggestions based on current query
func (m *SearchModel) updateSuggestions() {
	if len(m.query) < 2 {
		m.clearSuggestions()
		return
	}
	
	// Get current word being typed
	words := strings.Fields(m.query[:m.cursor])
	if len(words) == 0 {
		m.clearSuggestions()
		return
	}
	
	currentWord := words[len(words)-1]
	
	// Check if it's a tag search
	if strings.HasPrefix(currentWord, "tag:") {
		tagQuery := strings.TrimPrefix(currentWord, "tag:")
		m.suggestions = m.tagManager.SearchTags(tagQuery)
		// Prefix suggestions with "tag:"
		for i, suggestion := range m.suggestions {
			m.suggestions[i] = "tag:" + suggestion
		}
	} else if strings.HasPrefix(currentWord, "range:") || strings.HasPrefix(currentWord, "after:") || strings.HasPrefix(currentWord, "before:") || strings.HasPrefix(currentWord, "on:") {
		// Date range suggestions
		prefix := ""
		query := currentWord
		if strings.Contains(currentWord, ":") {
			parts := strings.SplitN(currentWord, ":", 2)
			prefix = parts[0] + ":"
			query = parts[1]
		}
		
		m.suggestions = m.getDateRangeSuggestions(query)
		// Prefix suggestions with the date range prefix
		for i, suggestion := range m.suggestions {
			m.suggestions[i] = prefix + suggestion
		}
	} else {
		// General suggestions (could include popular tags, recent searches, etc.)
		m.suggestions = m.getGeneralSuggestions(currentWord)
	}
	
	if len(m.suggestions) > 0 {
		m.showSuggestions = true
		m.suggestionCursor = 0
	} else {
		m.clearSuggestions()
	}
}

// updateTagResults updates tag search results
func (m *SearchModel) updateTagResults() {
	m.tagResults = m.tagManager.SearchTags(m.tagQuery)
	if len(m.tagResults) > 5 {
		m.tagResults = m.tagResults[:5] // Limit to 5 results
	}
}

// updateDateResults updates date range search results
func (m *SearchModel) updateDateResults() {
	m.dateResults = m.getDateRangeSuggestions(m.dateQuery)
	if len(m.dateResults) > 5 {
		m.dateResults = m.dateResults[:5] // Limit to 5 results
	}
}

// updateActiveDateRanges updates the list of active date ranges from the query
func (m *SearchModel) updateActiveDateRanges() {
	m.activeDateRanges = []models.DateRange{}
	
	// Parse date range terms from the query
	dateTerms := models.ParseDateRangeSearchTerms(m.query)
	for _, term := range dateTerms {
		if term.Range != nil {
			m.activeDateRanges = append(m.activeDateRanges, *term.Range)
		}
	}
}

// getDateRangeSuggestions returns date range suggestions
func (m *SearchModel) getDateRangeSuggestions(query string) []string {
	predefined := models.GetPredefinedDateRanges()
	
	if query == "" {
		return predefined
	}
	
	var matches []string
	query = strings.ToLower(query)
	
	// Filter predefined ranges
	for _, range_ := range predefined {
		if strings.Contains(strings.ToLower(range_), query) {
			matches = append(matches, range_)
		}
	}
	
	// Add some common patterns
	commonPatterns := []string{
		"2024-01-01",
		"2024-06-01",
		"last-7-days",
		"last-30-days",
		"this-month",
		"last-month",
	}
	
	for _, pattern := range commonPatterns {
		if strings.Contains(strings.ToLower(pattern), query) {
			found := false
			for _, existing := range matches {
				if existing == pattern {
					found = true
					break
				}
			}
			if !found {
				matches = append(matches, pattern)
			}
		}
	}
	
	return matches
}

// getGeneralSuggestions returns general search suggestions
func (m *SearchModel) getGeneralSuggestions(query string) []string {
	var suggestions []string
	
	// Add popular tags as suggestions
	popularTags := m.tagManager.GetPopularTags(5)
	for _, tag := range popularTags {
		if strings.Contains(strings.ToLower(tag), strings.ToLower(query)) {
			suggestions = append(suggestions, "tag:"+tag)
		}
	}
	
	// Add search history matches
	for _, historyItem := range m.searchHistory {
		if strings.Contains(strings.ToLower(historyItem), strings.ToLower(query)) {
			suggestions = append(suggestions, historyItem)
		}
	}
	
	// Add common search prefixes
	prefixes := []string{"title:", "content:", "model:", "tag:", "range:", "after:", "before:", "on:"}
	for _, prefix := range prefixes {
		if strings.HasPrefix(prefix, strings.ToLower(query)) {
			suggestions = append(suggestions, prefix)
		}
	}
	
	// Add date range suggestions
	if strings.Contains(strings.ToLower(query), "date") || strings.Contains(strings.ToLower(query), "time") {
		dateRanges := []string{"range:today", "range:yesterday", "range:this-week", "range:last-30-days"}
		suggestions = append(suggestions, dateRanges...)
	}
	
	return suggestions
}

// clearSuggestions clears the suggestions
func (m *SearchModel) clearSuggestions() {
	m.showSuggestions = false
	m.suggestions = []string{}
	m.suggestionCursor = 0
}

// addToHistory adds a query to search history
func (m *SearchModel) addToHistory(query string) {
	if query == "" {
		return
	}
	
	// Remove if already exists
	for i, item := range m.searchHistory {
		if item == query {
			m.searchHistory = append(m.searchHistory[:i], m.searchHistory[i+1:]...)
			break
		}
	}
	
	// Add to front
	m.searchHistory = append([]string{query}, m.searchHistory...)
	
	// Limit history size
	if len(m.searchHistory) > 20 {
		m.searchHistory = m.searchHistory[:20]
	}
	
	m.historyIndex = -1
}

// View renders the search model
func (m *SearchModel) View() string {
	if !m.focused {
		return ""
	}
	
	var sections []string
	
	// Search input with mode indicator
	searchInput := m.renderSearchInput()
	sections = append(sections, searchInput)
	
	// Date mode overlay
	if m.inDateMode {
		dateMode := m.renderDateMode()
		sections = append(sections, dateMode)
		return lipgloss.JoinVertical(lipgloss.Left, sections...)
	}
	
	// Tag mode overlay
	if m.inTagMode {
		tagMode := m.renderTagMode()
		sections = append(sections, tagMode)
		return lipgloss.JoinVertical(lipgloss.Left, sections...)
	}
	
	// Active date ranges
	if len(m.activeDateRanges) > 0 {
		dateRanges := m.renderActiveDateRanges()
		sections = append(sections, dateRanges)
	}
	
	// Suggestions
	if m.showSuggestions && len(m.suggestions) > 0 {
		suggestions := m.renderSuggestions()
		sections = append(sections, suggestions)
	}
	
	// Search results
	if len(m.results) > 0 {
		results := m.renderResults()
		sections = append(sections, results)
	} else if m.query != "" {
		noResults := m.styles.SearchBox.Render("No results found")
		sections = append(sections, noResults)
	}
	
	return lipgloss.JoinVertical(lipgloss.Left, sections...)
}

// renderSearchInput renders the search input field
func (m *SearchModel) renderSearchInput() string {
	prompt := "🔍 Search"
	
	// Add mode indicator
	switch m.searchMode {
	case SearchModeTags:
		prompt += " (tags)"
	case SearchModeContent:
		prompt += " (content)"
	case SearchModeTitle:
		prompt += " (title)"
	case SearchModeDateRange:
		prompt += " (dates)"
	}
	
	prompt += ": "
	
	// Render query with cursor
	displayQuery := m.query
	if m.focused {
		if m.cursor <= len(displayQuery) {
			displayQuery = displayQuery[:m.cursor] + "█" + displayQuery[m.cursor:]
		} else {
			displayQuery += "█"
		}
	}
	
	return m.styles.SearchBox.Render(prompt + displayQuery)
}

// renderDateMode renders the date range search overlay
func (m *SearchModel) renderDateMode() string {
	var sections []string
	
	// Date search input
	dateInput := m.styles.SearchBox.Render("📅 Date range: " + m.dateQuery + "█")
	sections = append(sections, dateInput)
	
	// Date results
	if len(m.dateResults) > 0 {
		var dateItems []string
		for i, dateRange := range m.dateResults {
			style := m.styles.FilterButton
			if i == 0 { // First item is selected
				style = m.styles.FilterButtonActive
			}
			
			label := fmt.Sprintf("📅 %s", dateRange)
			dateItems = append(dateItems, style.Render(label))
		}
		
		dateResults := lipgloss.JoinHorizontal(lipgloss.Left, dateItems...)
		sections = append(sections, dateResults)
	}
	
	// Help
	help := m.styles.Help.Render("Type date range • Enter: Select • Esc: Cancel")
	sections = append(sections, help)
	
	return lipgloss.JoinVertical(lipgloss.Left, sections...)
}

// renderTagMode renders the tag search overlay
func (m *SearchModel) renderTagMode() string {
	var sections []string
	
	// Tag search input
	tagInput := m.styles.SearchBox.Render("🏷️ Tag search: " + m.tagQuery + "█")
	sections = append(sections, tagInput)
	
	// Tag results
	if len(m.tagResults) > 0 {
		var tagItems []string
		for i, tag := range m.tagResults {
			style := m.styles.FilterButton
			if i == 0 { // First item is selected
				style = m.styles.FilterButtonActive
			}
			
			icon := models.GetTagIcon(tag)
			count := m.tagManager.GetTagCount(tag)
			label := fmt.Sprintf("%s %s (%d)", icon, tag, count)
			
			tagItems = append(tagItems, style.Render(label))
		}
		
		tagResults := lipgloss.JoinHorizontal(lipgloss.Left, tagItems...)
		sections = append(sections, tagResults)
	}
	
	// Help
	help := m.styles.Help.Render("Type to search tags • Enter: Select • Esc: Cancel")
	sections = append(sections, help)
	
	return lipgloss.JoinVertical(lipgloss.Left, sections...)
}

// renderActiveDateRanges renders active date range filters
func (m *SearchModel) renderActiveDateRanges() string {
	if len(m.activeDateRanges) == 0 {
		return ""
	}
	
	var rangeItems []string
	for _, dr := range m.activeDateRanges {
		item := m.styles.FilterButtonSelected.Render(fmt.Sprintf("📅 %s", dr.String()))
		rangeItems = append(rangeItems, item)
	}
	
	header := m.styles.FilterHeader.Render("Active Date Filters:")
	ranges := lipgloss.JoinHorizontal(lipgloss.Left, rangeItems...)
	
	return lipgloss.JoinVertical(lipgloss.Left, header, ranges)
}

// renderSuggestions renders search suggestions
func (m *SearchModel) renderSuggestions() string {
	var suggestionItems []string
	
	for i, suggestion := range m.suggestions {
		style := m.styles.FilterButton
		if i == m.suggestionCursor {
			style = m.styles.FilterButtonActive
		}
		
		suggestionItems = append(suggestionItems, style.Render(suggestion))
	}
	
	suggestions := lipgloss.JoinHorizontal(lipgloss.Left, suggestionItems...)
	header := m.styles.Help.Render("Suggestions:")
	
	return lipgloss.JoinVertical(lipgloss.Left, header, suggestions)
}

// renderResults renders search results
func (m *SearchModel) renderResults() string {
	var resultItems []string
	
	// Results header
	header := fmt.Sprintf("%d results found", len(m.results))
	resultItems = append(resultItems, m.styles.Help.Render(header))
	
	// Result items
	for i, result := range m.results {
		style := m.styles.ConversationItem
		if i == m.resultCursor {
			style = m.styles.ConversationItemSelected
		}
		
		// Format result
		icon := models.GetTagIcon(result.Conversation.GetPrimaryTag())
		title := result.Conversation.Title
		
		// Add matched tags if any
		if len(result.MatchedTags) > 0 {
			title += " [" + strings.Join(result.MatchedTags, ", ") + "]"
		}
		
		line1 := fmt.Sprintf(" %s %s", icon, title)
		line2 := fmt.Sprintf("  %s", result.MatchedText)
		
		item := style.Render(line1 + "\n" + line2)
		resultItems = append(resultItems, item)
	}
	
	return lipgloss.JoinVertical(lipgloss.Left, resultItems...)
}

// formatDate formats a date for display
func (m *SearchModel) formatDate(t time.Time) string {
	now := time.Now()
	
	if t.Year() == now.Year() && t.Month() == now.Month() && t.Day() == now.Day() {
		return t.Format("3:04 PM")
	} else if t.Year() == now.Year() {
		return t.Format("Jan 2")
	} else {
		return t.Format("Jan 2, 2006")
	}
}

// SetFocused sets the focus state
func (m *SearchModel) SetFocused(focused bool) {
	m.focused = focused
	if !focused {
		m.clearSuggestions()
		m.inTagMode = false
		m.inDateMode = false
	}
}

// SetSize sets the model dimensions
func (m *SearchModel) SetSize(width, height int) {
	m.width = width
	m.height = height
}

// GetQuery returns the current search query
func (m *SearchModel) GetQuery() string {
	return m.query
}

// SetQuery sets the search query
func (m *SearchModel) SetQuery(query string) {
	m.query = query
	m.cursor = len(query)
	m.updateSuggestions()
	m.updateActiveDateRanges()
}

// GetSearchMode returns the current search mode
func (m *SearchModel) GetSearchMode() SearchMode {
	return m.searchMode
}

// SetSearchMode sets the search mode
func (m *SearchModel) SetSearchMode(mode SearchMode) {
	m.searchMode = mode
}

