package todo

import (
	"fmt"
	"strings"

	"github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

// EnhancedModel represents the enhanced Bubbletea model with rich colors
type EnhancedModel struct {
	TodoList    *TodoList
	Mode        AppMode
	InputText   string
	Width       int
	Height      int
	Styles      EnhancedStyles
}

// EnhancedStyles contains enhanced styling with ANSI colors
type EnhancedStyles struct {
	Title         lipgloss.Style
	Subtitle      lipgloss.Style
	TodoItem      lipgloss.Style
	Selected      lipgloss.Style
	Completed     lipgloss.Style
	Input         lipgloss.Style
	Help          lipgloss.Style
	StatusBar     lipgloss.Style
	Border        lipgloss.Style
	Checkbox      lipgloss.Style
	CheckboxDone  lipgloss.Style
	Prompt        lipgloss.Style
	Error         lipgloss.Style
}

// NewEnhancedModel creates a new enhanced Bubbletea model
func NewEnhancedModel() EnhancedModel {
	return EnhancedModel{
		TodoList:  NewTodoList(),
		Mode:      ModeList,
		InputText: "",
		Width:     80,
		Height:    24,
		Styles:    NewEnhancedStyles(),
	}
}

// NewEnhancedStyles creates enhanced styles with rich colors
func NewEnhancedStyles() EnhancedStyles {
	return EnhancedStyles{
		Title: lipgloss.NewStyle().
			Bold(true).
			Foreground(lipgloss.Color("#00D7FF")).
			Background(lipgloss.Color("#1E1E2E")).
			Padding(0, 2).
			MarginBottom(1).
			Align(lipgloss.Center),
		
		Subtitle: lipgloss.NewStyle().
			Foreground(lipgloss.Color("#A6E3A1")).
			Italic(true).
			MarginBottom(1).
			Align(lipgloss.Center),
		
		TodoItem: lipgloss.NewStyle().
			Foreground(lipgloss.Color("#CDD6F4")).
			PaddingLeft(2),
		
		Selected: lipgloss.NewStyle().
			Background(lipgloss.Color("#313244")).
			Foreground(lipgloss.Color("#F9E2AF")).
			Bold(true).
			PaddingLeft(2).
			PaddingRight(2),
		
		Completed: lipgloss.NewStyle().
			Foreground(lipgloss.Color("#6C7086")).
			Strikethrough(true).
			PaddingLeft(2),
		
		Input: lipgloss.NewStyle().
			Border(lipgloss.RoundedBorder()).
			BorderForeground(lipgloss.Color("#89B4FA")).
			Foreground(lipgloss.Color("#CDD6F4")).
			Background(lipgloss.Color("#1E1E2E")).
			Padding(0, 1).
			MarginTop(1).
			MarginBottom(1),
		
		Help: lipgloss.NewStyle().
			Foreground(lipgloss.Color("#7F849C")).
			Background(lipgloss.Color("#181825")).
			Padding(0, 1).
			MarginTop(1),
		
		StatusBar: lipgloss.NewStyle().
			Background(lipgloss.Color("#313244")).
			Foreground(lipgloss.Color("#F38BA8")).
			Bold(true).
			Padding(0, 1),
		
		Border: lipgloss.NewStyle().
			Border(lipgloss.RoundedBorder()).
			BorderForeground(lipgloss.Color("#45475A")).
			Padding(1),
		
		Checkbox: lipgloss.NewStyle().
			Foreground(lipgloss.Color("#F38BA8")).
			Bold(true),
		
		CheckboxDone: lipgloss.NewStyle().
			Foreground(lipgloss.Color("#A6E3A1")).
			Bold(true),
		
		Prompt: lipgloss.NewStyle().
			Foreground(lipgloss.Color("#FAB387")).
			Bold(true),
		
		Error: lipgloss.NewStyle().
			Foreground(lipgloss.Color("#F38BA8")).
			Background(lipgloss.Color("#1E1E2E")).
			Bold(true).
			Padding(0, 1),
	}
}

// Init initializes the enhanced model
func (m EnhancedModel) Init() tea.Cmd {
	return nil
}

// Update handles messages and updates the enhanced model
func (m EnhancedModel) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		m.Width = msg.Width
		m.Height = msg.Height
		return m, nil

	case tea.KeyMsg:
		switch m.Mode {
		case ModeList:
			return m.updateListMode(msg)
		case ModeAdd:
			return m.updateAddMode(msg)
		}
	}

	return m, nil
}

// updateListMode handles key presses in list mode
func (m EnhancedModel) updateListMode(msg tea.KeyMsg) (tea.Model, tea.Cmd) {
	switch msg.String() {
	case "ctrl+c", "q":
		return m, tea.Quit
	case "up", "k":
		m.TodoList.MoveSelectionUp()
	case "down", "j":
		m.TodoList.MoveSelectionDown()
	case "enter", " ":
		if len(m.TodoList.Items) > 0 {
			m.TodoList.ToggleItem(m.TodoList.GetSelectedIndex())
		}
	case "a":
		m.Mode = ModeAdd
		m.InputText = ""
	case "d", "x":
		if len(m.TodoList.Items) > 0 {
			m.TodoList.DeleteItem(m.TodoList.GetSelectedIndex())
		}
	}
	return m, nil
}

// updateAddMode handles key presses in add mode
func (m EnhancedModel) updateAddMode(msg tea.KeyMsg) (tea.Model, tea.Cmd) {
	switch msg.String() {
	case "ctrl+c":
		return m, tea.Quit
	case "esc":
		m.Mode = ModeList
		m.InputText = ""
	case "enter":
		if strings.TrimSpace(m.InputText) != "" {
			m.TodoList.AddItem(strings.TrimSpace(m.InputText))
		}
		m.Mode = ModeList
		m.InputText = ""
	case "backspace":
		if len(m.InputText) > 0 {
			m.InputText = m.InputText[:len(m.InputText)-1]
		}
	default:
		if len(msg.String()) == 1 {
			m.InputText += msg.String()
		}
	}
	return m, nil
}

// View renders the enhanced model with rich colors
func (m EnhancedModel) View() string {
	var b strings.Builder

	// Clear screen and reset cursor
	b.WriteString("\033[2J\033[H")

	// Title with gradient-like effect
	title := "🫧 " + m.Styles.Title.Render("Bubbletea Todo App") + " 🫧"
	b.WriteString(title)
	b.WriteString("\n")

	// Status with colors
	completed := m.TodoList.GetCompletedCount()
	total := m.TodoList.GetTotalCount()
	
	var statusText string
	if completed == total && total > 0 {
		statusText = fmt.Sprintf("🎉 All %d tasks completed! 🎉", total)
	} else {
		statusText = fmt.Sprintf("📋 %d completed • %d total • %d remaining", 
			completed, total, total-completed)
	}
	
	status := m.Styles.Subtitle.Render(statusText)
	b.WriteString(status)
	b.WriteString("\n\n")

	switch m.Mode {
	case ModeList:
		b.WriteString(m.renderEnhancedListMode())
	case ModeAdd:
		b.WriteString(m.renderEnhancedAddMode())
	}

	return b.String()
}

// renderEnhancedListMode renders the enhanced list view
func (m EnhancedModel) renderEnhancedListMode() string {
	var b strings.Builder

	if len(m.TodoList.Items) == 0 {
		emptyMsg := m.Styles.TodoItem.Render("✨ No todos yet! Press 'a' to add your first task ✨")
		b.WriteString(emptyMsg)
		b.WriteString("\n\n")
	} else {
		for i, item := range m.TodoList.Items {
			var line string
			var checkbox string
			
			if item.Completed {
				checkbox = m.Styles.CheckboxDone.Render("✅")
			} else {
				checkbox = m.Styles.Checkbox.Render("⬜")
			}

			// Add priority indicators based on position
			priority := ""
			if i == 0 && !item.Completed {
				priority = "🔥 "
			} else if i < 3 && !item.Completed {
				priority = "⭐ "
			}

			line = fmt.Sprintf("%s %s%s", checkbox, priority, item.Text)

			var style lipgloss.Style
			if i == m.TodoList.GetSelectedIndex() {
				// Add selection indicator
				line = "▶ " + line
				style = m.Styles.Selected
			} else if item.Completed {
				style = m.Styles.Completed
			} else {
				style = m.Styles.TodoItem
			}

			b.WriteString(style.Render(line))
			b.WriteString("\n")
		}
		b.WriteString("\n")
	}

	// Enhanced help text with colors
	helpItems := []string{
		"🔼 ↑/k: up",
		"🔽 ↓/j: down", 
		"✅ space/enter: toggle",
		"➕ a: add",
		"🗑️  d/x: delete",
		"🚪 q: quit",
	}
	
	help := m.Styles.Help.Render(strings.Join(helpItems, " • "))
	b.WriteString(help)

	return b.String()
}

// renderEnhancedAddMode renders the enhanced add todo view
func (m EnhancedModel) renderEnhancedAddMode() string {
	var b strings.Builder

	prompt := m.Styles.Prompt.Render("✨ Add a new todo:")
	b.WriteString(prompt)
	b.WriteString("\n\n")
	
	// Enhanced input with cursor
	inputText := m.InputText + "█"
	input := m.Styles.Input.Render(inputText)
	b.WriteString(input)
	b.WriteString("\n\n")

	// Enhanced help for add mode
	helpText := "💾 enter: save • ❌ esc: cancel"
	help := m.Styles.Help.Render(helpText)
	b.WriteString(help)

	return b.String()
}

// GetTodoList returns the current todo list (for web interface)
func (m EnhancedModel) GetTodoList() *TodoList {
	return m.TodoList
}

// ProcessWebMessage processes messages from the web interface
func (m *EnhancedModel) ProcessWebMessage(msgType string, data interface{}) tea.Cmd {
	switch msgType {
	case "keypress":
		if keyData, ok := data.(map[string]interface{}); ok {
			if key, ok := keyData["key"].(string); ok {
				// Convert web key events to tea.KeyMsg
				keyMsg := tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune(key)}
				
				// Handle special keys
				switch key {
				case "ArrowUp":
					keyMsg = tea.KeyMsg{Type: tea.KeyUp}
				case "ArrowDown":
					keyMsg = tea.KeyMsg{Type: tea.KeyDown}
				case "Enter":
					keyMsg = tea.KeyMsg{Type: tea.KeyEnter}
				case "Escape":
					keyMsg = tea.KeyMsg{Type: tea.KeyEsc}
				case "Backspace":
					keyMsg = tea.KeyMsg{Type: tea.KeyBackspace}
				case " ":
					keyMsg = tea.KeyMsg{Type: tea.KeySpace}
				}
				
				// Update the model
				newModel, cmd := m.Update(keyMsg)
				*m = newModel.(EnhancedModel)
				return cmd
			}
		}
	}
	return nil
}

