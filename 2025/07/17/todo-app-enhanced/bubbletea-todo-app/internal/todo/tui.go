package todo

import (
	"fmt"
	"strings"

	"github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

// AppMode represents the current mode of the application
type AppMode int

const (
	ModeList AppMode = iota
	ModeAdd
)

// Model represents the Bubbletea model
type Model struct {
	TodoList    *TodoList
	Mode        AppMode
	InputText   string
	Width       int
	Height      int
	Styles      Styles
}

// Styles contains all the styling for the TUI
type Styles struct {
	Title       lipgloss.Style
	Subtitle    lipgloss.Style
	TodoItem    lipgloss.Style
	Selected    lipgloss.Style
	Completed   lipgloss.Style
	Input       lipgloss.Style
	Help        lipgloss.Style
	StatusBar   lipgloss.Style
}

// NewModel creates a new Bubbletea model
func NewModel() Model {
	return Model{
		TodoList:  NewTodoList(),
		Mode:      ModeList,
		InputText: "",
		Width:     80,
		Height:    24,
		Styles:    NewStyles(),
	}
}

// NewStyles creates the default styles
func NewStyles() Styles {
	return Styles{
		Title: lipgloss.NewStyle().
			Bold(true).
			Foreground(lipgloss.Color("#7C3AED")).
			MarginBottom(1),
		Subtitle: lipgloss.NewStyle().
			Foreground(lipgloss.Color("#6B7280")).
			MarginBottom(1),
		TodoItem: lipgloss.NewStyle().
			PaddingLeft(2),
		Selected: lipgloss.NewStyle().
			Background(lipgloss.Color("#374151")).
			Foreground(lipgloss.Color("#F9FAFB")).
			PaddingLeft(2),
		Completed: lipgloss.NewStyle().
			Foreground(lipgloss.Color("#6B7280")).
			Strikethrough(true).
			PaddingLeft(2),
		Input: lipgloss.NewStyle().
			Border(lipgloss.RoundedBorder()).
			BorderForeground(lipgloss.Color("#7C3AED")).
			Padding(0, 1),
		Help: lipgloss.NewStyle().
			Foreground(lipgloss.Color("#6B7280")).
			MarginTop(1),
		StatusBar: lipgloss.NewStyle().
			Background(lipgloss.Color("#374151")).
			Foreground(lipgloss.Color("#F9FAFB")).
			Padding(0, 1),
	}
}

// Init initializes the model
func (m Model) Init() tea.Cmd {
	return nil
}

// Update handles messages and updates the model
func (m Model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
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
func (m Model) updateListMode(msg tea.KeyMsg) (tea.Model, tea.Cmd) {
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
func (m Model) updateAddMode(msg tea.KeyMsg) (tea.Model, tea.Cmd) {
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

// View renders the model
func (m Model) View() string {
	var b strings.Builder

	// Title
	b.WriteString(m.Styles.Title.Render("📝 Todo List"))
	b.WriteString("\n")

	// Status
	completed := m.TodoList.GetCompletedCount()
	total := m.TodoList.GetTotalCount()
	status := fmt.Sprintf("Tasks: %d completed, %d total", completed, total)
	b.WriteString(m.Styles.Subtitle.Render(status))
	b.WriteString("\n")

	switch m.Mode {
	case ModeList:
		b.WriteString(m.renderListMode())
	case ModeAdd:
		b.WriteString(m.renderAddMode())
	}

	return b.String()
}

// renderListMode renders the list view
func (m Model) renderListMode() string {
	var b strings.Builder

	if len(m.TodoList.Items) == 0 {
		b.WriteString(m.Styles.TodoItem.Render("No todos yet. Press 'a' to add one!"))
		b.WriteString("\n\n")
	} else {
		for i, item := range m.TodoList.Items {
			var line string
			checkbox := "☐"
			if item.Completed {
				checkbox = "☑"
			}

			line = fmt.Sprintf("%s %s", checkbox, item.Text)

			var style lipgloss.Style
			if i == m.TodoList.GetSelectedIndex() {
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

	// Help text
	help := []string{
		"↑/k: up",
		"↓/j: down",
		"space/enter: toggle",
		"a: add",
		"d/x: delete",
		"q: quit",
	}
	b.WriteString(m.Styles.Help.Render(strings.Join(help, " • ")))

	return b.String()
}

// renderAddMode renders the add todo view
func (m Model) renderAddMode() string {
	var b strings.Builder

	b.WriteString("Add new todo:\n\n")
	
	input := m.Styles.Input.Render(m.InputText + "│")
	b.WriteString(input)
	b.WriteString("\n\n")

	help := "enter: save • esc: cancel"
	b.WriteString(m.Styles.Help.Render(help))

	return b.String()
}

// GetTodoList returns the current todo list (for web interface)
func (m Model) GetTodoList() *TodoList {
	return m.TodoList
}

// ProcessWebMessage processes messages from the web interface
func (m *Model) ProcessWebMessage(msgType string, data interface{}) tea.Cmd {
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
				*m = newModel.(Model)
				return cmd
			}
		}
	}
	return nil
}

