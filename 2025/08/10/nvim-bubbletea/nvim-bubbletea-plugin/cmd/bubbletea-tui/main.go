package main

import (
	"flag"
	"fmt"
	"io"
	"log"
	"strings"
	"time"

	"github.com/charmbracelet/bubbles/list"
	"github.com/charmbracelet/bubbles/progress"
	"github.com/charmbracelet/bubbles/textinput"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

var (
	width  = flag.Int("width", 80, "Terminal width")
	height = flag.Int("height", 24, "Terminal height")
)

// Define styles
var (
	titleStyle = lipgloss.NewStyle().
			Foreground(lipgloss.Color("#FAFAFA")).
			Background(lipgloss.Color("#7D56F4")).
			Padding(0, 1)

	itemStyle = lipgloss.NewStyle().PaddingLeft(4)

	selectedItemStyle = lipgloss.NewStyle().
				PaddingLeft(2).
				Foreground(lipgloss.Color("170"))

	paginationStyle = list.DefaultStyles().PaginationStyle.PaddingLeft(4)

	helpStyle = list.DefaultStyles().HelpStyle.PaddingLeft(4).PaddingBottom(1)

	quitTextStyle = lipgloss.NewStyle().Margin(1, 0, 2, 4)
)

// Item represents a list item
type item string

func (i item) FilterValue() string { return "" }

// Model represents the application state
type model struct {
	list        list.Model
	textInput   textinput.Model
	progress    progress.Model
	choice      string
	quitting    bool
	mode        string // "menu", "input", "progress", "result"
	inputValue  string
	progressVal float64
}

// Initialize the model
func initialModel() model {
	items := []list.Item{
		item("🎯 Interactive Demo"),
		item("📝 Text Input Example"),
		item("📊 Progress Bar Demo"),
		item("🎨 Style Showcase"),
		item("❌ Quit"),
	}

	const defaultWidth = 20

	l := list.New(items, itemDelegate{}, defaultWidth, 14)
	l.Title = "Neovim Bubble Tea Plugin Demo"
	l.SetShowStatusBar(false)
	l.SetFilteringEnabled(false)
	l.Styles.Title = titleStyle
	l.Styles.PaginationStyle = paginationStyle
	l.Styles.HelpStyle = helpStyle

	ti := textinput.New()
	ti.Placeholder = "Type something here..."
	ti.Focus()
	ti.CharLimit = 156
	ti.Width = 20

	p := progress.New(progress.WithDefaultGradient())

	return model{
		list:      l,
		textInput: ti,
		progress:  p,
		mode:      "menu",
	}
}

// Init initializes the model (required by tea.Model interface)
func (m model) Init() tea.Cmd {
	return nil
}

// Item delegate for list rendering
type itemDelegate struct{}

func (d itemDelegate) Height() int                             { return 1 }
func (d itemDelegate) Spacing() int                            { return 0 }
func (d itemDelegate) Update(_ tea.Msg, _ *list.Model) tea.Cmd { return nil }
func (d itemDelegate) Render(w io.Writer, m list.Model, index int, listItem list.Item) {
	i, ok := listItem.(item)
	if !ok {
		return
	}

	str := fmt.Sprintf("%d. %s", index+1, i)

	fn := itemStyle.Render
	if index == m.Index() {
		fn = func(s ...string) string {
			return selectedItemStyle.Render("> " + strings.Join(s, " "))
		}
	}

	fmt.Fprint(w, fn(str))
}

// Update handles messages and updates the model
func (m model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		m.list.SetWidth(msg.Width)
		m.textInput.Width = msg.Width - 4
		m.progress.Width = msg.Width - 4
		return m, nil

	case tea.KeyMsg:
		switch m.mode {
		case "menu":
			switch keypress := msg.String(); keypress {
			case "ctrl+c", "q":
				m.quitting = true
				return m, tea.Quit

			case "enter":
				i, ok := m.list.SelectedItem().(item)
				if ok {
					m.choice = string(i)
					switch m.choice {
					case "🎯 Interactive Demo":
						m.mode = "result"
						return m, nil
					case "📝 Text Input Example":
						m.mode = "input"
						return m, nil
					case "📊 Progress Bar Demo":
						m.mode = "progress"
						m.progressVal = 0
						return m, m.tickCmd()
					case "🎨 Style Showcase":
						m.mode = "result"
						return m, nil
					case "❌ Quit":
						m.quitting = true
						return m, tea.Quit
					}
				}
			}

		case "input":
			switch msg.String() {
			case "ctrl+c", "esc":
				m.mode = "menu"
				m.textInput.SetValue("")
				return m, nil
			case "enter":
				m.inputValue = m.textInput.Value()
				m.mode = "result"
				return m, nil
			}

		case "progress":
			switch msg.String() {
			case "ctrl+c", "esc":
				m.mode = "menu"
				m.progressVal = 0
				return m, nil
			}

		case "result":
			switch msg.String() {
			case "ctrl+c", "esc", "enter":
				m.mode = "menu"
				return m, nil
			}
		}

	case progressMsg:
		if m.mode == "progress" {
			if m.progressVal >= 1.0 {
				m.mode = "result"
				return m, nil
			}
			m.progressVal += 0.05
			return m, m.tickCmd()
		}
	}

	var cmd tea.Cmd
	switch m.mode {
	case "menu":
		m.list, cmd = m.list.Update(msg)
	case "input":
		m.textInput, cmd = m.textInput.Update(msg)
	case "progress":
		var progressModel tea.Model
		progressModel, cmd = m.progress.Update(msg)
		m.progress = progressModel.(progress.Model)
	}

	return m, cmd
}

// Progress message type
type progressMsg time.Time

// Tick command for progress bar
func (m model) tickCmd() tea.Cmd {
	return tea.Tick(time.Millisecond*100, func(t time.Time) tea.Msg {
		return progressMsg(t)
	})
}

// View renders the UI
func (m model) View() string {
	if m.quitting {
		return quitTextStyle.Render("Thanks for trying the Neovim Bubble Tea Plugin! 👋")
	}

	switch m.mode {
	case "menu":
		return "\n" + m.list.View()

	case "input":
		return fmt.Sprintf(
			"Text Input Demo\n\n%s\n\n%s",
			m.textInput.View(),
			"(Press Enter to submit, Esc to go back)",
		) + "\n"

	case "progress":
		return fmt.Sprintf(
			"Progress Bar Demo\n\n%s\n\n%s",
			m.progress.ViewAs(m.progressVal),
			"(Press Esc to go back)",
		) + "\n"

	case "result":
		var content string
		switch m.choice {
		case "🎯 Interactive Demo":
			content = "🎉 Welcome to the Interactive Demo!\n\n" +
				"This Bubble Tea TUI is running inside your Neovim buffer!\n" +
				"You can navigate with arrow keys, select with Enter,\n" +
				"and use various interactive components.\n\n" +
				"Features demonstrated:\n" +
				"• List navigation\n" +
				"• Text input handling\n" +
				"• Progress bars\n" +
				"• Styled components\n" +
				"• Real-time updates"

		case "📝 Text Input Example":
			if m.inputValue != "" {
				content = fmt.Sprintf("You entered: '%s'\n\nText input works perfectly in Neovim!", m.inputValue)
			} else {
				content = "No input provided."
			}

		case "📊 Progress Bar Demo":
			content = "✅ Progress completed!\n\nThe progress bar demo shows how\nreal-time updates work in the TUI."

		case "🎨 Style Showcase":
			content = lipgloss.NewStyle().
				Border(lipgloss.RoundedBorder()).
				BorderForeground(lipgloss.Color("62")).
				Padding(1).
				Render("🎨 Style Showcase\n\n" +
					lipgloss.NewStyle().Foreground(lipgloss.Color("86")).Render("✨ Colorful text\n") +
					lipgloss.NewStyle().Foreground(lipgloss.Color("212")).Render("🌈 Multiple colors\n") +
					lipgloss.NewStyle().Bold(true).Render("💪 Bold text\n") +
					lipgloss.NewStyle().Italic(true).Render("📖 Italic text\n") +
					"\nLipgloss styling works great!")
		}

		return fmt.Sprintf("%s\n\n%s", content, "(Press Enter or Esc to go back)")
	}

	return ""
}

func main() {
	flag.Parse()

	// Set terminal size if provided
	if *width > 0 && *height > 0 {
		// Note: In a real implementation, you might want to handle terminal resizing
		// For now, we'll just use the provided dimensions
	}

	p := tea.NewProgram(initialModel(), tea.WithAltScreen())
	if _, err := p.Run(); err != nil {
		log.Fatal(err)
	}
}

