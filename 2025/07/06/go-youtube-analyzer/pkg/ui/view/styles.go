package view

import (
	"github.com/charmbracelet/lipgloss"
)

// Color palette
var (
	Primary   = lipgloss.Color("#5af")
	Secondary = lipgloss.Color("#f5a")
	Success   = lipgloss.Color("#0f0")
	Warning   = lipgloss.Color("#fa0")
	Error     = lipgloss.Color("#f00")
	Muted     = lipgloss.Color("#666")
	Text      = lipgloss.Color("#fff")

	// Grays
	Gray100 = lipgloss.Color("#f7f7f7")
	Gray200 = lipgloss.Color("#e5e5e5")
	Gray300 = lipgloss.Color("#d4d4d4")
	Gray700 = lipgloss.Color("#404040")
	Gray800 = lipgloss.Color("#262626")
	Gray900 = lipgloss.Color("#171717")
)

// Styles represents the global style configuration
var Styles = struct {
	Title            lipgloss.Style
	Header           lipgloss.Style
	Footer           lipgloss.Style
	Content          lipgloss.Style
	Pane             lipgloss.Style
	PaneSelected     lipgloss.Style
	Input            lipgloss.Style
	InputFocused     lipgloss.Style
	Button           lipgloss.Style
	ButtonSelected   lipgloss.Style
	Error            lipgloss.Style
	Success          lipgloss.Style
	Warning          lipgloss.Style
	Label            lipgloss.Style
	Value            lipgloss.Style
	Spinner          lipgloss.Style
	Progress         lipgloss.Style
	Help             lipgloss.Style
	HelpKey          lipgloss.Style
	HelpValue        lipgloss.Style
	Section          lipgloss.Style
	SectionTitle     lipgloss.Style
	List             lipgloss.Style
	ListItem         lipgloss.Style
	ListSelected     lipgloss.Style
	Table            lipgloss.Style
	TableHeader      lipgloss.Style
	TableRow         lipgloss.Style
	TableRowSelected lipgloss.Style
}{
	Title: lipgloss.NewStyle().
		Bold(true).
		Foreground(Primary).
		PaddingLeft(1).
		PaddingRight(1),

	Header: lipgloss.NewStyle().
		Bold(true).
		Foreground(Text).
		Background(Gray800).
		PaddingLeft(2).
		PaddingRight(2).
		MarginBottom(1),

	Footer: lipgloss.NewStyle().
		Foreground(Muted).
		Background(Gray800).
		PaddingLeft(2).
		PaddingRight(2).
		MarginTop(1),

	Content: lipgloss.NewStyle().
		Padding(1, 2),

	Pane: lipgloss.NewStyle().
		Border(lipgloss.RoundedBorder()).
		BorderForeground(Gray300).
		Padding(1, 2).
		Margin(1),

	PaneSelected: lipgloss.NewStyle().
		Border(lipgloss.ThickBorder()).
		BorderForeground(Primary).
		Padding(1, 2).
		Margin(1),

	Input: lipgloss.NewStyle().
		Border(lipgloss.NormalBorder()).
		BorderForeground(Gray300).
		Padding(0, 1).
		Width(50),

	InputFocused: lipgloss.NewStyle().
		Border(lipgloss.NormalBorder()).
		BorderForeground(Primary).
		Padding(0, 1).
		Width(50),

	Button: lipgloss.NewStyle().
		Border(lipgloss.NormalBorder()).
		BorderForeground(Gray300).
		Padding(0, 2).
		Margin(0, 1).
		Align(lipgloss.Center),

	ButtonSelected: lipgloss.NewStyle().
		Border(lipgloss.NormalBorder()).
		BorderForeground(Primary).
		Background(Primary).
		Foreground(lipgloss.Color("#000")).
		Padding(0, 2).
		Margin(0, 1).
		Align(lipgloss.Center),

	Error: lipgloss.NewStyle().
		Foreground(Error).
		Bold(true).
		Padding(1, 2).
		Border(lipgloss.NormalBorder()).
		BorderForeground(Error),

	Success: lipgloss.NewStyle().
		Foreground(Success).
		Bold(true).
		Padding(1, 2).
		Border(lipgloss.NormalBorder()).
		BorderForeground(Success),

	Warning: lipgloss.NewStyle().
		Foreground(Warning).
		Bold(true).
		Padding(1, 2).
		Border(lipgloss.NormalBorder()).
		BorderForeground(Warning),

	Label: lipgloss.NewStyle().
		Foreground(Muted).
		Bold(true),

	Value: lipgloss.NewStyle().
		Foreground(Text),

	Spinner: lipgloss.NewStyle().
		Foreground(Primary).
		Bold(true),

	Progress: lipgloss.NewStyle().
		Foreground(Primary),

	Help: lipgloss.NewStyle().
		Foreground(Muted).
		Padding(1, 2),

	HelpKey: lipgloss.NewStyle().
		Foreground(Primary).
		Bold(true),

	HelpValue: lipgloss.NewStyle().
		Foreground(Muted),

	Section: lipgloss.NewStyle().
		Border(lipgloss.NormalBorder()).
		BorderForeground(Gray300).
		Padding(1, 2).
		Margin(1, 0),

	SectionTitle: lipgloss.NewStyle().
		Foreground(Primary).
		Bold(true).
		Underline(true).
		MarginBottom(1),

	List: lipgloss.NewStyle().
		Border(lipgloss.NormalBorder()).
		BorderForeground(Gray300).
		Padding(1, 2).
		Height(10),

	ListItem: lipgloss.NewStyle().
		Foreground(Text).
		Padding(0, 1),

	ListSelected: lipgloss.NewStyle().
		Foreground(Primary).
		Background(Gray700).
		Padding(0, 1).
		Bold(true),

	Table: lipgloss.NewStyle().
		Border(lipgloss.NormalBorder()).
		BorderForeground(Gray300).
		Padding(1, 2),

	TableHeader: lipgloss.NewStyle().
		Foreground(Primary).
		Bold(true).
		Underline(true).
		Padding(0, 1),

	TableRow: lipgloss.NewStyle().
		Foreground(Text).
		Padding(0, 1),

	TableRowSelected: lipgloss.NewStyle().
		Foreground(Primary).
		Background(Gray700).
		Padding(0, 1).
		Bold(true),
}

// Responsive helper functions
func AdaptWidth(width int) int {
	if width <= 40 {
		return width - 4
	}
	if width <= 80 {
		return width - 8
	}
	return width - 12
}

func AdaptHeight(height int) int {
	if height <= 20 {
		return height - 4
	}
	return height - 6
}

// Utility functions for rendering
func RenderHeader(title string, width int) string {
	return Styles.Header.Width(width).Render(title)
}

func RenderFooter(text string, width int) string {
	return Styles.Footer.Width(width).Render(text)
}

func RenderSection(title, content string, width int) string {
	titleStyle := Styles.SectionTitle
	contentStyle := Styles.Section.Width(width - 4)

	return lipgloss.JoinVertical(
		lipgloss.Left,
		titleStyle.Render(title),
		contentStyle.Render(content),
	)
}

func RenderError(err error) string {
	if err == nil {
		return ""
	}
	return Styles.Error.Render("Error: " + err.Error())
}

func RenderSuccess(msg string) string {
	return Styles.Success.Render(msg)
}

func RenderWarning(msg string) string {
	return Styles.Warning.Render(msg)
}

func RenderKeyValue(key, value string) string {
	return lipgloss.JoinHorizontal(
		lipgloss.Left,
		Styles.Label.Render(key+": "),
		Styles.Value.Render(value),
	)
}

func RenderList(items []string, selected int, width int) string {
	var renderedItems []string

	for i, item := range items {
		if i == selected {
			renderedItems = append(renderedItems, Styles.ListSelected.Render(item))
		} else {
			renderedItems = append(renderedItems, Styles.ListItem.Render(item))
		}
	}

	return Styles.List.Width(width).Render(
		lipgloss.JoinVertical(lipgloss.Left, renderedItems...),
	)
}
