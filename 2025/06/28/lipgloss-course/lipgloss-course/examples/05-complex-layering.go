package main

import (
	"fmt"

	"github.com/charmbracelet/lipgloss/v2"
)

func main() {
	// Create a desktop background
	desktopStyle := lipgloss.NewStyle().
		Background(lipgloss.Color("#1e1e2e")).
		Foreground(lipgloss.Color("#cdd6f4")).
		Width(80).
		Height(20)

	desktop := lipgloss.NewLayer(desktopStyle.Render("")).
		X(0).Y(0).Z(0).ID("desktop")

	// Create window style
	windowStyle := lipgloss.NewStyle().
		Border(lipgloss.RoundedBorder()).
		BorderForeground(lipgloss.Color("#89b4fa")).
		Background(lipgloss.Color("#313244")).
		Foreground(lipgloss.Color("#cdd6f4"))

	// Create window title bar style
	titleBarStyle := lipgloss.NewStyle().
		Background(lipgloss.Color("#89b4fa")).
		Foreground(lipgloss.Color("#1e1e2e")).
		Padding(0, 1).
		Bold(true)

	// Window 1 - Terminal
	terminalContent := "$ ls -la\ntotal 42\ndrwxr-xr-x  5 user user 4096 Dec 27 10:30 .\ndrwxr-xr-x  3 user user 4096 Dec 27 10:29 ..\n-rw-r--r--  1 user user  220 Dec 27 10:29 .bashrc"
	terminal := lipgloss.NewLayer(
		lipgloss.JoinVertical(lipgloss.Left,
			titleBarStyle.Render("Terminal"),
			windowStyle.Render(terminalContent),
		),
	).X(5).Y(2).Z(1).ID("terminal")

	// Window 2 - Editor
	editorContent := "package main\n\nimport \"fmt\"\n\nfunc main() {\n    fmt.Println(\"Hello, World!\")\n}"
	editor := lipgloss.NewLayer(
		lipgloss.JoinVertical(lipgloss.Left,
			titleBarStyle.Render("Editor - main.go"),
			windowStyle.Render(editorContent),
		),
	).X(25).Y(4).Z(2).ID("editor")

	// Window 3 - File Manager
	fileContent := "Documents/\nDownloads/\nPictures/\nVideos/\nprojects/\n  ├── lipgloss-demo/\n  └── other-project/"
	fileManager := lipgloss.NewLayer(
		lipgloss.JoinVertical(lipgloss.Left,
			titleBarStyle.Render("File Manager"),
			windowStyle.Render(fileContent),
		),
	).X(45).Y(6).Z(3).ID("fileManager")

	// Create a modal dialog (highest Z-index)
	modalStyle := lipgloss.NewStyle().
		Border(lipgloss.ThickBorder()).
		BorderForeground(lipgloss.Color("#f38ba8")).
		Background(lipgloss.Color("#45475a")).
		Foreground(lipgloss.Color("#cdd6f4")).
		Padding(1, 2)

	modalContent := "Save changes?\n\n[Y] Yes  [N] No  [C] Cancel"
	modal := lipgloss.NewLayer(modalStyle.Render(modalContent)).
		X(30).Y(8).Z(10).ID("modal")

	// Create canvas with all layers
	canvas := lipgloss.NewCanvas(desktop, terminal, editor, fileManager, modal)

	fmt.Println("Complex Layering Demo - Desktop Environment")
	fmt.Println("==========================================")
	fmt.Println()
	fmt.Println(canvas.Render())
}

