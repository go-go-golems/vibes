package doc

import (
	"embed"
	"github.com/go-go-golems/glazed/pkg/help"
)

//go:embed *
var docFS embed.FS

// AddDocToHelpSystem loads documentation sections from the embedded filesystem
// into the provided help system. This enables the help commands to display
// documentation pages.
func AddDocToHelpSystem(helpSystem *help.HelpSystem) error {
	return helpSystem.LoadSectionsFromFS(docFS, ".")
}

