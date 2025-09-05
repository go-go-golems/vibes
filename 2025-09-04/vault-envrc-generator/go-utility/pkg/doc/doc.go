package doc

import (
    "embed"
    "github.com/go-go-golems/glazed/pkg/help"
)

//go:embed *
var docFS embed.FS

// AddDocToHelpSystem loads this application's documentation pages into the Glazed help system.
func AddDocToHelpSystem(helpSystem *help.HelpSystem) error {
    return helpSystem.LoadSectionsFromFS(docFS, ".")
}

