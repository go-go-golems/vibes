package cmd

import (
	"embed"
	"github.com/go-go-golems/glazed/pkg/help"
)

//go:embed doc/*
var docFS embed.FS

// AddDocToHelpSystem loads documentation from the embedded filesystem into the help system
func AddDocToHelpSystem(helpSystem *help.HelpSystem) error {
	return helpSystem.LoadSectionsFromFS(docFS, "doc")
}

