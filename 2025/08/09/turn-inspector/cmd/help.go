package cmd

import (
	"os"

	help "github.com/go-go-golems/glazed/pkg/help"
	helpCmd "github.com/go-go-golems/glazed/pkg/help/cmd"
)

func init() {
	// Initialize Glazed help system and load local docs
	hs := help.NewHelpSystem()
	// Load markdown sections from ./docs if present
	_ = hs.LoadSectionsFromFS(os.DirFS("."), "docs")

	// Wire into Cobra root command with Glazed's helper
	helpCmd.SetupCobraRootCommand(hs, rootCmd)
}
