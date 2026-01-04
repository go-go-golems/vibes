package cmd

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/go-go-golems/glazed/pkg/cli"
	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/pkg/errors"

	gitpkg "github.com/user/git-precommit-guard/pkg/git"
)

// UninstallCommand implements the dual-mode uninstall command
type UninstallCommand struct {
	*cmds.CommandDescription
}

// UninstallSettings currently unused
type UninstallSettings struct{}

// NewUninstallCommand constructs the Glazed command description
func NewUninstallCommand() (*UninstallCommand, error) {
	commandSettingsLayer, err := cli.NewCommandSettingsLayer()
	if err != nil {
		return nil, errors.Wrap(err, "create command settings layer")
	}

	cd := cmds.NewCommandDescription(
		"uninstall",
		cmds.WithShort("Uninstall git pre-commit hook"),
		cmds.WithLong(`Remove the git pre-commit hook installed by git-precommit-guard.`),
		cmds.WithLayersList(commandSettingsLayer),
	)

	return &UninstallCommand{CommandDescription: cd}, nil
}

// Run removes the hook with a confirmation prompt
func (c *UninstallCommand) Run(ctx context.Context, pl *layers.ParsedLayers) error {
	if !gitpkg.IsGitRepository() {
		return errors.New("not in a git repository")
	}

	gitDir, err := gitpkg.GetGitDir()
	if err != nil {
		return errors.Wrap(err, "get git dir")
	}
	hooksDir := filepath.Join(gitDir, "hooks")
	hookPath := filepath.Join(hooksDir, "pre-commit")

	if _, err := os.Stat(hookPath); os.IsNotExist(err) {
		fmt.Printf("No pre-commit hook found at %s\n", hookPath)
		return nil
	}

	fmt.Printf("This will remove the pre-commit hook at %s. Proceed? (y/N)\n", hookPath)
	var response string
	fmt.Scanln(&response)
	if !strings.EqualFold(response, "y") {
		fmt.Println("Uninstall cancelled")
		return nil
	}

	if err := os.Remove(hookPath); err != nil {
		return errors.Wrap(err, "remove hook script")
	}

	fmt.Println("Successfully uninstalled pre-commit hook")
	return nil
}

var _ cmds.BareCommand = &UninstallCommand{}
