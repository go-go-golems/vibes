package main

import (
	"fmt"
	"os"

	"github.com/go-go-golems/glazed/pkg/cli"
	"github.com/go-go-golems/glazed/pkg/cmds/logging"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/help"
	help_cmd "github.com/go-go-golems/glazed/pkg/help/cmd"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"

	cmdpkg "github.com/user/git-precommit-guard/cmd"
)

func main() {
	rootCmd := &cobra.Command{
		Use:   "git-precommit-guard",
		Short: "A git pre-commit hook utility to detect undesired files",
		Long: `Git Pre-commit Guard is a utility designed to be used as a git pre-commit hook
	to detect and prevent committing undesired files such as:

	- ELF binaries and executables
	- Files with certain MIME types
	- Files that exceed size limits

	The tool is highly configurable through YAML configuration files and supports
	directory-specific overrides for different rules.`,
		Version: "1.0.0",
		PersistentPreRun: func(cmd *cobra.Command, args []string) {
			err := logging.InitLoggerFromViper()
			cobra.CheckErr(err)
		},
	}

	// Set up logging layer and initialize logger
	err := logging.AddLoggingLayerToRootCommand(rootCmd, "git-precommit-guard")
	cobra.CheckErr(err)

	err = viper.BindPFlags(rootCmd.PersistentFlags())
	cobra.CheckErr(err)

	err = logging.InitLoggerFromViper()
	cobra.CheckErr(err)

	checkCmd, err := cmdpkg.NewCheckCommand()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating check command: %v\n", err)
		os.Exit(1)
	}
	validateCmd, err := cmdpkg.NewValidateCommand()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating validate command: %v\n", err)
		os.Exit(1)
	}
	installCmd, err := cmdpkg.NewInstallCommand()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating install command: %v\n", err)
		os.Exit(1)
	}
	uninstallCmd, err := cmdpkg.NewUninstallCommand()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating uninstall command: %v\n", err)
		os.Exit(1)
	}
	debugRootCmd, err := cmdpkg.NewDebugRootCommand()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating debug root command: %v\n", err)
		os.Exit(1)
	}
	debugGitCmd, err := cmdpkg.NewDebugGitCommand()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating debug-git command: %v\n", err)
		os.Exit(1)
	}

	cobraCheckCmd, err := cli.BuildCobraCommand(checkCmd,
		cli.WithDualMode(true),
		cli.WithGlazeToggleFlag("with-glaze-output"),
		cli.WithParserConfig(cli.CobraParserConfig{
			ShortHelpLayers: []string{layers.DefaultSlug},
			MiddlewaresFunc: cli.CobraCommandDefaultMiddlewares,
		}),
	)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error building check command: %v\n", err)
		os.Exit(1)
	}

	cobraValidateCmd, err := cli.BuildCobraCommand(validateCmd,
		cli.WithDualMode(true),
		cli.WithGlazeToggleFlag("with-glaze-output"),
		cli.WithParserConfig(cli.CobraParserConfig{
			ShortHelpLayers: []string{layers.DefaultSlug},
			MiddlewaresFunc: cli.CobraCommandDefaultMiddlewares,
		}),
	)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error building validate command: %v\n", err)
		os.Exit(1)
	}

	cobraInstallCmd, err := cli.BuildCobraCommand(installCmd,
		cli.WithDualMode(true),
		cli.WithGlazeToggleFlag("with-glaze-output"),
		cli.WithParserConfig(cli.CobraParserConfig{
			ShortHelpLayers: []string{layers.DefaultSlug},
			MiddlewaresFunc: cli.CobraCommandDefaultMiddlewares,
		}),
	)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error building install command: %v\n", err)
		os.Exit(1)
	}

	cobraUninstallCmd, err := cli.BuildCobraCommand(uninstallCmd,
		cli.WithDualMode(true),
		cli.WithGlazeToggleFlag("with-glaze-output"),
		cli.WithParserConfig(cli.CobraParserConfig{
			ShortHelpLayers: []string{layers.DefaultSlug},
			MiddlewaresFunc: cli.CobraCommandDefaultMiddlewares,
		}),
	)
	cobraDebugRootCmd, err := cli.BuildCobraCommand(debugRootCmd,
		cli.WithDualMode(true),
		cli.WithGlazeToggleFlag("with-glaze-output"),
		cli.WithParserConfig(cli.CobraParserConfig{
			ShortHelpLayers: []string{layers.DefaultSlug},
			MiddlewaresFunc: cli.CobraCommandDefaultMiddlewares,
		}),
	)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error building debug root command: %v\n", err)
		os.Exit(1)
	}
	cobraDebugGitCmd, err := cli.BuildCobraCommand(debugGitCmd,
		cli.WithDualMode(true),
		cli.WithGlazeToggleFlag("with-glaze-output"),
		cli.WithParserConfig(cli.CobraParserConfig{
			ShortHelpLayers: []string{layers.DefaultSlug},
			MiddlewaresFunc: cli.CobraCommandDefaultMiddlewares,
		}),
	)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error building debug-git command: %v\n", err)
		os.Exit(1)
	}

	rootCmd.AddCommand(cobraCheckCmd)
	rootCmd.AddCommand(cobraValidateCmd)
	rootCmd.AddCommand(cobraInstallCmd)
	rootCmd.AddCommand(cobraUninstallCmd)
	rootCmd.AddCommand(cobraDebugRootCmd)
	cobraDebugRootCmd.AddCommand(cobraDebugGitCmd)

	helpSystem := help.NewHelpSystem()
	help_cmd.SetupCobraRootCommand(helpSystem, rootCmd)

	if err := rootCmd.Execute(); err != nil {
		os.Exit(1)
	}
}
