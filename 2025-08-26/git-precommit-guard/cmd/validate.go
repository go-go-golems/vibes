package cmd

import (
	"context"
	"fmt"

	"github.com/go-go-golems/glazed/pkg/cli"
	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/go-go-golems/glazed/pkg/middlewares"
	"github.com/go-go-golems/glazed/pkg/types"
	"github.com/pkg/errors"

	cfgpkg "github.com/user/git-precommit-guard/pkg/config"
)

// ValidateCommand implements the dual-mode validate-config command
 type ValidateCommand struct {
	*cmds.CommandDescription
}

// ValidateSettings holds parsed parameters
 type ValidateSettings struct {
	Config string `glazed.parameter:"config"`
}

// NewValidateCommand constructs the Glazed command description
 func NewValidateCommand() (*ValidateCommand, error) {
	commandSettingsLayer, err := cli.NewCommandSettingsLayer()
	if err != nil {
		return nil, errors.Wrap(err, "create command settings layer")
	}

	cd := cmds.NewCommandDescription(
		"validate-config",
		cmds.WithShort("Validate configuration file"),
		cmds.WithLong(`Validate the configuration file for syntax and logical errors.

This command loads and validates the configuration file, reporting any
issues found in the YAML syntax or configuration values.`),
		cmds.WithFlags(
			parameters.NewParameterDefinition("config", parameters.ParameterTypeString,
				parameters.WithDefault(""), parameters.WithHelp("config file (default is .precommit-guard.yml)"), parameters.WithShortFlag("c")),
		),
		cmds.WithLayersList(commandSettingsLayer),
	)

	return &ValidateCommand{CommandDescription: cd}, nil
}

// Run outputs human-readable validation summary
 func (c *ValidateCommand) Run(ctx context.Context, pl *layers.ParsedLayers) error {
	settings := &ValidateSettings{}
	if err := pl.InitializeStruct(layers.DefaultSlug, settings); err != nil {
		return errors.Wrap(err, "parse settings")
	}

	configPath := settings.Config
	fmt.Printf("Validating configuration file: %s\n", configPath)

	cfg, err := cfgpkg.LoadConfig(configPath)
	if err != nil {
		return errors.Wrap(err, "configuration validation failed")
	}

	fmt.Println("\n✓ Configuration is valid!")
	fmt.Printf("  Version: %s\n", cfg.Version)
	fmt.Printf("  Timeout: %v\n", cfg.Settings.Timeout)
	fmt.Printf("  Fail Fast: %t\n", cfg.Settings.FailFast)

	fmt.Printf("\nGlobal Excludes (%d patterns):\n", len(cfg.Excludes))
	for _, exclude := range cfg.Excludes {
		fmt.Printf("  - %s\n", exclude)
	}

	fmt.Printf("\nRules:\n")
	fmt.Printf("  ELF Detection: ")
	if cfg.Rules.ElfDetection.Enabled {
		fmt.Printf("✓ Enabled (%s)\n", cfg.Rules.ElfDetection.Severity)
		fmt.Printf("    Magic: %s\n", cfg.Rules.ElfDetection.Config.ElfMagic)
		fmt.Printf("    File Patterns: %d\n", len(cfg.Rules.ElfDetection.Config.FilePatterns))
		fmt.Printf("    MIME Types: %d\n", len(cfg.Rules.ElfDetection.Config.MimeTypes))
		fmt.Printf("    Directory Overrides: %d\n", len(cfg.Rules.ElfDetection.Config.DirectoryOverrides))
	} else {
		fmt.Printf("✗ Disabled\n")
	}

	fmt.Printf("  File Size: ")
	if cfg.Rules.FileSize.Enabled {
		fmt.Printf("✓ Enabled (%s)\n", cfg.Rules.FileSize.Severity)
		fmt.Printf("    Max Size: %d MB\n", cfg.Rules.FileSize.Config.MaxSizeMB)
		fmt.Printf("    Warn Size: %d MB\n", cfg.Rules.FileSize.Config.WarnSizeMB)
		fmt.Printf("    Directory Overrides: %d\n", len(cfg.Rules.FileSize.Config.DirectoryOverrides))
	} else {
		fmt.Printf("✗ Disabled\n")
	}

	fmt.Printf("  MIME Detection: ")
	if cfg.Rules.MimeDetection.Enabled {
		fmt.Printf("✓ Enabled (%s)\n", cfg.Rules.MimeDetection.Severity)
		fmt.Printf("    Blocked Types: %d\n", len(cfg.Rules.MimeDetection.Config.BlockedTypes))
		fmt.Printf("    Allowed Types: %d\n", len(cfg.Rules.MimeDetection.Config.AllowedTypes))
		fmt.Printf("    Directory Overrides: %d\n", len(cfg.Rules.MimeDetection.Config.DirectoryOverrides))
	} else {
		fmt.Printf("✗ Disabled\n")
	}

	fmt.Printf("\nReporting:\n")
	fmt.Printf("  Format: %s\n", cfg.Reporting.Format)
	fmt.Printf("  Colors: %t\n", cfg.Reporting.Colors)
	fmt.Printf("  Show Passed: %t\n", cfg.Reporting.ShowPassed)
	fmt.Printf("  Summary: %t\n", cfg.Reporting.Summary)

	return nil
}

// RunIntoGlazeProcessor outputs structured validation summary
 func (c *ValidateCommand) RunIntoGlazeProcessor(ctx context.Context, pl *layers.ParsedLayers, gp middlewares.Processor) error {
	settings := &ValidateSettings{}
	if err := pl.InitializeStruct(layers.DefaultSlug, settings); err != nil {
		return errors.Wrap(err, "parse settings")
	}
	cfg, err := cfgpkg.LoadConfig(settings.Config)
	if err != nil {
		return errors.Wrap(err, "configuration validation failed")
	}

	row := types.NewRow(
		types.MRP("version", cfg.Version),
		types.MRP("timeout", cfg.Settings.Timeout.String()),
		types.MRP("fail_fast", cfg.Settings.FailFast),
		types.MRP("excludes_count", len(cfg.Excludes)),
		types.MRP("elf_enabled", cfg.Rules.ElfDetection.Enabled),
		types.MRP("filesize_enabled", cfg.Rules.FileSize.Enabled),
		types.MRP("mime_enabled", cfg.Rules.MimeDetection.Enabled),
		types.MRP("report_format", cfg.Reporting.Format),
	)
	return gp.AddRow(ctx, row)
}

var _ cmds.BareCommand = &ValidateCommand{}
var _ cmds.GlazeCommand = &ValidateCommand{}

