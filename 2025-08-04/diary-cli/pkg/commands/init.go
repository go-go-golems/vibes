package commands

import (
	"fmt"
	"os"
	"path/filepath"
	"time"

	"github.com/spf13/cobra"

	"diary-cli/pkg/config"
)

// NewInitCommand creates the init command
func NewInitCommand(cfg *config.Config) *cobra.Command {
	var (
		vaultPath string
		force     bool
	)

	cmd := &cobra.Command{
		Use:   "init [path]",
		Short: "Initialize diary in current or specified directory",
		Long: `Initialize diary configuration and directory structure.

This command will:
- Set up the vault path in configuration
- Create the logs directory structure
- Create today's diary file if it doesn't exist

Examples:
  diary init                           # Initialize in current directory
  diary init /path/to/vault            # Initialize with specific vault path
  diary init --force                   # Overwrite existing configuration`,
		RunE: func(cmd *cobra.Command, args []string) error {
			// Determine vault path
			if len(args) > 0 {
				vaultPath = args[0]
			} else if vaultPath == "" {
				// Use current directory
				cwd, err := os.Getwd()
				if err != nil {
					return fmt.Errorf("failed to get current directory: %w", err)
				}
				vaultPath = cwd
			}

			return runInit(cfg, vaultPath, force)
		},
	}

	cmd.Flags().StringVarP(&vaultPath, "vault-path", "p", "", "Vault path (default: current directory)")
	cmd.Flags().BoolVarP(&force, "force", "f", false, "Overwrite existing configuration")

	return cmd
}

// runInit handles the initialization process
func runInit(cfg *config.Config, vaultPath string, force bool) error {
	// Expand and resolve path
	if vaultPath[0] == '~' {
		homeDir, _ := os.UserHomeDir()
		vaultPath = filepath.Join(homeDir, vaultPath[1:])
	}
	
	absPath, err := filepath.Abs(vaultPath)
	if err != nil {
		return fmt.Errorf("invalid path: %w", err)
	}

	// Check if vault path exists
	if _, err := os.Stat(absPath); os.IsNotExist(err) {
		return fmt.Errorf("vault path does not exist: %s", absPath)
	}

	// Check if already initialized (unless force)
	if !force {
		logsDir := filepath.Join(absPath, cfg.LogsPath)
		if _, err := os.Stat(logsDir); err == nil {
			return fmt.Errorf("diary already initialized in %s (use --force to overwrite)", absPath)
		}
	}

	fmt.Printf("Initializing diary in: %s\n", absPath)

	// Update configuration
	cfg.VaultPath = absPath
	if err := cfg.Save(); err != nil {
		return fmt.Errorf("failed to save configuration: %w", err)
	}
	fmt.Printf("✓ Updated configuration\n")

	// Create logs directory
	logsDir := cfg.GetLogsDir()
	if err := os.MkdirAll(logsDir, 0755); err != nil {
		return fmt.Errorf("failed to create logs directory: %w", err)
	}
	fmt.Printf("✓ Created logs directory: %s\n", logsDir)

	// Create today's file if it doesn't exist
	todayFile := cfg.GetTodayFile()
	if _, err := os.Stat(todayFile); os.IsNotExist(err) {
		if err := createTodayFile(todayFile); err != nil {
			return fmt.Errorf("failed to create today's file: %w", err)
		}
		fmt.Printf("✓ Created today's file: %s\n", todayFile)
	} else {
		fmt.Printf("✓ Today's file already exists: %s\n", todayFile)
	}

	// Create sample directory structure
	if err := createSampleStructure(cfg); err != nil {
		fmt.Printf("⚠️  Warning: failed to create sample structure: %v\n", err)
	} else {
		fmt.Printf("✓ Created sample directory structure\n")
	}

	fmt.Println()
	fmt.Println("🎉 Diary initialization complete!")
	fmt.Println()
	fmt.Println("Next steps:")
	fmt.Printf("  1. Add your first entry: diary add til \"Today I learned about diary CLI\"\n")
	fmt.Printf("  2. List entries: diary list\n")
	fmt.Printf("  3. Create a todo: diary todo \"Set up my diary workflow\"\n")
	fmt.Printf("  4. Check configuration: diary config\n")

	return nil
}

// createTodayFile creates today's diary file with template
func createTodayFile(filePath string) error {
	template := fmt.Sprintf(`# Log %s/%s

## To Process

`, getCurrentDate().Format("2006/01"), getCurrentDate().Format("2006-01-02"))

	return os.WriteFile(filePath, []byte(template), 0644)
}

// createSampleStructure creates a sample directory structure
func createSampleStructure(cfg *config.Config) error {
	// Create year/month subdirectories for better organization
	now := getCurrentDate()
	yearDir := filepath.Join(cfg.GetLogsDir(), now.Format("2006"))
	monthDir := filepath.Join(yearDir, now.Format("01"))

	if err := os.MkdirAll(monthDir, 0755); err != nil {
		return err
	}

	// Create a README file
	readmePath := filepath.Join(cfg.GetLogsDir(), "README.md")
	readmeContent := `# Diary Logs

This directory contains your diary entries organized by date.

## Structure

- Daily files are named using the format: YYYY-MM-DD.md
- Each file contains entries for that specific date
- Entries can be in different formats: default markdown, enhanced markdown, or Obsidian Tasks format

## Usage

Use the diary CLI tool to manage entries:

- Add entries: ` + "`diary add`" + `
- List entries: ` + "`diary list`" + `
- Search entries: ` + "`diary search \"keyword\"`" + `
- Manage todos: ` + "`diary todo`" + `

## Obsidian Integration

These files are designed to work with Obsidian and the Tasks plugin:

- Task format entries use the ` + "`#toProcess`" + ` tag for the Tasks plugin
- Files follow standard markdown format for compatibility
- You can edit files directly in Obsidian or use the CLI tool
`

	return os.WriteFile(readmePath, []byte(readmeContent), 0644)
}

// getCurrentDate returns the current date (using the same function as config)
func getCurrentDate() time.Time {
	return time.Now()
}

