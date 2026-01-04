package cmd

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
    "time"
    "os/exec"
    "strings"

	"github.com/go-go-golems/glazed/pkg/cli"
	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/go-go-golems/glazed/pkg/middlewares"
	"github.com/go-go-golems/glazed/pkg/settings"
	"github.com/go-go-golems/glazed/pkg/types"
	"github.com/pkg/errors"
	"github.com/rs/zerolog/log"
    "github.com/charmbracelet/lipgloss"
    "golang.org/x/term"

	utilspkg "github.com/user/git-precommit-guard/internal/utils"
	cfgpkg "github.com/user/git-precommit-guard/pkg/config"
	detectorpkg "github.com/user/git-precommit-guard/pkg/detector"
	gitpkg "github.com/user/git-precommit-guard/pkg/git"
	reporterpkg "github.com/user/git-precommit-guard/pkg/reporter"
)

// CheckCommand implements the dual-mode check command
 type CheckCommand struct {
	*cmds.CommandDescription
}

// CheckSettings holds parsed parameters and arguments
 type CheckSettings struct {
	All      bool     `glazed.parameter:"all"`
	FailFast bool     `glazed.parameter:"fail-fast"`
	Config   string   `glazed.parameter:"config"`
	Verbose  bool     `glazed.parameter:"verbose"`
	JSON     bool     `glazed.parameter:"json"`
    Fix      bool     `glazed.parameter:"fix"`
	Files    []string `glazed.parameter:"files"`
}

// NewCheckCommand constructs the Glazed command description
 func NewCheckCommand() (*CheckCommand, error) {
	glazeLayers, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, errors.Wrap(err, "create glazed parameter layers")
	}

	commandSettingsLayer, err := cli.NewCommandSettingsLayer()
	if err != nil {
		return nil, errors.Wrap(err, "create command settings layer")
	}

	cd := cmds.NewCommandDescription(
		"check",
		cmds.WithShort("Check staged files for undesired content"),
		cmds.WithLong(`Check staged files (or all files with --all) for undesired content such as
ELF binaries, large files, and files with blocked MIME types.

This command is designed to be used as a git pre-commit hook but can also be
run manually to check files.`),
		cmds.WithFlags(
			parameters.NewParameterDefinition("all", parameters.ParameterTypeBool,
				parameters.WithDefault(false), parameters.WithHelp("check all files instead of just staged files"), parameters.WithShortFlag("a")),
			parameters.NewParameterDefinition("fail-fast", parameters.ParameterTypeBool,
				parameters.WithDefault(false), parameters.WithHelp("stop on first failure (overrides config)")),
			parameters.NewParameterDefinition("config", parameters.ParameterTypeString,
				parameters.WithDefault(""), parameters.WithHelp("config file (default is .precommit-guard.yml)"), parameters.WithShortFlag("c")),
			parameters.NewParameterDefinition("verbose", parameters.ParameterTypeBool,
				parameters.WithDefault(false), parameters.WithHelp("verbose output"), parameters.WithShortFlag("v")),
			parameters.NewParameterDefinition("json", parameters.ParameterTypeBool,
				parameters.WithDefault(false), parameters.WithHelp("output results in JSON format"), parameters.WithShortFlag("j")),
            parameters.NewParameterDefinition("fix", parameters.ParameterTypeBool,
                parameters.WithDefault(false), parameters.WithHelp("unstage failing files")),
		),
		cmds.WithArguments(
			parameters.NewParameterDefinition(
				"files",
				parameters.ParameterTypeStringList,
				parameters.WithHelp("Files to check when using --all"),
			),
		),
		cmds.WithLayersList(glazeLayers, commandSettingsLayer),
	)

	return &CheckCommand{CommandDescription: cd}, nil
}

// Run is the human-readable mode
 func (c *CheckCommand) Run(ctx context.Context, pl *layers.ParsedLayers) error {
	settings := &CheckSettings{}
	if err := pl.InitializeStruct(layers.DefaultSlug, settings); err != nil {
		return errors.Wrap(err, "parse settings")
	}

	cfg, err := cfgpkg.LoadConfig(settings.Config)
	if err != nil {
		return errors.Wrap(err, "load configuration")
	}
	if settings.FailFast {
		cfg.Settings.FailFast = true
	}
	if settings.JSON {
		cfg.Reporting.Format = "json"
	}

	if !settings.All && !gitpkg.IsGitRepository() {
		return errors.New("not in a git repository. Use --all to check files without git")
	}

	var filesToCheck []string
	if settings.All {
		if len(settings.Files) == 0 {
			return errors.New("when using --all, specify files to check as arguments")
		}
		filesToCheck = settings.Files
	} else {
		stagedFiles, err := gitpkg.GetStagedFilePaths()
		if err != nil {
			return errors.Wrap(err, "get staged files")
		}
		repoRoot, err := gitpkg.GetRepositoryRoot()
		if err != nil {
			return errors.Wrap(err, "get repository root")
		}
		log.Debug().Str("repo_root", repoRoot).Int("staged_count", len(stagedFiles)).Msg("fetched staged files")
		if len(stagedFiles) == 0 {
			if settings.Verbose {
				fmt.Println("No staged files to check")
			}
			return nil
		}
		resolved := make([]string, 0, len(stagedFiles))
		for _, p := range stagedFiles {
			rp := p
			if !filepath.IsAbs(p) {
				rp = filepath.Join(repoRoot, p)
			}
			log.Debug().Str("rel", p).Str("abs", rp).Msg("resolved staged file path")
			resolved = append(resolved, rp)
		}
		filesToCheck = resolved
	}

	dm := detectorpkg.NewDetectorManager(cfg)
	rep := reporterpkg.NewReporter(cfg.Reporting, settings.Verbose)

	// Setup spinner if running in a dynamic TTY and console output
	showSpinner := term.IsTerminal(int(os.Stdout.Fd())) && cfg.Reporting.Format == "console" && !settings.Verbose
	type progressUpdate struct {
		index int
		path  string
	}
	var (
		progressCh chan progressUpdate
		doneCh     chan struct{}
	)
	if showSpinner && len(filesToCheck) > 0 {
		progressCh = make(chan progressUpdate, 1)
		doneCh = make(chan struct{})
		go func(total int) {
			frames := []string{"⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"}
			style := lipgloss.NewStyle().Foreground(lipgloss.Color("205")).Bold(true)
			ticker := time.NewTicker(80 * time.Millisecond)
			defer ticker.Stop()
			frameIdx := 0
			var last progressUpdate
			for {
				select {
				case u := <-progressCh:
					last = u
				case <-ticker.C:
					line := fmt.Sprintf("%s %d/%d %s", frames[frameIdx%len(frames)], last.index, total, last.path)
					frameIdx++
					fmt.Printf("\r\x1b[2K%s", style.Render(line))
				case <-doneCh:
					fmt.Print("\r\x1b[2K")
					return
				}
			}
		}(len(filesToCheck))
	}

	ctxTimeout, cancel := context.WithTimeout(ctx, cfg.Settings.Timeout)
	defer cancel()

	var allResults []*detectorpkg.DetectionResult
	var hasFailures bool

	for i, filePath := range filesToCheck {
		select {
		case <-ctxTimeout.Done():
			return errors.Errorf("operation timed out after %v", cfg.Settings.Timeout)
		default:
		}

		if showSpinner {
			select {
			case progressCh <- progressUpdate{index: i + 1, path: filePath}:
			default:
			}
		}

		info, err := os.Stat(filePath)
		if os.IsNotExist(err) {
			log.Debug().Str("path", filePath).Msg("skipping non-existent file")
			if settings.Verbose {
				fmt.Printf("Skipping non-existent file: %s\n", filePath)
			}
			continue
		}
		if err != nil {
			log.Debug().Str("path", filePath).Err(err).Msg("stat error")
		}

		fi, err := utilspkg.GetFileInfo(filePath)
		if err != nil {
			log.Debug().Str("path", filePath).Err(err).Msg("GetFileInfo error")
			if settings.Verbose {
				fmt.Printf("Warning: Failed to get info for %s: %v\n", filePath, err)
			}
			continue
		}
		log.Debug().Str("path", filePath).Int64("size", info.Size()).Str("mime", fi.MimeType).Msg("file info")

		results, err := dm.CheckFile(fi)
		if err != nil {
			return errors.Wrapf(err, "check file %s", filePath)
		}

		allResults = append(allResults, results...)
		for _, r := range results {
			log.Debug().Str("path", r.FilePath).Str("rule", r.RuleName).Str("severity", r.Severity).Bool("passed", r.Passed).Msg("detection result")
			if !r.Passed && r.Severity == "error" {
				hasFailures = true
				if cfg.Settings.FailFast {
					break
				}
			}
		}
		if hasFailures && cfg.Settings.FailFast {
			break
		}
	}

	if showSpinner {
		close(doneCh)
	}
	if err := rep.Report(allResults); err != nil {
		return errors.Wrap(err, "generate report")
	}
	log.Debug().Bool("has_failures", hasFailures).Bool("fix", settings.Fix).Msg("check results")
	if hasFailures {
        if settings.Fix {
            log.Debug().Msg("fixing failing files")
            // Unstage failing files
            toUnstage := make(map[string]struct{})
            for _, r := range allResults {
                if !r.Passed && r.Severity == "error" {
                    toUnstage[r.FilePath] = struct{}{}
                }
            }
            if len(toUnstage) > 0 {
                repoRoot, err := gitpkg.GetRepositoryRoot()
                if err == nil {
                    var rels []string
                    for p := range toUnstage {
                        // Convert to relative path for git reset
                        rel := p
                        if strings.HasPrefix(p, repoRoot+string(os.PathSeparator)) {
                            rel = p[len(repoRoot)+1:]
                        }
                        cmd := exec.Command("git", "reset", "-q", "HEAD", "--", rel)
                        cmd.Dir = repoRoot
                        if runErr := cmd.Run(); runErr == nil {
                            rels = append(rels, rel)
                        } else {
                            log.Debug().Str("path", rel).Err(runErr).Msg("git reset failed")
                        }
                    }
                    if len(rels) > 0 {
                        if settings.Verbose {
                            fmt.Println("Unstaged failing files (--fix):")
                            for _, r := range rels { fmt.Printf("  %s\n", r) }
                        } else {
                            fmt.Printf("Unstaged %d failing file(s) (--fix)\n", len(rels))
                        }
                    }
                }
            }
        }
		return errors.New("check failed")
	}
	return nil
}

// RunIntoGlazeProcessor is the structured output mode
 func (c *CheckCommand) RunIntoGlazeProcessor(ctx context.Context, pl *layers.ParsedLayers, gp middlewares.Processor) error {
	settings := &CheckSettings{}
	if err := pl.InitializeStruct(layers.DefaultSlug, settings); err != nil {
		return errors.Wrap(err, "parse settings")
	}

	cfg, err := cfgpkg.LoadConfig(settings.Config)
	if err != nil {
		return errors.Wrap(err, "load configuration")
	}
	if settings.FailFast {
		cfg.Settings.FailFast = true
	}

	if !settings.All && !gitpkg.IsGitRepository() {
		return errors.New("not in a git repository. Use --all to check files without git")
	}

	var filesToCheck []string
	if settings.All {
		if len(settings.Files) == 0 {
			return errors.New("when using --all, specify files to check as arguments")
		}
		filesToCheck = settings.Files
	} else {
		stagedFiles, err := gitpkg.GetStagedFilePaths()
		if err != nil {
			return errors.Wrap(err, "get staged files")
		}
		repoRoot, err := gitpkg.GetRepositoryRoot()
		if err != nil {
			return errors.Wrap(err, "get repository root")
		}
		log.Debug().Str("repo_root", repoRoot).Int("staged_count", len(stagedFiles)).Msg("fetched staged files")
		if len(stagedFiles) == 0 {
			return nil
		}
		resolved := make([]string, 0, len(stagedFiles))
		for _, p := range stagedFiles {
			rp := p
			if !filepath.IsAbs(p) {
				rp = filepath.Join(repoRoot, p)
			}
			log.Debug().Str("rel", p).Str("abs", rp).Msg("resolved staged file path")
			resolved = append(resolved, rp)
		}
		filesToCheck = resolved
	}

	dm := detectorpkg.NewDetectorManager(cfg)
	ctxTimeout, cancel := context.WithTimeout(ctx, cfg.Settings.Timeout)
	defer cancel()

	var hasFailures bool
    toUnstage := make(map[string]struct{})

	for _, filePath := range filesToCheck {
		select {
		case <-ctxTimeout.Done():
			return errors.Errorf("operation timed out after %v", cfg.Settings.Timeout)
		default:
		}

		if _, err := os.Stat(filePath); os.IsNotExist(err) {
			log.Debug().Str("path", filePath).Msg("skipping non-existent file")
			continue
		}

		fi, err := utilspkg.GetFileInfo(filePath)
		if err != nil {
			log.Debug().Str("path", filePath).Err(err).Msg("GetFileInfo error")
			continue
		}
		log.Debug().Str("path", filePath).Str("mime", fi.MimeType).Msg("file info")

		results, err := dm.CheckFile(fi)
		if err != nil {
			return errors.Wrapf(err, "check file %s", filePath)
		}

		for _, r := range results {
			row := types.NewRow(
				types.MRP("file_path", r.FilePath),
				types.MRP("rule_name", r.RuleName),
				types.MRP("severity", r.Severity),
				types.MRP("passed", r.Passed),
				types.MRP("message", r.Message),
			)
			for k, v := range r.Details {
				row.Set(k, v)
			}
            if settings.Fix && !r.Passed && r.Severity == "error" {
                row.Set("fix_action", "unstaged")
                row.Set("fixed", true)
                toUnstage[r.FilePath] = struct{}{}
            }
			if err := gp.AddRow(ctx, row); err != nil {
				return errors.Wrap(err, "add row")
			}
			if !r.Passed && r.Severity == "error" {
				log.Debug().Str("path", r.FilePath).Msg("error result detected")
				hasFailures = true
			}
		}
		if hasFailures && cfg.Settings.FailFast {
			break
		}
	}

    if settings.Fix && len(toUnstage) > 0 {
        repoRoot, err := gitpkg.GetRepositoryRoot()
        if err == nil {
            for p := range toUnstage {
                rel := p
                if strings.HasPrefix(p, repoRoot+string(os.PathSeparator)) {
                    rel = p[len(repoRoot)+1:]
                }
                cmd := exec.Command("git", "reset", "-q", "HEAD", "--", rel)
                cmd.Dir = repoRoot
                _ = cmd.Run()
            }
        }
    }

	if hasFailures {
		return errors.New("check failed")
	}
	return nil
}

var _ cmds.BareCommand = &CheckCommand{}
var _ cmds.GlazeCommand = &CheckCommand{}

