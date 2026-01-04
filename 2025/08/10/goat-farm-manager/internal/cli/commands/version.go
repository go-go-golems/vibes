package commands

import (
	"context"
	"fmt"

	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/go-go-golems/glazed/pkg/middlewares"
	"github.com/go-go-golems/glazed/pkg/types"

	"github.com/farm/goat-manager/ent"
	"github.com/farm/goat-manager/internal/database"
)

type VersionSettings struct {
	Action     string `glazed.parameter:"action"`
	BranchName string `glazed.parameter:"branch"`
	Message    string `glazed.parameter:"message"`
	Limit      int    `glazed.parameter:"limit"`
	Table      string `glazed.parameter:"table"`
	FromCommit string `glazed.parameter:"from-commit"`
	ToCommit   string `glazed.parameter:"to-commit"`
}

type VersionCommand struct {
	*cmds.CommandDescription
}

func NewVersionCommand() *VersionCommand {
	return &VersionCommand{
		CommandDescription: cmds.NewCommandDescription(
			"version",
			cmds.WithShort("Version control operations"),
			cmds.WithLong("Manage branches, commits, and view history using embedded Dolt"),
			cmds.WithLayersList(
				layers.NewParameterLayer(
					"version",
					"Version control parameters",
					parameters.NewParameterDefinition(
						"action",
						parameters.ParameterTypeChoice,
						parameters.WithHelp("Version control action to perform"),
						parameters.WithChoices("commit", "branch", "switch", "log", "diff", "branches", "status", "backup", "restore", "merge", "compare"),
						parameters.WithRequired(true),
					),
					parameters.NewParameterDefinition(
						"branch",
						parameters.ParameterTypeString,
						parameters.WithHelp("Branch name for branch operations"),
					),
					parameters.NewParameterDefinition(
						"message",
						parameters.ParameterTypeString,
						parameters.WithHelp("Commit message"),
					),
					parameters.NewParameterDefinition(
						"limit",
						parameters.ParameterTypeInteger,
						parameters.WithHelp("Limit number of results"),
						parameters.WithDefault(10),
					),
					parameters.NewParameterDefinition(
						"table",
						parameters.ParameterTypeString,
						parameters.WithHelp("Table name for diff operations"),
					),
					parameters.NewParameterDefinition(
						"from-commit",
						parameters.ParameterTypeString,
						parameters.WithHelp("From commit for diff"),
						parameters.WithDefault("HEAD~1"),
					),
					parameters.NewParameterDefinition(
						"to-commit",
						parameters.ParameterTypeString,
						parameters.WithHelp("To commit for diff"),
						parameters.WithDefault("HEAD"),
					),
				),
			),
		),
	}
}

func (c *VersionCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	settings := &VersionSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, settings); err != nil {
		return fmt.Errorf("failed to parse settings: %w", err)
	}

	// Get database client from context
	client, ok := ctx.Value("db_client").(*ent.Client)
	if !ok {
		return fmt.Errorf("database client not found in context")
	}

	switch settings.Action {
	case "commit":
		return c.commitChanges(ctx, client, settings, gp)
	case "branch":
		return c.createBranch(ctx, client, settings, gp)
	case "switch":
		return c.switchBranch(ctx, client, settings, gp)
	case "log":
		return c.showLog(ctx, client, settings, gp)
	case "diff":
		return c.showDiff(ctx, client, settings, gp)
	case "branches":
		return c.listBranches(ctx, client, settings, gp)
	case "status", "backup", "restore", "merge", "compare":
		return c.handleEnhancedActions(ctx, client, settings, gp)
	default:
		return fmt.Errorf("unknown action: %s", settings.Action)
	}
}

func (c *VersionCommand) commitChanges(
	ctx context.Context,
	client *ent.Client,
	settings *VersionSettings,
	gp middlewares.Processor,
) error {
	message := settings.Message
	if message == "" {
		message = "Farm data update"
	}

	if err := database.CommitChanges(ctx, client, message); err != nil {
		return fmt.Errorf("failed to commit changes: %w", err)
	}

	row := types.NewRow(types.MRFromMap(map[string]interface{}{
		"action":  "commit",
		"message": message,
		"status":  "success",
	}))
	return gp.AddRow(ctx, row)
}

func (c *VersionCommand) createBranch(
	ctx context.Context,
	client *ent.Client,
	settings *VersionSettings,
	gp middlewares.Processor,
) error {
	if settings.BranchName == "" {
		return fmt.Errorf("branch name is required")
	}

	if err := database.CreateBranch(ctx, client, settings.BranchName); err != nil {
		return fmt.Errorf("failed to create branch: %w", err)
	}

	row := types.NewRow(types.MRFromMap(map[string]interface{}{
		"action": "branch",
		"branch": settings.BranchName,
		"status": "created",
	}))
	return gp.AddRow(ctx, row)
}

func (c *VersionCommand) switchBranch(
	ctx context.Context,
	client *ent.Client,
	settings *VersionSettings,
	gp middlewares.Processor,
) error {
	if settings.BranchName == "" {
		return fmt.Errorf("branch name is required")
	}

	if err := database.SwitchBranch(ctx, client, settings.BranchName); err != nil {
		return fmt.Errorf("failed to switch branch: %w", err)
	}

	row := types.NewRow(types.MRFromMap(map[string]interface{}{
		"action": "switch",
		"branch": settings.BranchName,
		"status": "switched",
	}))
	return gp.AddRow(ctx, row)
}

func (c *VersionCommand) showLog(
	ctx context.Context,
	client *ent.Client,
	settings *VersionSettings,
	gp middlewares.Processor,
) error {
	commits, err := database.GetCommitHistory(ctx, client, settings.Limit)
	if err != nil {
		return fmt.Errorf("failed to get commit history: %w", err)
	}

	for _, commit := range commits {
		row := types.NewRowFromStruct(&commit, true)
		if err := gp.AddRow(ctx, row); err != nil {
			return fmt.Errorf("failed to add commit row: %w", err)
		}
	}

	return nil
}

func (c *VersionCommand) listBranches(
	ctx context.Context,
	client *ent.Client,
	settings *VersionSettings,
	gp middlewares.Processor,
) error {
	branches, err := database.GetBranches(ctx, client)
	if err != nil {
		return fmt.Errorf("failed to get branches: %w", err)
	}

	for _, branch := range branches {
		row := types.NewRow(types.MRFromMap(map[string]interface{}{
			"branch": branch,
		}))
		if err := gp.AddRow(ctx, row); err != nil {
			return fmt.Errorf("failed to add branch row: %w", err)
		}
	}

	return nil
}

func (c *VersionCommand) showDiff(
	ctx context.Context,
	client *ent.Client,
	settings *VersionSettings,
	gp middlewares.Processor,
) error {
	if settings.Table == "" {
		return fmt.Errorf("table name is required for diff")
	}

	diff, err := database.GetTableDiff(ctx, client, settings.FromCommit, settings.ToCommit, settings.Table)
	if err != nil {
		return fmt.Errorf("failed to get table diff: %w", err)
	}

	for _, row := range diff {
		diffRow := types.NewRow(types.MRFromMap(row))
		if err := gp.AddRow(ctx, diffRow); err != nil {
			return fmt.Errorf("failed to add diff row: %w", err)
		}
	}

	return nil
}



// Enhanced version control methods
func (c *VersionCommand) handleEnhancedActions(
	ctx context.Context,
	client *ent.Client,
	settings *VersionSettings,
	gp middlewares.Processor,
) error {
	wm := database.NewWorkflowManager(client)

	switch settings.Action {
	case "status":
		return c.showBranchStatus(ctx, wm, settings, gp)
	case "backup":
		return c.createBackup(ctx, wm, settings, gp)
	case "restore":
		return c.restoreFromBackup(ctx, wm, settings, gp)
	case "merge":
		return c.mergeBranch(ctx, wm, settings, gp)
	case "compare":
		return c.compareBranches(ctx, wm, settings, gp)
	default:
		return fmt.Errorf("unknown enhanced action: %s", settings.Action)
	}
}

func (c *VersionCommand) showBranchStatus(
	ctx context.Context,
	wm *database.WorkflowManager,
	settings *VersionSettings,
	gp middlewares.Processor,
) error {
	branchName := settings.BranchName
	if branchName == "" {
		branchName = "main" // Default to main branch
	}

	status, err := wm.GetBranchStatus(ctx, branchName)
	if err != nil {
		return fmt.Errorf("failed to get branch status: %w", err)
	}

	row := types.NewRowFromStruct(status, true)
	return gp.AddRow(ctx, row)
}

func (c *VersionCommand) createBackup(
	ctx context.Context,
	wm *database.WorkflowManager,
	settings *VersionSettings,
	gp middlewares.Processor,
) error {
	reason := settings.Message
	if reason == "" {
		reason = "Manual backup"
	}

	backupBranch, err := wm.CreateBackup(ctx, reason)
	if err != nil {
		return fmt.Errorf("failed to create backup: %w", err)
	}

	row := types.NewRow(types.MRFromMap(map[string]interface{}{
		"action":        "backup",
		"backup_branch": backupBranch,
		"reason":        reason,
		"status":        "created",
	}))
	return gp.AddRow(ctx, row)
}

func (c *VersionCommand) restoreFromBackup(
	ctx context.Context,
	wm *database.WorkflowManager,
	settings *VersionSettings,
	gp middlewares.Processor,
) error {
	if settings.BranchName == "" {
		return fmt.Errorf("backup branch name is required for restore")
	}

	if err := wm.RestoreFromBackup(ctx, settings.BranchName); err != nil {
		return fmt.Errorf("failed to restore from backup: %w", err)
	}

	row := types.NewRow(types.MRFromMap(map[string]interface{}{
		"action":        "restore",
		"backup_branch": settings.BranchName,
		"status":        "restored",
	}))
	return gp.AddRow(ctx, row)
}

func (c *VersionCommand) mergeBranch(
	ctx context.Context,
	wm *database.WorkflowManager,
	settings *VersionSettings,
	gp middlewares.Processor,
) error {
	if settings.BranchName == "" {
		return fmt.Errorf("source branch name is required for merge")
	}

	targetBranch := "main" // Default target
	mergeMessage := settings.Message
	if mergeMessage == "" {
		mergeMessage = fmt.Sprintf("Merged branch %s", settings.BranchName)
	}

	if err := wm.MergeBranch(ctx, settings.BranchName, targetBranch, mergeMessage); err != nil {
		return fmt.Errorf("failed to merge branch: %w", err)
	}

	row := types.NewRow(types.MRFromMap(map[string]interface{}{
		"action":        "merge",
		"source_branch": settings.BranchName,
		"target_branch": targetBranch,
		"message":       mergeMessage,
		"status":        "merged",
	}))
	return gp.AddRow(ctx, row)
}

func (c *VersionCommand) compareBranches(
	ctx context.Context,
	wm *database.WorkflowManager,
	settings *VersionSettings,
	gp middlewares.Processor,
) error {
	fromBranch := settings.FromCommit
	if fromBranch == "" {
		fromBranch = "main"
	}

	toBranch := settings.BranchName
	if toBranch == "" {
		return fmt.Errorf("target branch name is required for comparison")
	}

	comparison, err := wm.CompareData(ctx, fromBranch, toBranch)
	if err != nil {
		return fmt.Errorf("failed to compare branches: %w", err)
	}

	// Output comparison summary
	for tableName, tableComp := range comparison.Tables {
		row := types.NewRow(types.MRFromMap(map[string]interface{}{
			"table_name":    tableName,
			"rows_changed":  tableComp.RowsChanged,
			"from_branch":   fromBranch,
			"to_branch":     toBranch,
			"error":         tableComp.Error,
		}))
		if err := gp.AddRow(ctx, row); err != nil {
			return fmt.Errorf("failed to add comparison row: %w", err)
		}
	}

	return nil
}

