package pdfjobs

import (
	"context"
	"fmt"
	"strconv"
	"strings"

	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/go-go-golems/glazed/pkg/middlewares"
	"github.com/go-go-golems/glazed/pkg/settings"
	glazed_types "github.com/go-go-golems/glazed/pkg/types"
	"github.com/rs/zerolog"
	"photobook-backend-go/internal/config"
	"photobook-backend-go/internal/db"
	"photobook-backend-go/internal/pdfjobs"
	"photobook-backend-go/internal/photos"
	"photobook-backend-go/internal/storage"
)

// CreateJobCommand creates a new PDF job
type CreateJobCommand struct {
	*cmds.CommandDescription
}

type CreateJobSettings struct {
	UserID   int64  `glazed.parameter:"user-id"`
	PhotoIDs string `glazed.parameter:"photo-ids"` // comma-separated list
}

func (c *CreateJobCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	settings := &CreateJobSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, settings); err != nil {
		return err
	}

	if settings.UserID == 0 {
		return fmt.Errorf("--user-id is required")
	}
	if settings.PhotoIDs == "" {
		return fmt.Errorf("--photo-ids is required (comma-separated list)")
	}

	// Parse photo IDs
	parts := strings.Split(settings.PhotoIDs, ",")
	var photoIDs []int64
	for _, part := range parts {
		part = strings.TrimSpace(part)
		if part == "" {
			continue
		}
		id, err := strconv.ParseInt(part, 10, 64)
		if err != nil {
			return fmt.Errorf("invalid photo ID '%s': %w", part, err)
		}
		photoIDs = append(photoIDs, id)
	}

	if len(photoIDs) == 0 {
		return fmt.Errorf("no valid photo IDs provided")
	}

	cfg, err := config.LoadConfig()
	if err != nil {
		return fmt.Errorf("failed to load config: %w", err)
	}

	database, err := db.OpenDB(cfg.DatabaseURL)
	if err != nil {
		return fmt.Errorf("failed to open database: %w", err)
	}
	defer database.Close()

	repo := pdfjobs.NewSQLiteRepository(database)
	jobID, err := repo.Create(ctx, settings.UserID, photoIDs)
	if err != nil {
		return fmt.Errorf("failed to create PDF job: %w", err)
	}

	row := glazed_types.NewRow(
		glazed_types.MRP("job_id", jobID),
		glazed_types.MRP("user_id", settings.UserID),
		glazed_types.MRP("photo_ids", settings.PhotoIDs),
		glazed_types.MRP("status", "pending"),
		glazed_types.MRP("success", true),
	)

	return gp.AddRow(ctx, row)
}

func NewCreateJobCommand() (*CreateJobCommand, error) {
	glazedLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, err
	}

	cmdDesc := cmds.NewCommandDescription(
		"create",
		cmds.WithShort("Create a new PDF generation job"),
		cmds.WithLong("Creates a new PDF generation job for the specified user and photos"),
		cmds.WithFlags(
			parameters.NewParameterDefinition(
				"user-id",
				parameters.ParameterTypeInteger,
				parameters.WithRequired(true),
				parameters.WithHelp("User ID"),
			),
			parameters.NewParameterDefinition(
				"photo-ids",
				parameters.ParameterTypeString,
				parameters.WithRequired(true),
				parameters.WithHelp("Comma-separated list of photo IDs"),
			),
		),
		cmds.WithLayersList(glazedLayer),
	)

	return &CreateJobCommand{
		CommandDescription: cmdDesc,
	}, nil
}

// ListJobsCommand lists PDF jobs for a user
type ListJobsCommand struct {
	*cmds.CommandDescription
}

type ListJobsSettings struct {
	UserID int64 `glazed.parameter:"user-id"`
}

func (c *ListJobsCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	settings := &ListJobsSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, settings); err != nil {
		return err
	}

	if settings.UserID == 0 {
		return fmt.Errorf("--user-id is required")
	}

	cfg, err := config.LoadConfig()
	if err != nil {
		return fmt.Errorf("failed to load config: %w", err)
	}

	database, err := db.OpenDB(cfg.DatabaseURL)
	if err != nil {
		return fmt.Errorf("failed to open database: %w", err)
	}
	defer database.Close()

	repo := pdfjobs.NewSQLiteRepository(database)
	jobs, err := repo.GetByUserID(ctx, settings.UserID)
	if err != nil {
		return fmt.Errorf("failed to list PDF jobs: %w", err)
	}

	for _, job := range jobs {
		// Convert photo IDs to string
		photoIDsStr := ""
		if len(job.PhotoIDs) > 0 {
			parts := make([]string, len(job.PhotoIDs))
			for i, id := range job.PhotoIDs {
				parts[i] = fmt.Sprintf("%d", id)
			}
			photoIDsStr = strings.Join(parts, ",")
		}

		row := glazed_types.NewRow(
			glazed_types.MRP("id", job.ID),
			glazed_types.MRP("user_id", job.UserID),
			glazed_types.MRP("status", job.Status),
			glazed_types.MRP("photo_ids", photoIDsStr),
			glazed_types.MRP("file_key", job.FileKey),
			glazed_types.MRP("url", job.URL),
			glazed_types.MRP("error", job.Error),
			glazed_types.MRP("created_at", job.CreatedAt),
			glazed_types.MRP("updated_at", job.UpdatedAt),
		)

		if err := gp.AddRow(ctx, row); err != nil {
			return err
		}
	}

	return nil
}

func NewListJobsCommand() (*ListJobsCommand, error) {
	glazedLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, err
	}

	cmdDesc := cmds.NewCommandDescription(
		"list",
		cmds.WithShort("List PDF jobs for a user"),
		cmds.WithLong("Lists all PDF generation jobs for the specified user"),
		cmds.WithFlags(
			parameters.NewParameterDefinition(
				"user-id",
				parameters.ParameterTypeInteger,
				parameters.WithRequired(true),
				parameters.WithHelp("User ID"),
			),
		),
		cmds.WithLayersList(glazedLayer),
	)

	return &ListJobsCommand{
		CommandDescription: cmdDesc,
	}, nil
}

// ProcessJobsCommand manually triggers the worker to process pending jobs
type ProcessJobsCommand struct {
	*cmds.CommandDescription
}

type ProcessJobsSettings struct {
	Limit int `glazed.parameter:"limit"`
}

func (c *ProcessJobsCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	settings := &ProcessJobsSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, settings); err != nil {
		return err
	}

	if settings.Limit <= 0 {
		settings.Limit = 5
	}

	cfg, err := config.LoadConfig()
	if err != nil {
		return fmt.Errorf("failed to load config: %w", err)
	}

	database, err := db.OpenDB(cfg.DatabaseURL)
	if err != nil {
		return fmt.Errorf("failed to open database: %w", err)
	}
	defer database.Close()

	repo := pdfjobs.NewSQLiteRepository(database)
	photoRepo := photos.NewSQLiteRepository(database)

	storage, err := storage.NewDiskStorage(cfg.StoragePath, cfg.BaseURL)
	if err != nil {
		return fmt.Errorf("failed to create storage: %w", err)
	}

	logger := zerolog.Nop()
	worker := pdfjobs.NewWorker(repo, photoRepo, storage, logger)

	// Claim and process jobs
	jobs, err := repo.ClaimPendingJobs(ctx, settings.Limit)
	if err != nil {
		return fmt.Errorf("failed to claim jobs: %w", err)
	}

	if len(jobs) == 0 {
		row := glazed_types.NewRow(
			glazed_types.MRP("message", "no pending jobs found"),
			glazed_types.MRP("processed", 0),
		)
		return gp.AddRow(ctx, row)
	}

	processed := 0
	failed := 0
	for _, job := range jobs {
		if err := worker.ProcessJob(ctx, job); err != nil {
			failed++
			row := glazed_types.NewRow(
				glazed_types.MRP("job_id", job.ID),
				glazed_types.MRP("status", "failed"),
				glazed_types.MRP("error", err.Error()),
			)
			if err := gp.AddRow(ctx, row); err != nil {
				return err
			}
		} else {
			processed++
			row := glazed_types.NewRow(
				glazed_types.MRP("job_id", job.ID),
				glazed_types.MRP("status", "completed"),
			)
			if err := gp.AddRow(ctx, row); err != nil {
				return err
			}
		}
	}

	summary := glazed_types.NewRow(
		glazed_types.MRP("total", len(jobs)),
		glazed_types.MRP("processed", processed),
		glazed_types.MRP("failed", failed),
	)
	return gp.AddRow(ctx, summary)
}

func NewProcessJobsCommand() (*ProcessJobsCommand, error) {
	glazedLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, err
	}

	cmdDesc := cmds.NewCommandDescription(
		"process",
		cmds.WithShort("Manually trigger PDF job processing"),
		cmds.WithLong("Manually triggers the worker to process pending PDF jobs"),
		cmds.WithFlags(
			parameters.NewParameterDefinition(
				"limit",
				parameters.ParameterTypeInteger,
				parameters.WithDefault(5),
				parameters.WithHelp("Maximum number of jobs to process"),
			),
		),
		cmds.WithLayersList(glazedLayer),
	)

	return &ProcessJobsCommand{
		CommandDescription: cmdDesc,
	}, nil
}

var _ cmds.GlazeCommand = &CreateJobCommand{}
var _ cmds.GlazeCommand = &ListJobsCommand{}
var _ cmds.GlazeCommand = &ProcessJobsCommand{}

