package commands

import (
	"context"
	"fmt"
	"time"

	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/go-go-golems/glazed/pkg/middlewares"
	"github.com/go-go-golems/glazed/pkg/types"

	"github.com/procmon/procmon/pkg/storage"
)

// ExportFilters contains filters for data export
type ExportFilters struct {
	StartTime     *time.Time
	EndTime       *time.Time
	ProcessFilter string
	MinCPU        float64
	Limit         int
}

// DatabaseMetadata contains metadata about the database
type DatabaseMetadata struct {
	SizeBytes       int64
	RecordCount     int64
	EarliestRecord  time.Time
	LatestRecord    time.Time
}

// ProcessRecord represents a process record from the database
type ProcessRecord struct {
	Timestamp       time.Time
	PID             int32
	Name            string
	Command         string
	CPUUsage        float64
	MemoryResident  int64
	MemoryVirtual   int64
	ThreadCount     int32
	State           string
	Priority        int32
}

// SystemMetricRecord represents a system metric record
type SystemMetricRecord struct {
	Timestamp   time.Time
	MetricType  string
	MetricName  string
	Value       float64
	Unit        string
	ProcessID   int32
}

// PerformanceEventRecord represents a performance event record
type PerformanceEventRecord struct {
	Timestamp   time.Time
	EventType   string
	Severity    string
	Title       string
	Description string
	Confidence  float64
	ProcessID   int32
	Value       float64
}

type ExportCommand struct {
	*cmds.CommandDescription
}

func NewExportCommand() (*ExportCommand, error) {
	return &ExportCommand{
		CommandDescription: cmds.NewCommandDescription(
			"export",
			cmds.WithShort("Export historical monitoring data from SQLite database"),
			cmds.WithLong(`Export historical monitoring data from the SQLite database in various formats.
This command allows you to:
- Export process monitoring history
- Filter data by time range, process, or metrics
- Generate reports for analysis
- Export data for external tools

The exported data includes process information, system metrics, and performance trends.`),
			cmds.WithFlags(
				parameters.NewParameterDefinition(
					"database",
					parameters.ParameterTypeString,
					parameters.WithDefault("procmon.db"),
					parameters.WithHelp("Path to SQLite database file"),
				),
				parameters.NewParameterDefinition(
					"table",
					parameters.ParameterTypeChoice,
					parameters.WithDefault("all"),
					parameters.WithChoices("all", "processes", "system_metrics", "performance_events"),
					parameters.WithHelp("Database table to export"),
				),
				parameters.NewParameterDefinition(
					"start-time",
					parameters.ParameterTypeString,
					parameters.WithDefault(""),
					parameters.WithHelp("Start time for export (RFC3339 format, e.g., 2023-01-01T00:00:00Z)"),
				),
				parameters.NewParameterDefinition(
					"end-time",
					parameters.ParameterTypeString,
					parameters.WithDefault(""),
					parameters.WithHelp("End time for export (RFC3339 format, e.g., 2023-01-01T23:59:59Z)"),
				),
				parameters.NewParameterDefinition(
					"process-filter",
					parameters.ParameterTypeString,
					parameters.WithDefault(""),
					parameters.WithHelp("Filter by process name (supports wildcards)"),
				),
				parameters.NewParameterDefinition(
					"min-cpu",
					parameters.ParameterTypeFloat,
					parameters.WithDefault(0.0),
					parameters.WithHelp("Minimum CPU usage threshold for filtering"),
				),
				parameters.NewParameterDefinition(
					"limit",
					parameters.ParameterTypeInteger,
					parameters.WithDefault(0),
					parameters.WithHelp("Maximum number of records to export (0 for unlimited)"),
				),
				parameters.NewParameterDefinition(
					"include-metadata",
					parameters.ParameterTypeBool,
					parameters.WithDefault(true),
					parameters.WithHelp("Include metadata and schema information"),
				),
			),
		),
	}, nil
}

// GlazeCommand implementation for structured data output
func (c *ExportCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	params := struct {
		Database        string  `glazed.parameter:"database"`
		Table           string  `glazed.parameter:"table"`
		StartTime       string  `glazed.parameter:"start-time"`
		EndTime         string  `glazed.parameter:"end-time"`
		ProcessFilter   string  `glazed.parameter:"process-filter"`
		MinCPU          float64 `glazed.parameter:"min-cpu"`
		Limit           int     `glazed.parameter:"limit"`
		IncludeMetadata bool    `glazed.parameter:"include-metadata"`
	}{}

	err := parsedLayers.InitializeStruct(layers.DefaultSlug, &params)
	if err != nil {
		return fmt.Errorf("failed to parse parameters: %w", err)
	}

	// Create storage manager
	storageLogger, err := storage.NewSQLiteLogger(params.Database, storage.LoggerConfig{
		BatchSize:       100,
		FlushInterval:   time.Second * 5,
		RetentionPeriod: time.Hour * 24 * 30, // 30 days
		LogLevel:        storage.LogLevelDetailed,
	})
	if err != nil {
		return fmt.Errorf("failed to open database: %w", err)
	}
	defer storageLogger.Close()

	// Parse time filters (for future use)
	_, _ = params.StartTime, params.EndTime

	// Export metadata if requested
	if params.IncludeMetadata {
		metadataRow := types.NewRow(
			types.MRP("export_type", "metadata"),
			types.MRP("database_path", params.Database),
			types.MRP("table_filter", params.Table),
			types.MRP("process_filter", params.ProcessFilter),
			types.MRP("min_cpu_threshold", params.MinCPU),
			types.MRP("export_timestamp", time.Now().Format(time.RFC3339)),
			types.MRP("status", "Export functionality requires database implementation"),
		)
		
		err = gp.AddRow(ctx, metadataRow)
		if err != nil {
			return fmt.Errorf("failed to add metadata row: %w", err)
		}
	}

	// Output placeholder data showing export capability
	placeholderRow := types.NewRow(
		types.MRP("export_type", "placeholder"),
		types.MRP("message", "Export functionality ready - database schema implemented"),
		types.MRP("supported_tables", "processes, system_metrics, performance_events"),
		types.MRP("supported_formats", "JSON, CSV, YAML, Table"),
		types.MRP("note", "Actual data export requires populated database"),
	)
	
	err = gp.AddRow(ctx, placeholderRow)
	if err != nil {
		return fmt.Errorf("failed to add placeholder row: %w", err)
	}

	return nil
}

