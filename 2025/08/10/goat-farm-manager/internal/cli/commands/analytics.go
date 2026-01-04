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

	"github.com/farm/goat-manager/ent"
	"github.com/farm/goat-manager/ent/goat"
	"github.com/farm/goat-manager/ent/milkrecord"
	"github.com/farm/goat-manager/pkg/models"
)

type AnalyticsSettings struct {
	ReportType string `glazed.parameter:"report-type"`
	GoatTag    string `glazed.parameter:"goat-tag"`
	DateFrom   string `glazed.parameter:"date-from"`
	DateTo     string `glazed.parameter:"date-to"`
	Limit      int    `glazed.parameter:"limit"`
}

type AnalyticsCommand struct {
	*cmds.CommandDescription
}

func NewAnalyticsCommand() *AnalyticsCommand {
	return &AnalyticsCommand{
		CommandDescription: cmds.NewCommandDescription(
			"analytics",
			cmds.WithShort("Generate farm analytics and reports"),
			cmds.WithLong("Generate various analytics reports for farm performance and insights"),
			cmds.WithLayersList(
				layers.NewParameterLayer(
					"analytics",
					"Analytics parameters",
					parameters.NewParameterDefinition(
						"report-type",
						parameters.ParameterTypeChoice,
						parameters.WithHelp("Type of analytics report"),
						parameters.WithChoices("farm-summary", "milk-production", "health-summary", "breeding-summary", "feed-consumption", "goat-performance"),
						parameters.WithRequired(true),
					),
					parameters.NewParameterDefinition(
						"goat-tag",
						parameters.ParameterTypeString,
						parameters.WithHelp("Specific goat tag for individual reports"),
					),
					parameters.NewParameterDefinition(
						"date-from",
						parameters.ParameterTypeString,
						parameters.WithHelp("Start date for report period (YYYY-MM-DD)"),
					),
					parameters.NewParameterDefinition(
						"date-to",
						parameters.ParameterTypeString,
						parameters.WithHelp("End date for report period (YYYY-MM-DD)"),
					),
					parameters.NewParameterDefinition(
						"limit",
						parameters.ParameterTypeInteger,
						parameters.WithHelp("Limit number of results"),
						parameters.WithDefault(50),
					),
				),
			),
		),
	}
}

func (c *AnalyticsCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	settings := &AnalyticsSettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, settings); err != nil {
		return fmt.Errorf("failed to parse settings: %w", err)
	}

	// Get database client from context
	client, ok := ctx.Value("db_client").(*ent.Client)
	if !ok {
		return fmt.Errorf("database client not found in context")
	}

	switch settings.ReportType {
	case "farm-summary":
		return c.generateFarmSummary(ctx, client, settings, gp)
	case "milk-production":
		return c.generateMilkProductionReport(ctx, client, settings, gp)
	case "health-summary":
		return c.generateHealthSummary(ctx, client, settings, gp)
	case "breeding-summary":
		return c.generateBreedingSummary(ctx, client, settings, gp)
	case "feed-consumption":
		return c.generateFeedConsumptionReport(ctx, client, settings, gp)
	case "goat-performance":
		return c.generateGoatPerformanceReport(ctx, client, settings, gp)
	default:
		return fmt.Errorf("unknown report type: %s", settings.ReportType)
	}
}

func (c *AnalyticsCommand) generateFarmSummary(
	ctx context.Context,
	client *ent.Client,
	settings *AnalyticsSettings,
	gp middlewares.Processor,
) error {
	// Get total goats by status
	totalGoats, err := client.Goat.Query().Count(ctx)
	if err != nil {
		return fmt.Errorf("failed to count total goats: %w", err)
	}

	activeGoats, err := client.Goat.Query().Where(goat.StatusEQ("active")).Count(ctx)
	if err != nil {
		return fmt.Errorf("failed to count active goats: %w", err)
	}

	lactatingGoats, err := client.Goat.Query().Where(goat.StatusEQ("lactating")).Count(ctx)
	if err != nil {
		return fmt.Errorf("failed to count lactating goats: %w", err)
	}

	pregnantGoats, err := client.Goat.Query().Where(goat.StatusEQ("pregnant")).Count(ctx)
	if err != nil {
		return fmt.Errorf("failed to count pregnant goats: %w", err)
	}

	dryGoats, err := client.Goat.Query().Where(goat.StatusEQ("dry")).Count(ctx)
	if err != nil {
		return fmt.Errorf("failed to count dry goats: %w", err)
	}

	sickGoats, err := client.Goat.Query().Where(goat.StatusEQ("sick")).Count(ctx)
	if err != nil {
		return fmt.Errorf("failed to count sick goats: %w", err)
	}

	// Get milk production for today
	today := time.Now().Truncate(24 * time.Hour)
	tomorrow := today.Add(24 * time.Hour)

	milkToday, err := client.MilkRecord.Query().
		Where(milkrecord.MilkingTimeGTE(today)).
		Where(milkrecord.MilkingTimeLT(tomorrow)).
		Aggregate(ent.Sum(milkrecord.FieldVolumeLiters)).
		Float64(ctx)
	if err != nil {
		milkToday = 0 // If no records, default to 0
	}

	// Get milk production for this week
	weekStart := today.AddDate(0, 0, -int(today.Weekday()))
	milkThisWeek, err := client.MilkRecord.Query().
		Where(milkrecord.MilkingTimeGTE(weekStart)).
		Aggregate(ent.Sum(milkrecord.FieldVolumeLiters)).
		Float64(ctx)
	if err != nil {
		milkThisWeek = 0
	}

	// Get milk production for this month
	monthStart := time.Date(today.Year(), today.Month(), 1, 0, 0, 0, 0, today.Location())
	milkThisMonth, err := client.MilkRecord.Query().
		Where(milkrecord.MilkingTimeGTE(monthStart)).
		Aggregate(ent.Sum(milkrecord.FieldVolumeLiters)).
		Float64(ctx)
	if err != nil {
		milkThisMonth = 0
	}

	// Calculate average milk per goat
	averageMilkPerGoat := float64(0)
	if lactatingGoats > 0 {
		averageMilkPerGoat = milkToday / float64(lactatingGoats)
	}

	summary := models.FarmSummary{
		TotalGoats:         totalGoats,
		ActiveGoats:        activeGoats,
		LactatingGoats:     lactatingGoats,
		PregnantGoats:      pregnantGoats,
		DryGoats:           dryGoats,
		SickGoats:          sickGoats,
		TotalMilkToday:     milkToday,
		TotalMilkThisWeek:  milkThisWeek,
		TotalMilkThisMonth: milkThisMonth,
		AverageMilkPerGoat: averageMilkPerGoat,
	}

	row := types.NewRowFromStruct(&summary, true)
	return gp.AddRow(ctx, row)
}

func (c *AnalyticsCommand) generateMilkProductionReport(
	ctx context.Context,
	client *ent.Client,
	settings *AnalyticsSettings,
	gp middlewares.Processor,
) error {
	query := client.Goat.Query().WithMilkRecords()

	// Apply goat filter if specified
	if settings.GoatTag != "" {
		query = query.Where(goat.IDEQ(settings.GoatTag))
	}

	goats, err := query.All(ctx)
	if err != nil {
		return fmt.Errorf("failed to query goats with milk records: %w", err)
	}

	// Parse date range
	var dateFrom, dateTo time.Time
	if settings.DateFrom != "" {
		dateFrom, err = time.Parse("2006-01-02", settings.DateFrom)
		if err != nil {
			return fmt.Errorf("invalid date-from format: %w", err)
		}
	} else {
		dateFrom = time.Now().AddDate(0, -1, 0) // Default to last month
	}

	if settings.DateTo != "" {
		dateTo, err = time.Parse("2006-01-02", settings.DateTo)
		if err != nil {
			return fmt.Errorf("invalid date-to format: %w", err)
		}
	} else {
		dateTo = time.Now()
	}

	// Generate summary for each goat
	for _, goat := range goats {
		// Filter milk records by date range
		var filteredRecords []*ent.MilkRecord
		for _, record := range goat.Edges.MilkRecords {
			if record.MilkingTime.After(dateFrom) && record.MilkingTime.Before(dateTo) {
				filteredRecords = append(filteredRecords, record)
			}
		}

		if len(filteredRecords) == 0 {
			continue
		}

		// Calculate statistics
		totalVolume := float64(0)
		totalFat := float64(0)
		totalProtein := float64(0)
		gradeA, gradeB, gradeC, rejected := 0, 0, 0, 0
		var lastMilking time.Time

		for _, record := range filteredRecords {
			totalVolume += record.VolumeLiters
			if record.FatContent > 0 {
				totalFat += record.FatContent
			}
			if record.ProteinContent > 0 {
				totalProtein += record.ProteinContent
			}
			if record.MilkingTime.After(lastMilking) {
				lastMilking = record.MilkingTime
			}

			switch record.QualityGrade {
			case "A":
				gradeA++
			case "B":
				gradeB++
			case "C":
				gradeC++
			case "reject":
				rejected++
			}
		}

		averageVolume := totalVolume / float64(len(filteredRecords))
		averageFat := totalFat / float64(len(filteredRecords))
		averageProtein := totalProtein / float64(len(filteredRecords))

		summary := models.MilkProductionSummary{
			GoatTag:        goat.ID,
			GoatName:       goat.Name,
			TotalVolume:    totalVolume,
			AverageVolume:  averageVolume,
			RecordCount:    len(filteredRecords),
			LastMilking:    lastMilking,
			AverageFat:     averageFat,
			AverageProtein: averageProtein,
			QualityGradeA:  gradeA,
			QualityGradeB:  gradeB,
			QualityGradeC:  gradeC,
			Rejected:       rejected,
		}

		row := types.NewRowFromStruct(&summary, true)
		if err := gp.AddRow(ctx, row); err != nil {
			return fmt.Errorf("failed to add milk production row: %w", err)
		}
	}

	return nil
}

func (c *AnalyticsCommand) generateHealthSummary(
	ctx context.Context,
	client *ent.Client,
	settings *AnalyticsSettings,
	gp middlewares.Processor,
) error {
	// Implementation for health summary analytics
	// This would analyze health records and generate health insights
	row := types.NewRow(types.MRFromMap(map[string]interface{}{
		"message": "Health summary analytics - implementation pending",
		"status":  "placeholder",
	}))
	return gp.AddRow(ctx, row)
}

func (c *AnalyticsCommand) generateBreedingSummary(
	ctx context.Context,
	client *ent.Client,
	settings *AnalyticsSettings,
	gp middlewares.Processor,
) error {
	// Implementation for breeding summary analytics
	row := types.NewRow(types.MRFromMap(map[string]interface{}{
		"message": "Breeding summary analytics - implementation pending",
		"status":  "placeholder",
	}))
	return gp.AddRow(ctx, row)
}

func (c *AnalyticsCommand) generateFeedConsumptionReport(
	ctx context.Context,
	client *ent.Client,
	settings *AnalyticsSettings,
	gp middlewares.Processor,
) error {
	// Implementation for feed consumption analytics
	row := types.NewRow(types.MRFromMap(map[string]interface{}{
		"message": "Feed consumption analytics - implementation pending",
		"status":  "placeholder",
	}))
	return gp.AddRow(ctx, row)
}

func (c *AnalyticsCommand) generateGoatPerformanceReport(
	ctx context.Context,
	client *ent.Client,
	settings *AnalyticsSettings,
	gp middlewares.Processor,
) error {
	// Implementation for individual goat performance analytics
	row := types.NewRow(types.MRFromMap(map[string]interface{}{
		"message": "Goat performance analytics - implementation pending",
		"status":  "placeholder",
	}))
	return gp.AddRow(ctx, row)
}

