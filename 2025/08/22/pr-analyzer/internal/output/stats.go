package output

import (
	"encoding/json"
	"fmt"
	"os"
	"strconv"

	"github.com/olekukonko/tablewriter"
	"gopkg.in/yaml.v3"
	"pr-analyzer/internal/analysis"
)

// PrintStatsTable outputs aggregate statistics in table format
func PrintStatsTable(stats *analysis.AggregateStats) error {
	fmt.Printf("Repository Statistics\n")
	fmt.Printf("====================\n\n")

	// Summary table
	fmt.Printf("Summary\n")
	fmt.Printf("-------\n")
	summaryTable := tablewriter.NewWriter(os.Stdout)
	summaryTable.Header("Metric", "Value")

	summaryTable.Append([]string{"Total PRs", strconv.Itoa(stats.Summary.TotalPRs)})
	summaryTable.Append([]string{"Total Commits", strconv.Itoa(stats.Summary.TotalCommits)})
	summaryTable.Append([]string{"Total Files", strconv.Itoa(stats.Summary.TotalFiles)})
	summaryTable.Append([]string{"Total Lines", strconv.Itoa(stats.Summary.TotalLines)})
	summaryTable.Append([]string{"Avg Files/PR", fmt.Sprintf("%.1f", stats.Summary.AvgFilesPerPR)})
	summaryTable.Append([]string{"Avg Lines/PR", fmt.Sprintf("%.1f", stats.Summary.AvgLinesPerPR)})
	summaryTable.Append([]string{"Avg Commits/PR", fmt.Sprintf("%.1f", stats.Summary.AvgCommitsPerPR)})
	summaryTable.Append([]string{"Cross-System Rate", fmt.Sprintf("%.1f%%", stats.Summary.CrossSystemRate)})

	summaryTable.Render()
	fmt.Println()

	// Language trends
	if len(stats.LanguageTrends) > 0 {
		fmt.Printf("Language Trends\n")
		fmt.Printf("---------------\n")
		langTable := tablewriter.NewWriter(os.Stdout)
		langTable.Header("Language", "PRs", "Files", "Lines", "Percentage")

		for _, trend := range stats.LanguageTrends {
			if trend.TotalLines > 0 { // Only show languages with actual changes
				langTable.Append([]string{
					trend.Language,
					strconv.Itoa(trend.PRCount),
					strconv.Itoa(trend.TotalFiles),
					strconv.Itoa(trend.TotalLines),
					fmt.Sprintf("%.1f%%", trend.Percentage),
				})
			}
		}
		langTable.Render()
		fmt.Println()
	}

	// Cross-system trends
	fmt.Printf("Cross-System Analysis\n")
	fmt.Printf("--------------------\n")
	crossTable := tablewriter.NewWriter(os.Stdout)
	crossTable.Header("Metric", "Value")

	crossTable.Append([]string{"Single-System PRs", strconv.Itoa(stats.CrossSystemTrends.SingleSystemPRs)})
	crossTable.Append([]string{"Multi-System PRs", strconv.Itoa(stats.CrossSystemTrends.MultiSystemPRs)})
	crossTable.Append([]string{"Cross-System Rate", fmt.Sprintf("%.1f%%", stats.CrossSystemTrends.CrossSystemRate)})
	crossTable.Append([]string{"Avg Systems/PR", fmt.Sprintf("%.1f", stats.CrossSystemTrends.AvgSystemsPerPR)})
	crossTable.Append([]string{"Max Systems in PR", strconv.Itoa(stats.CrossSystemTrends.MaxSystemsInPR)})

	crossTable.Render()
	fmt.Println()

	// Most active systems
	if len(stats.MostActiveSystems) > 0 {
		fmt.Printf("Most Active Systems\n")
		fmt.Printf("------------------\n")
		systemTable := tablewriter.NewWriter(os.Stdout)
		systemTable.Header("System", "PRs")

		for _, system := range stats.MostActiveSystems {
			systemTable.Append([]string{
				system.System,
				strconv.Itoa(system.PRCount),
			})
		}
		systemTable.Render()
		fmt.Println()
	}

	// PR size distribution
	fmt.Printf("PR Size Distribution\n")
	fmt.Printf("-------------------\n")
	sizeTable := tablewriter.NewWriter(os.Stdout)
	sizeTable.Header("Size", "Count", "Description")

	sizeTable.Append([]string{"Small", strconv.Itoa(stats.PRSizeDistribution.Small), "< 10 files"})
	sizeTable.Append([]string{"Medium", strconv.Itoa(stats.PRSizeDistribution.Medium), "10-50 files"})
	sizeTable.Append([]string{"Large", strconv.Itoa(stats.PRSizeDistribution.Large), "50-200 files"})
	sizeTable.Append([]string{"X-Large", strconv.Itoa(stats.PRSizeDistribution.XLarge), "> 200 files"})

	sizeTable.Render()
	fmt.Println()

	return nil
}

// PrintStatsJSON outputs aggregate statistics in JSON format
func PrintStatsJSON(stats *analysis.AggregateStats) error {
	jsonData, err := json.MarshalIndent(stats, "", "  ")
	if err != nil {
		return fmt.Errorf("failed to marshal JSON: %w", err)
	}

	fmt.Println(string(jsonData))
	return nil
}

// PrintStatsYAML outputs aggregate statistics in YAML format
func PrintStatsYAML(stats *analysis.AggregateStats) error {
	yamlData, err := yaml.Marshal(stats)
	if err != nil {
		return fmt.Errorf("failed to marshal YAML: %w", err)
	}

	fmt.Println(string(yamlData))
	return nil
}

