package output

import (
	"fmt"
	"os"
	"strconv"

	"github.com/olekukonko/tablewriter"
	"pr-analyzer/internal/analysis"
)

// PrintTable outputs the analysis results in table format
func PrintTable(result *analysis.PRAnalysisResult) error {
	// Print PR Info
	fmt.Printf("Pull Request Analysis\n")
	fmt.Printf("=====================\n\n")

	if result.PRInfo.PRBranch != "" {
		fmt.Printf("Base Branch: %s\n", result.PRInfo.BaseBranch)
		fmt.Printf("PR Branch: %s\n", result.PRInfo.PRBranch)
	}
	if result.PRInfo.MergeCommit != "" {
		fmt.Printf("Merge Commit: %s\n", result.PRInfo.MergeCommit)
	}
	if result.PRInfo.RepoPath != "" {
		fmt.Printf("Repository: %s\n", result.PRInfo.RepoPath)
	}
	fmt.Printf("Total Files: %d\n", result.PRInfo.TotalFiles)
	fmt.Printf("Total Lines Changed: %d\n", result.PRInfo.TotalLines)
	fmt.Printf("Total Commits: %d\n\n", result.PRInfo.TotalCommits)

	// Merge commit metadata
	if result.PRInfo.MergeCommit != "" {
		fmt.Printf("Merge Commit Metadata\n")
		fmt.Printf("=====================\n")
		fmt.Printf("Author:    %s <%s>\n", result.PRInfo.MergeAuthorName, result.PRInfo.MergeAuthorEmail)
		fmt.Printf("Authored:  %s\n", result.PRInfo.MergeAuthorDate.Format("2006-01-02 15:04:05"))
		fmt.Printf("Committer: %s <%s>\n", result.PRInfo.MergeCommitterName, result.PRInfo.MergeCommitterEmail)
		fmt.Printf("Committed: %s\n", result.PRInfo.MergeCommitterDate.Format("2006-01-02 15:04:05"))
		if result.PRInfo.MergeSummary != "" {
			fmt.Printf("Summary:   %s\n", result.PRInfo.MergeSummary)
		}
		fmt.Println()
	}

	// Language Statistics Table
	fmt.Printf("Language Statistics\n")
	fmt.Printf("==================\n")
	
	langTable := tablewriter.NewWriter(os.Stdout)
	langTable.Header("Language", "Files", "Added", "Deleted", "Total", "Percentage")

	for _, lang := range result.LanguageStats {
		langTable.Append([]string{
			lang.Language,
			strconv.Itoa(lang.FilesChanged),
			strconv.Itoa(lang.LinesAdded),
			strconv.Itoa(lang.LinesDeleted),
			strconv.Itoa(lang.LinesModified),
			fmt.Sprintf("%.1f%%", lang.Percentage),
		})
	}
	langTable.Render()
	fmt.Println()

	// Cross-System Analysis Table
	fmt.Printf("Cross-System Analysis\n")
	fmt.Printf("====================\n")
	
	crossTable := tablewriter.NewWriter(os.Stdout)
	crossTable.Header("Metric", "Count")

	crossTable.Append([]string{"Total Commits", strconv.Itoa(result.CrossSystemStats.TotalCommits)})
	crossTable.Append([]string{"Single-System Commits", strconv.Itoa(result.CrossSystemStats.SingleSystemCommits)})
	crossTable.Append([]string{"Multi-System Commits", strconv.Itoa(result.CrossSystemStats.MultiSystemCommits)})
	crossTable.Append([]string{"Cross-System Rate", fmt.Sprintf("%.1f%%", result.CrossSystemStats.CrossSystemRate)})

	crossTable.Render()
	fmt.Println()

	// Most Touched Systems
	if len(result.CrossSystemStats.MostTouchedSystems) > 0 {
		fmt.Printf("Most Touched Systems\n")
		fmt.Printf("===================\n")
		
		systemTable := tablewriter.NewWriter(os.Stdout)
		systemTable.Header("System", "Commits")

		for _, system := range result.CrossSystemStats.MostTouchedSystems {
			systemTable.Append([]string{
				system.System,
				strconv.Itoa(system.Count),
			})
		}
		systemTable.Render()
		fmt.Println()
	}

	// System Co-occurrence Matrix (if there are multiple systems)
	if len(result.CrossSystemStats.SystemTouchMatrix) > 1 {
		fmt.Printf("System Co-occurrence Matrix\n")
		fmt.Printf("==========================\n")
		
		// Get all systems
		var systems []string
		for system := range result.CrossSystemStats.SystemTouchMatrix {
			systems = append(systems, system)
		}

		// Create matrix table
		matrixTable := tablewriter.NewWriter(os.Stdout)
		matrixTable.Header(append([]interface{}{"System"}, func() []interface{} {
			var headers []interface{}
			for _, s := range systems {
				headers = append(headers, s)
			}
			return headers
		}())...)

		for _, system1 := range systems {
			row := []string{system1}
			for _, system2 := range systems {
				if system1 == system2 {
					row = append(row, "-")
				} else {
					count := result.CrossSystemStats.SystemTouchMatrix[system1][system2]
					row = append(row, strconv.Itoa(count))
				}
			}
			matrixTable.Append(row)
		}
		matrixTable.Render()
		fmt.Println()
	}

	// Categories Configuration
	if len(result.Categories) > 0 {
		fmt.Printf("Categories Configuration\n")
		fmt.Printf("=======================\n")
		
		catTable := tablewriter.NewWriter(os.Stdout)
		catTable.Header("Category", "Patterns")

		for category, patterns := range result.Categories {
			patternStr := ""
			for i, pattern := range patterns {
				if i > 0 {
					patternStr += ", "
				}
				patternStr += pattern
			}
			catTable.Append([]string{category, patternStr})
		}
		catTable.Render()
		fmt.Println()
	}

	return nil
}

