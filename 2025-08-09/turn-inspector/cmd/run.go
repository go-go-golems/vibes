package cmd

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"text/tabwriter"

	"github.com/spf13/cobra"

	"turn-inspector/ent"
	"turn-inspector/ent/run"
)

var runCmd = &cobra.Command{
	Use:   "run",
	Short: "Manage runs",
	Long:  `Manage top-level runs which contain turns and have their own metadata.`,
}

var listRunsCmd = &cobra.Command{
	Use:   "list",
	Short: "List runs",
	RunE: func(cmd *cobra.Command, args []string) error {
		ctx := context.Background()
		client := GetClient()
		if client == nil {
			return fmt.Errorf("database client not initialized")
		}
		runs, err := client.Run.Query().
			WithMetadata().
			WithTurns().
			Order(ent.Desc(run.FieldID)).
			All(ctx)
		if err != nil {
			return fmt.Errorf("failed to query runs: %w", err)
		}
		w := tabwriter.NewWriter(os.Stdout, 0, 0, 2, ' ', 0)
		fmt.Fprintln(w, "ID\tName\tMetadata Count\tTurns Count")
		fmt.Fprintln(w, "--\t----\t--------------\t-----------")
		for _, r := range runs {
			name := r.Name
			fmt.Fprintf(w, "%d\t%s\t%d\t%d\n", r.ID, name, len(r.Edges.Metadata), len(r.Edges.Turns))
		}
		w.Flush()
		return nil
	},
}

var showRunCmd = &cobra.Command{
	Use:   "show",
	Short: "Show a run",
	RunE: func(cmd *cobra.Command, args []string) error {
		ctx := context.Background()
		client := GetClient()
		if client == nil {
			return fmt.Errorf("database client not initialized")
		}
		r, err := client.Run.Query().
			Where(run.IDEQ(showRunIDFlag)).
			WithMetadata().
			WithTurns().
			Only(ctx)
		if err != nil {
			return fmt.Errorf("failed to query run: %w", err)
		}
		if jsonOutputFlag {
			out := map[string]any{
				"id":       r.ID,
				"name":     r.Name,
				"metadata": r.Edges.Metadata,
				"turns":    r.Edges.Turns,
			}
			b, _ := json.MarshalIndent(out, "", "  ")
			fmt.Println(string(b))
			return nil
		}
		fmt.Printf("Run ID: %d\n", r.ID)
		if r.Name != "" {
			fmt.Printf("Name: %s\n", r.Name)
		}
		if len(r.Edges.Metadata) > 0 {
			fmt.Println("\nRun Metadata:")
			w := tabwriter.NewWriter(os.Stdout, 0, 0, 2, ' ', 0)
			fmt.Fprintln(w, "Source\tKey\tValue")
			fmt.Fprintln(w, "------\t---\t-----")
			for _, m := range r.Edges.Metadata {
				fmt.Fprintf(w, "%s\t%s\t%s\n", m.Source, m.Key, m.Value)
			}
			w.Flush()
		}
		fmt.Printf("\nTurns: %d\n", len(r.Edges.Turns))
		return nil
	},
}

var deleteRunCmd = &cobra.Command{
	Use:   "delete",
	Short: "Delete a run and its turns",
	RunE: func(cmd *cobra.Command, args []string) error {
		ctx := context.Background()
		client := GetClient()
		if client == nil {
			return fmt.Errorf("database client not initialized")
		}
		if !confirmFlag {
			return fmt.Errorf("--confirm required to delete a run")
		}
		if err := client.Run.DeleteOneID(deleteRunIDFlag).Exec(ctx); err != nil {
			return fmt.Errorf("failed to delete run: %w", err)
		}
		fmt.Printf("Deleted run %d\n", deleteRunIDFlag)
		return nil
	},
}

var (
	showRunIDFlag   int
	deleteRunIDFlag int
)

func init() {
	rootCmd.AddCommand(runCmd)

	runCmd.AddCommand(listRunsCmd)

	showRunCmd.Flags().IntVar(&showRunIDFlag, "id", 0, "Run ID to show")
	showRunCmd.Flags().BoolVar(&jsonOutputFlag, "json", false, "Output in JSON format")
	showRunCmd.MarkFlagRequired("id")
	runCmd.AddCommand(showRunCmd)

	deleteRunCmd.Flags().IntVar(&deleteRunIDFlag, "id", 0, "Run ID to delete")
	deleteRunCmd.Flags().BoolVar(&confirmFlag, "confirm", false, "Confirm deletion without prompting")
	deleteRunCmd.MarkFlagRequired("id")
	runCmd.AddCommand(deleteRunCmd)
}
