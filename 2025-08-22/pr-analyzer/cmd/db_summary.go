package cmd

import (
	"context"
	"fmt"

	"github.com/spf13/cobra"
	"pr-analyzer/internal/db"
	"pr-analyzer/pkg/dbfilters"
)

func init() {
	dbSummaryCmd := &cobra.Command{Use: "summary", Short: "Summary across stored analyses", RunE: func(cmd *cobra.Command, args []string) error {
		ctx := context.Background()
		store, err := db.Open(ctx, dbPathGlobal)
		if err != nil {
			return err
		}
		defer store.Close()

		f, err := dbfilters.FromCmdFlags(cmd)
		if err != nil {
			return fmt.Errorf("invalid filters: %w", err)
		}
		sum, err := store.Summary(ctx, db.Filters(f))
		if err != nil {
			return err
		}
		fmt.Printf("Summary\n=======\nPRs: %d\nFiles: %d\nLines: %d\n", sum.PRs, sum.Files, sum.Lines)
		return nil
	}}

	// expose comprehensive filters
	dbfilters.AddFlags(dbSummaryCmd)
	ensureDBCmd().AddCommand(dbSummaryCmd)
}


