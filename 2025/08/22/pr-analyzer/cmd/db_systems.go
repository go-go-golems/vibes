package cmd

import (
	"context"
	"fmt"

	"github.com/spf13/cobra"
	"pr-analyzer/internal/db"
)

func init() {
	dbSysCmd := &cobra.Command{Use: "systems", Short: "Aggregate system stats", RunE: func(cmd *cobra.Command, args []string) error {
		ctx := context.Background()
		store, err := db.Open(ctx, dbPathGlobal)
		if err != nil {
			return err
		}
		defer store.Close()
		rows, err := store.AggregateSystems(ctx)
		if err != nil {
			return err
		}
		fmt.Printf("System Aggregates\n===================\n")
		for _, r := range rows {
			fmt.Printf("%-16s PRs:%4d Count:%7d\n", r.System, r.PRs, r.Count)
		}
		return nil
	}}
	ensureDBCmd().AddCommand(dbSysCmd)
}


