package cmd

import (
	"context"
	"fmt"

	"github.com/spf13/cobra"
	"pr-analyzer/internal/db"
)

func init() {
	dbLangCmd := &cobra.Command{Use: "languages", Short: "Aggregate language stats", RunE: func(cmd *cobra.Command, args []string) error {
		ctx := context.Background()
		store, err := db.Open(ctx, dbPathGlobal)
		if err != nil {
			return err
		}
		defer store.Close()
		rows, err := store.AggregateLanguages(ctx)
		if err != nil {
			return err
		}
		fmt.Printf("Language Aggregates\n====================\n")
		for _, r := range rows {
			fmt.Printf("%-16s PRs:%4d Files:%5d Lines:%7d\n", r.Language, r.PRs, r.Files, r.Lines)
		}
		return nil
	}}
	ensureDBCmd().AddCommand(dbLangCmd)
}


