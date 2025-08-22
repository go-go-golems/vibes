package cmd

import (
	"context"
	"database/sql"
	"fmt"
	"time"

	"github.com/spf13/cobra"
	"pr-analyzer/internal/db"
	"pr-analyzer/pkg/dbfilters"
)

func init() {
	dbPRsCmd := &cobra.Command{Use: "prs", Short: "List stored analyses (per PR/commit)", RunE: func(cmd *cobra.Command, args []string) error {
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
		rows, err := store.ListPRs(ctx, db.Filters(f))
		if err != nil {
			return err
		}
		defer rows.Close()

		fmt.Printf("Stored Analyses\n================\n")
		for rows.Next() {
			var (
				id int64
				repo, base, pr, commit, merge string
				files, lines, commits int
				at time.Time
				an, ae string
				ad sql.NullTime
				cn, ce string
				cd sql.NullTime
				summary sql.NullString
			)
			if err := rows.Scan(&id, &repo, &base, &pr, &commit, &merge, &files, &lines, &commits, &at, &an, &ae, &ad, &cn, &ce, &cd, &summary); err != nil {
				return err
			}
			fmt.Printf("%d repo=%s commit=%s merge=%s files=%d lines=%d commits=%d at=%s author=%s <%s> summary=%s\n",
				id, repo, valueOrDash(commit), valueOrDash(merge), files, lines, commits, at.Format(time.RFC3339), an, ae, valueOrDash(summary.String))
		}
		return rows.Err()
	}}

	// expose comprehensive filters
	dbfilters.AddFlags(dbPRsCmd)
	ensureDBCmd().AddCommand(dbPRsCmd)
}


