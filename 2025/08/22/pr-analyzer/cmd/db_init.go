package cmd

import (
	"context"

	"github.com/spf13/cobra"
	"github.com/rs/zerolog/log"
	"pr-analyzer/internal/db"
)

func init() {
	dbInitCmd := &cobra.Command{Use: "init", Short: "Initialize sqlite schema", RunE: func(cmd *cobra.Command, args []string) error {
		ctx := context.Background()
		store, err := db.Open(ctx, dbPathGlobal)
		if err != nil {
			return err
		}
		defer store.Close()
		log.Info().Str("db", dbPathGlobal).Msg("sqlite schema ready")
		return nil
	}}
	ensureDBCmd().AddCommand(dbInitCmd)
}


