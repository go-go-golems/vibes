package cmd

import (
	"github.com/spf13/cobra"
)

var dbCmd *cobra.Command

// ensureDBCmd lazily initializes and attaches the parent DB command group.
func ensureDBCmd() *cobra.Command {
    if dbCmd == nil {
        dbCmd = &cobra.Command{Use: "db", Short: "SQLite database utilities"}
        rootCmd.AddCommand(dbCmd)
    }
    return dbCmd
}

func init() {
    // Eagerly ensure for the typical case
    ensureDBCmd()
}


