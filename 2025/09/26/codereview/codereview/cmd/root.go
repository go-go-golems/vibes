package cmd

import (
	"os"

	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

var rootCmd = &cobra.Command{
	Use:   "codereview",
	Short: "Local code review tool with Git integration",
	Long: `A command-line tool for conducting local code reviews with Git integration.
Store reviews in SQLite and serve a web interface for interactive review management.`,
}

func Execute() {
	err := rootCmd.Execute()
	if err != nil {
		os.Exit(1)
	}
}

func init() {
	cobra.OnInitialize(initConfig)

	// Global flags
	rootCmd.PersistentFlags().String("config", "", "config file (default is .codereview/config.yaml)")
	rootCmd.PersistentFlags().String("db", "", "database file (default is .codereview/reviews.db)")
	rootCmd.PersistentFlags().Bool("verbose", false, "verbose output")

	// Bind flags to viper
	viper.BindPFlag("config", rootCmd.PersistentFlags().Lookup("config"))
	viper.BindPFlag("db", rootCmd.PersistentFlags().Lookup("db"))
	viper.BindPFlag("verbose", rootCmd.PersistentFlags().Lookup("verbose"))

	// Add subcommands
	rootCmd.AddCommand(newInitCommand())
	rootCmd.AddCommand(newCreateCommand())
	rootCmd.AddCommand(newListCommand())
	rootCmd.AddCommand(newShowCommand())
	rootCmd.AddCommand(newAnnotateCommand())
	rootCmd.AddCommand(newServeCommand())
	rootCmd.AddCommand(newExportCommand())
	rootCmd.AddCommand(newImportCommand())
}

func initConfig() {
	if cfgFile := viper.GetString("config"); cfgFile != "" {
		viper.SetConfigFile(cfgFile)
	} else {
		viper.AddConfigPath(".codereview")
		viper.SetConfigType("yaml")
		viper.SetConfigName("config")
	}

	viper.AutomaticEnv()

	if err := viper.ReadInConfig(); err == nil {
		if viper.GetBool("verbose") {
			// Could add logging here
		}
	}
}
