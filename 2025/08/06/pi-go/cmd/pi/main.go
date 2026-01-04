package main

import (
	"fmt"
	"os"

	"github.com/pi-go/pi/internal/cli"
	"github.com/pi-go/pi/internal/config"
	"github.com/pi-go/pi/internal/util"
	"github.com/sirupsen/logrus"
)

var (
	version = "dev"
	commit  = "unknown"
	date    = "unknown"
)

func main() {
	// Initialize logger
	logger := logrus.New()
	logger.SetFormatter(&logrus.TextFormatter{
		FullTimestamp: true,
	})

	// Load configuration
	cfg, err := config.Load()
	if err != nil {
		logger.WithError(err).Fatal("Failed to load configuration")
	}

	// Set log level from configuration
	level, err := logrus.ParseLevel(cfg.Logging.Level)
	if err != nil {
		logger.WithError(err).Warn("Invalid log level, using info")
		level = logrus.InfoLevel
	}
	logger.SetLevel(level)

	// Initialize utilities
	util.SetLogger(logger)

	// Create and execute CLI
	rootCmd := cli.NewRootCommand(cfg, logger, version, commit, date)
	if err := rootCmd.Execute(); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}
}

