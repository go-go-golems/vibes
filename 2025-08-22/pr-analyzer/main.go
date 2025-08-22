package main

import (
	"os"
	"pr-analyzer/cmd"
	"time"

	"github.com/rs/zerolog"
	"github.com/rs/zerolog/log"
)

func main() {
	// Configure default zerolog; CLI may adjust level later
	zerolog.TimeFieldFormat = time.RFC3339
	log.Logger = log.Output(zerolog.ConsoleWriter{Out: os.Stderr, TimeFormat: time.RFC3339})
	log.Debug().Msg("starting pr-analyzer")
	if err := cmd.Execute(); err != nil {
		log.Error().Err(err).Msg("execution failed")
		os.Exit(1)
	}
}

