package main

import (
	"context"
	"flag"
	"log"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/go-go-golems/bobatea/pkg/eventbus"
	"github.com/go-go-golems/bobatea/pkg/logutil"
	"github.com/go-go-golems/bobatea/pkg/repl"
	"github.com/go-go-golems/bobatea/pkg/timeline"
	"github.com/rs/zerolog"
	"github.com/wesen/goja-prolog/internal/prolog"
)

func parseLevel(s string) zerolog.Level {
	switch s {
	case "trace":
		return zerolog.TraceLevel
	case "debug":
		return zerolog.DebugLevel
	case "info":
		return zerolog.InfoLevel
	case "warn", "warning":
		return zerolog.WarnLevel
	case "error", "err":
		return zerolog.ErrorLevel
	default:
		return zerolog.ErrorLevel
	}
}

func main() {
	// CLI flags for logging
	ll := flag.String("log-level", "error", "log level: trace, debug, info, warn, error")
	lf := flag.String("log-file", "", "log file path (optional)")
	flag.Parse()

	level := parseLevel(*ll)
	if *lf != "" {
		logutil.InitTUILoggingToFile(level, *lf)
	} else {
		logutil.InitTUILoggingToDiscard(level)
	}

	// Create the Prolog evaluator
	evaluator, err := prolog.NewPrologEvaluator()
	if err != nil {
		log.Fatal(err)
	}

	// Configure REPL
	config := repl.DefaultConfig()
	config.Title = "Prolog REPL"
	config.Prompt = "prolog> "
	config.Placeholder = "Enter Prolog facts, rules, or queries (use ?- for queries)"
	config.EnableHistory = true
	config.EnableExternalEditor = true

	// Set up event bus (for timeline/structured output)
	bus, err := eventbus.NewInMemoryBus()
	if err != nil {
		log.Fatal(err)
	}
	repl.RegisterReplToTimelineTransformer(bus)

	// Create REPL model
	model := repl.NewModel(evaluator, config, bus.Publisher)

	// Create Bubble Tea program
	p := tea.NewProgram(model, tea.WithAltScreen())
	timeline.RegisterUIForwarder(bus, p)

	// Run event bus and UI
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	errs := make(chan error, 2)
	go func() { errs <- bus.Run(ctx) }()
	go func() { _, e := p.Run(); cancel(); errs <- e }()

	if e := <-errs; e != nil {
		log.Fatal(e)
	}
}
