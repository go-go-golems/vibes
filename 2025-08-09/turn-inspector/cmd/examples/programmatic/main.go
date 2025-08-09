package main

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"strconv"

	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/go-go-golems/glazed/pkg/cmds/runner"
	mid "github.com/go-go-golems/glazed/pkg/middlewares"
	"github.com/go-go-golems/glazed/pkg/settings"
	"github.com/go-go-golems/glazed/pkg/types"
)

// SumSettings mirrors the guide
// A and B live on the default layer
// Tags map parameter names -> struct fields
// glazed.parameter tags are used by InitializeStruct
//
// See docs/programmatic-integration.md
//
// Note: we intentionally keep it minimal per the guide.
type SumSettings struct {
	A int `glazed.parameter:"a"`
	B int `glazed.parameter:"b"`
}

type SumCommand struct{ *cmds.CommandDescription }

func NewSumCommand() (*SumCommand, error) {
	glazedLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, err
	}

	desc := cmds.NewCommandDescription(
		"sum",
		cmds.WithShort("Compute a+b"),
		cmds.WithFlags(
			parameters.NewParameterDefinition(
				"a", parameters.ParameterTypeInteger,
				parameters.WithHelp("First operand"),
				parameters.WithRequired(true),
			),
			parameters.NewParameterDefinition(
				"b", parameters.ParameterTypeInteger,
				parameters.WithHelp("Second operand"),
				parameters.WithRequired(true),
			),
		),
		cmds.WithLayersList(glazedLayer),
	)
	return &SumCommand{CommandDescription: desc}, nil
}

func (c *SumCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsed *layers.ParsedLayers,
	gp mid.Processor,
) error {
	s := &SumSettings{}
	if err := parsed.InitializeStruct(layers.DefaultSlug, s); err != nil {
		return fmt.Errorf("parse: %w", err)
	}
	row := types.NewRow(
		types.MRP("a", s.A),
		types.MRP("b", s.B),
		types.MRP("sum", s.A+s.B),
	)
	return gp.AddRow(ctx, row)
}

var _ cmds.GlazeCommand = &SumCommand{}

// CollectingProcessor implements a simple in-memory collector for rows.
type CollectingProcessor struct{ Rows []*types.Row }

func (p *CollectingProcessor) AddRow(ctx context.Context, r *types.Row) error {
	p.Rows = append(p.Rows, r)
	return nil
}

// RunSumToStdout runs the command and writes output using the glazed writer (json)
func RunSumToStdout(a, b int) error {
	cmd, err := NewSumCommand()
	if err != nil {
		return err
	}

	parse := []runner.ParseOption{
		runner.WithValuesForLayers(map[string]map[string]interface{}{
			"default": {"a": a, "b": b},
			"glazed":  {"output": "json"},
		}),
	}
	run := []runner.RunOption{
		runner.WithWriter(os.Stdout),
	}
	return runner.ParseAndRun(context.Background(), cmd, parse, run)
}

// RunSumCollect runs the command, collects rows via a custom processor, and returns them.
func RunSumCollect(a, b int) ([]*types.Row, error) {
	cmd, err := NewSumCommand()
	if err != nil {
		return nil, err
	}

	collector := &CollectingProcessor{}
	run := []runner.RunOption{runner.WithProcessor(collector)}
	parse := []runner.ParseOption{
		runner.WithValuesForLayers(map[string]map[string]interface{}{
			"default": {"a": a, "b": b},
		}),
	}

	if err := runner.ParseAndRun(context.Background(), cmd, parse, run); err != nil {
		return nil, err
	}
	return collector.Rows, nil
}

// helpers
func getenvInt(key string) (int, bool) {
	v, ok := os.LookupEnv(key)
	if !ok {
		return 0, false
	}
	i, err := strconv.Atoi(v)
	if err != nil {
		return 0, false
	}
	return i, true
}

func main() {
	// Accept inputs via env or defaults
	// A is provided via defaults map (shows defaults usage)
	// B is provided via UpdateFromEnv (shows middleware usage)

	// Determine defaults for demonstration: A from env A or 1, B from env B or 2
	defA := 1
	if a, ok := getenvInt("A"); ok {
		defA = a
	}
	// Bridge env B -> SUM_B for UpdateFromEnv to pick up
	if _, ok := os.LookupEnv("SUM_B"); !ok {
		if b, ok := os.LookupEnv("B"); ok {
			_ = os.Setenv("SUM_B", b)
		} else {
			_ = os.Setenv("SUM_B", "2")
		}
	}

	// First path: write to stdout using glazed json output
	if err := RunSumToStdout(defA, 0 /* placeholder, will be overridden by middleware */); err != nil {
		fmt.Fprintf(os.Stderr, "RunSumToStdout failed: %v\n", err)
	}

	// Second path: collect rows using custom processor, then print as JSON
	rows, err := func() ([]*types.Row, error) {
		cmd, err := NewSumCommand()
		if err != nil {
			return nil, err
		}
		collector := &CollectingProcessor{}

		parse := []runner.ParseOption{
			// defaults for A
			runner.WithValuesForLayers(map[string]map[string]interface{}{
				"default": {"a": defA},
			}),
			// middlewares: Update B from env
			runner.WithMiddlewares(
				mid.SetFromDefaults(parameters.WithParseStepSource("defaults")),
				mid.UpdateFromEnv("SUM_", parameters.WithParseStepSource("env")),
			),
		}
		run := []runner.RunOption{runner.WithProcessor(collector)}

		if err := runner.ParseAndRun(context.Background(), cmd, parse, run); err != nil {
			return nil, err
		}
		return collector.Rows, nil
	}()
	if err != nil {
		fmt.Fprintf(os.Stderr, "RunSumCollect failed: %v\n", err)
		return
	}

	// Print collected rows as JSON
	toJSON := make([]map[string]interface{}, 0, len(rows))
	for _, r := range rows {
		m := map[string]interface{}{
			"a":   r.Get("a"),
			"b":   r.Get("b"),
			"sum": r.Get("sum"),
		}
		toJSON = append(toJSON, m)
	}
	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "  ")
	_ = enc.Encode(toJSON)
}
