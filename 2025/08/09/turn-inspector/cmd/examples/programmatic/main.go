package main

import (
	"context"
	"fmt"

	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/go-go-golems/glazed/pkg/cmds/runner"
	"github.com/go-go-golems/glazed/pkg/middlewares"
	"github.com/go-go-golems/glazed/pkg/settings"
	"github.com/go-go-golems/glazed/pkg/types"
)

// SumSettings mirrors the guide
// A and B live on the default layer
// Tags map parameter names -> struct fields
// glazed.parameter tags are used by InitializeStruct
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
	gp middlewares.Processor,
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

// RunSumToStdout runs the command and writes output using glazed (defaults to table; set output=json via layers if needed)
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
	return runner.ParseAndRun(context.Background(), cmd, parse, nil)
}

func main() {
	_ = RunSumToStdout(1, 2)
}
