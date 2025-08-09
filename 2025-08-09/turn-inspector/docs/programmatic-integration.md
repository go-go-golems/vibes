---
Title: Programmatic Integration Guide
Slug: programmatic-integration
Short: How to embed and run Glazed commands from another Go program
SectionType: GeneralTopic
IsTopLevel: true
ShowPerDefault: true
Topics: [glazed, integration, embedding, runner, layers, middlewares]
---

This guide shows how to integrate Glazed commands into another Go program: constructing commands, composing parameter layers, running them programmatically (without Cobra), and capturing structured results.

## When to use programmatic integration

- You have a library or service that should expose the same functionality as your CLI.
- You need to drive commands from tests or internal tools and capture results as Go values.
- You want to reuse Glazed's parameter parsing, validation, and output processing, but within your own app.

## Key packages

- `github.com/go-go-golems/glazed/pkg/cmds` — command interfaces and descriptions
- `github.com/go-go-golems/glazed/pkg/cmds/parameters` — typed parameters
- `github.com/go-go-golems/glazed/pkg/cmds/layers` — parameter layering
- `github.com/go-go-golems/glazed/pkg/cmds/runner` — programmatic execution helpers
- `github.com/go-go-golems/glazed/pkg/middlewares` — value loading and parsing
- `github.com/go-go-golems/glazed/pkg/types` — structured `Row`
- `github.com/go-go-golems/glazed/pkg/settings` — standard Glazed output layer

For background, see: `glaze help commands-reference`, `glaze help layers-guide`, `glaze help middlewares-guide`, and `glaze help build-first-command`.

## 1) Define a GlazeCommand

```go
package mypkg

import (
    "context"
    "fmt"

    "github.com/go-go-golems/glazed/pkg/cmds"
    "github.com/go-go-golems/glazed/pkg/cmds/layers"
    "github.com/go-go-golems/glazed/pkg/cmds/parameters"
    "github.com/go-go-golems/glazed/pkg/middlewares"
    "github.com/go-go-golems/glazed/pkg/settings"
    "github.com/go-go-golems/glazed/pkg/types"
)

type SumSettings struct {
    A int `glazed.parameter:"a"`
    B int `glazed.parameter:"b"`
}

type SumCommand struct { *cmds.CommandDescription }

func NewSumCommand() (*SumCommand, error) {
    glazedLayer, err := settings.NewGlazedParameterLayers()
    if err != nil { return nil, err }

    desc := cmds.NewCommandDescription(
        "sum",
        cmds.WithShort("Compute a+b"),
        cmds.WithFlags(
            parameters.NewParameterDefinition("a", parameters.ParameterTypeInteger,
                parameters.WithHelp("First operand"), parameters.WithRequired(true)),
            parameters.NewParameterDefinition("b", parameters.ParameterTypeInteger,
                parameters.WithHelp("Second operand"), parameters.WithRequired(true)),
        ),
        cmds.WithLayersList(glazedLayer),
    )
    return &SumCommand{ CommandDescription: desc }, nil
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
```

## 2) Run without Cobra using runner

```go
package mypkg

import (
    "context"
    "os"

    "github.com/go-go-golems/glazed/pkg/cmds/runner"
)

func RunSumToStdout(a, b int) error {
    cmd, _ := NewSumCommand()

    parse := []runner.ParseOption{
        runner.WithValuesForLayers(map[string]map[string]interface{}{
            "default": {"a": a, "b": b},
            // you can also set glazed fields, e.g.: "glazed": {"output": "json"}
        }),
    }

    run := []runner.RunOption{
        runner.WithWriter(os.Stdout), // table/json/yaml etc via glazed layer
    }

    return runner.ParseAndRun(context.Background(), cmd, parse, run)
}
```

## 3) Capture rows programmatically

Use a processor that collects rows instead of writing.

```go
package mypkg

import (
    "context"

    "github.com/go-go-golems/glazed/pkg/cmds/runner"
    "github.com/go-go-golems/glazed/pkg/types"
)

type CollectingProcessor struct { Rows []*types.Row }

func (p *CollectingProcessor) AddRow(ctx context.Context, r *types.Row) error {
    p.Rows = append(p.Rows, r)
    return nil
}

// Implement other methods if needed; a minimal AddRow is enough for many cases.

func RunSumCollect(a, b int) ([]*types.Row, error) {
    cmd, _ := NewSumCommand()

    collector := &CollectingProcessor{}
    run := []runner.RunOption{ runner.WithProcessor(collector) }
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
```

## 4) Load parameters from env/config (middlewares)

Order middlewares so that specific sources override general ones (`glaze help middlewares-guide`).

```go
package mypkg

import (
    "fmt"
    "os"

    mid "github.com/go-go-golems/glazed/pkg/middlewares"
    "github.com/go-go-golems/glazed/pkg/cmds/runner"
    "github.com/go-go-golems/glazed/pkg/cmds/parameters"
)

func RunSumWithEnv() error {
    cmd, _ := NewSumCommand()

    mws := []runner.ParseOption{
        runner.WithMiddlewares(
            mid.SetFromDefaults(parameters.WithParseStepSource("defaults")),
            mid.UpdateFromEnv("SUM_", parameters.WithParseStepSource("env")),
        ),
        runner.WithValuesForLayers(map[string]map[string]interface{}{
            "default": {"a": 1}, // base values
        }),
    }

    os.Setenv("SUM_B", "41")

    return runner.ParseAndRun(context.Background(), cmd, append(mws,
        runner.WithWriter(os.Stdout))...,
    )
}
```

## 5) Embedding in existing apps (Cobra or not)

- Cobra-based app: build your Glazed command into a `*cobra.Command` and add it to your root command.
- Non-Cobra app: use `runner.ParseAndRun` directly, or construct and execute middlewares manually.

```go
// Cobra integration (optional)
import (
    "github.com/go-go-golems/glazed/pkg/cli"
)

func AddSumToCobra(root *cobra.Command) error {
    cmd, _ := NewSumCommand()
    cobraCmd, err := cli.BuildCobraCommand(cmd) // auto-selects the right builder
    if err != nil { return err }
    root.AddCommand(cobraCmd)
    return nil
}
```

## 6) Multiple layers and type-safe settings

Split configuration across layers, then initialize settings from each layer.

```go
// Example: add a logging layer and extract two settings structs

type LoggingSettings struct {
    Level string `glazed.parameter:"log-level"`
}

func NewLoggingLayer() (layers.ParameterLayer, error) {
    return layers.NewParameterLayer(
        "logging", "Logging",
        layers.WithParameterDefinitions(
            parameters.NewParameterDefinition(
                "log-level", parameters.ParameterTypeChoice,
                parameters.WithChoices("debug","info","warn","error"),
                parameters.WithDefault("info"),
            ),
        ),
    )
}

func (c *SumCommand) RunIntoGlazeProcessor(
    ctx context.Context,
    parsed *layers.ParsedLayers,
    gp middlewares.Processor,
) error {
    // default layer
    s := &SumSettings{}
    if err := parsed.InitializeStruct(layers.DefaultSlug, s); err != nil { return err }

    // optional logging layer
    if parsed.Has("logging") {
        ls := &LoggingSettings{}
        if err := parsed.InitializeStruct("logging", ls); err == nil {
            // configure logger based on ls.Level
        }
    }

    return gp.AddRow(ctx, types.NewRow(
        types.MRP("a", s.A), types.MRP("b", s.B), types.MRP("sum", s.A+s.B)))
}
```

## 7) Testing commands programmatically

```go
package mypkg_test

import (
    "context"
    "testing"

    "github.com/go-go-golems/glazed/pkg/cmds/runner"
    "github.com/stretchr/testify/require"
)

func TestSum(t *testing.T) {
    cmd, _ := mypkg.NewSumCommand()

    collector := &mypkg.CollectingProcessor{}
    err := runner.ParseAndRun(context.Background(), cmd,
        []runner.ParseOption{
            runner.WithValuesForLayers(map[string]map[string]interface{}{
                "default": {"a": 1, "b": 2},
            }),
        },
        []runner.RunOption{ runner.WithProcessor(collector) },
    )
    require.NoError(t, err)
    require.Len(t, collector.Rows, 1)
    require.Equal(t, 3, collector.Rows[0].Get("sum"))
}
```

## 8) Patterns and tips

- Prefer GlazeCommand + `types.Row` for flexible output (JSON/YAML/CSV/tables) without code changes.
- Centralize shared layers (logging, database) and compose per-command.
- Use `runner.WithValuesForLayers` for simple programmatic calls.
- Use middlewares to honor env/config/CLI precedence (`glaze help middlewares-guide`).
- For multi-section outputs, include a `section` field to help downstream processing.

## References

- `glaze help commands-reference`
- `glaze help layers-guide`
- `glaze help middlewares-guide`
- `glaze help build-first-command`
- `glaze help help-system`
