// Guide to Implementing Glazed Commands Correctly

## Overview

This guide explains how to properly implement commands using the go-go-golems/glazed framework. The glazed framework provides a structured way to build CLI applications with rich output formatting options.

## Key Components

1. **CommandDescription**: Defines the command's metadata, flags, and arguments
2. **ParsedLayers**: Contains parsed parameters from different layers
3. **Processor**: Handles the output of command execution

## Correct Implementation Pattern

### 1. Import the right packages

```go
import (
    "github.com/go-go-golems/glazed/pkg/cmds"
    "github.com/go-go-golems/glazed/pkg/cmds/layers"  // Important for ParsedLayers
    "github.com/go-go-golems/glazed/pkg/cmds/parameters"
    "github.com/go-go-golems/glazed/pkg/middlewares"
    "github.com/go-go-golems/glazed/pkg/types"
)
```

### 2. Define your settings struct with glazed tags

```go
type CommandSettings struct {
    Param1 string `glazed.parameter:"param1"`
    Param2 int    `glazed.parameter:"param2"`
}
```

### 3. Create a command struct that embeds CommandDescription

```go
type MyCommand struct {
    *cmds.CommandDescription
}
```

### 4. Implement the command constructor

```go
func NewMyCommand() (*MyCommand, error) {
    return &MyCommand{
        CommandDescription: cmds.NewCommandDescription(
            "command-name",
            cmds.WithShort("Short description"),
            cmds.WithLong("Longer description"),
            cmds.WithFlags(
                parameters.NewParameterDefinition(
                    "param1",
                    parameters.ParameterTypeString,
                    parameters.WithHelp("Help text"),
                    parameters.WithDefault("default value"),
                ),
                parameters.NewParameterDefinition(
                    "param2",
                    parameters.ParameterTypeInteger,
                    parameters.WithHelp("Help text"),
                    parameters.WithDefault(10),
                ),
            ),
        ),
    }, nil
}
```

### 5. Implement the Run or RunIntoGlazeProcessor method

For commands that output structured data:

```go
func (c *MyCommand) RunIntoGlazeProcessor(
    ctx context.Context, 
    parsedLayers *layers.ParsedLayers,  // Note: use layers.ParsedLayers, not cmds.ParsedLayers
    gp middlewares.Processor,
) error {
    s := &CommandSettings{}
    err := parsedLayers.InitializeStruct(layers.DefaultSlug, s)  // Note: use layers.DefaultSlug, not cmds.DefaultSlug
    if err != nil {
        return err
    }
    
    // Command implementation
    row := types.NewRow(
        types.MRP("column1", "value1"),
        types.MRP("column2", 42),
    )
    
    return gp.AddRow(ctx, row)
}
```

For commands that don't output structured data:

```go
func (c *MyCommand) Run(
    ctx context.Context, 
    parsedLayers *layers.ParsedLayers,  // Note: use layers.ParsedLayers, not cmds.ParsedLayers
) error {
    s := &CommandSettings{}
    err := parsedLayers.InitializeStruct(layers.DefaultSlug, s)  // Note: use layers.DefaultSlug, not cmds.DefaultSlug
    if err != nil {
        return err
    }
    
    // Command implementation
    fmt.Println("Command executed with param1:", s.Param1)
    
    return nil
}
```

## Common Mistakes

1. Using `cmds.ParsedLayers` instead of `layers.ParsedLayers`
2. Using `cmds.DefaultSlug` instead of `layers.DefaultSlug`
3. Not implementing the correct interface methods
4. Forgetting to initialize the settings struct from parsed layers

## Best Practices

1. Always validate input parameters
2. Use meaningful parameter names and provide helpful descriptions
3. Implement proper error handling
4. For structured output, use the RunIntoGlazeProcessor method
5. For simple output, use the Run method

By following this guide, you can correctly implement commands using the glazed framework and avoid common errors.
