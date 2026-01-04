package main

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"

	"github.com/go-go-golems/glazed/pkg/cli"
	"github.com/go-go-golems/glazed/pkg/cmds"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/go-go-golems/glazed/pkg/middlewares"
	"github.com/go-go-golems/glazed/pkg/settings"
	"github.com/go-go-golems/glazed/pkg/types"
)

type QueryCommand struct {
	*cmds.CommandDescription
}

type QuerySettings struct {
	Query      string `glazed.parameter:"query"`
	CayleyURL  string `glazed.parameter:"cayley-url"`
	Predefined string `glazed.parameter:"predefined"`
}

func (c *QueryCommand) RunIntoGlazeProcessor(
	ctx context.Context,
	parsedLayers *layers.ParsedLayers,
	gp middlewares.Processor,
) error {
	settings := &QuerySettings{}
	if err := parsedLayers.InitializeStruct(layers.DefaultSlug, settings); err != nil {
		return err
	}

	var query string
	
	// Use predefined query if specified
	if settings.Predefined != "" {
		predefinedQuery, exists := getPredefinedQuery(settings.Predefined)
		if !exists {
			return fmt.Errorf("predefined query '%s' not found", settings.Predefined)
		}
		query = predefinedQuery
	} else {
		query = settings.Query
	}

	if query == "" {
		return fmt.Errorf("either --query or --predefined must be specified")
	}

	// Execute query against Cayley
	result, err := executeGizmoQuery(settings.CayleyURL, query)
	if err != nil {
		return fmt.Errorf("failed to execute query: %w", err)
	}

	// Process and output results
	return outputQueryResult(ctx, gp, result, settings.Predefined, query)
}

func executeGizmoQuery(cayleyURL, query string) (interface{}, error) {
	// Prepare request body
	requestBody := map[string]string{
		"query": query,
	}
	
	jsonBody, err := json.Marshal(requestBody)
	if err != nil {
		return nil, fmt.Errorf("failed to marshal request: %w", err)
	}

	// Make HTTP request to Cayley
	resp, err := http.Post(cayleyURL+"/api/v1/query/gizmo", "application/json", bytes.NewBuffer(jsonBody))
	if err != nil {
		return nil, fmt.Errorf("failed to make request: %w", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		body, _ := io.ReadAll(resp.Body)
		return nil, fmt.Errorf("query failed with status %d: %s", resp.StatusCode, string(body))
	}

	// Parse response
	var result interface{}
	if err := json.NewDecoder(resp.Body).Decode(&result); err != nil {
		return nil, fmt.Errorf("failed to decode response: %w", err)
	}

	return result, nil
}

func outputQueryResult(ctx context.Context, gp middlewares.Processor, result interface{}, predefined, query string) error {
	// Convert result to rows
	switch v := result.(type) {
	case map[string]interface{}:
		if resultArray, ok := v["result"].([]interface{}); ok {
			for i, item := range resultArray {
				row := types.NewRow(
					types.MRP("index", i),
					types.MRP("result", fmt.Sprintf("%v", item)),
				)
				if err := gp.AddRow(ctx, row); err != nil {
					return err
				}
			}
		} else {
			// Single result
			row := types.NewRow(
				types.MRP("query", query),
				types.MRP("predefined", predefined),
				types.MRP("result", fmt.Sprintf("%v", v)),
			)
			if err := gp.AddRow(ctx, row); err != nil {
				return err
			}
		}
	case []interface{}:
		for i, item := range v {
			row := types.NewRow(
				types.MRP("index", i),
				types.MRP("result", fmt.Sprintf("%v", item)),
			)
			if err := gp.AddRow(ctx, row); err != nil {
				return err
			}
		}
	default:
		row := types.NewRow(
			types.MRP("query", query),
			types.MRP("predefined", predefined),
			types.MRP("result", fmt.Sprintf("%v", result)),
		)
		if err := gp.AddRow(ctx, row); err != nil {
			return err
		}
	}

	return nil
}

func getPredefinedQuery(name string) (string, bool) {
	queries := map[string]string{
		"all-documents": `g.V().has("ex:type", "Document").all();`,
		"all-people": `g.V().has("ex:type", "Person").all();`,
		"draft-documents": `g.V().has("ex:type", "Document").has("ex:hasStatus", "draft").all();`,
		"long-lived-docs": `g.V().has("ex:type", "Document").has("ex:isLongLived", "true").all();`,
		"document-owners": `
			g.V().has("ex:type", "Document")
			.tag("doc")
			.out("ex:ownedBy")
			.tag("owner")
			.all();`,
		"stale-docs": `
			g.V().has("ex:type", "Document")
			.has("ex:isLongLived", "true")
			.tag("doc")
			.out("ex:aboutSymbol")
			.in("ex:touchesSymbol")
			.has("ex:type", "Commit")
			.tag("recent_commit")
			.all();`,
	}
	
	query, exists := queries[name]
	return query, exists
}

// Ensure interface compliance
var _ cmds.GlazeCommand = &QueryCommand{}

func NewQueryCommand() (*QueryCommand, error) {
	glazedLayer, err := settings.NewGlazedParameterLayers()
	if err != nil {
		return nil, err
	}

	commandSettingsLayer, err := cli.NewCommandSettingsLayer()
	if err != nil {
		return nil, err
	}

	cmdDesc := cmds.NewCommandDescription(
		"query",
		cmds.WithShort("Execute Gizmo queries against Cayley"),
		cmds.WithLong(`Execute Gizmo queries against the Cayley graph database.

This command allows you to run both custom Gizmo queries and predefined
queries for common operations.

Available predefined queries:
  all-documents    - List all documents
  all-people       - List all people
  draft-documents  - List documents in draft status
  long-lived-docs  - List long-lived documents
  document-owners  - Show document ownership relationships
  stale-docs       - Find potentially stale long-lived documents

Examples:
  docmgmt query --predefined all-documents
  docmgmt query --predefined document-owners --output json
  docmgmt query --query 'g.V().has("ex:type", "Document").getLimit(5);'
		`),

		cmds.WithFlags(
			parameters.NewParameterDefinition(
				"query",
				parameters.ParameterTypeString,
				parameters.WithDefault(""),
				parameters.WithHelp("Gizmo query to execute"),
			),
			parameters.NewParameterDefinition(
				"predefined",
				parameters.ParameterTypeChoice,
				parameters.WithChoices("all-documents", "all-people", "draft-documents", "long-lived-docs", "document-owners", "stale-docs"),
				parameters.WithHelp("Use a predefined query"),
			),
			parameters.NewParameterDefinition(
				"cayley-url",
				parameters.ParameterTypeString,
				parameters.WithDefault("http://127.0.0.1:64210"),
				parameters.WithHelp("Cayley server URL"),
			),
		),

		cmds.WithLayersList(glazedLayer, commandSettingsLayer),
	)

	return &QueryCommand{
		CommandDescription: cmdDesc,
	}, nil
}

