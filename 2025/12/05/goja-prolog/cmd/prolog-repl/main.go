package main

import (
	"embed"
	"fmt"
	"github.com/dop251/goja"
	"github.com/dop251/goja_nodejs/require"
	"github.com/rs/zerolog"
	"github.com/rs/zerolog/log"
	"os"
)

//go:embed assets/prolog-ts.js
var jsBundle embed.FS

func main() {
	setupLogger()
	log.Info().Msg("Starting Prolog interpreter")

	// Create a new JavaScript VM
	vm := goja.New()
	log.Debug().Msg("JavaScript VM created")

	// Set up module loader for embedded bundle
	reg := require.NewRegistry(require.WithLoader(func(path string) ([]byte, error) {
		log.Debug().Str("path", path).Msg("Loading module from embedded bundle")

		// Handle different path requests
		var fullPath string
		if path == "prolog-ts.js" || path == "app.js" || path == "node_modules/prolog-ts.js" {
			fullPath = "assets/prolog-ts.js"
		} else {
			return nil, fmt.Errorf("module not found: %s", path)
		}

		log.Debug().Str("path", path).Str("fullPath", fullPath).Msg("Resolving module path")

		data, err := jsBundle.ReadFile(fullPath)
		if err != nil {
			log.Error().
				Err(err).
				Str("requestedPath", path).
				Str("fullPath", fullPath).
				Msg("Failed to load module from embedded bundle")
			return nil, err
		}
		return data, nil
	}))

	// Hook Node polyfills (fs, path, etc.)
	reg.Enable(vm)
	log.Debug().Msg("Node.js polyfills enabled")

	// Set up console
	setupConsole(vm)

	// Load the Prolog module
	log.Info().Msg("Loading Prolog interpreter module")
	prologModule := require.Require(vm, "prolog-ts.js")
	if prologModule == nil {
		log.Fatal().Msg("Failed to load Prolog module")
	}

	// Get the module exports
	prologObj := prologModule.ToObject(vm)
	if prologObj == nil {
		log.Fatal().Msg("Failed to get Prolog module object")
	}

	// Get factory function
	createDBValue := prologObj.Get("createPrologDB")
	createDBFunc, ok := goja.AssertFunction(createDBValue)
	if !ok {
		log.Fatal().Msg("createPrologDB is not a function")
	}

	// Create PrologDB instance
	log.Info().Msg("Creating PrologDB instance")
	dbValue, err := createDBFunc(goja.Undefined())
	if err != nil {
		log.Fatal().Err(err).Msg("Failed to create PrologDB")
	}
	dbObj := dbValue.ToObject(vm)

	// Get parseClause function
	parseClauseValue := prologObj.Get("parseClause")
	parseClauseFunc, ok := goja.AssertFunction(parseClauseValue)
	if !ok {
		log.Fatal().Msg("parseClause is not a function")
	}

	// Get parseTerm function
	parseTermValue := prologObj.Get("parseTerm")
	parseTermFunc, ok := goja.AssertFunction(parseTermValue)
	if !ok {
		log.Fatal().Msg("parseTerm is not a function")
	}

	// Get formatTerm function
	formatTermValue := prologObj.Get("formatTerm")
	formatTermFunc, ok := goja.AssertFunction(formatTermValue)
	if !ok {
		log.Fatal().Msg("formatTerm is not a function")
	}

	// Test: Add a fact
	log.Info().Msg("Adding fact: (likes alice bob)")
	factClauseValue, err := parseClauseFunc(goja.Undefined(), vm.ToValue("(likes alice bob)"))
	if err != nil {
		log.Fatal().Err(err).Msg("Failed to parse clause")
	}
	factClause := factClauseValue.ToObject(vm)

	headValue := factClause.Get("head")
	bodyValue := factClause.Get("body")

	// Call addClause method
	addClauseValue := dbObj.Get("addClause")
	addClauseFunc, ok := goja.AssertFunction(addClauseValue)
	if !ok {
		log.Fatal().Msg("addClause is not a function")
	}

	_, err = addClauseFunc(dbValue, headValue, bodyValue)
	if err != nil {
		log.Fatal().Err(err).Msg("Failed to add clause")
	}
	log.Info().Msg("Fact added successfully")

	// Test: Query
	log.Info().Msg("Querying: (likes alice ?x)")
	queryValue, err := parseTermFunc(goja.Undefined(), vm.ToValue("(likes alice ?x)"))
	if err != nil {
		log.Fatal().Err(err).Msg("Failed to parse query")
	}

	// Get prove method
	proveValue := dbObj.Get("prove")
	proveFunc, ok := goja.AssertFunction(proveValue)
	if !ok {
		log.Fatal().Msg("prove is not a function")
	}

	// Create empty bindings Map using JavaScript
	bindingsValue, err := vm.RunString("new Map()")
	if err != nil {
		log.Fatal().Err(err).Msg("Failed to create Map")
	}
	bindings := bindingsValue

	// Call prove
	solutionsValue, err := proveFunc(dbValue, queryValue, bindings)
	if err != nil {
		log.Fatal().Err(err).Msg("Failed to prove query")
	}

	// Get solutions array
	solutionsArray := solutionsValue.ToObject(vm)
	solutionsLength := solutionsArray.Get("length").ToInteger()
	log.Info().Int64("count", solutionsLength).Msg("Found solutions")

	// Format and display solutions
	if solutionsLength > 0 {
		for i := int64(0); i < solutionsLength; i++ {
			solutionValue := solutionsArray.Get(fmt.Sprintf("%d", i))
			_ = solutionValue // Solution bindings (for future use)

			// Format the query with bindings applied
			formattedValue, err := formatTermFunc(goja.Undefined(), queryValue)
			if err != nil {
				log.Error().Err(err).Msg("Failed to format term")
				continue
			}
			fmt.Printf("Solution %d: %s\n", i+1, formattedValue.String())
		}
	} else {
		fmt.Println("No solutions found")
	}

	log.Info().Msg("Prolog interpreter test completed successfully")
}

func setupLogger() {
	log.Logger = log.Output(zerolog.ConsoleWriter{Out: os.Stderr}).
		With().
		Caller().
		Logger()
	zerolog.SetGlobalLevel(zerolog.DebugLevel)
}

func setupConsole(vm *goja.Runtime) {
	console := vm.NewObject()
	console.Set("log", func(call goja.FunctionCall) goja.Value {
		args := make([]interface{}, len(call.Arguments))
		for i, arg := range call.Arguments {
			args[i] = arg.Export()
		}
		fmt.Println(args...)
		return goja.Undefined()
	})
	console.Set("error", func(call goja.FunctionCall) goja.Value {
		args := make([]interface{}, len(call.Arguments))
		for i, arg := range call.Arguments {
			args[i] = arg.Export()
		}
		fmt.Fprintln(os.Stderr, "ERROR:", args)
		return goja.Undefined()
	})
	vm.Set("console", console)
}

