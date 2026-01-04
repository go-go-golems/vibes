package main

import (
	"embed"
	"fmt"
	"github.com/dop251/goja"
	"github.com/dop251/goja_nodejs/require"
	"github.com/example/myvm/internal/handlers"
	"github.com/fsnotify/fsnotify"
	"github.com/rs/zerolog"
	"github.com/rs/zerolog/log"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
)

//go:embed assets/app.js
var jsBundle embed.FS

// setupLogger configures zerolog with caller information for debugging
func setupLogger() {
	// Configure zerolog to include caller information
	log.Logger = log.Output(zerolog.ConsoleWriter{Out: os.Stderr}).
		With().
		Caller().
		Logger()

	// Set global log level
	zerolog.SetGlobalLevel(zerolog.DebugLevel)
}

// isDevelopmentMode returns true if the app is running in development mode
func isDevelopmentMode() bool {
	return os.Getenv("APP_ENV") == "development"
}

// runEsbuild runs esbuild to compile TypeScript to JavaScript
func runEsbuild() error {
	cmd := exec.Command("npx", "esbuild", 
		"app.ts", 
		"--bundle", 
		"--format=cjs", 
		"--platform=node", 
		"--target=es2019", 
		"--sourcemap=inline",
		"--outfile=app.js")
	
	cmd.Dir = filepath.Join(".", "web")
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	
	log.Info().Msg("Running esbuild to compile TypeScript")
	return cmd.Run()
}

// watchForChanges watches for file changes and triggers hot reload
func watchForChanges(registry *require.Registry, vm *goja.Runtime) error {
	watcher, err := fsnotify.NewWatcher()
	if err != nil {
		return err
	}
	defer watcher.Close()

	// Watch TypeScript files
	err = watcher.Add(filepath.Join(".", "web"))
	if err != nil {
		return err
	}

	log.Info().Msg("Watching for file changes (hot reload enabled)")

	for {
		select {
		case event, ok := <-watcher.Events:
			if !ok {
				return nil
			}
			
			// Only react to TypeScript file changes
			if !strings.HasSuffix(event.Name, ".ts") {
				continue
			}
			
			if event.Op&(fsnotify.Write|fsnotify.Create) != 0 {
				log.Info().Str("file", event.Name).Msg("File changed, rebuilding")
				
				// Rebuild TypeScript
				err := runEsbuild()
				if err != nil {
					log.Error().Err(err).Msg("Failed to rebuild TypeScript")
					continue
				}
				
				// Reload the module in the VM
				log.Info().Msg("Hot reloading JavaScript module")
				// Note: In newer versions of goja_nodejs, we need to create a new registry
				// for module reloading instead of using RequireNew
				newReg := require.NewRegistry(require.WithLoader(func(path string) ([]byte, error) {
					log.Debug().Str("path", path).Msg("Loading module from filesystem")
					
					// Check if the file exists in the web directory
					fullPath := filepath.Join(".", "web", path)
					data, err := os.ReadFile(fullPath)
					if err != nil {
						_, file, line, _ := runtime.Caller(1)
						log.Error().
							Err(err).
							Str("file", file).
							Int("line", line).
							Str("requestedPath", path).
							Str("fullPath", fullPath).
							Msg("Failed to load module from filesystem")
						return nil, err
					}
					return data, nil
				}))
				
				// Enable Node.js polyfills
				newReg.Enable(vm)
				
				// Re-expose Go handlers
				if err := vm.Set("CreateUser", handlers.CreateUser); err != nil {
					log.Error().Err(err).Msg("Failed to re-expose CreateUser function")
					return nil
				}
				
				// Execute the module using the package-level Require function
				// Note: require.Require returns a single value, not (value, error)
				func() {
					defer func() {
						if r := recover(); r != nil {
							log.Error().
								Interface("panic", r).
								Msg("JavaScript execution failed with panic")
						}
					}()
					_ = require.Require(vm, "app.js")
				}()
				log.Info().Msg("Module reloaded successfully")
			}
		case err, ok := <-watcher.Errors:
			if !ok {
				return nil
			}
			log.Error().Err(err).Msg("Watcher error")
		}
	}
}

func main() {
	// Set up zerolog with caller info
	setupLogger()
	log.Info().Msg("Starting application")

	// Create a new JavaScript VM
	vm := goja.New()
	log.Debug().Msg("JavaScript VM created")

	// Determine if we're in development mode
	devMode := isDevelopmentMode()
	log.Info().Bool("development_mode", devMode).Msg("Application mode")

	// Set up the module loader based on mode
	var reg *require.Registry
	
	if devMode {
		// In development mode, load from the filesystem
		reg = require.NewRegistry(require.WithLoader(func(path string) ([]byte, error) {
			log.Debug().Str("path", path).Msg("Loading module from filesystem")
			
			// Check if the file exists in the web directory
			fullPath := filepath.Join(".", "web", path)
			data, err := os.ReadFile(fullPath)
			if err != nil {
				_, file, line, _ := runtime.Caller(1)
				log.Error().
					Err(err).
					Str("file", file).
					Int("line", line).
					Str("requestedPath", path).
					Str("fullPath", fullPath).
					Msg("Failed to load module from filesystem")
				return nil, err
			}
			return data, nil
		}))
		
		// Compile TypeScript on startup
		if err := runEsbuild(); err != nil {
			log.Fatal().Err(err).Msg("Failed to compile TypeScript")
		}
	} else {
		// In production mode, load from embedded bundle
		reg = require.NewRegistry(require.WithLoader(func(path string) ([]byte, error) {
			log.Debug().Str("path", path).Msg("Loading module from embedded bundle")
			
			// Handle direct app.js request or node_modules/app.js request
			var fullPath string
			if path == "app.js" || path == "node_modules/app.js" {
				fullPath = "assets/app.js"
			} else {
				// For other modules, use the standard path
				fullPath = "../../web/" + path
			}
			
			log.Debug().Str("path", path).Str("fullPath", fullPath).Msg("Resolving module path")
			
			data, err := jsBundle.ReadFile(fullPath)
			if err != nil {
				// Log the error with caller information
				_, file, line, _ := runtime.Caller(1)
				log.Error().
					Err(err).
					Str("file", file).
					Int("line", line).
					Str("requestedPath", path).
					Str("fullPath", fullPath).
					Msg("Failed to load module from embedded bundle")
				return nil, err
			}
			return data, nil
		}))
	}

	// Hook Node polyfills (fs, path, etc.)
	reg.Enable(vm)
	log.Debug().Msg("Node.js polyfills enabled")

	// Expose Go handlers to JavaScript
	if err := vm.Set("CreateUser", handlers.CreateUser); err != nil {
		log.Fatal().
			Err(err).
			Msg("Failed to expose CreateUser function to JavaScript")
	}
	log.Debug().Msg("Go handlers exposed to JavaScript")

	// Execute the JavaScript bundle using the package-level Require function
	log.Info().Msg("Executing JavaScript bundle")
	
	// Note: require.Require returns a single value and panics on error
	// We need to recover from potential panics
	var result goja.Value
	func() {
		defer func() {
			if r := recover(); r != nil {
				log.Error().
					Interface("panic", r).
					Msg("JavaScript execution failed with panic")
				// Print the panic details for debugging
				fmt.Printf("Panic details: %v\n", r)
			}
		}()
		
		// Define console object by binding Go functions
		console := vm.NewObject()
		err := console.Set("log", func(call goja.FunctionCall) goja.Value {
			args := make([]interface{}, len(call.Arguments))
			for i, arg := range call.Arguments {
				args[i] = arg.Export()
			}
			fmt.Println(args...)
			return goja.Undefined()
		})
		if err != nil {
			log.Error().Err(err).Msg("Failed to set console.log")
			return
		}
		
		err = console.Set("error", func(call goja.FunctionCall) goja.Value {
			args := make([]interface{}, len(call.Arguments))
			for i, arg := range call.Arguments {
				args[i] = arg.Export()
			}
			fmt.Println("ERROR:", args)
			return goja.Undefined()
		})
		if err != nil {
			log.Error().Err(err).Msg("Failed to set console.error")
			return
		}
		
		err = vm.Set("console", console)
		if err != nil {
			log.Error().Err(err).Msg("Failed to define console object")
			return
		}
		
		// Try to execute the module directly
		result, err = vm.RunString(`
			try {
				// Call the Go function exposed to our VM
				const user = CreateUser({
					username: "testuser",
					email: "test@example.com"
				});
				
				console.log("User created successfully:");
				console.log("ID: " + user.id);
				console.log("Username: " + user.username);
				console.log("Email: " + user.email);
				console.log("Created at: " + user.createdAt);
				console.log("Active: " + user.isActive);
				
				({ success: true, user: user });
			} catch (error) {
				console.error("Error creating user:", error);
				({ success: false, error: String(error) });
			}
		`)
		if err != nil {
			log.Error().Err(err).Msg("Failed to execute JavaScript")
			return
		}
	}()

	// Print the result
	fmt.Println("Execution completed with result:", result)
	log.Info().
		Interface("result", result).
		Msg("JavaScript execution completed")
		
	// In development mode, watch for file changes and hot reload
	if devMode {
		log.Info().Msg("Starting file watcher for hot reload")
		if err := watchForChanges(reg, vm); err != nil {
			log.Fatal().Err(err).Msg("Failed to start file watcher")
		}
	} else {
		log.Info().Msg("Hot reload disabled in production mode")
	}
}
