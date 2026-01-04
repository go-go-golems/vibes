package main

import (
	"context"
	"os"
	"os/signal"
	"syscall"
	"time"

	"github.com/dop251/goja"
	"github.com/example/goja-watermill/pkg/watermill"
	"github.com/rs/zerolog"
	"github.com/rs/zerolog/log"
)

func main() {
	// Configure zerolog
	zerolog.TimeFieldFormat = time.RFC3339
	log.Logger = log.Output(zerolog.ConsoleWriter{Out: os.Stdout, TimeFormat: time.RFC3339})
	
	logger := log.With().Str("component", "main").Logger()
	logger.Info().Msg("Starting Goja-Watermill integration demo")
	
	// Create Goja runtime
	vm := goja.New()
	
	// Create Watermill module
	module, err := watermill.NewModule(vm, logger)
	if err != nil {
		logger.Fatal().Err(err).Msg("Failed to create Watermill module")
	}
	defer module.Close()
	
	// Load and run JavaScript code
	jsCode := `
		console.log("JavaScript runtime initialized");
		
		// Create an in-memory pub/sub
		const bus = watermill.createPubSub("memory", { persistent: false });
		console.log("Created memory pub/sub:", bus.getId());
		
		// Add middleware
		bus.useMiddleware(function(msg, next) {
			console.log("Middleware: Processing message", msg.uuid, "on topic", msg.getMetadata("topic"));
			msg.setMetadata("processed_by", "middleware");
			const result = next(msg);
			console.log("Middleware: Completed processing message", msg.uuid);
			return result;
		});
		
		// Subscribe to a topic
		const handler = bus.subscribe("test.topic", function(msg) {
			console.log("Handler: Received message", msg.uuid);
			console.log("Handler: Payload:", msg.payload);
			console.log("Handler: Metadata:", JSON.stringify(msg.metadata));
			
			// Try to parse as JSON
			try {
				const data = msg.getPayloadAsJSON();
				console.log("Handler: Parsed JSON:", JSON.stringify(data));
			} catch (e) {
				console.log("Handler: Payload is not JSON");
			}
		});
		
		console.log("Subscribed handler:", handler.id);
		
		// Start the router
		watermill.start();
		console.log("Router started");
		
		// Publish some test messages
		setTimeout(function() {
			console.log("Publishing test messages...");
			
			// Publish a string message
			bus.publish("test.topic", "Hello, World!");
			
			// Publish a JSON message
			bus.publish("test.topic", { message: "Hello from JSON", timestamp: new Date().toISOString() });
			
			// Publish with metadata
			bus.publish("test.topic", "Message with metadata", { priority: "high", source: "demo" });
			
		}, 1000);
		
		// Set up cleanup after 5 seconds
		setTimeout(function() {
			console.log("Cleaning up...");
			handler.stop();
			watermill.stop();
		}, 5000);
	`
	
	// Set up console.log for JavaScript
	console := vm.NewObject()
	console.Set("log", func(call goja.FunctionCall) goja.Value {
		args := make([]interface{}, len(call.Arguments))
		for i, arg := range call.Arguments {
			args[i] = arg.Export()
		}
		logger.Info().Interface("js_args", args).Msg("JS Console")
		return goja.Undefined()
	})
	vm.Set("console", console)
	
	// Set up setTimeout for JavaScript
	vm.Set("setTimeout", func(call goja.FunctionCall) goja.Value {
		if len(call.Arguments) < 2 {
			panic(vm.NewTypeError("setTimeout requires 2 arguments"))
		}
		
		fn, ok := goja.AssertFunction(call.Arguments[0])
		if !ok {
			panic(vm.NewTypeError("first argument must be a function"))
		}
		
		delay := call.Arguments[1].ToInteger()
		
		go func() {
			time.Sleep(time.Duration(delay) * time.Millisecond)
			_, err := fn(goja.Undefined())
			if err != nil {
				logger.Error().Err(err).Msg("setTimeout callback error")
			}
		}()
		
		return goja.Undefined()
	})
	
	// Run the JavaScript code
	_, err = vm.RunString(jsCode)
	if err != nil {
		logger.Fatal().Err(err).Msg("Failed to run JavaScript code")
	}
	
	// Set up signal handling
	sigChan := make(chan os.Signal, 1)
	signal.Notify(sigChan, syscall.SIGINT, syscall.SIGTERM)
	
	logger.Info().Msg("Demo running... Press Ctrl+C to exit")
	
	// Wait for signal or timeout
	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()
	
	select {
	case sig := <-sigChan:
		logger.Info().Str("signal", sig.String()).Msg("Received signal, shutting down")
	case <-ctx.Done():
		logger.Info().Msg("Demo timeout reached")
	}
	
	logger.Info().Msg("Demo completed")
}

