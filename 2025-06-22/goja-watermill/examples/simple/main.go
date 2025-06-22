package main

import (
	"time"

	"github.com/dop251/goja"
	"github.com/example/goja-watermill/pkg/watermill"
	"github.com/rs/zerolog/log"
)

func main() {
	// Configure simple logging
	logger := log.With().Str("component", "simple-example").Logger()
	
	// Create Goja runtime
	vm := goja.New()
	
	// Create Watermill module
	module, err := watermill.NewModule(vm, logger)
	if err != nil {
		log.Fatal().Err(err).Msg("Failed to create module")
	}
	defer module.Close()
	
	// Set up console.log
	console := vm.NewObject()
	console.Set("log", func(call goja.FunctionCall) goja.Value {
		args := make([]interface{}, len(call.Arguments))
		for i, arg := range call.Arguments {
			args[i] = arg.Export()
		}
		log.Info().Interface("args", args).Msg("JS")
		return goja.Undefined()
	})
	vm.Set("console", console)
	
	// Simple JavaScript example
	jsCode := `
		console.log("Creating pub/sub...");
		
		// Create a memory-based pub/sub
		const bus = watermill.createPubSub("memory");
		
		// Subscribe to a topic
		bus.subscribe("greetings", function(msg) {
			console.log("Received greeting:", msg.payload);
		});
		
		// Start the router
		watermill.start();
		
		// Publish a message
		bus.publish("greetings", "Hello, Watermill!");
		
		console.log("Message published");
	`
	
	// Execute JavaScript
	_, err = vm.RunString(jsCode)
	if err != nil {
		log.Fatal().Err(err).Msg("JavaScript execution failed")
	}
	
	// Wait for message processing
	time.Sleep(500 * time.Millisecond)
	
	log.Info().Msg("Simple example completed")
}

