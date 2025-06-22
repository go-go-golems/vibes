package main

import (
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
	// Configure logging
	zerolog.TimeFieldFormat = zerolog.TimeFormatUnix
	log.Logger = log.Output(zerolog.ConsoleWriter{Out: os.Stderr})

	// Create Goja runtime
	vm := goja.New()

	// Create Watermill module
	module, err := watermill.NewModule(vm, log.Logger)
	if err != nil {
		log.Fatal().Err(err).Msg("Failed to create watermill module")
	}
	defer module.Close()

	// Set up console for JavaScript
	console := vm.NewObject()
	console.Set("log", func(args ...interface{}) {
		log.Info().Interface("args", args).Msg("JS Console")
	})
	console.Set("error", func(args ...interface{}) {
		log.Error().Interface("args", args).Msg("JS Console Error")
	})
	vm.Set("console", console)

	// Execute CQRS example
	jsCode := `
		// Create CQRS facade
		const cqrs = watermill.createCQRS("memory", {
			commandsTopic: "commands",
			eventsTopic: "events"
		});

		// Add command handlers
		cqrs.addCommandHandler("CreateUser", function(cmd) {
			console.log("Processing CreateUser command:", cmd.name);
			const payload = cmd.getPayloadAsJSON();
			
			// Validate command
			if (!payload.email || !payload.name) {
				throw new Error("Invalid user data: email and name are required");
			}
			
			console.log("Creating user:", payload.name, "with email:", payload.email);
			
			// Publish UserCreated event
			cqrs.publishEvent("UserCreated", {
				userId: "user-" + Math.random().toString(36).substr(2, 9),
				email: payload.email,
				name: payload.name,
				createdAt: new Date().toISOString()
			}, {
				source: "CreateUserHandler",
				version: "1.0"
			});
		});

		cqrs.addCommandHandler("UpdateUser", function(cmd) {
			console.log("Processing UpdateUser command:", cmd.name);
			const payload = cmd.getPayloadAsJSON();
			
			console.log("Updating user:", payload.userId);
			
			// Publish UserUpdated event
			cqrs.publishEvent("UserUpdated", {
				userId: payload.userId,
				changes: payload.changes,
				updatedAt: new Date().toISOString()
			});
		});

		// Add event handlers
		cqrs.addEventHandler("UserCreated", function(event) {
			console.log("✅ UserCreated event received:", event.name);
			const payload = event.getPayloadAsJSON();
			
			console.log("📧 Sending welcome email to:", payload.email);
			console.log("👤 User profile created for:", payload.name);
			
			// Publish WelcomeEmailSent event
			cqrs.publishEvent("WelcomeEmailSent", {
				userId: payload.userId,
				email: payload.email,
				sentAt: new Date().toISOString()
			});
		});

		cqrs.addEventHandler("UserCreated", function(event) {
			console.log("📊 Analytics: Recording user creation");
			const payload = event.getPayloadAsJSON();
			
			console.log("📈 User registration tracked for analytics");
		});

		cqrs.addEventHandler("UserUpdated", function(event) {
			console.log("✅ UserUpdated event received:", event.name);
			const payload = event.getPayloadAsJSON();
			
			console.log("🔄 User cache invalidated for:", payload.userId);
			console.log("📝 Audit log updated with changes:", JSON.stringify(payload.changes));
		});

		cqrs.addEventHandler("WelcomeEmailSent", function(event) {
			console.log("✅ WelcomeEmailSent event received:", event.name);
			const payload = event.getPayloadAsJSON();
			
			console.log("📧 Email delivery confirmed for:", payload.email);
		});

		// Start CQRS
		cqrs.start();
		
		// Start the router
		watermill.start();

		console.log("🚀 CQRS system started - sending commands...");

		// Send commands immediately
		console.log("📤 Sending CreateUser command...");
		cqrs.sendCommand("CreateUser", {
			email: "john.doe@example.com",
			name: "John Doe"
		}, {
			requestId: "req-001",
			source: "web-app"
		});

		console.log("📤 Sending another CreateUser command...");
		cqrs.sendCommand("CreateUser", {
			email: "jane.smith@example.com",
			name: "Jane Smith"
		});

		console.log("📤 Sending UpdateUser command...");
		cqrs.sendCommand("UpdateUser", {
			userId: "user-123",
			changes: {
				name: "John Doe Jr.",
				phone: "+1-555-0123"
			}
		});
	`

	_, err = vm.RunString(jsCode)
	if err != nil {
		log.Fatal().Err(err).Msg("Failed to execute JavaScript")
	}

	// Wait for processing
	time.Sleep(2 * time.Second)

	// Set up graceful shutdown
	sigChan := make(chan os.Signal, 1)
	signal.Notify(sigChan, syscall.SIGINT, syscall.SIGTERM)

	log.Info().Msg("🎯 CQRS example running - Press Ctrl+C to stop")

	// Wait for shutdown signal
	<-sigChan
	log.Info().Msg("🛑 Shutting down CQRS example...")
}

