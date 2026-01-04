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
	// Setup zerolog with caller info
	zerolog.TimeFieldFormat = time.RFC3339
	zerolog.CallerMarshalFunc = func(pc uintptr, file string, line int) string {
		return file + ":" + string(rune(line))
	}
	log.Logger = log.Output(zerolog.ConsoleWriter{Out: os.Stdout}).With().Caller().Logger()

	log.Info().Msg("🚀 Starting Goja-Watermill CQRS Example")

	// Create Goja VM
	vm := goja.New()

	// Setup console for JavaScript
	console := vm.NewObject()
	console.Set("log", func(call goja.FunctionCall) goja.Value {
		args := make([]interface{}, len(call.Arguments))
		for i, arg := range call.Arguments {
			args[i] = arg.Export()
		}
		log.Info().Interface("args", args).Msg("JS Console")
		return goja.Undefined()
	})
	vm.Set("console", console)

	// Create Watermill module
	module, err := watermill.NewModule(vm, log.Logger)
	if err != nil {
		log.Fatal().Err(err).Msg("Failed to create Watermill module")
	}

	// JavaScript code that demonstrates CQRS patterns
	jsCode := `
		console.log("🎯 Setting up CQRS system");
		
		// Create CQRS facade
		const cqrs = watermill.createCQRS("memory");
		
		// Add command handlers (one command = one handler)
		cqrs.addCommandHandler("CreateUser", function(command) {
			console.log("👤 Processing CreateUser command:", command.payload.name, command.payload.email);
			
			// Validate command
			if (!command.payload.email || !command.payload.name) {
				throw new Error("Email and name are required");
			}
			
			// Generate user ID
			const userId = "user-" + Math.floor(Math.random() * 1000000);
			console.log("✅ User created with ID:", userId);
			
			// Publish UserCreated event
			cqrs.publishEvent({
				name: "UserCreated",
				payload: {
					userId: userId,
					email: command.payload.email,
					name: command.payload.name
				},
				metadata: {
					source: "CreateUserHandler",
					timestamp: new Date().toISOString()
				}
			});
		});
		
		cqrs.addCommandHandler("UpdateUser", function(command) {
			console.log("🔄 Processing UpdateUser command:", command.payload.userId);
			
			// Validate command
			if (!command.payload.userId) {
				throw new Error("User ID is required");
			}
			
			console.log("✅ User updated:", command.payload.userId);
			
			// Publish UserUpdated event
			cqrs.publishEvent({
				name: "UserUpdated",
				payload: {
					userId: command.payload.userId,
					changes: command.payload.changes || {}
				},
				metadata: {
					source: "UpdateUserHandler",
					timestamp: new Date().toISOString()
				}
			});
		});
		
		// Add event handlers (one event = multiple handlers)
		cqrs.addEventHandler("UserCreated", "WelcomeEmailHandler", function(event) {
			console.log("📧 Sending welcome email to:", event.payload.email);
			
			// Simulate email sending
			console.log("✅ Welcome email sent to:", event.payload.email);
			
			// Publish WelcomeEmailSent event
			cqrs.publishEvent({
				name: "WelcomeEmailSent",
				payload: {
					userId: event.payload.userId,
					email: event.payload.email,
					sentAt: new Date().toISOString()
				},
				metadata: {
					source: "WelcomeEmailHandler",
					timestamp: new Date().toISOString()
				}
			});
		});
		
		cqrs.addEventHandler("UserCreated", "AnalyticsHandler", function(event) {
			console.log("📊 Recording user creation analytics for:", event.payload.userId);
			console.log("✅ Analytics recorded for user:", event.payload.userId);
		});
		
		cqrs.addEventHandler("WelcomeEmailSent", "EmailConfirmationHandler", function(event) {
			console.log("✉️ Email delivery confirmed for:", event.payload.email);
			console.log("✅ Email confirmation processed");
		});
		
		console.log("🚀 CQRS system ready, starting Watermill");
		
		// Start Watermill
		watermill.start();
		
		console.log("📤 Sending commands");
		
		// Send commands
		cqrs.sendCommand({
			name: "CreateUser",
			payload: {
				name: "John Doe",
				email: "john.doe@example.com"
			},
			metadata: {
				source: "UserRegistration",
				timestamp: new Date().toISOString()
			}
		});
		
		cqrs.sendCommand({
			name: "CreateUser",
			payload: {
				name: "Jane Smith",
				email: "jane.smith@example.com"
			},
			metadata: {
				source: "UserRegistration",
				timestamp: new Date().toISOString()
			}
		});
		
		cqrs.sendCommand({
			name: "UpdateUser",
			payload: {
				userId: "user-123",
				changes: {
					name: "John Updated"
				}
			},
			metadata: {
				source: "UserProfile",
				timestamp: new Date().toISOString()
			}
		});
		
		console.log("✅ All commands sent");
	`

	// Execute JavaScript
	_, err = vm.RunString(jsCode)
	if err != nil {
		log.Fatal().Err(err).Msg("Failed to execute JavaScript")
	}

	// Wait for processing
	time.Sleep(2 * time.Second)

	log.Info().Msg("✅ CQRS example completed successfully")

	// Setup graceful shutdown
	sigChan := make(chan os.Signal, 1)
	signal.Notify(sigChan, syscall.SIGINT, syscall.SIGTERM)

	log.Info().Msg("🔄 Waiting for shutdown signal...")
	<-sigChan

	log.Info().Msg("🛑 Shutting down...")
	module.Close()
	log.Info().Msg("✅ Shutdown complete")
}

