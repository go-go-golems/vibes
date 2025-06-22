package main

import (
	"os"
	"time"

	"github.com/dop251/goja"
	"github.com/example/goja-watermill/pkg/watermill"
	"github.com/rs/zerolog"
	"github.com/rs/zerolog/log"
)

func main() {
	// Configure logging
	zerolog.TimeFieldFormat = time.RFC3339
	log.Logger = log.Output(zerolog.ConsoleWriter{Out: os.Stderr, TimeFormat: "15:04:05"})
	logger := log.With().Str("component", "router-handler-demo").Logger()
	
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
		log.Info().Interface("js_output", args).Msg("JavaScript")
		return goja.Undefined()
	})
	vm.Set("console", console)
	
	log.Info().Msg("🚀 Starting Router Handler Demo")
	
	// Router handler JavaScript example
	jsCode := `
		console.log("=== Goja-Watermill Router Handler Demo ===");
		
		// Create a memory-based pub/sub
		console.log("Creating pub/sub...");
		const bus = watermill.createPubSub("memory");
		
		// Example 1: Simple handler without publishing
		console.log("Adding simple handler...");
		const simpleHandler = bus.addHandler({
			name: "simple_processor",
			subscribeTopic: "orders.created"
		}, function(msg) {
			const order = msg.getPayloadAsJSON();
			console.log("Processing order:", order.id, "for", order.customer);
			
			// Simulate processing
			console.log("Order processed successfully");
		});
		
		// Example 2: Handler with automatic publishing
		console.log("Adding handler with auto-publish...");
		const publishingHandler = bus.addHandler({
			name: "order_processor",
			subscribeTopic: "orders.new",
			publishTopic: "orders.processed"
		}, function(msg) {
			const order = msg.getPayloadAsJSON();
			console.log("Processing new order:", order.id);
			
			// Return message to be published automatically
			return {
				payload: JSON.stringify({
					orderId: order.id,
					status: "processed",
					processedAt: new Date().toISOString()
				}),
				metadata: {
					source: "order_processor",
					version: "1.0"
				}
			};
		});
		
		// Example 3: Handler with custom publishing logic
		console.log("Adding handler with custom publisher...");
		const customHandler = bus.addHandler({
			name: "notification_processor",
			subscribeTopic: "orders.processed",
			publishTopic: "notifications.send"
		}, function(msg) {
			const processedOrder = msg.getPayloadAsJSON();
			console.log("Creating notifications for order:", processedOrder.orderId);
			
			// Return data for custom publisher
			return {
				orderId: processedOrder.orderId,
				notifications: [
					{ type: "email", recipient: "customer@example.com" },
					{ type: "sms", recipient: "+1234567890" }
				]
			};
		}, function(handlerResult) {
			// Custom publishing logic
			const data = handlerResult;
			const messages = [];
			
			for (const notification of data.notifications) {
				messages.push({
					payload: JSON.stringify({
						orderId: data.orderId,
						type: notification.type,
						recipient: notification.recipient,
						createdAt: new Date().toISOString()
					}),
					metadata: {
						notification_type: notification.type,
						source: "notification_processor"
					}
				});
			}
			
			console.log("Publishing", messages.length, "notification messages");
			return messages;
		});
		
		// Add middleware to handlers
		console.log("Adding middleware...");
		publishingHandler.addMiddleware(function(msg) {
			console.log("Middleware: Processing message", msg.uuid);
			msg.setMetadata("processed_by_middleware", "true");
			return msg;
		});
		
		// Subscribe to see the results
		bus.subscribe("orders.processed", function(msg) {
			console.log("✅ Order processed:", msg.getPayloadAsJSON());
		});
		
		bus.subscribe("notifications.send", function(msg) {
			const notification = msg.getPayloadAsJSON();
			console.log("📧 Notification:", notification.type, "for order", notification.orderId);
		});
		
		// Start the router
		console.log("Starting router...");
		watermill.start();
		
		console.log("Publishing test messages...");
		
		// Test the handlers
		bus.publish("orders.created", JSON.stringify({
			id: "order-001",
			customer: "John Doe",
			amount: 99.99
		}));
		
		bus.publish("orders.new", JSON.stringify({
			id: "order-002",
			customer: "Jane Smith",
			amount: 149.99,
			items: ["laptop", "mouse"]
		}));
		
		console.log("=== Demo Complete ===");
	`
	
	// Execute JavaScript
	log.Info().Msg("Executing JavaScript...")
	_, err = vm.RunString(jsCode)
	if err != nil {
		log.Fatal().Err(err).Msg("JavaScript execution failed")
	}
	
	// Wait for message processing
	log.Info().Msg("Waiting for message processing...")
	time.Sleep(2 * time.Second)
	
	log.Info().Msg("✅ Router handler demo completed successfully")
}

