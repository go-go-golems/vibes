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
	// Configure pretty logging
	log.Logger = log.Output(zerolog.ConsoleWriter{Out: os.Stdout, TimeFormat: time.RFC3339})
	logger := log.With().Str("component", "example").Logger()
	
	logger.Info().Msg("Starting Goja-Watermill Example")
	
	// Create Goja runtime
	vm := goja.New()
	
	// Create Watermill module
	module, err := watermill.NewModule(vm, logger)
	if err != nil {
		logger.Fatal().Err(err).Msg("Failed to create Watermill module")
	}
	defer module.Close()
	
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
	
	// Example JavaScript code demonstrating various features
	jsCode := `
		console.log("🚀 Goja-Watermill Example Starting");
		
		// Create multiple pub/sub instances
		const eventBus = watermill.createPubSub("memory", { 
			persistent: false,
			enable_metrics: true,
			output_channel_buffer: 128
		});
		
		const logBus = watermill.createPubSub("memory", {
			persistent: true,
			enable_metrics: true
		});
		
		console.log("Created event bus:", eventBus.getId());
		console.log("Created log bus:", logBus.getId());
		
		// Add global middleware for logging
		eventBus.useMiddleware(function(msg, next) {
			console.log("🔄 Processing event:", msg.uuid, "on topic:", msg.getMetadata("topic") || "unknown");
			msg.setMetadata("processed_by", "event_middleware");
			msg.setMetadata("processing_start", Date.now().toString());
			
			const result = next(msg);
			
			const duration = Date.now() - parseInt(msg.getMetadata("processing_start"));
			console.log("✅ Event processed in", duration, "ms");
			
			// Log to log bus
			logBus.publish("system.event_processed", {
				messageId: msg.uuid,
				duration: duration,
				topic: msg.getMetadata("topic"),
				timestamp: new Date().toISOString()
			});
			
			return result;
		});
		
		// Add authentication middleware
		eventBus.useMiddleware(function(msg, next) {
			const userId = msg.getMetadata("user_id");
			if (!userId) {
				console.log("⚠️ No user_id in message metadata");
				msg.setMetadata("authenticated", "false");
			} else {
				console.log("🔐 Authenticated user:", userId);
				msg.setMetadata("authenticated", "true");
			}
			
			return next(msg);
		});
		
		// User management handlers
		eventBus.subscribe("user.signup", function(msg) {
			const user = msg.getPayloadAsJSON();
			console.log("👤 New user signup:", user.name, "(" + user.email + ")");
			
			// Validate user data
			if (!user.email || !user.name) {
				throw new Error("Invalid user data: missing email or name");
			}
			
			// Send welcome email
			eventBus.publish("email.send", {
				to: user.email,
				template: "welcome",
				data: { name: user.name }
			}, { user_id: user.id, priority: "high" });
			
			// Track analytics
			eventBus.publish("analytics.track", {
				event: "user_signup",
				userId: user.id,
				properties: {
					email_domain: user.email.split("@")[1],
					signup_method: user.method || "direct"
				}
			}, { user_id: user.id });
		});
		
		// Email service handler
		eventBus.subscribe("email.send", function(msg) {
			const email = msg.getPayloadAsJSON();
			const authenticated = msg.getMetadata("authenticated");
			
			if (authenticated !== "true") {
				console.log("❌ Email send rejected: not authenticated");
				return;
			}
			
			console.log("📧 Sending email to:", email.to, "template:", email.template);
			
			// Simulate email sending delay
			setTimeout(function() {
				eventBus.publish("email.sent", {
					to: email.to,
					template: email.template,
					sentAt: new Date().toISOString()
				}, { user_id: msg.getMetadata("user_id") });
			}, 100);
		});
		
		// Email confirmation handler
		eventBus.subscribe("email.sent", function(msg) {
			const result = msg.getPayloadAsJSON();
			console.log("✉️ Email sent successfully to:", result.to);
		});
		
		// Analytics handler
		eventBus.subscribe("analytics.track", function(msg) {
			const event = msg.getPayloadAsJSON();
			console.log("📊 Analytics event:", event.event, "for user:", event.userId);
			
			// Store analytics data
			logBus.publish("analytics.stored", {
				event: event.event,
				userId: event.userId,
				timestamp: new Date().toISOString(),
				properties: event.properties
			});
		});
		
		// Order processing system
		eventBus.subscribe("order.created", function(msg) {
			const order = msg.getPayloadAsJSON();
			console.log("🛒 New order created:", order.id, "amount:", order.amount);
			
			// Validate order
			if (order.amount <= 0) {
				throw new Error("Invalid order amount: " + order.amount);
			}
			
			if (!order.customerId) {
				throw new Error("Missing customer ID");
			}
			
			// Process payment
			eventBus.publish("payment.process", {
				orderId: order.id,
				amount: order.amount,
				customerId: order.customerId,
				method: order.paymentMethod || "credit_card"
			}, { user_id: order.customerId });
		});
		
		// Payment processing
		eventBus.subscribe("payment.process", function(msg) {
			const payment = msg.getPayloadAsJSON();
			console.log("💳 Processing payment for order:", payment.orderId, "amount:", payment.amount);
			
			// Simulate payment processing
			setTimeout(function() {
				const success = Math.random() > 0.1; // 90% success rate
				
				if (success) {
					eventBus.publish("payment.success", {
						orderId: payment.orderId,
						amount: payment.amount,
						transactionId: "txn_" + Date.now(),
						processedAt: new Date().toISOString()
					}, { user_id: payment.customerId });
				} else {
					eventBus.publish("payment.failed", {
						orderId: payment.orderId,
						amount: payment.amount,
						reason: "Insufficient funds",
						failedAt: new Date().toISOString()
					}, { user_id: payment.customerId });
				}
			}, 200);
		});
		
		// Payment success handler
		eventBus.subscribe("payment.success", function(msg) {
			const payment = msg.getPayloadAsJSON();
			console.log("✅ Payment successful for order:", payment.orderId, "transaction:", payment.transactionId);
			
			// Fulfill order
			eventBus.publish("order.fulfill", {
				orderId: payment.orderId,
				transactionId: payment.transactionId,
				fulfillmentDate: new Date().toISOString()
			}, { user_id: msg.getMetadata("user_id") });
		});
		
		// Payment failure handler
		eventBus.subscribe("payment.failed", function(msg) {
			const failure = msg.getPayloadAsJSON();
			console.log("❌ Payment failed for order:", failure.orderId, "reason:", failure.reason);
			
			// Notify customer
			eventBus.publish("email.send", {
				to: "customer@example.com", // In real app, get from order
				template: "payment_failed",
				data: { orderId: failure.orderId, reason: failure.reason }
			}, { user_id: msg.getMetadata("user_id"), priority: "high" });
		});
		
		// Order fulfillment
		eventBus.subscribe("order.fulfill", function(msg) {
			const fulfillment = msg.getPayloadAsJSON();
			console.log("📦 Fulfilling order:", fulfillment.orderId);
			
			// Send confirmation email
			eventBus.publish("email.send", {
				to: "customer@example.com",
				template: "order_shipped",
				data: { 
					orderId: fulfillment.orderId,
					trackingNumber: "TRK" + Date.now()
				}
			}, { user_id: msg.getMetadata("user_id") });
		});
		
		// Log system handlers
		logBus.subscribe("system.event_processed", function(msg) {
			const log = msg.getPayloadAsJSON();
			console.log("📝 System log: Event", log.messageId, "processed in", log.duration + "ms");
		});
		
		logBus.subscribe("analytics.stored", function(msg) {
			const analytics = msg.getPayloadAsJSON();
			console.log("💾 Analytics stored:", analytics.event, "for user:", analytics.userId);
		});
		
		// Error handler for failed messages
		eventBus.subscribe("error.handler", function(msg) {
			const error = msg.getPayloadAsJSON();
			console.log("🚨 Error occurred:", error.message, "in handler:", error.handler);
		});
		
		// Start the router
		watermill.start();
		console.log("🎯 Router started, ready to process events");
		
		// Simulate some events
		setTimeout(function() {
			console.log("\n🎬 Starting event simulation...\n");
			
			// User signup
			eventBus.publish("user.signup", {
				id: "user_001",
				name: "Alice Johnson",
				email: "alice@example.com",
				method: "web_form"
			}, { user_id: "user_001" });
			
			// Another user signup
			setTimeout(function() {
				eventBus.publish("user.signup", {
					id: "user_002",
					name: "Bob Smith",
					email: "bob@company.com",
					method: "api"
				}, { user_id: "user_002" });
			}, 500);
			
			// Create some orders
			setTimeout(function() {
				eventBus.publish("order.created", {
					id: "order_001",
					customerId: "user_001",
					amount: 99.99,
					items: ["laptop", "mouse"],
					paymentMethod: "credit_card"
				}, { user_id: "user_001" });
				
				eventBus.publish("order.created", {
					id: "order_002",
					customerId: "user_002",
					amount: 49.99,
					items: ["book"],
					paymentMethod: "paypal"
				}, { user_id: "user_002" });
			}, 1000);
			
			// Test error handling
			setTimeout(function() {
				eventBus.publish("order.created", {
					id: "order_003",
					customerId: "user_001",
					amount: -10, // Invalid amount
					items: ["invalid_item"]
				}, { user_id: "user_001" });
			}, 1500);
			
		}, 1000);
		
		// Cleanup after demo
		setTimeout(function() {
			console.log("\n🏁 Demo completed, stopping router...");
			watermill.stop();
		}, 8000);
	`
	
	// Execute the JavaScript code
	_, err = vm.RunString(jsCode)
	if err != nil {
		logger.Fatal().Err(err).Msg("Failed to execute JavaScript code")
	}
	
	// Set up signal handling for graceful shutdown
	sigChan := make(chan os.Signal, 1)
	signal.Notify(sigChan, syscall.SIGINT, syscall.SIGTERM)
	
	logger.Info().Msg("Example running... Press Ctrl+C to exit")
	
	// Wait for signal or timeout
	select {
	case sig := <-sigChan:
		logger.Info().Str("signal", sig.String()).Msg("Received signal, shutting down")
	case <-time.After(15 * time.Second):
		logger.Info().Msg("Example timeout reached")
	}
	
	logger.Info().Msg("Example completed")
}

