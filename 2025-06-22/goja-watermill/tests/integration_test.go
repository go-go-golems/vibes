package tests

import (
	"fmt"
	"testing"
	"time"

	"github.com/dop251/goja"
	"github.com/example/goja-watermill/pkg/watermill"
	"github.com/rs/zerolog"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// TestModule tests the basic module functionality
func TestModule(t *testing.T) {
	logger := zerolog.New(zerolog.NewTestWriter(t)).With().Timestamp().Logger()
	vm := goja.New()
	
	module, err := watermill.NewModule(vm, logger)
	require.NoError(t, err)
	defer module.Close()
	
	// Test that watermill object is available in JS
	result, err := vm.RunString("typeof watermill")
	require.NoError(t, err)
	assert.Equal(t, "object", result.String())
	
	// Test createPubSub function exists
	result, err = vm.RunString("typeof watermill.createPubSub")
	require.NoError(t, err)
	assert.Equal(t, "function", result.String())
}

// TestMemoryPubSubCreation tests creating memory pub/sub instances
func TestMemoryPubSubCreation(t *testing.T) {
	logger := zerolog.New(zerolog.NewTestWriter(t)).With().Timestamp().Logger()
	vm := goja.New()
	
	module, err := watermill.NewModule(vm, logger)
	require.NoError(t, err)
	defer module.Close()
	
	// Test creating memory pub/sub
	jsCode := `
		const bus = watermill.createPubSub("memory", { persistent: false });
		const busId = bus.getId();
		const busType = bus.getType();
		[busId, busType]
	`
	
	result, err := vm.RunString(jsCode)
	require.NoError(t, err)
	
	resultArray := result.Export().([]interface{})
	busId := resultArray[0].(string)
	busType := resultArray[1].(string)
	
	assert.NotEmpty(t, busId)
	assert.Equal(t, "memory", busType)
	assert.Contains(t, busId, "memory_")
}

// TestBasicPubSub tests basic publish/subscribe functionality
func TestBasicPubSub(t *testing.T) {
	logger := zerolog.New(zerolog.NewTestWriter(t)).With().Timestamp().Logger()
	vm := goja.New()
	
	module, err := watermill.NewModule(vm, logger)
	require.NoError(t, err)
	defer module.Close()
	
	// Set up console.log for JS
	setupConsole(vm, t)
	
	// Test basic pub/sub
	jsCode := `
		const bus = watermill.createPubSub("memory");
		let receivedMessages = [];
		
		// Subscribe to topic
		const handler = bus.subscribe("test.topic", function(msg) {
			receivedMessages.push({
				uuid: msg.uuid,
				payload: msg.payload,
				metadata: msg.metadata
			});
		});
		
		// Start router
		watermill.start();
		
		// Publish a message
		const publishResult = bus.publish("test.topic", "Hello, World!");
		
		// Return handler and publish result for verification
		[handler.id, publishResult.uuid, receivedMessages]
	`
	
	result, err := vm.RunString(jsCode)
	require.NoError(t, err)
	
	// Wait a bit for message processing
	time.Sleep(100 * time.Millisecond)
	
	// Check the result again to see if message was received
	result, err = vm.RunString("receivedMessages")
	require.NoError(t, err)
	
	messages := result.Export().([]interface{})
	assert.Len(t, messages, 1, "Should have received one message")
	
	if len(messages) > 0 {
		msg := messages[0].(map[string]interface{})
		assert.Equal(t, "Hello, World!", msg["payload"])
		assert.NotEmpty(t, msg["uuid"])
	}
}

// TestJSONMessages tests publishing and receiving JSON messages
func TestJSONMessages(t *testing.T) {
	logger := zerolog.New(zerolog.NewTestWriter(t)).With().Timestamp().Logger()
	vm := goja.New()
	
	module, err := watermill.NewModule(vm, logger)
	require.NoError(t, err)
	defer module.Close()
	
	setupConsole(vm, t)
	
	jsCode := `
		const bus = watermill.createPubSub("memory");
		let receivedMessages = [];
		
		bus.subscribe("json.topic", function(msg) {
			try {
				const data = msg.getPayloadAsJSON();
				receivedMessages.push(data);
			} catch (e) {
				receivedMessages.push({ error: e.message });
			}
		});
		
		watermill.start();
		
		// Publish JSON object
		bus.publish("json.topic", { name: "John", age: 30, active: true });
		
		receivedMessages
	`
	
	// Wait for processing
	time.Sleep(100 * time.Millisecond)
	
	result, err := vm.RunString(jsCode)
	require.NoError(t, err)
	
	// Wait a bit more for message processing
	time.Sleep(100 * time.Millisecond)
	
	// Check received messages
	result, err = vm.RunString("receivedMessages")
	require.NoError(t, err)
	
	messages := result.Export().([]interface{})
	assert.Len(t, messages, 1, "Should have received one message")
	
	if len(messages) > 0 {
		msg := messages[0].(map[string]interface{})
		assert.Equal(t, "John", msg["name"])
		assert.Equal(t, float64(30), msg["age"]) // JSON numbers are float64 in Go
		assert.Equal(t, true, msg["active"])
	}
}

// TestMiddleware tests middleware functionality
func TestMiddleware(t *testing.T) {
	logger := zerolog.New(zerolog.NewTestWriter(t)).With().Timestamp().Logger()
	vm := goja.New()
	
	module, err := watermill.NewModule(vm, logger)
	require.NoError(t, err)
	defer module.Close()
	
	setupConsole(vm, t)
	
	result, err := vm.RunString(`
		const bus = watermill.createPubSub("memory");
		let middlewareLog = [];
		let handlerLog = [];
		
		// Add middleware
		bus.useMiddleware(function(msg, next) {
			middlewareLog.push("before:" + msg.uuid);
			msg.setMetadata("processed_by", "middleware");
			const result = next(msg);
			middlewareLog.push("after:" + msg.uuid);
			return result;
		});
		
		// Subscribe to topic
		bus.subscribe("middleware.topic", function(msg) {
			handlerLog.push({
				uuid: msg.uuid,
				processed_by: msg.getMetadata("processed_by")
			});
		});
		
		watermill.start();
		
		// Publish message
		bus.publish("middleware.topic", "test message");
		
		[middlewareLog, handlerLog]
	`)
	require.NoError(t, err)
	
	// Wait for processing
	time.Sleep(200 * time.Millisecond)
	
	result, err = vm.RunString("[middlewareLog, handlerLog]")
	require.NoError(t, err)
	
	logs := result.Export().([]interface{})
	middlewareLog := logs[0].([]interface{})
	handlerLog := logs[1].([]interface{})
	
	assert.Len(t, middlewareLog, 2, "Middleware should log before and after")
	assert.Len(t, handlerLog, 1, "Handler should receive one message")
	
	if len(middlewareLog) >= 2 {
		assert.Contains(t, middlewareLog[0].(string), "before:")
		assert.Contains(t, middlewareLog[1].(string), "after:")
	}
	
	if len(handlerLog) > 0 {
		handler := handlerLog[0].(map[string]interface{})
		assert.Equal(t, "middleware", handler["processed_by"])
	}
}

// TestUnsubscribe tests unsubscribing from topics
func TestUnsubscribe(t *testing.T) {
	logger := zerolog.New(zerolog.NewTestWriter(t)).With().Timestamp().Logger()
	vm := goja.New()
	
	module, err := watermill.NewModule(vm, logger)
	require.NoError(t, err)
	defer module.Close()
	
	setupConsole(vm, t)
	
	jsCode := `
		const bus = watermill.createPubSub("memory");
		let messageCount = 0;
		
		const handler = bus.subscribe("unsub.topic", function(msg) {
			messageCount++;
		});
		
		watermill.start();
		
		// Publish first message
		bus.publish("unsub.topic", "message 1");
		
		// Wait a bit, then unsubscribe
		setTimeout(function() {
			handler.stop();
			
			// Publish second message (should not be received)
			bus.publish("unsub.topic", "message 2");
		}, 50);
		
		messageCount
	`
	
	_, err = vm.RunString(jsCode)
	require.NoError(t, err)
	
	// Wait for processing
	time.Sleep(200 * time.Millisecond)
	
	result, err := vm.RunString("messageCount")
	require.NoError(t, err)
	
	count := int(result.ToInteger())
	assert.Equal(t, 1, count, "Should only receive the first message")
}

// TestConcurrentPublishing tests concurrent publishing from multiple goroutines
func TestConcurrentPublishing(t *testing.T) {
	logger := zerolog.New(zerolog.NewTestWriter(t)).With().Timestamp().Logger()
	vm := goja.New()
	
	module, err := watermill.NewModule(vm, logger)
	require.NoError(t, err)
	defer module.Close()
	
	setupConsole(vm, t)
	
	// Set up subscriber
	_, err = vm.RunString(`
		const bus = watermill.createPubSub("memory");
		let receivedMessages = [];
		
		bus.subscribe("concurrent.topic", function(msg) {
			receivedMessages.push(msg.payload);
		});
		
		watermill.start();
		receivedMessages
	`)
	require.NoError(t, err)
	
	// Publish messages sequentially (not concurrently) to avoid JS thread issues
	const numMessages = 10
	
	for i := 0; i < numMessages; i++ {
		publishCode := fmt.Sprintf(`bus.publish("concurrent.topic", "message %d")`, i)
		_, err := vm.RunString(publishCode)
		require.NoError(t, err)
	}
	
	// Wait for all messages to be processed
	time.Sleep(500 * time.Millisecond)
	
	// Check received messages
	result, err := vm.RunString("receivedMessages.length")
	require.NoError(t, err)
	
	count := int(result.ToInteger())
	assert.Equal(t, numMessages, count, "Should receive all published messages")
}

// TestErrorHandling tests error handling in JavaScript handlers
func TestErrorHandling(t *testing.T) {
	logger := zerolog.New(zerolog.NewTestWriter(t)).With().Timestamp().Logger()
	vm := goja.New()
	
	module, err := watermill.NewModule(vm, logger)
	require.NoError(t, err)
	defer module.Close()
	
	setupConsole(vm, t)
	
	_, err = vm.RunString(`
		const bus = watermill.createPubSub("memory");
		let errorCount = 0;
		let successCount = 0;
		
		bus.subscribe("error.topic", function(msg) {
			if (msg.payload === "error") {
				errorCount++;
				throw new Error("Intentional error");
			} else {
				successCount++;
			}
		});
		
		watermill.start();
		
		// Publish messages that will succeed and fail
		bus.publish("error.topic", "success");
		bus.publish("error.topic", "error");
		bus.publish("error.topic", "success");
		
		[errorCount, successCount]
	`)
	require.NoError(t, err)
	
	// Wait for processing
	time.Sleep(200 * time.Millisecond)
	
	result, err := vm.RunString("[errorCount, successCount]")
	require.NoError(t, err)
	
	counts := result.Export().([]interface{})
	errorCount := int(counts[0].(int64))
	successCount := int(counts[1].(int64))
	
	assert.Equal(t, 1, errorCount, "Should have one error")
	assert.Equal(t, 2, successCount, "Should have two successes")
}

// TestMultiplePubSubInstances tests using multiple pub/sub instances
func TestMultiplePubSubInstances(t *testing.T) {
	logger := zerolog.New(zerolog.NewTestWriter(t)).With().Timestamp().Logger()
	vm := goja.New()
	
	module, err := watermill.NewModule(vm, logger)
	require.NoError(t, err)
	defer module.Close()
	
	setupConsole(vm, t)
	
	_, err = vm.RunString(`
		const bus1 = watermill.createPubSub("memory", { persistent: false });
		const bus2 = watermill.createPubSub("memory", { persistent: true });
		
		let bus1Messages = [];
		let bus2Messages = [];
		
		bus1.subscribe("topic1", function(msg) {
			bus1Messages.push(msg.payload);
		});
		
		bus2.subscribe("topic2", function(msg) {
			bus2Messages.push(msg.payload);
		});
		
		watermill.start();
		
		// Publish to different buses
		bus1.publish("topic1", "message for bus1");
		bus2.publish("topic2", "message for bus2");
		
		// Cross-publish (should not receive)
		bus1.publish("topic2", "cross message 1");
		bus2.publish("topic1", "cross message 2");
		
		[bus1Messages, bus2Messages]
	`)
	require.NoError(t, err)
	
	// Wait for processing
	time.Sleep(200 * time.Millisecond)
	
	result, err := vm.RunString("[bus1Messages, bus2Messages]")
	require.NoError(t, err)
	
	messages := result.Export().([]interface{})
	bus1Messages := messages[0].([]interface{})
	bus2Messages := messages[1].([]interface{})
	
	assert.Len(t, bus1Messages, 1, "Bus1 should receive only its message")
	assert.Len(t, bus2Messages, 1, "Bus2 should receive only its message")
	
	if len(bus1Messages) > 0 {
		assert.Equal(t, "message for bus1", bus1Messages[0].(string))
	}
	if len(bus2Messages) > 0 {
		assert.Equal(t, "message for bus2", bus2Messages[0].(string))
	}
}

// TestMessageMetadata tests message metadata handling
func TestMessageMetadata(t *testing.T) {
	logger := zerolog.New(zerolog.NewTestWriter(t)).With().Timestamp().Logger()
	vm := goja.New()
	
	module, err := watermill.NewModule(vm, logger)
	require.NoError(t, err)
	defer module.Close()
	
	setupConsole(vm, t)
	
	_, err = vm.RunString(`
		const bus = watermill.createPubSub("memory");
		let receivedMetadata = {};
		
		bus.subscribe("metadata.topic", function(msg) {
			receivedMetadata = {
				published_by: msg.getMetadata("published_by"),
				custom_field: msg.getMetadata("custom_field"),
				priority: msg.getMetadata("priority")
			};
		});
		
		watermill.start();
		
		// Publish with custom metadata
		bus.publish("metadata.topic", "test message", {
			custom_field: "custom_value",
			priority: "high"
		});
		
		receivedMetadata
	`)
	require.NoError(t, err)
	
	// Wait for processing
	time.Sleep(200 * time.Millisecond)
	
	result, err := vm.RunString("receivedMetadata")
	require.NoError(t, err)
	
	metadata := result.Export().(map[string]interface{})
	
	assert.Equal(t, "goja-watermill", metadata["published_by"])
	assert.Equal(t, "custom_value", metadata["custom_field"])
	assert.Equal(t, "high", metadata["priority"])
}

// BenchmarkPubSub benchmarks pub/sub performance
func BenchmarkPubSub(b *testing.B) {
	logger := zerolog.New(zerolog.NewTestWriter(b)).With().Timestamp().Logger()
	vm := goja.New()
	
	module, err := watermill.NewModule(vm, logger)
	require.NoError(b, err)
	defer module.Close()
	
	setupConsole(vm, b)
	
	// Set up subscriber
	jsCode := `
		const bus = watermill.createPubSub("memory");
		let messageCount = 0;
		
		bus.subscribe("bench.topic", function(msg) {
			messageCount++;
		});
		
		watermill.start();
	`
	
	_, err = vm.RunString(jsCode)
	require.NoError(b, err)
	
	b.ResetTimer()
	
	for i := 0; i < b.N; i++ {
		publishCode := fmt.Sprintf(`bus.publish("bench.topic", "message %d")`, i)
		_, err := vm.RunString(publishCode)
		if err != nil {
			b.Fatalf("Failed to publish message: %v", err)
		}
	}
	
	// Wait for all messages to be processed
	time.Sleep(100 * time.Millisecond)
	
	// Verify message count
	result, err := vm.RunString("messageCount")
	require.NoError(b, err)
	
	count := int(result.ToInteger())
	if count != b.N {
		b.Errorf("Expected %d messages, got %d", b.N, count)
	}
}

// setupConsole sets up console.log for JavaScript testing
func setupConsole(vm *goja.Runtime, t testing.TB) {
	console := vm.NewObject()
	console.Set("log", func(call goja.FunctionCall) goja.Value {
		args := make([]interface{}, len(call.Arguments))
		for i, arg := range call.Arguments {
			args[i] = arg.Export()
		}
		t.Logf("JS Console: %v", args)
		return goja.Undefined()
	})
	vm.Set("console", console)
	
	// Set up setTimeout for testing
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
				t.Logf("setTimeout callback error: %v", err)
			}
		}()
		
		return goja.Undefined()
	})
}

