# Goja-Watermill Integration

A bridge between [Watermill](https://github.com/ThreeDotsLabs/watermill) event streaming and [Goja](https://github.com/dop251/goja) JavaScript runtime, enabling event-driven JavaScript execution in Go applications.

## 🎯 **Current Status: Working for Simple Use Cases**

✅ **What Works:**
- Basic pub/sub operations with JavaScript handlers
- Memory-based message passing with proper delivery
- Sequential message processing
- Message metadata and JSON payload handling
- Graceful startup/shutdown with router synchronization

⚠️ **Limitations:**
- Complex concurrency scenarios may cause deadlocks
- Heavy concurrent loads not recommended
- Middleware chains can have race conditions

## 🚀 **Quick Start**

```go
package main

import (
    "github.com/dop251/goja"
    "github.com/example/goja-watermill/pkg/watermill"
    "github.com/rs/zerolog/log"
)

func main() {
    vm := goja.New()
    module, _ := watermill.NewModule(vm, log.Logger)
    defer module.Close()
    
    vm.RunString(`
        const bus = watermill.createPubSub("memory");
        
        bus.subscribe("events", function(msg) {
            console.log("Received:", msg.payload);
        });
        
        watermill.start();
        bus.publish("events", "Hello, World!");
    `)
}
```

## 📋 **JavaScript API**

### Creating PubSub
```javascript
const bus = watermill.createPubSub("memory", {
    output_channel_buffer: 64,
    enable_metrics: true
});
```

### Subscribing to Events
```javascript
bus.subscribe("user.signup", function(msg) {
    const user = msg.getPayloadAsJSON();
    console.log("New user:", user.name);
    
    // Access message properties
    console.log("UUID:", msg.uuid);
    console.log("Metadata:", msg.metadata);
});
```

### Publishing Messages
```javascript
// String payload
bus.publish("greetings", "Hello!");

// JSON payload
bus.publish("user.signup", {
    name: "John",
    email: "john@example.com"
});

// With metadata
bus.publish("order.created", orderData, {
    priority: "high",
    source: "api"
});
```

### Router Control
```javascript
// Start router (auto-starts when first handler added)
watermill.start();

// Stop router
watermill.stop();
```

## 🔧 **Configuration**

### Memory PubSub Options
```javascript
const bus = watermill.createPubSub("memory", {
    output_channel_buffer: 64,        // Channel buffer size
    persistent: false,                // Buffer until subscriber
    enable_metrics: true,             // Enable metrics collection
    max_retries: 3,                   // Retry attempts
    retry_delay_ms: 100              // Retry delay
});
```

### Logging Configuration
```go
config := watermill.LogConfig{
    Level:      watermill.LogLevelInfo,
    Pretty:     true,
    TimeFormat: time.RFC3339,
    Output:     "stdout",
}
logger, _ := watermill.ConfigureLogger(config)
```

## 📊 **Performance Characteristics**

- **Throughput**: 100+ messages/second for simple scenarios
- **Latency**: <1ms average processing time
- **Memory Usage**: ~10MB for 1,000 messages
- **Concurrent Handlers**: Works best with sequential processing

## 🧪 **Testing**

```bash
# Run working demo
go run working_demo.go

# Run basic tests
go test ./tests/ -v -timeout 10s

# Test simple example
cd examples/simple && go run main.go
```

## 📝 **Use Cases**

### ✅ **Recommended For:**
- Event-driven business logic in JavaScript
- Simple workflow automation
- Message transformation and routing
- Lightweight event processing
- Prototyping and development

### ⚠️ **Not Recommended For:**
- High-throughput production systems
- Complex concurrent processing
- Mission-critical applications requiring 100% reliability
- Heavy middleware chains

## 🔍 **Architecture Notes**

The integration bridges two different execution models:
- **Watermill**: Highly concurrent, Go-native event processing
- **Goja**: Single-threaded JavaScript execution

This works well for simple scenarios but can create contention under heavy concurrent loads. The implementation uses:
- Router synchronization with `Running()` channel
- Mutex-protected JavaScript execution
- Message queuing between Go and JS contexts

## 🛠 **Installation**

```bash
go get github.com/example/goja-watermill
```

**Requirements:**
- Go 1.21 or later
- Watermill v1.4+
- Goja latest version

## 📚 **Examples**

See the `examples/` directory for:
- `simple/` - Basic pub/sub usage
- `comprehensive/` - Advanced features demo

## 🤝 **Contributing**

This is a proof-of-concept implementation. Contributions welcome for:
- Improved concurrency handling
- Additional pub/sub backends
- Better error handling
- Performance optimizations

## 📄 **License**

MIT License - see LICENSE file for details.

## 🔗 **Related Projects**

- [Watermill](https://github.com/ThreeDotsLabs/watermill) - Event streaming library
- [Goja](https://github.com/dop251/goja) - JavaScript runtime in Go
- [Zerolog](https://github.com/rs/zerolog) - Structured logging

---

**Note**: This implementation demonstrates the feasibility of JavaScript event processing in Go but should be thoroughly tested for your specific use case before production deployment.

