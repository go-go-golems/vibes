# Chapter 15: Monitoring & Observability

In event-driven systems, particularly those built around Kafka, understanding what's happening inside your application is both crucial and challenging. The distributed, asynchronous nature of these systems means that traditional monitoring approaches often fall short. When a business process spans multiple services connected through events, pinpointing the source of issues, understanding performance bottlenecks, or simply confirming that everything is working correctly requires a comprehensive observability strategy.

This chapter explores the principles and practices of monitoring and observability for Kafka-based, event-driven systems. We'll cover key metrics to track, logging strategies, distributed tracing, alerting, and dashboarding, with practical examples from our reference implementation and insights from industry best practices.

## Monitoring vs. Observability

Before diving into specifics, let's clarify the distinction between monitoring and observability:

- **Monitoring** is about tracking and alerting on known, predefined metrics and states. It answers questions like "Is the system up?" or "Is the consumer lag exceeding our threshold?"

- **Observability** is a broader concept that enables understanding the internal state of a system through its external outputs. It helps answer questions you didn't anticipate, like "Why is this particular order stuck in processing?" or "What's the exact path this event took through our system?"

A mature system needs both: monitoring to alert you when things go wrong, and observability to help you understand why they went wrong and how to fix them.

## Key Metrics for Kafka-Based Systems

Effective monitoring starts with identifying the right metrics to track. For Kafka-based systems, these fall into several categories:

### 1. Kafka Broker Metrics

These metrics help monitor the health and performance of the Kafka cluster itself:

- **Broker Uptime**: Time since the broker started.
- **Request Rate**: Number of requests per second (produce, fetch, metadata).
- **Request Latency**: Time taken to process requests.
- **Disk Usage**: Available disk space and utilization.
- **Network I/O**: Bytes in/out per second.
- **Under-replicated Partitions**: Partitions with fewer than the configured number of replicas.
- **Offline Partitions**: Partitions without an active leader.
- **Active Controller Count**: Should be exactly 1 across the cluster.

### 2. Producer Metrics

These metrics help monitor the health and performance of your Kafka producers:

- **Record Send Rate**: Number of records sent per second.
- **Byte Rate**: Bytes sent per second.
- **Request Latency**: Time taken for the broker to acknowledge records.
- **Record Error Rate**: Number of records that failed to be sent.
- **Batch Size**: Average and max size of batches sent.
- **Record Queue Time**: Time records spend in the producer buffer.

### 3. Consumer Metrics

These metrics help monitor the health and performance of your Kafka consumers:

- **Record Consumption Rate**: Number of records consumed per second.
- **Byte Rate**: Bytes consumed per second.
- **Consumer Lag**: Number of messages the consumer is behind the producer (critical metric).
- **Poll Rate**: Number of poll calls per second.
- **Poll Latency**: Time taken to complete poll calls.
- **Commit Success/Failure Rate**: Rate of successful/failed offset commits.
- **Rebalance Rate**: Number of consumer group rebalances.

### 4. Application-Specific Metrics

Beyond Kafka-specific metrics, you should track metrics relevant to your business domain:

- **Event Processing Rate**: Number of events processed per second, by event type.
- **Event Processing Latency**: Time taken to process each event.
- **Business Process Completion Rate**: Rate at which end-to-end business processes (e.g., order fulfillment) complete.
- **Business Process Duration**: Time taken for end-to-end business processes.
- **Error Rates**: Number of errors encountered during event processing.
- **Dead Letter Queue (DLQ) Rate**: Rate at which messages are sent to DLQs.

### Example: Metrics in Our Kotlin Service

Our reference implementation includes metrics in the Kotlin Shipping service using Spring Boot Actuator and Micrometer:

```kotlin
// From kafka_content/kotlin-service/src/main/kotlin/com/scrapybara/kw/shipping/config/MetricsConfig.kt
@Configuration
class MetricsConfig(private val registry: MeterRegistry) {
    
    @Bean
    fun shippingMetrics(): ShippingMetrics {
        return ShippingMetrics(registry)
    }
}

// Metrics collection class
class ShippingMetrics(private val registry: MeterRegistry) {
    
    // Counter metrics
    val kafkaMessagesReceived = registry.counter("shipping.kafka.messages.received")
    val kafkaMessagesFailed = registry.counter("shipping.kafka.messages.failed")
    val ordersProcessed = registry.counter("shipping.orders.processed")
    
    // Timer metric for measuring processing duration
    val shippingProcessingTime = registry.timer("shipping.processing.time")
    
    // Gauge metrics could be added for things like "current orders in processing"
    
    // Methods to increment counters with business context
    fun incrementOrdersFulfilled() {
        registry.counter("shipping.orders.fulfilled").increment()
    }
    
    fun incrementOrdersCancelled() {
        registry.counter("shipping.orders.cancelled").increment()
    }
}
```

These metrics are then used in the service code:

```kotlin
// From kafka_content/kotlin-service/src/main/kotlin/com/scrapybara/kw/shipping/ShippingServiceApplication.kt
@Service
class ShippingService(
    // ... other dependencies
    private val metrics: ShippingMetrics,
    private val registry: MeterRegistry
) {
    
    @KafkaListener(topics = ["inventory.checked"], containerFactory = "kafkaListenerContainerFactory")
    fun handleInventoryChecked(payloadBytes: ByteArray) {
        metrics.kafkaMessagesReceived.increment()
        
        val sample = Timer.start(registry)
        try {
            val event = OrderProto.InventoryChecked.parseFrom(payloadBytes)
            
            // ... business logic ...
            
            if (event.allItemsAvailable) {
                metrics.incrementOrdersFulfilled()
            } else {
                metrics.incrementOrdersCancelled()
            }
            metrics.ordersProcessed.increment()
            
        } catch (e: Exception) {
            metrics.kafkaMessagesFailed.increment()
            throw e
        } finally {
            sample?.stop(metrics.shippingProcessingTime)
        }
    }
}
```

This approach captures:
- The rate of Kafka messages received and failed
- The rate of orders processed, fulfilled, and cancelled
- The time taken to process shipping requests

These metrics can be exposed via Spring Boot Actuator's `/actuator/metrics` endpoint and collected by monitoring systems like Prometheus.

## Logging Strategies

While metrics provide aggregated numerical data, logs provide detailed contextual information about specific events and operations. Effective logging is crucial for debugging and understanding system behavior.

### Structured Logging

Traditional text-based logs are difficult to parse and analyze at scale. Structured logging formats like JSON make logs machine-readable while preserving human readability:

```json
{
  "timestamp": "2023-06-15T14:30:00.123Z",
  "level": "INFO",
  "service": "shipping-service",
  "traceId": "abc123",
  "message": "Processing inventory checked event",
  "orderId": "order-456",
  "allItemsAvailable": true,
  "processingTimeMs": 45
}
```

### Contextual Information

Include relevant business context in logs to make them useful for debugging:

- **Correlation IDs**: Include trace and span IDs (discussed in the Distributed Tracing section).
- **Business Identifiers**: Include order IDs, customer IDs, or other business-relevant identifiers.
- **Event Metadata**: Include event types, Kafka topics, partitions, and offsets.
- **Performance Data**: Include processing times and resource usage where relevant.

### Log Levels

Use appropriate log levels to control verbosity:

- **ERROR**: Use for exceptions and errors that require immediate attention.
- **WARN**: Use for unusual but non-critical situations.
- **INFO**: Use for normal operational events (e.g., "Processing order XYZ").
- **DEBUG**: Use for detailed information useful during development and debugging.
- **TRACE**: Use for very detailed information, typically only enabled during specific debugging sessions.

### Example: Logging in Our Go Service

Our reference implementation includes structured logging in the Go Order service using the `watermill` logger:

```go
// Example from Go service (conceptual)
logger := watermill.NewStdLogger(false, false)

// Logging with context
logger.Info("Processing order created event", watermill.LogFields{
    "order_id":      event.OrderID,
    "customer_id":   event.CustomerID,
    "total_amount":  event.TotalAmount,
    "items_count":   len(event.Items),
    "kafka_topic":   msg.Metadata.Get("kafka_topic"),
    "kafka_partition": msg.Metadata.Get("kafka_partition"),
    "kafka_offset":  msg.Metadata.Get("kafka_offset"),
})

// Error logging
if err != nil {
    logger.Error("Failed to process order", err, watermill.LogFields{
        "order_id": event.OrderID,
        // Additional context
    })
}
```

## Distributed Tracing

In a distributed system where a single business transaction spans multiple services connected by Kafka events, understanding the end-to-end flow is challenging. Distributed tracing addresses this by tracking the journey of a request or event through the entire system.

### Key Concepts

- **Trace**: Represents the end-to-end journey of a request or business transaction.
- **Span**: Represents a single operation within a trace (e.g., processing an event, making a database query).
- **Trace Context**: Metadata (trace ID, span ID, etc.) that's propagated between services to link spans together.

### Propagating Trace Context via Kafka

To maintain trace continuity across Kafka-based service boundaries:

1. **Producer Side**: Include trace context in Kafka message headers or metadata.
2. **Consumer Side**: Extract trace context from the message and continue the trace.

```kotlin
// Example: Propagating trace context in Kotlin with Spring Cloud Sleuth (conceptual)
@Service
class OrderService(private val kafkaTemplate: KafkaTemplate<String, ByteArray>) {
    
    fun createOrder(order: Order) {
        // Business logic...
        
        // Create OrderCreated event
        val event = OrderCreated(/* ... */)
        val payload = event.toByteArray()
        
        // Create Kafka message with trace headers
        val headers = KafkaHeaders()
        // Spring Cloud Sleuth automatically adds trace headers
        
        // Send to Kafka with headers
        kafkaTemplate.send("order.created", order.id, payload)
    }
}

@Service
class PaymentService {
    
    @KafkaListener(topics = ["order.created"])
    fun handleOrderCreated(payload: ByteArray, 
                          @Header(KafkaHeaders.RECEIVED_MESSAGE_KEY) key: String,
                          @Header("X-B3-TraceId") traceId: String?,
                          @Header("X-B3-SpanId") spanId: String?) {
        // Spring Cloud Sleuth automatically continues the trace using headers
        
        // Process the event...
    }
}
```

### Visualizing Traces

Distributed tracing systems like Jaeger, Zipkin, or AWS X-Ray provide UIs to visualize traces:

- **Trace Timeline**: Shows the sequence and duration of spans.
- **Service Dependency Graph**: Shows how services interact.
- **Span Details**: Shows detailed metadata about each operation.

These visualizations help identify bottlenecks, errors, and the flow of requests through the system.

## Alerting and Dashboarding

Collecting metrics, logs, and traces is only valuable if you can act on this information. This requires effective alerting and dashboarding.

### Alerting Best Practices

- **Alert on Symptoms, Not Causes**: Alert on customer-impacting issues (e.g., high error rates, excessive latency) rather than internal implementation details.
- **Reduce Noise**: Avoid alert fatigue by only alerting on actionable issues.
- **Define Clear Thresholds**: Set appropriate thresholds based on historical data and business requirements.
- **Include Context**: Provide enough information in alerts to understand the issue without additional investigation.
- **Implement Escalation Policies**: Define who should be notified and when.

### Key Alerts for Kafka Systems

- **Consumer Lag**: Alert when consumers fall too far behind producers.
- **Error Rates**: Alert on elevated error rates in producers, consumers, or application code.
- **Dead Letter Queue**: Alert when messages are sent to DLQs.
- **Processing Latency**: Alert when event processing takes longer than expected.
- **Broker Health**: Alert on broker failures, under-replicated partitions, or disk space issues.

### Dashboarding

Effective dashboards provide at-a-glance visibility into system health and performance. Consider organizing dashboards by:

- **Service**: Dashboards for each service showing its specific metrics.
- **Business Process**: Dashboards tracking end-to-end business processes (e.g., order fulfillment).
- **Infrastructure**: Dashboards for Kafka brokers, databases, and other infrastructure.
- **User Experience**: Dashboards focused on customer-facing metrics.

### Example Dashboard Components

- **Kafka Health**: Broker status, under-replicated partitions, consumer lag.
- **Throughput**: Messages per second, bytes per second.
- **Latency**: Processing times, end-to-end latency.
- **Error Rates**: Failed messages, application errors.
- **Business Metrics**: Orders processed, payments completed, etc.

## Implementing Observability in Our Polyglot System

Let's explore how observability might be implemented across our polyglot system:

### Go Service with Prometheus and OpenTelemetry

```go
// Conceptual example for Go service
import (
    "github.com/prometheus/client_golang/prometheus"
    "go.opentelemetry.io/otel"
    "go.opentelemetry.io/otel/trace"
)

// Prometheus metrics
var (
    ordersProcessed = prometheus.NewCounter(prometheus.CounterOpts{
        Name: "orders_processed_total",
        Help: "Total number of processed orders",
    })
    processingDuration = prometheus.NewHistogram(prometheus.HistogramOpts{
        Name: "order_processing_duration_seconds",
        Help: "Time taken to process orders",
        Buckets: prometheus.DefBuckets,
    })
)

func init() {
    prometheus.MustRegister(ordersProcessed, processingDuration)
}

// Handler with tracing and metrics
func HandleOrderCreated(msg *message.Message) ([]*message.Message, error) {
    // Extract trace context from message metadata
    ctx := extractTraceContext(msg)
    
    // Start a new span
    ctx, span := otel.Tracer("order-service").Start(ctx, "HandleOrderCreated")
    defer span.End()
    
    // Record start time for metrics
    startTime := time.Now()
    
    // Deserialize message
    var orderCreated OrderCreated
    if err := proto.Unmarshal(msg.Payload, &orderCreated); err != nil {
        span.RecordError(err)
        span.SetStatus(codes.Error, "Failed to unmarshal")
        return nil, err
    }
    
    // Add event details to span
    span.SetAttributes(
        attribute.String("order.id", orderCreated.OrderId),
        attribute.Float64("order.amount", orderCreated.TotalAmount),
    )
    
    // Process the event...
    
    // Update metrics
    ordersProcessed.Inc()
    processingDuration.Observe(time.Since(startTime).Seconds())
    
    return []*message.Message{outMsg}, nil
}
```

### Kotlin Service with Micrometer and Spring Cloud Sleuth

```kotlin
// Conceptual example for Kotlin service
@Service
class PaymentService(
    private val
(Content truncated due to size limit. Use line ranges to read in chunks)