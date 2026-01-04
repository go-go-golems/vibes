# Chapter 8: Kotlin: JVM Power with Spring Kafka

Continuing our exploration of polyglot Kafka integration in Part III, we now turn our attention to the Java Virtual Machine (JVM) ecosystem, specifically focusing on Kotlin combined with the widely adopted Spring Framework and its Spring Kafka module. As highlighted in Chapter 3, the JVM offers a mature, robust platform with a vast ecosystem of libraries and tools. Kotlin, as a modern JVM language, brings conciseness, null safety, and powerful features like coroutines, making it an excellent choice for building sophisticated, enterprise-grade event-driven services.

Spring Kafka, part of the larger Spring ecosystem, provides high-level abstractions over the native Kafka client libraries, significantly simplifying the development of Kafka producers and consumers within a Spring Boot application. It integrates seamlessly with Spring's dependency injection, transaction management, and monitoring capabilities.

In this chapter, we'll examine how our reference implementation leverages Kotlin and Spring Kafka to build the Shipping service (`kafka_content/kotlin-service`), focusing on configuration, message handling, error management, and integration with other Spring features.

## Why Kotlin and Spring Kafka?

While Go (Chapter 7) excels in simplicity and raw performance, Kotlin on the JVM offers different advantages:

- **Rich Ecosystem**: Access to the extensive Java ecosystem, including mature libraries for databases (JPA/Hibernate), web frameworks (Spring WebFlux/MVC), security (Spring Security), and more.
- **Developer Productivity**: Kotlin's concise syntax and features like data classes, extension functions, and coroutines enhance developer productivity compared to traditional Java.
- **Spring Integration**: Spring Boot and Spring Kafka provide a highly opinionated yet flexible framework that handles much of the boilerplate configuration and integration work.
- **Maturity and Stability**: The JVM and Spring Framework are battle-tested platforms used in countless large-scale enterprise systems.

Spring Kafka, in particular, simplifies Kafka interactions by providing:

- **Annotation-driven listeners**: Easy creation of consumers using the `@KafkaListener` annotation.
- **Template-based producers**: Simplified message publishing via `KafkaTemplate`.
- **Error handling and retries**: Built-in support for handling exceptions and retrying failed messages.
- **Transaction management**: Integration with Spring's transaction abstraction for exactly-once semantics (if needed).
- **Serialization/Deserialization**: Flexible configuration for message converters (e.g., JSON, Avro, Protobuf).

## Setting Up Spring Kafka in Kotlin

Let's look at the core configuration required to integrate Spring Kafka into our Kotlin-based Shipping service.

### Dependencies

We need to include the necessary Spring Boot and Spring Kafka dependencies in our `build.gradle.kts` (or `pom.xml` if using Maven):

```kotlin
// build.gradle.kts (simplified)
dependencies {
    implementation("org.springframework.boot:spring-boot-starter")
    implementation("org.springframework.kafka:spring-kafka")
    implementation("org.jetbrains.kotlin:kotlin-reflect")
    implementation("org.jetbrains.kotlin:kotlin-stdlib-jdk8")
    implementation("com.google.protobuf:protobuf-java:3.19.4") // For Protobuf
    // ... other dependencies like Spring Web, Spring Data JPA, etc.
}
```

### Configuration (`application.yml`)

Spring Boot allows configuring Kafka connection details and other properties in `application.yml` or `application.properties`:

```yaml
# application.yml
spring:
  application:
    name: shipping-service
  kafka:
    bootstrap-servers: kafka:9092 # Comma-separated list of brokers
    consumer:
      group-id: shipping-service-group # Default consumer group ID
      auto-offset-reset: earliest # Start consuming from the beginning if no offset exists
      key-deserializer: org.apache.kafka.common.serialization.StringDeserializer
      value-deserializer: org.apache.kafka.common.serialization.ByteArrayDeserializer # Using byte arrays for Protobuf
    producer:
      key-serializer: org.apache.kafka.common.serialization.StringSerializer
      value-serializer: org.apache.kafka.common.serialization.ByteArraySerializer # Using byte arrays for Protobuf
      acks: all # Ensure producer waits for all replicas
```

This configuration sets up the broker addresses, default consumer group ID, and serializers/deserializers. We use `ByteArrayDeserializer` and `ByteArraySerializer` because we are handling Protobuf serialization manually in our service code.

### Kafka Configuration Beans (`KafkaConfig.kt`)

While `application.yml` provides basic settings, we often define explicit configuration beans for more control, especially over factories and templates. Our reference implementation does this in `KafkaConfig.kt`:

```kotlin
// From kafka_content/kotlin-service/src/main/kotlin/com/scrapybara/kw/shipping/ShippingServiceApplication.kt
@Configuration
class KafkaConfig {
    @Value("\${spring.kafka.bootstrap-servers}")
    private lateinit var bootstrapServers: String

    // Producer Factory: Creates KafkaProducer instances
    @Bean
    fun producerFactory(): ProducerFactory<String, ByteArray> {
        val configProps = mapOf(
            ProducerConfig.BOOTSTRAP_SERVERS_CONFIG to bootstrapServers,
            ProducerConfig.KEY_SERIALIZER_CLASS_CONFIG to StringSerializer::class.java,
            ProducerConfig.VALUE_SERIALIZER_CLASS_CONFIG to ByteArraySerializer::class.java,
            ProducerConfig.ACKS_CONFIG to "all" // Moved from application.yml for clarity
        )
        return DefaultKafkaProducerFactory(configProps)
    }

    // KafkaTemplate: High-level abstraction for sending messages
    @Bean
    fun kafkaTemplate(producerFactory: ProducerFactory<String, ByteArray>): KafkaTemplate<String, ByteArray> {
        return KafkaTemplate(producerFactory)
    }

    // Consumer Factory: Creates KafkaConsumer instances
    @Bean
    fun consumerFactory(): ConsumerFactory<String, ByteArray> {
        val props = mapOf(
            ConsumerConfig.BOOTSTRAP_SERVERS_CONFIG to bootstrapServers,
            ConsumerConfig.GROUP_ID_CONFIG to "shipping-service-group", // Explicit group ID
            ConsumerConfig.KEY_DESERIALIZER_CLASS_CONFIG to StringDeserializer::class.java,
            ConsumerConfig.VALUE_DESERIALIZER_CLASS_CONFIG to ByteArrayDeserializer::class.java,
            ConsumerConfig.AUTO_OFFSET_RESET_CONFIG to "earliest"
        )
        return DefaultKafkaConsumerFactory(props)
    }

    // Listener Container Factory: Creates containers for @KafkaListener methods
    @Bean
    fun kafkaListenerContainerFactory(consumerFactory: ConsumerFactory<String, ByteArray>): ConcurrentKafkaListenerContainerFactory<String, ByteArray> {
        val factory = ConcurrentKafkaListenerContainerFactory<String, ByteArray>()
        factory.consumerFactory = consumerFactory
        // Additional configuration (e.g., concurrency, error handling) can go here
        return factory
    }
}
```

This code defines beans for `ProducerFactory`, `KafkaTemplate`, `ConsumerFactory`, and `ConcurrentKafkaListenerContainerFactory`. These factories are used by Spring Kafka under the hood to create producers and consumers. The `KafkaTemplate` is what we inject into our services to publish messages.

## Consuming Messages with `@KafkaListener`

Spring Kafka makes consuming messages incredibly simple using the `@KafkaListener` annotation on a method within a Spring-managed bean (e.g., a `@Service`).

```kotlin
// From kafka_content/kotlin-service/src/main/kotlin/com/scrapybara/kw/shipping/ShippingServiceApplication.kt
@Service
class ShippingService(
    // ... other dependencies
    private val shippingSagaManager: ShippingSagaManager,
    private val metrics: ShippingMetrics,
    private val registry: MeterRegistry
) {
    private val logger = LoggerFactory.getLogger(ShippingService::class.java)

    @KafkaListener(topics = ["inventory.checked"], containerFactory = "kafkaListenerContainerFactory")
    fun handleInventoryChecked(payloadBytes: ByteArray) {
        metrics.kafkaMessagesReceived.increment()
        val sample = Timer.start(registry)
        try {
            // 1. Deserialize using Protobuf
            val event = OrderProto.InventoryChecked.parseFrom(payloadBytes)
            logger.info("Received inventory checked event for order: ${event.orderId}, all items available: ${event.allItemsAvailable}")

            // 2. Delegate to business logic (Saga Manager in this case)
            // Using runBlocking here for simplicity in the example, consider structured concurrency
            runBlocking {
                shippingSagaManager.startShippingSaga(
                    orderId = event.orderId,
                    allItemsAvailable = event.allItemsAvailable
                )
            }

            // 3. Update metrics based on outcome
            if (event.allItemsAvailable) {
                metrics.incrementOrdersFulfilled()
            } else {
                metrics.incrementOrdersCancelled()
            }
            metrics.ordersProcessed.increment()

        } catch (e: Exception) {
            logger.error("Error processing inventory checked event", e)
            metrics.kafkaMessagesFailed.increment()
            // Re-throw to let Spring Kafka handle retries/error handling
            throw e
        } finally {
            // Record processing time metric
            sample?.stop(metrics.shippingProcessingTime)
        }
    }
    // ... other listener methods for different topics ...
}
```

Key aspects of this listener:

- **`@KafkaListener`**: Marks the method as a Kafka message consumer.
- **`topics`**: Specifies the Kafka topic(s) to subscribe to.
- **`containerFactory`**: Refers to the bean name of the `ConcurrentKafkaListenerContainerFactory` we defined earlier. This factory configures the underlying consumer container.
- **Method Parameter (`payloadBytes: ByteArray`)**: Spring Kafka injects the raw message payload as a byte array (because we configured `ByteArrayDeserializer`).
- **Deserialization**: We manually deserialize the byte array using the generated Protobuf code (`OrderProto.InventoryChecked.parseFrom(payloadBytes)`).
- **Business Logic**: The core logic is delegated to other components (`shippingSagaManager`).
- **Error Handling**: Exceptions are caught, logged, and re-thrown. Spring Kafka's default error handling mechanisms (including potential retries) will then take over.
- **Acknowledgment**: By default, if the listener method completes without throwing an exception, Spring Kafka automatically acknowledges the message (commits the offset).

## Publishing Messages with `KafkaTemplate`

To publish messages, we inject the `KafkaTemplate` bean (defined in `KafkaConfig.kt`) into our service and use its `send` method.

```kotlin
// From kafka_content/kotlin-service/src/main/kotlin/com/scrapybara/kw/shipping/config/KafkaOperations.kt
@Component
class KafkaOperations(private val kafkaTemplate: KafkaTemplate<String, ByteArray>) {
    private val logger = LoggerFactory.getLogger(KafkaOperations::class.java)

    fun publishShipmentCreated(event: OrderProto.ShipmentCreated) {
        val topic = "shipment.created"
        val key = event.orderId // Use orderId as partition key
        val payload = event.toByteArray()
        
        logger.info("Publishing ShipmentCreated event for order: ${event.orderId} to topic: $topic")
        kafkaTemplate.send(topic, key, payload)
            .addCallback(
                { result -> logger.info("Successfully published event to ${result?.recordMetadata?.topic()}-${result?.recordMetadata?.partition()} @ ${result?.recordMetadata?.offset()}") },
                { ex -> logger.error("Failed to publish event for order ${event.orderId}", ex) }
            )
    }
    
    // ... other publish methods for different event types ...
}

// Usage within another service:
@Service
class ShippingSaga(/* ... */, private val kafkaOps: KafkaOperations) {
    suspend fun createShipment(orderId: String, /* ... */) {
        // ... logic to create shipment ...
        
        val shipmentCreatedEvent = OrderProto.ShipmentCreated.newBuilder()
            .setShipmentId(shipment.id)
            .setOrderId(orderId)
            // ... set other fields ...
            .build()
            
        kafkaOps.publishShipmentCreated(shipmentCreatedEvent)
    }
}
```

Key aspects of publishing:

- **`KafkaTemplate` Injection**: The template is injected via constructor injection.
- **`send(topic, key, payload)`**: The primary method for sending messages. We provide the topic name, the partition key (crucial for ordering, using `orderId` here), and the serialized payload (byte array from Protobuf).
- **Serialization**: We manually serialize the Protobuf object to a byte array using `event.toByteArray()`.
- **Asynchronous Sending**: `kafkaTemplate.send` returns a `ListenableFuture` (or `CompletableFuture` in newer versions). We use `addCallback` to handle success and failure asynchronously, preventing the main thread from blocking.

## Error Handling and Retries in Spring Kafka

Spring Kafka provides several mechanisms for handling errors during message consumption.

### Default Behavior

If a `@KafkaListener` method throws an exception, Spring Kafka's default error handler logs the error but does not stop consumption. The offset is not committed, meaning the failed message (and subsequent messages in the same partition) will likely be redelivered on the next poll.

### Retries with Backoff

Spring Retry can be integrated to automatically retry failed listener executions with configurable backoff policies. This is often enabled via `@EnableRetry` and configuring a `RetryTemplate` or using `@Retryable` annotations (though listener retries are often configured at the container factory level).

Our reference implementation includes a `RetryConfig.kt` which likely configures this, although the specific mechanism (e.g., `SeekToCurrentErrorHandler` with backoff) isn't shown in the provided snippets.

```kotlin
// Example configuration snippet (conceptual)
@Bean
fun kafkaListenerContainerFactory(/* ... */): ConcurrentKafkaListenerContainerFactory<String, ByteArray> {
    // ... other factory config ...
    
    // Configure error handler with retries and backoff
    factory.setErrorHandler(SeekToCurrentErrorHandler(
        FixedBackOff(1000L, 3L) // 1-second interval, 3 retries
    ))
    
    return factory
}
```

### Dead Letter Queues (DLQ)

After exhausting retries, it's common practice to send the persistently failing message to a Dead Letter Queue (DLQ) topic for later inspection. Spring Kafka supports this through error handlers like `SeekToCurrentErrorHandler` or custom configurations.

```kotlin
// Example configuration snippet (conceptual)
@Bean
fun kafkaListenerContainerFactory(/* ... */): ConcurrentKafkaListenerContainerFactory<String, ByteArray> {
    // ... other factory config ...
    
    // Configure error handler with DLQ
    val recoverer = DeadLetterPublishingRecoverer(kafkaTemplate) // Pass KafkaTemplate
    factory.setErrorHandler(SeekToCurrentErrorHandler(
        recoverer,
        FixedBackOff(1000L, 3L)
    ))
    
    return factory
}
```

The `DeadLetterPublishingRecoverer` automatically sends the failed message to a topic named `<original-topic-name>.DLT` by default, adding headers with error information.

## Integrating with Kotlin Coroutines

Kotlin's coroutines provide excellent support for asynchronous programming. While Spring Kafka's listener model is inherently synchronous per thread, coroutines can be used within the listener method for non-blocking I/O operations or complex asynchronous logic.

```kotlin
@KafkaListener(topics = ["some.topic"])
fun handleEvent(payloadBytes: ByteArray) = runBlocking { // Use runBlocking or la
(Content truncated due to size limit. Use line ranges to read in chunks)