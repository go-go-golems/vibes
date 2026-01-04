# Chapter 10: Contracts & Schemas with Protocol Buffers

In a polyglot event-driven system, one of the most critical challenges is ensuring that services written in different languages can reliably communicate with each other. As we transition from Part III, where we explored language-specific implementations, to Part IV, where we examine higher-level architectural patterns, we begin with the foundation of inter-service communication: data contracts and schemas.

This chapter explores how Protocol Buffers (Protobuf) provides a language-agnostic way to define, evolve, and enforce data contracts across our polyglot services. We'll examine the practical implementation in our reference system, covering schema design principles, versioning strategies, and integration with Kafka.

## The Need for Strong Contracts in Event-Driven Systems

In traditional request-response architectures, API contracts are often defined using specifications like OpenAPI (formerly Swagger), which document the endpoints, request/response formats, and validation rules. In event-driven architectures, we need a similar mechanism for defining the structure and semantics of events flowing through the system.

Without well-defined contracts, several problems can arise:

### 1. Misinterpretation of Data

When a Go service produces an event that a Ruby service consumes, how does the Ruby service know what fields to expect and how to interpret them? Without a shared understanding, the consumer might misinterpret the data, leading to subtle bugs that are difficult to detect and debug.

### 2. Brittle Coupling

If producers and consumers rely on implicit, undocumented contracts, changes to event structures can easily break downstream services. This creates a form of brittle coupling that undermines one of the key benefits of event-driven architecture: the ability to evolve services independently.

### 3. Validation Challenges

Without a formal schema, validating incoming events becomes a manual, error-prone process. Each service must implement its own validation logic, leading to inconsistencies and potential security vulnerabilities.

### 4. Documentation Overhead

Maintaining separate documentation for event structures across multiple languages creates significant overhead and increases the risk of documentation drift, where the actual implementation diverges from the documented contract.

## Enter Protocol Buffers

Protocol Buffers (Protobuf), developed by Google, is a language-neutral, platform-neutral, extensible mechanism for serializing structured data. It addresses the challenges above by providing:

1. **Language-Agnostic Schema Definition**: Define data structures once, generate code for multiple languages.
2. **Efficient Serialization**: Compact binary format that's faster and smaller than alternatives like JSON or XML.
3. **Schema Evolution**: Built-in mechanisms for backward and forward compatibility.
4. **Strong Typing**: Compile-time type checking helps catch errors early.
5. **Documentation**: Self-documenting schemas with support for comments.

### Protobuf Basics

A Protobuf schema is defined in a `.proto` file using a simple, language-independent syntax. Here's a simplified example from our reference implementation:

```protobuf
// From kafka_content/idl/order.proto
syntax = "proto3";

package order;

option go_package = "github.com/scrapybara/kafka-watermill/idl/go/order";
option java_package = "com.scrapybara.kw.idl";
option java_outer_classname = "OrderProto";
option java_multiple_files = true;
option ruby_package = "KafkaWatermill.IDL";

// Event representing an order created by a user
message OrderCreated {
  string order_id = 1;
  string user_id = 2;
  repeated OrderItem items = 3;
  float total_amount = 4;
  string timestamp = 5;
}

// Supporting message type
message OrderItem {
  string product_id = 1;
  string name = 2;
  int32 quantity = 3;
  float price = 4;
}
```

This schema defines two message types: `OrderCreated` and `OrderItem`. The `OrderCreated` message includes a list (`repeated`) of `OrderItem` messages, demonstrating composition. Each field has a type (e.g., `string`, `int32`, `float`) and a unique field number (e.g., `1`, `2`, `3`) that identifies the field in the binary encoding.

The `option` directives specify language-specific settings, such as package names and class names, ensuring that the generated code follows idiomatic conventions for each target language.

## Implementing Protobuf in Our Polyglot System

Let's examine how our reference implementation uses Protobuf to define and enforce contracts across Go, Kotlin, and Ruby services.

### 1. Centralized Schema Repository

All `.proto` files are stored in a central location (`kafka_content/idl/`), serving as the single source of truth for event definitions. This centralization ensures consistency and makes it easier to review and evolve schemas over time.

```
kafka_content/
├── idl/
│   ├── order.proto      # Defines order-related events
│   └── ...              # Other domain-specific schemas
```

### 2. Code Generation for Multiple Languages

The Protobuf compiler (`protoc`) generates language-specific code from the `.proto` files. This generated code includes classes/structs for each message type, serialization/deserialization methods, and utility functions.

For our polyglot system, we generate code for Go, Kotlin (Java), and Ruby:

```bash
# Generate Go code
protoc --proto_path=idl --go_out=go/gen order.proto

# Generate Java code (for Kotlin)
protoc --proto_path=idl --java_out=kotlin/src/main/java order.proto

# Generate Ruby code
protoc --proto_path=idl --ruby_out=ruby/lib order.proto
```

The generated code follows the package/namespace conventions specified in the `option` directives, ensuring idiomatic integration with each language.

### 3. Integration with Kafka Producers and Consumers

Each service uses the generated Protobuf code to serialize events before publishing them to Kafka and deserialize events when consuming from Kafka.

#### Go (with Watermill)

```go
// Publishing an event
orderCreated := &order.OrderCreated{
    OrderId:     uuid.New().String(),
    UserId:      userID,
    Items:       mapToProtoItems(items),
    TotalAmount: calculateTotal(items),
    Timestamp:   time.Now().Format(time.RFC3339),
}

// Serialize using Protobuf
payload, err := proto.Marshal(orderCreated)
if err != nil {
    return err
}

// Create Watermill message
msg := message.NewMessage(uuid.New().String(), payload)
msg.Metadata.Set("content-type", "application/protobuf")
msg.Metadata.Set("event-type", "OrderCreated")

// Publish to Kafka
return publisher.Publish("order.created", msg)

// Consuming an event
func HandleOrderCreated(msg *message.Message) ([]*message.Message, error) {
    // Deserialize using Protobuf
    var orderCreated order.OrderCreated
    if err := proto.Unmarshal(msg.Payload, &orderCreated); err != nil {
        return nil, fmt.Errorf("failed to unmarshal OrderCreated: %w", err)
    }
    
    // Process the event
    log.Printf("Received order: %s for user: %s with %d items",
        orderCreated.OrderId, orderCreated.UserId, len(orderCreated.Items))
    
    // ... business logic ...
}
```

#### Kotlin (with Spring Kafka)

```kotlin
// Publishing an event
val orderCreated = OrderCreated.newBuilder()
    .setOrderId(UUID.randomUUID().toString())
    .setUserId(userId)
    .addAllItems(items.map { mapToProtoItem(it) })
    .setTotalAmount(calculateTotal(items))
    .setTimestamp(Instant.now().toString())
    .build()

// Serialize using Protobuf
val payload = orderCreated.toByteArray()

// Publish to Kafka
kafkaTemplate.send("order.created", orderCreated.orderId, payload)

// Consuming an event
@KafkaListener(topics = ["order.created"])
fun handleOrderCreated(payload: ByteArray) {
    // Deserialize using Protobuf
    val orderCreated = OrderCreated.parseFrom(payload)
    
    // Process the event
    logger.info("Received order: ${orderCreated.orderId} for user: ${orderCreated.userId} with ${orderCreated.itemsCount} items")
    
    // ... business logic ...
}
```

#### Ruby (with ruby-kafka)

```ruby
# Publishing an event (less common in our reference implementation)
order_created = KafkaWatermill::IDL::OrderCreated.new(
  order_id: SecureRandom.uuid,
  user_id: user_id,
  items: items.map { |item| map_to_proto_item(item) },
  total_amount: calculate_total(items),
  timestamp: Time.now.iso8601
)

# Serialize using Protobuf
payload = KafkaWatermill::IDL::OrderCreated.encode(order_created)

# Publish to Kafka
producer.produce(payload, topic: "order.created", key: order_created.order_id)

# Consuming an event
consumer.each_message do |message|
  case message.topic
  when "order.created"
    # Deserialize using Protobuf
    order_created = KafkaWatermill::IDL::OrderCreated.decode(message.value)
    
    # Process the event
    logger.info("Received order: #{order_created.order_id} for user: #{order_created.user_id} with #{order_created.items.size} items")
    
    # ... business logic ...
  end
end
```

### 4. Metadata and Content Type

In addition to the serialized payload, it's often useful to include metadata about the event, such as its type and serialization format. This metadata can be included in Kafka message headers or, as in our Watermill example, in message metadata.

```go
// Go example with Watermill
msg := message.NewMessage(uuid.New().String(), payload)
msg.Metadata.Set("content-type", "application/protobuf")
msg.Metadata.Set("event-type", "OrderCreated")
```

This metadata helps consumers determine how to deserialize the message and can be used for routing, filtering, and monitoring.

## Schema Design Principles

Designing effective Protobuf schemas requires careful consideration of several factors. Here are key principles we follow in our reference implementation:

### 1. Domain-Driven Design

Our schemas reflect the domain model, with message types corresponding to domain events like `OrderCreated`, `PaymentProcessed`, and `OrderFulfilled`. This alignment with the domain model makes the schemas more intuitive and maintainable.

### 2. Event-First Design

Rather than starting with internal data models and exposing them as events, we design our events specifically for inter-service communication. This "event-first" approach ensures that events contain exactly the information needed by consumers, no more and no less.

### 3. Explicit Naming

We use explicit, descriptive names for message types and fields, avoiding abbreviations and ambiguous terms. This makes the schemas self-documenting and reduces the risk of misinterpretation.

```protobuf
// Good: Explicit naming
message OrderCreated {
  string order_id = 1;
  string user_id = 2;
  repeated OrderItem items = 3;
  float total_amount = 4;
  string timestamp = 5;
}

// Avoid: Ambiguous naming
message Order {
  string id = 1;
  string uid = 2;
  repeated Item i = 3;
  float total = 4;
  string ts = 5;
}
```

### 4. Composition Over Inheritance

Protobuf doesn't support inheritance, but it does support composition through nested messages and imports. We use composition to create reusable components like `OrderItem` that can be included in multiple message types.

```protobuf
// Reusable component
message OrderItem {
  string product_id = 1;
  string name = 2;
  int32 quantity = 3;
  float price = 4;
}

// Used in multiple message types
message OrderCreated {
  // ... other fields ...
  repeated OrderItem items = 3;
}

message OrderFulfilled {
  // ... other fields ...
  repeated OrderItem items = 4;
}
```

### 5. Minimal Dependencies

We minimize dependencies between schema files to reduce coupling and make evolution easier. When dependencies are necessary, we use imports to include only what's needed.

```protobuf
// Importing only what's needed
import "common/address.proto";

message OrderShipped {
  string order_id = 1;
  common.Address shipping_address = 2;
  // ... other fields ...
}
```

## Schema Evolution and Compatibility

One of the most powerful features of Protobuf is its support for schema evolution while maintaining compatibility. This is crucial in event-driven systems, where producers and consumers may be updated independently.

### Backward and Forward Compatibility

- **Backward Compatibility**: New schema can read data written with old schema.
- **Forward Compatibility**: Old schema can read data written with new schema.

Protobuf supports both types of compatibility through careful field numbering and default values.

### Compatibility Rules

To maintain compatibility when evolving schemas, follow these rules:

1. **Never change field numbers**: Field numbers are used in the binary encoding, so changing them would break compatibility.
2. **Never change field types**: Changing a field from `string` to `int32`, for example, would cause deserialization errors.
3. **Never remove required fields**: In proto3, all fields are optional by default, but removing a field that consumers expect can still cause issues.
4. **Add new fields with care**: New fields should have default values that make sense when old consumers encounter them.
5. **Use reserved fields and tags**: When removing fields, mark them as reserved to prevent future reuse.

```protobuf
message OrderCreated {
  // Existing fields
  string order_id = 1;
  string user_id = 2;
  repeated OrderItem items = 3;
  float total_amount = 4;
  string timestamp = 5;
  
  // New field added in v2
  string currency = 6;
  
  // Reserved fields from removed features
  reserved 7, 8, 9;
  reserved "promotion_code", "referral_source";
}
```

### Versioning Strategies

There are several approaches to versioning Protobuf schemas:

#### 1. Implicit Versioning

With implicit versioning, we rely on Protobuf's compatibility features without explicitly marking versions. New fields are added, and consumers are designed to handle missing fields gracefully.

This approach works well for minor changes but can become difficult to manage for significant schema changes.

#### 2. Explicit Versioning in Message Names

Another approach is to include version numbers in message names:

```protobuf
message OrderCreatedV1 {
  // V1 fields
}

message OrderCreatedV2 {
  // V2 fields, possibly including all V1 fields
}
```

This approach makes versions explicit but requires producers to know which version to use and may lead to code duplication.

#### 3. Versioned Packages

A third approach is to version at the package level:

```protobuf
// v1/order.proto
package order.v1;

message OrderCreated {
  // V1 fields
}

// v2/order.proto
package order.v2;

message OrderCreated {
  // V2 fields
}
```

This approach provides clear separation between versions but requires more complex import management.

#### Our Approach: Evolutionary Design with Careful Documentation

In our reference implementation, we primarily use implicit versioning with careful documentation of changes. Each schema file includes a version history in comments, and we follow strict compatibility rules when making changes.

```protobuf
// order.proto
// Version History:
// v1.0.0 (2023-01-15): Initial version
// v1.1.0 (2023-03-22): Added 'currency' field to OrderCreated
// v1.2.0 (2023-06-10): Added 'shipping_method' field to OrderFulfilled

syntax = "proto3";
// ... rest of the schema ...
```

For major breaking changes, we would consider more explicit versioning strategies, but our focus on backward compatibility has made this unnecessary so far.

## Beyond Basic Protobuf: Advanced Techniques

While basic Protobuf usage covers most needs, our reference implementation employs several advanced techniques to enhance our schema management.

### 1. Custom Scalar Types

Protobuf's built-in scalar types (string, int32, etc.) are often sufficient, but sometimes we need more specific types. For example, we might want to ensure that a string field contains a valid UUID or timestamp.

We address this through validation in the application code and clear document
(Content truncated due to size limit. Use line ranges to read in chunks)