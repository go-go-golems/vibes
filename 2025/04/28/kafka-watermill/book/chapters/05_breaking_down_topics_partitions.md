# Chapter 5: Breaking Down Topics & Partitions (Enhanced)

In the previous chapter, we introduced Apache Kafka from a high-level perspective, covering its core components and architecture. Now, we delve deeper into two of the most fundamental concepts: topics and partitions. Understanding how to design, configure, and manage topics and partitions effectively is crucial for building scalable, resilient, and maintainable Kafka-based systems. This chapter explores the nuances of topic design, the intricacies of partitioning, and the dynamics of consumer groups and rebalancing, drawing on insights from "Kafka in Action" and "Kafka Streams in Action."

## Designing Effective Topics

Topics are the primary organizational unit in Kafka, representing streams of related events. While seemingly simple, the design of your topics significantly impacts system architecture, performance, and maintainability.

### Naming Conventions

Establishing clear and consistent naming conventions for topics is essential for managing a growing Kafka deployment. Good naming conventions make it easier to understand the purpose of a topic, manage access control, and configure monitoring and alerting.

Common patterns include:

- **Domain-driven naming**: `domain.event_type` (e.g., `orders.created`, `payments.processed`, `inventory.updated`). This aligns topics with business domains and events, promoting loose coupling.
- **Hierarchical naming**: Using dots (`.`) or underscores (`_`) to create namespaces (e.g., `production.ecommerce.orders.v1`).
- **Including versioning**: Explicitly versioning topics (e.g., `orders.created.v2`) can help manage schema evolution, although schema registries often provide more robust solutions.
- **Environment prefixes**: Including environment information (e.g., `dev.orders.created`, `prod.orders.created`) to distinguish between development, testing, and production environments.

In our reference implementation, we primarily use domain-driven naming, focusing on the business event being represented (e.g., `order.created`, `payment.processed`).

As Scott, Gamov, and Klein note in "Kafka in Action," consistent naming conventions become increasingly important as your Kafka ecosystem grows: "A well-thought-out naming convention will save you countless hours of confusion and debugging as your Kafka deployment scales."

### Topic Granularity: Single vs. Multiple Topics

A key design decision is the granularity of your topics. Should you use a single topic for all events related to a domain, or multiple topics for different event types?

#### Single Topic per Domain

Using a single topic (e.g., `orders`) for all order-related events (`OrderCreated`, `OrderUpdated`, `OrderShipped`):

**Advantages:**
- Simplifies producer configuration
- Maintains ordering across all event types for the same entity (e.g., all events for Order #1234)
- Reduces the number of topics to manage
- Simplifies consumer code when processing multiple event types together

**Disadvantages:**
- Requires consumers to filter events they aren't interested in
- May lead to unnecessary data transfer if consumers only need specific event types
- Can complicate schema evolution if different event types evolve at different rates

#### Multiple Topics per Event Type

Using separate topics for each event type (e.g., `orders.created`, `orders.updated`, `orders.shipped`):

**Advantages:**
- Provides clearer separation of concerns
- Allows consumers to subscribe only to the specific events they need
- Simplifies schema evolution for individual event types
- Enables more granular access control and monitoring

**Disadvantages:**
- Increases the number of topics to manage
- Complicates maintaining order across different event types for the same entity
- May require consumers to join data from multiple topics

```
// Single Topic Approach
Topic: orders
Events: OrderCreated, OrderUpdated, OrderShipped

// Multiple Topic Approach
Topic: orders.created
Events: OrderCreated

Topic: orders.updated
Events: OrderUpdated

Topic: orders.shipped
Events: OrderShipped
```

The best approach depends on the specific use case, the relationships between events, and the access patterns of consumers. Often, a hybrid approach works well, grouping closely related events into a single topic while separating distinct event streams.

Bejeck emphasizes in "Kafka Streams in Action" that "topic design should be driven by how the data will be consumed, not just how it's produced." Consider the downstream consumers and their needs when designing your topic structure.

### Topic Configuration Parameters

Beyond naming and granularity, several key configuration parameters affect topic behavior:

#### Number of Partitions (`num.partitions`)

The number of partitions for a topic is a critical configuration parameter that impacts scalability, parallelism, and ordering guarantees. Choosing the right number of partitions requires balancing several factors:

- **Throughput**: More partitions generally allow for higher producer and consumer throughput, as the load can be distributed across more brokers and consumer instances.
- **Parallelism**: The maximum parallelism for a consumer group is limited by the number of partitions. If you have 10 partitions, you can run at most 10 consumer instances in parallel within a single group.
- **Ordering**: Kafka only guarantees message order within a single partition. If strict ordering is required for related events, they must be sent to the same partition.
- **Resource Utilization**: Each partition consumes resources on the brokers (file handles, memory, CPU). Too many partitions can increase broker load and potentially impact latency.
- **Rebalancing Time**: More partitions can lead to longer consumer group rebalance times.

From "Kafka in Action," a general formula for estimating the minimum number of partitions needed for a topic:

```
min_partitions = max(throughput_in / producer_throughput, throughput_out / consumer_throughput)
```

Where:
- `throughput_in` is the expected peak write throughput for the topic
- `producer_throughput` is the throughput a single producer can achieve
- `throughput_out` is the expected peak read throughput
- `consumer_throughput` is the throughput a single consumer can achieve

For example, if you expect a peak write throughput of 100MB/s, each producer can handle 10MB/s, and each consumer can process 5MB/s, you would need at least max(100/10, 100/5) = max(10, 20) = 20 partitions.

In practice, it's common to add a buffer (e.g., 30-50%) to account for future growth and throughput variations.

#### Replication Factor (`replication.factor`)

The replication factor determines how many copies of each partition are maintained across the cluster:

- **Durability**: Higher replication factors provide better durability in case of broker failures.
- **Availability**: With more replicas, the system can tolerate more broker failures while remaining available.
- **Resource Usage**: Each replica consumes disk space and network bandwidth.

Common settings:
- **Development/Testing**: 1 (no replication)
- **Production (minimum)**: 3 (can tolerate 2 broker failures)
- **Critical Production**: 5 (can tolerate 4 broker failures)

The formula for the number of broker failures a system can tolerate is:
```
max_failures = replication_factor - 1
```

#### Retention Settings

- **Time-based retention** (`retention.ms`): How long messages are kept before deletion.
- **Size-based retention** (`retention.bytes`): Maximum size a partition can grow to before old segments are deleted.
- **Cleanup policy** (`cleanup.policy`): 
  - `delete`: Standard deletion based on retention settings
  - `compact`: Log compaction (keep only the latest value for each key)
  - `compact,delete`: Combination of both (compact but also enforce time/size limits)

#### Segment Settings

- **Segment size** (`segment.bytes`): Maximum size of a segment file before a new one is created.
- **Segment time** (`segment.ms`): Maximum time before a new segment is created.

Smaller segments allow for more granular retention but create more files to manage.

## The Power of Partitioning

Partitioning is Kafka's mechanism for achieving horizontal scalability and parallelism. Understanding how partitioning works is key to leveraging Kafka effectively.

### Purpose of Partitioning

1. **Scalability**: A topic can handle more data than can fit on a single broker by distributing its partitions across multiple brokers.
2. **Parallelism**: Multiple consumers within a group can process different partitions concurrently, increasing overall consumption throughput.
3. **Ordering**: Partitions provide the unit of ordering in Kafka—messages within a partition are strictly ordered.

### Partition Keys and Partitioning Strategies

When a producer sends a message, it can specify a **partition key**. Kafka uses this key to determine which partition the message should be sent to. The default partitioning strategy typically involves hashing the key and taking the result modulo the number of partitions:

```
partition_index = hash(key) % num_partitions
```

This ensures that **all messages with the same key are always sent to the same partition**. This property is fundamental for maintaining order for related events.

#### Built-in Partitioning Strategies

Modern Kafka clients offer several partitioning strategies:

1. **Default Partitioner**:
   - If a partition is explicitly specified, use it
   - If a key is provided, hash the key to determine the partition
   - If no key is provided, use a sticky partitioning strategy (older clients used round-robin)

2. **Sticky Partitioner** (default for keyless messages in newer clients):
   - Assigns batches of messages to the same partition until the batch is full or `linger.ms` is reached
   - Then switches to another partition
   - Improves batching efficiency compared to round-robin

3. **Round-Robin Partitioner**:
   - Distributes messages evenly across all partitions in a circular manner
   - Maximizes load balancing but may reduce batching efficiency

4. **Custom Partitioners**:
   - Implement custom logic for special use cases (e.g., geographic partitioning)

#### Choosing a Partition Key

The choice of partition key is critical and depends on the ordering requirements of your application:

- **Order ID**: If all events related to a specific order must be processed sequentially, use the order ID as the key.
- **Customer ID**: If events related to a specific customer need ordering (e.g., for session tracking), use the customer ID.
- **Device ID**: For IoT scenarios, use the device ID to ensure events from the same device are processed in order.
- **Null Key**: If no specific ordering is required, sending messages with a null key causes the producer to distribute them using the sticky or round-robin strategy.

**Impact of Key Choice:**

- **Ordering**: Guarantees sequential processing for messages with the same key.
- **Distribution**: A good key choice leads to even distribution across partitions. Poor key choices (e.g., keys with low cardinality or skewed distribution) can lead to "hot partitions" where some partitions receive significantly more traffic than others, creating bottlenecks.

```go
// Example: Publishing with a partition key in Go
err = producer.Produce(&kafka.Message{
    TopicPartition: kafka.TopicPartition{
        Topic:     &topic,
        Partition: kafka.PartitionAny, // Let Kafka determine partition based on key
    },
    Key:   []byte(event.OrderID),    // Use OrderID as the key
    Value: payload,
}, nil)
```

### Partition Skew and Hot Partitions

Partition skew occurs when some partitions receive significantly more data or requests than others. This can happen due to:

1. **Uneven Key Distribution**: If the partition key has a skewed distribution (e.g., some values are much more common than others).
2. **Poor Hashing**: If the hash function doesn't distribute keys evenly.
3. **Changing Partition Count**: When partitions are added, the mapping of keys to partitions changes, potentially causing temporary skew.

Hot partitions can cause several problems:

- **Throughput Bottlenecks**: The overall throughput is limited by the most heavily loaded partition.
- **Uneven Broker Load**: Brokers hosting hot partitions experience higher resource utilization.
- **Consumer Lag**: Consumers processing hot partitions may fall behind.

**Mitigating Partition Skew:**

- **Choose High-Cardinality Keys**: Use keys with many possible values that are evenly distributed.
- **Composite Keys**: Combine multiple fields to create a more evenly distributed key.
- **Custom Partitioning**: Implement a custom partitioner that accounts for known skew in your data.
- **Monitor Partition Metrics**: Track bytes-in and message counts per partition to identify skew early.

### Ordering Guarantees Revisited

It's crucial to reiterate Kafka's ordering guarantees:

- **Within a partition**: Messages are strictly ordered based on their offset.
- **Across partitions**: There is **no** guaranteed order between messages in different partitions.

If your application requires global ordering across all events in a topic, you must use a topic with only a single partition. However, this severely limits scalability and parallelism and is generally discouraged unless absolutely necessary.

As noted in "Kafka in Action," "Understanding and leveraging Kafka's ordering guarantees is essential for designing correct event-driven systems. Many distributed systems problems can be solved by ensuring related events are processed in order."

## Consumer Groups and Rebalancing Dynamics

Consumer groups are Kafka's mechanism for enabling parallel consumption while ensuring that each message is processed by only one consumer within the group.

### Managing Offsets

Each consumer group maintains its own set of committed offsets for each partition it consumes. The committed offset represents the position up to which the group has successfully processed messages.

#### Offset Storage

Offsets are stored in a special Kafka topic called `__consumer_offsets`. Each consumer group periodically commits its current position in each partition to this topic. The key format is:
```
[group_id, topic, partition]
```

And the value contains:
```
[offset, metadata, timestamp]
```

#### Offset Commit Strategies

Consumers can commit offsets in several ways:

1. **Auto-commit** (`enable.auto.commit=true`, `auto.commit.interval.ms`):
   - The consumer automatically commits offsets periodically in the background
   - Simple but can lead to:
     - **At-most-once delivery**: If a crash occurs after processing but before the next auto-commit
     - **Duplicate processing**: If a crash occurs after auto-commit but before processing completes

2. **Synchronous Manual Commit** (`commitSync()`):
   - The application explicitly commits offsets and waits for acknowledgment
   - Provides better control but blocks until the commit succeeds
   - Typically used after processing a batch of records

3. **Asynchronous Manual Commit** (`commitAsync()`):
   - The application explicitly commits offsets without waiting for acknowledgment
   - Non-blocking but provides no guarantee that the commit succeeded
   - Often used with a callback to handle commit failures

4. **Exactly-Once Commit** (Transactions API):
   - Atomically commits both processing results and offsets
   - Requires transactional producers and consumers

```kotlin
// Example: Manual offset commit in Kotlin with Spring Kafka
@KafkaListener(topics = ["order.created"], groupId = "payment-service")
fun processOrder(record: ConsumerRecord<String, ByteArray>, acknowledgment: Acknowledgment) {
    try {
        // Deserialize and process the event
        val orderCreated = OrderCreatedEvent.parseFrom(record.value())
        processPayment(orderCreated)
        
        // Manually acknowledge (commit) the offset after successful processing
        acknowled
(Content truncated due to size limit. Use line ranges to read in chunks)