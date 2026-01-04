# Chapter 9: Ruby: Dynamic Productivity with ruby-kafka

Completing our tour of the polyglot landscape in Part III, we arrive at Ruby—a language celebrated for its elegant syntax, developer productivity, and vibrant ecosystem. While often associated with web development frameworks like Ruby on Rails, Ruby is also a capable language for building various types of backend services, including those participating in event-driven architectures. As discussed in Chapter 3, Ruby excels in scenarios where rapid development, readability, and data manipulation are key priorities.

In our reference implementation, Ruby powers the Analytics service (`kafka_content/ruby-service`), which consumes events from various stages of the order lifecycle to compute and expose business metrics. This chapter explores how the `ruby-kafka` gem provides an idiomatic and effective way for Ruby applications to interact with Apache Kafka.

## The Role of Ruby in Our System

While Go handles high-throughput core services and Kotlin manages complex stateful processes, Ruby finds its niche in the Analytics service. This service listens to events like `OrderCreated`, `PaymentProcessed`, `OrderFulfilled`, and `OrderCancelled`, aggregating data to provide insights into the system's performance. Ruby's strengths make it suitable for this task:

- **Expressiveness**: Ruby code is often concise and reads almost like natural language, making the analytics logic easy to understand and maintain.
- **Data Manipulation**: Ruby's rich set of built-in methods for arrays, hashes, and strings simplifies the processing and transformation of event data.
- **Rapid Development**: The dynamic nature of Ruby allows for quick iteration and adaptation as analytics requirements evolve.
- **Ecosystem**: Gems for data processing, web serving (like Sinatra, used here), and database interaction are readily available.

## Introducing `ruby-kafka`

`ruby-kafka` is the most widely used and actively maintained Kafka client library for Ruby. It provides a comprehensive API for interacting with Kafka brokers, covering both producing and consuming messages.

Key features of `ruby-kafka` include:

- **Producer API**: Methods for sending messages synchronously or asynchronously, with support for partitioning.
- **Consumer API**: Support for consumer groups, manual and automatic offset management, and different subscription modes.
- **Broker Discovery**: Automatic discovery of brokers in the cluster.
- **Compression**: Support for Gzip and Snappy compression.
- **SSL/SASL Authentication**: Secure communication with Kafka brokers.

## Setting Up `ruby-kafka`

Integrating `ruby-kafka` into our Ruby service involves adding the gem and configuring the client.

### Dependencies (`Gemfile`)

First, we add `ruby-kafka` to our project's `Gemfile`:

```ruby
# Gemfile
source "https://rubygems.org"

gem "kafka", "~> 1.4" # Or the latest version of ruby-kafka
gem "sinatra", "~> 3.0" # For the web interface
gem "protobuf", "~> 3.19" # For Protobuf serialization
gem "concurrent-ruby" # For background processing
# ... other gems
```

Then, run `bundle install` to install the dependencies.

### Configuring the Kafka Client

We initialize the `ruby-kafka` client, providing the necessary configuration, such as the list of seed brokers and a client ID.

```ruby
# From kafka_content/ruby-service/analytics_service.rb
require 'kafka'
require 'logger'

# Set up logging
logger = Logger.new(STDOUT)
logger.level = Logger::INFO

# Configure Kafka client
kafka = Kafka.new(
  seed_brokers: ['kafka:9092'], # List of initial brokers to connect to
  client_id: 'analytics-service', # Identifier for this client instance
  logger: logger # Integrate with Ruby's standard logger
)
```

`ruby-kafka` uses the `seed_brokers` to connect to the cluster and discover the rest of the brokers. The `client_id` helps identify this specific client instance in Kafka logs and metrics.

## Consuming Messages with `ruby-kafka`

The Analytics service primarily acts as a consumer, listening to various topics. `ruby-kafka` provides a straightforward way to create consumers and subscribe to topics.

### Creating a Consumer

We create a consumer instance associated with a specific `group_id`. This ID is essential for Kafka's consumer group functionality, ensuring that partitions are distributed among consumers in the group and offsets are tracked correctly.

```ruby
# From kafka_content/ruby-service/analytics_service.rb
consumer = kafka.consumer(
  group_id: 'analytics-service-group' # Unique ID for this consumer group
)
```

### Subscribing to Topics

Once the consumer is created, we subscribe it to the topics we're interested in. The Analytics service subscribes to multiple topics related to the order lifecycle.

```ruby
# From kafka_content/ruby-service/analytics_service.rb
consumer.subscribe('order.created')
consumer.subscribe('payment.processed')
consumer.subscribe('inventory.checked') # Assuming this topic exists
consumer.subscribe('order.fulfilled')
consumer.subscribe('order.cancelled')
```

### Processing Messages

The `each_message` method provides a simple blocking loop that yields each message received from the subscribed topics. This is the core of our consumer logic.

```ruby
# From kafka_content/ruby-service/analytics_service.rb

# Initialize analytics data structure (simple example)
order_counts = {
  created: 0,
  payment_success: 0,
  payment_failed: 0,
  fulfilled: 0,
  cancelled: 0
}

begin
  consumer.each_message do |message|
    topic = message.topic
    value = message.value # Raw message payload (bytes)
    key = message.key     # Partition key (bytes or nil)
    offset = message.offset
    partition = message.partition

    logger.debug("Received message from topic=#{topic}, partition=#{partition}, offset=#{offset}")

    begin
      # 1. Deserialize based on topic (assuming Protobuf)
      case topic
      when 'order.created'
        # Assumes Protobuf generated classes exist in KafkaWatermill::IDL
        event = KafkaWatermill::IDL::OrderCreated.decode(value)
        order_counts[:created] += 1
        logger.info("Order created event processed: #{event.order_id}")

      when 'payment.processed'
        event = KafkaWatermill::IDL::PaymentProcessed.decode(value)
        if event.status == 'COMPLETED' # Assuming status field exists
          order_counts[:payment_success] += 1
          logger.info("Payment success event processed: #{event.order_id}")
        else
          order_counts[:payment_failed] += 1
          logger.info("Payment failed event processed: #{event.order_id}")
        end

      when 'order.fulfilled'
        event = KafkaWatermill::IDL::OrderFulfilled.decode(value)
        order_counts[:fulfilled] += 1
        logger.info("Order fulfilled event processed: #{event.order_id}")

      when 'order.cancelled'
        event = KafkaWatermill::IDL::OrderCancelled.decode(value)
        order_counts[:cancelled] += 1
        logger.info("Order cancelled event processed: #{event.order_id}")
        
      # Add cases for other subscribed topics like 'inventory.checked'
      # when 'inventory.checked'
      #   event = KafkaWatermill::IDL::InventoryChecked.decode(value)
      #   logger.info("Inventory checked event processed: #{event.order_id}")

      else
        logger.warn("Received message from unexpected topic: #{topic}")
      end

      # 2. Update analytics (in this case, simple counts)
      # (Already done within the case statement)

      # 3. Log aggregated metrics periodically (example)
      if (order_counts[:created] % 10) == 0 && order_counts[:created] > 0
        logger.info("ANALYTICS REPORT: #{order_counts.inspect}")
      end

      # 4. Acknowledge message (handled automatically by each_message by default)

    rescue => e
      # Handle deserialization or processing errors for a single message
      logger.error("Error processing message from topic #{topic} at offset #{offset}: #{e.message}")
      logger.error(e.backtrace.join("\n"))
      # Decide whether to stop the consumer or skip the message
      # By default, `each_message` continues on error
    end
  end
rescue => e
  # Handle consumer-level errors (e.g., connection issues)
  logger.error("Kafka consumer error: #{e.message}")
  logger.error(e.backtrace.join("\n"))
  # Consider exiting or attempting to reconnect
end
```

Key points in this processing loop:

- **`each_message` Loop**: Continuously fetches and yields messages.
- **Deserialization**: The raw `message.value` needs to be deserialized. We use a `case` statement based on the `message.topic` and assume corresponding Protobuf classes (`KafkaWatermill::IDL::*`) generated from our `.proto` files are available. The `decode` method is used for Protobuf deserialization.
- **Error Handling**: Includes `begin`/`rescue` blocks to catch errors during individual message processing and errors related to the consumer itself.
- **Offset Management**: By default, `each_message` automatically commits offsets after the block for a message executes successfully. This provides "at least once" delivery semantics. For more control, `each_message` can be configured with `automatically_mark_as_processed: false`, requiring manual offset marking.

## Integrating Protobuf with Ruby

Similar to Go and Kotlin, we rely on Protocol Buffers for schema definition and serialization across our polyglot services.

1.  **Define Schemas**: Use the same `.proto` files as the other services.
2.  **Generate Ruby Code**: Use `protoc` with the Ruby plugin (`gem install google-protobuf`) to generate Ruby classes (`*_pb.rb` files) corresponding to the Protobuf messages.

   ```bash
   protoc --proto_path=../idl --ruby_out=lib/idl *.proto
   ```

3.  **Require Generated Files**: Ensure the generated Ruby files are required in your application.

   ```ruby
   # Example: Assuming generated files are in lib/idl
   require_relative 'lib/idl/order_events_pb'
   # ... require other generated files ...
   ```

4.  **Serialize/Deserialize**: Use the `encode` and `decode` methods provided by the generated classes.

   ```ruby
   # Deserialize
   event = KafkaWatermill::IDL::OrderCreated.decode(message.value)
   puts event.order_id

   # Serialize (if producing)
   # new_event = KafkaWatermill::IDL::AnalyticsUpdated.new(order_id: '123', metric: 'count', value: 10)
   # payload = new_event.encode
   # producer.produce(payload, topic: 'analytics.updated', key: '123')
   ```

## Running the Consumer

The `each_message` loop is blocking. To allow other tasks (like running a web server) to execute concurrently, we run the consumer loop in a separate thread or use a concurrency library like `concurrent-ruby`.

```ruby
# From kafka_content/ruby-service/analytics_service.rb
require 'concurrent'

# Start the analytics processor in a background thread using concurrent-ruby
processor = Concurrent::Promise.execute do
  logger.info('Analytics service starting to consume events...')
  # ... consumer.each_message loop as shown above ...
end

# ... Run Sinatra web server or other tasks ...

# Ensure graceful shutdown
at_exit do
  logger.info('Shutting down analytics service...')
  consumer.stop # Signals the consumer loop to terminate
end

# Optional: Wait for the processor promise if needed
# processor.wait!
```

The `at_exit` block is crucial for ensuring the consumer is stopped gracefully when the application terminates, allowing it to commit final offsets and leave the consumer group cleanly.

## Exposing Analytics via a Web Interface (Sinatra)

The reference implementation uses the lightweight Sinatra web framework to expose the aggregated analytics data via a simple HTTP endpoint.

```ruby
# From kafka_content/ruby-service/analytics_service.rb
require 'sinatra'
require 'json'

class AnalyticsAPI < Sinatra::Base
  set :bind, '0.0.0.0' # Bind to all interfaces
  set :port, 3000      # Run on port 3000

  # Make analytics data accessible (needs proper scoping/sharing)
  # This assumes order_counts is accessible here; consider using a class
  # or other mechanism for sharing state between the consumer thread and web server.
  # For simplicity, let's assume it's globally accessible (not recommended for production).
  $analytics_data = order_counts 

  get '/analytics' do
    content_type :json
    {
      order_counts: $analytics_data,
      timestamp: Time.now.iso8601
    }.to_json
  end

  get '/health' do
    content_type :json
    { status: 'UP' }.to_json
  end
end

# Start the Sinatra app (blocks the main thread)
AnalyticsAPI.run!
```

This demonstrates how easily other Ruby gems can be integrated alongside `ruby-kafka` to build a complete service.

## Considerations for Production Ruby Services

While the reference implementation provides a functional example, building production-grade Ruby Kafka consumers involves additional considerations:

- **Concurrency**: The `each_message` loop processes messages sequentially within a single thread. For higher throughput, you might need multiple consumer processes or explore libraries that offer thread-based parallel processing within a single consumer instance (though this can complicate offset management).
- **Error Handling**: Implement robust strategies for handling persistent message failures, potentially including manual DLQ logic if automatic retries are insufficient.
- **Offset Management**: For critical applications, consider manual offset commits (`automatically_mark_as_processed: false`) to ensure messages are only acknowledged after successful processing, potentially involving database transactions (e.g., the Outbox pattern, though more complex in Ruby).
- **Deployment**: Use tools like Docker, systemd, or process managers (like Foreman or Puma with background workers) to manage the consumer process lifecycle.
- **Monitoring**: Integrate monitoring tools (e.g., Prometheus exporters, Datadog agents) to track consumer lag, processing rates, and error counts.

## Conclusion

Ruby, paired with the `ruby-kafka` gem, offers a productive and expressive way to build services that participate in a Kafka-based event-driven architecture. Its dynamic nature and rich ecosystem make it particularly well-suited for tasks like data aggregation, analytics, and rapid prototyping, as demonstrated by our Analytics service.

In this chapter, we covered:

- Configuring the `ruby-kafka` client.
- Subscribing to topics and processing messages using `consumer.each_message`.
- Integrating Protobuf for cross-language event serialization.
- Running the consumer concurrently with other tasks.
- Basic error handling and offset management concepts in `ruby-kafka`.

This concludes our exploration of the specific language implementations in Part III. We've seen how Go, Kotlin, and Ruby each bring their unique strengths to the table, all effectively integrated through Kafka as the central event backbone. In Part IV, we will shift our focus to higher-level architectural patterns commonly used in event-driven systems, starting with managing data contracts using schemas.
