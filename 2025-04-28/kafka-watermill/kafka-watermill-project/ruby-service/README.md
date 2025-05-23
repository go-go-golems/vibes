# Analytics Service (Ruby)

This is a Ruby microservice that processes and analyzes events from Kafka. It's part of the larger Kafka/Watermill event-driven microservices system.

## Features

- Consumes events from various Kafka topics
- Processes and stores analytics data
- Provides a web dashboard for visualizing analytics
- Exposes RESTful APIs for accessing analytics data
- Publishes analytical insights back to Kafka
- Exports Prometheus metrics

## Architecture

The service is built with a clean, modular architecture:

- `KafkaClient`: Handles Kafka communication
- `EventProcessor`: Processes incoming Kafka messages
- `AnalyticsRepository`: Stores and retrieves analytics data
- `Metrics`: Tracks operational and business metrics
- `AnalyticsWeb`: Provides web UI and APIs

## Getting Started

### Prerequisites

- Ruby 3.0 or higher
- Bundler gem
- Access to a Kafka broker
- **Protocol Buffers Compiler (`protoc`)**: Needed if you intend to modify or regenerate the Protobuf definitions (`.proto` files).

#### Installing `protoc`

Choose the command appropriate for your system:

*   **Debian/Ubuntu:**
    ```bash
    sudo apt update && sudo apt install -y protobuf-compiler
    ```
*   **Fedora/CentOS/RHEL:**
    ```bash
    sudo dnf install -y protobuf-compiler # Or yum install protobuf-compiler
    ```
*   **macOS (Homebrew):**
    ```bash
    brew install protobuf
    ```
*   **Windows:** See the [official Protobuf installation instructions](https://grpc.io/docs/protoc-installation/).

### Generating Ruby Code from Protobuf (IDL)

This service uses Protocol Buffers (Protobuf) to define the structure of events exchanged via Kafka. The definitions are located in the `../idl/` directory (relative to this service).

While the Ruby classes used for events (`../idl/ruby/order_events.rb`) are checked into the repository, you might need to regenerate them if the `.proto` files change.

1.  **Install the Ruby Protobuf Gem:**
    This gem provides the necessary runtime libraries and the `protoc` plugin for Ruby.
    ```bash
    gem install google-protobuf
    ```
    *(Note: This should already be handled by `bundle install` if `google-protobuf` is in the Gemfile.)*

2.  **Run the `protoc` command:**
    From the root directory of the `kafka-watermill-project` (where the `protoc-gen-order_events` script is located), run:
    ```bash
    chmod +x ./protoc-gen-order_events # Ensure the script is executable
    protoc --proto_path=idl \
           --plugin=protoc-gen-order_events=./protoc-gen-order_events \
           --order_events_out=idl/ruby \
           idl/order.proto
    ```
    *   `--proto_path=idl`: Specifies the directory containing the `.proto` source files.
    *   `--plugin=protoc-gen-order_events=./protoc-gen-order_events`: Specifies the custom plugin script.
    *   `--order_events_out=idl/ruby`: Specifies the output directory for the generated Ruby code using our custom plugin.
    *   `idl/order.proto`: The input Protobuf definition file.

    This command uses the custom `./protoc-gen-order_events` script to generate the `idl/ruby/order_events.rb` file directly, incorporating the custom logic.

3.  **Understanding `order_events.rb` vs. `order_pb.rb`:**
    The file generated and used by this service is `idl/ruby/order_events.rb`. This file is created by our custom `protoc-gen-order_events` plugin.

    If you were to use the standard Ruby Protobuf plugin (`--ruby_out` instead of `--plugin` and `--order_events_out`), `protoc` would generate a different file, typically named `idl/ruby/order_pb.rb`. This standard file would lack the custom Ruby logic (like specific initializers, timestamp parsing, custom JSON methods) present in `order_events.rb`.

    Always use the command provided in step 2 to ensure `order_events.rb` is generated correctly for this project.

### Installation

```bash
# Install dependencies
bundle install

# Run the service
./run.sh
```

### Configuration

The service can be configured via environment variables:

- `KAFKA_BROKERS`: Comma-separated list of Kafka brokers (default: `kafka:9092`)
- `KAFKA_CLIENT_ID`: Client ID for Kafka (default: `analytics-service`)
- `KAFKA_CONSUMER_GROUP`: Consumer group ID (default: `analytics-service-group`)
- `LOG_LEVEL`: Logging level (default: `INFO`)
- `DB_PATH`: Path to SQLite database file (default: `db/analytics.db`)

## API Endpoints

- `GET /`: Web dashboard
- `GET /api/analytics/summary`: Summary statistics
- `GET /api/analytics/orders/status`: Orders by status
- `GET /api/analytics/orders/timeline`: Orders timeline
- `GET /api/analytics/products/top`: Top products
- `GET /api/analytics/products/unavailable`: Inventory shortage analysis
- `POST /api/insights/publish`: Publish analytical insights to Kafka
- `GET /metrics`: Prometheus metrics
- `GET /health`: Health check

## Development

### Running Tests

```bash
bundle exec rspec
```

### Adding New Features

1. Add any new dependencies to the `Gemfile`
2. Create new classes in the `lib/` directory
3. Update the main `app.rb` file to use new components
4. Add tests in the `spec/` directory

## License

MIT