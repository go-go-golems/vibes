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