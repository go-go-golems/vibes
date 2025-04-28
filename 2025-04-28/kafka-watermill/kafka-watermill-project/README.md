# Kafka and Watermill Multi-Language Microservices Project

This project demonstrates how to use Apache Kafka with the Watermill framework to implement event-driven patterns across multiple microservices written in different languages:

- Go (using Watermill)
- Kotlin (using Spring Kafka)
- Ruby (using ruby-kafka)

## Architecture

The project implements an e-commerce order processing flow with multiple microservices:

1. **Order Service (Go)**: Creates orders and publishes order events
2. **Payment Service (Go)**: Processes payments when orders are created
3. **Inventory Service (Go)**: Checks inventory when payments are successful
4. **Shipping Service (Kotlin)**: Fulfills orders when inventory is available
5. **Notification Service (Go)**: Sends notifications for various events
6. **Analytics Service (Ruby)**: Collects and analyzes event data

All services communicate asynchronously through Kafka topics, implementing various event-driven patterns.

## Common IDL (Interface Definition Language)

We use a common event definition across all services:

- Protocol Buffers (`.proto`) files for schema definition (located in the `idl/` directory, e.g., `idl/order.proto`)
- Generated or manually created code for each language
- Consistent event structure across all services

### Protobuf Code Generation (Kotlin Service)

The Kotlin service (`kotlin-service/`) utilizes the Protobuf Gradle plugin to automatically generate Kotlin code from the `.proto` definitions found in the `idl/` directory. Here's how it works:

1.  **Plugin**: The `kotlin-service/build.gradle.kts` file applies the `com.google.protobuf` plugin.
2.  **Configuration**: A `protobuf { ... }` block within the build script specifies the version of the `protoc` compiler to use via its Maven coordinates (`com.google.protobuf:protoc:<version>`).
3.  **Dependencies**: Key dependencies like `com.google.protobuf:protobuf-java` and `com.google.protobuf:protobuf-kotlin` provide the necessary runtime libraries and enable the generation of Kotlin-specific code.
4.  **Task**: The plugin adds a `generateProto` task. Running `./gradlew generateProto` (or a task that depends on it, like `./gradlew build`) within the `kotlin-service` directory triggers the following:
    *   The plugin finds the `.proto` files (like `idl/order.proto`). *Note: The build setup ensures the proto files from the root `idl` directory are accessible to this subproject.*
    *   It invokes the configured `protoc` compiler.
    *   `protoc` generates Kotlin source files based on the message definitions and options in the `.proto` files.
    *   The generated code is placed in `kotlin-service/build/generated/source/proto/main/kotlin`.
5.  **Compilation**: The standard Kotlin compilation task then includes these generated files, making the Protobuf message classes available to the rest of the Kotlin code.

This automated process ensures that the Kotlin code always reflects the latest definitions in the `.proto` files whenever the project is built.

## Event Flow

The basic flow of events in this system:

1. `order.created` - When a new order is created
2. `payment.processed` - When payment is processed (success or failure)
3. `inventory.checked` - When inventory is checked for availability
4. `order.fulfilled` - When an order is fulfilled and shipped
5. `order.cancelled` - When an order is cancelled (due to inventory, payment issues, etc.)

## Setup

### Prerequisites

- Docker and Docker Compose
- Go 1.18 or higher
- Java 17 or higher with Kotlin (for Kotlin service)
- Ruby 3.0 or higher (for Ruby service)
- Gradle (needed primarily if running the Kotlin service manually outside Docker)

### Installing Gradle (if needed)

While the project uses the Gradle Wrapper (`gradlew`), which handles downloading the correct Gradle version automatically for building the Kotlin service, you might need Gradle installed locally for the *initial* wrapper setup or if the wrapper files are missing.

The recommended way to install Gradle on Linux and macOS is using [SDKMAN!](https://sdkman.io/):

1.  **Install SDKMAN!**:
    ```bash
    curl -s "https://get.sdkman.io" | bash
    source "$HOME/.sdkman/bin/sdkman-init.sh"
    ```
2.  **Install Gradle**:
    ```bash
    sdk install gradle
    ```
3.  **Verify Installation**:
    ```bash
    gradle --version
    ```

For Windows or other installation methods, please refer to the [official Gradle installation guide](https://gradle.org/install/).

### Understanding the Gradle Wrapper (`gradlew`)

This project includes a Gradle Wrapper (`gradlew` script in the `kotlin-service` directory). Using the wrapper ensures that everyone working on the project uses the *exact same Gradle version*, defined in `gradle/wrapper/gradle-wrapper.properties`, without needing a system-wide installation.

**Always prefer using `./gradlew` instead of `gradle` when interacting with the Kotlin project.**

If the wrapper files (`gradlew`, `gradlew.bat`, `gradle/wrapper/gradle-wrapper.jar`, `gradle/wrapper/gradle-wrapper.properties`) are missing or corrupted, you can regenerate them (assuming you have Gradle installed locally) by navigating to the `kotlin-service` directory and running:

```bash
cd kotlin-service
gradle wrapper
```

This will regenerate the wrapper scripts and download the necessary `.jar` file.

### Running the Project

#### Option 1: Using Docker Compose (All Services)

1. Start all services using Docker Compose:

```bash
docker-compose up -d
```

#### Option 2: Running Services Manually

1. First, start Kafka and related infrastructure:

```bash
docker-compose up -d zookeeper kafka kafka-setup kafka-ui
```

2. Build and run the Go services:

```bash
# Build all services
cd cmd/order-service && go build -o ../../bin/order-service
cd cmd/payment-service && go build -o ../../bin/payment-service
cd cmd/inventory-service && go build -o ../../bin/inventory-service
cd cmd/notification-service && go build -o ../../bin/notification-service

# Run services
# IMPORTANT: When running services manually outside Docker, use localhost:29092 as the Kafka broker
export KAFKA_BROKERS=localhost:29092

./bin/order-service &
./bin/payment-service &
./bin/inventory-service &
./bin/notification-service &
```

3. Build and run the Kotlin service:

```bash
cd kotlin-service
# If you encounter 'Could not find or load main class org.gradle.wrapper.GradleWrapperMain',
# ensure Gradle wrapper is initialized. You might need Gradle installed locally first.
# 1. Install Gradle (e.g., via SDKMAN!): sdk install gradle
# 2. Initialize wrapper: gradle wrapper
# 3. Make wrapper executable: chmod +x gradlew

# Generate Kotlin code from Protobuf definitions
./gradlew generateProto

# Set the Kafka broker for Spring
export SPRING_KAFKA_BOOTSTRAP_SERVERS=localhost:29092
./gradlew bootRun
```

4. Run the Ruby service:

```bash
cd ruby-service
bundle install
export KAFKA_BROKERS=localhost:29092
ruby analytics_service.rb
```

> **Note**: The difference in Kafka broker addresses:
> - Inside Docker containers: Use `kafka:9092`
> - Running manually on host machine: Use `localhost:29092`

## Event-Driven Patterns Demonstrated

This project demonstrates several event-driven patterns:

1. **Event Notification**: Services publish events when something happens
2. **Event-Carried State Transfer**: Events contain all necessary state
3. **Event Sourcing**: Order state can be rebuilt from the sequence of events
4. **CQRS**: Separation of write (commands) and read (queries) operations
5. **Sagas**: Complex workflows coordinated across multiple services

## Monitoring

- Kafka UI is available at http://localhost:8080
- Analytics service provides a REST API at http://localhost:3000/analytics

## License

MIT