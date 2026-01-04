# Detailed Book Outline: Polyglot Event-Driven Systems with Kafka

*Based on book-toc.md, kafka_content code examples, and research.*

---

## Preface

*   **Introduction:** The rise of distributed, polyglot, real-time systems.
*   **Problem Statement:** Challenges of migrating existing systems (e.g., Go-centric) to Kafka, integrating multiple languages (Kotlin, Ruby), and moving from REST/queues to event-driven.
*   **Book's Approach:** Distilling first principles, practical, hands-on, code-focused (referencing `kafka_content` repo), opinionated, light on ceremony.
*   **Target Audience:** Senior engineers, tech leads, developers (Go, Ruby, JVM), architects seeking production patterns.
*   **Book Structure Overview:** Briefly describe Parts I-V and Appendices.
*   **Reference Implementation:** Mention the accompanying code (`kafka_content`), encourage cloning and running (`docker-compose up`).

---

## Part I — Principles

### Chapter 1: The Log-centric World-view

*   **Concept:** Introduce the distributed log as the system's memory (immutable, ordered events).
    *   Reference: Jay Kreps' article.
    *   Diagram: Simple log structure (append-only, offsets).
*   **Contrast:** Log vs. traditional queues/databases.
*   **State Reconstruction:** Consumers replay the log to build state.
*   **Implications:**
    *   Cheap state rebuilding (snapshots as optimization).
    *   Historical debugging (time-travel replay).
    *   Integration via messages, not endpoints.
*   **Why Kafka?** How Kafka embodies the log abstraction at scale.
*   **Application Logging vs. Data Logs:** Clarify the distinction.

### Chapter 2: Loose Coupling & Eventual Consistency

*   **Concept:** Define loose coupling in microservices.
    *   Reference: Medium article on Loose Coupling & Kafka.
*   **Contrast:** REST (direct calls, tight coupling of availability/latency) vs. Event-Driven (Kafka as intermediary, backlog persistence).
    *   Diagram: REST vs. Kafka communication flow.
*   **The Trade-off:** Eventual Consistency.
    *   Explain the concept and its implications.
    *   Need for idempotent handlers and compensation logic.
*   **Enabling Patterns:** Introduce Sagas and Outbox pattern (detailed later).
*   **Kafka's Role:** How Kafka facilitates loose coupling via asynchronous communication and acting as a buffer.

### Chapter 3: Why Polyglot Services are Inevitable

*   **Concept:** Define Polyglot Architecture and Polyglot Persistence.
    *   Reference: Confluent Developer article notes.
*   **Rationale:** Choosing the best tool (language/database) for the job.
*   **Language Strengths (Examples from TOC):**
    *   Go: Network IO, small footprints, static binaries.
    *   Kotlin/JVM: Mature tooling, coroutines, library ecosystem.
    *   Ruby: Rapid iteration, data science/analytics.
*   **Kafka as the Backbone:** Enabling communication between diverse runtimes via a shared 

truth stream".
*   **Benefits & Challenges:** Discuss trade-offs (flexibility vs. complexity, talent acquisition).
*   **Mitigation:** Limited polyglot approach, choosing compatible technologies (open standards, Kafka).

---

## Part II — Kafka Essentials

### Chapter 4: Kafka From 10,000 ft

*   **Core Components:**
    *   Topics (named, append-only logs).
    *   Partitions (sharding for parallelism).
    *   Brokers (replication, durability).
    *   Producers (writing).
    *   Consumers & Consumer Groups (reading, parallelism guarantee).
    *   ZooKeeper/KRaft (metadata management - briefly mention transition).
*   **Architecture Diagram:** High-level overview (similar to `architecture.txt` but refined).
    *   *Action: Need to create/find a suitable diagram.* 
*   **Key Concepts Explained:**
    *   Retention Policies (time/size-based).
    *   Log Compaction (use cases: event sourcing state, latest config).
    *   Replayability.

### Chapter 5: Breaking Down Topics & Partitions

*   **Topic Design:**
    *   Naming conventions.
    *   Single vs. multiple topics (granularity trade-offs).
*   **Partitioning Deep Dive:**
    *   Purpose: Scalability and parallelism.
    *   Partition Keys: Importance, selection strategies (e.g., order ID, customer ID).
    *   Ordering Guarantees (within a partition).
    *   Impact of key choice on distribution and ordering.
*   **Consumer Groups & Rebalancing:**
    *   How groups manage offsets.
    *   The rebalance process (causes, impact - stop-the-world).
    *   Static group membership (benefits).
*   **Diagrams:** Partition assignment, rebalancing illustration.

### Chapter 6: Delivery Guarantees in Practice

*   **The Spectrum:**
    *   At Most Once (potential loss).
    *   At Least Once (Kafka default, requires idempotent consumers).
    *   Exactly Once Semantics (EOS - transactional producers/consumers, Kafka Streams).
*   **Idempotency:**
    *   Why it's crucial for At Least Once.
    *   Techniques: Using unique message IDs, database constraints, stateful checks.
*   **Producer Configuration:** `acks`, `retries`, `enable.idempotence`.
*   **Consumer Configuration:** `enable.auto.commit`, manual offset commits.
*   **Practical Verification:** Emphasize testing over theoretical guarantees (chaos testing example from TOC).
*   **Code Snippets:** Illustrate producer/consumer config for different guarantees (referencing `kafka_content` where applicable).

---

## Part III — The Polyglot System

*(Focus on integrating code examples from `kafka_content` for each chapter)*

### Chapter 7: Go + Watermill

*   **Introduction to Watermill:** Why use it? (Abstraction over Kafka clients like Sarama).
*   **Core Abstractions:** Publisher, Subscriber, Router, Middleware.
*   **Code Walkthrough (`kafka_content/cmd/order-service`, `kafka_content/pkg/`):**
    *   Setting up publisher/subscriber.
    *   Defining handlers (e.g., `HandleOrderCreated`).
    *   Using the router (`router.AddHandler`).
    *   Implementing middleware (retries, correlation IDs, logging - reference `pkg/logger`).
    *   Domain model vs. Kafka details separation.
    *   Configuration (`docker-compose.yml` environment variables).
*   **Diagram:** Go service interaction with Kafka via Watermill.

### Chapter 8: Introducing Kotlin & Spring Kafka

*   **Why Kotlin/Spring Kafka?** Coroutines, type safety, JVM ecosystem, Spring Boot integration.
*   **Core Components:** `KafkaTemplate`, `@KafkaListener`, `ConsumerFactory`, `ProducerFactory`.
*   **Code Walkthrough (`kafka_content/kotlin-service`):**
    *   Gradle setup (dependencies, Protobuf plugin).
    *   Configuration (`application.yml`).
    *   Producer implementation (`KafkaTemplate`).
    *   Consumer implementation (`@KafkaListener`, error handling, retries - `RetryConfig.kt`).
    *   Deserialization (using Protobuf generated classes - `OrderEvents.kt`).
    *   Coroutines for concurrency (if applicable in examples).
*   **Diagram:** Kotlin service interaction with Kafka via Spring Kafka.

### Chapter 9: Bridging Legacy Ruby with `ruby-kafka`

*   **Use Case:** Integrating existing Ruby services (e.g., analytics).
*   **Introduction to `ruby-kafka`:** Key features.
*   **Code Walkthrough (`kafka_content/ruby-service`):**
    *   Gemfile setup.
    *   Client configuration (`kafka_client.rb`).
    *   Producer implementation.
    *   Consumer implementation (long-running loop, `each_message`).
    *   Handling different topics (`case msg.topic`).
    *   Deserialization (Protobuf - `order_pb.rb`, `order_events.rb`).
    *   Focus on business logic within handlers (`event_processor.rb`).
*   **Performance Considerations:** Ruby's suitability for specific tasks (batching, analytics).
*   **Diagram:** Ruby service interaction with Kafka via `ruby-kafka`.

---

## Part IV — Event-Driven Patterns

*(Focus on illustrating patterns with examples, potentially drawing from `kafka_content/cmd/patterns-demo` or service implementations)*

### Chapter 10: Contracts & Schemas with Protocol Buffers

*   **Importance of Contracts:** Why schemas are vital in distributed systems.
*   **Schema Options:** Avro, JSON Schema, Protocol Buffers (Protobuf).
*   **Why Protobuf?** (Chosen for the book/example).
    *   Language neutrality, efficiency, schema evolution.
*   **Defining Schemas (`idl/order.proto`):**
    *   Syntax basics.
    *   Defining messages and fields.
*   **Code Generation:**
    *   Using `protoc` for Go, Kotlin, Ruby.
    *   Integrating with build tools (Gradle example).
    *   Checking in generated code (rationale).
*   **Schema Evolution:**
    *   Best practices: using optional fields, reserving tags, avoiding renaming/reusing tags.
    *   Compatibility (forward/backward).
*   **Schema Registry (Brief Mention):** Role in managing schemas centrally (though not implemented in detail in `kafka_content`).

### Chapter 11: Sagas: Reliable, Reversible Workflows

*   **Problem:** Achieving consistency across multiple services without distributed transactions.
*   **Saga Pattern Explained:** Sequence of local transactions with compensating actions for failures.
*   **Types:**
    *   Choreography (services react to each other's events - primary focus based on TOC/code).
    *   Orchestration (central coordinator manages flow - mention as alternative).
*   **Implementation (Conceptual & Code References):**
    *   State machine approach (Go example from TOC - `pkg/saga/saga.go`?).
    *   Handling events to trigger steps/compensations.
    *   Idempotency in saga steps.
    *   Kotlin coroutine approach (conceptual, reference `kotlin-service/src/main/kotlin/com/scrapybara/kw/shipping/saga/` if applicable).
*   **Diagram:** Saga flow for order processing (Order -> Payment -> Inventory -> Shipping) with compensations.

### Chapter 12: Event Sourcing & Immutable History

*   **Concept:** Storing the full history of state changes as a sequence of events, not just the current state.
*   **Contrast:** Traditional CRUD vs. Event Sourcing.
*   **Benefits:** Audit trail, historical state reconstruction, debugging.
*   **Implementation with Kafka:**
    *   Using Kafka topics as the event store.
    *   Log compaction for aggregate state snapshots.
    *   Reconstructing aggregate state by replaying events.
*   **Code References (`pkg/eventsourcing`? `ruby-service/lib/event_sourcing.rb`?):**
    *   Defining events.
    *   Appending events.
    *   Loading aggregates from events.
*   **Snapshots:** As performance optimization.
*   **Diagram:** Event Sourcing flow - Command -> Event -> Event Store (Kafka) -> Aggregate Reconstruction.

### Chapter 13: CQRS without the Buzzword Bingo

*   **Concept:** Command Query Responsibility Segregation - Separating the model for writes (commands) from the model for reads (queries).
*   **Rationale:** Optimizing reads and writes independently, different data needs.
*   **Relationship to Event Sourcing:** Often used together but not required.
*   **Practical Implementation (TOC description & code references):**
    *   **Command Side:**
        *   Receiving commands (e.g., via REST API).
        *   Validating commands.
        *   Publishing events to Kafka (as the result of command processing).
        *   Reference: Go services, potentially Ruby service command handling (`analytics_commands.rb`?).
    *   **Query Side:**
        *   Consumers listening to Kafka events.
        *   Updating dedicated read models (e.g., Postgres, Elasticsearch).
        *   Serving queries from the read models.
        *   Reference: Ruby analytics service (`analytics_repository.rb`, `order_analytics_aggregate.rb`?).
*   **Eventual Consistency:** Acknowledging the delay between write and read model updates.
*   **Diagram:** CQRS flow showing command path, event bus (Kafka), and query path.

---

## Part V — Operations

*(Focus on practical aspects, referencing scripts and configurations in `kafka_content`)*

### Chapter 14: Observability: ELK, Traces & Correlation IDs

*   **Importance:** Understanding system behavior, debugging distributed flows.
*   **Three Pillars (Briefly):** Logs, Metrics, Traces.
*   **Logging Strategy:**
    *   Structured logging (JSON).
    *   Correlation IDs: Generating, propagating across services/events.
    *   Shipping logs: Filebeat -> Logstash -> Elasticsearch (ELK stack).
    *   Kibana for visualization.
    *   Reference: `pkg/logger`, `logging/` directory configs, `collect_logs.sh`.
*   **Tracing:**
    *   Concept: Following a request across service boundaries.
    *   Using correlation IDs as a basic form of tracing.
    *   Mention OpenTelemetry as the standard (potential future integration).
*   **Metrics:**
    *   Kafka metrics (broker, producer, consumer lag).
    *   Application metrics.
    *   Mention Prometheus/Grafana as common tooling (though ELK is focus here).
    *   Reference: `kotlin-service/src/main/kotlin/com/scrapybara/kw/shipping/config/MetricsConfig.kt`? `ruby-service/lib/metrics.rb`?

### Chapter 15: Testing: From Contract to Chaos

*   **Testing Pyramid/Diamond in Microservices:** Unit, Integration, Contract, End-to-End, Chaos.
*   **Contract Testing:**
    *   Ensuring schema compatibility between producers and consumers.
    *   Approach: CI job, dump/load binary samples (as described in TOC).
    *   Reference: Potential scripts or CI setup ideas.
*   **Integration Testing:**
    *   Testing flows involving Kafka.
    *   Using embedded Kafka (e.g., `testcontainers`) for local testing.
    *   Reference: `kotlin-service/src/test/kotlin/com/scrapybara/kw/shipping/KafkaIntegrationTest.kt`, `test_system.sh`?
*   **End-to-End Testing:** Verifying business flows across multiple services.
*   **Chaos Testing:**
    *   Injecting failures (killing brokers/consumers, network issues).
    *   Observing system resilience, backlog behavior.
    *   Reference: `test_scenarios.sh`? Chaos scripts ideas.

### Chapter 16: Deployment: Containers, Compose & Kubernetes

*   **Containerization:** Dockerfiles for Go, Kotlin, Ruby services.
    *   Reference: `docker/` directory.
*   **Local Development:**
    *   Using `docker-compose` (`docker-compose.yml`).
    *   Setting up Kafka, ZooKeeper/KRaft, services.
    *   Network configuration (`advertised.listeners`).
    *   Reference: `start_project.sh`.
*   **Moving to Kubernetes (Conceptual):**
    *   StatefulSets for Kafka brokers.
    *   Headless services for discovery.
    *   Volume claims for persistent logs.
    *   Deploying services (Deployments, Services).
    *   Configuration management (ConfigMaps, Secrets).
    *   Reference: `deploy.sh` (if it contains K8s elements).

### Chapter 17: Scaling & Evolution Strategies

*   **Scaling Kafka:**
    *   Adding brokers.
    *   Increasing partition count (primary way to scale throughput).
    *   Scaling consumers (up to partition count per group).
*   **Scaling Services:** Independently scaling consumer/producer instances.
*   **Topic Evolution:**
    *   When to split topics (different retention needs, logical boundaries).
    *   Strategies for migrating consumers to new topics.
*   **Service Evolution:**
    *   Handling schema changes (Chapter 10).
    *   Rewriting services in different languages (leveraging Kafka contracts).
    *   Strangler Fig pattern for gradual replacement.

---

## Appendices

### Appendix A: Configuration Cheat-sheets

*   **Broker Configuration:** Key settings (listeners, replication factor, retention).
*   **Producer Configuration:** Key settings per language (brokers, acks, idempotence, batching, compression).
    *   Reference table from TOC.
*   **Consumer Configuration:** Key settings per language (brokers, group ID, auto commit, offset reset).
    *   Reference table from TOC.

### Appendix B: Glossary

*   Compile key terms used throughout the book (Broker, Topic, Partition, Offset, Consumer Group, Idempotency, Saga, Event Sourcing, CQRS, DLQ, CDC, etc.).
    *   Expand on TOC examples.

### Appendix C: Further Reading

*   Include books/articles mentioned in TOC (Kleppmann, etc.).
*   Add links to Kafka documentation, Confluent resources, Watermill docs, Spring Kafka docs, `ruby-kafka` docs, Protobuf docs.
*   Link to relevant KIPs (e.g., for EOS, static membership).
*   Link to research articles used (Kreps log article, etc.).

---

*End of Detailed Outline*
