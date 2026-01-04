# Polyglot Event‑Driven Systems with Kafka

*A practical guide to migrating to Kafka and orchestrating Go, Kotlin, and Ruby microservices at scale.*

---

## Preface

Modern software rarely lives in isolation.  It pulses as a network of loosely‑coupled services written in more than one language, deployed by more than one team, and expected to react to events in real time.  
This book grew out of the journey of migrating an existing Go‑centric stack to **Apache Kafka** while introducing **Kotlin** for JVM workloads and keeping the existing **Ruby** data‑science service alive.  
Along the way we distilled a set of **first‑principles** that turn an overwhelmed collection of queues and REST calls into a coherent, observable, *event‑driven* system.

The text is opinionated, hands‑on, and intentionally light on ceremony.  It shows complete, functioning slices of code rather than pseudo‑interfaces.

### Who is this for?

* Senior engineers or tech‑lead profiles who need to guide a team through a Kafka adoption.  
* Developers comfortable in Go, Ruby, or JVM languages who want to learn *how* the other runtimes can plug into the same backbone.  
* Architects looking for real‑life, production‑oriented patterns rather than vendor slides.

### How this book is organised

* **Part I — Principles** introduces the conceptual tools: distributed‑systems fallacies, log‑based thinking, idempotency, and polyglot contracts.  
* **Part II — Kafka Essentials** dives into topics, partitions, consumer groups, delivery semantics and exactly‑once myths.  
* **Part III — The Polyglot System** walks through each service, language by language.  
* **Part IV — Event‑Driven Patterns** explains sagas, event sourcing, CQRS and choreographed vs. orchestrated flows.  
* **Part V — Operations** covers observability, testing, failure drills and continuous deployment.  
* **Appendices** gather cheat‑sheets, glossary, and reference configuration.

Every code listing is copied from an integration‑tested reference implementation that accompanies this book.  Clone it, run `docker‑compose up`, and follow along.

---

## Table of Contents

1. Part I — Principles  
   1. The Log‑centric World‑view  
   2. Loose Coupling & Eventual Consistency  
   3. Why Polyglot Services are Inevitable  

2. Part II — Kafka Essentials  
   4. Kafka From 10 000 ft  
   5. Breaking Down Topics & Partitions  
   6. Delivery Guarantees in Practice  

3. Part III — The Polyglot System  
   7. Go + Watermill  
   8. Introducing Kotlin & Spring Kafka  
   9. Bridging Legacy Ruby with `ruby‑kafka`  

4. Part IV — Event‑Driven Patterns  
   10. Contracts & Schemas with Protocol Buffers  
   11. Sagas: Reliable, Reversible Workflows  
   12. Event Sourcing & Immutable History  
   13. CQRS without the Buzzword Bingo  

5. Part V — Operations  
   14. Observability: ELK, Traces & Correlation IDs  
   15. Testing: From Contract to Chaos  
   16. Deployment: Containers, Compose & Kubernetes  
   17. Scaling & Evolution Strategies  

6. Appendices  
   A. Configuration Cheat‑sheets  
   B. Glossary  
   C. Further Reading  

---

# Part I — Principles

## 1  The Log‑centric World‑view

A **distributed log** is not just a queue‑with‑seek.  It’s the *memory* of your system.  
Everything that happens—an order submission, an inventory check, a payment rejection—is persisted as an **immutable, ordered event**.  
Consumers re‑create state by *replaying* the log instead of asking a mutable database for “the truth”.

This inversion has implications:

* Rebuilding is cheap; snapshots are an optimisation.
* Debugging is historical; you *time‑travel* by replaying into a scratch environment.
* Integration boundaries are messages, not endpoints.

Kafka became popular because it maps exactly to this log abstraction while remaining horizontally scalable.

## 2  Loose Coupling & Eventual Consistency

REST encourages direct calls: *Service A → Service B*.  
That path couples availability & latency.  In an event‑driven topology *A* only needs Kafka to be up; *B* can be upgraded, throttled, or temporarily unavailable—the backlog persists.

The price is **eventual consistency**.  
You trade the comfort of in‑process transactions for explicit *compensation* logic and idempotent handlers.  
Patterns such as **sagas** and **out‑box** tables formalise this trade‑off.

## 3  Why Polyglot Services are Inevitable

Go excels at network IO, small container footprints, and static binaries.  
Kotlin brings mature JVM tooling, non‑blocking coroutines, and an immense library ecosystem.  
Ruby remains unparalleled for rapid iteration where raw throughput is not critical.

A Kafka backbone lets each runtime speak its native tongue while sharing the same *truth stream*.

---

# Part II — Kafka Essentials

## 4  Kafka From 10 000 ft

* **Topics** are named, append‑only logs.  
* **Partitions** shard a topic for horizontal throughput.  
* **Brokers** replicate partitions for durability.  
* **Producers** write; **consumers** read via ordered offsets.  
* **Consumer groups** guarantee that each partition is processed by at most one member in the group, unlocking parallelism without duplicates.

Figure 4‑1 shows the mapping from partitions to consumer‑group members.citeturn0file0

### 4.1  Retention, Compaction & Replay

Kafka’s log is *durable*.  You configure a **retention policy**—time‑based or size‑based.  
Enable **log compaction** on the *event‑sourcing* topics so that only the latest record per key is kept while retaining full detail on audit topics.

### 4.2  Delivery Guarantees

* **At most once** — fast but may lose messages if a consumer crashes before processing.  
* **At least once** — Kafka’s default; duplicate‑tolerant consumers must be idempotent.  
* **Exactly once** — achievable, but only within Kafka + transactional sinks.  Simpler: embrace at‑least‑once and make handlers idempotent.

## 5  Breaking Down Topics & Partitions

*Coming soon: deep dive into partitioning keys, ordering guarantees, and consumer‑group rebalances.*

## 6  Delivery Guarantees in Practice

Forget marketing diagrams—verify behaviour.  
Use a chaos‑script that kills a consumer midway; replay the offset and compare processed IDs.  
Observability beats theoretical guarantees.

---

# Part III — The Polyglot System

## 7  Go + Watermill

Watermill wraps the Sarama client with a *router* abstraction:

```go
router.AddHandler(
    "order.created.handler",
    "order.created",
    subscriber,
    "payment.requested",
    publisher,
    handlers.HandleOrderCreated,
)
```

* Handlers are ordinary functions returning zero, one, or many outbound messages.  
* Middleware adds retries, dead‑letter queues, and correlation IDs without scattering boilerplate.

Keep the **domain model** pure Go structs; keep Kafka‑specific details at the edges.

## 8  Introducing Kotlin & Spring Kafka

Why Kotlin instead of Java?

* Coroutines let you handle thousands of concurrent offsets with minimal threads.  
* Type‑safe builders make configuration readable.

Spring Kafka auto‑wires a `KafkaTemplate` for producing bytes and a `@KafkaListener` for consuming:

```kotlin
@KafkaListener(topics = ["order.confirmed"])
fun onOrderConfirmed(eventBytes: ByteArray) {
    val event = OrderConfirmedEvent.parseFrom(eventBytes)
    shippingService.initiateShipment(event)
}
```

Gradle’s Protobuf plugin generates the Kotlin event classes directly from the shared `.proto` files, eliminating manual DTO drift.

## 9  Bridging Legacy Ruby with `ruby‑kafka`

In Ruby, performance is acceptable for analytics tasks that batch events.  
Use a long‑running consumer:

```ruby
consumer.each_message do |msg|
  case msg.topic
  when "order.created"
    handle_order_created(Ruby::IDL::OrderCreatedEvent.decode(msg.value))
  end
end
```

Reconnect logic belongs in the library; your handler stays focused on business rules.

---

# Part IV — Event‑Driven Patterns

## 10  Contracts & Schemas with Protocol Buffers

Single source of truth:

```
idl/
└── order.proto
```

Run `protoc` for Go, Kotlin, Ruby; check in the generated code.  
Version evolution via optional fields, never rename numeric tags.

## 11  Sagas: Reliable, Reversible Workflows

A **saga** is a sequence of local transactions with explicit compensations.  
Keep the orchestrator simple: one state‑machine struct whose transitions are triggered exclusively by events.

Go example:

```go
func (s *OrderSaga) HandlePaymentResult(ev *PaymentResult) {
    if ev.Ok {
        s.step("reserve_inventory", ev.OrderID)
    } else {
        s.compensate("cancel_order", ev.OrderID)
    }
}
```

Kotlin flips the model: a coroutine per saga instance; suspending functions make the flow read like imperative code.

## 12  Event Sourcing & Immutable History

Store the **change events**, not the state.  
Reconstruct aggregates on demand; snapshot only for performance.  
Use a compacted topic per aggregate plus a *projection* topic feeding read‑models.

## 13  CQRS without the Buzzword Bingo

Separate **commands** that mutate state from **queries** that read projections.  
In practice this means:

* A thin REST façade that publishes commands to Kafka.  
* A projection updater that listens to events and maintains a Postgres read‑model.

---

# Part V — Operations

## 14  Observability: ELK, Traces & Correlation IDs

Each log line embeds `trace_id=<uuid>`.  
Filebeat ships container logs to Logstash; Kibana dashboards visualise per‑saga timelines.  
Add OpenTelemetry exporters later—you already have the IDs.

## 15  Testing: From Contract to Chaos

* **Schema compatibility tests** run in CI; producers dump a binary sample, consumers load it in each language.  
* **Embedded Kafka integration tests** verify end‑to‑end flows with milliseconds of overhead.  
* **Chaos drills**: kill a broker, introduce network partitions; observe backlog behaviour.

## 16  Deployment: Containers, Compose & Kubernetes

Start with `docker‑compose` for local dev; pin your advertised listeners to `kafka:9092`.  
When moving to Kubernetes, create a **headless service** and override DNS in client configs.  
StatefulSets handle broker identity; use volume claims for logs.

## 17  Scaling & Evolution Strategies

* Increase partitions to scale *throughput*, not consumer replicas.  
* Split topics when retention requirements diverge.  
* When a service outgrows its language, rewrite only the slice; the contract stays the same.

---

# Appendices

## A  Configuration Cheat‑sheets

| Setting | Go (Watermill) | Kotlin (Spring Kafka) | Ruby |
| --- | --- | --- | --- |
| Brokers | `Brokers: []string{"kafka:9092"}` | `spring.kafka.bootstrap‑servers=...` | `seed_brokers: [...]` |
| Idempotent producer | `Producer.Idempotent = true` | `enable.idempotence=true` | _n/a_ |
| Consumer group | `ConsumerGroup: "order‑svc"` | `spring.kafka.consumer.group‑id=...` | `group_id: ...` |

## B  Glossary

* **DLQ** – Dead‑letter queue.  
* **CDC** – Change‑data capture.  
* **Idempotency key** – unique token that makes retries safe.

## C  Further Reading

* Kleppmann – *Designing Data‑Intensive Applications*  
* Chen & Rybka – *Distributed Systems Patterns*  
* Kafka Improvement Proposals (KIPs) 98 & 447

---

*End of Manuscript*


