# Chapter 1: The Log-centric World-view

In the realm of distributed systems, few abstractions have proven as powerful and transformative as the humble log. Despite its simplicity—or perhaps because of it—the log has emerged as a foundational concept that underpins many of today's most scalable and resilient systems. This chapter explores how adopting a log-centric perspective fundamentally changes how we design, build, and reason about distributed applications, particularly when using Apache Kafka.

## Beyond Simple Logging: The Distributed Log

When most developers hear the term "log," they immediately think of application logs—those streams of text messages that help debug issues or trace program execution. While useful, these text-based logs represent only a narrow application of a much more powerful concept.

A **distributed log** is not merely a queue with the ability to seek backward. It represents something far more profound: it is the **memory** of your system. In a log-centric architecture, the log becomes the authoritative record of everything that has happened within your system—the single source of truth from which all other state can be derived.

At its core, a log is an append-only, totally-ordered sequence of records ordered by time. Each entry is assigned a unique, sequential identifier (often called an offset or sequence number) that establishes its position in the log. This structure is deceptively simple yet incredibly powerful:

```
[0] Order #1234 created for customer C001
[1] Payment of $59.99 received for order #1234
[2] Inventory item SKU-789 reserved for order #1234
[3] Shipping label created for order #1234
[4] Order #1234 marked as shipped
```

In this example, each log entry represents an event that occurred in the system. The numbers in brackets represent the log offsets—immutable positions that define the strict ordering of events. This ordering creates a notion of "time" that is decoupled from physical clocks, providing a consistent view of causality across distributed components.

## The Inversion of Data Flow

Traditional systems often rely on mutable databases as their source of truth. Applications query these databases to retrieve the current state, modify that state, and write it back. This approach, while familiar, creates numerous challenges in distributed environments: contention, locking, consistency issues across services, and difficulties in tracking how state evolved over time.

The log-centric approach inverts this model. Instead of treating the current state as primary and changes as transient operations, it treats the **changes themselves** as the primary record. Everything that happens—an order submission, an inventory check, a payment rejection—is persisted as an **immutable, ordered event** in the log. The current state becomes a derived artifact, a point-in-time projection created by processing the log from the beginning (or from a known checkpoint) up to the present.

This inversion has profound implications:

### 1. Rebuilding State is Cheap; Snapshots are an Optimization

Since the log contains the complete history of changes, any system can reconstruct the full state by replaying events from the beginning. This property makes recovery from failures straightforward—a crashed service simply reprocesses the log to rebuild its state.

Of course, for efficiency, systems often maintain snapshots or checkpoints to avoid replaying the entire log. But importantly, these snapshots are merely optimizations, not the source of truth. The log remains the authoritative record.

```go
// Simplified example of state reconstruction from events
func rebuildOrderState(events []Event) Order {
    var order Order
    
    for _, event := range events {
        switch event.Type {
        case "OrderCreated":
            order = Order{ID: event.OrderID, Status: "Created"}
        case "PaymentReceived":
            order.Status = "Paid"
        case "InventoryReserved":
            order.Status = "Preparing"
        case "Shipped":