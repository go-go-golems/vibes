# Single-Connection Redis Streams Subscriber for Watermill

## Overview

This project implements a **single-connection Redis Streams subscriber** for Watermill that:

* Uses **one** `go-redis` TCP connection for reading messages
* Runs **one** blocking read loop (`XREADGROUP`) over **many topics**
* Delivers messages to Watermill handlers
* Sends ACKs **via the same single connection** (queued and flushed between read calls)

This keeps connection count constant (1 for the subscriber), even if you subscribe to many topics.

## Project Structure

```
single-conn-redis-watermill/
├── docker-compose.yml          # Redis setup (optional, can use direct docker run)
├── go.mod                      # Go module dependencies
├── README.md                   # This file
├── internal/
│   └── singleconnredis/
│       └── subscriber.go       # Single-connection subscriber implementation
└── cmd/
    └── demo/
        └── main.go             # Demo application
```

## Prerequisites

- Go 1.22 or later
- Docker (for Redis)
- Redis 7.x

## Quick Start

### 1. Start Redis

The project includes a Docker setup. Redis is already running via:

```bash
sudo docker run -d --name redis-watermill --network host redis:7
```

Or use docker-compose (if networking issues are resolved):

```bash
sudo docker compose up -d
```

### 2. Build the Demo

```bash
go build -o demo ./cmd/demo
```

### 3. Run the Demo

```bash
./demo
```

You should see output like:

```
[PUBLISHER] sent to orders.created: {"order_id": 1, "status":"created"}
[HANDLER orders-created-handler] got: <uuid> | payload={"order_id": 1, "status":"created"}
[PUBLISHER] sent to orders.cancelled: {"order_id": 1, "status":"cancelled"}
[HANDLER orders-cancelled-handler] got: <uuid> | payload={"order_id": 1, "status":"cancelled"}
...
=== Demo completed successfully ===
```

## Verifying Single Connection

To verify that the subscriber truly uses a single connection:

1. Run the demo in the background:
```bash
./demo &
```

2. Check Redis connections:
```bash
redis-cli CLIENT LIST | grep -v 'redis-cli'
```

You should see:
- **1 connection** with `cmd=xreadgroup` (the subscriber's single connection)
- Additional short-lived connections from the publisher when publishing messages

The key connection to observe is the one running `xreadgroup` - this is the single persistent connection handling all subscribed topics.

## How It Works

### Subscriber Implementation

The `Subscriber` type in `internal/singleconnredis/subscriber.go`:

1. **Single Connection**: Creates one dedicated `redis.Conn` from the client
2. **Read Loop**: Runs a single goroutine that:
   - Snapshots all subscribed topics
   - Drains queued ACKs
   - Issues one `XREADGROUP` call for all topics
   - Fans out messages to topic-specific channels
3. **ACK Queueing**: When handlers call `msg.Ack()`, the ACK is queued and flushed before the next blocking read
4. **Dynamic Topics**: New topics are picked up on the next `BlockTime` timeout

### Key Features

- **Consumer Groups**: Uses Redis Streams consumer groups for fan-out
- **Finite Block Timeout**: Uses a configurable timeout (e.g., 400ms) to periodically refresh the topic list and flush ACKs
- **Thread-Safe**: Properly synchronized access to shared state
- **Clean Shutdown**: Gracefully closes connections and channels

### Constraints

- **Redis Cluster**: All streams read in one call must share a hash slot (use `{tag}` in stream names)
- **Wakeup Latency**: Can't instantly wake a blocking `XREADGROUP` without a second connection; relies on `BlockTime` timeout

## Configuration

The `SubscriberConfig` accepts:

- `Client`: `*redis.Client` (single-node client)
- `ConsumerGroup`: Consumer group name (required)
- `Consumer`: Consumer name (required)
- `BlockTime`: Timeout for `XREADGROUP` blocking (default: 500ms)
- `GroupStartID`: Starting ID for new groups (default: "$")
- `Unmarshaler`: Message unmarshaler (required)

## Testing

The demo publishes to two topics (`orders.created` and `orders.cancelled`) and consumes them over a single connection. All 12 messages (6 per topic) are successfully delivered and acknowledged.

## Production Considerations

1. **Error Handling**: The implementation includes basic error handling with backoff
2. **Monitoring**: Add metrics for connection health, message throughput, and ACK latency
3. **Consumer Group Management**: Ensure consumer groups are properly created before use
4. **PEL Management**: Implement periodic claiming of pending messages for fault tolerance
5. **Graceful Shutdown**: The demo shows proper cleanup on shutdown

## License

MIT License (following Watermill's license)
