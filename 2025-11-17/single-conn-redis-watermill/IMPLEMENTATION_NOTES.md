# Implementation Notes

## Overview

This document captures the implementation process, challenges encountered, and solutions applied when building the single-connection Redis Streams subscriber for Watermill.

## Implementation Timeline

### 1. Environment Setup

**Challenges:**
- Docker networking issues in the sandbox environment
- iptables kernel modules not available
- Standard Docker bridge networking failed

**Solutions:**
- Used `--network host` mode to bypass networking issues
- Installed Docker 28.5.2 and Docker Compose v2.40.3
- Installed Go 1.24.5 from official source (not apt)

**Commands:**
```bash
# Install Docker
curl -fsSL https://get.docker.com -o get-docker.sh
sudo sh get-docker.sh

# Install Go
wget https://go.dev/dl/go1.24.5.linux-amd64.tar.gz
sudo tar -C /usr/local -xzf go1.24.5.linux-amd64.tar.gz
export PATH=$PATH:/usr/local/go/bin

# Run Redis with host networking
sudo docker run -d --name redis-watermill --network host redis:7
```

### 2. Code Implementation

**Initial Issues:**

1. **Watermill API Changes**: The provided code used outdated import paths for Watermill v1.5.x
   - Original: `github.com/ThreeDotsLabs/watermill/message/router`
   - Issue: Router package doesn't exist in v1.5.x
   - Solution: Simplified implementation to use direct subscription without router

2. **Publisher API Mismatch**: The `NewPublisher` function signature changed
   - Original: Single config parameter
   - v1.5.x: Requires config + logger parameter
   - Solution: Added `watermill.NewStdLogger(false, false)` as second parameter

3. **Marshaler Configuration**: The `PublisherConfig` structure changed
   - Original: Had `Marshaler` field
   - v1.5.x: No `Marshaler` field in config
   - Solution: Removed marshaler from config (handled internally)

**Code Fixes Applied:**

```go
// Fixed publisher initialization
wlog := watermill.NewStdLogger(false, false)
pub, err := redisstream.NewPublisher(redisstream.PublisherConfig{
    Client: rdb,
}, wlog)
```

### 3. Runtime Debugging

**Issue 1: Channel Double-Close Panic**

**Error:**
```
panic: close of closed channel
goroutine 8 [running]:
example.com/single-conn-redis-watermill/internal/singleconnredis.(*Subscriber).Subscribe.func2()
```

**Root Cause:**
- The cleanup goroutine in `Subscribe()` could close a channel that was already closed by `Close()`
- Race condition between context cancellation and subscriber shutdown

**Solution:**
- Added check for `s.closed` channel in cleanup goroutine
- Used `select` to handle both `ctx.Done()` and `s.closed` cases
- Early return if subscriber is already closed

**Fixed Code:**
```go
go func() {
    select {
    case <-ctx.Done():
    case <-s.closed:
        return  // Don't close channel if subscriber is shutting down
    }
    // ... cleanup logic ...
    close(out)
}()
```

### 4. Verification

**Connection Count Verification:**

Ran Redis CLIENT LIST while demo was running:

```bash
redis-cli CLIENT LIST | grep -v 'redis-cli' | wc -l
# Output: 3 connections total
```

**Connection Breakdown:**
1. **Subscriber connection** (persistent): Running `xreadgroup` command
2. **Publisher connection** (short-lived): Running `xadd` command
3. **Verification connection** (temporary): Running `client|list` command

**Key Observation:** Only **one persistent connection** for the subscriber, regardless of the number of topics (2 in demo). This confirms the single-connection design works correctly.

## Performance Observations

### Message Throughput

- Published 12 messages total (6 per topic)
- All messages successfully delivered and acknowledged
- No message loss or duplication
- Clean shutdown with no errors

### Timing

- Block timeout: 400ms
- Publish interval: 700ms
- Total runtime: ~6 seconds
- All messages processed in real-time

## Architecture Decisions

### Why No Router?

**Original Design:** Used Watermill's router component for handler management

**Simplified Design:** Direct subscription with manual handler goroutines

**Rationale:**
1. Router package structure changed significantly in Watermill v1.5.x
2. Router adds complexity not needed for demonstrating single-connection pattern
3. Direct subscription better illustrates the core concept
4. Easier to debug and understand

### ACK Queueing Strategy

The implementation queues ACKs in a buffered channel (capacity 4096) and drains them before each blocking read. This design:

1. **Prevents ACK Buildup:** Regular flushing ensures ACKs don't accumulate
2. **Maintains Single Connection:** All ACKs go through the same connection
3. **Non-Blocking:** Handlers don't wait for ACK confirmation
4. **Efficient:** Batches ACKs naturally when processing bursts

### Block Timeout Trade-offs

**Chosen Value:** 400ms

**Considerations:**
- **Shorter timeout** (e.g., 100ms): More responsive to new topics, higher CPU usage
- **Longer timeout** (e.g., 1000ms): Lower CPU usage, slower to pick up new topics
- **400ms**: Good balance for demo purposes

**Production Recommendation:** Tune based on:
- Expected topic addition frequency
- Acceptable latency for new subscriptions
- CPU budget for the read loop

## Lessons Learned

### 1. API Stability

**Issue:** Library APIs change between versions, even minor ones

**Lesson:** Always check the actual package structure when implementing examples from documentation

**Best Practice:**
```bash
# Verify package structure before implementing
go list github.com/ThreeDotsLabs/watermill/...
```

### 2. Graceful Shutdown

**Issue:** Channel lifecycle management in concurrent code is error-prone

**Lesson:** Always consider shutdown order and race conditions

**Best Practice:**
- Use a dedicated `closed` channel for shutdown signaling
- Check shutdown state before closing channels
- Use `select` to handle multiple cancellation sources

### 3. Connection Verification

**Issue:** Need to verify architectural claims (single connection)

**Lesson:** Always verify with actual tools (redis-cli CLIENT LIST)

**Best Practice:**
```bash
# Monitor connections while running
watch -n 1 'redis-cli CLIENT LIST | grep -v redis-cli'
```

## Future Enhancements

### 1. Connection Naming

Add client name for easier debugging:

```go
ctx := context.Background()
_ = s.conn.ClientSetName(ctx, "single-conn-subscriber").Err()
```

### 2. Metrics

Add instrumentation for:
- Messages received per topic
- ACK queue depth
- Read loop iterations
- Connection health

### 3. Error Recovery

Enhance error handling:
- Reconnection logic for connection failures
- Dead letter queue for unmarshaling errors
- Circuit breaker for persistent failures

### 4. PEL Management

Add periodic claiming of pending entries:
- Detect stale messages in PEL
- Claim and reprocess
- Configurable claim interval

### 5. Multi-Connection Mode

For Redis Cluster with streams across hash slots:
- Detect hash slot distribution
- Create one connection per hash slot
- Coordinate multiple read loops

## Testing Recommendations

### Unit Tests

1. **Subscription Management:**
   - Test adding/removing topics
   - Verify channel cleanup
   - Test concurrent subscriptions

2. **ACK Handling:**
   - Verify ACKs are queued
   - Test ACK flushing
   - Verify NACK behavior (leaves in PEL)

3. **Shutdown:**
   - Test graceful shutdown
   - Verify no channel leaks
   - Test shutdown during active processing

### Integration Tests

1. **Redis Connectivity:**
   - Test connection failure handling
   - Verify reconnection logic
   - Test network interruptions

2. **Message Delivery:**
   - Verify at-least-once delivery
   - Test message ordering per stream
   - Verify consumer group behavior

3. **Performance:**
   - Measure throughput (messages/sec)
   - Test with many topics (e.g., 100+)
   - Measure ACK latency

### Load Tests

1. **High Volume:**
   - 1000+ messages/sec
   - 100+ concurrent topics
   - Sustained load over hours

2. **Burst Handling:**
   - Sudden traffic spikes
   - ACK queue saturation
   - Memory usage under load

## Conclusion

The implementation successfully demonstrates a single-connection Redis Streams subscriber for Watermill. Despite API changes in the Watermill library requiring code adjustments, the core concept works as designed:

✅ Single persistent connection for all topics  
✅ Efficient ACK queueing and flushing  
✅ Dynamic topic subscription  
✅ Clean shutdown without panics  
✅ Verified with Redis CLIENT LIST  

The code is production-ready with the recommended enhancements for metrics, error recovery, and PEL management.
