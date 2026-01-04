# 🦆 Pelican Genome Sequencer

A demonstration of event-driven progress reporting using Go, Watermill, and Server-Sent Events. This system simulates genome sequencing for various pelican species with real-time progress tracking.

## Features

- **Event-driven architecture** using Watermill for pub/sub messaging
- **Real-time progress updates** via Server-Sent Events (SSE)
- **Multiple pelican species** support with realistic simulation
- **Prometheus metrics** for operational monitoring
- **Dual pub/sub backends**: In-memory (development) and Redis Streams (production)
- **CLI worker** for batch processing
- **Bootstrap-based UI** with live progress visualization

## Quick Start

### Prerequisites

- Go 1.21+ (automatically installed by dev script)
- Redis (optional, for production mode)

### Development Mode

```bash
# Clone and setup
git clone <repository>
cd pelican-demo

# Run development server
./scripts/dev.sh
```

The application will be available at http://localhost:8080

### Production Mode (with Redis)

```bash
# Start Redis
redis-server

# Run with Redis backend
REDIS=1 ./scripts/dev.sh
```

## API Endpoints

| Method | Path | Description |
|--------|------|-------------|
| `GET /` | Main interface | Web UI for starting sequencing jobs |
| `POST /jobs` | Create job | `{"species": "brown_pelican"}` → `{"job_id": "uuid"}` |
| `GET /jobs/{id}/events` | Stream events | Server-Sent Events stream of progress |
| `GET /metrics` | Prometheus metrics | Operational metrics and counters |
| `GET /health` | Health check | Service health status |

## CLI Usage

```bash
# Build CLI worker
go build -o bin/worker ./cmd/worker

# Run sequencing job
./bin/worker --species brown_pelican

# Available options
./bin/worker --help
```

## Supported Species

- **Brown Pelican** (`brown_pelican`) - Pelecanus occidentalis
- **Peruvian Pelican** (`peruvian_pelican`) - Pelecanus thagus  
- **Dalmatian Pelican** (`dalmatian_pelican`) - Pelecanus crispus
- **American White Pelican** (`american_white_pelican`) - Pelecanus erythrorhynchos
- **Australian Pelican** (`australian_pelican`) - Pelecanus conspicillatus

## Architecture

```
Frontend (Bootstrap + JS)
    ↓ HTTP/SSE
HTTP Server (Chi Router)
    ↓ Events
Watermill Publisher/Subscriber
    ↓ Topics: jobs.{id}.progress
Redis Streams / In-Memory Channel
    ↓ Fan-out
[Genome Worker] [Metrics] [Logs]
```

### Components

- **`internal/genome`** - Sequencing simulation logic
- **`internal/progress`** - Event structures and Watermill integration  
- **`internal/http`** - HTTP handlers and SSE streaming
- **`internal/metrics`** - Prometheus metrics collection
- **`cmd/api`** - Main HTTP server
- **`cmd/worker`** - CLI worker for batch jobs

## Event Flow

1. **Job Creation**: POST to `/jobs` creates UUID and starts goroutine
2. **Progress Events**: Genome worker publishes events to Watermill topic
3. **Fan-out**: Events distributed to SSE clients and metrics collectors
4. **Real-time UI**: Browser receives events via SSE and updates progress

## Metrics

Available at `/metrics` endpoint:

- `pelican_events_total{stage,job_id}` - Total events by stage
- `pelican_rate_limited_total{job_id}` - Rate limiting occurrences  
- `pelican_active_jobs` - Currently running jobs
- `pelican_job_duration_seconds{species,status}` - Job completion times
- `pelican_records_processed_total{job_id,species,type}` - Records processed

## Testing

```bash
# Run all tests
go test ./...

# Run with coverage
go test -cover ./...

# Test specific package
go test ./internal/genome
```

## Development

### Project Structure

```
pelican-demo/
├── cmd/
│   ├── api/            # HTTP server main
│   └── worker/         # CLI worker main
├── internal/
│   ├── genome/         # Sequencing simulation
│   ├── progress/       # Events + Watermill
│   ├── http/           # HTTP handlers
│   └── metrics/        # Prometheus metrics
├── web/
│   └── static/         # Frontend assets
├── scripts/
│   └── dev.sh          # Development script
└── bin/                # Built binaries
```

### Adding New Features

1. **New Species**: Add to `genome.GetSpeciesInfo()`
2. **New Metrics**: Add to `internal/metrics/metrics.go`
3. **New Events**: Extend `progress.Event` struct
4. **New Endpoints**: Add routes in `http.SetupRoutes()`

## Deployment

The system is designed for containerized deployment:

```dockerfile
FROM golang:1.21-alpine AS builder
WORKDIR /app
COPY . .
RUN go build -o api ./cmd/api

FROM alpine:latest
RUN apk --no-cache add ca-certificates
WORKDIR /root/
COPY --from=builder /app/api .
COPY --from=builder /app/web ./web
CMD ["./api"]
```

## Troubleshooting

### Common Issues

1. **Port already in use**: Change `PORT` environment variable
2. **Redis connection failed**: Ensure Redis is running on localhost:6379
3. **SSE not working**: Check browser console for connection errors
4. **Missing events**: Verify Watermill topic subscription

### Debug Mode

```bash
# Enable verbose logging
WATERMILL_DEBUG=1 ./bin/api

# Check health endpoint
curl http://localhost:8080/health

# Test job creation
curl -X POST http://localhost:8080/jobs \
  -H "Content-Type: application/json" \
  -d '{"species":"brown_pelican"}'
```

## License

MIT License - see LICENSE file for details.

