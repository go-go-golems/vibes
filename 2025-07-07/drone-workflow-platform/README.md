# Drone Manufacturing Workflow Platform

A comprehensive microservice platform for managing drone manufacturing workflows using Watermill + Redis + Protobuf + Golang with CLI interfaces and tmux for testing.

## 🚀 Overview

This platform provides a complete solution for managing complex drone manufacturing workflows with the following key features:

- **Microservice Architecture**: Modular services for workflow management, stage execution, quality control, and documentation
- **Event-Driven Communication**: Real-time messaging using Watermill and Redis Streams
- **Type-Safe Messaging**: Protocol Buffers for reliable inter-service communication
- **Comprehensive CLI Tools**: Command-line interfaces for all operations
- **Real-Time Monitoring**: Live dashboards and system health monitoring
- **Quality Control Integration**: Built-in quality checks and reporting
- **Document Management**: Secure document access with role-based permissions

## 🏗️ Architecture

### Services

1. **Workflow Service** (Port 50051)
   - Manages workflow definitions and executions
   - Orchestrates stage dependencies
   - Tracks execution progress

2. **Stage Service** (Port 50052)
   - Handles individual stage execution
   - Manages worker assignments
   - Tracks stage status and quality results

3. **Quality Service** (Port 50053)
   - Performs quality checks
   - Generates quality reports
   - Maintains quality metrics

4. **Documentation Service** (Port 50054)
   - Manages technical documents
   - Enforces access control
   - Validates document permissions

### Technology Stack

- **Language**: Go 1.21+
- **Messaging**: Watermill with Redis Streams
- **Database**: Redis for caching and event storage
- **Protocol**: gRPC with Protocol Buffers
- **CLI Framework**: Cobra
- **Testing**: tmux for orchestration, testify for unit tests

## 📋 Prerequisites

- Go 1.21 or higher
- Redis server
- tmux (for testing and development)
- Protocol Buffers compiler (protoc)

## 🚀 Quick Start

### 1. Clone and Setup

```bash
git clone <repository-url>
cd drone-workflow-platform
```

### 2. Install Dependencies

```bash
# Install Go dependencies
go mod tidy

# Install protobuf tools (if not already installed)
go install google.golang.org/protobuf/cmd/protoc-gen-go@latest
go install google.golang.org/grpc/cmd/protoc-gen-go-grpc@latest
```

### 3. Start the Platform

```bash
# Start all services using tmux
./scripts/start-platform.sh
```

This will:
- Start Redis server
- Build all microservices
- Launch services in separate tmux windows
- Set up monitoring dashboard
- Prepare CLI testing environment

### 4. Run the Demo

```bash
# In a new terminal or tmux window
./scripts/demo.sh
```

### 5. Stop the Platform

```bash
./scripts/stop-platform.sh
```

## 🛠️ CLI Usage

### Workflow Management

```bash
# Create a sample workflow
./bin/workflow-cli sample -o my-workflow.json

# Create workflow in the system
./bin/workflow-cli create -f my-workflow.json

# List all workflows
./bin/workflow-cli list

# Execute a workflow
./bin/workflow-cli execute <workflow-id> --batch-id batch-001

# Get workflow details
./bin/workflow-cli get <workflow-id>
```

### Stage Management

```bash
# Execute a stage
./bin/stage-cli execute --workflow-id <id> --stage-id <stage> --execution-id <exec-id>

# Check stage status
./bin/stage-cli status <stage-execution-id>

# Watch stage execution in real-time
./bin/stage-cli watch <stage-execution-id>
```

### System Monitoring

```bash
# Real-time dashboard
./bin/monitor-cli dashboard

# System health check
./bin/monitor-cli health

# View system metrics
./bin/monitor-cli metrics

# View recent events
./bin/monitor-cli events --count 10

# Get quality report
./bin/monitor-cli quality-report <execution-id>
```

## 📊 Workflow Definition

Workflows are defined using a comprehensive DSL that includes:

### Basic Structure

```yaml
workflow:
  name: "QuadCopter_Basic_Assembly"
  version: "1.2"
  drone_model: "QC-200"
  
  settings:
    parallel_stations: 4
    quality_threshold: 0.95
    batch_size: 10
    
  materials:
    - id: "frame_main"
      name: "Carbon Fiber Frame"
      type: "component"
      quantity: 1
      supplier: "AeroTech"
      inspection_required: true
      
  stages:
    - id: "frame_prep"
      name: "Frame Preparation"
      type: "assembly"
      depends_on: []
      
      process:
        duration: "10m"
        workers: 1
        skill_level: "basic"
        instructions: "Clean frame, check for defects"
        
      quality_checks:
        - type: "visual"
          criteria: "No cracks, clean surfaces"
          required: true
```

### Documentation Integration

The platform includes comprehensive document management:

```yaml
documentation:
  base_path: "/docs/manufacturing/"
  document_server: "https://docs.company.com/"
  
  standards:
    - id: "iso_9001"
      title: "Quality Management Systems"
      type: "iso"
      number: "ISO 9001:2015"
      mandatory: true
      
  procedures:
    - id: "sop_001"
      title: "Frame Inspection SOP"
      type: "sop"
      approval_level: "operator"
```

## 🔧 Development

### Building Services

```bash
# Build all services
make build

# Build individual service
go build -o bin/workflow-service ./cmd/workflow-service/
```

### Running Tests

```bash
# Unit tests
go test ./...

# Integration tests (requires running services)
go test ./tests/

# Run with coverage
go test -cover ./...
```

### Generating Protobuf Code

```bash
# Regenerate protobuf files
protoc --go_out=. --go-grpc_out=. proto/workflow.proto
```

## 📈 Monitoring and Observability

### Real-Time Dashboard

The monitor CLI provides a real-time dashboard showing:
- Service health status
- Active workflow executions
- Quality metrics and pass rates
- Recent system events
- Performance statistics

### Event Tracking

All system events are tracked in Redis:
- Workflow started/completed
- Stage execution progress
- Quality check results
- System health changes

### Quality Metrics

Comprehensive quality tracking includes:
- Pass/fail rates by check type
- Inspector performance metrics
- Trend analysis over time
- Compliance reporting

## 🔒 Security and Access Control

### Document Access Control

The platform implements role-based access control:

- **Public**: Publicly available documents
- **Internal**: Company internal documents
- **Restricted**: Limited access documents
- **Confidential**: Highly sensitive documents

### Digital Signatures

Support for digital signatures on critical documents and quality approvals.

## 🚀 Deployment

### Production Deployment

For production deployment:

1. **Configure Environment Variables**:
   ```bash
   export REDIS_ADDR=your-redis-server:6379
   export REDIS_PASSWORD=your-redis-password
   ```

2. **Build Production Binaries**:
   ```bash
   CGO_ENABLED=0 GOOS=linux go build -o workflow-service ./cmd/workflow-service/
   ```

3. **Deploy Services**:
   - Use Docker containers or systemd services
   - Configure load balancers for high availability
   - Set up monitoring and alerting

### Scaling Considerations

- **Horizontal Scaling**: Multiple instances of each service
- **Redis Clustering**: For high-throughput messaging
- **Load Balancing**: Distribute requests across service instances
- **Monitoring**: Comprehensive observability stack

## 🧪 Testing

### Test Structure

```
tests/
├── integration_test.go    # End-to-end integration tests
├── unit/                  # Unit tests for individual components
└── fixtures/              # Test data and fixtures
```

### Running Integration Tests

```bash
# Start services first
./scripts/start-platform.sh

# Run integration tests
go test ./tests/ -v

# Stop services
./scripts/stop-platform.sh
```

## 📚 API Reference

### gRPC Services

All services expose gRPC APIs defined in `proto/workflow.proto`:

- **WorkflowService**: Workflow CRUD and execution
- **StageService**: Stage execution and monitoring
- **QualityService**: Quality checks and reporting
- **DocumentationService**: Document management

### Message Types

Key message types include:
- `Workflow`: Complete workflow definition
- `Stage`: Individual stage configuration
- `QualityCheck`: Quality control specification
- `StageStatus`: Real-time stage execution status

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests for new functionality
5. Run the test suite
6. Submit a pull request

## 📄 License

This project is licensed under the MIT License - see the LICENSE file for details.

## 🆘 Support

For support and questions:
- Check the documentation in the `docs/` directory
- Review the CLI help: `./bin/workflow-cli --help`
- Run the demo: `./scripts/demo.sh`
- Check system health: `./bin/monitor-cli health`

## 🎯 Roadmap

Future enhancements planned:
- Web-based dashboard UI
- Advanced analytics and reporting
- Integration with external ERP systems
- Mobile app for floor workers
- AI-powered quality prediction
- Blockchain-based audit trails

