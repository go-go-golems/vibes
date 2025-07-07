# Drone Workflow Platform Makefile

.PHONY: all build clean test proto start stop demo help

# Default target
all: build

# Build all services and CLI tools
build:
	@echo "🔨 Building all services and CLI tools..."
	@mkdir -p bin
	@go build -o bin/workflow-service ./cmd/workflow-service/
	@go build -o bin/stage-service ./cmd/stage-service/
	@go build -o bin/quality-service ./cmd/quality-service/
	@go build -o bin/doc-service ./cmd/doc-service/
	@go build -o bin/workflow-cli ./cmd/workflow-cli/
	@go build -o bin/stage-cli ./cmd/stage-cli/
	@go build -o bin/monitor-cli ./cmd/monitor-cli/
	@echo "✅ Build complete!"

# Build individual services
build-workflow:
	@go build -o bin/workflow-service ./cmd/workflow-service/

build-stage:
	@go build -o bin/stage-service ./cmd/stage-service/

build-quality:
	@go build -o bin/quality-service ./cmd/quality-service/

build-doc:
	@go build -o bin/doc-service ./cmd/doc-service/

# Build CLI tools
build-cli:
	@go build -o bin/workflow-cli ./cmd/workflow-cli/
	@go build -o bin/stage-cli ./cmd/stage-cli/
	@go build -o bin/monitor-cli ./cmd/monitor-cli/

# Generate protobuf code
proto:
	@echo "🔄 Generating protobuf code..."
	@protoc --go_out=. --go-grpc_out=. proto/workflow.proto
	@echo "✅ Protobuf generation complete!"

# Install dependencies
deps:
	@echo "📦 Installing dependencies..."
	@go mod tidy
	@go install google.golang.org/protobuf/cmd/protoc-gen-go@latest
	@go install google.golang.org/grpc/cmd/protoc-gen-go-grpc@latest
	@echo "✅ Dependencies installed!"

# Run tests
test:
	@echo "🧪 Running tests..."
	@go test ./...

# Run integration tests
test-integration:
	@echo "🧪 Running integration tests..."
	@go test ./tests/ -v

# Start the platform
start:
	@echo "🚀 Starting platform..."
	@./scripts/start-platform.sh

# Stop the platform
stop:
	@echo "🛑 Stopping platform..."
	@./scripts/stop-platform.sh

# Run demo
demo:
	@echo "🎬 Running demo..."
	@./scripts/demo.sh

# Clean build artifacts
clean:
	@echo "🧹 Cleaning build artifacts..."
	@rm -rf bin/
	@go clean
	@echo "✅ Clean complete!"

# Install Redis (Ubuntu/Debian)
install-redis:
	@echo "📦 Installing Redis..."
	@sudo apt update
	@sudo apt install -y redis-server
	@sudo systemctl enable redis-server
	@sudo systemctl start redis-server
	@echo "✅ Redis installed and started!"

# Install tmux
install-tmux:
	@echo "📦 Installing tmux..."
	@sudo apt update
	@sudo apt install -y tmux
	@echo "✅ tmux installed!"

# Setup development environment
setup: install-redis install-tmux deps proto build
	@echo "🎉 Development environment setup complete!"

# Format code
fmt:
	@echo "🎨 Formatting code..."
	@go fmt ./...
	@echo "✅ Code formatted!"

# Lint code
lint:
	@echo "🔍 Linting code..."
	@golangci-lint run
	@echo "✅ Linting complete!"

# Run security scan
security:
	@echo "🔒 Running security scan..."
	@gosec ./...
	@echo "✅ Security scan complete!"

# Generate documentation
docs:
	@echo "📚 Generating documentation..."
	@godoc -http=:6060 &
	@echo "✅ Documentation server started on http://localhost:6060"

# Create sample workflow
sample:
	@echo "📝 Creating sample workflow..."
	@./bin/workflow-cli sample -o sample-workflow.json
	@echo "✅ Sample workflow created: sample-workflow.json"

# Health check
health:
	@echo "🏥 Checking system health..."
	@./bin/monitor-cli health

# Show system metrics
metrics:
	@echo "📊 Showing system metrics..."
	@./bin/monitor-cli metrics

# Show recent events
events:
	@echo "📋 Showing recent events..."
	@./bin/monitor-cli events --count 10

# Quick workflow test
quick-test: build
	@echo "⚡ Running quick workflow test..."
	@./bin/workflow-cli sample -o /tmp/test-workflow.json
	@echo "✅ Quick test complete!"

# Development mode (rebuild on changes)
dev:
	@echo "🔄 Starting development mode..."
	@while true; do \
		inotifywait -e modify -r . --exclude bin/ --exclude .git/; \
		make build; \
	done

# Show help
help:
	@echo "Drone Workflow Platform - Available Commands:"
	@echo ""
	@echo "Build Commands:"
	@echo "  make build          - Build all services and CLI tools"
	@echo "  make build-workflow - Build workflow service only"
	@echo "  make build-stage    - Build stage service only"
	@echo "  make build-quality  - Build quality service only"
	@echo "  make build-doc      - Build documentation service only"
	@echo "  make build-cli      - Build CLI tools only"
	@echo ""
	@echo "Development Commands:"
	@echo "  make proto          - Generate protobuf code"
	@echo "  make deps           - Install dependencies"
	@echo "  make setup          - Setup complete development environment"
	@echo "  make fmt            - Format code"
	@echo "  make lint           - Lint code"
	@echo "  make clean          - Clean build artifacts"
	@echo ""
	@echo "Testing Commands:"
	@echo "  make test           - Run unit tests"
	@echo "  make test-integration - Run integration tests"
	@echo "  make quick-test     - Run quick workflow test"
	@echo ""
	@echo "Platform Commands:"
	@echo "  make start          - Start the platform"
	@echo "  make stop           - Stop the platform"
	@echo "  make demo           - Run demo"
	@echo "  make health         - Check system health"
	@echo "  make metrics        - Show system metrics"
	@echo "  make events         - Show recent events"
	@echo ""
	@echo "Utility Commands:"
	@echo "  make sample         - Create sample workflow"
	@echo "  make docs           - Generate documentation"
	@echo "  make security       - Run security scan"
	@echo "  make help           - Show this help"

