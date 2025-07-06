# YouTube Analyzer Go - Makefile

.PHONY: build clean test install deps run help

# Variables
BINARY_NAME=youtube-analyzer
BUILD_DIR=./bin
MAIN_PATH=./main.go
VERSION=1.0.0
LDFLAGS=-ldflags "-X main.Version=$(VERSION)"

# Default target
all: deps build

# Install dependencies
deps:
	@echo "📦 Installing dependencies..."
	go mod tidy
	go mod download

# Build the binary
build: deps
	@echo "🔨 Building $(BINARY_NAME)..."
	mkdir -p $(BUILD_DIR)
	go build $(LDFLAGS) -o $(BUILD_DIR)/$(BINARY_NAME) $(MAIN_PATH)
	@echo "✅ Build complete: $(BUILD_DIR)/$(BINARY_NAME)"

# Build for multiple platforms
build-all: deps
	@echo "🔨 Building for multiple platforms..."
	mkdir -p $(BUILD_DIR)
	
	# Linux AMD64
	GOOS=linux GOARCH=amd64 go build $(LDFLAGS) -o $(BUILD_DIR)/$(BINARY_NAME)-linux-amd64 $(MAIN_PATH)
	
	# Linux ARM64
	GOOS=linux GOARCH=arm64 go build $(LDFLAGS) -o $(BUILD_DIR)/$(BINARY_NAME)-linux-arm64 $(MAIN_PATH)
	
	# macOS AMD64
	GOOS=darwin GOARCH=amd64 go build $(LDFLAGS) -o $(BUILD_DIR)/$(BINARY_NAME)-darwin-amd64 $(MAIN_PATH)
	
	# macOS ARM64 (Apple Silicon)
	GOOS=darwin GOARCH=arm64 go build $(LDFLAGS) -o $(BUILD_DIR)/$(BINARY_NAME)-darwin-arm64 $(MAIN_PATH)
	
	# Windows AMD64
	GOOS=windows GOARCH=amd64 go build $(LDFLAGS) -o $(BUILD_DIR)/$(BINARY_NAME)-windows-amd64.exe $(MAIN_PATH)
	
	@echo "✅ Multi-platform build complete!"

# Install the binary to system PATH
install: build
	@echo "📥 Installing $(BINARY_NAME) to /usr/local/bin..."
	sudo cp $(BUILD_DIR)/$(BINARY_NAME) /usr/local/bin/
	@echo "✅ Installation complete!"

# Run tests
test:
	@echo "🧪 Running tests..."
	go test -v ./...

# Run with example parameters
run: build
	@echo "🚀 Running example analysis..."
	$(BUILD_DIR)/$(BINARY_NAME) "https://www.youtube.com/watch?v=J3oJqan2Gv8" \
		--api-key "$(API_KEY)" \
		--mode quick \
		--verbose

# Run comprehensive analysis
run-comprehensive: build
	@echo "🚀 Running comprehensive analysis..."
	$(BUILD_DIR)/$(BINARY_NAME) "https://www.youtube.com/watch?v=J3oJqan2Gv8" \
		--api-key "$(API_KEY)" \
		--mode comprehensive \
		--verbose \
		--output comprehensive_analysis.json

# Clean build artifacts
clean:
	@echo "🧹 Cleaning build artifacts..."
	rm -rf $(BUILD_DIR)
	rm -rf ./analysis_results
	rm -rf ./logs
	@echo "✅ Clean complete!"

# Format code
fmt:
	@echo "🎨 Formatting code..."
	go fmt ./...

# Lint code
lint:
	@echo "🔍 Linting code..."
	golangci-lint run

# Generate documentation
docs:
	@echo "📚 Generating documentation..."
	godoc -http=:6060 &
	@echo "📖 Documentation available at http://localhost:6060"

# Show help
help:
	@echo "YouTube Analyzer Go - Available Commands:"
	@echo ""
	@echo "  make deps              Install dependencies"
	@echo "  make build             Build the binary"
	@echo "  make build-all         Build for multiple platforms"
	@echo "  make install           Install to system PATH"
	@echo "  make test              Run tests"
	@echo "  make run               Run example analysis (requires API_KEY env var)"
	@echo "  make run-comprehensive Run comprehensive analysis"
	@echo "  make clean             Clean build artifacts"
	@echo "  make fmt               Format code"
	@echo "  make lint              Lint code"
	@echo "  make docs              Generate documentation"
	@echo "  make help              Show this help"
	@echo ""
	@echo "Environment Variables:"
	@echo "  API_KEY                Google Gemini API key (required for run targets)"
	@echo ""
	@echo "Examples:"
	@echo "  make build"
	@echo "  API_KEY=your_key make run"
	@echo "  ./bin/youtube-analyzer --help"

