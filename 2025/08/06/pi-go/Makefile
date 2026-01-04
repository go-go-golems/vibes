# Pi-Go Makefile

# Build variables
BINARY_NAME=pi
TUI_BINARY_NAME=pi-tui
VERSION?=dev
COMMIT?=$(shell git rev-parse --short HEAD 2>/dev/null || echo "unknown")
DATE?=$(shell date -u +"%Y-%m-%dT%H:%M:%SZ")
LDFLAGS=-ldflags "-X main.version=$(VERSION) -X main.commit=$(COMMIT) -X main.date=$(DATE)"

# Go variables
GOCMD=go
GOBUILD=$(GOCMD) build
GOCLEAN=$(GOCMD) clean
GOTEST=$(GOCMD) test
GOGET=$(GOCMD) get
GOMOD=$(GOCMD) mod
GOFMT=$(GOCMD) fmt

# Directories
BUILD_DIR=build
DIST_DIR=dist
CMD_DIR=cmd

# Default target
.PHONY: all
all: clean deps fmt lint test build

# Install dependencies
.PHONY: deps
deps:
	$(GOMOD) download
	$(GOMOD) tidy

# Format code
.PHONY: fmt
fmt:
	$(GOFMT) ./...

# Lint code
.PHONY: lint
lint:
	@if command -v golangci-lint >/dev/null 2>&1; then \
		golangci-lint run; \
	else \
		echo "golangci-lint not installed, skipping lint"; \
	fi

# Run tests
.PHONY: test
test:
	$(GOTEST) -v ./...

# Run tests with coverage
.PHONY: test-coverage
test-coverage:
	$(GOTEST) -v -coverprofile=coverage.out ./...
	$(GOCMD) tool cover -html=coverage.out -o coverage.html

# Build CLI binary
.PHONY: build
build: build-cli build-tui

.PHONY: build-cli
build-cli:
	mkdir -p $(BUILD_DIR)
	$(GOBUILD) $(LDFLAGS) -o $(BUILD_DIR)/$(BINARY_NAME) $(CMD_DIR)/pi

.PHONY: build-tui
build-tui:
	mkdir -p $(BUILD_DIR)
	$(GOBUILD) $(LDFLAGS) -o $(BUILD_DIR)/$(TUI_BINARY_NAME) $(CMD_DIR)/pi-tui

# Build for multiple platforms
.PHONY: build-all
build-all: clean
	mkdir -p $(DIST_DIR)
	
	# Linux AMD64
	GOOS=linux GOARCH=amd64 $(GOBUILD) $(LDFLAGS) -o $(DIST_DIR)/$(BINARY_NAME)-linux-amd64 $(CMD_DIR)/pi
	GOOS=linux GOARCH=amd64 $(GOBUILD) $(LDFLAGS) -o $(DIST_DIR)/$(TUI_BINARY_NAME)-linux-amd64 $(CMD_DIR)/pi-tui
	
	# Linux ARM64
	GOOS=linux GOARCH=arm64 $(GOBUILD) $(LDFLAGS) -o $(DIST_DIR)/$(BINARY_NAME)-linux-arm64 $(CMD_DIR)/pi
	GOOS=linux GOARCH=arm64 $(GOBUILD) $(LDFLAGS) -o $(DIST_DIR)/$(TUI_BINARY_NAME)-linux-arm64 $(CMD_DIR)/pi-tui
	
	# macOS AMD64
	GOOS=darwin GOARCH=amd64 $(GOBUILD) $(LDFLAGS) -o $(DIST_DIR)/$(BINARY_NAME)-darwin-amd64 $(CMD_DIR)/pi
	GOOS=darwin GOARCH=amd64 $(GOBUILD) $(LDFLAGS) -o $(DIST_DIR)/$(TUI_BINARY_NAME)-darwin-amd64 $(CMD_DIR)/pi-tui
	
	# macOS ARM64
	GOOS=darwin GOARCH=arm64 $(GOBUILD) $(LDFLAGS) -o $(DIST_DIR)/$(BINARY_NAME)-darwin-arm64 $(CMD_DIR)/pi
	GOOS=darwin GOARCH=arm64 $(GOBUILD) $(LDFLAGS) -o $(DIST_DIR)/$(TUI_BINARY_NAME)-darwin-arm64 $(CMD_DIR)/pi-tui
	
	# Windows AMD64
	GOOS=windows GOARCH=amd64 $(GOBUILD) $(LDFLAGS) -o $(DIST_DIR)/$(BINARY_NAME)-windows-amd64.exe $(CMD_DIR)/pi
	GOOS=windows GOARCH=amd64 $(GOBUILD) $(LDFLAGS) -o $(DIST_DIR)/$(TUI_BINARY_NAME)-windows-amd64.exe $(CMD_DIR)/pi-tui

# Install binaries to GOPATH/bin
.PHONY: install
install: build
	cp $(BUILD_DIR)/$(BINARY_NAME) $(GOPATH)/bin/
	cp $(BUILD_DIR)/$(TUI_BINARY_NAME) $(GOPATH)/bin/

# Clean build artifacts
.PHONY: clean
clean:
	$(GOCLEAN)
	rm -rf $(BUILD_DIR)
	rm -rf $(DIST_DIR)
	rm -f coverage.out coverage.html

# Run CLI in development mode
.PHONY: run
run: build-cli
	./$(BUILD_DIR)/$(BINARY_NAME)

# Run TUI in development mode
.PHONY: run-tui
run-tui: build-tui
	./$(BUILD_DIR)/$(TUI_BINARY_NAME)

# Generate documentation
.PHONY: docs
docs:
	@echo "Generating documentation..."
	@if [ -f $(BUILD_DIR)/$(BINARY_NAME) ]; then \
		$(BUILD_DIR)/$(BINARY_NAME) help > docs/CLI.md; \
		echo "CLI documentation generated in docs/CLI.md"; \
	else \
		echo "Build CLI first with 'make build-cli'"; \
	fi

# Development setup
.PHONY: dev-setup
dev-setup:
	@echo "Setting up development environment..."
	$(GOGET) -u github.com/golangci/golangci-lint/cmd/golangci-lint
	$(GOMOD) download
	@echo "Development environment ready!"

# Quick development cycle
.PHONY: dev
dev: fmt test build

# Release preparation
.PHONY: release
release: clean deps fmt lint test build-all
	@echo "Release artifacts built in $(DIST_DIR)/"

# Help target
.PHONY: help
help:
	@echo "Available targets:"
	@echo "  all          - Run clean, deps, fmt, lint, test, build"
	@echo "  deps         - Download and tidy Go dependencies"
	@echo "  fmt          - Format Go code"
	@echo "  lint         - Run linter (requires golangci-lint)"
	@echo "  test         - Run tests"
	@echo "  test-coverage- Run tests with coverage report"
	@echo "  build        - Build both CLI and TUI binaries"
	@echo "  build-cli    - Build CLI binary only"
	@echo "  build-tui    - Build TUI binary only"
	@echo "  build-all    - Build for multiple platforms"
	@echo "  install      - Install binaries to GOPATH/bin"
	@echo "  clean        - Clean build artifacts"
	@echo "  run          - Run CLI in development mode"
	@echo "  run-tui      - Run TUI in development mode"
	@echo "  docs         - Generate documentation"
	@echo "  dev-setup    - Set up development environment"
	@echo "  dev          - Quick development cycle (fmt, test, build)"
	@echo "  release      - Prepare release artifacts"
	@echo "  help         - Show this help message"

