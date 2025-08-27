# Git Pre-commit Guard Makefile

# Variables
BINARY_NAME=git-precommit-guard
VERSION=$(shell git describe --tags --always --dirty 2>/dev/null || echo "dev")
BUILD_TIME=$(shell date -u '+%Y-%m-%d_%H:%M:%S')
LDFLAGS=-ldflags "-X main.Version=$(VERSION) -X main.BuildTime=$(BUILD_TIME)"

# Default target
.PHONY: all
all: build

# Build the binary
.PHONY: build
build:
	go build $(LDFLAGS) -o $(BINARY_NAME) .

# Build for multiple platforms
.PHONY: build-all
build-all:
	GOOS=linux GOARCH=amd64 go build $(LDFLAGS) -o $(BINARY_NAME)-linux-amd64 .
	GOOS=linux GOARCH=arm64 go build $(LDFLAGS) -o $(BINARY_NAME)-linux-arm64 .
	GOOS=darwin GOARCH=amd64 go build $(LDFLAGS) -o $(BINARY_NAME)-darwin-amd64 .
	GOOS=darwin GOARCH=arm64 go build $(LDFLAGS) -o $(BINARY_NAME)-darwin-arm64 .
	GOOS=windows GOARCH=amd64 go build $(LDFLAGS) -o $(BINARY_NAME)-windows-amd64.exe .

# Install to system PATH
.PHONY: install
install: build
	sudo cp $(BINARY_NAME) /usr/local/bin/

# Clean build artifacts
.PHONY: clean
clean:
	rm -f $(BINARY_NAME)
	rm -f $(BINARY_NAME)-*

# Run tests
.PHONY: test
test:
	go test ./...

# Run tests with coverage
.PHONY: test-coverage
test-coverage:
	go test -coverprofile=coverage.out ./...
	go tool cover -html=coverage.out -o coverage.html

# Format code
.PHONY: fmt
fmt:
	go fmt ./...

# Lint code
.PHONY: lint
lint:
	golangci-lint run

# Tidy dependencies
.PHONY: tidy
tidy:
	go mod tidy

# Validate configuration
.PHONY: validate
validate: build
	./$(BINARY_NAME) validate-config

# Create test files for demonstration
.PHONY: demo-files
demo-files:
	mkdir -p demo
	echo "This is a text file" > demo/sample.txt
	echo "console.log('hello');" > demo/script.js
	dd if=/dev/zero of=demo/large_file.bin bs=1M count=15 2>/dev/null
	cp /bin/ls demo/elf_binary 2>/dev/null || echo "#!/bin/bash\necho hello" > demo/elf_binary

# Run demo
.PHONY: demo
demo: build demo-files
	@echo "=== Testing with sample files ==="
	./$(BINARY_NAME) check --all demo/sample.txt demo/script.js demo/large_file.bin demo/elf_binary || true
	@echo "\n=== JSON output ==="
	./$(BINARY_NAME) check --all --json demo/elf_binary || true

# Package for distribution
.PHONY: package
package: build-all
	mkdir -p dist
	tar -czf dist/$(BINARY_NAME)-linux-amd64.tar.gz $(BINARY_NAME)-linux-amd64 README.md LICENSE .precommit-guard.yml.example
	tar -czf dist/$(BINARY_NAME)-linux-arm64.tar.gz $(BINARY_NAME)-linux-arm64 README.md LICENSE .precommit-guard.yml.example
	tar -czf dist/$(BINARY_NAME)-darwin-amd64.tar.gz $(BINARY_NAME)-darwin-amd64 README.md LICENSE .precommit-guard.yml.example
	tar -czf dist/$(BINARY_NAME)-darwin-arm64.tar.gz $(BINARY_NAME)-darwin-arm64 README.md LICENSE .precommit-guard.yml.example
	zip -j dist/$(BINARY_NAME)-windows-amd64.zip $(BINARY_NAME)-windows-amd64.exe README.md LICENSE .precommit-guard.yml.example

# Help
.PHONY: help
help:
	@echo "Available targets:"
	@echo "  build        - Build the binary"
	@echo "  build-all    - Build for all platforms"
	@echo "  install      - Install to /usr/local/bin"
	@echo "  clean        - Remove build artifacts"
	@echo "  test         - Run tests"
	@echo "  test-coverage- Run tests with coverage"
	@echo "  fmt          - Format code"
	@echo "  lint         - Lint code"
	@echo "  tidy         - Tidy dependencies"
	@echo "  validate     - Validate configuration"
	@echo "  demo         - Run demonstration"
	@echo "  package      - Create distribution packages"
	@echo "  help         - Show this help"

