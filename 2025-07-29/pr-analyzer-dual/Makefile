# PR Analyzer Makefile

.PHONY: build test clean install deps lint fmt vet

# Build the application
build:
	go build -o pr-analyzer .

# Install dependencies
deps:
	go mod tidy
	go mod download

# Run tests
test:
	go test ./...

# Run tests with coverage
test-coverage:
	go test -coverprofile=coverage.out ./...
	go tool cover -html=coverage.out -o coverage.html

# Format code
fmt:
	go fmt ./...

# Vet code
vet:
	go vet ./...

# Lint code (requires golangci-lint)
lint:
	golangci-lint run

# Clean build artifacts
clean:
	rm -f pr-analyzer
	rm -f coverage.out coverage.html

# Install the binary to GOPATH/bin
install: build
	cp pr-analyzer $(GOPATH)/bin/

# Run all checks
check: fmt vet lint test

# Build for multiple platforms
build-all:
	GOOS=linux GOARCH=amd64 go build -o dist/pr-analyzer-linux-amd64 .
	GOOS=darwin GOARCH=amd64 go build -o dist/pr-analyzer-darwin-amd64 .
	GOOS=darwin GOARCH=arm64 go build -o dist/pr-analyzer-darwin-arm64 .
	GOOS=windows GOARCH=amd64 go build -o dist/pr-analyzer-windows-amd64.exe .

# Create release package
package: build-all
	mkdir -p dist/package
	cp README.md EXAMPLES.md TEST_RESULTS.md .envrc.example dist/package/
	cd dist && tar -czf pr-analyzer.tar.gz package/ pr-analyzer-*

# Development setup
dev-setup:
	go install github.com/golangci/golangci-lint/cmd/golangci-lint@latest
	go mod tidy

# Quick test with real data
test-real:
	./pr-analyzer get commits --owner go-go-golems --repo geppetto --pr-number 181 --fields sha,message,author_name
	./pr-analyzer analyze functions --owner go-go-golems --repo geppetto --pr-number 181 --only-changed --fields function_name,is_changed

# Help
help:
	@echo "Available targets:"
	@echo "  build        - Build the application"
	@echo "  deps         - Install dependencies"
	@echo "  test         - Run tests"
	@echo "  test-coverage- Run tests with coverage"
	@echo "  fmt          - Format code"
	@echo "  vet          - Vet code"
	@echo "  lint         - Lint code"
	@echo "  clean        - Clean build artifacts"
	@echo "  install      - Install binary to GOPATH/bin"
	@echo "  check        - Run all checks (fmt, vet, lint, test)"
	@echo "  build-all    - Build for multiple platforms"
	@echo "  package      - Create release package"
	@echo "  dev-setup    - Set up development environment"
	@echo "  test-real    - Quick test with real GitHub data"
	@echo "  help         - Show this help"

