# Code Review Tool Makefile
# Builds Go CLI and React frontend, bundles them together

.PHONY: all build build-go build-frontend clean install dev serve test help

# Default target
all: build

# Build everything
build: build-go build-frontend

# Build the Go CLI
build-go:
	@echo "Building Go CLI..."
	@export PATH=/usr/local/go/bin:$$PATH && \
	cd . && \
	go mod tidy && \
	go build -o codereview .
	@echo "✅ Go CLI built successfully"

# Build the React frontend
build-frontend:
	@echo "Building React frontend..."
	@cd ../codereview-frontend && \
	pnpm install && \
	pnpm run build
	@echo "✅ React frontend built successfully"

# Bundle frontend into Go binary (embed dist files)
bundle: build-frontend
	@echo "Bundling frontend assets..."
	@rm -rf cmd/web/dist
	@mkdir -p cmd/web
	@cp -r ../codereview-frontend/dist cmd/web/
	@echo "✅ Frontend assets bundled to cmd/web/dist"

# Clean build artifacts
clean:
	@echo "Cleaning build artifacts..."
	@rm -f codereview
	@rm -rf cmd/web/dist web/dist
	@cd ../codereview-frontend && rm -rf dist node_modules
	@echo "✅ Clean completed"

# Install dependencies
install:
	@echo "Installing dependencies..."
	@export PATH=/usr/local/go/bin:$$PATH && go mod download
	@cd ../codereview-frontend && pnpm install
	@echo "✅ Dependencies installed"

# Development mode - build and run
dev: build bundle
	@echo "Starting development server..."
	@./codereview serve --port 8080 --dev

# Production build and serve
serve: build bundle
	@echo "Starting production server..."
	@./codereview serve --port 8080

# Run tests
test:
	@echo "Running Go tests..."
	@export PATH=/usr/local/go/bin:$$PATH && go test ./...
	@echo "Running frontend tests..."
	@cd ../codereview-frontend && pnpm test --run
	@echo "✅ All tests passed"

# Create distribution package
dist: build bundle
	@echo "Creating distribution package..."
	@mkdir -p dist
	@cp codereview dist/
	@cp -r web dist/
	@cp README.md dist/ 2>/dev/null || echo "README.md not found, skipping"
	@tar -czf codereview-$(shell date +%Y%m%d-%H%M%S).tar.gz -C dist .
	@echo "✅ Distribution package created"

# Install globally
install-global: build bundle
	@echo "Installing globally..."
	@sudo cp codereview /usr/local/bin/
	@echo "✅ Installed to /usr/local/bin/codereview"

# Show help
help:
	@echo "Code Review Tool - Makefile Commands"
	@echo ""
	@echo "Available targets:"
	@echo "  all            - Build everything (default)"
	@echo "  build          - Build Go CLI and React frontend"
	@echo "  build-go       - Build only the Go CLI"
	@echo "  build-frontend - Build only the React frontend"
	@echo "  bundle         - Bundle frontend assets into Go binary"
	@echo "  clean          - Clean all build artifacts"
	@echo "  install        - Install dependencies"
	@echo "  dev            - Build and run in development mode"
	@echo "  serve          - Build and run in production mode"
	@echo "  test           - Run all tests"
	@echo "  dist           - Create distribution package"
	@echo "  install-global - Install binary globally"
	@echo "  help           - Show this help message"
	@echo ""
	@echo "Examples:"
	@echo "  make build     - Build the complete application"
	@echo "  make dev       - Start development server on port 8080"
	@echo "  make clean     - Clean all build artifacts"
