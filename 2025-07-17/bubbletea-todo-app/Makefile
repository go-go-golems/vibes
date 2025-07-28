# Bubbletea Todo App Makefile

.PHONY: all build clean tui web install deps test help

# Default target
all: build

# Build both applications
build: tui-app web-app

# Build TUI application
tui-app:
	@echo "Building TUI application..."
	go build -o tui-app ./cmd/tui

# Build web application
web-app:
	@echo "Building web application..."
	go build -o web-app ./cmd/web

# Install dependencies
deps:
	@echo "Installing dependencies..."
	go mod download
	go mod tidy

# Run TUI application
tui: tui-app
	@echo "Starting TUI application..."
	./tui-app

# Run web application
web: web-app
	@echo "Starting web server on http://localhost:8080"
	./web-app

# Clean build artifacts
clean:
	@echo "Cleaning build artifacts..."
	rm -f tui-app web-app

# Install applications to system
install: build
	@echo "Installing applications to /usr/local/bin..."
	sudo cp tui-app /usr/local/bin/bubbletea-todo-tui
	sudo cp web-app /usr/local/bin/bubbletea-todo-web
	@echo "Applications installed successfully!"
	@echo "Run 'bubbletea-todo-tui' for TUI or 'bubbletea-todo-web' for web server"

# Run tests (placeholder for future test implementation)
test:
	@echo "Running tests..."
	go test ./...

# Development mode - rebuild and run web app on changes
dev: web-app
	@echo "Starting development server..."
	@echo "Open http://localhost:8080 in your browser"
	./web-app

# Show help
help:
	@echo "Bubbletea Todo App - Available commands:"
	@echo ""
	@echo "  make build    - Build both TUI and web applications"
	@echo "  make tui-app  - Build only the TUI application"
	@echo "  make web-app  - Build only the web application"
	@echo "  make tui      - Build and run TUI application"
	@echo "  make web      - Build and run web application"
	@echo "  make deps     - Install Go dependencies"
	@echo "  make clean    - Remove build artifacts"
	@echo "  make install  - Install applications to system"
	@echo "  make test     - Run tests"
	@echo "  make dev      - Start development server"
	@echo "  make help     - Show this help message"
	@echo ""
	@echo "Examples:"
	@echo "  make tui      # Run the terminal interface"
	@echo "  make web      # Start web server on port 8080"
	@echo "  make clean    # Clean up build files"

