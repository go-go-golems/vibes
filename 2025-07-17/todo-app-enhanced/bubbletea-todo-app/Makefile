# Bubbletea Todo App Makefile

.PHONY: all build clean tui web install deps test help enhanced

# Default port for development
PORT ?= 8080

# Default target
all: build

# Build both applications
build: tui-app web-app

# Build enhanced applications
build-enhanced: tui-app-enhanced web-app-enhanced

# Build all applications (original + enhanced)
build-all: build build-enhanced

# Build TUI application
tui-app:
	@echo "Building TUI application..."
	go build -o tui-app ./cmd/tui

# Build web application
web-app:
	@echo "Building web application..."
	go build -o web-app ./cmd/web

# Build enhanced TUI application
tui-app-enhanced:
	@echo "Building enhanced TUI application..."
	go build -o tui-app-enhanced ./cmd/tui-enhanced

# Build enhanced web application
web-app-enhanced:
	@echo "Building enhanced web application..."
	go build -o web-app-enhanced ./cmd/web-enhanced

# Install dependencies
deps:
	@echo "Installing dependencies..."
	go mod download
	go mod tidy

# Run TUI application
tui: tui-app
	@echo "Starting TUI application..."
	./tui-app

# Run enhanced TUI application
tui-enhanced: tui-app-enhanced
	@echo "Starting enhanced TUI application..."
	./tui-app-enhanced

# Run web application
web: web-app
	@echo "Starting web server on port $(PORT)..."
	./web-app --port $(PORT)

# Run enhanced web application
web-enhanced: web-app-enhanced
	@echo "Starting enhanced web server on port $(PORT)..."
	./web-app-enhanced --port $(PORT)

# Run web application with custom port
web-port: web-app
	@echo "Starting web server on custom port..."
	@echo "Usage: make web-port PORT=3000"
	./web-app --port $(PORT)

# Run enhanced web application with custom port
web-enhanced-port: web-app-enhanced
	@echo "Starting enhanced web server on custom port..."
	@echo "Usage: make web-enhanced-port PORT=3000"
	./web-app-enhanced --port $(PORT)

# Clean build artifacts
clean:
	@echo "Cleaning build artifacts..."
	rm -f tui-app web-app tui-app-enhanced web-app-enhanced

# Install applications to system
install: build-all
	@echo "Installing applications to /usr/local/bin..."
	sudo cp tui-app /usr/local/bin/bubbletea-todo-tui
	sudo cp web-app /usr/local/bin/bubbletea-todo-web
	sudo cp tui-app-enhanced /usr/local/bin/bubbletea-todo-tui-enhanced
	sudo cp web-app-enhanced /usr/local/bin/bubbletea-todo-web-enhanced
	@echo "Applications installed successfully!"
	@echo "Run 'bubbletea-todo-tui' for TUI or 'bubbletea-todo-web' for web server"
	@echo "Run 'bubbletea-todo-tui-enhanced' for enhanced TUI or 'bubbletea-todo-web-enhanced' for enhanced web server"

# Run tests (placeholder for future test implementation)
test:
	@echo "Running tests..."
	go test ./...

# Development mode - rebuild and run enhanced web app on changes
dev: web-app-enhanced
	@echo "Starting development server on port $(PORT)..."
	@echo "Open http://localhost:$(PORT) in your browser"
	./web-app-enhanced --port $(PORT)

# Development mode with custom port
dev-port: web-app-enhanced
	@echo "Starting development server on custom port..."
	@echo "Usage: make dev-port PORT=3000"
	@echo "Open http://localhost:$(PORT) in your browser"
	./web-app-enhanced --port $(PORT)

# Show help
help:
	@echo "Bubbletea Todo App - Available commands:"
	@echo ""
	@echo "Building:"
	@echo "  make build           - Build original TUI and web applications"
	@echo "  make build-enhanced  - Build enhanced TUI and web applications"
	@echo "  make build-all       - Build all applications (original + enhanced)"
	@echo "  make tui-app         - Build only the TUI application"
	@echo "  make web-app         - Build only the web application"
	@echo "  make tui-app-enhanced - Build only the enhanced TUI application"
	@echo "  make web-app-enhanced - Build only the enhanced web application"
	@echo ""
	@echo "Running:"
	@echo "  make tui             - Build and run TUI application"
	@echo "  make tui-enhanced    - Build and run enhanced TUI application"
	@echo "  make web             - Build and run web application (port $(PORT))"
	@echo "  make web-enhanced    - Build and run enhanced web application (port $(PORT))"
	@echo "  make web-port        - Run web app with custom port"
	@echo "  make web-enhanced-port - Run enhanced web app with custom port"
	@echo ""
	@echo "Development:"
	@echo "  make dev             - Start enhanced development server (port $(PORT))"
	@echo "  make dev-port        - Start development server with custom port"
	@echo "  make deps            - Install Go dependencies"
	@echo "  make test            - Run tests"
	@echo ""
	@echo "Maintenance:"
	@echo "  make clean           - Remove build artifacts"
	@echo "  make install         - Install applications to system"
	@echo "  make help            - Show this help message"
	@echo ""
	@echo "Port Configuration:"
	@echo "  PORT=3000 make web   - Set port via environment variable"
	@echo "  make web PORT=3000   - Set port via make variable"
	@echo "  ./web-app --port 3000 - Set port via command line flag"
	@echo "  ./web-app -p 3000    - Set port via short flag"
	@echo ""
	@echo "Examples:"
	@echo "  make tui-enhanced    # Run enhanced terminal interface"
	@echo "  make web-enhanced    # Start enhanced web server on port 8080"
	@echo "  make dev PORT=3000   # Start development server on port 3000"
	@echo "  make clean           # Clean up build files"

