.PHONY: build test clean install run-test

# Build the main application
build:
	go build -o bin/goat-manager ./cmd/goat-manager

# Build and run tests
test:
	go test ./...

# Run the database test
run-test:
	go run test_main.go

# Clean build artifacts
clean:
	rm -rf bin/

# Install dependencies
deps:
	go mod tidy
	go mod download

# Generate Ent code
generate:
	go run entgo.io/ent/cmd/ent generate ./ent/schema

# Initialize database
init-db:
	go run test_main.go

# Run the application
run: build
	./bin/goat-manager

# Install the application
install: build
	cp bin/goat-manager /usr/local/bin/

