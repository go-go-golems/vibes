APP=mock-openai-server
MOCK_SERVER_CONFIG ?= config/bot.yaml

.PHONY: help fmt vet build run test test-chat test-responses test-stream clean docs

help:
	@echo "Common targets:"
	@echo "  make fmt           - go fmt ./..."
	@echo "  make vet           - go vet ./..."
	@echo "  make build         - build $(APP)"
	@echo "  make run           - run server (uses config/bot.yaml)"
	@echo "  make docs          - print all embedded docs (help --all)"
	@echo "  make test          - run all Python tests (server must be running)"
	@echo "  make test-chat     - run chat SDK tests"
	@echo "  make test-responses- run Responses API suite"
	@echo "  make test-stream   - run streaming tests"
	@echo "  make clean         - remove binary"

docs:
	go run . help --all

fmt:
	go fmt ./...

vet:
	go vet ./...

build:
	go build -o $(APP) .

run:
	@echo "Starting server with $(MOCK_SERVER_CONFIG)"
	MOCK_SERVER_CONFIG=$(MOCK_SERVER_CONFIG) go run . serve

test: test-chat test-responses test-stream

test-chat:
	python3 test_mock_server.py || true

test-responses:
	python3 test_responses_api.py || true

test-stream:
	python3 streaming_test.py || true

clean:
	rm -f $(APP)
