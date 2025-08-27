---
Title: Getting Started
Slug: getting-started
Short: Quick setup to run and test the Mock OpenAI Server.
Topics:
- setup
- quick-start
IsTopLevel: true
ShowPerDefault: true
SectionType: Tutorial
---

# Getting Started

This server mocks OpenAI-compatible endpoints so you can develop and test locally.

## Prerequisites
- Go 1.21+ installed (`go version`)
- Python 3.7+ for test scripts

## Run
- Makefile: `make run` (uses `config/bot.yaml` if present, else built-in defaults)
- Plain Go: `go run . serve`

Server listens on `http://localhost:3117` by default.

## Test
- Chat completions: `python3 test_mock_server.py`
- Responses API: `python3 test_responses_api.py`
- Streaming demo: `python3 streaming_test.py`

## Next Steps
- Explore configuration via `/help/configuration`
- View API references: `/help/api-chat-completions`, `/help/api-responses`

