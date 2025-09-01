# Repository Guidelines

## Project Structure & Module Organization
- `main.go`: HTTP server, routing, health, models, chat completions.
- `responses_api.go`: Responses API handlers, streaming, in‑memory store.
- `go.mod`, `go.sum`: Go module config.
- Python clients/tests: `test_mock_server.py`, `test_responses_api.py`, `streaming_test.py`, plus small demos under repo root.
- Docs: `README.md`, `RESPONSES_API_README.md`, `STREAMING_DEMO_README.md`.

## Build, Test, and Development Commands
- Run server (dev): `go run .`
- Build binary: `go build -o mock-openai-server .` then `./mock-openai-server serve`
- Quick health check: `curl http://localhost:3117/health`
- Python smoke tests (server running):
  - Basic SDK tests: `python3 test_mock_server.py`
  - Responses API suite: `python3 test_responses_api.py`
  - Streaming demo: `python3 streaming_test.py`

## Coding Style & Naming Conventions
- Go formatting: run `gofmt -w .` or `go fmt ./...` before pushing.
- Linting: prefer `go vet ./...` for static checks.
- Go naming: types/interfaces `CamelCase`; vars/funcs `lowerCamel`; HTTP handlers prefixed `handle…`; route setup as `setup…Routes`.
- Python demos/tests: follow PEP 8; file names `test_*.py` or `*_test.py`.

## Testing Guidelines
- Start the server locally, then run Python test scripts above.
- Tests cover: chat completions, Responses API (create/list/retrieve), streaming (SSE), tools (web_search/file_search), health/models.
- New tests: add `test_*.py` alongside existing scripts; keep tests independent and idempotent (no external network assumptions).

## Commit & Pull Request Guidelines
- Commits: clear, scoped messages. If unsure, follow Conventional Commits (e.g., `feat: add responses streaming`, `fix: correct models list`).
- PRs: include purpose summary, linked issues, how to run/reproduce, and screenshots or sample requests if UI/CLI behavior changes.
- Keep diffs focused; update relevant docs (`README.md`, this file) when adding endpoints or flags.

## Security & Configuration Tips
- Server runs on `:3117` by default (see `main.go`). Do not expose publicly; CORS is wide‑open for dev convenience and there is no auth.
- The API is mock; avoid sending sensitive data.
- When adding endpoints, ensure CORS headers and JSON error shapes match existing handlers.
