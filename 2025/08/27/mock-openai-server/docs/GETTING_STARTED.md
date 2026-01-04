# Getting Started

This guide helps you run the Mock OpenAI Server quickly and customize it when ready.

## Prerequisites
- Go 1.21+ installed (`go version`)
- Python 3.7+ for test scripts

## Run the server
- With Makefile:
  - `make run` — starts the server using `config/bot.yaml` (override with `make run MOCK_SERVER_CONFIG=path.yaml`).
- Without Makefile:
  - `go run . serve` — uses the built‑in default configuration when no YAML file is found.

The server listens on `http://localhost:3117` by default.

## Quick tests
- Chat completions (Python SDK): `python3 test_mock_server.py`
- Responses API suite: `python3 test_responses_api.py`
- Streaming demo: `python3 streaming_test.py`

## Configuration (optional)
Use a YAML file to control models, streaming delay, and rule‑based responses for both Chat Completions and the Responses API.
- Default path: `config/bot.yaml`
- Override with env var: `MOCK_SERVER_CONFIG=/path/to/bot.yaml`
- Full schema and examples: see `docs/CONFIGURATION.md`

## Default configuration (used when no YAML is provided)
```yaml
version: 1
server: { port: 3117, cors: "*" }
models:
  - { id: gpt-4o, owned_by: openai }
  - { id: gpt-4o-mini, owned_by: openai }
  - { id: gpt-3.5-turbo, owned_by: openai }
streaming: { enabled: true, chunk_delay_ms: 120 }
variables: { bot_name: "Mock OpenAI" }
tools:
  enabled: [web_search, file_search]
  registry:
    custom_demo:
      call_type: custom_demo_call
      status: completed
      message: { text: "Custom tool ran with input: '{{input_text}}'" }
rules:
  - match: { endpoint: chat, contains: ["hello", "hi"] }
    respond: { text: "Hello! I'm {{bot_name}}. How can I help?" }
  - match: { endpoint: chat, contains: ["joke"] }
    respond:
      choose:
        - { weight: 1, text: "Why don't scientists trust atoms? They make up everything!" }
        - { weight: 1, text: "What do you call a fake noodle? An impasta!" }
  - match: { endpoint: chat, contains: ["search", "latest"] }
    respond:
      use_tools: [web_search]
      text: "Summary above. Let me know if you want more details."
  - match: { endpoint: responses, contains: ["news", "latest", "AI"] }
    respond:
      use_tools: [web_search]
      message: { text: "Here are the latest AI headlines with citations." }
  - match: { endpoint: responses, contains: ["custom tool"] }
    respond: { use_tools: [custom_demo] }
fallback:
  respond:
    text: "This is a mock response to: '{{last_user_message}}'."
```

Start with the defaults, then copy `config/bot.yaml` and tailor rules, tools, and models to your needs.
