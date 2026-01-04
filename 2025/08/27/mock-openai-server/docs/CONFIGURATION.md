# YAML Configuration

This server can be customized with a single YAML file to control models, streaming behavior, and how requests are matched to responses for both Chat Completions and the Responses API.

## Location & Startup
- Default path: `config/bot.yaml`
- Override with env var: `MOCK_SERVER_CONFIG=/path/to/bot.yaml`
- Port and CORS can be set in YAML; server logs the loaded config on start.

## Schema Overview
- `version`: Integer config version.
- `server`: `{ port: 3117, cors: "*" }`
- `models`: List of `{ id, owned_by }` exposed by `/v1/models`.
- `streaming`: `{ enabled: true, chunk_delay_ms: 120 }` (affects SSE token pacing).
- `variables`: Key/values available in templates (e.g., `bot_name`).
- `tools`: Configure available tools and which are enabled.
  - `enabled`: list of tool names allowed to be used.
  - `registry`: map of tool definitions by name:
    - `call_type`: emitted call object type (e.g., `web_search_call`).
    - `status`: call status (e.g., `completed`).
    - `message`: optional default message appended after the call with `text` and `annotations`.
- `rules`: Ordered list; first match wins unless `continue: true`.
  - `match`: `{ endpoint: chat|responses, model: string|[...], role, contains: [...], regex }`
  - `respond`: one of:
    - `text` or `choose: [{ weight, text }]`
    - `use_tools: [name, ...]` to emit configured tools, or `tools: [{ type, status }]` for explicit calls.
    - `message: { text, annotations: [...] }` (Responses API)
    - `error: { status, code, message }` (inject HTTP errors)
  - `stream_override`: `{ chunk_delay_ms }` per‑rule
- `fallback.respond`: Used when no rule matches.

Template variables: `{{input_text}}`, `{{last_user_message}}`, `{{model}}`, `{{timestamp}}`, plus any `variables` you define.

## Examples
Minimal config
```yaml
version: 1
models: [ { id: gpt-4o, owned_by: openai } ]
fallback:
  respond:
    text: "Mock reply to: '{{last_user_message}}'"
```

Greeting + jokes for Chat
```yaml
rules:
  - match: { endpoint: chat, contains: ["hello", "hi"] }
    respond: { text: "Hello! I'm {{bot_name}}." }
  - match: { endpoint: chat, contains: ["joke"] }
    respond:
      choose:
        - { weight: 1, text: "Why don't scientists trust atoms? They make up everything!" }
        - { weight: 1, text: "An impasta!" }
```

Use tools in Chat (aggregates tool default message into reply)
```yaml
rules:
  - match: { endpoint: chat, contains: ["search", "latest"] }
    respond:
      use_tools: [web_search]
      text: "Summary above."
```

Responses API with tools and citations
```yaml
rules:
  - match: { endpoint: responses, contains: ["news", "latest", "AI"] }
    respond:
      use_tools: [web_search]
      message: { text: "Latest AI headlines with citations." }
```

Custom tool definition
```yaml
tools:
  enabled: [web_search, custom_demo]
  registry:
    custom_demo:
      call_type: custom_demo_call
      status: completed
      message:
        text: "Custom tool ran with input: '{{input_text}}'"
```

## Notes
- Streaming: rule or global `chunk_delay_ms` changes token pacing. Tools are emitted only in non‑streaming responses.
- Errors: `respond.error` returns an OpenAI‑style error JSON with the given HTTP status.
- Backwards‑compatible: without a config file, the server behaves as before.
