---
Title: API Reference — Responses API
Slug: api-responses
Short: Create, stream, list, and retrieve responses with optional tools.
Topics:
- api
- responses
IsTopLevel: true
ShowPerDefault: true
SectionType: GeneralTopic
---

# API Reference — Responses API

## Create
Endpoint: `POST /v1/responses`
```json
{
  "model": "gpt-4o",
  "input": "Tell me a joke",
  "tools": [{"type": "web_search"}],
  "stream": false
}
```

### Response (non-streaming)
- `output` includes tool call objects (e.g., `web_search_call`) followed by a `message` with `content[0].text` and optional `annotations`.

## Streaming
- Set `stream: true`. SSE events with `type: response.output_text.delta`, then `response.done`.

## Retrieve
`GET /v1/responses/{response_id}` returns a stored response object.

## List
`GET /v1/responses?limit=20` returns a list of recent responses.

## Tool support
- Prefer YAML `respond.use_tools: [name]` to emit configured tools and merge default tool messages.
- You can also pass `tools` in the request (legacy behavior) to trigger built-in mock tool flows.

