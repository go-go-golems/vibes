---
Title: API Reference — Chat Completions
Slug: api-chat-completions
Short: POST /v1/chat/completions request/response format, streaming, and tooling.
Topics:
- api
- chat
IsTopLevel: true
ShowPerDefault: true
SectionType: GeneralTopic
---

# API Reference — Chat Completions

Endpoint: `POST /v1/chat/completions`

## Request
```json
{
  "model": "gpt-3.5-turbo",
  "messages": [
    {"role": "user", "content": "Hello!"}
  ],
  "stream": false
}
```

## Response (non-streaming)
```json
{
  "id": "chatcmpl-...",
  "object": "chat.completion",
  "created": 1710000000,
  "model": "gpt-3.5-turbo",
  "choices": [{
    "index": 0,
    "message": {"role": "assistant", "content": "..."},
    "finish_reason": "stop"
  }],
  "usage": {"prompt_tokens": 5, "completion_tokens": 10, "total_tokens": 15}
}
```

## Streaming
- Set `stream: true`. Server sends SSE `data: {chunk}` entries ending with `data: [DONE]`.
- First chunk sets `delta.role`, subsequent chunks set `delta.content`.

## Tool support
- Rules with `endpoint: chat` and `respond.use_tools: [name]` prepend tool default messages to the assistant text (also streamed).
- Configure tools in YAML (`tools.registry`). See `/help/configuration`.

