---
Title: Configuration
Slug: configuration
Short: YAML-driven rules, models, streaming, and tools configuration.
Topics:
- config
- rules
IsTopLevel: true
ShowPerDefault: true
SectionType: GeneralTopic
---

# Configuration

Control models, streaming, and rule-based responses with a single YAML file.

## Location
- Default: `config/bot.yaml`
- Override: `MOCK_SERVER_CONFIG=/path/to/bot.yaml`

## Schema
- `server`: `{ port, cors }`
- `models`: list of `{ id, owned_by }`
- `streaming`: `{ enabled, chunk_delay_ms }`
- `variables`: key/value for templates
- `tools`: `{ enabled: [...], registry: { name: { call_type, status, message }}}`
- `rules`: ordered; first match wins (unless `continue: true`)
  - `match`: `{ endpoint: chat|responses, model, role, contains, regex }`
  - `respond`: `text` or `choose`, `use_tools`, optional `message` (Responses)
  - `stream_override`: `{ chunk_delay_ms }`
- `fallback.respond`: default reply

Template vars: `{{input_text}}`, `{{last_user_message}}`, `{{model}}`, `{{timestamp}}`.

## Examples
Chat tools
```yaml
rules:
  - match: { endpoint: chat, contains: ["search", "latest"] }
    respond: { use_tools: [web_search], text: "Summary above." }
```

Responses tools
```yaml
rules:
  - match: { endpoint: responses, contains: ["news", "AI"] }
    respond: { use_tools: [web_search], message: { text: "Headlines with citations." } }
```

