---
Title: Getting Started
Slug: getting-started
Short: Build the binary, create a run, add a turn, and inspect data
SectionType: Tutorial
IsTopLevel: true
ShowPerDefault: true
---

## Build

```bash
go build -o turn-inspector
```

## Create a run

```bash
./turn-inspector create run --name "Demo Run" --metadata '{"source":"session","key":"id","value":"abc123"}'
```

## Add a turn to the run

```bash
./turn-inspector create turn --run-id 1 --metadata '{"source":"user","key":"tier","value":"premium"}' --blocks '[
  {"order":0, "kind":"user", "role":"user", "payload": {"text": "Hello"}},
  {"order":1, "kind":"llm_text", "role":"assistant", "payload": {"text": "Hi there!"}}
]'
```

## List and show

```bash
./turn-inspector run list
./turn-inspector list turns --run-id 1
./turn-inspector show turn --id 1
```
