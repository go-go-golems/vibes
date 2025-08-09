---
Title: Turn Inspector Overview
Slug: overview
Short: Concepts and tasks for runs, turns, blocks, and metadata
SectionType: GeneralTopic
IsTopLevel: true
ShowPerDefault: true
---

Turn Inspector stores conversations as runs and turns with rich metadata.

## Concepts

- Run: A top-level container for a session or experiment; holds turns and run-level metadata.
- Turn: A single back-and-forth unit; contains ordered blocks and turn-level metadata.
- Block: A piece of content (user/system/llm/tool) with a JSON payload.
- Metadata: Source / key / value triplets on runs, turns, and blocks.

## Common tasks

- Create a run: `turn-inspector create run --name "Demo"`
- Add a turn: `turn-inspector create turn --run-id 1 --blocks '[{"order":0,"kind":"user","payload":{"text":"Hi"}}]'`
- Inspect: `turn-inspector run list`, `turn-inspector list turns --run-id 1`, `turn-inspector show turn --id 1`
