---
Title: Commands Reference
Slug: commands
Short: Quick reference for turn-inspector commands
SectionType: GeneralTopic
IsTopLevel: true
ShowPerDefault: true
---

## Runs

- Create: `turn-inspector create run --name NAME [--metadata '{"source":"...","key":"...","value":"..."}']`
- List: `turn-inspector run list`
- Show: `turn-inspector run show --id ID`
- Delete: `turn-inspector run delete --id ID --confirm`

## Turns

- Create: `turn-inspector create turn --run-id ID --blocks JSON [--metadata JSON ...]`
- List: `turn-inspector list turns [--run-id ID] [--limit N] [--offset N]`
- Show: `turn-inspector show turn --id ID [--json]`
- Query: `turn-inspector query turns [--metadata-key K] [--metadata-value V] [--text T] [--block-kind KIND] [--run-id ID]`
- Delete: `turn-inspector delete turn --id ID [--confirm]`
- Delete all: `turn-inspector delete all --confirm`

## Blocks

- Show blocks of a turn: `turn-inspector show blocks --turn-id ID`
