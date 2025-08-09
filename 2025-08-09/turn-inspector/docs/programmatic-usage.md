---
Title: Programmatic Usage with Ent
Slug: programmatic-usage
Short: How to use the Ent models for runs, turns, blocks, and metadata
SectionType: Tutorial
IsTopLevel: true
ShowPerDefault: true
---

# Programmatic Usage with Ent

This guide shows how to work with the Turn Inspector data model directly from Go using Ent: creating runs, turns, and blocks; attaching metadata; and running common queries. It assumes basic familiarity with Go and Ent.

## Data model overview

- **Run**: Top-level container for a session/experiment. Fields: `name?`. Edges: `turns` (o2m), `metadata` (o2m).
- **Turn**: A single conversation turn within a run. Edges: `run` (m2o, required), `blocks` (o2m), `metadata` (o2m).
- **Block**: A unit within a turn. Fields: `order` (int), `kind` (enum: `user`, `llm_text`, `tool_call`, `tool_use`, `system`, `other`), `role?` (string), `payload?` (JSON map). Edges: `turn` (m2o, required), `metadata` (o2m).
- **Metadata**: Triplets (`source`, `key`, `value`) attached to `Run`, `Turn`, or `Block`. Uniqueness per parent: `(run, source, key)`, `(turn, source, key)`, `(block, source, key)`.

Relationship shape (high level): Run → Turn → Block, with metadata on each level. Blocks are ordered within a turn by their `order` and are unique per turn (`(turn, order)` is unique).

## Setup the Ent client

Initialize the Ent client and run migrations. The CLI uses SQLite; you can reuse the same DSN scheme.

```go
package main

import (
  "context"
  "log"

  "entgo.io/ent/dialect"
  entsql "entgo.io/ent/dialect/sql"
  _ "github.com/mattn/go-sqlite3"

  "turn-inspector/ent"
)

func newClient() *ent.Client {
  // Open sqlite3 database; enable FKs
  db, err := entsql.Open(dialect.SQLite, "file:turns.db?_fk=1")
  if err != nil { log.Fatalf("open db: %v", err) }
  client := ent.NewClient(ent.Driver(db))
  if err := client.Schema.Create(context.Background()); err != nil {
    log.Fatalf("migrate: %v", err)
  }
  return client
}
```

Tip: The CLI honors `TURN_INSPECTOR_DB`; you can adopt the same env var to share a database.

## Creating data

### Create a run with metadata

```go
ctx := context.Background()
client := newClient()

r, err := client.Run.Create().
  SetName("Demo Run").
  Save(ctx)
if err != nil { /* handle */ }

// Attach run-level metadata (unique per (source,key))
_, err = client.RunMetadata.Create().
  SetRunID(r.ID).
  SetSource("session").
  SetKey("id").
  SetValue("abc123").
  Save(ctx)
if err != nil { /* handle */ }
```

### Create a turn and its blocks (transactional)

Creating a turn requires associating it with a parent run. Blocks require `order`, `kind`, and a parent turn. Use a transaction to ensure all-or-nothing.

```go
tx, err := client.Tx(ctx)
if err != nil { /* handle */ }
defer func() { if err != nil { _ = tx.Rollback() } }()

t, err := tx.Turn.Create().
  SetRunID(r.ID).
  Save(ctx)
if err != nil { /* handle */ }

// user message @ order 0
_, err = tx.Block.Create().
  SetTurnID(t.ID).
  SetOrder(0).
  SetKind(entblock.KindUser). // import alias: entblock "turn-inspector/ent/block"
  SetRole("user").
  SetPayload(map[string]any{"text": "Hello"}).
  Save(ctx)
if err != nil { /* handle */ }

// assistant response @ order 1
_, err = tx.Block.Create().
  SetTurnID(t.ID).
  SetOrder(1).
  SetKind(entblock.KindLlmText).
  SetRole("assistant").
  SetPayload(map[string]any{"text": "Hi there!"}).
  Save(ctx)
if err != nil { /* handle */ }

// optional: add per-turn metadata
_, err = tx.TurnMetadata.Create().
  SetTurnID(t.ID).
  SetSource("user").
  SetKey("tier").
  SetValue("premium").
  Save(ctx)
if err != nil { /* handle */ }

err = tx.Commit()
if err != nil { /* handle */ }
```

Notes:
- Enum values for `kind` live in `turn-inspector/ent/block` (e.g., `block.KindUser`, `block.KindToolCall`).
- The `(turn, order)` pair must be unique; assign sequential orders per turn.

## Query recipes

### List runs with counts

```go
runs, err := client.Run.Query().
  WithMetadata().
  WithTurns().
  Order(ent.Desc(entrun.FieldID)). // import entrun "turn-inspector/ent/run"
  All(ctx)
if err != nil { /* handle */ }
for _, r := range runs {
  mc := len(r.Edges.Metadata)
  tc := len(r.Edges.Turns)
  _ = mc; _ = tc
}
```

### Fetch a run’s turns with ordered blocks

```go
ts, err := client.Turn.Query().
  Where(entturn.HasRunWith(entrun.IDEQ(r.ID))). // entturn "turn-inspector/ent/turn"
  WithBlocks(func(bq *ent.BlockQuery) {
    bq.Order(ent.Asc(entblock.FieldOrder))
  }).
  WithMetadata().
  All(ctx)
if err != nil { /* handle */ }
```

### Filter turns by metadata

```go
// Find turns where (source,key,value) matches any subset; schema has a unique (turn, source, key)
ts, err := client.Turn.Query().
  Where(entturn.HasMetadataWith(
    entturnmeta.SourceEQ("user"),                 // entturnmeta "turn-inspector/ent/turnmetadata"
    entturnmeta.KeyEQ("tier"),
    entturnmeta.ValueEQ("premium"),
  )).
  All(ctx)
if err != nil { /* handle */ }
```

### Find turns containing blocks of a given kind

```go
ts, err := client.Turn.Query().
  Where(entturn.HasBlocksWith(entblock.KindEQ(entblock.KindToolCall))).
  All(ctx)
if err != nil { /* handle */ }
```

### Search for text in block payloads (client-side)

The `payload` is stored as JSON. For SQLite, prefer loading blocks and filtering in Go.

```go
ts, _ := client.Turn.Query().WithBlocks().All(ctx)
var filtered []*ent.Turn
for _, t := range ts {
  for _, b := range t.Edges.Blocks {
    if txt, ok := b.Payload["text"].(string); ok && containsIgnoreCase(txt, "error") {
      filtered = append(filtered, t)
      break
    }
  }
}
```

### Pagination

```go
turns, err := client.Turn.Query().
  Limit(100).Offset(200).
  Order(ent.Desc(entturn.FieldID)).
  All(ctx)
if err != nil { /* handle */ }
```

## Updates

### Update a run name

```go
err := client.Run.UpdateOneID(r.ID).
  SetName("Renamed Run").
  Exec(ctx)
```

### Update a block’s payload and role

```go
_, err := client.Block.UpdateOneID(b.ID).
  SetRole("assistant").
  SetPayload(map[string]any{"text": "Revised message"}).
  Save(ctx)
```

### Reorder blocks in a turn (ensure uniqueness)

```go
tx, err := client.Tx(ctx)
if err != nil { /* handle */ }
defer func() { if err != nil { _ = tx.Rollback() } }()

// Example: swap orders 1 and 2 safely by using a temporary value
_, err = tx.Block.Update().
  Where(entblock.OrderEQ(1), entblock.HasTurnWith(entturn.IDEQ(t.ID))).
  SetOrder(-1). // temp value outside normal range
  Save(ctx)
if err != nil { /* handle */ }

_, err = tx.Block.Update().
  Where(entblock.OrderEQ(2), entblock.HasTurnWith(entturn.IDEQ(t.ID))).
  SetOrder(1).
  Save(ctx)
if err != nil { /* handle */ }

_, err = tx.Block.Update().
  Where(entblock.OrderEQ(-1), entblock.HasTurnWith(entturn.IDEQ(t.ID))).
  SetOrder(2).
  Save(ctx)
if err != nil { /* handle */ }

err = tx.Commit()
if err != nil { /* handle */ }
```

## Deletions

By default, foreign keys are required but not configured with cascade in the schema. To delete a run, first delete its children in dependency order.

```go
tx, err := client.Tx(ctx)
if err != nil { /* handle */ }
defer func() { if err != nil { _ = tx.Rollback() } }()

// Delete block metadata → blocks for all turns in the run
turnIDs, err := tx.Turn.Query().Where(entturn.HasRunWith(entrun.IDEQ(r.ID))).IDs(ctx)
if err != nil { /* handle */ }

_, err = tx.BlockMetadata.Delete().
  Where(entblockmeta.HasBlockWith(entblock.HasTurnWith(entturn.IDIn(turnIDs...)))). // entblockmeta "turn-inspector/ent/blockmetadata"
  Exec(ctx)
if err != nil { /* handle */ }

_, err = tx.Block.Delete().
  Where(entblock.HasTurnWith(entturn.IDIn(turnIDs...))).
  Exec(ctx)
if err != nil { /* handle */ }

// Delete turn metadata → turns
_, err = tx.TurnMetadata.Delete().
  Where(entturnmeta.HasTurnWith(entturn.IDIn(turnIDs...))).
  Exec(ctx)
if err != nil { /* handle */ }

_, err = tx.Turn.Delete().
  Where(entturn.IDIn(turnIDs...)).
  Exec(ctx)
if err != nil { /* handle */ }

// Delete run metadata → run
_, err = tx.RunMetadata.Delete().
  Where(entrunmeta.HasRunWith(entrun.IDEQ(r.ID))). // entrunmeta "turn-inspector/ent/runmetadata"
  Exec(ctx)
if err != nil { /* handle */ }

err = tx.Run.DeleteOneID(r.ID).Exec(ctx)
if err != nil { /* handle */ }

err = tx.Commit()
if err != nil { /* handle */ }
```

## Best practices and constraints

- **Required edges**: `Turn` must `SetRunID`; `Block` must `SetTurnID`; `Block.order` and `Block.kind` are required.
- **Block ordering**: `(turn, order)` is unique. Use transactions when reordering to avoid conflicts.
- **Metadata uniqueness**: For each parent, `(source, key)` must be unique. Use `Update()` if you need to change a value for an existing key.
- **Eager loading**: Prefer `WithBlocks`, `WithMetadata`, and `WithTurns` to avoid N+1 queries.
- **Transactions**: Use `client.Tx(ctx)` for multi-step writes that must succeed or fail together.

## End-to-end example

```go
package main

import (
  "context"
  "log"
  "turn-inspector/ent"
  entblock "turn-inspector/ent/block"
  entrun "turn-inspector/ent/run"
  entturn "turn-inspector/ent/turn"
)

func main() {
  ctx := context.Background()
  client := newClient()
  defer client.Close()

  run, err := client.Run.Create().SetName("Session A").Save(ctx)
  if err != nil { log.Fatal(err) }

  tx, err := client.Tx(ctx)
  if err != nil { log.Fatal(err) }
  defer func() { if err != nil { _ = tx.Rollback() } }()

  turn, err := tx.Turn.Create().SetRunID(run.ID).Save(ctx)
  if err != nil { log.Fatal(err) }
  if _, err = tx.Block.Create().SetTurnID(turn.ID).SetOrder(0).SetKind(entblock.KindUser).SetRole("user").SetPayload(map[string]any{"text": "Hi"}).Save(ctx); err != nil { log.Fatal(err) }
  if _, err = tx.Block.Create().SetTurnID(turn.ID).SetOrder(1).SetKind(entblock.KindLlmText).SetRole("assistant").SetPayload(map[string]any{"text": "Hello!"}).Save(ctx); err != nil { log.Fatal(err) }
  if err = tx.Commit(); err != nil { log.Fatal(err) }

  // Query back with ordering
  ts, err := client.Turn.Query().Where(entturn.HasRunWith(entrun.IDEQ(run.ID))).WithBlocks(func(bq *ent.BlockQuery) { bq.Order(ent.Asc(entblock.FieldOrder)) }).All(ctx)
  if err != nil { log.Fatal(err) }
  _ = ts
}
```

## See also

- CLI usage and commands: `glaze help commands`
- Concepts overview: `glaze help overview`

## Helper

```go
func containsIgnoreCase(haystack, needle string) bool {
  return strings.Contains(strings.ToLower(haystack), strings.ToLower(needle))
}
```


