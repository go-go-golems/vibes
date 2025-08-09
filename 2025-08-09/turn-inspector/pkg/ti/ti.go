package ti

import (
    "context"
    "fmt"

    "turn-inspector/ent"
    entblock "turn-inspector/ent/block"
    entblockmeta "turn-inspector/ent/blockmetadata"
    entrun "turn-inspector/ent/run"
    entrunmeta "turn-inspector/ent/runmetadata"
    entturn "turn-inspector/ent/turn"
    entturnmeta "turn-inspector/ent/turnmetadata"
)

// MetadataKV captures a metadata triplet.
type MetadataKV struct {
    Source string
    Key    string
    Value  string
}

// BlockInput describes a block to create.
type BlockInput struct {
    Order   int
    Kind    entblock.Kind
    Role    *string
    Payload map[string]any
    // Optional block-level metadata
    Metadata []MetadataKV
}

// TurnInput describes a turn with optional blocks and metadata to create.
type TurnInput struct {
    RunID    int
    Metadata []MetadataKV
    Blocks   []BlockInput
}

// CreateRun creates a run with optional metadata.
func CreateRun(ctx context.Context, client *ent.Client, name string, metadata []MetadataKV) (*ent.Run, error) {
    r, err := client.Run.Create().SetName(name).Save(ctx)
    if err != nil {
        return nil, err
    }
    if len(metadata) > 0 {
        if err := UpsertRunMetadataBatch(ctx, client, r.ID, metadata); err != nil {
            return nil, err
        }
    }
    return r, nil
}

// UpsertRunMetadataBatch upserts run metadata entries by (source,key).
func UpsertRunMetadataBatch(ctx context.Context, client *ent.Client, runID int, metadata []MetadataKV) error {
    for _, kv := range metadata {
        if _, err := UpsertRunMetadata(ctx, client, runID, kv); err != nil {
            return err
        }
    }
    return nil
}

// UpsertRunMetadata upserts a single metadata entry on a run.
func UpsertRunMetadata(ctx context.Context, client *ent.Client, runID int, kv MetadataKV) (*ent.RunMetadata, error) {
    existing, err := client.RunMetadata.Query().
        Where(
            entrunmeta.SourceEQ(kv.Source),
            entrunmeta.KeyEQ(kv.Key),
            entrunmeta.HasRunWith(entrun.IDEQ(runID)),
        ).
        Only(ctx)
    if ent.IsNotFound(err) {
        return client.RunMetadata.Create().
            SetRunID(runID).
            SetSource(kv.Source).
            SetKey(kv.Key).
            SetValue(kv.Value).
            Save(ctx)
    }
    if err != nil {
        return nil, err
    }
    return client.RunMetadata.UpdateOneID(existing.ID).SetValue(kv.Value).Save(ctx)
}

// CreateTurnWithBlocks creates a turn with optional metadata and blocks in a transaction.
func CreateTurnWithBlocks(ctx context.Context, client *ent.Client, input TurnInput) (*ent.Turn, []*ent.Block, error) {
    tx, err := client.Tx(ctx)
    if err != nil {
        return nil, nil, err
    }
    committed := false
    defer func() {
        if !committed {
            _ = tx.Rollback()
        }
    }()

    t, err := tx.Turn.Create().SetRunID(input.RunID).Save(ctx)
    if err != nil {
        return nil, nil, err
    }
    if len(input.Metadata) > 0 {
        for _, kv := range input.Metadata {
            if _, err := upsertTurnMetadataTx(ctx, tx, t.ID, kv); err != nil {
                return nil, nil, err
            }
        }
    }
    blocks := make([]*ent.Block, 0, len(input.Blocks))
    for _, bi := range input.Blocks {
        bc := tx.Block.Create().
            SetTurnID(t.ID).
            SetOrder(bi.Order).
            SetKind(bi.Kind)
        if bi.Role != nil {
            bc = bc.SetRole(*bi.Role)
        }
        if bi.Payload != nil {
            bc = bc.SetPayload(bi.Payload)
        }
        b, err := bc.Save(ctx)
        if err != nil {
            return nil, nil, err
        }
        if len(bi.Metadata) > 0 {
            for _, kv := range bi.Metadata {
                if _, err := upsertBlockMetadataTx(ctx, tx, b.ID, kv); err != nil {
                    return nil, nil, err
                }
            }
        }
        blocks = append(blocks, b)
    }

    if err := tx.Commit(); err != nil {
        return nil, nil, err
    }
    committed = true
    return t, blocks, nil
}

// UpsertTurnMetadataBatch upserts metadata for a turn.
func UpsertTurnMetadataBatch(ctx context.Context, client *ent.Client, turnID int, metadata []MetadataKV) error {
    for _, kv := range metadata {
        if _, err := UpsertTurnMetadata(ctx, client, turnID, kv); err != nil {
            return err
        }
    }
    return nil
}

// UpsertTurnMetadata upserts a single metadata entry on a turn.
func UpsertTurnMetadata(ctx context.Context, client *ent.Client, turnID int, kv MetadataKV) (*ent.TurnMetadata, error) {
    existing, err := client.TurnMetadata.Query().
        Where(
            entturnmeta.SourceEQ(kv.Source),
            entturnmeta.KeyEQ(kv.Key),
            entturnmeta.HasTurnWith(entturn.IDEQ(turnID)),
        ).
        Only(ctx)
    if ent.IsNotFound(err) {
        return client.TurnMetadata.Create().
            SetTurnID(turnID).
            SetSource(kv.Source).
            SetKey(kv.Key).
            SetValue(kv.Value).
            Save(ctx)
    }
    if err != nil {
        return nil, err
    }
    return client.TurnMetadata.UpdateOneID(existing.ID).SetValue(kv.Value).Save(ctx)
}

// UpsertBlockMetadataBatch upserts metadata for a block.
func UpsertBlockMetadataBatch(ctx context.Context, client *ent.Client, blockID int, metadata []MetadataKV) error {
    for _, kv := range metadata {
        if _, err := UpsertBlockMetadata(ctx, client, blockID, kv); err != nil {
            return err
        }
    }
    return nil
}

// UpsertBlockMetadata upserts a single metadata entry on a block.
func UpsertBlockMetadata(ctx context.Context, client *ent.Client, blockID int, kv MetadataKV) (*ent.BlockMetadata, error) {
    existing, err := client.BlockMetadata.Query().
        Where(
            entblockmeta.SourceEQ(kv.Source),
            entblockmeta.KeyEQ(kv.Key),
            entblockmeta.HasBlockWith(entblock.IDEQ(blockID)),
        ).
        Only(ctx)
    if ent.IsNotFound(err) {
        return client.BlockMetadata.Create().
            SetBlockID(blockID).
            SetSource(kv.Source).
            SetKey(kv.Key).
            SetValue(kv.Value).
            Save(ctx)
    }
    if err != nil {
        return nil, err
    }
    return client.BlockMetadata.UpdateOneID(existing.ID).SetValue(kv.Value).Save(ctx)
}

// UpdateBlock modifies optional role and payload. Pass nil to keep a field unchanged.
func UpdateBlock(ctx context.Context, client *ent.Client, blockID int, role *string, payload map[string]any) (*ent.Block, error) {
    u := client.Block.UpdateOneID(blockID)
    if role != nil {
        u = u.SetRole(*role)
    }
    if payload != nil {
        u = u.SetPayload(payload)
    }
    return u.Save(ctx)
}

// SwapBlockOrders swaps two block orders within a turn safely using a transaction.
func SwapBlockOrders(ctx context.Context, client *ent.Client, turnID int, orderA, orderB int) error {
    tx, err := client.Tx(ctx)
    if err != nil {
        return err
    }
    committed := false
    defer func() {
        if !committed {
            _ = tx.Rollback()
        }
    }()

    // Move A to a temporary value
    if _, err := tx.Block.Update().
        Where(entblock.OrderEQ(orderA), entblock.HasTurnWith(entturn.IDEQ(turnID))).
        SetOrder(-1).Save(ctx); err != nil {
        return err
    }
    if _, err := tx.Block.Update().
        Where(entblock.OrderEQ(orderB), entblock.HasTurnWith(entturn.IDEQ(turnID))).
        SetOrder(orderA).Save(ctx); err != nil {
        return err
    }
    if _, err := tx.Block.Update().
        Where(entblock.OrderEQ(-1), entblock.HasTurnWith(entturn.IDEQ(turnID))).
        SetOrder(orderB).Save(ctx); err != nil {
        return err
    }

    if err := tx.Commit(); err != nil {
        return err
    }
    committed = true
    return nil
}

// DeleteTurnCascade deletes a turn with its blocks and metadata.
func DeleteTurnCascade(ctx context.Context, client *ent.Client, turnID int) error {
    tx, err := client.Tx(ctx)
    if err != nil { return err }
    committed := false
    defer func() { if !committed { _ = tx.Rollback() } }()

    // Delete block metadata -> blocks
    blockIDs, err := tx.Block.Query().Where(entblock.HasTurnWith(entturn.IDEQ(turnID))).IDs(ctx)
    if err != nil { return err }
    if len(blockIDs) > 0 {
        if _, err := tx.BlockMetadata.Delete().Where(entblockmeta.HasBlockWith(entblock.IDIn(blockIDs...))).Exec(ctx); err != nil { return err }
        if _, err := tx.Block.Delete().Where(entblock.IDIn(blockIDs...)).Exec(ctx); err != nil { return err }
    }
    // Turn metadata -> turn
    if _, err := tx.TurnMetadata.Delete().Where(entturnmeta.HasTurnWith(entturn.IDEQ(turnID))).Exec(ctx); err != nil { return err }
    if err := tx.Turn.DeleteOneID(turnID).Exec(ctx); err != nil { return err }

    if err := tx.Commit(); err != nil { return err }
    committed = true
    return nil
}

// DeleteRunCascade deletes a run and all its child data.
func DeleteRunCascade(ctx context.Context, client *ent.Client, runID int) error {
    tx, err := client.Tx(ctx)
    if err != nil { return err }
    committed := false
    defer func() { if !committed { _ = tx.Rollback() } }()

    turnIDs, err := tx.Turn.Query().Where(entturn.HasRunWith(entrun.IDEQ(runID))).IDs(ctx)
    if err != nil { return err }
    if len(turnIDs) > 0 {
        // Delete all blocks and metadata for those turns
        blockIDs, err := tx.Block.Query().Where(entblock.HasTurnWith(entturn.IDIn(turnIDs...))).IDs(ctx)
        if err != nil { return err }
        if len(blockIDs) > 0 {
            if _, err := tx.BlockMetadata.Delete().Where(entblockmeta.HasBlockWith(entblock.IDIn(blockIDs...))).Exec(ctx); err != nil { return err }
            if _, err := tx.Block.Delete().Where(entblock.IDIn(blockIDs...)).Exec(ctx); err != nil { return err }
        }
        if _, err := tx.TurnMetadata.Delete().Where(entturnmeta.HasTurnWith(entturn.IDIn(turnIDs...))).Exec(ctx); err != nil { return err }
        if _, err := tx.Turn.Delete().Where(entturn.IDIn(turnIDs...)).Exec(ctx); err != nil { return err }
    }
    if _, err := tx.RunMetadata.Delete().Where(entrunmeta.HasRunWith(entrun.IDEQ(runID))).Exec(ctx); err != nil { return err }
    if err := tx.Run.DeleteOneID(runID).Exec(ctx); err != nil { return fmt.Errorf("delete run %d: %w", runID, err) }

    if err := tx.Commit(); err != nil { return err }
    committed = true
    return nil
}

// Internal helpers that operate within an existing transaction.
func upsertTurnMetadataTx(ctx context.Context, tx *ent.Tx, turnID int, kv MetadataKV) (*ent.TurnMetadata, error) {
    existing, err := tx.TurnMetadata.Query().
        Where(
            entturnmeta.SourceEQ(kv.Source),
            entturnmeta.KeyEQ(kv.Key),
            entturnmeta.HasTurnWith(entturn.IDEQ(turnID)),
        ).Only(ctx)
    if ent.IsNotFound(err) {
        return tx.TurnMetadata.Create().SetTurnID(turnID).SetSource(kv.Source).SetKey(kv.Key).SetValue(kv.Value).Save(ctx)
    }
    if err != nil { return nil, err }
    return tx.TurnMetadata.UpdateOneID(existing.ID).SetValue(kv.Value).Save(ctx)
}

func upsertBlockMetadataTx(ctx context.Context, tx *ent.Tx, blockID int, kv MetadataKV) (*ent.BlockMetadata, error) {
    existing, err := tx.BlockMetadata.Query().
        Where(
            entblockmeta.SourceEQ(kv.Source),
            entblockmeta.KeyEQ(kv.Key),
            entblockmeta.HasBlockWith(entblock.IDEQ(blockID)),
        ).Only(ctx)
    if ent.IsNotFound(err) {
        return tx.BlockMetadata.Create().SetBlockID(blockID).SetSource(kv.Source).SetKey(kv.Key).SetValue(kv.Value).Save(ctx)
    }
    if err != nil { return nil, err }
    return tx.BlockMetadata.UpdateOneID(existing.ID).SetValue(kv.Value).Save(ctx)
}


