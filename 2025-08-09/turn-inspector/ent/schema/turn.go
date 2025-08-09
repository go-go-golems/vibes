package schema

import (
	"entgo.io/ent"
	"entgo.io/ent/schema/edge"
)

// Turn holds the schema definition for the Turn entity.
type Turn struct {
	ent.Schema
}

// Fields of the Turn.
func (Turn) Fields() []ent.Field {
	return nil
}

// Edges of the Turn.
func (Turn) Edges() []ent.Edge {
	return []ent.Edge{
		// Belongs to a run
		edge.From("run", Run.Type).
			Ref("turns").
			Required().
			Unique(),
		// Per-turn metadata
		edge.To("metadata", TurnMetadata.Type),
		// Blocks (ordered by Block.order in queries)
		edge.To("blocks", Block.Type),
	}
}
