package schema

import (
	"entgo.io/ent"
	"entgo.io/ent/schema/edge"
	"entgo.io/ent/schema/field"
)

// Run holds the schema definition for the Run entity.
type Run struct {
	ent.Schema
}

// Fields of the Run.
func (Run) Fields() []ent.Field {
	return []ent.Field{
		// Optional human-friendly name for the run
		field.String("name").Optional(),
	}
}

// Edges of the Run.
func (Run) Edges() []ent.Edge {
	return []ent.Edge{
		// A run contains many turns
		edge.To("turns", Turn.Type),
		// Per-run metadata
		edge.To("metadata", RunMetadata.Type),
	}
}
