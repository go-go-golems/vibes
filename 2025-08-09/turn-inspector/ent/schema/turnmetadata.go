package schema

import (
	"entgo.io/ent"
	"entgo.io/ent/schema/edge"
	"entgo.io/ent/schema/field"
	"entgo.io/ent/schema/index"
)

// TurnMetadata holds the schema definition for the TurnMetadata entity.
type TurnMetadata struct {
	ent.Schema
}

// Fields of the TurnMetadata.
func (TurnMetadata) Fields() []ent.Field {
	return []ent.Field{
		field.String("source"),
		field.String("key"),
		field.String("value"),
	}
}

// Edges of the TurnMetadata.
func (TurnMetadata) Edges() []ent.Edge {
	return []ent.Edge{
		edge.From("turn", Turn.Type).
			Ref("metadata").
			Unique().
			Required(),
	}
}

// Indexes of the TurnMetadata.
func (TurnMetadata) Indexes() []ent.Index {
	return []ent.Index{
		index.Fields("source", "key"),
		index.Fields("source", "key").Edges("turn").Unique(), // (turn, source, key) unique
	}
}
