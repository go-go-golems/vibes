package schema

import (
	"entgo.io/ent"
	"entgo.io/ent/schema/edge"
	"entgo.io/ent/schema/field"
	"entgo.io/ent/schema/index"
)

// RunMetadata holds the schema definition for the RunMetadata entity.
type RunMetadata struct {
	ent.Schema
}

// Fields of the RunMetadata.
func (RunMetadata) Fields() []ent.Field {
	return []ent.Field{
		field.String("source"),
		field.String("key"),
		field.String("value"),
	}
}

// Edges of the RunMetadata.
func (RunMetadata) Edges() []ent.Edge {
	return []ent.Edge{
		edge.From("run", Run.Type).
			Ref("metadata").
			Unique().
			Required(),
	}
}

// Indexes of the RunMetadata.
func (RunMetadata) Indexes() []ent.Index {
	return []ent.Index{
		index.Fields("source", "key"),
		index.Fields("source", "key").Edges("run").Unique(), // (run, source, key) unique
	}
}
