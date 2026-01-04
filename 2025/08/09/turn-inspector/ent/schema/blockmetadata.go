package schema

import (
	"entgo.io/ent"
	"entgo.io/ent/schema/edge"
	"entgo.io/ent/schema/field"
	"entgo.io/ent/schema/index"
)

// BlockMetadata holds the schema definition for the BlockMetadata entity.
type BlockMetadata struct {
	ent.Schema
}

// Fields of the BlockMetadata.
func (BlockMetadata) Fields() []ent.Field {
	return []ent.Field{
		field.String("source"),
		field.String("key"),
		field.String("value"),
	}
}

// Edges of the BlockMetadata.
func (BlockMetadata) Edges() []ent.Edge {
	return []ent.Edge{
		edge.From("block", Block.Type).
			Ref("metadata").
			Unique().
			Required(),
	}
}

// Indexes of the BlockMetadata.
func (BlockMetadata) Indexes() []ent.Index {
	return []ent.Index{
		index.Fields("source", "key"),
		index.Fields("source", "key").Edges("block").Unique(), // (block, source, key) unique
	}
}
