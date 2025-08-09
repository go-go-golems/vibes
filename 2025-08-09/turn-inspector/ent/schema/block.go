package schema

import (
	"entgo.io/ent"
	"entgo.io/ent/schema/edge"
	"entgo.io/ent/schema/field"
	"entgo.io/ent/schema/index"
)

// Block holds the schema definition for the Block entity.
type Block struct {
	ent.Schema
}

// Fields of the Block.
func (Block) Fields() []ent.Field {
	return []ent.Field{
		// Position of the block within the turn's conversation sequence.
		field.Int("order"),

		// Kind of block in the conversation.
		field.Enum("kind").
			Values("llm_text", "tool_call", "tool_use", "system", "user", "other"),

		// Optional role label if relevant (e.g., "assistant", "user", "tool").
		field.String("role").Optional(),

		// Generic payload for the block (text, tool name/args, etc).
		field.JSON("payload", map[string]any{}).Optional(),
	}
}

// Edges of the Block.
func (Block) Edges() []ent.Edge {
	return []ent.Edge{
		edge.From("turn", Turn.Type).
			Ref("blocks").
			Required().
			Unique(),
		edge.To("metadata", BlockMetadata.Type),
	}
}

// Indexes of the Block.
func (Block) Indexes() []ent.Index {
	return []ent.Index{
		// (turn, order) unique - ensures blocks are ordered within a turn
		index.Fields("order").Edges("turn").Unique(),
	}
}
