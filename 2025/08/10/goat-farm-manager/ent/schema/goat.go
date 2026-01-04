package schema

import (
	"time"

	"entgo.io/ent"
	"entgo.io/ent/schema/edge"
	"entgo.io/ent/schema/field"
	"entgo.io/ent/schema/index"
)

// Goat holds the schema definition for the Goat entity.
type Goat struct {
	ent.Schema
}

// Fields of the Goat.
func (Goat) Fields() []ent.Field {
	return []ent.Field{
		field.String("id").
			StorageKey("tag_id").
			Comment("Unique identifier tag for the goat"),
		field.String("name").
			Optional().
			Comment("Name of the goat"),
		field.Enum("breed").
			Values("nubian", "alpine", "saanen", "toggenburg", "lamancha", "boer", "angus", "other").
			Comment("Breed of the goat"),
		field.Enum("gender").
			Values("male", "female").
			Comment("Gender of the goat"),
		field.Time("birth_date").
			Optional().
			Comment("Birth date of the goat"),
		field.Float("weight").
			Optional().
			Positive().
			Comment("Current weight in kg"),
		field.Enum("status").
			Values("active", "pregnant", "lactating", "dry", "sick", "sold", "deceased").
			Default("active").
			Comment("Current status of the goat"),
		field.String("sire_tag").
			Optional().
			Comment("Tag ID of the father"),
		field.String("dam_tag").
			Optional().
			Comment("Tag ID of the mother"),
		field.Text("notes").
			Optional().
			Comment("Additional notes about the goat"),
		field.Time("created_at").
			Default(time.Now).
			Immutable(),
		field.Time("updated_at").
			Default(time.Now).
			UpdateDefault(time.Now),
	}
}

// Edges of the Goat.
func (Goat) Edges() []ent.Edge {
	return []ent.Edge{
		edge.To("milk_records", MilkRecord.Type),
		edge.To("health_records", HealthRecord.Type),
		edge.To("breeding_records", BreedingRecord.Type),
	}
}

// Indexes of the Goat.
func (Goat) Indexes() []ent.Index {
	return []ent.Index{
		index.Fields("breed"),
		index.Fields("status"),
		index.Fields("gender"),
	}
}

