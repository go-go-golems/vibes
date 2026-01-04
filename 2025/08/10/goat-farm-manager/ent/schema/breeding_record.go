package schema

import (
	"time"

	"entgo.io/ent"
	"entgo.io/ent/schema/edge"
	"entgo.io/ent/schema/field"
	"entgo.io/ent/schema/index"
)

// BreedingRecord holds the schema definition for the BreedingRecord entity.
type BreedingRecord struct {
	ent.Schema
}

// Fields of the BreedingRecord.
func (BreedingRecord) Fields() []ent.Field {
	return []ent.Field{
		field.String("doe_tag").
			Comment("Tag ID of the female goat (doe)"),
		field.String("buck_tag").
			Comment("Tag ID of the male goat (buck)"),
		field.Time("breeding_date").
			Comment("Date of breeding"),
		field.Time("expected_kidding_date").
			Optional().
			Comment("Expected kidding date (approximately 150 days after breeding)"),
		field.Time("actual_kidding_date").
			Optional().
			Comment("Actual kidding date"),
		field.Int("kids_born").
			Optional().
			Min(0).
			Comment("Number of kids born"),
		field.Int("kids_alive").
			Optional().
			Min(0).
			Comment("Number of kids that survived"),
		field.Enum("breeding_method").
			Values("natural", "artificial_insemination").
			Default("natural").
			Comment("Method of breeding"),
		field.Enum("status").
			Values("bred", "confirmed_pregnant", "kidded", "failed", "aborted").
			Default("bred").
			Comment("Status of the breeding"),
		field.Text("complications").
			Optional().
			Comment("Any complications during breeding or kidding"),
		field.Text("notes").
			Optional().
			Comment("Additional notes"),
		field.Time("created_at").
			Default(time.Now).
			Immutable(),
		field.Time("updated_at").
			Default(time.Now).
			UpdateDefault(time.Now),
	}
}

// Edges of the BreedingRecord.
func (BreedingRecord) Edges() []ent.Edge {
	return []ent.Edge{
		edge.From("doe", Goat.Type).
			Ref("breeding_records").
			Field("doe_tag").
			Required().
			Unique(),
	}
}

// Indexes of the BreedingRecord.
func (BreedingRecord) Indexes() []ent.Index {
	return []ent.Index{
		index.Fields("doe_tag", "breeding_date"),
		index.Fields("buck_tag", "breeding_date"),
		index.Fields("breeding_date"),
		index.Fields("expected_kidding_date"),
		index.Fields("status"),
	}
}

