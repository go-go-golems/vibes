package schema

import (
	"time"

	"entgo.io/ent"
	"entgo.io/ent/schema/edge"
	"entgo.io/ent/schema/field"
	"entgo.io/ent/schema/index"
)

// HealthRecord holds the schema definition for the HealthRecord entity.
type HealthRecord struct {
	ent.Schema
}

// Fields of the HealthRecord.
func (HealthRecord) Fields() []ent.Field {
	return []ent.Field{
		field.String("goat_tag").
			Comment("Tag ID of the goat"),
		field.Time("record_date").
			Default(time.Now).
			Comment("Date of the health record"),
		field.Enum("record_type").
			Values("vaccination", "treatment", "checkup", "injury", "illness", "medication", "deworming", "hoof_trim").
			Comment("Type of health record"),
		field.String("description").
			Comment("Description of the health event"),
		field.String("veterinarian").
			Optional().
			Comment("Veterinarian who performed the treatment"),
		field.String("medication").
			Optional().
			Comment("Medication administered"),
		field.String("dosage").
			Optional().
			Comment("Dosage of medication"),
		field.Float("temperature").
			Optional().
			Comment("Body temperature in Celsius"),
		field.Float("weight").
			Optional().
			Positive().
			Comment("Weight at time of record in kg"),
		field.Time("next_due_date").
			Optional().
			Comment("Next due date for follow-up or recurring treatment"),
		field.Float("cost").
			Optional().
			Min(0).
			Comment("Cost of treatment or medication"),
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

// Edges of the HealthRecord.
func (HealthRecord) Edges() []ent.Edge {
	return []ent.Edge{
		edge.From("goat", Goat.Type).
			Ref("health_records").
			Field("goat_tag").
			Required().
			Unique(),
	}
}

// Indexes of the HealthRecord.
func (HealthRecord) Indexes() []ent.Index {
	return []ent.Index{
		index.Fields("goat_tag", "record_date"),
		index.Fields("record_type"),
		index.Fields("record_date"),
		index.Fields("next_due_date"),
	}
}

