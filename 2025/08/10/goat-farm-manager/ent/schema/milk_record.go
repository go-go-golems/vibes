package schema

import (
	"time"

	"entgo.io/ent"
	"entgo.io/ent/schema/edge"
	"entgo.io/ent/schema/field"
	"entgo.io/ent/schema/index"
)

// MilkRecord holds the schema definition for the MilkRecord entity.
type MilkRecord struct {
	ent.Schema
}

// Fields of the MilkRecord.
func (MilkRecord) Fields() []ent.Field {
	return []ent.Field{
		field.String("goat_tag").
			Comment("Tag ID of the goat that produced the milk"),
		field.Time("milking_time").
			Comment("Date and time of milking"),
		field.Enum("milking_session").
			Values("morning", "afternoon", "evening").
			Comment("Which milking session of the day"),
		field.Float("volume_liters").
			Positive().
			Comment("Volume of milk produced in liters"),
		field.Float("fat_content").
			Optional().
			Min(0).
			Max(100).
			Comment("Fat content percentage"),
		field.Float("protein_content").
			Optional().
			Min(0).
			Max(100).
			Comment("Protein content percentage"),
		field.Float("somatic_cell_count").
			Optional().
			Positive().
			Comment("Somatic cell count (cells/ml)"),
		field.Enum("quality_grade").
			Values("A", "B", "C", "reject").
			Default("A").
			Comment("Quality grade of the milk"),
		field.String("milked_by").
			Optional().
			Comment("Person who performed the milking"),
		field.Text("notes").
			Optional().
			Comment("Additional notes about the milking session"),
		field.Time("created_at").
			Default(time.Now).
			Immutable(),
	}
}

// Edges of the MilkRecord.
func (MilkRecord) Edges() []ent.Edge {
	return []ent.Edge{
		edge.From("goat", Goat.Type).
			Ref("milk_records").
			Field("goat_tag").
			Required().
			Unique(),
	}
}

// Indexes of the MilkRecord.
func (MilkRecord) Indexes() []ent.Index {
	return []ent.Index{
		index.Fields("goat_tag", "milking_time"),
		index.Fields("milking_time"),
		index.Fields("quality_grade"),
		index.Fields("milking_session"),
	}
}

