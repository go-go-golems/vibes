package schema

import (
	"time"

	"entgo.io/ent"
	"entgo.io/ent/schema/field"
	"entgo.io/ent/schema/index"
)

// FeedRecord holds the schema definition for the FeedRecord entity.
type FeedRecord struct {
	ent.Schema
}

// Fields of the FeedRecord.
func (FeedRecord) Fields() []ent.Field {
	return []ent.Field{
		field.Time("feeding_date").
			Default(time.Now).
			Comment("Date and time of feeding"),
		field.Enum("feed_type").
			Values("hay", "grain", "pellets", "pasture", "silage", "supplements", "treats", "other").
			Comment("Type of feed given"),
		field.String("feed_name").
			Optional().
			Comment("Specific name or brand of the feed"),
		field.Float("quantity").
			Positive().
			Comment("Quantity of feed given"),
		field.Enum("unit").
			Values("kg", "lbs", "cups", "scoops", "bales").
			Default("kg").
			Comment("Unit of measurement"),
		field.String("goat_tags").
			Optional().
			Comment("Comma-separated list of goat tags that received this feed (empty for group feeding)"),
		field.Enum("feeding_method").
			Values("individual", "group", "pasture").
			Default("group").
			Comment("Method of feeding"),
		field.String("fed_by").
			Comment("Person who performed the feeding"),
		field.Float("cost_per_unit").
			Optional().
			Min(0).
			Comment("Cost per unit of feed"),
		field.Text("notes").
			Optional().
			Comment("Additional notes about feeding"),
		field.Time("created_at").
			Default(time.Now).
			Immutable(),
	}
}

// Edges of the FeedRecord.
func (FeedRecord) Edges() []ent.Edge {
	return []ent.Edge{}
}

// Indexes of the FeedRecord.
func (FeedRecord) Indexes() []ent.Index {
	return []ent.Index{
		index.Fields("feeding_date"),
		index.Fields("feed_type"),
		index.Fields("fed_by"),
		index.Fields("feeding_date", "feed_type"),
	}
}

