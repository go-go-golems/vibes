package schema

import (
	"time"

	"entgo.io/ent"
	"entgo.io/ent/schema/field"
	"entgo.io/ent/schema/index"
)

// FarmOperation holds the schema definition for the FarmOperation entity.
type FarmOperation struct {
	ent.Schema
}

// Fields of the FarmOperation.
func (FarmOperation) Fields() []ent.Field {
	return []ent.Field{
		field.Time("operation_date").
			Default(time.Now).
			Comment("Date of the farm operation"),
		field.Enum("operation_type").
			Values(
				"feeding", "milking", "cleaning", "maintenance", "vaccination_batch",
				"deworming_batch", "hoof_trimming", "pasture_rotation", "equipment_maintenance",
				"feed_purchase", "supply_purchase", "milk_sale", "goat_sale", "other",
			).
			Comment("Type of farm operation"),
		field.String("description").
			Comment("Description of the operation performed"),
		field.String("performed_by").
			Comment("Person who performed the operation"),
		field.String("affected_goats").
			Optional().
			Comment("Comma-separated list of goat tags affected by this operation"),
		field.Float("quantity").
			Optional().
			Comment("Quantity involved (feed amount, milk volume, etc.)"),
		field.String("unit").
			Optional().
			Comment("Unit of measurement for quantity"),
		field.Float("cost").
			Optional().
			Min(0).
			Comment("Cost associated with the operation"),
		field.Float("revenue").
			Optional().
			Min(0).
			Comment("Revenue generated from the operation"),
		field.String("supplier_buyer").
			Optional().
			Comment("Supplier or buyer involved in the operation"),
		field.Text("notes").
			Optional().
			Comment("Additional notes about the operation"),
		field.Time("created_at").
			Default(time.Now).
			Immutable(),
		field.Time("updated_at").
			Default(time.Now).
			UpdateDefault(time.Now),
	}
}

// Edges of the FarmOperation.
func (FarmOperation) Edges() []ent.Edge {
	return []ent.Edge{}
}

// Indexes of the FarmOperation.
func (FarmOperation) Indexes() []ent.Index {
	return []ent.Index{
		index.Fields("operation_date"),
		index.Fields("operation_type"),
		index.Fields("performed_by"),
		index.Fields("operation_date", "operation_type"),
	}
}

