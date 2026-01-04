package models

import (
	"time"
)

// MilkProductionSummary represents milk production analytics
type MilkProductionSummary struct {
	GoatTag        string    `json:"goat_tag"`
	GoatName       string    `json:"goat_name"`
	TotalVolume    float64   `json:"total_volume"`
	AverageVolume  float64   `json:"average_volume"`
	RecordCount    int       `json:"record_count"`
	LastMilking    time.Time `json:"last_milking"`
	AverageFat     float64   `json:"average_fat"`
	AverageProtein float64   `json:"average_protein"`
	QualityGradeA  int       `json:"quality_grade_a"`
	QualityGradeB  int       `json:"quality_grade_b"`
	QualityGradeC  int       `json:"quality_grade_c"`
	Rejected       int       `json:"rejected"`
}

// HealthSummary represents health analytics for a goat
type HealthSummary struct {
	GoatTag              string    `json:"goat_tag"`
	GoatName             string    `json:"goat_name"`
	LastCheckup          time.Time `json:"last_checkup"`
	VaccinationCount     int       `json:"vaccination_count"`
	TreatmentCount       int       `json:"treatment_count"`
	LastVaccination      time.Time `json:"last_vaccination"`
	NextDueVaccination   time.Time `json:"next_due_vaccination"`
	TotalHealthCost      float64   `json:"total_health_cost"`
	RecentIllnesses      int       `json:"recent_illnesses"`
	LastWeight           float64   `json:"last_weight"`
	WeightTrend          string    `json:"weight_trend"` // "increasing", "decreasing", "stable"
}

// BreedingSummary represents breeding analytics
type BreedingSummary struct {
	DoeTag               string    `json:"doe_tag"`
	DoeName              string    `json:"doe_name"`
	TotalBreedings       int       `json:"total_breedings"`
	SuccessfulBreedings  int       `json:"successful_breedings"`
	TotalKidsBorn        int       `json:"total_kids_born"`
	TotalKidsAlive       int       `json:"total_kids_alive"`
	LastBreeding         time.Time `json:"last_breeding"`
	NextExpectedKidding  time.Time `json:"next_expected_kidding"`
	AverageKidsPerLitter float64   `json:"average_kids_per_litter"`
	SuccessRate          float64   `json:"success_rate"`
}

// FarmSummary represents overall farm analytics
type FarmSummary struct {
	TotalGoats           int     `json:"total_goats"`
	ActiveGoats          int     `json:"active_goats"`
	LactatingGoats       int     `json:"lactating_goats"`
	PregnantGoats        int     `json:"pregnant_goats"`
	DryGoats             int     `json:"dry_goats"`
	SickGoats            int     `json:"sick_goats"`
	TotalMilkToday       float64 `json:"total_milk_today"`
	TotalMilkThisWeek    float64 `json:"total_milk_this_week"`
	TotalMilkThisMonth   float64 `json:"total_milk_this_month"`
	AverageMilkPerGoat   float64 `json:"average_milk_per_goat"`
	TotalFeedCostToday   float64 `json:"total_feed_cost_today"`
	TotalFeedCostWeek    float64 `json:"total_feed_cost_week"`
	TotalFeedCostMonth   float64 `json:"total_feed_cost_month"`
	TotalHealthCostMonth float64 `json:"total_health_cost_month"`
	TotalRevenueMonth    float64 `json:"total_revenue_month"`
	ProfitMarginMonth    float64 `json:"profit_margin_month"`
}

// FeedConsumptionSummary represents feed consumption analytics
type FeedConsumptionSummary struct {
	FeedType        string  `json:"feed_type"`
	TotalQuantity   float64 `json:"total_quantity"`
	TotalCost       float64 `json:"total_cost"`
	AverageCostUnit float64 `json:"average_cost_unit"`
	RecordCount     int     `json:"record_count"`
	LastFed         time.Time `json:"last_fed"`
}

// GoatPerformance represents individual goat performance metrics
type GoatPerformance struct {
	GoatTag                string    `json:"goat_tag"`
	GoatName               string    `json:"goat_name"`
	Age                    int       `json:"age_days"`
	CurrentWeight          float64   `json:"current_weight"`
	WeightGain             float64   `json:"weight_gain"`
	MilkProductionLast30   float64   `json:"milk_production_last_30"`
	MilkProductionAverage  float64   `json:"milk_production_average"`
	HealthScore            float64   `json:"health_score"` // 0-100 based on recent health records
	FeedEfficiency         float64   `json:"feed_efficiency"` // milk per kg of feed
	ProfitabilityScore     float64   `json:"profitability_score"`
	LastMilking            time.Time `json:"last_milking"`
	DaysSinceLastMilking   int       `json:"days_since_last_milking"`
	RecommendedAction      string    `json:"recommended_action"`
}

