package metrics

import (
	"github.com/prometheus/client_golang/prometheus"
	"github.com/prometheus/client_golang/prometheus/promauto"
)

var (
	// EventsTotal counts total events by stage
	EventsTotal = promauto.NewCounterVec(
		prometheus.CounterOpts{
			Name: "pelican_events_total",
			Help: "Total number of progress events by stage",
		},
		[]string{"stage", "job_id"},
	)
	
	// RateLimitedEvents counts rate-limited events
	RateLimitedEvents = promauto.NewCounterVec(
		prometheus.CounterOpts{
			Name: "pelican_rate_limited_total",
			Help: "Total number of rate-limited events",
		},
		[]string{"job_id"},
	)
	
	// ActiveJobs tracks currently active jobs
	ActiveJobs = promauto.NewGauge(
		prometheus.GaugeOpts{
			Name: "pelican_active_jobs",
			Help: "Number of currently active genome sequencing jobs",
		},
	)
	
	// JobDuration tracks job completion times
	JobDuration = promauto.NewHistogramVec(
		prometheus.HistogramOpts{
			Name:    "pelican_job_duration_seconds",
			Help:    "Duration of genome sequencing jobs",
			Buckets: prometheus.DefBuckets,
		},
		[]string{"species", "status"},
	)
	
	// RecordsProcessed tracks total records processed
	RecordsProcessed = promauto.NewCounterVec(
		prometheus.CounterOpts{
			Name: "pelican_records_processed_total",
			Help: "Total number of gene records processed",
		},
		[]string{"job_id", "species", "type"}, // type: fetched or indexed
	)
)

// RecordEvent updates metrics based on a progress event
func RecordEvent(jobID, stage string, fetched, indexed int, rateLimited bool, species string) {
	EventsTotal.WithLabelValues(stage, jobID).Inc()
	
	if rateLimited {
		RateLimitedEvents.WithLabelValues(jobID).Inc()
	}
	
	// Update record counters
	RecordsProcessed.WithLabelValues(jobID, species, "fetched").Add(float64(fetched))
	RecordsProcessed.WithLabelValues(jobID, species, "indexed").Add(float64(indexed))
}

// JobStarted increments active jobs counter
func JobStarted() {
	ActiveJobs.Inc()
}

// JobCompleted decrements active jobs counter and records duration
func JobCompleted(species, status string, duration float64) {
	ActiveJobs.Dec()
	JobDuration.WithLabelValues(species, status).Observe(duration)
}

