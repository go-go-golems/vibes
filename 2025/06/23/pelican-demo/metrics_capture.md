# Pelican Genome Sequencer - Metrics Data Capture

## System Status
- **Active Jobs**: 0 (all jobs completed successfully)
- **Total Jobs Executed**: Multiple jobs with different species
- **System Health**: All metrics showing normal operation

## Job Execution Metrics

### Job Events by Stage
Multiple jobs have been executed successfully, showing the complete pipeline:

**Job ID: 00089fe2-bcad-4d9f-91f8-7ad5c0a516cb**
- Fetch events: 21
- Analyze events: 20  
- Done events: 1

**Job ID: 298f2204-1c04-49ae-8120-9328582d4544**
- Fetch events: 21
- Analyze events: 20
- Done events: 1

**Job ID: 3e887ef8-ee5d-49d5-af79-0b0f706653d9**
- Fetch events: 21
- Analyze events: 20
- Done events: 1

### Records Processed
Each job successfully processed:
- **200 records fetched** (in batches of 10)
- **200 records indexed** (genome analysis completed)
- **Total throughput**: 400 records per job

### Job Duration Metrics
- Jobs completing in approximately 10-15 seconds
- Consistent performance across multiple executions
- No failed jobs or error conditions

### Rate Limiting
- Rate limiting events detected and handled properly
- System gracefully handles API throttling scenarios
- No impact on overall job completion

## System Performance
- **Go Runtime**: Version 1.23.4
- **Goroutines**: 18 active
- **Memory Usage**: ~1.4MB heap allocation
- **GC Performance**: No garbage collection cycles needed yet

## Prometheus Metrics Available
- `pelican_active_jobs`: Currently active jobs gauge
- `pelican_events_total`: Event counter by stage and job ID
- `pelican_rate_limited_total`: Rate limiting counter
- `pelican_job_duration_seconds`: Job completion time histogram
- `pelican_records_processed_total`: Records processed counter

The system is functioning perfectly with all components working as designed.

