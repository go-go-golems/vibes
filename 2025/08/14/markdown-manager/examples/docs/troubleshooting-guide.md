---
title: "System Troubleshooting Guide"
description: "Comprehensive troubleshooting guide for common system issues and their solutions"
tags: ["troubleshooting", "support", "debugging", "operations", "maintenance"]
category: "documentation"
created: 2024-07-15T09:30:00Z
modified: 2024-08-13T14:20:00Z
project: "system-operations"
repository: "https://github.com/company/ops-docs"
branch: "main"
status: "final"
priority: "high"
version: "3.2"
author: "Operations Team"
contributors: ["DevOps Team", "Support Team", "Engineering Team"]
language: "markdown"
format: "troubleshooting"
template: "operations"
related_files: ["monitoring-setup.md", "incident-response.md", "system-architecture.md"]
dependencies: ["monitoring-tools.md", "log-aggregation.md"]
references: ["https://prometheus.io/", "https://grafana.com/", "https://www.elastic.co/"]
custom:
  severity_levels: ["P0", "P1", "P2", "P3"]
  escalation_time: "30 minutes"
  on_call_rotation: "weekly"
---

# System Troubleshooting Guide

## Overview

This guide provides step-by-step troubleshooting procedures for common system issues. It's designed for on-call engineers, support staff, and anyone responsible for maintaining system reliability.

## Severity Levels

| Level | Description | Response Time | Escalation |
|-------|-------------|---------------|------------|
| P0 | System down, data loss | 15 minutes | Immediate |
| P1 | Major functionality impaired | 1 hour | 30 minutes |
| P2 | Minor functionality issues | 4 hours | 2 hours |
| P3 | Cosmetic or enhancement | 24 hours | Next business day |

## Quick Reference

### Emergency Contacts
- **On-call Engineer**: +1-555-0123
- **Engineering Manager**: +1-555-0124
- **DevOps Lead**: +1-555-0125
- **CTO**: +1-555-0126

### Critical System URLs
- **Monitoring Dashboard**: https://monitoring.company.com
- **Log Aggregation**: https://logs.company.com
- **Status Page**: https://status.company.com
- **Runbook Repository**: https://runbooks.company.com

## Common Issues

### 1. Application Not Responding (P0)

#### Symptoms
- HTTP 5xx errors
- Timeouts on API requests
- Users unable to access the application
- Health check failures

#### Initial Assessment (2 minutes)
```bash
# Check application status
curl -I https://api.company.com/health

# Check load balancer status
kubectl get pods -n production

# Check recent deployments
kubectl rollout history deployment/api -n production
```

#### Troubleshooting Steps

**Step 1: Check Application Pods**
```bash
# List all pods in production namespace
kubectl get pods -n production

# Check pod logs for errors
kubectl logs -f deployment/api -n production --tail=100

# Describe problematic pods
kubectl describe pod <pod-name> -n production
```

**Step 2: Check Resource Usage**
```bash
# Check CPU and memory usage
kubectl top pods -n production

# Check node resources
kubectl top nodes

# Check for resource limits
kubectl describe deployment api -n production
```

**Step 3: Check Database Connectivity**
```bash
# Test database connection
kubectl exec -it deployment/api -n production -- \
  psql -h db.company.com -U app_user -d production -c "SELECT 1;"

# Check database performance
kubectl exec -it deployment/api -n production -- \
  psql -h db.company.com -U app_user -d production -c "
    SELECT query, calls, mean_time, total_time 
    FROM pg_stat_statements 
    ORDER BY total_time DESC 
    LIMIT 10;"
```

**Step 4: Check External Dependencies**
```bash
# Test Redis connection
kubectl exec -it deployment/api -n production -- \
  redis-cli -h redis.company.com ping

# Test third-party APIs
curl -I https://api.external-service.com/status
```

#### Resolution Actions

**Option 1: Restart Application**
```bash
# Rolling restart
kubectl rollout restart deployment/api -n production

# Monitor restart progress
kubectl rollout status deployment/api -n production
```

**Option 2: Scale Up Resources**
```bash
# Increase replica count
kubectl scale deployment api --replicas=10 -n production

# Increase resource limits
kubectl patch deployment api -n production -p '
{
  "spec": {
    "template": {
      "spec": {
        "containers": [
          {
            "name": "api",
            "resources": {
              "limits": {
                "cpu": "2000m",
                "memory": "4Gi"
              }
            }
          }
        ]
      }
    }
  }
}'
```

**Option 3: Rollback Deployment**
```bash
# Check rollout history
kubectl rollout history deployment/api -n production

# Rollback to previous version
kubectl rollout undo deployment/api -n production

# Rollback to specific revision
kubectl rollout undo deployment/api --to-revision=2 -n production
```

### 2. High Response Times (P1)

#### Symptoms
- API response times >2 seconds
- Database query timeouts
- User complaints about slow performance
- High CPU/memory usage

#### Troubleshooting Steps

**Step 1: Identify Bottlenecks**
```bash
# Check application metrics
curl -s http://api.company.com/metrics | grep response_time

# Check database slow queries
kubectl exec -it postgres-0 -n production -- \
  psql -U postgres -d production -c "
    SELECT query, calls, mean_time, total_time 
    FROM pg_stat_statements 
    WHERE mean_time > 1000 
    ORDER BY mean_time DESC 
    LIMIT 10;"
```

**Step 2: Analyze Resource Usage**
```bash
# Check CPU usage by pod
kubectl top pods -n production --sort-by=cpu

# Check memory usage by pod
kubectl top pods -n production --sort-by=memory

# Check disk I/O
kubectl exec -it deployment/api -n production -- iostat -x 1 5
```

**Step 3: Check Cache Performance**
```bash
# Redis cache hit ratio
kubectl exec -it redis-0 -n production -- \
  redis-cli info stats | grep keyspace

# Application cache metrics
curl -s http://api.company.com/metrics | grep cache_hit_ratio
```

#### Resolution Actions

**Option 1: Scale Horizontally**
```bash
# Increase pod replicas
kubectl scale deployment api --replicas=8 -n production

# Enable horizontal pod autoscaler
kubectl autoscale deployment api --cpu-percent=70 --min=3 --max=15 -n production
```

**Option 2: Optimize Database**
```bash
# Analyze and vacuum database
kubectl exec -it postgres-0 -n production -- \
  psql -U postgres -d production -c "ANALYZE; VACUUM;"

# Check for missing indexes
kubectl exec -it postgres-0 -n production -- \
  psql -U postgres -d production -f /scripts/check_missing_indexes.sql
```

**Option 3: Clear Cache**
```bash
# Clear Redis cache
kubectl exec -it redis-0 -n production -- redis-cli FLUSHDB

# Clear application cache
curl -X POST http://api.company.com/admin/cache/clear
```

### 3. Database Connection Issues (P1)

#### Symptoms
- Connection pool exhausted
- Database timeouts
- "Too many connections" errors
- Application unable to connect to database

#### Troubleshooting Steps

**Step 1: Check Connection Pool**
```bash
# Check active connections
kubectl exec -it postgres-0 -n production -- \
  psql -U postgres -c "
    SELECT count(*) as active_connections, 
           state, 
           application_name 
    FROM pg_stat_activity 
    GROUP BY state, application_name;"

# Check connection limits
kubectl exec -it postgres-0 -n production -- \
  psql -U postgres -c "SHOW max_connections;"
```

**Step 2: Identify Long-Running Queries**
```bash
# Find long-running queries
kubectl exec -it postgres-0 -n production -- \
  psql -U postgres -c "
    SELECT pid, 
           now() - pg_stat_activity.query_start AS duration, 
           query, 
           state
    FROM pg_stat_activity 
    WHERE (now() - pg_stat_activity.query_start) > interval '5 minutes';"
```

**Step 3: Check Database Locks**
```bash
# Check for blocking queries
kubectl exec -it postgres-0 -n production -- \
  psql -U postgres -c "
    SELECT blocked_locks.pid AS blocked_pid,
           blocked_activity.usename AS blocked_user,
           blocking_locks.pid AS blocking_pid,
           blocking_activity.usename AS blocking_user,
           blocked_activity.query AS blocked_statement,
           blocking_activity.query AS current_statement_in_blocking_process
    FROM pg_catalog.pg_locks blocked_locks
    JOIN pg_catalog.pg_stat_activity blocked_activity ON blocked_activity.pid = blocked_locks.pid
    JOIN pg_catalog.pg_locks blocking_locks ON blocking_locks.locktype = blocked_locks.locktype
    JOIN pg_catalog.pg_stat_activity blocking_activity ON blocking_activity.pid = blocking_locks.pid
    WHERE NOT blocked_locks.granted;"
```

#### Resolution Actions

**Option 1: Kill Long-Running Queries**
```bash
# Kill specific query by PID
kubectl exec -it postgres-0 -n production -- \
  psql -U postgres -c "SELECT pg_terminate_backend(<PID>);"

# Kill all idle connections
kubectl exec -it postgres-0 -n production -- \
  psql -U postgres -c "
    SELECT pg_terminate_backend(pid) 
    FROM pg_stat_activity 
    WHERE state = 'idle' 
    AND state_change < current_timestamp - INTERVAL '1 hour';"
```

**Option 2: Increase Connection Limits**
```bash
# Temporarily increase max_connections
kubectl exec -it postgres-0 -n production -- \
  psql -U postgres -c "ALTER SYSTEM SET max_connections = 200;"

# Reload configuration
kubectl exec -it postgres-0 -n production -- \
  psql -U postgres -c "SELECT pg_reload_conf();"
```

**Option 3: Restart Database Connection Pool**
```bash
# Restart PgBouncer
kubectl rollout restart deployment/pgbouncer -n production

# Or restart application to reset connection pool
kubectl rollout restart deployment/api -n production
```

### 4. Disk Space Issues (P1)

#### Symptoms
- "No space left on device" errors
- Application unable to write logs
- Database write failures
- Pod evictions due to disk pressure

#### Troubleshooting Steps

**Step 1: Check Disk Usage**
```bash
# Check node disk usage
kubectl get nodes -o custom-columns=NAME:.metadata.name,DISK:.status.allocatable.ephemeral-storage

# Check pod disk usage
kubectl exec -it deployment/api -n production -- df -h

# Check specific directories
kubectl exec -it deployment/api -n production -- du -sh /var/log /tmp /app
```

**Step 2: Identify Large Files**
```bash
# Find largest files
kubectl exec -it deployment/api -n production -- \
  find /var/log -type f -size +100M -exec ls -lh {} \;

# Find largest directories
kubectl exec -it deployment/api -n production -- \
  du -h /var/log | sort -rh | head -10
```

#### Resolution Actions

**Option 1: Clean Up Logs**
```bash
# Rotate logs
kubectl exec -it deployment/api -n production -- logrotate -f /etc/logrotate.conf

# Clean old logs
kubectl exec -it deployment/api -n production -- \
  find /var/log -name "*.log" -mtime +7 -delete

# Truncate large log files
kubectl exec -it deployment/api -n production -- \
  truncate -s 0 /var/log/application.log
```

**Option 2: Clean Temporary Files**
```bash
# Clean temp directories
kubectl exec -it deployment/api -n production -- rm -rf /tmp/*

# Clean application cache
kubectl exec -it deployment/api -n production -- rm -rf /app/cache/*
```

**Option 3: Increase Disk Space**
```bash
# Scale up persistent volume (if supported)
kubectl patch pvc data-postgres-0 -n production -p '{"spec":{"resources":{"requests":{"storage":"200Gi"}}}}'

# Add additional storage
kubectl apply -f - <<EOF
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  name: additional-storage
  namespace: production
spec:
  accessModes:
    - ReadWriteOnce
  resources:
    requests:
      storage: 100Gi
EOF
```

### 5. Memory Leaks (P2)

#### Symptoms
- Gradually increasing memory usage
- Out of memory (OOM) kills
- Application becoming unresponsive
- Garbage collection taking too long

#### Troubleshooting Steps

**Step 1: Monitor Memory Usage**
```bash
# Check current memory usage
kubectl top pods -n production --sort-by=memory

# Check memory limits and requests
kubectl describe deployment api -n production | grep -A 5 -B 5 memory

# Check for OOM kills
kubectl get events -n production | grep OOMKilled
```

**Step 2: Analyze Memory Patterns**
```bash
# Get heap dump (for Java applications)
kubectl exec -it deployment/api -n production -- \
  jcmd <PID> GC.run_finalization

# Check garbage collection logs
kubectl logs deployment/api -n production | grep -i "gc\|garbage"

# Monitor memory over time
kubectl exec -it deployment/api -n production -- \
  ps aux | grep java | awk '{print $6}' | head -1
```

#### Resolution Actions

**Option 1: Restart Application**
```bash
# Rolling restart to clear memory
kubectl rollout restart deployment/api -n production

# Monitor memory after restart
watch kubectl top pods -n production
```

**Option 2: Adjust Memory Limits**
```bash
# Increase memory limits
kubectl patch deployment api -n production -p '
{
  "spec": {
    "template": {
      "spec": {
        "containers": [
          {
            "name": "api",
            "resources": {
              "limits": {
                "memory": "8Gi"
              },
              "requests": {
                "memory": "4Gi"
              }
            }
          }
        ]
      }
    }
  }
}'
```

**Option 3: Enable Memory Profiling**
```bash
# Enable profiling endpoint
curl -X POST http://api.company.com/admin/profiling/enable

# Generate memory profile
curl http://api.company.com/debug/pprof/heap > heap.prof
```

## Monitoring and Alerting

### Key Metrics to Monitor

1. **Application Metrics**
   - Response time (95th percentile)
   - Error rate
   - Throughput (requests per second)
   - Active connections

2. **Infrastructure Metrics**
   - CPU usage
   - Memory usage
   - Disk usage
   - Network I/O

3. **Database Metrics**
   - Query response time
   - Connection count
   - Lock waits
   - Replication lag

### Alert Thresholds

```yaml
# Prometheus alerting rules
groups:
  - name: application
    rules:
      - alert: HighErrorRate
        expr: rate(http_requests_total{status=~"5.."}[5m]) > 0.1
        for: 2m
        labels:
          severity: critical
        annotations:
          summary: "High error rate detected"

      - alert: HighResponseTime
        expr: histogram_quantile(0.95, rate(http_request_duration_seconds_bucket[5m])) > 2
        for: 5m
        labels:
          severity: warning
        annotations:
          summary: "High response time detected"

      - alert: DatabaseConnectionsHigh
        expr: pg_stat_activity_count > 80
        for: 2m
        labels:
          severity: warning
        annotations:
          summary: "Database connections approaching limit"
```

## Escalation Procedures

### Level 1: On-Call Engineer
- **Response Time**: 15 minutes
- **Actions**: Initial triage, basic troubleshooting
- **Escalation**: If not resolved in 30 minutes

### Level 2: Engineering Manager
- **Response Time**: 30 minutes
- **Actions**: Advanced troubleshooting, resource allocation
- **Escalation**: If not resolved in 1 hour

### Level 3: CTO/VP Engineering
- **Response Time**: 1 hour
- **Actions**: Executive decision making, external communication
- **Escalation**: CEO notification for business-critical issues

## Post-Incident Actions

### Immediate (Within 1 hour)
1. Confirm issue resolution
2. Update status page
3. Notify stakeholders
4. Document timeline

### Short-term (Within 24 hours)
1. Conduct post-mortem meeting
2. Identify root cause
3. Create action items
4. Update runbooks

### Long-term (Within 1 week)
1. Implement preventive measures
2. Update monitoring/alerting
3. Conduct team retrospective
4. Share learnings organization-wide

## Tools and Resources

### Monitoring Tools
- **Prometheus**: Metrics collection
- **Grafana**: Visualization and dashboards
- **AlertManager**: Alert routing and management
- **Jaeger**: Distributed tracing

### Log Management
- **ELK Stack**: Elasticsearch, Logstash, Kibana
- **Fluentd**: Log collection and forwarding
- **Loki**: Log aggregation system

### Debugging Tools
- **kubectl**: Kubernetes CLI
- **docker**: Container debugging
- **curl**: HTTP testing
- **jq**: JSON processing

### Documentation
- **Runbooks**: https://runbooks.company.com
- **Architecture Docs**: https://docs.company.com/architecture
- **API Documentation**: https://api-docs.company.com
- **Incident Reports**: https://incidents.company.com

## Best Practices

### Prevention
1. **Proactive Monitoring**: Set up comprehensive alerts
2. **Regular Testing**: Conduct chaos engineering exercises
3. **Capacity Planning**: Monitor trends and plan for growth
4. **Documentation**: Keep runbooks up to date

### During Incidents
1. **Stay Calm**: Follow established procedures
2. **Communicate**: Keep stakeholders informed
3. **Document**: Record all actions taken
4. **Focus**: Prioritize restoration over root cause analysis

### After Incidents
1. **Learn**: Conduct blameless post-mortems
2. **Improve**: Implement preventive measures
3. **Share**: Distribute learnings across teams
4. **Practice**: Update and test procedures regularly

Remember: The goal is to restore service quickly and safely. When in doubt, escalate early and often.

