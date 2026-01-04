# 🚀 Pelican Farm Management System - Deployment Guide

This document provides comprehensive deployment instructions for the Pelican Farm Management System in various environments.

## 📋 Table of Contents

- [Prerequisites](#prerequisites)
- [Environment Configuration](#environment-configuration)
- [Development Deployment](#development-deployment)
- [Production Deployment](#production-deployment)
- [Database Migrations](#database-migrations)
- [Monitoring & Health Checks](#monitoring--health-checks)
- [Backup & Recovery](#backup--recovery)
- [Troubleshooting](#troubleshooting)

## 🔧 Prerequisites

### Required Software

- **Docker** 20.10+
- **Docker Compose** 2.0+
- **Git** 2.0+
- **SQLite3** (for local development)

### Optional Tools

- **jq** - JSON processing for health checks
- **trivy** - Security scanning for Docker images
- **curl** - API testing and health checks

### System Requirements

| Environment | RAM | CPU | Storage |
|-------------|-----|-----|---------|
| Development | 2GB | 1 Core | 10GB |
| Production  | 4GB | 2 Cores | 50GB |

## ⚙️ Environment Configuration

### 1. Copy Environment Template

```bash
cp .env.example .env
```

### 2. Configure Environment Variables

Edit `.env` file with your specific configuration:

```bash
# Basic Configuration
APP_NAME=pelican-farm
ENVIRONMENT=production
PORT=8080

# Database
DB_PATH=/app/data/pelican_farm.db

# Security (Generate secure random strings)
SESSION_SECRET=$(openssl rand -base64 32)
JWT_SECRET=$(openssl rand -base64 32)
```

### 3. Generate SSL Certificates (Production)

```bash
# Self-signed certificate (for testing)
mkdir -p ssl
openssl req -x509 -nodes -days 365 -newkey rsa:2048 \
  -keyout ssl/key.pem -out ssl/cert.pem

# For production, use Let's Encrypt or your certificate authority
```

## 🔨 Development Deployment

### Quick Start

```bash
# Clone repository
git clone <repository-url>
cd pelican-farm

# Start development environment
./deploy.sh
```

### Manual Development Setup

```bash
# Build and run locally
go mod tidy
go build -o server ./cmd/server
./server

# Or using Docker
docker-compose up -d
```

### Development URLs

- **Web Interface**: http://localhost:8080
- **API Base**: http://localhost:8080/api/v1
- **Health Check**: http://localhost:8080/api/v1/pelicans/stats

## 🏭 Production Deployment

### 1. Prepare Production Environment

```bash
# Create production directories
mkdir -p logs backups ssl

# Set environment
export ENVIRONMENT=production
```

### 2. Run Production Deployment

```bash
# Full production deployment
./scripts/prod-deploy.sh deploy

# Or step by step
./scripts/prod-deploy.sh backup
./scripts/prod-deploy.sh deploy
```

### 3. Production URLs

- **Web Interface**: http://your-domain.com
- **API Base**: http://your-domain.com/api/v1
- **Health Check**: http://your-domain.com/health

### 4. Post-Deployment Validation

```bash
# Run health checks
./scripts/health-check.sh full

# Check deployment status
./scripts/prod-deploy.sh status
```

## 🗄️ Database Migrations

### Run Migrations

```bash
# Apply all pending migrations
./scripts/migrate.sh up

# Check migration status
./scripts/migrate.sh status
```

### Migration Files

Migrations are stored in `migrations/` directory:

- `001_initial_schema.sql` - Creates pelicans and farms tables
- Add new migrations as `XXX_description.sql`

### Migration Best Practices

1. **Always backup** before running migrations
2. **Test migrations** in development first
3. **Use transactions** for data integrity
4. **Version control** all migration files

## 📊 Monitoring & Health Checks

### Built-in Health Checks

```bash
# Comprehensive health check
./scripts/health-check.sh full

# Quick health check
./scripts/health-check.sh quick

# Performance metrics
./scripts/health-check.sh performance
```

### Docker Health Checks

Containers include built-in health checks:

```bash
# Check container health
docker-compose ps

# View health check logs
docker-compose logs pelican-farm
```

### Monitoring Stack (Optional)

Enable monitoring with Prometheus:

```bash
# Start with monitoring
docker-compose --profile monitoring up -d

# Access Prometheus
open http://localhost:9090
```

## 💾 Backup & Recovery

### Automated Backups

Backups are created automatically during deployments:

```bash
# Manual backup
./scripts/prod-deploy.sh backup

# View backups
ls -la backups/
```

### Backup Schedule

Set up automated backups with cron:

```bash
# Add to crontab
0 2 * * * /path/to/pelican-farm/scripts/prod-deploy.sh backup
```

### Recovery

```bash
# Rollback to previous backup
./scripts/prod-deploy.sh rollback

# Manual database restore
docker run --rm -v pelican_data_prod:/data -v $(pwd)/backups:/backup alpine \
  tar xzf /backup/pelican_db_backup_TIMESTAMP.tar.gz -C /data
```

## 🐛 Troubleshooting

### Common Issues

#### Container Won't Start

```bash
# Check logs
docker-compose logs pelican-farm

# Check disk space
df -h

# Check Docker resources
docker system df
```

#### Database Issues

```bash
# Validate database
./scripts/migrate.sh validate

# Check database file permissions
ls -la pelican_farm.db

# Test database connection
sqlite3 pelican_farm.db ".tables"
```

#### API Not Responding

```bash
# Check service status
./scripts/health-check.sh api

# Test direct connection
curl -v http://localhost:8080/api/v1/pelicans/stats

# Check network connectivity
docker network ls
```

#### High Memory Usage

```bash
# Check container stats
docker stats --no-stream

# Check logs for memory leaks
docker-compose logs --tail=100

# Restart services
docker-compose restart
```

### Performance Optimization

#### Database Optimization

```bash
# Vacuum database
sqlite3 pelican_farm.db "VACUUM;"

# Analyze query performance
sqlite3 pelican_farm.db ".timer on" ".explain on"
```

#### Container Optimization

```bash
# Limit container resources
docker update --memory="512m" --cpus="0.5" pelican-farm-app

# Use production build
docker build -f Dockerfile.prod -t pelican-farm:prod .
```

### Logs and Debugging

#### View Logs

```bash
# Application logs
./scripts/prod-deploy.sh logs

# Specific service logs
docker-compose logs -f pelican-farm

# System logs
journalctl -u docker
```

#### Debug Mode

```bash
# Enable debug logging
export LOG_LEVEL=debug
export GIN_MODE=debug

# Restart with debug
docker-compose down && docker-compose up -d
```

## 🔐 Security Considerations

### Production Security

1. **Use HTTPS** in production
2. **Set strong secrets** in environment variables
3. **Enable rate limiting** via nginx
4. **Regular security updates** for base images
5. **Monitor access logs** for suspicious activity

### Security Scanning

```bash
# Scan Docker image for vulnerabilities
trivy image pelican-farm:latest

# Scan filesystem
trivy fs .
```

## 📈 Scaling

### Horizontal Scaling

```bash
# Scale application containers
docker-compose up -d --scale pelican-farm=3

# Load balancer configuration
# Update nginx.conf upstream section
```

### Database Scaling

For production scaling considerations:

1. **Read replicas** for read-heavy workloads
2. **Connection pooling** for high concurrency
3. **Database clustering** for high availability

## 🚀 CI/CD Integration

### GitHub Actions Example

```yaml
name: Deploy Pelican Farm
on:
  push:
    branches: [main]

jobs:
  deploy:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Deploy to production
        run: ./scripts/prod-deploy.sh deploy
```

### Health Check Integration

```bash
# Post-deployment health check
./scripts/health-check.sh full || exit 1
```

## 📞 Support

For deployment issues:

1. Check this documentation
2. Review logs: `./scripts/prod-deploy.sh logs`
3. Run health checks: `./scripts/health-check.sh full`
4. Check [troubleshooting section](#troubleshooting)

---

Built with ❤️ for the Pelican Farm Management System
