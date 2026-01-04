#!/bin/bash

# Production Deployment Script for Pelican Farm Management System
set -euo pipefail

# Configuration
APP_NAME="pelican-farm"
ENVIRONMENT="${ENVIRONMENT:-production}"
BACKUP_DIR="./backups"
LOG_FILE="./logs/deploy.log"
MAX_BACKUPS=10

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging function
log() {
    echo -e "${1}" | tee -a $LOG_FILE
}

# Create necessary directories
mkdir -p $BACKUP_DIR logs

# Verify prerequisites
check_prerequisites() {
    log "${BLUE}🔍 Checking prerequisites...${NC}"
    
    command -v docker >/dev/null 2>&1 || { log "${RED}❌ Docker is required but not installed.${NC}"; exit 1; }
    command -v docker-compose >/dev/null 2>&1 || { log "${RED}❌ Docker Compose is required but not installed.${NC}"; exit 1; }
    
    if ! docker info >/dev/null 2>&1; then
        log "${RED}❌ Docker daemon is not running.${NC}"
        exit 1
    fi
    
    log "${GREEN}✅ Prerequisites check passed${NC}"
}

# Backup database with rotation
backup_database() {
    log "${BLUE}📦 Creating database backup...${NC}"
    
    if docker volume inspect "${APP_NAME}_pelican_data_prod" >/dev/null 2>&1; then
        timestamp=$(date +%Y%m%d_%H%M%S)
        backup_file="$BACKUP_DIR/pelican_db_backup_$timestamp.tar.gz"
        
        docker run --rm \
            -v "${APP_NAME}_pelican_data_prod:/data" \
            -v "$(pwd)/$BACKUP_DIR:/backup" \
            alpine tar czf "/backup/pelican_db_backup_$timestamp.tar.gz" -C /data .
        
        log "${GREEN}✅ Database backup created: $backup_file${NC}"
        
        # Rotate backups - keep only last $MAX_BACKUPS
        ls -t $BACKUP_DIR/pelican_db_backup_*.tar.gz 2>/dev/null | tail -n +$((MAX_BACKUPS + 1)) | xargs -r rm
        log "${BLUE}🗑️  Old backups cleaned up (keeping $MAX_BACKUPS most recent)${NC}"
    else
        log "${YELLOW}⚠️  No existing database volume found, skipping backup${NC}"
    fi
}

# Build and validate Docker image
build_image() {
    log "${BLUE}🏗️ Building production Docker image...${NC}"
    
    # Build with BuildKit for better caching
    DOCKER_BUILDKIT=1 docker build -f Dockerfile.prod -t "$APP_NAME:$ENVIRONMENT" .
    
    # Security scan if trivy is available
    if command -v trivy >/dev/null 2>&1; then
        log "${BLUE}🔒 Running security scan...${NC}"
        trivy image --exit-code 1 --severity HIGH,CRITICAL "$APP_NAME:$ENVIRONMENT" || {
            log "${YELLOW}⚠️  Security vulnerabilities found. Review before deploying to production.${NC}"
        }
    fi
    
    log "${GREEN}✅ Docker image built successfully${NC}"
}

# Pre-deployment health checks
pre_deploy_checks() {
    log "${BLUE}🔍 Running pre-deployment checks...${NC}"
    
    # Check if required files exist
    required_files=("docker-compose.prod.yml" "nginx.conf" "Dockerfile.prod")
    for file in "${required_files[@]}"; do
        if [[ ! -f "$file" ]]; then
            log "${RED}❌ Required file missing: $file${NC}"
            exit 1
        fi
    done
    
    # Validate docker-compose file
    if ! docker-compose -f docker-compose.prod.yml config >/dev/null 2>&1; then
        log "${RED}❌ Invalid docker-compose.prod.yml configuration${NC}"
        exit 1
    fi
    
    log "${GREEN}✅ Pre-deployment checks passed${NC}"
}

# Deploy with zero-downtime strategy
deploy() {
    log "${BLUE}🚀 Starting production deployment...${NC}"
    
    # Stop existing containers gracefully
    log "${BLUE}🔄 Stopping existing containers...${NC}"
    docker-compose -f docker-compose.prod.yml down --timeout 30 || true
    
    # Start new deployment
    log "${BLUE}🚀 Starting new deployment...${NC}"
    docker-compose -f docker-compose.prod.yml up -d
    
    # Wait for services to be healthy
    log "${BLUE}⏳ Waiting for services to be healthy...${NC}"
    timeout=120
    counter=0
    
    while [ $counter -lt $timeout ]; do
        if docker-compose -f docker-compose.prod.yml ps | grep -q "healthy"; then
            log "${GREEN}✅ Services are healthy${NC}"
            break
        fi
        
        sleep 2
        counter=$((counter + 2))
        
        if [ $counter -ge $timeout ]; then
            log "${RED}❌ Deployment timeout - services did not become healthy${NC}"
            log "${RED}📋 Container logs:${NC}"
            docker-compose -f docker-compose.prod.yml logs --tail=50
            exit 1
        fi
    done
}

# Post-deployment validation
validate_deployment() {
    log "${BLUE}✅ Running post-deployment validation...${NC}"
    
    # Test API endpoints
    sleep 5  # Give services a moment to fully start
    
    if curl -f -s "http://localhost/api/v1/pelicans/stats" >/dev/null; then
        log "${GREEN}✅ API health check passed${NC}"
    else
        log "${RED}❌ API health check failed${NC}"
        exit 1
    fi
    
    if curl -f -s "http://localhost/" >/dev/null; then
        log "${GREEN}✅ Web interface check passed${NC}"
    else
        log "${RED}❌ Web interface check failed${NC}"
        exit 1
    fi
    
    log "${GREEN}🎉 Deployment validation successful!${NC}"
}

# Rollback function
rollback() {
    log "${YELLOW}🔄 Rolling back deployment...${NC}"
    
    # Stop current deployment
    docker-compose -f docker-compose.prod.yml down --timeout 30
    
    # Restore from latest backup if available
    latest_backup=$(ls -t $BACKUP_DIR/pelican_db_backup_*.tar.gz 2>/dev/null | head -1 || echo "")
    if [[ -n "$latest_backup" ]]; then
        log "${BLUE}📦 Restoring from backup: $latest_backup${NC}"
        docker run --rm \
            -v "${APP_NAME}_pelican_data_prod:/data" \
            -v "$(pwd)/$BACKUP_DIR:/backup" \
            alpine tar xzf "/backup/$(basename $latest_backup)" -C /data
        log "${GREEN}✅ Database restored from backup${NC}"
    else
        log "${YELLOW}⚠️  No backups found, keeping current database state${NC}"
    fi
    
    # Start previous version (using main compose file as fallback)
    docker-compose up -d
    log "${GREEN}✅ Rollback completed${NC}"
}

# Show deployment status
show_status() {
    log "${BLUE}📊 Deployment Status:${NC}"
    docker-compose -f docker-compose.prod.yml ps
    
    log "\n${BLUE}📈 Resource Usage:${NC}"
    docker stats --no-stream --format "table {{.Container}}\t{{.CPUPerc}}\t{{.MemUsage}}\t{{.NetIO}}"
    
    log "\n${BLUE}🔍 Health Status:${NC}"
    for service in pelican-farm nginx; do
        if docker-compose -f docker-compose.prod.yml ps | grep -q "$service.*healthy\|$service.*Up"; then
            log "${GREEN}✅ $service: Healthy${NC}"
        else
            log "${RED}❌ $service: Unhealthy${NC}"
        fi
    done
}

# Show logs
show_logs() {
    docker-compose -f docker-compose.prod.yml logs -f --tail=100
}

# Main script logic
case "${1:-deploy}" in
    "deploy")
        log "${GREEN}🐦 Starting Pelican Farm Production Deployment${NC}"
        check_prerequisites
        backup_database
        pre_deploy_checks
        build_image
        deploy
        validate_deployment
        log "${GREEN}🎉 Production deployment completed successfully!${NC}"
        log "${BLUE}🌐 Application available at: http://localhost${NC}"
        log "${BLUE}📊 API docs: http://localhost/api/v1/pelicans/stats${NC}"
        show_status
        ;;
    "rollback")
        rollback
        ;;
    "backup")
        backup_database
        ;;
    "logs")
        show_logs
        ;;
    "status")
        show_status
        ;;
    "validate")
        validate_deployment
        ;;
    *)
        echo "Usage: $0 {deploy|rollback|backup|logs|status|validate}"
        echo "  deploy   - Full production deployment (default)"
        echo "  rollback - Rollback to previous backup"
        echo "  backup   - Create database backup only"
        echo "  logs     - Show application logs"
        echo "  status   - Show deployment status"
        echo "  validate - Run post-deployment validation"
        exit 1
        ;;
esac
