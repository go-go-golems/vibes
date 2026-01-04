#!/bin/bash

# Pelican Farm Management System Deployment Script
set -euo pipefail

echo "🐦 Deploying Pelican Farm Management System..."

# Configuration
APP_NAME="pelican-farm"
ENVIRONMENT="${ENVIRONMENT:-development}"
DOCKER_IMAGE="$APP_NAME:$ENVIRONMENT"
BACKUP_DIR="./backups"
LOG_FILE="./logs/deploy.log"

# Colors for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

# Create necessary directories
mkdir -p $BACKUP_DIR logs

# Logging function
log() {
    echo -e "${1}" | tee -a $LOG_FILE
}

# Function to backup database
backup_database() {
    if docker volume inspect pelican_data >/dev/null 2>&1; then
        echo "📦 Creating database backup..."
        timestamp=$(date +%Y%m%d_%H%M%S)
        docker run --rm -v pelican_data:/data -v $(pwd)/$BACKUP_DIR:/backup alpine \
            tar czf "/backup/pelican_db_backup_$timestamp.tar.gz" -C /data .
        echo "✅ Database backup created: $BACKUP_DIR/pelican_db_backup_$timestamp.tar.gz"
    fi
}

# Function to build and deploy
deploy() {
    echo "🏗️ Building Docker image..."
    docker build -t $DOCKER_IMAGE .
    
    echo "🔄 Stopping existing containers..."
    docker-compose down || true
    
    echo "🚀 Starting new deployment..."
    docker-compose up -d
    
    echo "⏳ Waiting for health check..."
    sleep 10
    
    # Check if service is healthy
    if docker-compose ps | grep -q "healthy\|Up"; then
        echo "✅ Deployment successful!"
        echo "🌐 Application available at: http://localhost:8080"
        echo "📊 API docs: http://localhost:8080/api/v1/pelicans/stats"
    else
        echo "❌ Deployment failed - checking logs..."
        docker-compose logs
        exit 1
    fi
}

# Function to rollback
rollback() {
    echo "🔄 Rolling back deployment..."
    if [ -f "$BACKUP_DIR/pelican_db_backup_*.tar.gz" ]; then
        latest_backup=$(ls -t $BACKUP_DIR/pelican_db_backup_*.tar.gz | head -1)
        echo "📦 Restoring from backup: $latest_backup"
        docker run --rm -v pelican_data:/data -v $(pwd)/$BACKUP_DIR:/backup alpine \
            tar xzf "/backup/$(basename $latest_backup)" -C /data
    fi
    docker-compose down
    docker-compose up -d
}

# Parse command line arguments
case "${1:-deploy}" in
    "deploy")
        backup_database
        deploy
        ;;
    "rollback")
        rollback
        ;;
    "backup")
        backup_database
        ;;
    "logs")
        docker-compose logs -f
        ;;
    "status")
        docker-compose ps
        ;;
    *)
        echo "Usage: $0 {deploy|rollback|backup|logs|status}"
        echo "  deploy   - Deploy the application (default)"
        echo "  rollback - Rollback to previous backup"
        echo "  backup   - Create database backup only"
        echo "  logs     - Show application logs"
        echo "  status   - Show container status"
        exit 1
        ;;
esac
