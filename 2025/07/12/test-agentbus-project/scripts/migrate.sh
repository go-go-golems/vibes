#!/bin/bash

# Database Migration Script for Pelican Farm Management System
set -euo pipefail

# Configuration
DB_PATH="${DB_PATH:-./pelican_farm.db}"
MIGRATIONS_DIR="./migrations"
LOG_FILE="./logs/migration.log"

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m'

# Logging function
log() {
    echo -e "${1}" | tee -a $LOG_FILE
}

# Create logs directory
mkdir -p logs

# Check if SQLite is available
if ! command -v sqlite3 >/dev/null 2>&1; then
    log "${RED}❌ SQLite3 is required but not installed.${NC}"
    exit 1
fi

# Create migrations tracking table
create_migrations_table() {
    log "${BLUE}📋 Creating migrations tracking table...${NC}"
    sqlite3 "$DB_PATH" << 'EOF'
CREATE TABLE IF NOT EXISTS schema_migrations (
    version TEXT PRIMARY KEY,
    applied_at DATETIME DEFAULT CURRENT_TIMESTAMP
);
EOF
    log "${GREEN}✅ Migrations table ready${NC}"
}

# Get applied migrations
get_applied_migrations() {
    sqlite3 "$DB_PATH" "SELECT version FROM schema_migrations ORDER BY version;" 2>/dev/null || echo ""
}

# Get available migrations
get_available_migrations() {
    if [[ -d "$MIGRATIONS_DIR" ]]; then
        find "$MIGRATIONS_DIR" -name "*.sql" -type f | sort | xargs -I {} basename {} .sql
    else
        echo ""
    fi
}

# Apply a single migration
apply_migration() {
    local migration_file="$1"
    local version=$(basename "$migration_file" .sql)
    
    log "${BLUE}🔄 Applying migration: $version${NC}"
    
    # Start transaction and apply migration
    sqlite3 "$DB_PATH" << EOF
BEGIN TRANSACTION;
.read $migration_file
INSERT INTO schema_migrations (version) VALUES ('$version');
COMMIT;
EOF
    
    if [[ $? -eq 0 ]]; then
        log "${GREEN}✅ Migration $version applied successfully${NC}"
    else
        log "${RED}❌ Migration $version failed${NC}"
        exit 1
    fi
}

# Run all pending migrations
migrate_up() {
    log "${BLUE}🚀 Starting database migration...${NC}"
    
    create_migrations_table
    
    applied_migrations=$(get_applied_migrations)
    available_migrations=$(get_available_migrations)
    
    if [[ -z "$available_migrations" ]]; then
        log "${YELLOW}⚠️  No migration files found in $MIGRATIONS_DIR${NC}"
        return
    fi
    
    pending_count=0
    for migration in $available_migrations; do
        if ! echo "$applied_migrations" | grep -q "^$migration$"; then
            migration_file="$MIGRATIONS_DIR/$migration.sql"
            apply_migration "$migration_file"
            ((pending_count++))
        fi
    done
    
    if [[ $pending_count -eq 0 ]]; then
        log "${GREEN}✅ Database is already up to date${NC}"
    else
        log "${GREEN}🎉 Applied $pending_count migration(s) successfully${NC}"
    fi
}

# Show migration status
migration_status() {
    log "${BLUE}📊 Migration Status:${NC}"
    
    if [[ ! -f "$DB_PATH" ]]; then
        log "${YELLOW}⚠️  Database file does not exist: $DB_PATH${NC}"
        return
    fi
    
    create_migrations_table
    
    applied_migrations=$(get_applied_migrations)
    available_migrations=$(get_available_migrations)
    
    log "${BLUE}Applied migrations:${NC}"
    if [[ -n "$applied_migrations" ]]; then
        while IFS= read -r migration; do
            applied_at=$(sqlite3 "$DB_PATH" "SELECT applied_at FROM schema_migrations WHERE version='$migration';")
            log "  ${GREEN}✅ $migration${NC} (applied: $applied_at)"
        done <<< "$applied_migrations"
    else
        log "  ${YELLOW}No migrations applied yet${NC}"
    fi
    
    log "\n${BLUE}Available migrations:${NC}"
    if [[ -n "$available_migrations" ]]; then
        for migration in $available_migrations; do
            if echo "$applied_migrations" | grep -q "^$migration$"; then
                log "  ${GREEN}✅ $migration${NC} (applied)"
            else
                log "  ${YELLOW}⏳ $migration${NC} (pending)"
            fi
        done
    else
        log "  ${YELLOW}No migration files found${NC}"
    fi
}

# Create backup before migration
backup_database() {
    if [[ -f "$DB_PATH" ]]; then
        backup_file="$DB_PATH.backup.$(date +%Y%m%d_%H%M%S)"
        log "${BLUE}📦 Creating database backup: $backup_file${NC}"
        cp "$DB_PATH" "$backup_file"
        log "${GREEN}✅ Backup created${NC}"
    else
        log "${YELLOW}⚠️  No database file to backup${NC}"
    fi
}

# Validate database integrity
validate_database() {
    log "${BLUE}🔍 Validating database integrity...${NC}"
    
    if [[ ! -f "$DB_PATH" ]]; then
        log "${RED}❌ Database file does not exist: $DB_PATH${NC}"
        exit 1
    fi
    
    # Check database integrity
    integrity_check=$(sqlite3 "$DB_PATH" "PRAGMA integrity_check;")
    if [[ "$integrity_check" == "ok" ]]; then
        log "${GREEN}✅ Database integrity check passed${NC}"
    else
        log "${RED}❌ Database integrity check failed: $integrity_check${NC}"
        exit 1
    fi
    
    # Check if required tables exist
    tables=$(sqlite3 "$DB_PATH" ".tables")
    required_tables=("pelicans" "farms" "schema_migrations")
    
    for table in "${required_tables[@]}"; do
        if echo "$tables" | grep -q "\b$table\b"; then
            log "${GREEN}✅ Table '$table' exists${NC}"
        else
            log "${YELLOW}⚠️  Table '$table' missing${NC}"
        fi
    done
}

# Main script logic
case "${1:-up}" in
    "up")
        backup_database
        migrate_up
        validate_database
        ;;
    "status")
        migration_status
        ;;
    "validate")
        validate_database
        ;;
    "backup")
        backup_database
        ;;
    *)
        echo "Usage: $0 {up|status|validate|backup}"
        echo "  up       - Apply pending migrations (default)"
        echo "  status   - Show migration status"
        echo "  validate - Validate database integrity"
        echo "  backup   - Create database backup"
        exit 1
        ;;
esac
