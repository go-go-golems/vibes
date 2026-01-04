
# Examples and Use Cases

This document provides comprehensive examples of using the Goat Farm Management System for various real-world scenarios.

## Complete Farm Setup Example

### Initial Setup
```bash
# Initialize the system
make init-db

# Set up environment
export GOAT_FARM_COMMIT_NAME="John Farmer"
export GOAT_FARM_COMMIT_EMAIL="john@example-farm.com"
```

### Adding Initial Herd
```bash
# Add breeding buck
goat-manager goat --tag-id "B001" --name "Thunder" --breed "alpine" --gender "male" --birth-date "2022-04-10" --status "active" --weight 85.0 --color "brown and white"

# Add does
goat-manager goat --tag-id "D001" --name "Bella" --breed "alpine" --gender "female" --birth-date "2022-06-15" --status "lactating" --weight 65.5 --color "white with brown spots"

goat-manager goat --tag-id "D002" --name "Luna" --breed "alpine" --gender "female" --birth-date "2022-07-20" --status "lactating" --weight 62.0 --color "solid brown"

goat-manager goat --tag-id "D003" --name "Star" --breed "nubian" --gender "female" --birth-date "2022-05-30" --status "dry" --weight 70.0 --color "black and tan"

# Commit initial herd
goat-manager version --action "commit" --message "Initial herd setup - 1 buck, 3 does"
```

## Daily Operations

### Morning Routine
```bash
#!/bin/bash
# morning_routine.sh

DATE=$(date +%Y-%m-%d)
TIME=$(date +%H:%M)

echo "Starting morning routine for $DATE at $TIME"

# Morning milking for lactating does
goat-manager milk --goat-tag "D001" --volume 3.2 --session "morning" --quality-grade "A" --fat-content 3.8 --protein-content 3.2 --milked-by "John"

goat-manager milk --goat-tag "D002" --volume 2.8 --session "morning" --quality-grade "A" --fat-content 3.6 --protein-content 3.1 --milked-by "John"

# Morning feeding
goat-manager feed --feed-type "hay" --feed-name "Alfalfa" --quantity 20 --unit "kg" --feeding-method "group" --fed-by "John" --cost-per-unit 0.50

goat-manager feed --feed-type "grain" --feed-name "Dairy Pellets" --quantity 8 --unit "kg" --feeding-method "group" --fed-by "John" --cost-per-unit 0.75

# Log morning operations
goat-manager farm --operation-type "milking" --description "Morning milking completed - 2 does" --performed-by "John" --quantity 6.0 --unit "liters"

echo "Morning routine completed"
```

### Evening Routine
```bash
#!/bin/bash
# evening_routine.sh

DATE=$(date +%Y-%m-%d)

# Evening milking
goat-manager milk --goat-tag "D001" --volume 2.9 --session "evening" --quality-grade "A" --milked-by "Sarah"

goat-manager milk --goat-tag "D002" --volume 2.5 --session "evening" --quality-grade "B" --milked-by "Sarah"

# Evening feeding
goat-manager feed --feed-type "hay" --quantity 15 --unit "kg" --feeding-method "group" --fed-by "Sarah"

# Daily commit
goat-manager version --action "commit" --message "Daily records - $DATE"

echo "Evening routine completed and committed"
```

## Health Management

### Vaccination Schedule
```bash
# Annual CDT vaccination for all goats
goat-manager health --goat-tag "B001" --record-type "vaccination" --description "CDT (Clostridium perfringens types C & D, tetanus)" --veterinarian "Dr. Smith" --medication "CDT vaccine" --dosage "2ml subcutaneous" --cost 15.00 --next-due-date "2025-03-01"

goat-manager health --goat-tag "D001" --record-type "vaccination" --description "CDT vaccination" --veterinarian "Dr. Smith" --medication "CDT vaccine" --dosage "2ml subcutaneous" --cost 15.00 --next-due-date "2025-03-01"

goat-manager health --goat-tag "D002" --record-type "vaccination" --description "CDT vaccination" --veterinarian "Dr. Smith" --medication "CDT vaccine" --dosage "2ml subcutaneous" --cost 15.00 --next-due-date "2025-03-01"

goat-manager health --goat-tag "D003" --record-type "vaccination" --description "CDT vaccination" --veterinarian "Dr. Smith" --medication "CDT vaccine" --dosage "2ml subcutaneous" --cost 15.00 --next-due-date "2025-03-01"

# Commit vaccination records
goat-manager version --action "commit" --message "Annual CDT vaccinations completed"
```

### Handling Illness
```bash
# Goat D002 shows signs of illness
goat-manager goat --tag-id "D002" --status "sick"

# Record initial symptoms
goat-manager health --goat-tag "D002" --record-type "illness" --description "Lethargy, reduced appetite, slight nasal discharge" --temperature 39.2 --weight 61.5 --notes "Isolated from herd as precaution"

# Veterinary consultation
goat-manager health --goat-tag "D002" --record-type "treatment" --description "Veterinary examination - mild respiratory infection" --veterinarian "Dr. Johnson" --medication "Penicillin" --dosage "3ml IM daily for 5 days" --cost 45.00

# Follow-up treatments
goat-manager health --goat-tag "D002" --record-type "treatment" --description "Day 2 - Penicillin injection, slight improvement" --medication "Penicillin" --dosage "3ml IM" --temperature 38.8

goat-manager health --goat-tag "D002" --record-type "treatment" --description "Day 3 - Penicillin injection, eating normally" --medication "Penicillin" --dosage "3ml IM" --temperature 38.5

# Recovery
goat-manager health --goat-tag "D002" --record-type "checkup" --description "Full recovery - normal temperature, good appetite" --temperature 38.3 --weight 62.2

goat-manager goat --tag-id "D002" --status "lactating"

# Commit health management
goat-manager version --action "commit" --message "D002 illness treatment and recovery"
```

## Breeding Management

### Breeding Season Setup
```bash
# Create branch for breeding season
goat-manager version --action "branch" --branch "breeding-season-2024"
goat-manager version --action "switch" --branch "breeding-season-2024"

# Record breeding activities
goat-manager breeding --doe-tag "D001" --buck-tag "B001" --breeding-date "2024-02-15" --breeding-method "natural" --status "bred"

goat-manager breeding --doe-tag "D003" --buck-tag "B001" --breeding-date "2024-02-16" --breeding-method "natural" --status "bred"

# Pregnancy confirmation (30 days later)
goat-manager breeding --doe-tag "D001" --buck-tag "B001" --status "confirmed_pregnant" --expected-kidding-date "2024-07-15"

goat-manager breeding --doe-tag "D003" --buck-tag "B001" --status "confirmed_pregnant" --expected-kidding-date "2024-07-16"

# Update doe status
goat-manager goat --tag-id "D001" --status "pregnant"
goat-manager goat --tag-id "D003" --status "pregnant"

# Commit breeding records
goat-manager version --action "commit" --message "Breeding season 2024 - pregnancies confirmed"
```

### Kidding Management
```bash
# D001 kids
goat-manager breeding --doe-tag "D001" --buck-tag "B001" --actual-kidding-date "2024-07-12" --kids-born 2 --kids-alive 2 --status "kidded" --notes "Twin doelings, both healthy"

# Add new kids to herd
goat-manager goat --tag-id "K001" --name "Daisy" --breed "alpine" --gender "female" --birth-date "2024-07-12" --sire-tag "B001" --dam-tag "D001" --status "active" --weight 3.2 --color "white with brown spots"

goat-manager goat --tag-id "K002" --name "Rose" --breed "alpine" --gender "female" --birth-date "2024-07-12" --sire-tag "B001" --dam-tag "D001" --status "active" --weight 3.0 --color "brown and white"

# Update doe status
goat-manager goat --tag-id "D001" --status "lactating"

# Record kidding health check
goat-manager health --goat-tag "D001" --record-type "checkup" --description "Post-kidding examination - healthy recovery" --veterinarian "Dr. Smith" --weight 58.5 --temperature 38.4

# Commit kidding records
goat-manager version --action "commit" --message "D001 kidding - twin doelings born healthy"
```

## Financial Tracking

### Feed Purchase
```bash
# Record major feed purchase
goat-manager farm --operation-type "feed_purchase" --description "Monthly alfalfa hay purchase" --quantity 100 --unit "bales" --cost 800.00 --supplier-buyer "Green Valley Feed" --performed-by "Manager" --notes "Premium quality alfalfa, stored in barn"

goat-manager farm --operation-type "feed_purchase" --description "Dairy pellet purchase" --quantity 500 --unit "kg" --cost 375.00 --supplier-buyer "Farm Supply Co" --performed-by "Manager"

# Record milk sales
goat-manager farm --operation-type "milk_sale" --description "Weekly milk sales to local creamery" --quantity 150 --unit "liters" --revenue 450.00 --supplier-buyer "Mountain Creamery" --performed-by "Manager"

# Commit financial records
goat-manager version --action "commit" --message "Monthly feed purchases and weekly milk sales"
```

## Analytics and Reporting

### Monthly Performance Review
```bash
# Generate comprehensive monthly report
echo "=== MONTHLY FARM REPORT ===" > monthly_report.txt
echo "Date: $(date)" >> monthly_report.txt
echo "" >> monthly_report.txt

# Farm summary
echo "FARM SUMMARY:" >> monthly_report.txt
goat-manager analytics --report-type "farm-summary" --output table >> monthly_report.txt
echo "" >> monthly_report.txt

# Milk production analysis
echo "MILK PRODUCTION (Last 30 days):" >> monthly_report.txt
goat-manager analytics --report-type "milk-production" --date-from "$(date -d '30 days ago' +%Y-%m-%d)" --output table >> monthly_report.txt
echo "" >> monthly_report.txt

# Individual goat performance
echo "TOP PERFORMERS:" >> monthly_report.txt
goat-manager analytics --report-type "goat-performance" --limit 5 --output table >> monthly_report.txt

echo "Monthly report generated: monthly_report.txt"
```

### Health Monitoring
```bash
# Check upcoming vaccinations
echo "Checking health schedules..."

# Get all health records with due dates
goat-manager health --limit 100 --output json | jq -r '.[] | select(.next_due_date != null and .next_due_date != "") | "\(.goat_tag): \(.record_type) due \(.next_due_date)"'

# Check recent health issues
echo "Recent health events:"
goat-manager health --limit 20 | grep -E "(illness|injury|treatment)"
```

## Data Management and Backups

### Weekly Backup Routine
```bash
#!/bin/bash
# weekly_backup.sh

WEEK=$(date +%Y-W%U)
BACKUP_REASON="Weekly backup - Week $WEEK"

echo "Creating weekly backup..."

# Create backup branch
BACKUP_BRANCH=$(goat-manager version --action "backup" --message "$BACKUP_REASON" --output json | jq -r '.backup_branch')

echo "Backup created: $BACKUP_BRANCH"

# Generate backup report
echo "BACKUP REPORT - $WEEK" > "backup_report_$WEEK.txt"
echo "Backup Branch: $BACKUP_BRANCH" >> "backup_report_$WEEK.txt"
echo "Date: $(date)" >> "backup_report_$WEEK.txt"
echo "" >> "backup_report_$WEEK.txt"

# Add current status to backup report
goat-manager version --action "status" --branch "main" --output table >> "backup_report_$WEEK.txt"

echo "Backup completed: backup_report_$WEEK.txt"
```

### Data Comparison
```bash
# Compare current data with last week's backup
LAST_BACKUP=$(goat-manager version --action "log" --limit 10 --output json | jq -r '.[] | select(.message | contains("Weekly backup")) | .commit_hash' | head -1)

if [ ! -z "$LAST_BACKUP" ]; then
    echo "Comparing current data with last backup..."
    goat-manager version --action "compare" --branch "main" --from-commit "$LAST_BACKUP" --output table
else
    echo "No previous backup found"
fi
```

## Advanced Workflows

### Seasonal Management
```bash
# Spring preparation workflow
create_spring_branch() {
    YEAR=$(date +%Y)
    BRANCH="spring-management-$YEAR"
    
    goat-manager version --action "branch" --branch "$BRANCH"
    goat-manager version --action "switch" --branch "$BRANCH"
    
    echo "Created and switched to branch: $BRANCH"
    
    # Spring health checks
    for goat in $(goat-manager goat --output json | jq -r '.[].tag_id'); do
        goat-manager health --goat-tag "$goat" --record-type "checkup" --description "Spring health assessment" --weight "$(shuf -i 50-80 -n 1).$(shuf -i 0-9 -n 1)"
    done
    
    # Commit spring preparations
    goat-manager version --action "commit" --message "Spring health assessments completed"
    
    echo "Spring management branch ready"
}

# Call the function
create_spring_branch
```

### Breeding Program Analysis
```bash
# Analyze breeding program effectiveness
analyze_breeding() {
    echo "BREEDING PROGRAM ANALYSIS"
    echo "========================"
    
    # Get all breeding records
    echo "Total breeding records:"
    goat-manager breeding --limit 1000 --output json | jq length
    
    echo ""
    echo "Success rates by buck:"
    goat-manager breeding --output json | jq -r 'group_by(.buck_tag) | .[] | "\(.[0].buck_tag): \(map(select(.status == "kidded")) | length)/\(length) = \((map(select(.status == "kidded")) | length) / length * 100 | floor)%"'
    
    echo ""
    echo "Average kids per successful breeding:"
    goat-manager breeding --output json | jq -r 'map(select(.status == "kidded" and .kids_born > 0)) | (map(.kids_born) | add) / length'
}

analyze_breeding
```

## Integration Examples

### Export to Spreadsheet
```bash
# Export all data for spreadsheet analysis
mkdir -p exports/$(date +%Y-%m-%d)
cd exports/$(date +%Y-%m-%d)

# Export all entities
goat-manager goat --output csv > goats.csv
goat-manager milk --limit 1000 --output csv > milk_records.csv
goat-manager health --limit 1000 --output csv > health_records.csv
goat-manager breeding --limit 1000 --output csv > breeding_records.csv
goat-manager feed --limit 1000 --output csv > feed_records.csv
goat-manager farm --limit 1000 --output csv > farm_operations.csv

echo "Data exported to exports/$(date +%Y-%m-%d)/"
```

### API Integration (JSON)
```bash
# Get data in JSON format for API integration
get_farm_data_json() {
    cat << EOF > farm_data.json
{
    "export_date": "$(date -Iseconds)",
    "farm_summary": $(goat-manager analytics --report-type "farm-summary" --output json),
    "goats": $(goat-manager goat --output json),
    "recent_milk": $(goat-manager milk --limit 100 --output json),
    "recent_health": $(goat-manager health --limit 50 --output json)
}
EOF
    echo "Farm data exported to farm_data.json"
}

get_farm_data_json
```

These examples demonstrate the comprehensive capabilities of the Goat Farm Management System across all aspects of farm operations, from daily routines to advanced analytics and data management.

