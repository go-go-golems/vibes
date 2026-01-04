# Getting Started with Goat Farm Manager

This guide will help you get up and running with the Goat Farm Management System quickly.

## Installation

### System Requirements
- Linux, macOS, or Windows
- Go 1.24.5 or later
- At least 1GB of free disk space
- Build tools (gcc, make)

### Step 1: Install Go
Download and install Go from the official website: https://golang.org/dl/

Verify installation:
```bash
go version
```

### Step 2: Install Build Tools
On Ubuntu/Debian:
```bash
sudo apt update
sudo apt install build-essential
```

On macOS:
```bash
xcode-select --install
```

### Step 3: Clone and Build
```bash
git clone <repository-url>
cd goat-farm-manager
make deps
make build
```

### Step 4: Initialize Database
```bash
make init-db
```

This creates the database directory and initial schema.

### Step 5: Install System-wide (Optional)
```bash
make install
```

## First Steps

### 1. Add Your First Goat
```bash
goat-manager goat --tag-id "G001" --name "Bella" --breed "alpine" --gender "female" --birth-date "2023-03-15" --status "lactating"
```

### 2. Record Milk Production
```bash
goat-manager milk --goat-tag "G001" --volume 3.2 --session "morning" --quality-grade "A" --milked-by "John"
```

### 3. Add Health Record
```bash
goat-manager health --goat-tag "G001" --record-type "checkup" --description "Monthly health check" --veterinarian "Dr. Smith" --temperature 38.5 --weight 65.5
```

### 4. View Your Data
```bash
# List all goats
goat-manager goat

# View milk production
goat-manager milk --goat-tag "G001"

# Check farm summary
goat-manager analytics --report-type "farm-summary"
```

### 5. Commit Your Changes
```bash
goat-manager version --action "commit" --message "Added first goat and initial records"
```

## Basic Workflow

### Daily Operations

1. **Morning Milking**
```bash
# Record morning milk for each goat
goat-manager milk --goat-tag "G001" --volume 3.2 --session "morning" --milked-by "John"
goat-manager milk --goat-tag "G002" --volume 2.8 --session "morning" --milked-by "John"
```

2. **Feeding**
```bash
# Record group feeding
goat-manager feed --feed-type "hay" --quantity 25 --unit "kg" --fed-by "John" --feeding-method "group"
```

3. **Evening Milking**
```bash
# Record evening milk
goat-manager milk --goat-tag "G001" --volume 2.9 --session "evening" --milked-by "Jane"
```

4. **Daily Commit**
```bash
goat-manager version --action "commit" --message "Daily records - $(date +%Y-%m-%d)"
```

### Weekly Tasks

1. **Health Checks**
```bash
# Record weekly health observations
goat-manager health --goat-tag "G001" --record-type "checkup" --description "Weekly health check - all normal" --weight 66.2
```

2. **Farm Operations**
```bash
# Log maintenance activities
goat-manager farm --operation-type "cleaning" --description "Deep clean milking parlor" --performed-by "Team"
```

3. **Analytics Review**
```bash
# Check weekly production
goat-manager analytics --report-type "milk-production" --date-from "$(date -d '7 days ago' +%Y-%m-%d)"
```

### Monthly Tasks

1. **Breeding Records**
```bash
# Record breeding activities
goat-manager breeding --doe-tag "G003" --buck-tag "G004" --breeding-date "2024-01-15" --breeding-method "natural"
```

2. **Financial Review**
```bash
# Check monthly analytics
goat-manager analytics --report-type "farm-summary"
```

3. **Backup**
```bash
# Create monthly backup
goat-manager version --action "backup" --message "Monthly backup - $(date +%Y-%m)"
```

## Understanding Output Formats

### Table Format (Default)
```bash
goat-manager goat
```
Displays data in a human-readable table format.

### JSON Format
```bash
goat-manager goat --output json
```
Machine-readable format, useful for integration with other tools.

### CSV Format
```bash
goat-manager milk --output csv > milk_records.csv
```
Spreadsheet-compatible format for data analysis.

## Common Use Cases

### Scenario 1: New Goat Arrival
```bash
# Add the goat
goat-manager goat --tag-id "G005" --name "Luna" --breed "nubian" --gender "female" --birth-date "2023-08-20" --sire-tag "G004" --dam-tag "G002"

# Initial health check
goat-manager health --goat-tag "G005" --record-type "checkup" --description "Arrival health check" --veterinarian "Dr. Smith" --weight 45.0 --temperature 38.3

# Commit the addition
goat-manager version --action "commit" --message "Added new goat Luna (G005)"
```

### Scenario 2: Breeding Season Management
```bash
# Create a branch for breeding season
goat-manager version --action "branch" --branch "breeding-season-2024"

# Switch to the branch
goat-manager version --action "switch" --branch "breeding-season-2024"

# Record breeding activities
goat-manager breeding --doe-tag "G001" --buck-tag "G004" --breeding-date "2024-02-01"
goat-manager breeding --doe-tag "G002" --buck-tag "G004" --breeding-date "2024-02-02"

# Commit breeding records
goat-manager version --action "commit" --message "Recorded breeding activities for February 2024"
```

### Scenario 3: Health Issue Management
```bash
# Record illness
goat-manager health --goat-tag "G003" --record-type "illness" --description "Mild respiratory symptoms" --veterinarian "Dr. Johnson" --medication "Antibiotics" --dosage "5ml twice daily"

# Update goat status
goat-manager goat --tag-id "G003" --status "sick"

# Follow-up treatment
goat-manager health --goat-tag "G003" --record-type "treatment" --description "Follow-up treatment - improvement noted" --veterinarian "Dr. Johnson"

# Recovery
goat-manager goat --tag-id "G003" --status "active"
goat-manager health --goat-tag "G003" --record-type "checkup" --description "Full recovery confirmed"
```

## Tips and Best Practices

### Data Entry
- Use consistent naming conventions for goat tags (e.g., G001, G002, etc.)
- Always include units when recording measurements
- Be descriptive in notes and descriptions
- Record data promptly to ensure accuracy

### Version Control
- Commit changes regularly (daily recommended)
- Use descriptive commit messages
- Create branches for major changes or experiments
- Make backups before significant operations

### Analytics
- Review analytics regularly to identify trends
- Use date ranges to focus on specific periods
- Export data to CSV for advanced analysis in spreadsheets

### Troubleshooting
- Use `goat-manager help` for command-specific help
- Check the logs if operations fail
- Ensure database directory has proper permissions
- Verify Go installation and PATH settings

## Next Steps

Once you're comfortable with the basics:

1. Explore advanced analytics features
2. Set up automated backups
3. Integrate with other farm management tools
4. Customize output formats for your needs
5. Consider setting up a web dashboard (future feature)

For more detailed information, see the full documentation in the `docs/` directory.

